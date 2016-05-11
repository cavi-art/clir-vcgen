;;; CAVIART-VCGEN - A verification condition generator for the CAVI-ART project
;;; developed originally at GPD UCM.
;;; Copyright (C) 2016 Santiago Saavedra López, Grupo de Programación Declarativa -
;;; Universidad Complutense de Madrid
;;;
;;; This file is part of CAVIART-VCGEN.
;;;
;;; CAVIART-VCGEN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; CAVIART-VCGEN is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with CAVIART-VCGEN.  If not, see <http://www.gnu.org/licenses/>.


(in-package :cl-user)

(declaim (optimize debug))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels
      ((delete-package-recursive (package-identifier)
	 (let ((pkg (find-package package-identifier)))
	   (when pkg
	     (mapcar #'delete-package-recursive (package-used-by-list pkg))
	     (when (find-package package-identifier)
	       (delete-package pkg))))))
    (delete-package-recursive :caviart.vc-gen)))

(defpackage :ir.vc
  (:use)
  (:documentation "Verification conditions package. Defines no symbols
  a priori, but will later export those of core."))

;; (delete-package :ir.vc.core)
;; (delete-package :ir.vc.core.impl)

(defpackage :ir.vc.core
  (:use)

  ;; Runtime directives
  (:import-from :cl :declare :optimize :speed :debug :safety)
  (:export :declare :optimize :speed :debug :safety)

  ;; Logical connections
  (:import-from :cl :and :or)
  (:export :and :or)

  ;; We can use the same `the' and `type' as CL.
  (:export :the :type)

  ;; Our basic macro for defining everything else.
  (:export :verification-unit)

  ;; Basic types
  (:export :int)
  (:export :bool :true :false)

  (:export :*assume-verified* :*verify-only* :*external-functions*)

  ;; Our own DSL keywords
  (:export :assertion :precd :postcd)
  (:export :define :lettype :letvar :letconst :let :let* :letfun :case :default "@" "@@")

  ;; Assertion comparators (will later be in another package)
  (:shadowing-import-from :cl :=)
  (:export :=))

(defpackage :ir.vc.core.impl
  (:documentation "Functionality for generating Verification Conditions for later use in Why3")
  (:use :cl :ir.utils)
  (:import-from :ir.vc.core :assertion :precd :postcd :default :*external-functions* :true :false)
  (:export :verifier-identifier :verifier-output :verifier-output-comment *entry-points*
	   :remove-decls))

(in-package :ir.vc.core.impl)

(defun get-toplevel-functions (clir)
  (loop
     for a in clir
     when (eq (car a) 'ir.vc.core:define) collect (cadr a)))

(defun get-toplevel-definitions (clir)
  (loop
     for a in clir
     when (eq (car a) 'ir.vc.core:define) collect (cdr a)))


(defun verifier-identifier (symb)
  "Sanitizes a symbol name so that it is a valid identififer in the
  underlying verifier grammar."
  (symbol-name symb))


(defun verifier-output (&rest elements)
  (write-to-string elements))


(defun verifier-output-comment (&rest forms)
  (format nil " (* ~s *)~%"
	  (with-output-to-string (s)
	    (apply #'format s forms))))


(defun get-precondition (function-definition)
  (let* ((body (function-body function-definition))
	 (declarations (cdr (assoc 'declare body)))
	 (assertions (cdr (assoc 'assertion declarations)))
	 (precd (assoc 'precd assertions)))
    (when precd
      (cadr precd))))

(defun get-postcondition (function-definition)
  (let* ((body (function-body function-definition))
	 (declarations (cdr (assoc 'declare body)))
	 (assertions (cdr (assoc 'assertion declarations)))
	 (postcd (assoc 'postcd assertions)))
    (when postcd
      (cadr postcd))))


(defparameter *entry-points* nil)
(defvar *external-functions* nil)


;;;; Grammar follows 
(defmacro ir.vc.core:verification-unit (package-id &key sources uses documentation verify-only assume-verified)
  (declare (ignorable sources))
  (let ((pkg (get-package-symbol package-id)))
      `(progn (when (find-package ,pkg)
		(unuse-package ',@uses ,pkg)
		(delete-package ,pkg))
	      (defpackage ,pkg
		 (:use ,@uses)
		 (:documentation ,documentation))
	    (in-package ,pkg)
	    (cl:mapcar (cl:lambda (f) (cl:push f ir.vc.core:*assume-verified*)) ,assume-verified)
	    (cl:mapcar (cl:lambda (f) (cl:push f ir.vc.core:*verify-only*)) ,verify-only)
	    ;; (verifier-output-comment "Parsing units in package ~a~%" ,package-id)
	    )))


(defmacro ir.vc.core:lettype (type-symbol param-list type-boolean-expression optional-data)
  (declare (ignorable type-boolean-expression))
  `(prog1
       (verifier-output 'type ,type-symbol ,param-list)
     (verifier-output-comment-to-string ,optional-data)))


(defvar *premise-list* nil)
(defvar *goal-set* nil)
(defvar *variable-list* nil)
(defvar *function-list* nil)
(defvar *current-function* nil)

(defun remove-decls (body &optional declarations)
  "Gets the `declare'-d and docstring forms (if there are any) of a
defun-ish body and the resulting body as values."
  (declare (type list body declarations))
  (let ((form (car body)))
    (typecase form
      ((or symbol nil) (values body (reverse declarations)))
      (cons (if (eq (car form) 'declare)
		(remove-decls (cdr body) (cons form declarations))
		(values body (reverse declarations)))))))

;; TODO at some point, replace function "list" with a proper struct
(defun function-body (function-definition)
  (cdddr function-definition))

(defun typed-lambda-list (function-definition)
  "Gets the lambda list of a function definition. E.g., simple
   accessor"
  (second function-definition))

(defun typed-result-list (function-definition)
  "Gets the result list of a function definition. E.g., simple
   accessor"
  (third function-definition))

(defun get-current-typed-result-list ()
  (typed-result-list (assoc *current-function* *function-list*)))


(defmacro with-empty-premise-list (&body body)
  `(let ((*premise-list* nil))
     ,@body))

(defmacro with-named-premise (name content &body body)
  (if (or name (macroexpand-1 content))
      `(let ((*premise-list* (cons (list ,name ,content) *premise-list*)))
	 ,@body)
      `(progn ,@body)))

(defmacro with-premise (content &body body)
  (macroexpand-1 `(with-named-premise "" ,content ,@body)))

(defmacro terminal-expression (expression &key type norename)
  "We have to do postcd/{result=expression}"
  (declare (ignore type))
  (if norename
      `(output-goal (@postcd *current-function*))
      `(output-goal (rename-symbols (@postcd *current-function*)
				    (drop-types (get-current-typed-result-list))
				    ',expression))))


(defmacro with-variables (typed-variable-list &body body)
  "Introduces variables in the `premise-list' and `symbol-macrolet's
  those variables so that when they are `macroexpand-1'-ed they will
  be known as variables and the postcondition will be well formed."
  (if typed-variable-list
      (labels ((symbol-macroletize (typed-var)
		 (destructuring-bind (var-name var-type) typed-var
		   `(,var-name (terminal-expression (,var-name) :type ,var-type)))))
	`(with-premise (list :forall ',typed-variable-list)
	   (symbol-macrolet ,(mapcar #'symbol-macroletize typed-variable-list)
	     ,@body)))
      `(progn ,@body)))

(defmacro with-function-definitions (new-definitions &body body)
  `(let ((*function-list* (append ,new-definitions (list *function-list*))))
     ,@body))

(defmacro with-function-definition (new-definition &body body)
  `(let ((*function-list* (cons ,new-definition *function-list*)))
     ,@body))

(defmacro with-current-function (function-name &body body)
  `(let ((*current-function* ,function-name))
     ,@body))


(defun @precd (function-name function-args)
  (let* ((f (assoc function-name (append *function-list* *external-functions*)))
	 (precd (get-precondition f))
	 (parameter-list (drop-types (typed-lambda-list f))))
    (if precd
	;; Rename parameters
	(rename-symbols precd parameter-list function-args)
	;; Else
	'true
	;; (list 'THE_PRECD_PLACEHOLDER_FOR
	;;       function-name
	;;       (list parameter-list function-args))
	)))

(defun @postcd (function-name &optional function-args result-args)
  (let* ((f (assoc function-name (append *function-list* *external-functions*)))
	 (postcd (get-postcondition f))
	 (parameter-list (drop-types (typed-lambda-list f)))
	 (result-list (drop-types (typed-result-list f))))
    (if postcd
	;; Rename lambda parameters and result parameters
	(rename-symbols
	 (rename-symbols postcd parameter-list function-args)
	 result-list result-args)
	;; Else
	(list 'THE_POSTCD_PLACEHOLDER_FOR function-name
	      (list parameter-list function-args)
	      (list result-list result-args))
	)))


(defun rename-symbols (formula original-symbols new-symbols &optional rec)
  (unless (or (not new-symbols) rec)
    (format t ";; Renaming ~S from ~s in terms of ~s~%" formula original-symbols new-symbols))
  (if (eq nil new-symbols)
      formula
      ;; Simple s-exp tree walker
      (typecase formula
	(nil nil)
	(symbol (let ((p (position formula original-symbols)))
		  (if p
		      (nth p new-symbols)
		      formula)))
	(cons (cons (rename-symbols (car formula) original-symbols new-symbols t)
		    (rename-symbols (cdr formula) original-symbols new-symbols t)))
	(t formula))))


(defmacro with-goal-set (&body body)
  `(let ((*goal-set* nil))
     (let ((result (progn ,@body)))
       (values (reverse *goal-set*) result))))

(defun @-p (form)
  "Returns whether the form is a funcall."
  (and (consp form)
       (eq 'ir.vc.core:@
	   (car form))))

(defun expr-postcondition (form &optional results)
  "Gets the postcondition of an expression, if that expression is a
  funcall. If it is not, it returns NIL (which is a suitable return
  value for `with-premise')."
  (if (@-p form)
      (@postcd (cadr form) (cddr form) results)
      (list 'ir.vc.core:@ '= results form)))

(defmacro assume-binding (lhs form &body body)
  (if lhs
      `(with-premise (expr-postcondition ',form ',lhs)
	,@body)))


(defmacro ir.vc.core:define (&whole definition
			       function-name typed-lambda-list result-arg
			     &body full-body)
  "`DEFINE' introduces a set of goals. There should be pre/post
  conditions in this body, and from the expression (likely a letfun)
  we should increase our goal-set until we finally prove the latest
  expression implies the postcondition. For the moment, a define is
  self-contained, meaning definitions inside cannot call another
  define."
  (declare (ignorable result-arg full-body))
  (let ((body (remove-decls (function-body (cdr definition)))))
    `(defun ,function-name ()
       (with-goal-set
	 (with-empty-premise-list
	   (with-variables ,typed-lambda-list
	     (with-function-definition ',(cdr definition)
	       (with-current-function ',function-name
		 (with-named-premise ,(symbol-name function-name)
		     (@precd ',function-name ',(drop-types typed-lambda-list))
		   ,@body)))))))))

(defmacro ir.vc.core:letfun (definitions
			     &body full-body)
  `(with-function-definitions ',definitions
     ;; We need to verify each function in the letfun.  Inner
     ;; non-pre/post functions will be verified as if the
     ;; current-function was the toplevel one. Functions with
     ;; pre/post conditions will be verified only up to their
     ;; postcondition.

     ,@(mapcar
	(lambda (e)
	  (destructuring-bind
		(function-name typed-lambda-list result-lambda-list &body inner-body) e
	    (declare (ignore typed-lambda-list result-lambda-list))
	    (multiple-value-bind (body decls) (remove-decls inner-body)
	      (let* ((assertions (cdr (assoc 'assertion decls)))
		     (precd (assoc 'precd assertions))
		     (postcd (assoc 'postcd assertions)))
		(if (and precd postcd)
		    `(with-current-function ,function-name
		       ,@body)
		    `(progn ,@body))))))
	definitions)

     ;; We also need to verify the main toplevel function
     ,@full-body))


(defun drop-types (typed-var-list)
  (mapcar #'car typed-var-list))

(defun drop-types-from-case-pattern (pattern)
  (typecase pattern
    (symbol pattern)
    (cons (case (car pattern)
	    (ir.vc.core:the (third pattern)
			    )
	    (ir.vc.core:@ pattern)
	    ;; Unmatched case TUPLE
	    ))))

(defun case-alternative (case-condition)
  (lambda (alt idx)
    (destructuring-bind (pattern form) alt
      `(with-named-premise (format nil "case_~D" ,idx) 'true
	 (assume-binding ,(drop-types-from-case-pattern pattern) ',case-condition
	   ,form)))))

(defun case-default-p (alt)
  (eq (car alt)
      'default))

(defun case-alternative-default (condition default-alternative alternative-list)
  (if alternative-list
      (let ((pattern (caar alternative-list)))
	`(with-premise (list 'ir.vc.core:@ '<> ,(drop-types-from-case-pattern pattern) ',condition)
	   ,(case-alternative-default condition default-alternative (cdr alternative-list))))
      (let ((default-body (second default-alternative)))
	default-body)))

(defmacro ir.vc.core:case (condition &body alternative-list)
  "A `ir.vc.core:CASE' with n alternatives will generate n goals, one
  per alternative, sequentially."
  (let ((non-default-alternative-list (remove-if #'case-default-p alternative-list))
	(default-alternative-list (remove-if-not #'case-default-p alternative-list)))
    (let ((default-alternative (car default-alternative-list)))
      (assert (eq nil (cdr default-alternative-list)))
      `(let ((case-condition ',condition))
	 (declare (ignore case-condition))
	 ,@(loop for elt in non-default-alternative-list and idx from 0
	      collect (funcall (case-alternative condition) elt idx))
	 ,(when default-alternative
		`(with-named-premise "case_def" 'true
		   ,(case-alternative-default condition default-alternative non-default-alternative-list)))))))

(defmacro ir.vc.core:the (expr-type value)
  `(terminal-expression (,value) :type ,expr-type))

(defmacro maybe-output-precd-goal (val &key name)
  (when (@-p val)
    `(output-goal (@precd ',(cadr val) ',(cddr val)) :name ,name)))

(defmacro ir.vc.core:let (typed-var-list val &body body)
  "Lexically binds a var, syntax is: (let var val body-form). It can
   destructure tuples as (let (a b) (list a b) a)"
  (assert (not (cdr body))) ;; Only one expression
  `(progn
     (maybe-output-precd-goal ,val :name "letpre")
     (with-variables ,typed-var-list
       (with-named-premise "inlet" (@precd ',(cadr val) ',(cddr val))
	 (assume-binding ,(drop-types typed-var-list) ,val
	   ,@body)))))

(defmacro ir.vc.core:@ (function-name &rest rest)
  "We will use this macro to call all further functions. Depending
  on whether they already have pre/post conditions on them, we will
  terminate the goal or pursue further by interpreting the next
  function."
  (let ((precd `(@precd ',function-name ',rest))
	(postcd `(@postcd ',function-name ',rest (drop-types (get-current-typed-result-list)))))
    `(progn
       (output-goal ,precd :name "pre")

       (with-premise ,precd
	 (macrolet ((ir.vc.core:the (type value) (declare (ignore type)) value))
	   (with-premise (list :forall (get-current-typed-result-list))
	     (if ,postcd
		 (with-named-premise "post" ,postcd
		   (format t "Assuming postcd = ~S for function ~S" ,postcd ',function-name)
		   (terminal-expression ((ir.vc.core:@ ,function-name ,@(mapcar #'macroexpand-1 rest))) :norename t))
		 (terminal-expression ((ir.vc.core:@ ,function-name ,@(mapcar #'macroexpand-1 rest))) :norename t))))))))

(defun output-goal (target &key name)
  (when (not (eq target 'true))
    (push (list (reverse *premise-list*) (list name target)) *goal-set*)))

;;;; End of grammar. 


(defun clir-formula-to-string (formula)
  (labels ((is-infix (op)
	     (and (symbolp op)
		  (member op
			  '(+ - * / < <= >= = > % <>)
			  :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b))))))
	   (clir-term-to-string (term)
	     (typecase term
	       (number term)
	       (symbol term)
	       (string term)
	       (cons (case (car term)
		       (quote (second term))
		       (ir.vc.core:the (third term))
		       (ir.vc.core:@ (apply-predicate (rest term)))
		       (t (error "Term ~S not understood." term))))
	       (t (error "Term ~S not understood." term))))
	   (apply-predicate (p)
	     (if (is-infix (first p))
		 (format nil (format nil "~~{~~A~~^ ~A ~~}"
				     (first p))
			 (mapcar #'clir-term-to-string (rest p)))
		 (format nil "~A(~{~A~^,~})" (first p) (mapcar #'clir-term-to-string (rest p))))))
    (typecase formula
      (symbol formula)
      (string formula)
      (number formula)
      (cons (case (car formula)
	      (:forall (format nil "forall ~:{~A:~A~:^,~}. " (second formula)))
	      (and (format nil "~{~A~^ /\\ ~}" (rest formula)))
	      (or (format nil "~{~A~^ \\/ ~}" (rest formula)))
	      (ir.vc.core.impl::the_postcd_placeholder_for (format nil "POSTCD[~A]" (rest formula)))
	      (ir.vc.core.impl::the_precd_placeholder_for "true(*PRE[~A]*)" (rest formula))
	      (ir.vc.core:@ (apply-predicate (rest formula)))
	      (t (error "Formula ~S not understood. (car=~S)" formula (car formula)))))
      (t (error "Formula ~S not understood." formula)))))

(defun clir-premises-to-string (premises)
  (flet ((not-quantifier (premise)
	   (or (not (consp premise))
	       (not (member (car premise) (list :forall :exists))))))
    (when premises
      (concatenate 'string (format nil "~(~A~)~@[~&~I -> ~]"
				   (clir-formula-to-string (first premises))
				   (and (not-quantifier (first premises))
					(rest premises)))
		   (clir-premises-to-string (rest premises))))))

(defun clir-goal-to-string (goal)
  (destructuring-bind (premise-list target) goal
    (let ((formulae (append premise-list (list target))))
      (format nil "goal ~A: ~A~%"
	      (apply #'concatenate 'string (mapcar #'first formulae))
	      (clir-premises-to-string (mapcar #'second formulae))))))



(defmacro quantified-lambda-params (lambda-params)
  "Adds forall quantifiers for the received typed parameters. This is
  useful for generating goals involving preconditions."
  `'(:forall . ,lambda-params))


(defun load-file (pathname)
  "Loads a file eval'uating package changes, so that identifiers will
get read and `INTERN'-ed on their proper packages."
  (with-throwaway-package (:ir.vc.core :ir.vc.builtins :ir.vc) (:ir)
    ;; We need to use the IR package so that we import the
    ;; verification-unit construct in order to `EVAL' it on the `LOOP'
    ;; to make the new package definition.
    (with-open-file (clir-stream pathname)
      (loop
	 for a = (read clir-stream nil)
	 while a
	 if (and (consp a)
		 (symbolp (car a))
		 (string-equal (symbol-name (car a))
			       "verification-unit"))
	 collect (progn (eval a) a)
	 else
	 collect a))))

(defun eval-clir-file (pathname)
  (with-throwaway-package (:ir.vc.core :ir.vc.builtins :ir.vc) (:ir)
    (with-open-file (clir-stream pathname)
      (loop for a = (read clir-stream nil)
	 while a
	 collect (eval a)))))



(setf *entry-points* nil)

(defparameter *clir-extension* ".clir")
(defparameter *prover-extension* ".why")

(defun prover-file-from-clir (path)
  (let* ((name (pathname-name path))
	 (basename (subseq name 0 (find #\. name :from-end t))))
    (merge-pathnames
     (make-pathname :directory (pathname-directory path) :type :unspecific)
     (make-pathname :name (concatenate 'string basename *prover-extension*) :type :unspecific))))

(defun generate-theory (clir-file f)
  "f is a lambda taking zero arguments which returns the goals. It
  should be something like (lambda () (factorial::factorial))."
  ;; TODO Use real imports
  (eval-clir-file clir-file)
  (let* ((prover-file (prover-file-from-clir clir-file))
	 (goals (mapcar #'clir-goal-to-string (funcall f))))

    (when (probe-file prover-file)
      (delete-file (probe-file prover-file)))

    (with-open-file (stream prover-file :direction :output)
      (format stream "theory UntitledTheory ~% use import int.Int~% use import int.Fact~%~{~A~^~%~} ~%~%end~%" goals))))

(defun test-clir (clir-file f)
  (generate-theory clir-file f)
  (let ((prover-file (prover-file-from-clir clir-file)))
    (asdf::run-program (list "why3" "ide" (namestring prover-file)))))

(defmacro easy-file (basename)
  (format nil "../test/~(~A~).clir" (symbol-name basename)))

(defmacro easy-test (basename function &optional package)
  `(test-clir (pathname (easy-file ,basename))
	      (lambda () ,(if package
			      `(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
			      (list function)))))

(defmacro easy-goals (basename function &optional package)
  `(progn
     (eval-clir-file (pathname (easy-file ,basename)))
     (mapcar #'clir-goal-to-string ,(if package
					`(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
					(list function)))))

;; (cadr (load-file (easy-file qsort)))
;; (mapcar #'clir-goal-to-string (qsort::quicksort))
;; (easy-test qsort qsort::quicksort)
;; (easy-goals inssort |inssort|::inssort)
;; (easy-test inssort |inssort|::inssort)

;; (easy-test factorial factorial::factorial)
;; (test-clir (pathname "../test/factorial.clir") (lambda () (factorial::factorial)))
