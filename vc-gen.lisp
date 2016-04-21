(in-package :cl-user)

(declaim (optimize debug))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun delete-package-recursive (package-identifier)
    (let ((pkg (find-package package-identifier)))
      (when pkg
	(mapcar #'delete-package-recursive (package-used-by-list pkg))
	(when (find-package package-identifier)
	  (delete-package pkg)))))
  (delete-package-recursive :caviart.vc-gen))

(defpackage :caviart.vc-gen
  (:documentation "Functionality for generating Verification Conditions for later use in Why3")
  (:use :cl)
  (:export :verifier-identifier :verifier-output :verifier-output-comment *entry-points*
	   :get-decls :remove-decls))

(in-package caviart.vc-gen)

(defun eval-when-p (form)
  (or (eq (car form) 'cl:eval-when)
      (eq (car form) 'eval-when)))

(defun remove-eval-when (clir)
  "Removes eval-when from a clir if it is the first form."
  (let ((first-form (car clir)))
    (if (and (consp first-form)
	     (eval-when-p first-form))
	(cdr clir)
	clir)))

;; (defun verify-toplevel-grammar (clir)
;;   "TODO:: Verify that the grammar conforms to valid CLIR grammar.
;; This makes it hard for a malicious CLIR representation to get inside
;; this parser, by analizing possible external CL calls, which would be
;; forbidden."
;;   (labels ((clir-has-package (clir)
;; 	     (eq (caar clir)
;; 		 'package))
;; 	   (clir-has-only-one-package (clir)
;; 	     (and (clir-has-package clir)
;; 		  (not (clir-has-package (cdr clir)))))
;; 	   (clir-has-only-allowed-symbols (clir)
;; 	     (block nil (when (eq nil clir)
;; 			  (return t))

;; 		    (let ((next-form (car clir)))
;; 		      (when (not (consp next-form))
;; 			(return (clir-has-only-allowed-symbols (cdr clir))))
;; 		      (when (is-allowed-symbol (car next-form))
;; 			(return (and (or (eq (car next-form)
;; 					     'declare) ;; Either it is a declaration
				      
;; 					 ;; or we need to check further
;; 					 ;; allowed symbols in the
;; 					 ;; nested expression
;; 					 (clir-has-only-allowed-symbols next-form))

;; 				     ;; Plus we always check in the rest
;; 				     ;; of the list
;; 				     (clir-has-only-allowed-symbols (cdr clir)))))
		      
;; 		      nil)))
;; 	   (is-allowed-symbol (symbol)
;; 	     (let ((allowed '(package assertion define defvar deftype let
;; 			      case sel-array mod-array sel-array-heap
;; 			      mod-array-heap len-array-heap declare
;; 			      the :forall -> <-> and or t nil)))
	       
;; 	       (position symbol allowed)))))
;;   (let ((checks (list 'clir-has-only-one-package
;; 		      'clir-has-only-allowed-symbols)))

;;     (apply #'and (mapcar (lambda (f) (funcall f clir)) checks))))


(defun read-file (pathname)
  (with-open-file (clir-stream pathname)
    (let ((data (loop
		   for a = (read clir-stream nil)
		   while a
		   collect a)))
      (remove-eval-when data))))

(defun get-toplevel-functions (clir)
  (loop
     for a in clir
     when (eq (car a) 'define) collect (cadr a)))

(defun get-decls (body)
  "Gets the `declare'-d and docstring forms (if there are any) of a
defun-ish body"
  (let ((form (car body)))
    (if (or (and (listp form)
		 (eq (car form)
		     'declare))
	    (stringp form))
	(cons form (get-decls (cdr body)))
	nil)))

(defun remove-decls (body)
  "Returns the `declare'-stripped forms of a `defun'-ish body so
that only executable things get there."
  (let ((form (car body)))
    (if (or (and (listp form)
		 (eq (car form)
		     'declare))
	    (stringp form))
	(remove-decls (cdr body))
	body)))


;; Cleanse the environment every time this file gets loaded (helps
;; debugging each time we change a definition).
(when (find-package :vc.rt)
  (mapcar #'delete-package (package-used-by-list :vc.rt))
  (delete-package :vc.rt)
  (delete-package :vc.rt.core)
  (delete-package :vc.rt.builtins))

(defmacro with-package (pkgname &body body)
  (let ((pkg-var (gensym)))
    `(let ((,pkg-var *package*))
       (in-package ,pkgname)
       ,@body
       (unless (eq ,pkg-var *package*)
	 (in-package ,pkg-var)))))


(defun delete-package-recursive (package-identifier)
  (let ((pkg (find-package package-identifier)))
    (when pkg
      (mapcar #'delete-package (package-used-by-list pkg))
      (when (find-package package-identifier)
	(delete-package pkg)))))

(defun verifier-identifier (symb)
  "Sanitizes a symbol name so that it is a valid identififer in the
  underlying verifier grammar."
  (symbol-name symb))

(defun verifier-output (&rest elements)
  (write-to-string elements))

(defun verifier-output-comment (&rest forms)
  (format t "(* ~s *)~%"
	  (with-output-to-string (s)
	    (apply #'format s forms))))

(defparameter *entry-points* nil)

(defpackage :vcgen.rt.core
  (:use :cl :caviart.vc-gen)
  (:export :eval-when :declare)
  (:export :load :assertion :declare :the :type :optimize :speed :debug :safety)
  (:export :*assume-verified* :*verify-only* :generate-goals)
  (:export :precd :postcd)
  (:export :verification-unit :define :lettype :letvar :letconst :let :let* :letfun :case)
  (:export "@")

  (:import-from :cl :list)
  (:export :list)
  
  (:export :int)
  (:export :bool :true :false)
  

  (:shadow :package)
  (:shadow :let :case :funcall))

(in-package :vcgen.rt.core)

(defmacro verification-unit (package-id &key sources uses documentation verify-only assume-verified)
  (declare (ignorable sources uses documentation verify-only assume-verified))
  `(verifier-output-comment "Parsing units in package ~a~%" ,package-id))

(defmacro lettype (type-symbol param-list type-boolean-expression optional-data)
  (declare (ignorable type-boolean-expression))
  `(verifier-output 'type ,type-symbol ,param-list "; " ,optional-data))

;; TODO Decide whether to delete or not
(defmacro letvar (symbol initial-value)
  (labels ((get-type (form)
	     (unless (eq (car form) 'the)
	       (error "Untyped value"))
	     (cadr form))
	   (get-value (form)
	     (unless (eq (car form) 'the)
	       (error "Untyped value"))
	     (caddr form)))
    `(verifier-output 'axiom ,(verifier-identifier symbol) ":" ,(get-type initial-value) '= ,(get-value initial-value))))

(deftype int () `(cl:integer ,cl:most-negative-fixnum ,cl:most-positive-fixnum))
(deftype bool () '(cl:member true false))


(defmacro define (function-name typed-lambda-list result-arg &body full-body)
  "`DEFINE' introduces a set of goals. There should be pre/post
conditions in this body, and from the expression (likely a letfun) we
should increase our goal-set until we finally prove the latest
expression implies the postcondition. Additionally, the definitions in
this define may call definitions in another define, so the goals
cannot be checked inline by macroexpansion."
  `(progn
     (push '(,function-name ,typed-lambda-list ,full-body) *entry-points*)))

;; Just to keep emacs happy
(defmacro let-clir (var val &body body)
  `(progn
     (append-to-current-goals (verifier-output '-> ,var '= ,val))
     ,@body))

(defmacro let (typed-var val &body body)
  `(progn
     (append-to-current-goals (verifier-output '-> (list :forall (list typed-var) ,(car typed-var) '= ,val)))
     ,@body))

(defmacro letfun (funlist &body body)
  (declare (ignorable funlist body))
  `(let* ((*inner-functions* (append ,funlist *inner-functions*)))
     ,@body))

(defmacro case (condition &body cases)
  (flet ((split-case (case-clause)
	   (cl:let ((pattern (car case-clause))
		    (body (cdr case-clause)))
	     `(progn
		(append-to-current-goals
		  (verifier-output'-> ,condition '= ,pattern))
		,@body))))
    `(split-current-goals-for
       ,(mapcar #'split-case cases))))

(defun get-precondition (body)
  (let* ((declarations (cdr (assoc 'declare body)))
	 (assertions (cdr (assoc 'assertion declarations)))
	 (precd (assoc 'precd assertions)))
    (when precd
      (cadr precd))))

(defun get-postcondition (body)
  (let* ((declarations (cdr (assoc 'declare body)))
	 (assertions (cdr (assoc 'assertion declarations)))
	 (postcd (assoc 'postcd assertions)))
    (when postcd
      (cadr postcd))))

(defmacro @ (function-name &rest rest)
    "We will use this macro to call all further functions. Depending on
  whether they already have pre/post conditions on them, we will terminate
  the goal or pursue further by interpreting the next function."
  (let* ((f (assoc function-name *entry-points*))
	 (lambda-params (cadr f))
	 (body (caddr f))
	 (precd (get-precondition body))
	 (postcd (get-postcondition body)))
    (if (or precd postcd)
	`(progn
	   ,(when precd
		  `(append-to-current-goals
		     (verifier-output '-> (with-bound-params (,lambda-params ,rest)
					    ,precd))))
	   ,(when postcd
		  `(with-new-goal-set
		     (verifier-output (with-bound-params (,lambda-params ,rest)
					,postcd))
		     (append-to-current-goals (get-caller-postcd)))))
	;; No pre/postcd. We have to inline the function
	`(append-to-current-goals
	   ,@body))))

(defmacro generate-goal (function-name)
  (let* ((f (assoc function-name *entry-points*))
	 (full-body (caddr f)))
    (cl:let ((lambda-params (cadr f))
	     (body (remove-decls full-body))
	     (decls (get-decls full-body)))
      `(with-new-goal-set
	 (set-goal-entry-point ',function-name)
	 (append-to-current-goals
	   (verifier-output (quantified-lambda-params ,lambda-params))
	   (verifier-output '->
			    ,(get-precondition decls)))
	 ,@body))))


(defmacro quantified-lambda-params (lambda-params)
  "Adds forall quantifiers for the received typed parameters. This is
  useful for generating goals involving preconditions."
  `'(:forall . ,lambda-params))

(defmacro with-goal-set (goal-set name &body body)
  `(let* ((goals ,goal-set)
	  (goal-name-pattern (or ,name "unnamed-entry-point")))
     (flet ((set-goal-entry-point (new-name)
	      (setf goal-name-pattern (if (stringp new-name)
					  new-name
					  (symbol-name new-name)))))
       (macrolet
	   ((split-current-goals-for
		(&body body)
	      `(progn
		 ,@(mapcar (lambda (goal)
			     `(with-goal-set
				  ,goal
				  (gensym goal-name-pattern)
				,@body))
			   goals)))
	    (append-to-current-goals
		(&body body)
	      `(progn ,@(mapcar (lambda (form)
				  (mapcar (lambda (goal) (push form goal)) goals)) ,body))))
	 ,@(mapcar #'macroexpand body)))))

(defmacro with-new-goal-set (&body body)
  `(with-goal-set nil "unnamed-entry-point"
     ,@ (mapcar #'macroexpand body)
     (print-goals)))


(defmacro generate-goals ()
1  (flet ((generate-goal (elt)
	   (if (get-precondition (caddr elt))
	       (list (list 'generate-goal (car elt)))
	       nil)))
    `(progn
       ,@(mapcan #'generate-goal *entry-points*))))


(get-precondition (caddr (seventh *entry-points*)))
(cdr (assoc 'assertion (cdr (assoc 'declare (caddr (seventh *entry-points*))))))

(defmacro print-goals ()
  `(mapcar #'print goals))

(defmacro with-new-goal-for-entry-point (entry-point &body body)
  `(with-new-goal-set
     (set-goal-entry-point ,entry-point)
     ,@body))

(in-package :caviart.vc-gen)

(defmacro with-gensyms (var-list &body body)
  `(let ,(mapcar (lambda (s)
		   `(,s (gensym ,(symbol-name s))))
		 var-list)
     ,@body))

(defmacro without-packages (package-list &body body)
  (with-gensyms (renamed-packages rename-package-function restore-package-names)
    `(let ((,renamed-packages nil))
       (flet ((,rename-package-function (pkg)
		(when (find-package pkg)
		  (let ((new-name (gensym)))
 (push (list pkg new-name (package-nicknames pkg)) ,renamed-packages)
		    (rename-package pkg new-name))))
	      (,restore-package-names ()
		(mapc (lambda (pkg)
			(rename-package (cadr pkg) (car pkg) (cddr pkg))) ,renamed-packages)))
	 (mapc #',rename-package-function ',package-list)
	 (unwind-protect
	      (progn ,@body)
	   (,restore-package-names))))))

(defun parse-in-environ (pathname)
  (cl:in-package :caviart.vc-gen)
  (progn
    (let ((pkg (find-package :CLIR.VC)))
      (when pkg
	(unuse-package (package-use-list pkg) pkg)
	(delete-package pkg)))

    (without-packages (:ir :ir.core :ir.builtins)
      (make-package :CLIR.VC :nicknames '(:ir.core))
      (use-package :vcgen.rt.core :CLIR.VC)
      (in-package :CLIR.VC)
      (let ((result (append (caviart.vc-gen::read-file pathname)
			    '((macroexpand-1 '(generate-goals))))))
	(in-package :caviart.vc-gen)
	result))))

(defun eval-in-environ (forms)
  (cl:in-package :CLIR.VC)
  (let ((result (cl:mapcar #'cl:eval forms)))
    (cl:in-package :caviart.vc-gen)
    result))


(setf *entry-points* nil)
(defparameter *test-file* "test/simple.clir")

(caddr (read-file *test-file*))

(get-toplevel-functions (read-file *test-file*))

(car (parse-in-environ *test-file*))

(cadddr (mapcar #'macroexpand-1 (parse-in-environ *test-file*)))

(mapcar #'eval (parse-in-environ *test-file*))

(macroexpand (cadr (macroexpand-1 '(vcgen.rt.core::generate-goals))))

;; VCGEN.RT.CORE:INT is unbound
;; (macroexpand '(VCGEN.RT.CORE::GENERATE-GOAL CLIR.VC::test-define))


;; (eval (macroexpand-1 '(VCGEN.RT.CORE::GENERATE-GOAL CLIR.VC::test-define)))


(values *entry-points*)


