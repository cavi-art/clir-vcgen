;; CLIR Core. Written in less than 200 lines of Common Lisp.

(cl:in-package :cl-user)
(defpackage :ir.rt
  (:documentation "Runtime package. Defines no symbols a priori."))

(defpackage :ir.core.impl
  (:use :cl)
  (:export :assertion :get-package-symbol :assertion-decl-to-code :signature-to-typedecl
	   :lambda-list-type-decls :maybe-macroexpand))

(defpackage :ir.core
  (:use :ir.core.impl)
  (:import-from :cl :nil :t)
  (:import-from :cl &allow-other-keys &body &key &rest)
  (:import-from :cl :let*)
  (:import-from :cl :declare :optimize :speed :debug :safety)
  (:import-from :cl :require :the :type)
  (:import-from :cl :defmacro :let* :cons :car :cdr :length :if :eq "=" :symbolp :first :and :or :nconc :append :consp )
  (:import-from :cl :list :funcall)

  (:export :list)
  (:export :int)
  (:export :bool :true :false)
  (:export :load :assertion :declare :var :the :type :optimize :speed :debug :safety)
  (:export :*assume-verified* :*verify-only*)
  (:export :define :lettype :letvar :letconst :let :let* :letfun :case "@"))

(in-package :ir.core.impl)
;;;; Package ir.core.impl follows.

;; Declare that "assertion" is a valid "declare" form.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (declaration assertion)))

(defparameter *auto-macroexpand* t)

(defun get-package-symbol (input-package-symbol)
  (return-from get-package-symbol input-package-symbol)
  (flet ((get-package-name (input-package-symbol)
	   (symbol-name input-package-symbol)))
    (intern (get-package-name input-package-symbol) "IR.RT")))

(defun assertion-decl-to-code (body-forms)
  (if (assoc 'declare body-forms)
      (let ((declarations (cdr (assoc 'declare body-forms))))
	(declare (cl:ignore declarations))
	body-forms
	)
      body-forms))

(defun maybe-macroexpand (forms)
  (if *auto-macroexpand*
      (mapcar #'macroexpand-1 forms)
      forms))

(cl:defun lambda-list-type-decls (typed-lambda-list)
  (cl:mapcar
   (cl:lambda (e)
     (list 'type (cadr e) (car e)))
   typed-lambda-list))

;;;; Package ir.core must define vars before overriding package in cl-user.
(in-package :ir.core)

(cl:deftype int () `(cl:integer ,cl:most-negative-fixnum ,cl:most-positive-fixnum))
(cl:deftype bool () '(cl:member true false))

(cl:defparameter *assume-verified* nil)
(cl:defparameter *verify-only* nil)

;; Override CL-USER environment to define package (CLIR entry point)
(cl:in-package :cl-user)

(defmacro verification-unit (package-id &key sources uses documentation verify-only assume-verified)
  (declare (ignorable sources))
  (let ((pkg (ir.core.impl:get-package-symbol package-id)))
      `(progn (when (find-package ,pkg)
		(unuse-package ,@uses ,pkg)
		(delete-package ,pkg))
	      (defpackage ,pkg
		 (:use ,@uses)
		 (:documentation ,documentation))
	    (in-package ,pkg)
	    (mapcar (lambda (f) (push f ir.core:*assume-verified*)) ,assume-verified)
	    (mapcar (lambda (f) (push f ir.core:*verify-only*)) ,verify-only))))

;;;; Rest of ir.core follows. 
(cl:in-package :ir.core)

(cl:defmacro var (symbol)
  symbol)


(cl:defmacro lettype (type-symbol param-list type-boolean-expresssion optional-data)
  (declare (cl:ignore optional-data))
  "Defines a type globally in the environment."
  `(cl:deftype ,type-symbol ,param-list ,type-boolean-expresssion)
  ;; TODO: Use `optional-data'
  )


(cl:defmacro define (function-name typed-lambda-list result-lambda-list &body full-body)
  "Defines an exportable function in the verification unit. The
function may be mutually recursive with other DEFINE-d functions."
  (declare (ignore result-lambda-list))
  (cl:let ((body
	       (assertion-decl-to-code full-body)))
    `(cl:defun ,function-name ,(cl:mapcar #'cl:car typed-lambda-list)
       (declare ,@(lambda-list-type-decls typed-lambda-list))
       ,@ (maybe-macroexpand body))))


(cl:defmacro letfun (function-decls &body body)
  "Defines a lexically bound set of possibly mutually-recursive
functions."
  (cl:assert (= 1 (length body)))
  `(cl:labels
       ,(cl:mapcar (cl:lambda (f)
		     (let ((function-name (car f))
			   (function-lambda-list (cl:mapcar #'car (cl:cadr f)))
			   (return-type (caddr f))
			   (function-body (cdddr f)))
			 (cons function-name function-lambda-list (maybe-macroexpand function-body))))
		   function-decls)
     ,@(maybe-macroexpand body)))

(cl:defmacro let (typed-var-list val &body body)
  "Lexically binds a var, syntax is: (let var val body-form). It can
destructure tuples as (let (a b) (list a b) a)"
  (cl:assert (= 1 (length body)))
  (cl:if (= 1 (length typed-var-list))
	 `(let ,(car typed-var-list) ,val ,@(maybe-macroexpand body))
	 (cl:if (and (= 2 (length typed-var-list))
		     (symbolp (first typed-var-list)))

		(cl:destructuring-bind
		      (var-name var-type) typed-var-list
		  `(cl:let ((,var-name ,val))
		       (declare (type ,var-type ,var-name))
		     ,@ (maybe-macroexpand body)))
		
		;; TODO Correctly treat constructor application
		(cl:labels
		    ((strip-var-types (typed-var-list)
		       "Strips variable types from a let-pattern (more
or less, a simple destructuring lambda list)"
		       (cl:if (consp (car typed-var-list))
			      (cons (strip-var-types (car typed-var-list))
				    (strip-var-types (cdr typed-var-list)))
			      (car typed-var-list)))
		     (get-type-for-decl (typed-var-list)
		       (cl:reduce #'get-type-for-decl-acc typed-var-list))

		     (get-type-for-decl-acc (typed-var-list acc)
		       (cl:if (consp (car typed-var-list))
			      (nconc (get-type-for-decl typed-var-list) acc)
			      (cons 'type typed-var-list))))
		  `(cl:destructuring-bind ,(strip-var-types typed-var-list) ,val
		     (declare ,@(get-type-for-decl typed-var-list))
		     ,@ (maybe-macroexpand body))))))

(cl:defmacro case (condition &body cases)
  "Defines a case conditional."
  ;; TODO The cases may be destructuring
  `(cl:case
       ,condition
     ,@ (cl:mapcar
	 (cl:lambda (c)
	   (cl:destructuring-bind
		 (pattern form) c
	     (list pattern (car (maybe-macroexpand (list form)))))) cases)))


(cl:defmacro @@ (cname &rest args)
    "Substitutes the @ function application form for the appropriate
executable funcall."
    (if (eq fname :external)
	`(funcall #'call-external ,@args)
	`(funcall #',cname ,@args)))

(cl:defmacro @ (fname &rest args)
    "Substitutes the @ function application form for the appropriate
executable funcall."
    (if (eq fname :external)
	`(funcall #'call-external ,@args)
	`(funcall #',fname ,@args)))

