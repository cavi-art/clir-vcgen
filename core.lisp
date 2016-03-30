(cl:in-package :cl-user)

(defpackage :ir.rt
  (:documentation "Runtime package. Defines no symbols a priori."))

(defpackage :ir.core.impl
  (:use :cl)
  (:export :assertion :get-package-symbol :assertion-decl-to-code :signature-to-typedecl))

(defpackage :ir.core
  (:use :ir.core.impl)
  (:import-from :cl :nil :t)
  (:import-from :cl &allow-other-keys &body &key)
  (:import-from :cl :let*)
  (:import-from :cl :declare :optimize :speed :debug :safety)
  (:import-from :cl :require :the :type)
  (:import-from :keyword :compile-toplevel :load-toplevel :execute)

  (:export :int)
  (:export :load :assertion :declare :the :type :optimize :speed :debug :safety)
  (:export :*assume-verified* :*verify-only*)
  (:export :define :lettype :letvar :letconst :let :let* :letfun :case))

(in-package :ir.core.impl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (declaration assertion)))

;;;; Package ir.core.impl follows.

;; Declare that "assertion" is a valid "declare" form.
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

(defun signature-to-typedecl (lambda-list body-forms)
  (return-from signature-to-typedecl body-forms)
  (if (assoc 'declare body-forms)
      (let* ((declarations (cdr (assoc 'declare body-forms)))
	     (signature (assoc 'signature declarations)))
	(if signature
	    (progn
	      ;; (loop
	      ;; 	 for var in signature
	      ;; 	 for param in lambda-list
	      ;; 	 collecting (list var param))
	      ;; (rplacd last-body-form (the t last-body-form))
		   body-forms)
	    body-forms)
	)
      body-forms))


;;;; Package ir.core must define vars before overriding package in cl-user.
(in-package :ir.core)

(cl:deftype int () `(cl:integer ,cl:most-negative-fixnum ,cl:most-positive-fixnum))

(cl:defparameter *assume-verified* nil)
(cl:defparameter *verify-only* nil)

;; Override CL-USER environment to define package (CLIR entry point)
(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
	   (shadow :package :cl-user))
(defmacro package (package-id &key uses documentation verify-only assume-verified)
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

(cl:defmacro lettype (type-symbol param-list type-boolean-expresssion optional-data)
  (declare (cl:ignore optional-data))
  "Defines a type globally in the environment."
  `(cl:deftype ,type-symbol ,param-list ,type-boolean-expresssion)
  ;; TODO: Use `optional-data'
  )

(cl:defmacro letvar (symbol initial-value)
  "Declares a variable globally in the environment."
  `(cl:defparameter ,symbol ,initial-value))

;; TODO Ensure it is constant
(cl:defmacro letconst (symbol value)
  "Defines a constant globally in the environment."
  `(cl:defconstant ,symbol ,value))

(cl:defmacro define (function-name simplified-lambda-list &body full-body)
  "Defines a function globally in the environment. The function may be
recursive."
  (cl:let ((body
	    (assertion-decl-to-code
	     (signature-to-typedecl simplified-lambda-list full-body))))
    `(cl:defun ,function-name ,simplified-lambda-list
       ,@body)))

;; TODO: make let admit tuple in LHS (conditional macro on shape of var)
(cl:defmacro let (var val &body body)
  "Defines a let binding of one var, syntax is:
(let var val
  body-forms+)"
  `(cl:let ((,var ,val))
     ,@body))

(cl:defmacro letfun (function-decls &body body)
  "Defines a set of possibly mutually-recursive functions."
  `(cl:labels ,function-decls
     ,@body))

(cl:defmacro case (condition &body cases)
  "Defines a case conditional"
  `(cl:case ,condition
     ,@cases))

