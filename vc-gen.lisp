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

(defpackage :ir.vc.core
  (:use)

  ;; Runtime directives
  (:import-from :cl :declare :optimize :speed :debug :safety)
  (:export :declare :optimize :speed :debug :safety)

  ;; Logical connections
  (:import-from :cl :and :or)
  (:export :and :or)

  ;; We can use the same `the' and `type' as CL.
  (:import-from :cl :the :type)
  (:export :the :type)

  ;; Our basic macro for defining everything else.
  (:export :verification-unit)

  ;; Basic types
  (:export :int)
  (:export :bool :true :false)

  (:export :*assume-verified* :*verify-only*)

  ;; Our own DSL keywords
  (:export :assertion :define :lettype :letvar :letconst :let :let* :letfun :case "@" "@@"))


(defpackage :ir.vc.core.impl
  (:documentation "Functionality for generating Verification Conditions for later use in Why3")
  (:use :cl :ir.utils)
  (:export :verifier-identifier :verifier-output :verifier-output-comment *entry-points*
	   :get-decls))

(in-package :ir.vc.core.impl)

(defun get-decls (body &optional decls)
  "Gets the `declare'-d and docstring forms (if there are any) of a
defun-ish body"
  (let ((form (car body)))
    (if (or (and (listp form)
		 (eq (car form)
		     'declare))
	    (stringp form))
	(get-decls (cdr body)
		   (cons form decls))
	(values (reverse decls) body))))


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
  (format t "(* ~s *)~%"
	  (with-output-to-string (s)
	    (apply #'format s forms))))


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


(defparameter *entry-points* nil)


;;;; Grammar follows 
(defmacro ir.rt.core:verification-unit (package-id &key sources uses documentation verify-only assume-verified)
  (declare (ignorable sources))
)


(defmacro ir.vc.core:verification-unit (package-id &key sources uses documentation verify-only assume-verified)
  (declare (ignorable sources uses documentation verify-only assume-verified))
  (let ((pkg (ir.rt.core.impl:get-package-symbol package-id)))
      `(progn (when (find-package ,pkg)
		(unuse-package ',@uses ,pkg)
		(delete-package ,pkg))
	      (defpackage ,pkg
		 (:use ,@uses)
		 (:documentation ,documentation))
	    (in-package ,pkg)
	    (cl:mapcar (cl:lambda (f) (cl:push f ir.rt.core:*assume-verified*)) ,assume-verified)
	    (cl:mapcar (cl:lambda (f) (cl:push f ir.rt.core:*verify-only*)) ,verify-only)
	    (verifier-output-comment "Parsing units in package ~a~%" ,package-id))))

(defmacro ir.vc.core:lettype (type-symbol param-list type-boolean-expression optional-data)
  (declare (ignorable type-boolean-expression))
  `(prog1
       (verifier-output 'type ,type-symbol ,param-list)
     (verifier-output-comment-to-string ,optional-data)))

(deftype ir.vc.core:int () `(cl:integer ,cl:most-negative-fixnum ,cl:most-positive-fixnum))
(deftype ir.vc.core:bool () '(cl:member true false))

(defmacro ir.vc.core:define (function-name typed-lambda-list result-arg &body full-body)
  "`DEFINE' introduces a set of goals. There should be pre/post
  conditions in this body, and from the expression (likely a letfun)
  we should increase our goal-set until we finally prove the latest
  expression implies the postcondition. Additionally, the definitions
  in this define may call definitions in another define, so the goals
  cannot be checked inline by macroexpansion."
  (declare (ignorable function-name typed-lambda-list result-arg full-body))
  (push (list function-name typed-lambda-list result-arg full-body) *entry-points*))

(defmacro ir.vc.core:@ (function-name &rest rest)
  "We will use this macro to call all further functions. Depending
  on whether they already have pre/post conditions on them, we will
  terminate the goal or pursue further by interpreting the next
  function."
  (declare (ignorable function-name rest)))



;;;; End of grammar. 


(defmacro quantified-lambda-params (lambda-params)
  "Adds forall quantifiers for the received typed parameters. This is
  useful for generating goals involving preconditions."
  `'(:forall . ,lambda-params))


(defun load-file (pathname)
  "Loads a file eval'uating package changes, so that identifiers will
get read and `INTERN'-ed on their proper packages."
  (with-throwaway-package (:ir.vc.core :ir.vc) (:ir)
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
  (with-throwaway-package (:ir.rt) (:ir)
    (with-open-file (clir-stream pathname)
      (loop for a = (read clir-stream nil)
	 while a
	 collect (eval a)))))

(defun formula-of (body)
  ;; Ideally this would be (macroexpand-1 body)
  nil ;; No idea
  )


(defun verify-function (symbol)
  (destructuring-bind (typed-lambda-list typed-result-list &rest body)
      (cdr (assoc symbol *entry-points*))
    (let ((precd (get-precondition body))
	  (postcd (get-postcondition body)))
      (verifier-output 'axiom precd)

      (mapcar (lambda (param)
		(let ((param-name (car param))
		      (param-type (cadr param))))
		(verifier-output 'constant param-name ': param-type))
	      typed-lambda-list)

      (with-empty-goal-set
	  (with-new-goal
	      (formula-of body)
	    (implies postcd))))))



(setf *entry-points* nil)
(defparameter *test-file* "test/simple.clir")

(cadr (load-file *test-file*))

(eval-clir-file *test-file*)

(setf *entry-points* (get-toplevel-definitions (load-file *test-file*)))


(macroexpand (cadr (macroexpand-1 '(generate-goals))))

;; VCGEN.RT.CORE:INT is unbound
;; (macroexpand '(VCGEN.RT.CORE::GENERATE-GOAL CLIR.VC::test-define))


;; (eval (macroexpand-1 '(VCGEN.RT.CORE::GENERATE-GOAL CLIR.VC::test-define)))


(values *entry-points*)


