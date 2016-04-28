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
  (:export :assertion :precd :postcd)
  (:export :define :lettype :letvar :letconst :let :let* :letfun :case "@" "@@")

  ;; Assertion comparators (will later be in another package)
  (:shadowing-import-from :cl :=)
  (:export :=))

(defpackage :ir.vc.core.impl
  (:documentation "Functionality for generating Verification Conditions for later use in Why3")
  (:use :cl :ir.utils)
  (:import-from :ir.vc.core :assertion :precd :postcd)
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
	    (verifier-output-comment "Parsing units in package ~a~%" ,package-id))))


(defmacro ir.vc.core:lettype (type-symbol param-list type-boolean-expression optional-data)
  (declare (ignorable type-boolean-expression))
  `(prog1
       (verifier-output 'type ,type-symbol ,param-list)
     (verifier-output-comment-to-string ,optional-data)))

(deftype ir.vc.core:int () `(cl:integer ,cl:most-negative-fixnum ,cl:most-positive-fixnum))
(deftype ir.vc.core:bool () '(cl:member true false))


(defvar *premise-list*)
(defvar *variable-list*)
(defvar *function-list*)

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


(defmacro with-empty-premise-list (&body body)
  `(let ((*premise-list* nil))
     ,@body))

(defmacro with-premise (content &body body)
  `(let ((*premise-list* (cons ,content *premise-list*)))
     ,@body))

(defmacro terminal-expression (expression &key type)
  "We have to do postcd/{result=expression}"
  (@postcd *current-function* :result expression))

(defmacro with-variables (typed-variable-list &body body)
  "Introduces variables in the `premise-list' and `symbol-macrolet's
  those variables so that when they are `macroexpand-1'-ed they will
  be known as variables and the postcondition will be well formed."
  (labels ((symbol-macroletize (typed-var)
	     (destructuring-bind (var-name var-type) typed-var
	       `(,var-name (terminal-expression ',var-name :type ,var-type)))))
    `(let ((*premise-list* (append (list :forall typed-variable-list) *premise-list*)))
       (symbol-macrolet ,(mapcar #'symbol-macroletize typed-variable-list)
	 ,@body))))

(defun rename-symbols (formula original-symbols new-symbols)
  ;; Simple s-exp tree walker
  (typecase formula
    (symbol (let ((p (position formula original-symbols)))
	      (if p
		  (nth p new-symbols)
		  formula)))
    (cons (rename-symbols (car formula) original-symbols new-symbols)
	  (rename-symbols (cdr formula) original-symbols new-symbols))
    (nil nil)))

(defmacro ir.vc.core:define (&whole definition
			       function-name typed-lambda-list result-arg
			     &body full-body)
  "`DEFINE' introduces a set of goals. There should be pre/post
  conditions in this body, and from the expression (likely a letfun)
  we should increase our goal-set until we finally prove the latest
  expression implies the postcondition. For the moment, a define is
  self-contained, meaning definitions inside cannot call another
  define."
  (declare (ignorable result-arg))
  `(defun ,function-name ()
     (with-empty-premise-list
       (with-variables ,typed-lambda-list
	 (with-function-definition ',(cdr definition)
	   (with-current-function ,function-name
	     ,@full-body))))))

(defun @precd (function-name &rest rest)
  (let* ((f (assoc function-name *function-list*))
	 (precd (get-precondition f))
	 (parameter-list (get-typed-lambda-list f)))
    (when precd
      ;; Rename parameters
      (rename-symbols precd parameter-list rest))))

(defmacro ir.vc.core:@ (function-name &rest rest)
  "We will use this macro to call all further functions. Depending
  on whether they already have pre/post conditions on them, we will
  terminate the goal or pursue further by interpreting the next
  function."
  (let ((precd (gensym))
	(postcd (gensym)))
    `(let ((,precd (@precd ,function-name ,@rest))
	   (,postcd (@postcd ,function-name ,@rest)))
       (if ,precd
	   (output-goal (@precd ,function-name ,@rest))
	   (progn
	     ;; Literally macroexpand the result of the function with our arguments
	     (let ((sub-fun-body (rename-symbols (get-function-body function-name) ,@rest)))
	       sub-fun-body)
	     ))

       (if ,postcd
	   (with-premise (@postcd ,function-name ,@rest)
	     (terminal-expression '(@ ,function-name ,@rest)))
	   (terminal-expression '(@ ,function-name ,@rest))))))



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
  (with-throwaway-package (:ir.vc.core :ir.vc) (:ir)
    (with-open-file (clir-stream pathname)
      (loop for a = (read clir-stream nil)
	 while a
	 collect (eval a)))))


;; (defun clir-formula-to-string (formula)
;;   (labels ((quantifier-decl-to-string (formula)
;; 	     (concatenate
;; 	      'string
;; 	      (symbol-name (first formula))
;; 	      " "
;; 	      (typed-var-decl-list-to-string (second formula))
;; 	      ". "
;; 	      (clir-formula-to-string (third formula))))
;; 	   (parentized (formula)
;; 	     (concatenate
;; 	      'string
;; 	      "("
;; 	      (clir-formula-to-string formula)
;; 	      ")")))
    
;;     (if (symbolp formula)
;; 	(symbol-to-external-identifier formula)
;; 	(case (first formula)
;; 	  (:forall (quantifier-decl-to-string formula))
;; 	  (:exists (quantifier-decl-to-string formula))
;; 	  ('-> (concatenate 'string
;; 			    (parentized (second formula))
;; 			    " -> "
;; 			    (parentized (third formula))))
;; 	  ('<-> (concatenate 'string
;; 			     (parentized (second formula))
;; 			     " <-> "
;; 			     (parentized (third formula))))
;; 	  ('= ;; Elements are not formulas!
;; 	   (concatenate
;; 	    'string
;; 	    (clir-term-to-string (second formula))
;; 	    " = "
;; 	    (clir-term-to-string (third formula))))))))


(setf *entry-points* nil)
(defparameter *test-file* "test/simple.clir")

(cadr (load-file *test-file*))

;; (eval-clir-file *test-file*)

;; (setf *entry-points* (get-toplevel-definitions (load-file *test-file*)))





