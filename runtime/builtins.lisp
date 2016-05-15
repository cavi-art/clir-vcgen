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


(cl:in-package :cl-user)
(defpackage :ir.rt.builtins
  (:use :ir.rt.core :common-lisp)
  (:shadow + - * / < <= > >= 1+ 1-)
  (:export + - * / < <= > >= 1+ 1-)

  (:shadowing-import-from :common-lisp :let :let* :case :load)

  ;; Export generalized boolean constants
  (:export :t :nil)

  ;; Export our minimal core-lisp for execution
  (:export :format)

  ;; Export types
  (:export :fixnum :number :real :float)

  (:shadow :array)
  (:export :array :length :partition)

  ;; Export simulated heap and array
  (:export :assertion :defun-with-assertion)
  (:export :heap :heap-p :loc :new-heap :sel-heap :mod-heap :newptr-in-heap)
  (:export :sel-array :mod-array :sel-array-heap
	   :mod-array-heap :len-array-heap))

(defpackage :ir.rt.builtins.impl
  (:use :common-lisp)
  (:import-from :ir.rt.builtins :sel-array :mod-array))

(in-package :ir.rt.builtins)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize safety)))

;; PARAMETERS
(defparameter *assertions-executable-p* nil
  "This parameter influences whether assertions are to be executable or not.")

;; Redeclare some CL builtins so that they operate only on fixnums
;; Do not pollute the package with define-op definitions.
(macrolet ((define-op (opname)
	     `(defun ,opname (&rest vals)
		(declare (optimize speed (safety 0)))
		(the fixnum (apply (function ,(find-symbol (symbol-name opname) "COMMON-LISP")) vals))))

	   (boolean-op (opname)
	     (let ((result (gensym)))
	       `(defun ,opname (&rest vals)
		  (declare (optimize speed (safety 0)))
		  (let ((,result (apply (function ,(find-symbol (symbol-name opname) "COMMON-LISP")) vals)))
		    (if ,result
			'true
			'false)))))

	   (define-op1 (opname)
	     `(defun ,opname (val)
		(declare (type fixnum val)
			 (optimize speed (safety 0)))
		(the fixnum (funcall (function ,(find-symbol (symbol-name opname) "COMMON-LISP")) val)))))
  (define-op +)
  (define-op -)
  (define-op *)
  (define-op /)
  (boolean-op <)
  (boolean-op <=)
  (boolean-op >)
  (boolean-op >=)
  (define-op1 1+)
  (define-op1 1-))


;; 
;; Now the assertion basics get here


(defun assert-get (type form)
  (assoc type form))

(defun assert-precd (form) (assert-get 'precd form))
(defun assert-postcd (form) (assert-get 'postcd form))

(eval-when (:compile-toplevel :execute :load-toplevel)
  ;; We need these accessible on compiling so that
  ;; `defun-with-assertion' can be computed in compile-time
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
	  body))))

(defun assert-as-code (assertion &key fname)
  "TODO: Make it work. Now it is just returning t and printing that it should be checked"
  `(prog1 t (format t "[At ~a] Should now be checking ~a~%" ',fname ',assertion)))


(defmacro defun-with-assertion (name args assertion-form &body full-body)
  "This macro is like a defun but it also introduces assertions as
declarations in the function metadata as well as allows executing the
assertions, provided a working implementation of `assert-as-code' for
assertion formulae and a non-nil `*assertions-executable-p*' parameter
value."
  (let ((body (remove-decls full-body))
	(decls (get-decls full-body)))
    (flet ((structured-body-with-postconditions-maybe (body &key postcd)
	     (if (and *assertions-executable-p*
		      postcd)
		 (let ((first-forms (butlast body))
		       (last-form (last body)))
		   (list first-forms
			 `(assert ,(assert-as-code postcd))
			 last-form))
		 body)
	     ))
      `(defun ,name ,args
	 (declare (assertion ,@assertion-form))
	 ,@decls
	 ,@(when (and *assertions-executable-p*
		      (assert-precd assertion-form))
		 `((assert ,(assert-as-code (assert-precd assertion-form)))))

	 ,@(structured-body-with-postconditions-maybe body :postcd (assert-postcd assertion-form))))))


;; Define the builtin functions

(defun heap-p (h)
  "Test whether a given parameter is a heap"
  (assoc :is-heap h))

(deftype heap () '(and list
		   (satisfies heap-p)))

;; For the while the information attached to loc is not really useful,
;; but we should eventually annotate a loc with its associated heap
;; and type check the value at the end.
(deftype loc (a) 'symbol)
(deftype ir.rt.builtins:array (&optional type) `(cl:simple-vector))

(defun get-heap (H L)
  (declare (type heap H)
	   (type (loc (vector fixnum)) L))
  "Dereferences a pointer from a heap"
  (cdr (assoc L H)))

(defun mod-heap (H L V)
  (declare (type heap H)
	   (type (loc (vector fixnum)) L)
	   (type vector V))
  "Modifies a heap `H' so that the position pointed by `L' should
point to `V' in the resulting heap. TODO: should not be fixed to fixnum"
  (cons (cons L V) (remove L H :key #'car)))

(defun newptr-in-heap (H V)
  (declare (type heap H)
	   (type t V))
  "Creates a new pointer in the heap and returns `values' for both the
newly allocated loc and the modified heap with such a pointer pointing
to value `V'"
  (let ((p (gensym)))
    (values (the (loc (array int)) p)
	    (the heap (cons (cons p V) H)))))

(defmacro with-pointer-in-heap (H H2 V &body body)
  (assert (and (typep H2 'atom)
	       (typep V  'atom)))
  `(multiple-value-bind (,V ,H2) (newptr-in-heap ,H nil)
     ,@body))

(defun new-heap ()
  "Creates an empty heap"
  '((:is-heap . t)))

(defun sel-array (E I)
  (declare (type array E)
	   (type fixnum I))
  "Returns a value by index in a modelled array"
  (the fixnum (svref E I)))

(defun mod-array (E I V)
  "Modifies an array `E' so that position `I' now points to `V'"
  (let ((E2 (copy-seq e)))
    (setf (svref E2 I) V)
    (the vector E2)))

(defun sel-array-heap (H V I)
  "Convenience method for selecting an array without dereferencing the
pointer to the heap before."
  (sel-array (get-heap H V) I))

(defun mod-array-heap (H L I V)
  "Convenience method for modifying an array without dereferencing the
pointer to the heap before."
  (the heap (mod-heap H L (mod-array (get-heap H L) I V))))

(defun len-array-heap (H V)
  "Convenience method for getting the length of an array without
dereferencing the pointer to the heap before."
  (the fixnum (length (the array (get-heap H V)))))

;; END OF BUILTIN DECLS

(defun partition (v l r)
  (declare (type (array int) v)
	   (type int l r))
  (let ((p 0))
    (the (values (array int) int)
	 (values v
		 p))))


;; Local Variables:
;; mode: common-lisp
;; coding: utf-8
;; End:
