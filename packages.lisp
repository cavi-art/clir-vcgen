(in-package :cl-user)

(defpackage :cavi-art.ir.builtins
  (:use
   :common-lisp)
  (:shadow + - * / < <= > >= 1+ 1-)
  (:export + - * / < <= > >= 1+ 1-)

  ;; Export generalized boolean constants
  (:export :t :nil)

  ;; Export our minimal core-lisp for execution
  (:export :case :declare :defun :export :format :function :lambda
	   :let :let* :multiple-value-bind :require :the :type)

  ;; Export types
  (:export :fixnum :number :real :float)

  ;; Export simulated heap and array
  (:export :assertion :defun-with-assertion)
  (:export :heap :heap-p :loc :new-heap :sel-heap :mod-heap :newptr-in-heap)
  (:export :sel-array :mod-array :sel-array-heap
	   :mod-array-heap :len-array-heap))

(provide "packages.lisp")
