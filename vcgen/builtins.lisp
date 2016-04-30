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

  ;; Export simulated heap and array
  (:export :assertion :defun-with-assertion)
  (:export :heap :heap-p :loc :new-heap :sel-heap :mod-heap :newptr-in-heap)
  (:export :sel-array :mod-array :sel-array-heap
	   :mod-array-heap :len-array-heap))


