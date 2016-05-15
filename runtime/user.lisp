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


(defpackage :ir.rt.user
  (:use :common-lisp :ir.utils)
  (:export :load-eval-file :read-file))

(cl:in-package :ir.rt.user)

(defun load-eval-file (pathname)
  "Reads, loads and evals a CLIR file in the executable semantics."
  (with-throwaway-package (:ir.rt.core :ir.rt.builtins) (:ir)
    (with-open-file (clir-stream pathname)
      (loop for a = (read clir-stream nil)
	 while a
	 collect (eval a)))))

(defun read-file (pathname)
  "Loads a file eval'uating package changes, so that identifiers will
get read and `INTERN'-ed on their proper packages."
  (with-throwaway-package (:ir.rt.core :ir.rt.builtins) (:ir)
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
