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
(require "asdf")
;; (push (directory-namestring *load-pathname*) asdf:*central-registry*)

(defpackage :ir.vc.load
  (:use :cl :asdf))

(cl:in-package :ir.vc.load)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :qlot))

(load "clir-vcgen.asd")
(ql:quickload 'prove)
(qlot:install 'clir-vcgen)
(qlot:quickload 'clir-vcgen)

(defun test ()
  (load "vcgen-test.asd")
  (funcall (eval '(find-symbol (symbol-name '#:run) 'prove))
           'vcgen-test))

