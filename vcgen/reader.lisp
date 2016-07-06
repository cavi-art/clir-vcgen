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

(declaim (optimize debug))
(in-package :ir.vc.reader)

(defparameter *sharpsign-exclam-subordinates* (make-hash-table))
(defvar *sharpsign-exclam-subordinates* (make-hash-table))

(defun |#!-reader| (stream subchar arg)
  (declare (ignore subchar))
  (flet ((err (stream subsubchar arg)
           (declare (ignore stream arg))
           (error "There is no handler set for char ~S" subsubchar)))
    (let ((subsubchar (read-char stream)))
      (let ((dispatch-function (gethash subsubchar *sharpsign-exclam-subordinates* #'err)))
        (funcall dispatch-function stream subsubchar arg)))))

(defun set-sharpsign-exclam-dispatch-character (subsubchar function)
  (setf (gethash subsubchar *sharpsign-exclam-subordinates*) function))

(defun set-#!-dispatch-character (subsubchar function)
  (set-sharpsign-exclam-dispatch-character subsubchar function))

(set-dispatch-macro-character #\# #\! #'|#!-reader|)
