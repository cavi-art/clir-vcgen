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
(defpackage :ir.vc.backend
  (:use :cl :ir.utils)
  (:export #:*vc-backend*)
  (:export #:defbackend)
  (:export #:clir-pathspec-to-backend #:launch-ide #:launch-noninteractive
           #:generate-theory))
(in-package :ir.vc.backend)

(defvar *vc-backends* (make-hash-table :test #'equal))
(defvar *vc-backend* 'why3)

(defun toalist% (plist)
  (when plist
    (destructuring-bind (a b . rest) plist
      (cons (cons a b)
            (toalist% rest)))))

(defstruct backend
  "A backend for representing CLIR proof obligations in order to prove them."
  (name nil :type (or string symbol))
  (description nil :type (or nil string))
  (url nil :type (or nil string))
  (file-extension ".dat" :type (or string pathname))
  (theory-generator nil :type function)
  (launch-ide nil :type function)
  (launch-noninteractive nil :type function))

(defmacro defbackend (name &rest args)
  `(setf (gethash ',(string name) *vc-backends*)
         (apply #'make-backend :name ',(string name) ,(cons 'list args))))

(defun find-backend (backend-designator)
  (let ((backend (gethash (string backend-designator) *vc-backends*)))
    (unless backend
      (error "Unknown backend ~S" backend-designator))
    (the backend backend)))

(defun set-backend (name)
  (let ((backend (gethash (string name) *vc-backends*)))
    (unless backend
      (error "Unknown backend ~S" name))
    (setf *vc-backend* backend)))

(defun clir-pathspec-to-backend (path &optional (backend *vc-backend*))
  (let ((prover-extension (backend-file-extension (find-backend backend))))
    (let* ((name (pathname-name path))
           (dir (pathname-directory path))
           (basename (subseq name 0 (find #\. name :from-end t)))
           (prover-file-name (concatenate 'string
                                          basename
                                          prover-extension)))
      (merge-pathnames
       (make-pathname :directory dir :type :unspecific)
       (make-pathname :name prover-file-name :type :unspecific)))))

(defun generate-theory (goal-set stream &optional (backend *vc-backend*))
  (funcall (backend-theory-generator (find-backend backend))
           goal-set
           stream))

(defun launch-ide (prover-file &optional (backend *vc-backend*))
  (funcall (backend-launch-ide (find-backend backend))
           prover-file))

(defun launch-noninteractive (prover-file &optional (backend *vc-backend*))
  (funcall (backend-launch-noninteractive (find-backend backend))
           prover-file))

