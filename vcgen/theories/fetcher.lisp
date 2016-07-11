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

(in-package :ir.vc.theories)

(defparameter *default-lemma-db* 'lemmadb)
(defvar *lemma-databases* (make-hash-table))
(defvar *enabled-lemma-databases* (list (string *default-lemma-db*)))


(defun toalist% (plist)
  (when plist
    (destructuring-bind (a b . rest) plist
      (cons (cons a b)
            (toalist% rest)))))

(defmacro define-lemma-db (&whole args &key name description compatibility modules &allow-other-keys)
  (declare (ignore description compatibility modules))
  (let ((gname (gensym "NAME")))
    `(let ((,gname (string ',name)))
         (setf (gethash ,gname *lemma-databases*)
               ',(toalist% (cdr args))))))

(defun enable-lemma-db (db-name)
  (pushnew (string db-name) *enabled-lemma-databases*))

(defun disable-lemma-db (db-name)
  (delete (string db-name) *enabled-lemma-databases*))

(defun enable-default-lemma-db ()
  (enable-lemma-db (string *default-lemma-db*)))

(defun find-import-in-lemma-db (pkg)
  (cdr
   (assoc :import
          (find-if (lambda (module)
                     (string= (string pkg) (string (cdr (assoc :name module)))))
                   (apply #'append
                          (mapcar
                           (lambda (lemma-name)
                             (mapcar #'toalist%
                                     (cdr
                                      (assoc :modules
                                             (gethash lemma-name
                                                      *lemma-databases*)))))
                           *enabled-lemma-databases*))))))

(load #P"./theories/lemmadb/database.lisp")
(enable-default-lemma-db)
