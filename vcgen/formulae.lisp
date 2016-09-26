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
(in-package :ir.vc.formulae)

(defclass premise ()
  ((formula :accessor premise-formula
            :initarg :formula
            :initform nil)
   (comment :accessor premise-comment
            :initarg :comment
            :initform "")
   (name :accessor premise-name
         :initarg :name
         :initform "")
   (metadata :accessor premise-meta
             :initarg :meta
             :initform nil)))

(defstruct goal
  name
  proof-obligations)

(defmethod premise-named-formula ((premise premise))
  (with-slots ((formula formula) (name name)) premise
    (if name
        (list :name name formula)
        formula)))

(defgeneric premise-placeholder (premise)
  (:documentation "Gets the possible placeholder that a premise may have."))

(defmethod premise-placeholder ((premise premise))
  (with-slots ((formula formula)) premise
    (when (consp formula)
      (member (first formula)
              '(:precd_placeholder :postcd_placeholder)))))

(defmethod print-object ((premise premise) stream)
  (with-accessors ((formula premise-formula)
                   (comment premise-comment)
                   (name premise-name)
                   (metadata premise-meta)) premise
    ;; (format stream "#!P(~S ~S ~S ~S)" name comment metadata formula)
    (format stream "~@<~_~I(make-premise ~4I~_:name ~S ~_~
                    ~@[:comment ~S~_ ~]~
                    ~@[:meta ~S~_ ~]~
                    :formula '~S)~:>"
            name
            (unless (string= comment "")
              comment)
            metadata
            formula)))

(defun premise-reader (stream subsubchar arg)
  (declare (ignore subsubchar arg))
  (let ((read-stream (read stream t nil t)))
    (unless read-stream
      (read-line stream)
      (error "Somehow this is nil. Next is ~S" (read-line stream)))
    (destructuring-bind (name comment metadata formula)
        read-stream
      `(make-premise
        :name ,name
        :comment ,comment
        :meta ',metadata
        :formula ',formula))))

(set-sharpsign-exclam-dispatch-character #\P #'premise-reader)


(defun premise-list-p% (elt)
  (typecase elt
    (null t)
    (cons (and (typep (car elt) 'premise)
               (premise-list-p% (cdr elt))))
    (t nil)))
(deftype premise-list () '(and list (satisfies premise-list-p%)))

(defun make-metapremise (&key premises meta)
  (make-instance 'metapremise
                 :premises premises
                 :meta meta))

(defun make-premise (&rest kwargs)
  (apply #'make-instance 'premise kwargs))
