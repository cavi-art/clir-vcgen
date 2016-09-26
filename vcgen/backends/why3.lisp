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
(defpackage :ir.vc.backend.why3
  (:use :cl :ir.vc.backend :ir.vc.formulae :ir.vc.theories)
  (:import-from :ir.vc.theories #:find-import-in-theory-db)
  (:import-from :ir.vc.assemble #:protogoals-to-goals)
  (:import-from :ir.vc.backend.why3.formatter #:clir-goal-to-string #:clir-goals-to-string)
  (:import-from :ir.vc.core #:*verification-unit-name* #:*verification-unit-use-list*))
(in-package :ir.vc.backend.why3)

(defvar *why3-parameters* nil)
(defvar *why3-executable* "why3")
(defparameter *why3-executable* #P"~/usr/why3-0.87.1/bin/why3")

(defun why3-parameters ()
  (append
   *why3-parameters*
   (apply #'append
          (loop for db in *enabled-theory-databases*
             collecting
               (list "-L" (princ-to-string (get-theory-directory db)))))))


(defun why3-generate-theory (goal-set stream)
  (let ((goals (clir-goals-to-string (protogoals-to-goals (goal-proof-obligations goal-set))
                                     (goal-name goal-set)))
        (theory-name (format nil "~A_~A"
                             *verification-unit-name*
                             (goal-name goal-set)))
        (why3-imports (remove-if-not
                       #'identity
                       (mapcar
                        #'find-import-in-theory-db
                        *verification-unit-use-list*))))

    (format stream "~@<~2Itheory ~A ~:@_~
~{use import ~A ~^~:@_~}~:@_~:@_~
function arrcopy (v:array int):array int = v~:@_
~{~2I~A~:@_~}~
~:@_end~:@_~:>"
            theory-name
            why3-imports
            goals)))


(defun why3-launch-ide (prover-file)
  (asdf::run-program (append (list *why3-executable* "ide")
                             (why3-parameters)
                             (list (namestring prover-file)))))

(defun why3-launch-solve (prover-file)
  (asdf::run-program (append (list *why3-executable* "prove" "-F" "why")
                             (why3-parameters)
                             (list (namestring prover-file)))))


(defbackend why3
    :description "Why3 is a deductive reasoning tool supported by LRI"
    :url "http://why3.lri.fr"
    :file-extension ".why"
    :theory-generator #'why3-generate-theory
    :launch-ide #'why3-launch-ide
    :launch-noninteractive #'why3-launch-solve)
