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
(in-package :ir.vc.assemble)


;;; Public interface

(defun merge-protogoal (synthetic-preconditions synthetic-postconditions)
  "Merges a protogoal with all the possible preconditions and
postconditions previously synthetized. Returns a closure with the
pre/postconditions so that this function result can be used in a
`mapcar' of the goals."
  (labels ((find-premise-possibilities (premise)
             (if (hole-p premise)
                 (patch-hole premise synthetic-preconditions synthetic-postconditions)
                 (list (list premise))))
           (find-goal-possibilities (protogoal)
             (mapcar #'find-premise-possibilities protogoal))
           (cartesian-possibilities (protogoal-sublists)
             ;; We depend on alexandria for this
             (mapcar (lambda (l) (apply #'append l))
                     (apply #'alexandria:map-product #'list protogoal-sublists)))
           (merge-it (protogoal)
             (cartesian-possibilities (find-goal-possibilities protogoal))))
    #'merge-it))

(defun synthetic-preconditions (protogoals)
  "Returns all the protogoals which corrrespond to synthetic
preconditions. (Need to be cleaned from placeholders before actual
use). See `remove-first-placeholder'"
  (remove-if-not #'synthetic-precondition-p protogoals))

(defun synthetic-postconditions (protogoals)
  "Returns all the protogoals which corrrespond to synthetic
postconditions. (Need to be cleaned from placeholders before actual
use). See `remove-both-placeholders'"
  (remove-if-not #'synthetic-postcondition-p protogoals))


(defun protogoals-to-goals (protogoals)
  "Parses the protogoals so that it returns proper goals, either
because the protogoals already were, or due to appending the
synthetized pre/post conditions to them. The synthetic constructs
themselves are not output.

CAVEAT: Currently this function does not cycle through the synthetized
conditions to see whether they may also have placeholders. This will
require cycle detection and is expected in an upcoming version of this
program."
  (let* ((proper-goals (remove-if-not #'proper-goal-p protogoals))
         (synthetic-postconditions (synthetic-postconditions protogoals))
         (synthetic-preconditions (synthetic-preconditions protogoals))
         (protogoals-with-holes (remove-if #'synthetic-postcondition-p
                                           (remove-if #'proper-goal-p protogoals))))
    (progn
      (apply #'append
             proper-goals
             (mapcar (merge-protogoal synthetic-preconditions synthetic-postconditions)
                     protogoals-with-holes)))))



;;; Implementation details

(defun synthetic-precondition-p (protogoal)
  (let ((first-elt (second protogoal)))
    (and (eq (first (premise-placeholder first-elt))
             :precd_placeholder)
         (not (synthetic-postcondition-p protogoal)))))

(defun synthetic-postcondition-p (protogoal)
  (let ((first-placeholder (premise-placeholder (second protogoal)))
        (last-placeholder (premise-placeholder (car (last protogoal)))))
    (and (eq (first first-placeholder) :precd_placeholder)
         (eq (first last-placeholder) :postcd_placeholder)
         (eq (second first-placeholder)
             (second last-placeholder)))))

(defun proper-goal-p (protogoal)
  "A goal is proper if it does not contain any hole."
  (or (not protogoal)
      (and (not (hole-p (car protogoal)))
           (proper-goal-p (cdr protogoal)))))

(defun hole-p (premise)
  (premise-placeholder premise))

(defun rename-symbol-list (hole real-premise)
  (if (eq (first real-premise)
          :name)

      (list :name
            (second real-premise)
            (rename-symbol-list hole (third real-premise)))

      (reduce (lambda (premise current-rename)
                (ir.vc.core.impl::rename-symbols premise
                                                 (first current-rename)
                                                 (second current-rename)))
              (cddr hole)
              :initial-value real-premise)) ;; TODO Check if renames was a list or the rest of the list
  )

(defun find-all-in-hole-haystack (premise haystack)
  (let ((function-name (second (premise-named-formula premise))))
    (or (remove-if-not (lambda (maybe-needle)
                         (eq (second (premise-named-formula (second maybe-needle)))
                             function-name))
                       haystack)
        (list (list premise)))))

(defun has-placeholder (placeholder premise)
  (or (eq (first (premise-placeholder (car premise)))
          placeholder)
      (has-placeholder placeholder (cdr premise))))

(defun remove-first-placeholder (premise)
  (assert (or (not (second premise))
              (has-placeholder :precd_placeholder
                               premise)))
  (cons (first premise)
        ;; Remove second element which is the first placeholder
        (cddr premise)))

(defun remove-both-placeholders (premise)
  (assert (or (not (second premise))
              (has-placeholder :precd_placeholder
                               premise)
              (has-placeholder :postcd_placeholder
                               premise)))
  (butlast (cddr premise)))

(defun patch-hole (premise pre post)
  (let ((hole-haystack (case (first (premise-placeholder premise))
                         (:precd_placeholder pre)
                         (:postcd_placeholder post)))
        (removal-function (case (first (premise-placeholder premise))
                            (:precd_placeholder #'remove-first-placeholder)
                            (:postcd_placeholder #'remove-both-placeholders))))
    (mapcar
     #'(lambda (subst)
         (rename-symbol-list (premise-named-formula premise)
                             (funcall removal-function subst)))
     (find-all-in-hole-haystack premise
                                hole-haystack))))
