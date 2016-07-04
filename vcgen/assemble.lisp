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
  (labels ((merge-it (protogoal)
             (when protogoal

               (if (hole-p (car protogoal))

                   ;; Patch-hole returns the list of premises for that
                   ;; hole, so we need to use `APPEND' instead of just
                   ;; `CONS'ing.
                   (append
                    ;; (list (car protogoal))
                    (apply #'append (patch-hole (car protogoal) synthetic-preconditions synthetic-postconditions))
                    (merge-it (cdr protogoal)))

                   (cons (car protogoal)
                         (merge-it (cdr protogoal)))))))
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
      (append
       proper-goals
       (mapcar (merge-protogoal synthetic-preconditions synthetic-postconditions)
               protogoals-with-holes)))))



;;; Implementation details

(defun synthetic-precondition-p (protogoal)
  (let ((first-elt (premise-formula (second protogoal))))
    (and (consp first-elt)
         (eq (first first-elt)
             :precd_placeholder)
         (not (synthetic-postcondition-p protogoal)))))

(defun synthetic-postcondition-p (protogoal)
  (let ((first-elt (premise-formula (second protogoal)))
        (last-elt (premise-formula (car (last protogoal)))))
    (and
     (consp first-elt)
     (consp last-elt)
     ;; Both are placeholders
     (eq (first first-elt)
         :precd_placeholder)
     (eq (first last-elt)
         :postcd_placeholder)

     ;; for the same function
     (eq (second first-elt)
         (second last-elt)))))

(defun proper-goal-p (protogoal)
  "A goal is proper if it does not contain any hole."
  (and (or
        (not (consp (premise-formula (car protogoal))))
        (and
         (not (eq (first (premise-formula (car protogoal)))
                  :precd_placeholder))
         (not (eq (first (premise-formula (car protogoal)))
                  :postcd_placeholder))))
       (or (not (cdr protogoal))
           (proper-goal-p (cdr protogoal)))))

(defun hole-p (premise)
  (and (consp (premise-formula premise))
       (or (eq (first (premise-formula premise))
               :precd_placeholder)
           (eq (first (premise-formula premise))
               :postcd_placeholder))))

(defun rename-symbol-list (hole real-premise)
  (reduce (lambda (premise current-rename)
            (ir.vc.core.impl::rename-symbols premise
                                             (first current-rename)
                                             (second current-rename)))
          (cddr hole)
          :initial-value real-premise) ;; TODO Check if renames was a list or the rest of the list
  )

(defun find-all-in-hole-haystack (premise haystack)
  (let ((function-name (second (premise-formula premise))))
    function-name haystack
    (or (remove-if-not (lambda (maybe-needle)
                         (eq (second (premise-formula (second maybe-needle)))
                             function-name))
                       haystack)
        (list (list premise)))))

(defun has-placeholder (placeholder premise)
  (and (consp (premise-formula (second premise)))
       (or (eq (first (premise-formula (second premise)))
               placeholder)
           (has-placeholder placeholder (cdr premise)))))

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
  (let ((hole-haystack (case (first (premise-formula premise))
                         (:precd_placeholder pre)
                         (:postcd_placeholder post)))
        (removal-function (case (first (premise-formula premise))
                            (:precd_placeholder #'remove-first-placeholder)
                            (:postcd_placeholder #'remove-both-placeholders))))
    ;; (break "Removing hole of type ~A from premise ~A. Got ~A candidates." (caadr premise) premise (length (find-all-in-hole-haystack premise hole-haystack)))
    (mapcar
     #'(lambda (subst)
         (rename-symbol-list (premise-formula premise)
                             (funcall removal-function subst)))
     (find-all-in-hole-haystack premise
                                hole-haystack))))
