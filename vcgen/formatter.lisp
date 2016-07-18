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
(in-package :ir.vc.formatter)

(defun clir-formula-to-string (formula)
  (labels ((is-infix (op)
             (and (symbolp op)
                  (member op
                          '(+ - * / < <= >= = > % <>)
                          :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b))))))
           (clir-term-to-string (term &optional recursive)
             (typecase term
               (number term)
               (symbol term)
               (string term)
               (cons (case (first term)
                       (quote (second term))
                       (ir.vc.core:the (third term))
                       (ir.vc.core:@ (apply-predicate (rest term) recursive))
                       (t (error "Term ~S not understood." term))))
               (t (error "Term ~S not understood." term))))
           (is-array-access (fname)
             (string-equal (symbol-name fname)
                           :get))
           (apply-predicate (p &optional recursive)
             (let ((predicate-str
                    (if (is-infix (first p))
                        (format nil (format nil "~~{(~~A)~~^ ~A ~~}"
                                            (first p))
                                (mapcar (lambda (x) (clir-term-to-string x t)) (rest p)))
                        (if (is-array-access (first p))
                            (format nil "~A[~A]" (second p) (third p))
                            (format nil "~A ~{~A~^ ~}" (first p) (mapcar (lambda (x) (clir-term-to-string x t)) (rest p)))))))
               (if recursive
                   (format nil "(~A)" predicate-str)
                   predicate-str))))
    (typecase formula
      (symbol formula)
      (string formula)
      (number formula)
      (cons (case (car formula)
              (forall (format nil "forall ~:{~A:~A~:^,~}. ~A" (second formula) (if (third formula)
                                                                                    (clir-formula-to-string (third formula))
                                                                                    "")))
              (-> (format nil "~{(~A)~^ -> ~}" (mapcar #'clir-formula-to-string (rest formula))))
              (and (format nil "(~{~A~^ /\\ ~})" (mapcar #'clir-formula-to-string (rest formula))))
              (or (format nil "~{~A~^ \\/ ~}" (mapcar #'clir-formula-to-string (rest formula))))
              (:name (format nil "(~10< \"~A\" ~> ~A)" (second formula) (clir-formula-to-string (third formula))))
              (:postcd_placeholder (format nil "true (* POSTCD OF ~A([~A]) *)" (second formula) (rest formula)))
              (:precd_placeholder (format nil "true (* PRECD OF ~A([~A]) *)" (second formula) (rest (rest formula))))
              (ir.vc.core:@ (apply-predicate (rest formula)))
              (t (error "Formula ~S not understood. (car=~S)" formula (car formula)))))
      (t (error "Formula ~S not understood." formula)))))

(defun clir-premises-to-string (premises)
  (flet ((not-quantifier (premise)
           (or (not (consp premise))
               (and
                (and (eq (first premise) :name)
                     (not (member (second premise) (list 'forall 'exists))))
                (not (member (first premise) (list 'forall 'exists)))))))
    (when premises
      (concatenate 'string (format nil "~(~A~)~@[~&~I -> ~]"
                                   (clir-formula-to-string (premise-formula (first premises)))
                                   (and (not-quantifier (premise-formula (first premises)))
                                        (rest premises)))
                   (clir-premises-to-string (rest premises))))))


(defun get-premise-name (premise-list)
  (declare (ignore premise-list))
  "name_not_implemented")

(defun clir-goals-to-string (goals &optional function-name)
  (let ((goal-count 0))
    (mapcar (lambda (premises)
              (format nil "goal ~A_~A: ~A~%"
                      (or function-name
                          (get-premise-name premises))
                      (incf goal-count)
                      (clir-premises-to-string premises)))
            goals)))

(defun clir-goal-to-string (goals function-name)
   (format nil "goal ~A: ~{(**)(~A)(**) ~^ /\\ ~}~%"
           function-name
           (mapcar #'clir-premises-to-string goals)))
