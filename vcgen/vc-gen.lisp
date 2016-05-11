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
(declaim (optimize debug))
(defpackage :vcgen.vc
  (:use :cl :ir.utils :ir.vc.core.impl)
  (:import-from :ir.vc.core :assertion :precd :postcd :default :*external-functions* :true :false)
  ;; (:import-from :ir.vc.core.impl #:*function-list* #:*external-functions*)
  (:export :clir-formula-to-string :clir-premises-to-string)
  (:export :generate-theory :test-clir)
  (:export :easy-test :easy-goals))
(in-package :vcgen.vc)


(defun clir-formula-to-string (formula)
  (labels ((is-infix (op)
	     (and (symbolp op)
		  (member op
			  '(+ - * / < <= >= = > % <>)
			  :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b))))))
	   (clir-term-to-string (term)
	     (typecase term
	       (number term)
	       (symbol term)
	       (string term)
	       (cons (case (car term)
		       (quote (second term))
		       (ir.vc.core:the (third term))
		       (ir.vc.core:@ (apply-predicate (rest term)))
		       (t (error "Term ~S not understood." term))))
	       (t (error "Term ~S not understood." term))))
	   (apply-predicate (p)
	     (if (is-infix (first p))
		 (format nil (format nil "~~{~~A~~^ ~A ~~}"
				     (first p))
			 (mapcar #'clir-term-to-string (rest p)))
		 (format nil "~A(~{~A~^,~})" (first p) (mapcar #'clir-term-to-string (rest p))))))
    (typecase formula
      (symbol formula)
      (string formula)
      (number formula)
      (cons (case (car formula)
	      (:forall (format nil "forall ~:{~A:~A~:^,~}. " (second formula)))
	      (and (format nil "~{~A~^ /\\ ~}" (rest formula)))
	      (or (format nil "~{~A~^ \\/ ~}" (rest formula)))
	      (ir.vc.core.impl::the_postcd_placeholder_for (format nil "POSTCD[~A]" (rest formula)))
	      (ir.vc.core.impl::the_precd_placeholder_for "true(*PRE[~A]*)" (rest formula))
	      (ir.vc.core:@ (apply-predicate (rest formula)))
	      (t (error "Formula ~S not understood. (car=~S)" formula (car formula)))))
      (t (error "Formula ~S not understood." formula)))))

(defun clir-premises-to-string (premises)
  (flet ((not-quantifier (premise)
	   (or (not (consp premise))
	       (not (member (car premise) (list :forall :exists))))))
    (when premises
      (concatenate 'string (format nil "~(~A~)~@[~&~I -> ~]"
				   (clir-formula-to-string (first premises))
				   (and (not-quantifier (first premises))
					(rest premises)))
		   (clir-premises-to-string (rest premises))))))

(defun clir-goal-to-string (goal)
  (destructuring-bind (premise-list target) goal
    (let ((formulae (append premise-list (list target))))
      (format nil "goal ~A: ~A~%"
	      (apply #'concatenate 'string (mapcar #'first formulae))
	      (clir-premises-to-string (mapcar #'second formulae))))))



(defmacro quantified-lambda-params (lambda-params)
  "Adds forall quantifiers for the received typed parameters. This is
  useful for generating goals involving preconditions."
  `'(:forall . ,lambda-params))


(defun load-file (pathname)
  "Loads a file eval'uating package changes, so that identifiers will
get read and `INTERN'-ed on their proper packages."
  (with-throwaway-package (:ir.vc.core :ir.vc.builtins :ir.vc) (:ir)
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

(defun eval-clir-file (pathname)
  (with-throwaway-package (:ir.vc.core :ir.vc.builtins :ir.vc) (:ir)
    (with-open-file (clir-stream pathname)
      (loop for a = (read clir-stream nil)
	 while a
	 collect (eval a)))))



(defparameter *clir-extension* ".clir")
(defparameter *prover-extension* ".why")

(defun prover-file-from-clir (path)
  (let* ((name (pathname-name path))
	 (basename (subseq name 0 (find #\. name :from-end t))))
    (merge-pathnames
     (make-pathname :directory (pathname-directory path) :type :unspecific)
     (make-pathname :name (concatenate 'string basename *prover-extension*) :type :unspecific))))

(defun generate-theory (clir-file f)
  "f is a lambda taking zero arguments which returns the goals. It
  should be something like (lambda () (factorial::factorial))."
  ;; TODO Use real imports
  (eval-clir-file clir-file)
  (let* ((prover-file (prover-file-from-clir clir-file))
	 (goals (mapcar #'clir-goal-to-string (funcall f))))

    (when (probe-file prover-file)
      (delete-file (probe-file prover-file)))

    (with-open-file (stream prover-file :direction :output)
      (format stream "theory UntitledTheory ~% use import int.Int~% use import int.Fact~%~{~A~^~%~} ~%~%end~%" goals))))

(defun test-clir (clir-file f)
  (generate-theory clir-file f)
  (let ((prover-file (prover-file-from-clir clir-file)))
    (asdf::run-program (list "why3" "ide" (namestring prover-file)))))

(defmacro easy-file (basename)
  (format nil "../test/~(~A~).clir" (symbol-name basename)))

(defmacro easy-test (basename function &optional package)
  `(test-clir (pathname (easy-file ,basename))
	      (lambda () ,(if package
			      `(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
			      (list function)))))

(defmacro easy-goals (basename function &optional package)
  `(progn
     (eval-clir-file (pathname (easy-file ,basename)))
     (mapcar #'clir-goal-to-string ,(if package
					`(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
					(list function)))))

;; (cadr (load-file (easy-file qsort)))
;; (mapcar #'clir-goal-to-string (qsort::quicksort))
;; (easy-test qsort qsort::quicksort)
;; (easy-goals inssort |inssort|::inssort)
;; (easy-test inssort |inssort|::inssort)

;; (easy-test factorial factorial::factorial)
;; (test-clir (pathname "../test/factorial.clir") (lambda () (factorial::factorial)))
