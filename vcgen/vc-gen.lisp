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
  (:import-from :ir.vc.core #:->)
  ;; (:import-from :ir.vc.core.impl #:*function-list* #:*external-functions*)
  (:export :clir-formula-to-string :clir-premises-to-string)
  (:export :generate-theory :test-clir)
  (:export :easy-test :easy-protogoals :easy-goals))
(in-package :vcgen.vc)


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
	       (cons (case (car term)
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
	      (:forall (format nil "forall ~:{~A:~A~:^,~}. ~A" (second formula) (if (third formula)
										    (clir-formula-to-string (third formula))
										    "")))
	      (-> (format nil "~{(~A)~^ -> ~}" (mapcar #'clir-formula-to-string (rest formula))))
	      (and (format nil "(~{~A~^ /\\ ~})" (mapcar #'clir-formula-to-string (rest formula))))
	      (or (format nil "~{~A~^ \\/ ~}" (mapcar #'clir-formula-to-string (rest formula))))
	      (ir.vc.core.impl::the_postcd_placeholder_for (format nil "true (* POSTCD OF ~A([~A]) *)" (second formula) (rest formula)))
	      (ir.vc.core.impl::the_precd_placeholder_for (format nil "true (* PRECD OF ~A([~A]) *)" (second formula) (rest (rest formula))))
	      (ir.vc.core:@ (apply-predicate (rest formula)))
	      (t (error "Formula ~S not understood. (car=~S)" formula (car formula)))))
      (t (error "Formula ~S not understood." formula)))))

(defun clir-premises-to-string (premises)
  (flet ((not-quantifier (premise)
	   (or (not (consp premise))
	       (not (member (car premise) (list :forall :exists))))))
    (when premises
      (concatenate 'string (format nil "~10<(* ~A *) ~>~(~A~)~@[~&~I -> ~]"
				   (first (first premises))
				   (clir-formula-to-string (second (first premises)))
				   (and (not-quantifier (second (first premises)))
					(rest premises)))
		   (clir-premises-to-string (rest premises))))))

(defparameter *goal-count* 0)
(defun clir-goal-to-string (formulae)
  (format nil "goal ~A_~A: ~A~%"
	  (caadr formulae)
	  (incf *goal-count*)
	  (clir-premises-to-string formulae)))



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
	 (goals (mapcar #'clir-goal-to-string (protogoals-to-goals (funcall f)))))

    (when (probe-file prover-file)
      (delete-file (probe-file prover-file)))

    (with-open-file (stream prover-file :direction :output)
      (format stream "theory UntitledTheory ~% use import int.Int~%
use import int.Fact~% use import array.Array~% use import
array.IntArraySorted~% use import array.ArrayPermut~%~{~A~^~%~} ~%~%end~%"
goals))))

(defun test-clir (clir-file f)
  (generate-theory clir-file f)
  (let ((prover-file (prover-file-from-clir clir-file)))
    (asdf::run-program (list "why3" "ide" (namestring prover-file)))))

(defmacro easy-file (basename)
  (format nil "../test/~(~A~).clir" (symbol-name basename)))

(defmacro easy-test (basename function &optional package only-theory)
  (let ((testing-function (if only-theory
			      'generate-theory
			      'test-clir)))
    `(,testing-function (pathname (easy-file ,basename))
			(lambda () ,(if package
					`(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
					(list function))))))

(defmacro easy-protogoals (basename function &optional package)
  `(progn
     (eval-clir-file (pathname (easy-file ,basename)))
     (mapcar #'clir-goal-to-string ,(if package
					`(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
					(list function)))))



(defun synthetic-precondition-p (protogoal)
  (let ((first-elt (second (second protogoal)))
	(last-elt (second (car (last protogoal)))))
    (and (consp first-elt)
	 (eq (first first-elt)
	     'ir.vc.core.impl::the_precd_placeholder_for)
	 (not (synthetic-postcondition-p protogoal)))))

(defun synthetic-postcondition-p (protogoal)
  (let ((first-elt (second (second protogoal)))
	(last-elt (second (car (last protogoal)))))
    (and
     (consp first-elt)
     (consp last-elt)
     ;; Both are placeholders
     (eq (first first-elt)
	 'ir.vc.core.impl::the_precd_placeholder_for)
     (eq (first last-elt)
	 'ir.vc.core.impl::the_postcd_placeholder_for)

     ;; for the same function
     (eq (second first-elt)
	 (second last-elt)))))

(defun proper-goal-p (protogoal)
  "A goal is proper if it does not contain any hole."
  (and (or
	(not (consp (second (car protogoal))))
	(and
	 (not (eq (first (second (car protogoal)))
		  'ir.vc.core.impl::the_precd_placeholder_for))
	 (not (eq (first (second (car protogoal)))
		  'ir.vc.core.impl::the_postcd_placeholder_for))))
       (or (not (cdr protogoal))
	   (proper-goal-p (cdr protogoal)))))

(defun hole-p (premise)
  (and (consp (second premise))
       (or (eq (first (second premise))
	       'ir.vc.core.impl::the_precd_placeholder_for)
	   (eq (first (second premise))
	       'ir.vc.core.impl::the_postcd_placeholder_for))))

(defun rename-symbol-list (hole real-premise)
  (reduce (lambda (premise current-rename)
	    (ir.vc.core.impl::rename-symbols premise
					     (first current-rename)
					     (second current-rename)))
	  (cddr hole)
	  :initial-value real-premise) ;; TODO Check if renames was a list or the rest of the list
  )

(defun find-all-in-hole-haystack (premise haystack)
  (let ((function-name (second (second premise))))
    function-name haystack
    (or (remove-if-not (lambda (maybe-needle)
			 (eq (second (second (second maybe-needle)))
			     function-name))
		       haystack)
	(list (list premise)))))

(defun has-placeholder (placeholder premise)
  (and (consp (second (second premise)))
       (or (eq (first (second (second premise)))
	       placeholder)
	   (has-placeholder placeholder (cdr premise)))))

(defun remove-first-placeholder (premise)
  (assert (or (not (second premise))
	      (has-placeholder 'ir.vc.core.impl::the_precd_placeholder_for
			       premise)))
  (cons (first premise)
	;; Remove second element which is the first placeholder
	(cddr premise)))

(defun remove-both-placeholders (premise)
  (assert (or (not (second premise))
	     (has-placeholder 'ir.vc.core.impl::the_precd_placeholder_for
			      premise)
	     (has-placeholder 'ir.vc.core.impl::the_postcd_placeholder_for
			      premise)))
  (butlast (cons (first premise)
		 (cddr premise))))


(defun patch-hole (premise pre post)
  (let ((hole-haystack (case (caadr premise)
			 (ir.vc.core.impl::the_precd_placeholder_for pre)
			 (ir.vc.core.impl::the_postcd_placeholder_for post)))
	(removal-function (case (caadr premise)
			    (ir.vc.core.impl::the_precd_placeholder_for #'remove-first-placeholder)
			    (ir.vc.core.impl::the_postcd_placeholder_for #'remove-both-placeholders))))
    ;; (break "Removing hole of type ~A from premise ~A. Got ~A candidates." (caadr premise) premise (length (find-all-in-hole-haystack premise hole-haystack)))
    (mapcar
     #'(lambda (subst)
	 (rename-symbol-list (second premise)
			     (funcall removal-function subst)))
     (find-all-in-hole-haystack premise
				hole-haystack))))

(defun merge-protogoal (synthetic-preconditions synthetic-postconditions)
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


(defun protogoals-to-goals (protogoals)
  (let* ((proper-goals (remove-if-not #'proper-goal-p protogoals))
	 (synthetic-postconditions (remove-if-not #'synthetic-postcondition-p protogoals))
	 (synthetic-preconditions (remove-if-not #'synthetic-precondition-p protogoals))
	 (protogoals-with-holes (remove-if #'synthetic-postcondition-p (remove-if #'proper-goal-p protogoals))))
    (progn
      protogoals
      (append proper-goals
	      (mapcar (merge-protogoal synthetic-preconditions synthetic-postconditions)
		      protogoals-with-holes)))))

;; (cadr (load-file (easy-file qsort)))
;; (mapcar #'clir-goal-to-string (qsort::quicksort))
;; (easy-test qsort qsort::quicksort)
;; (easy-goals inssort |inssort|::inssort)

;; (easy-test inssort inssort "inssort")

;; (cadr (load-file (easy-file qsort)))

;; (reverse (mapcar #'clir-goal-to-string (qsort::quicksort)))

;; (eval-clir-file (easy-file qsort))
;; (easy-test qsort quicksort 'qsort)

;; (defun x ()
;;   (let ((protogoals (qsort::quicksort)))
;;     (let* ((proper-goals (remove-if-not #'proper-goal-p protogoals))
;; 	   (synthetic-postconditions (remove-if-not #'synthetic-postcondition-p protogoals))
;; 	   (synthetic-preconditions (remove-if-not #'synthetic-precondition-p protogoals))
;; 	   (protogoals-with-holes (remove-if #'synthetic-postcondition-p (remove-if #'proper-goal-p protogoals))))
;;       (funcall (merge-protogoal synthetic-preconditions synthetic-postconditions)
;; 	       (car protogoals-with-holes))
;;       (values
;;        protogoals-with-holes
;;        ))))

;; (easy-test factorial factorial::factorial)
;; (test-clir (pathname "../test/factorial.clir") (lambda () (factorial::factorial)))

;; (clir-formula-to-string
;;  (ir.vc.core.impl::get-postcondition (assoc 'ir.vc.builtins:partition *external-functions*)))
