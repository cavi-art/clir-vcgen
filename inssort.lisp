(cl:in-package :cl-user)
(require "asdf")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "builtins.lisp")

  (proclaim '(optimize ;; debug
	      ;; speed (safety 0)
	      )))

(defpackage :cavi-art.ir.unit.inssort
  (:use :cavi-art.ir.builtins))
(in-package :cavi-art.ir.unit.inssort)


;; Creator functions
(defun create-list-and-heap ()
  (let ((l '((0 . 1)
	     (1 . 2)
	     (2 . 234)
	     (3 . 23)
	     (4 . 56)
	     (5 . 41))))
    (multiple-value-bind (v h) (newptr-in-heap (new-heap) l)
      (inssort v h))))

(defun f1 (V H)
  (declare (type heap H)
	   (type (loc (array fixnum)) V))
  (let ((I (the fixnum 0)))
    (f2 V I H)))


(defun-with-assertion f2 (V I H)
  ((precd (:forall ((E (array fixnum)))
		   (-> (= (deref H V) E)
		       (and (sorted_sub E 0 I)
			    (<= 0 I)))))
   (postcd (:forall ((E (array fixnum)))
		    (-> (= (deref H V) E)
			(sorted E)))))
  (declare (type (loc (array fixnum)) V)
	   (type heap H)
	   (type fixnum I))
  (let ((X1 (len-array-heap H V)))
    (let ((B (< I X1)))
      (case B
	(break)
	((t) (f3 V I H))
	((nil) H)))))



(defun f3 (V I H)
  (declare (type heap H)
	   (type (loc (array fixnum)) V)
	   (type fixnum I))
  (let ((J (- I (the fixnum 1))))
    (f4 V I J H)))


(defun-with-assertion f4 (V I J H)
  ((precd (:forall ((E (array fixnum)))
		   (-> (= (deref H V) E)
		       (and (sorted_sub E 0 (+ J 1))
			    (sorted_sub E (+ J 1) (+ I 1))
			    (<= -1 J)
			    (<= (sel-array E (- J 1))
				(sel-array E (+ J 1))))))))
  (declare (type heap H)
	   (type (loc (array fixnum)) V)
	   (type fixnum I J))
  (let ((B1 (>= J (the fixnum 0))))
    (case B1
	  ((nil) (f6 V I H))
	  ((t) (let* ((VJ (sel-array-heap H V J))
		      (J1 (+ J (the fixnum 1)))
		      (VJ1 (sel-array-heap H V J1))
		      (B2 (> VJ VJ1)))
		 (case B2
		   ((nil) (f6 V I H))
		   ((t) (f5 V I J H))))))))


(defun f5 (V I J H)
  (declare (type heap H)
	   (type (loc (array fixnum)) V)
	   (type fixnum I J))
  (let* ((E (sel-array-heap H V J))
	 (J1  (+ J (the fixnum 1)))
	 (E2 (sel-array-heap H V J1))
	 (H1 (mod-array-heap H V J E2))
	 (H2 (mod-array-heap H1 V J1 E))
	 (J2 (- J (the fixnum 1))))
    (f4 V I J2 H2)))


(defun f6 (V I H)
  (declare (type heap H)
	   (type (loc (array fixnum)) V)
	   (type fixnum I))
  (let ((I1 (+ I (the fixnum 1))))
    (f2 V I1 H)))


(defun inssort (V H)
  (f1 V H))



;; Local Variables:
;; mode: common-lisp
;; coding: utf-8
;; End:
