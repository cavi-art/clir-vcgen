(defpackage :ir.vc.t.assemble
  (:use :cl
        :ir.utils :ir.vc.formulae
        :ir.vc.formatter
        :ir.vc.assemble)
  (:use :prove)
  (:import-from :ir.vc.core :define :declare :assertion :precd :postcd :and "@" "=" :tuple))

;; Idempotent renaming to include nickname.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :ir.vc.core :ir.vc.core '(:vc)))

(in-package :ir.vc.t.assemble)

(plan 1)

;;;;;;;;;;
(subtest "Multiple postcondition test"
  (let* ((one (make-premise :name nil :formula
                            '(forall
                              ((v (array vc:int)) (i vc:int)
                               (j vc:int)))))
         
         (two (make-premise :name "precondition" :formula
                            '(and (@ <= (vc:the vc:int 0) i j)
                              (@ < j (@ length v)))))
         (protogoals (list (list one two
                                 (make-premise :name nil :formula
                                               '(forall ((vsort (array vc:int)))))
                                 (make-premise :name "f1 postcondition" :formula
                                               '(:postcd_placeholder f1 ((v i j) (v i j)) ((result) (vsort))))
                                 (make-premise :name "postcondition" :formula
                                               '(and
                                                 (@ sorted_sub vsort i
                                                  (@ + (vc:the vc:int 1) j))
                                                 (@ permut_sub v vsort i
                                                  (@ + (vc:the vc:int 1) j)))))
                           (list
                            (make-premise :name nil :formula
                                          '(forall
                                            ((v (array vc:int)) (i vc:int)
                                             (j vc:int))))
                            (make-premise :name "f1_" :formula
                                          '(:precd_placeholder f1 ((v i j) nil)))
                            (make-premise :name "something-before-partition-hole" :formula
                                          '(forall ((v1 (array vc:int)) (p vc:int))))
                            (make-premise :name "partition hole begin" :formula
                                          '(:precd_placeholder partition (nil (v i j))))
                            (make-premise :name "partition hole end" :formula
                                          '(:postcd_placeholder partition (nil (v i j)) (nil (v1 p))))
                            (make-premise :name "something after partition" :formula
                                          '(and
                                            (@ sorted_sub result p2
                                             (@ + (vc:the vc:int 1) j))
                                            (@ permut_sub v2 result p2
                                             (@ + (vc:the vc:int 1) j))))
                            (make-premise :name "postcondition" :formula
                                          '(:postcd_placeholder f1 ((v i j) nil) ((result) nil))))
                           (list
                            (make-premise :name nil :formula
                                          '(forall
                                            ((v (array vc:int)) (i vc:int)
                                             (j vc:int))))
                            (make-premise :name "f1_" :formula
                                          '(:precd_placeholder f1 ((v i j) nil)))
                            (make-premise :name "other formula with no partition" :formula
                                          '(forall ((v1 (array vc:int)) (p vc:int))))
                            (make-premise :name "something after formula" :formula
                                          '(and
                                            (@ sorted_sub result p2
                                             (@ + (vc:the vc:int 1) j))
                                            (@ permut_sub v2 result p2
                                             (@ + (vc:the vc:int 1) j))))
                            (make-premise :name "postcondition" :formula
                                          '(:postcd_placeholder f1 ((v i j) nil) ((result) nil))))
                           )))
    (let ((post (synthetic-postconditions protogoals))
          (computed-goals (protogoals-to-goals protogoals)))
      (is (length computed-goals)
          2
          "There should two goals in this example")
      (is (length post)
          2
          "There are two different postconditions in this example"))))



(finalize)
