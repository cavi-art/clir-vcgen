(defpackage :ir.vc.t.assemble
  (:use :cl
        :ir.utils :ir.vc.formulae
        :ir.vc.backend.why3.formatter
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
                                               'some-f1-postcondition-formula))
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
                                          'after-partition-formula-in-f1)
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
                                          'no-partition-formula)
                            (make-premise :name "something after formula" :formula
                                          'something-after-no-partition)
                            (make-premise :name "postcondition" :formula
                                          '(:postcd_placeholder f1 ((v i j) nil) ((result) nil))))
                           )))
    (let ((post (synthetic-postconditions protogoals))
          (computed-goals (protogoals-to-goals protogoals)))
      (is (length post)
          2
          "There are two different postconditions in this example")
      (is (length computed-goals)
          2
          "There should be two goals in this example"))))


(finalize)

