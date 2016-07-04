
(defpackage :ir.vc.t.vc-gen
  (:use :cl
        :ir.utils :ir.vc.formulae
        :ir.vc.formatter
        :ir.vc.assemble)
  (:use :prove)
  (:import-from :ir.vc.core :define :declare :assertion :precd :postcd :and "@" "=" :tuple))

;; Idempotent renaming to include nickname.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :ir.vc.core :ir.vc.core '(:vc)))

(in-package :ir.vc.t.vc-gen)

;; We define IR-toplevel functions with `GENSYM'-created names so
;; that they do not pollute any namespace at all. We also import most
;; keywords, but beware when using let and case that they have to
;; explicitly refer to the IR ones.

(defun test/multiple-ret-values/simple ()
  (let ((f (gensym)))
    (:printv (eval `(progn
                      (define ,f ((a int)) ((b int) (c int) (d int))
                        (declare (assertion
                                  (precd true)
                                  (postcd (and (@ = a b)
                                               (@ = a c)
                                               (@ = a d)))))
                        (ir.vc.core.impl::terminal-expression (a a a))))))
    (protogoals-to-goals (funcall f))))

(defun test/multiple-ret-values/tuple ()
  (let ((f (gensym)))
    (eval `(define ,f ((a int)) ((b int) (c int) (d int))
             (declare (assertion
                       (precd true)
                       (postcd (and (@ = a b)
                                    (@ = a c)
                                    (@ = a d)))))
             (tuple a a a)))
    (protogoals-to-goals (funcall f))))

(subtest "Testing tuple returns"
  (let ((r (test/multiple-ret-values/simple)))
    (let* ((third-premise (third (first r)))
           (formula (premise-formula third-premise)))
      (is formula '(and (@ = a a) (@ = a a) (@ = a a)) "A function returning multiple
    values handles all its values correctly.")))

  (let ((r (test/multiple-ret-values/tuple)))
    (let* ((third-premise (third (first r)))
           (formula (premise-formula third-premise)))
      (is formula '(and (@ = a a) (@ = a a) (@ = a a)) "A function returning multiple
    values handles all its values correctly when returning them as a bare tuple."))))

;;;;

(subtest "Testing protogoal assembly")

(defun test/assemble/simple ()
  (let ((f (gensym)))
    (eval `(vc:define ,f ((in int)) ((out int))
             (declare (assertion
                       (precd true)
                       (postcd (and (@ = 1 1)
                                    (@ = in out)))))
             (vc:letfun ((f1 ((f1in int)) ((f1out int))
                           f1in))
               (@ f1 in))))
    (funcall f)))

(let ((r (test/assemble/simple)))
  (:printv
   r
   :hr
   (protogoals-to-goals r)))

(let ((protogoals (list
                   (list
                    (make-premise :formula '(:forall (a int)) :name "var intro")
                    (make-premise :formula 'true :name "precondition 2")
                    (make-premise :formula '(:postcd_placeholder f))
                    (make-premise :formula '(@ = a a) :name "1"))
                   (list
                    (make-premise :formula '(:forall (a int)) :name "f var intro")
                    (make-premise :formula '(:precd_placeholder f))
                    (make-premise :formula '(@ = 1 1) :name "inner f")
                    (make-premise :formula '(:postcd_placeholder f))))))
  (protogoals-to-goals protogoals))
