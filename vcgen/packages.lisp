(defpackage :ir.vc.formulae
  (:use :cl :ir.utils)

  (:import-from :common-lisp #:and #:or)

  (:export #:-> #:<-> #:and #:or #:forall #:exists)
  (:export #:assertion #:precd #:postcd #:true #:false)

  (:export #:make-premise #:premise-formula #:premise-name))


(defpackage :ir.vc.core
  (:use)
  (:documentation "This package is the core IR language. It conatins
  and exports the IR grammar for VC generation.")

  ;; Runtime directives
  (:import-from :cl :declare :optimize :speed :debug :safety)
  (:export :declare :optimize :speed :debug :safety)

  ;; Logical connections
  (:import-from :ir.vc.formulae #:assertion #:precd #:postcd #:true #:false)
  (:import-from :ir.vc.formulae #:-> #:<-> #:and #:or #:forall #:exists)

  ;; Re-export those in formulae
  (:export :assertion :precd :postcd #:true #:false)
  (:export #:-> #:<-> #:and #:or #:forall #:exists)

  ;; We can use the same `the' and `type' as CL.
  (:export :the :type)

  (:export :tuple)

  ;; Our basic macro for defining everything else.
  (:export :verification-unit)

  (:export :predicate)

  ;; Basic types
  (:export :int)
  (:export :bool :true :false)

  (:export :*assume-verified* :*verify-only* :*external-functions* :*goal-set-hook* :*default-goal-set-hook*)

  ;; Our own DSL keywords
  (:export :define :lettype :letvar :letconst :let :let* :letfun :case :default "@" "@@")


  ;; Assertion comparators (will later be in another package)
  (:shadowing-import-from :cl :=)
  (:export :=))

(defpackage :ir.vc.assemble
  (:use :cl :ir.utils :ir.vc.formulae)
  (:import-from :ir.vc.core :assertion :precd :postcd :default :*external-functions* :true :false)
  (:import-from :ir.vc.core #:->)
  (:export #:protogoals-to-goals #:merge-protogoal)
  (:export #:synthetic-preconditions #:synthetic-postconditions))


(defpackage :ir.vc.builtins
  (:use :ir.vc.core :ir.utils :common-lisp)
  (:shadow + - * / < <= > >=)
  (:export + - * / < <= > >= =)

  (:shadow :length)
  (:export :length)

  (:shadowing-import-from :common-lisp :let :let* :case :load)
  (:shadowing-import-from :common-lisp :type :the)

  ;; Export generalized boolean constants
  (:export :t :nil)

  ;; Export our minimal core-lisp for execution
  (:export :format)

  ;; Export types
  (:export #:int #:bool)
  (:export #:true #:false)

  (:export :fixnum :number :real :float)

  ;; Export simulated heap and array
  (:export :heap :heap-p :loc :new-heap :sel-heap :mod-heap :newptr-in-heap)
  (:export :sel-array :mod-array :sel-array-heap
           :mod-array-heap :len-array-heap))

(defpackage :ir.vc.formatter
  (:use :cl :ir.utils :ir.vc.formulae)
  (:import-from :ir.vc.core :assertion :precd :postcd :default :*external-functions* :true :false)
  (:import-from :ir.vc.core #:->)
  (:export :clir-formula-to-string :clir-premises-to-string :clir-goals-to-string))
