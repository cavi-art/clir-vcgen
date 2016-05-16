(defpackage :ir.vc.core
  (:use)

  ;; Runtime directives
  (:import-from :cl :declare :optimize :speed :debug :safety)
  (:export :declare :optimize :speed :debug :safety)

  ;; Logical connections
  (:import-from :cl :and :or)
  (:export :and :or)
  (:export #:->)

  ;; We can use the same `the' and `type' as CL.
  (:export :the :type)

  ;; Our basic macro for defining everything else.
  (:export :verification-unit)

  ;; Basic types
  (:export :int)
  (:export :bool :true :false)

  (:export :*assume-verified* :*verify-only* :*external-functions*)

  ;; Our own DSL keywords
  (:export :assertion :precd :postcd)
  (:export :define :lettype :letvar :letconst :let :let* :letfun :case :default "@" "@@")

  ;; Assertion comparators (will later be in another package)
  (:shadowing-import-from :cl :=)
  (:export :=))

(defpackage :ir.vc.assemble
  (:use :cl :ir.utils)
  (:import-from :ir.vc.core :assertion :precd :postcd :default :*external-functions* :true :false)
  (:import-from :ir.vc.core #:->)
  (:export #:protogoals-to-goals #:merge-protogoal)
  (:export #:synthetic-preconditions #:synthetic-postconditions))


(defpackage :ir.vc.builtins
  (:use :ir.vc.core :common-lisp)
  (:shadow + - * / < <= > >=)
  (:export + - * / < <= > >= =)

  (:shadow :length)
  (:export :length)

  (:export :partition) ; Temporarily here

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
  (:use :cl :ir.utils)
  (:import-from :ir.vc.core :assertion :precd :postcd :default :*external-functions* :true :false)
  (:import-from :ir.vc.core #:->)
  (:export :clir-formula-to-string :clir-premises-to-string :clir-goals-to-string))
