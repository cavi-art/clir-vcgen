(cl:in-package :cl-user)
(proclaim '(optimize debug
	    ;; speed (safety 0)
	    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ir)
    (defpackage :ir
      (:use :ir.rt.core :ir.rt.builtins))))

(in-package :ir)
(cl-reexport:reexport-from :ir.rt.core)
(cl-reexport:reexport-from :ir.rt.builtins)
