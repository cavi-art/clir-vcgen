(cl:in-package :cl-user)
(proclaim '(optimize debug
	    ;; speed (safety 0)
	    ))

(unless (find-package :ir)
  (defpackage :ir
    (:use :ir.core :ir.builtins)))

(in-package :ir)
(cl-reexport:reexport-from :ir.core)
(cl-reexport:reexport-from :ir.builtins)
