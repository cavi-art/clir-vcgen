(cl:in-package :cl-user)
(proclaim '(optimize debug
	    ;; speed (safety 0)
	    ))

(print *load-pathname*)
(print (directory-namestring *load-pathname*))
(print (pathname (directory-namestring *load-pathname*)))

(unless (find-package :ir)
  (defpackage :ir
    (:use :ir.core :ir.builtins)))

(in-package :ir)
(cl-reexport:reexport-from :ir.core)
(cl-reexport:reexport-from :ir.builtins)
