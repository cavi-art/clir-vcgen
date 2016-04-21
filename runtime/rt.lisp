(cl:in-package :cl-user)
(proclaim '(optimize debug
	    ;; speed (safety 0)
	    ))

(print *load-pathname*)
(print (directory-namestring *load-pathname*))
(print (pathname (directory-namestring *load-pathname*)))

(load (merge-pathnames (directory-namestring *load-pathname*) "cl-reexport.lisp"))
(load (merge-pathnames (directory-namestring *load-pathname*) "core.lisp"))
(load (merge-pathnames (directory-namestring *load-pathname*) "builtins.lisp"))
(unless (find-package :ir)
  (defpackage :ir
    (:use :ir.core :ir.builtins)))

(in-package :ir)
(cl-reexport:reexport-from :ir.core)
(cl-reexport:reexport-from :ir.builtins)
