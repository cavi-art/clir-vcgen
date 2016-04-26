(defsystem "clir"
  :description "Common Lisp-like Intermediate Representation for CAVI-ART"
  :version "0.0.1"
  :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
  :licence "AGPL3"
  :components ((:file "cl-reexport")
	       (:file "utils" :depends-on ("cl-reexport"))
	       (:file "core" :depends-on ("utils"))
	       (:file "builtins" :depends-on ("core"))))
