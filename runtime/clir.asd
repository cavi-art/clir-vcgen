(defsystem "clir"
  :description "Common Lisp-like Intermediate Representation for CAVI-ART"
  :version "0.0.1"
  :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
  :licence "GPL3"
  :components ((:file "cl-reexport")
	       (:file "core")
	       (:file "builtins" :depends-on ("core"))
	       (:file "rt" :depends-on ("cl-reexport" "core" "builtins"))
	       ))
