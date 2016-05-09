(defsystem "clir-vcgen"
  :description "Verification Condition Generator for CAVI-ART"
  :version "0.0.1"
  :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
  :licence "AGPL3"
  :components ((:file "../cl-reexport")
	       (:file "../utils" :depends-on ("../cl-reexport"))
	       (:file "vc-gen" :depends-on ("../utils"))
	       (:file "builtins" :depends-on ("../utils"))))

