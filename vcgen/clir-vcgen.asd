(defsystem "clir-vcgen"
  :description "Verification Condition Generator for CAVI-ART"
  :version "0.0.1"
  :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
  :licence "AGPL3"
  :depends-on (:alexandria :printv :clir-theories :net.didierverna.clon)
  :components ((:file "../cl-reexport")
               (:file "../utils" :depends-on ("../cl-reexport"))
               (:file "packages")
               (:file "reader" :depends-on ("packages"))
               (:file "formulae" :depends-on ("packages" "../utils" "reader"))
               (:file "core" :depends-on ("packages" "../utils" "formulae"))
               (:file "builtins" :depends-on ("packages"  "../utils" "core"))
               (:file "assemble" :depends-on ("core"))
               (:file "backends")
               (:file "backends/why3-formatter" :depends-on ("core"))
               (:file "backends/why3" :depends-on ("assemble"
                                                   "backends"
                                                   "core"
                                                   "backends/why3-formatter"))
               (:file "facade" :depends-on ("core"
                                            "../utils"
                                            "formulae"
                                            "assemble"
                                            "backends"))
               (:file "vc-gen" :depends-on ("builtins"
                                            "../utils"
                                            "facade"))
               ))
