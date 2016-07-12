(defsystem "clir-vcgen"
    :description "Verification Condition Generator for CAVI-ART"
    :version "0.0.1"
    :author "Santiago Saavedra <s.saavedra@fdi.ucm.es>"
    :licence "AGPL3"
    :depends-on (:printv)
    :components ((:file "../cl-reexport")
                 (:file "../utils" :depends-on ("../cl-reexport"))
                 (:file "packages")
                 (:file "reader" :depends-on ("packages"))
                 (:file "formulae" :depends-on ("packages" "../utils" "reader"))
                 (:file "core" :depends-on ("packages" "../utils" "formulae"))
                 (:file "builtins" :depends-on ("packages"  "../utils" "core"))
                 (:file "assemble" :depends-on ("core"))
                 (:file "formatter" :depends-on ("core"))
                 (:file "user" :depends-on ("core"))
                 (:file "theories/fetcher")
                 (:file "vc-gen" :depends-on ("core" "builtins" "formatter" "assemble" "../utils" "user" "theories/fetcher"))
                 ))

