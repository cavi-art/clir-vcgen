(defsystem vcgen-test
  :depends-on ("clir-vcgen"
               :prove
               :printv)
  :defsystem-depends-on (:prove-asdf)
  :components
  ((:test-file "t/vc-gen-test")
   (:test-file "t/assemble-test"))
  :perform (test-op :after (op c) (funcall (intern #.(string :run) :prove) c)))
