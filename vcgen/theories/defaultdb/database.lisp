(define-theory-db
  :name defaultdb
  :description "Generic Why3 Theories DB"
  :compatibility ((why3))

  :modules ((:name ir.int
                   :files ("numbers.why")
                   :import "numbers.IntegerImports"
                   :description "Integer theory for the prover")
            (:name ir.array
                   :files ("arrays.why")
                   :import "arrays.ArrayImports"
                   :description "Array theory for the prover")
            (:name lemmas.arrays ;; This is the VU-name on :uses
             :files ("arrays.why")
             :import "arrays.ArrayLemmas"
             :description "Several additional lemmas on Arrays")))


;; Local Variables:
;; slime-buffer-package: ir\.vc\.theories
;; End:
