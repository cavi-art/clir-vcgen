(define-lemma-db
  :name lemmadb
  :description "Generic Lemma DB"
  :compatibility ((why3))

  :modules ((:name lemmas.arrays ;; This is the VU-name on :uses
             :files ("arrays.why")
             :import "arrays.ArrayLemmas"
             :description "Several additional lemmas on Arrays")))


;; Local Variables:
;; slime-buffer-package: ir\.vc\.theories
;; End:
