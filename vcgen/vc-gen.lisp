;;; CAVIART-VCGEN - A verification condition generator for the CAVI-ART project
;;; developed originally at GPD UCM.
;;; Copyright (C) 2016 Santiago Saavedra López, Grupo de Programación Declarativa -
;;; Universidad Complutense de Madrid
;;;
;;; This file is part of CAVIART-VCGEN.
;;;
;;; CAVIART-VCGEN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; CAVIART-VCGEN is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with CAVIART-VCGEN.  If not, see <http://www.gnu.org/licenses/>.


(in-package :cl-user)
(declaim (optimize debug))
(defpackage :ir.vc-user
  (:use :cl :ir.vc))
(in-package :ir.vc-user)


;;; How-to test:
;;; To load a file (and inspect the second toplevel sexp):
;; (cadr (read-file (easy-file qsort)))

;;; To throw it to Why3, just put (see `(documentation 'easy-test)')
;; (easy-test qsort quicksort 'qsort)
;; (easy-test qsort partition 'qsort)

;;(cadr (read-file (easy-file qsort)))

;; (easy-test inssort inssort "inssort")
;; (easy-test factorial factorial 'factorial)
;; (load-eval-file (easy-file qsort))

;;; Look at the protogoals in a file
;; (let ((protogoals-file (easy-file qsort ".p.why"))
;;       (theory (easy-protogoals qsort quicksort 'qsort)))
;;   (with-open-file (stream protogoals-file :direction :output)
;;     (format stream "prototheory UntitledTheory~%~A~%end" theory)))

;;; To do special handling on the function call proper, bind the
;;; dynamic variable `*goal-set-hook*' before the call.
;; (let ((*goal-set-hook*  (lambda (protogoals)
;;                        (clir-goals-to-string (protogoals-to-goals protogoals)))))
;;   (qsort::quicksort))

;;; To just show the goals
;; (easy-protogoals inssort inssort "inssort")


;;; When the package name is in lowercase you need to do this:
;; (easy-test inssort inssort "inssort")

;;; Just eval the file without further testing
;;(load-eval-file (easy-file qsort))

;;; Get the synthetic postconditions
;; (clir-goals-to-string (synthetic-postconditions (qsort::quicksort)))

;;; Needed for the next test
;; (easy-test factorial factorial 'factorial)

;;; Launch the VC generation without fancy macros
;; (test-clir (pathname "../test/factorial.clir") (lambda () (factorial::factorial)))
