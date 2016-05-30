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
(defpackage :ir.vc
  (:use :cl :ir.utils :ir.vc.core.impl :ir.vc.assemble :ir.vc.formatter :ir.vc.user)
  (:import-from :ir.vc.core :assertion :precd :postcd :default :*external-functions* :true :false)
  (:import-from :ir.vc.core #:-> :*goal-set-hook*)
  (:export :generate-theory :test-clir)
  (:export :easy-test :easy-protogoals :easy-goals))
(in-package :ir.vc)


(defparameter *clir-extension* ".clir"
  "The extension for source clir files. easy- macros use this
  extension to look for source files.")

(defparameter *prover-extension* ".why"
  "The extension for prover files. The `generate-theory' function uses
  this extension to write the theory files to disk. Some proof
  assistants require certain extensions to work. For example, Why3
  requres the extension \".why\" to be used.")

(defun prover-file-from-clir (path)
  (let* ((name (pathname-name path))
	 (basename (subseq name 0 (find #\. name :from-end t))))
    (merge-pathnames
     (make-pathname :directory (pathname-directory path) :type :unspecific)
     (make-pathname :name (concatenate 'string basename *prover-extension*) :type :unspecific))))

(defun generate-theory (clir-file f)
  "f is a lambda taking zero arguments which returns the goals. It
  should be something like (lambda () (factorial::factorial))."
  ;; TODO Use real imports
  (load-eval-file clir-file)
  (let* ((prover-file (prover-file-from-clir clir-file))
	 (goals (clir-goals-to-string (protogoals-to-goals (funcall f)))))

    (when (probe-file prover-file)
      (delete-file (probe-file prover-file)))

    (with-open-file (stream prover-file :direction :output)
      (format stream "theory UntitledTheory ~% use import int.Int~%
use import int.Fact~% use import array.Array~% use import
array.IntArraySorted~% use import array.ArrayPermut~%~{~A~^~%~} ~%~%end~%"
goals))))

(defun test-clir (clir-file f)
  (generate-theory clir-file f)
  (let ((prover-file (prover-file-from-clir clir-file)))
    (asdf::run-program (list "why3" "ide" (namestring prover-file)))))

(defmacro easy-file (basename)
  "Returns the path to a file in ../test/basename.clir"
  (format nil "../test/~(~A~)~A" (symbol-name basename) *clir-extension*))

(defmacro easy-test (basename function &optional package only-theory)
  "Tests a file. The \"basename\" must be the name of the file without
  the ending .clir (or whatever `*clir-extension*' is set). See the
  code of `easy-file' for more information. The \"function\" is the
  name of the function to test. The package of that function can be
  provided as a package designator in the third parameter. If the
  fourth parameter is set, then the why file gets created but why3 is
  not launched."
  (let ((testing-function (if only-theory
			      'generate-theory
			      'test-clir)))
    `(,testing-function (pathname (easy-file ,basename))
			(lambda () ,(if package
					`(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
					(list function))))))

(defmacro easy-protogoals (basename function &optional package)
  `(progn
     (load-eval-file (pathname (easy-file ,basename)))
     (clir-goals-to-string ,(if package
				`(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
				(list function)))))


;;; How-to test:
;;; To load a file (and inspect the second toplevel sexp):
;; (cadr (load-file (easy-file qsort)))

;;; To throw it to Why3, just put (see `(documentation 'easy-test)')
(easy-test qsort quicksort 'qsort)

;; (load-eval-file (easy-file qsort))

;;; To do special handling on the function call proper, bind the
;;; dynamic variable `*goal-set-hook*' before the call.
;; (let ((*goal-set-hook*  (lambda (protogoals)
;;			  (clir-goals-to-string (protogoals-to-goals protogoals)))))
;;   (qsort::quicksort))

;;; To just show the goals
;; (easy-goals inssort |inssort|::inssort)

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
