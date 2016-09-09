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


(defpackage :ir.vc.facade
  (:use :common-lisp :ir.utils :ir.vc.backend)
  (:nicknames :ir.vc)
  (:export #:load-eval-file #:read-file #:generate-theory-file #:test-clir
           #:easy-file #:easy-test #:easy-protogoals))

(cl:in-package :ir.vc.facade)

(defun load-eval-file (pathname)
  "Reads, loads and evals a CLIR file in the VCGEN semantics."
  (with-throwaway-package (:ir.vc.core :ir.vc.builtins) (:ir)
                          (with-open-file (clir-stream pathname)
                            (loop for a = (read clir-stream nil)
                               while a
                               collect (eval a)))))

(defun read-file (pathname)
  "Loads a file eval'uating package changes, so that identifiers will
get read and `INTERN'-ed on their proper packages."
  (with-throwaway-package (:ir.vc.core :ir.vc.builtins) (:ir)
                          ;; We need to use the IR package so that we import the
                          ;; verification-unit construct in order to `EVAL' it on the `LOOP'
                          ;; to make the new package definition.
                          (with-open-file (clir-stream pathname)
                            (loop
                               for a = (read clir-stream nil)
                               while a
                               if (and (consp a)
                                       (symbolp (car a))
                                       (string-equal (symbol-name (car a))
                                                     "verification-unit"))
                               collect (progn (eval a) a)
                               else
                               collect a))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *clir-extension* ".clir"
    "The extension for source clir files. easy- macros use this
  extension to look for source files."))


(defun generate-theory-file (clir-file closure)
  (load-eval-file clir-file)
  (let* ((prover-file (clir-pathspec-to-backend clir-file))
         (existing-file (probe-file prover-file)))
    (when existing-file
      (delete-file existing-file))

    (with-open-file (stream prover-file :direction :output)
      (generate-theory (funcall closure) stream))))


(defun test-clir (clir-file f)
  (generate-theory-file clir-file f)
  (launch-ide (clir-pathspec-to-backend clir-file)))

(defmacro easy-file (basename &optional (extension *clir-extension*))
  "Returns the path to a file in ../test/basename.clir"
  (truename
   (merge-pathnames
    (make-pathname :directory (list :relative :up "test")
                   :name (format nil "~(~A~)~A" (symbol-name basename) extension)
                   :type :unspecific))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun easy-funcall% (function package)
    (if package
        `(funcall (find-symbol ,(symbol-name function) (find-package ,package)))
        (list function))))

(defmacro easy-test (basename function &optional package only-theory)
  "Tests a file. The \"basename\" must be the name of the file without
  the ending .clir (or whatever `*clir-extension*' is set). See the
  code of `easy-file' for more information. The \"function\" is the
  name of the function to test. The package of that function can be
  provided as a package designator in the third parameter. If the
  fourth parameter is set, then the why file gets created but why3 is
  not launched."
  (let ((testing-function (if only-theory
                              'generate-theory-file
                              'test-clir)))
    `(,testing-function (pathname (easy-file ,basename))
                        (lambda () ,(easy-funcall% function package)))))


(defmacro easy-protogoals (basename function &optional package)
  "This is a debugging facility which does not produce automatically
consumable output."
  `(progn
     (load-eval-file (pathname (easy-file ,basename)))
     (ir.vc.backend.why3.formatter::clir-goals-to-string ,(easy-funcall% function package))))
