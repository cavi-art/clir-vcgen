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

#|
This file is used to build a binary to interact with our tool.

Requires asdf and quicklisp. This script does the rest, including
requiring the appropriate commands onto the lisp image.

|#

(declaim (optimize (speed 3) (safety 0) (space 3) (debug 0)))
(require "asdf")
(defpackage :ir.vc.binary.load (:use :cl :asdf))
(cl:in-package :ir.vc.binary.load)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :qlot))
(load "clir-vcgen.asd")
(qlot:quickload 'clir-vcgen)

(defpackage :ir.vc.binary
  (:use :cl
        :ir.vc.facade
        :net.didierverna.clon)

  (:export :main))
(in-package :ir.vc.binary)
(defvar *dump-binary* t)

;;; Synopsis for the command line
(defsynopsis (:postfix "FILE ...")
  (text :contents "A CLIR parser to Why3 conditions.")
  (text :contents (format nil "Depending on -a, files have to be ~
  either pairs CLIR-FILE.CLIR CLIR-FILE.WHY or just a list of CLIR ~
  files, and the WHY will be derived from the original names."))
  (group (:header "Options which immediately exit:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit.")
         (flag :long-name "why3-libs"
               :description (format nil "Show the used why3 library ~
               paths to be used with -L in why3 and exit.")))

  (group (:header "Options controlling the output")
         (switch :short-name "a" :long-name "no-auto-file-name"
                 :description (format nil "Choose automatically the ~
                 file name. If an output file is not set, this sets ~
                 the output file to the same as the input file, except ~
                 the extension is replaced. Otherwise, the output is ~
                 sent to stdout."))
         (flag :short-name "s" :long-name "to-stdout"
               :description "Send the output of all files to stdout.")
         (stropt :long-name "override-output-extension"
                 :description "Override the extension to save files with"
                 :fallback-value *output-file-name-extension*))
  (group (:header "Options controlling operations with the resulting file")
         (switch :long-name "why3"
                 :description (format nil "Automatically launch Why3 ~
                 on the generated file(s). This will make ~
                 interaction easier due to the fact that Why3 needs ~
                 to be loaded with a special path for getting ~
                 theories from our database."))))


(defun show-version ()
  (format t "CAVIART-VCGEN is free software: you can redistribute it
and/or modify it under the terms of the GNU Affero General Public
License as published by the Free Software Foundation, either version 3
of the License, or (at your option) any later version.

Version: ~A" (asdf:component-version (asdf:find-system :clir-vcgen))))

(defun main ()
  (make-context)
  (when (getopt :short-name "h")
    (help)
    (exit))
  (when (getopt :short-name "v")
    (show-version)
    (terpri)
    (exit))
  (when (getopt :long-name "why3-libs")
    (format nil "~{~A~^ ~}" (ir.vc.backend.why3::why3-parameters))
    (terpri)
    (exit))

  (let ((ext (getopt :long-name "override-output-extension")))
    (when ext
      (setf *output-file-name-extension* ext)
      (format *error-output* "~&Overriden automatic extension to ~A~%" *output-file-name-extension*)))

  (let ((launch-why3 (getopt :long-name "why3")))
    (in-package :ir.vc.facade)

    (let ((files (remainder)))
      (unless files
        (let ((*output-stream* *standard-output*))
          (clir->why *standard-input* :launch-why3 launch-why3)))
      (when files
        (if (getopt :short-name "s")
            (let ((*output-stream* *standard-output*))
              (clir-batch->why files))
            (clir-batch->multifile-why files
                                       :auto-file-name (getopt :short-name "a")
                                       :launch-why3 launch-why3)))))
  (terpri)
  (exit))


(defmacro trap-all (&body body)
  `(catch 'end-program
     (handler-bind ((t #'(lambda (x)
                           (format *error-output* "A bug happened, it is related to ~S.~%" x)
                           (throw 'end-program nil))))
       ,@body)))

(defun main-launcher ()
  ;; The abort restart is needed in case we drop into the debugger.
  (with-simple-restart (abort "Exit the program.")
    (main)))


(defun main-launcher-nodebug ()
  (with-simple-restart (abort "Exit the program.")
    (trap-all (main))))

(when *dump-binary*
  (dump "vcgen" main-launcher))
