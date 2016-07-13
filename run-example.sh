#!/bin/bash
#|-*- mode:lisp -*-|#
#| Launches a test run of verifying quicksort

if ! /usr/bin/which ros >/dev/null 2>&1; then
    echo "Could not find roswell installed in your PATH."
    echo "Roswell (ros) is REQUIRED to use this project."
    echo
    echo "You can find more information on how to install it at:"
    echo
    echo "https://github.com/roswell/roswell/wiki/1.-Installation"
    exit 1
fi

exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (setf *default-pathname-defaults* (merge-pathnames #P"vcgen/"))
  (load #P"init.lisp"))

(defpackage :ros.script.ir.vc.run-example
  (:use :cl :ir.vc.user))
(in-package :ros.script.ir.vc.run-example)

(defun main (&rest argv)
  (declare (ignorable argv))
  (easy-test qsort quicksort 'qsort))
;;; vim: set ft=lisp lisp:
