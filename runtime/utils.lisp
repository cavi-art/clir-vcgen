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

(defpackage :ir.utils
  (:use :cl)
  (:export :protect-package :with-changed-package :with-throwaway-package)
  (:export :delete-package-recursive))
(in-package :ir.utils)

(defmacro protect-package (pkg body &body cleanup)
  "Ensures that a package name and nicknames, if any, are preserved
  after the execution of body (but does not impede deletion, as that
  would imply non-portably hooking into `DELETE-PACKAGE'). Executes
  cleanup forms later, but returns body."
  (let ((protected-pkg (gensym))
	(pkg-name (gensym))
	(pkg-nicknames (gensym)))
    `(let ((,protected-pkg ,pkg)
	   (,pkg-name (package-name ,pkg))
	   (,pkg-nicknames (package-nicknames ,pkg)))
       (unwind-protect
	    ,body
	 ,@cleanup
	 (rename-package ,protected-pkg ,pkg-name ,pkg-nicknames)))))

(defmacro protect-current-package (&body body)
  "Runs `protect-package' over the current package ensuring that
`*package*' is unaltered after the evaluation of body."

  (let ((prev-package (gensym)))
    `(let ((,prev-package *package*))
       (protect-package *package*
	   (progn ,@body)
	 (setq *package* ,prev-package)))))

(defmacro with-changed-package (pkg &body body)
  "Changes the current package to pkg ensuring that the previous
package is unaltered (via `protect-current-package')."
  `(protect-current-package
       (setq *package*
	     (if (packagep ,pkg)
		 ,pkg
		 (let ((the-pkg (find-package ,pkg)))
		   (unless the-pkg
		     (error "Package not found ~S" ,pkg))
		   the-pkg)))
     ,@body))

(defmacro with-throwaway-package (uses nicknames &body body)
  "Evaluates content in a throwaway `GENSYM' package, which is unused
from any package that uses it and deleted."
  (let ((pkg (gensym)))
    `(let* ((pkg-name (symbol-name (gensym "THROWPKG")))
	    (,pkg (funcall #'make-package pkg-name :use ',uses :nicknames ',nicknames)))
       (unwind-protect
	    (with-changed-package ,pkg
	      (mapcar #'cl-reexport:reexport-from ',uses)
	      ,@body)
	 (mapcar (lambda (p) (unuse-package ,pkg p))
		 (package-used-by-list ,pkg))
	 (funcall #'delete-package pkg-name)))))
