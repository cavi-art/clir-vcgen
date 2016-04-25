;;; Copyright (C) 2016 Santiago Saavedra López
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

;; These are several definitions for making the editing experience
;; with emacs more pleasant.

(defun slime-clir-buffer-package ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)"))
	(regex2 (concat "^(\\(\\)verification-unit\\>[ \t']*"
			"\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regex2 nil t)
		(re-search-backward regexp nil t)
		(re-search-forward regexp nil t)
		)
        (match-string-no-properties 2)))))

(eval-after-load "slime"
  '(slime-setup '(slime-asdf)))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

(setq slime-find-buffer-package-function #'slime-repl-buffer-package)
(setq slime-find-buffer-package-function #'slime-clir-buffer-package)

(put 'letfun 'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 &lambda 4 &body)) &body))

(put 'let-cl 'common-lisp-indent-function (get 'let 'common-lisp-indent-function))

(put 'let 'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 &lambda 2 &body)) 6 &body))

(put 'define 'common-lisp-indent-function
     '(4 &lambda &lambda &body))
