(cl:in-package :cl-user)
(require "asdf")
(push (directory-namestring *load-pathname*) asdf:*central-registry*)

(cl:in-package :asdf-user)
(oos 'load-op 'clir)

