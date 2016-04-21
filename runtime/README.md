CLIR Executable Rewriter
========================

This compiles CAVI-ART project's CLIR to an executable program.


Requirements
============

Needs a Common Lisp compiler with ASDF System Definition support.

Tested with SBCL.


How to use
==========

    $ cd path/to/here
    $ sbcl --load init.lisp
    > (load "your-file.clir")
    > (|package-name|::my-function) ;; Just call the function you want to execute in its package


