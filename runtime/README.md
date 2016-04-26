CLIR Executable Rewriter
========================

This compiles CAVI-ART project's CLIR to an executable program.


Requirements
============

Needs a Common Lisp compiler with ASDF System Definition support.

Tested with SBCL.


How to use
==========

```bash
cd path/to/here
sbcl --load init.lisp
```
```common-lisp
(in-package :ir.rt.repl)          ;; To have the REPL convenience functions in scope
(eval-clir-file "your-file.clir") ;; Parse and eval file
(|package-name|::my-function)     ;; Just call the function you want to execute in its package
```


Technical documentation
=======================

In order to make the examples portable across different semantics
(i.e., runtime or vc-generation) we are using an approach by which the
verification unit will use an unregistered package (for the base core,
the IR package), and at load time (when loading the example onto the
lisp system) a ~with-throwaway-package~ macro will take care of
defining IR as IR.RT or IR.VC depending on the use case. This way the
verification units need not be rewritten for both cases and both
approaches can be in simultaneous use by the same Lisp System without
any collision.
