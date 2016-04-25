#!/bin/bash

which () {
    /usr/bin/which "$@"
}

run-lisp-example () {
    if which sbcl >/dev/null 2>&1; then
	run-with-sbcl
	# LISP=sbcl run-with-generic-lisp
    elif which clisp >/dev/null 2>&1; then
	LISP=clisp run-with-generic-lisp
    elif which lisp >/dev/null 2>&1; then
	LISP=lisp run-with-generic-lisp
    else
	echo <<EOF
Could not find a lisp interpreter. Make sure you have it in your PATH
that it is called either lisp or sbcl (or send a patch).
EOF
	
	exit 1
    fi
}

run-with-sbcl () {
    sbcl --load ./runtime/init.lisp \
	 --non-interactive \
	 --eval '(ir.rt.core.impl::execute-clir-file "test/inssort.clir")' \
	 --eval '(print (|inssort|::create-list-and-heap))' \
	 2>/dev/null
}

run-with-generic-lisp () {
    $LISP <<EOF
(load "./runtime/init.lisp")
(ir.rt.core.impl::execute-clir-file "test/inssort.clir")
(|inssort|::create-list-and-heap)
EOF
}


run-lisp-example

