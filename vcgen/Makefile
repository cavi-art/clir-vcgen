LISP ?= sbcl
ARCH ?= $(shell uname -p)

vcgen: *.lisp quicklisp
	ros run -L $(LISP) -l binary.lisp

.PHONY: clir2mlw-tagged
vcgen-tagged: clir2mlw
	mv vcgen vcgen-linux-$(LISP)-$(ARCH)

.PHONY: test
test:
	ros run -L $(LISP) -l test.lisp -q

quicklisp: qlfile qlfile.lock
	./autogen-quicklisp.ros
	touch quicklisp


.PHONY: update-quicklisp
update-quicklisp: qlfile qlfile.lock
	./autogen-quicklisp.ros --update
