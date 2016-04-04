BUILDAPP=buildapp

SBCL=sbcl --noinform --no-userinit --non-interactive

BUILDAPP=buildapp

QUICKLISP_HOME=$(HOME)/quicklisp

QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp

all: funweb

deps.txt: funweb.asd
	$(QUICKLISP) --load funweb.asd --eval '(ql:quickload "funweb")'
	$(QUICKLISP) --eval '(format t "(:quicklisp-version ~S)~%" (ql:dist-version "quicklisp"))' > deps.txt

manifest.txt: deps.txt
	$(QUICKLISP) --eval '(ql:write-asdf-manifest-file "manifest.txt")'

funweb: manifest.txt
	$(BUILDAPP) --output funweb \
		--asdf-path . \
		--load-system funweb-cli \
		--manifest-file manifest.txt \
		--entry funweb::cli

clean:
	rm -f manifest.txt deps.txt funweb

