TARGETS=hunchentoot-test ideal-test wookie-test

all: $(TARGETS)

hunchentoot-test: hunchentoot.lisp
	./compile-sbcl-lisp.sh $< $@

ideal-test: ideal.lisp
	./compile-sbcl-lisp.sh $< $@

wookie-test: wookie.lisp
	./compile-sbcl-lisp.sh $< $@

clean:
	$(RM) $(TARGETS) *.fasl
