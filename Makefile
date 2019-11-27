
progs := $(wildcard progs/*.rkt)
execs := $(patsubst %.rkt,%.run,$(wildcard progs/*.rkt))

%.run: %.rkt
	make -C .. fp-test/$@

compile: $(execs)

%.test: %.run
	@echo "$(shell $<)" = "$(shell racket $*.rkt)"

test:

clean:
	-rm progs/*run
