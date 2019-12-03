
progs := $(wildcard progs/*.rkt)
execs := $(patsubst %.rkt,%.run,$(wildcard progs/*.rkt))

progs-io := $(wildcard progs-io/*.rkt)
execs-io := $(patsubst %.rkt,%.run,$(wildcard progs-io/*.rkt))
dir-io   := $(wildcard input/*)

%.run: %.rkt
	make -C .. fp-test/$@

compile: $(execs)

compile-io: $(execs-io)

run-io: $(execs-io)
	for d in `ls input`; do for f in input/$$d/*txt; do cat $$f | progs-io/$$d.run; done; done

%.test: %.run
	@echo "$(shell $<)" = "$(shell racket $*.rkt)"

test:

clean:
	-rm progs/*run
	-rm progs-io/*run
