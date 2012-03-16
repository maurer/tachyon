CFLAGS=-static
all: test
.PHONY: clean
clean:
	rm -f hello.log hello goodbye logger.* emulate.*
.PHONY: test
test: hello goodbye
	$(TRACER) RecordMode hello hello.log
	$(TRACER) TandemMode hello hello
	sh -c '! $(TRACER) TandemMode hello goodbye'
