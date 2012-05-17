CFLAGS=-static
all: test
.PHONY: clean
clean:
	rm -f hello.log hello goodbye logger.* emulator.*
.PHONY: test
test: hello goodbye
	$(TRACER) RecordMode hello hello.log
	$(TRACER) TandemMode hello hello
	sh -c '! $(TRACER) TandemMode hello goodbye'
	rm -f logger.* emulator.*
	$(TRACER) -d TandemMode hello hello
	$(COREDIFF) logger*.0 emulator*.0
