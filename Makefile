SANDBOX=$(CURDIR)/cabal-dev
TRACER=$(SANDBOX)/bin/tracer
export TRACER
all: $(TRACER)

.PHONY : init
#TODO make this not need sudo when the variables are already right
#TODO make this fail when variables aren't present
init:
	sudo sysctl kernel.vsyscall64=0
	sudo sysctl kernel.randomize_va_space=0

CD=cabal-dev -s $(SANDBOX)

.PHONY: $(SANDBOX)
$(SANDBOX):
	$(CD) add-source ptrace
	$(CD) add-source trace
	$(CD) add-source .

.PHONY: $(TRACER)
$(TRACER): $(SANDBOX)
	$(CD) install tracer

test: init $(TRACER)
	for test in tests/*; do \
		make -C $$test test; \
	done
	
clean:
	rm -rf $(SANDBOX)
	cd ptrace
	cabal clean
	cd ../trace
	cabal clean
	cd ..
	cabal clean
	for test in tests/*; do \
		make -C $$test clean; \
	done
