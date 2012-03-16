all: $(SANDBOX)/bin/tracer

.PHONY : init
#TODO make this not need sudo when the variables are already right
#TODO make this fail when variables aren't present
init:
	sudo sysctl kernel.vsyscall64=0
	sudo sysctl kernel.randomize_va_space=0

CD=cabal-dev -s $(SANDBOX)

$(SANDBOX):
	$(CD) add-source ptrace
	$(CD) add-source trace
	$(CD) add-source .

$(SANDBOX)/bin/tracer: $(SANDBOX)
	$(CD) install tracer

test: init $(SANDBOX)/bin/tracer
	
clean:
	rm -rf $(SANDBOX)
	cd ptrace
	cabal clean
	cd ../trace
	cabal clean
	cd ..
	cabal clean
