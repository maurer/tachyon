# Tachyon
Tachyon is a system for providing record, replay, and tandem execution by forwarding one process's system calls to the other.

This is still research quality code and is not extraordinarily stable.

## Building

Tachyon's non-hackage dependencies are pointed to by git submodules.
To pull them down, run
```
git submodule init
git submodule update
```

If you have [Nix](http://nixos.org) installed, you should just be able to run `nix-build shell.nix` to build.
Otherwise, descend into each of `ptrace` and `trace`, running `cabal install` in each, then run `cabal install` in the root directory.

## Usage

The Tachyon package provides two utilities: `tracer` and `corediff`.

### Tracer
`tracer` can be invoked in 3 different modes: Record, Replay, and Tandem.

```
tracer RecordMode executable logfile
```
will record the stream of system calls made by `executable` to `logfile`.

Similarly,
```
tracer ReplayMode executable logfile
```
will execute `executable` using `logfile` as its system call stream source.

Finally,
```
tracer TandemMode exe1 exe2
```
will run `exe1` and `exe2` simultaneously, attempting to rewrite the system call from `exe1` into something that `exe2` can use.

### Corediff
`corediff` allows you to diff cores generated when running the `tracer` process.
This is primarily of interest for debugging in the case that replay is not proceeding as expected.

## Caveats
This code is somewhat brittle, you may need to update or add features to get it to work on your particular kernel version/libc version etc.
A good start if you notice unexpected deviations is to run `corediff` on cores you expect to be equal, and try to determine if any differences detected are problematic.

Threading is implemented by forcing system calls to interleave the same way on subsequent runs.
This does not actually induce determinism, though it does come close.
If you want to operate on threaded binaries in any kind of stable environment, I suggest you combine Tachyon with some sort of memory determinizer (e.g. [dthreads](http://www.cs.umass.edu/~emery/pubs/dthreads-sosp11.pdf)).

If you have a specific problem, feel free to file an issue, and I'll try to look into it.
However, this is not my current project, so any support is on a strictly best effort basis.
