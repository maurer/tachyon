{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  ptrace = pkgs.haskell.packages.${compiler}.callPackage ./ptrace {};
  trace  = pkgs.haskell.packages.${compiler}.callPackage ./trace {};

  f = { mkDerivation, base, binary, bounded-tchan, bytestring
      , containers, process, split, stdenv, stm, trace, transformers
      , unix
      }:
      mkDerivation {
        pname = "tachyon";
        version = "0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          base binary bounded-tchan bytestring containers process split stm
          trace transformers unix
        ];
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {trace = trace;};

in

  if pkgs.lib.inNixShell then drv.env else drv
