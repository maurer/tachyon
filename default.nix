{ mkDerivation, base, binary, bounded-tchan, bytestring, containers
, process, split, stdenv, stm, trace, transformers, unix
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
}
