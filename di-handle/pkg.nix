{ mkDerivation, base, bytestring, di-core, exceptions, stdenv, unix
}:
mkDerivation {
  pname = "di-handle";
  version = "1.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring di-core exceptions unix
  ];
  homepage = "https://github.com/k0001/di";
  description = "IO support for file handles in di-core";
  license = stdenv.lib.licenses.bsd3;
}
