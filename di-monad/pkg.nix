{ mkDerivation, base, containers, di-core, exceptions, mtl, pipes
, stdenv, stm, transformers
}:
mkDerivation {
  pname = "di-monad";
  version = "1.3";
  src = ./.;
  libraryHaskellDepends = [
    base containers di-core exceptions mtl pipes stm transformers
  ];
  homepage = "https://github.com/k0001/di";
  description = "mtl flavoured typeful hierarchical structured logging for di-core";
  license = stdenv.lib.licenses.bsd3;
}
