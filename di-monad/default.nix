{ mkDerivation, base, containers, di-core, exceptions, lib, mtl
, pipes, stm, streaming, transformers, unliftio-core
}:
mkDerivation {
  pname = "di-monad";
  version = "1.3.2";
  src = ./.;
  libraryHaskellDepends = [
    base containers di-core exceptions mtl pipes stm streaming
    transformers unliftio-core
  ];
  homepage = "https://github.com/k0001/di";
  description = "mtl flavoured typeful hierarchical structured logging for di-core";
  license = lib.licenses.bsd3;
}
