{ mkDerivation, base, conduit, containers, di-core, exceptions, lib
, monad-control, mtl, pipes, pipes-safe, primitive, resourcet, stm
, streaming, transformers, transformers-base, unliftio-core
}:
mkDerivation {
  pname = "di-monad";
  version = "1.3.6";
  src = ./.;
  libraryHaskellDepends = [
    base conduit containers di-core exceptions monad-control mtl pipes
    pipes-safe primitive resourcet stm streaming transformers
    transformers-base unliftio-core
  ];
  homepage = "https://github.com/k0001/di";
  description = "mtl flavoured typeful hierarchical structured logging for di-core";
  license = lib.licenses.bsd3;
}
