{ mkDerivation, base, containers, di-core, exceptions, free, lib
, mmorph, monad-control, mtl, pipes-safe, primitive, resourcet, stm
, transformers, transformers-base, unliftio-core
}:
mkDerivation {
  pname = "di-monad";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers di-core exceptions free mmorph monad-control mtl
    pipes-safe primitive resourcet stm transformers transformers-base
    unliftio-core
  ];
  homepage = "https://github.com/k0001/di";
  description = "mtl flavoured typeful hierarchical structured logging for di-core";
  license = lib.licenses.bsd3;
}
