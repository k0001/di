{ mkDerivation, base, containers, df1, di-core, di-df1, di-handle
, di-monad, exceptions, lib
}:
mkDerivation {
  pname = "di";
  version = "1.3";
  src = ./.;
  libraryHaskellDepends = [
    base containers df1 di-core di-df1 di-handle di-monad exceptions
  ];
  homepage = "https://github.com/k0001/di";
  description = "Typeful hierarchical structured logging using di, mtl and df1";
  license = lib.licenses.bsd3;
}
