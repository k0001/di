{ mkDerivation, base, df1, di-core, di-df1, di-handle, di-monad
, exceptions, stdenv
}:
mkDerivation {
  pname = "di";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [
    base df1 di-core di-df1 di-handle di-monad exceptions
  ];
  homepage = "https://github.com/k0001/di";
  description = "Typeful hierarchical structured logging using di, mtl and df1";
  license = stdenv.lib.licenses.bsd3;
}
