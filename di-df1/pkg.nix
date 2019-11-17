{ mkDerivation, base, bytestring, df1, di-core, di-handle, di-monad
, QuickCheck, stdenv, stm, tasty, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "di-df1";
  version = "1.2";
  src = ./.;
  libraryHaskellDepends = [
    base df1 di-core di-handle di-monad stm
  ];
  testHaskellDepends = [
    base bytestring df1 di-core QuickCheck tasty tasty-quickcheck text
    time
  ];
  homepage = "https://github.com/k0001/di";
  description = "Write logs in the df1 format using the di logging framework";
  license = stdenv.lib.licenses.bsd3;
}
