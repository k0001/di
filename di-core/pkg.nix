{ mkDerivation, base, containers, QuickCheck, safe-exceptions
, stdenv, stm, tasty, tasty-hunit, tasty-quickcheck, time
}:
mkDerivation {
  pname = "di-core";
  version = "1.0.5";
  src = ./.;
  libraryHaskellDepends = [
    base containers safe-exceptions stm time
  ];
  testHaskellDepends = [
    base QuickCheck safe-exceptions stm tasty tasty-hunit
    tasty-quickcheck time
  ];
  homepage = "https://github.com/k0001/di";
  description = "Typeful hierarchical structured logging without monad towers";
  license = stdenv.lib.licenses.bsd3;
}
