{ mkDerivation, base, containers, exceptions, QuickCheck, stdenv
, stm, tasty, tasty-hunit, tasty-quickcheck, time
}:
mkDerivation {
  pname = "di-core";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [ base containers exceptions stm time ];
  testHaskellDepends = [
    base exceptions QuickCheck stm tasty tasty-hunit tasty-quickcheck
    time
  ];
  homepage = "https://github.com/k0001/di";
  description = "Typeful hierarchical structured logging without monad towers";
  license = stdenv.lib.licenses.bsd3;
}
