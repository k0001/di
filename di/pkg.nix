{ mkDerivation, base, bytestring, QuickCheck, stdenv, stm, tasty
, tasty-hunit, tasty-quickcheck, time, transformers
}:
mkDerivation {
  pname = "di";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [ base stm time transformers ];
  testHaskellDepends = [
    base bytestring QuickCheck stm tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/di";
  description = "Easy, powerful, structured and typeful logging without monad towers";
  license = stdenv.lib.licenses.bsd3;
}
