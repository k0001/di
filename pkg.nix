{ mkDerivation, base, bytestring, mtl, QuickCheck, stdenv, stm
, tasty, tasty-hunit, tasty-quickcheck, text, time, transformers
, unix
}:
mkDerivation {
  pname = "di";
  version = "0.2";
  src = ./.;
  libraryHaskellDepends = [
    base mtl stm text time transformers unix
  ];
  testHaskellDepends = [
    base bytestring QuickCheck stm tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/di";
  description = "Easy, powerful, structured and typeful logging without monad towers";
  license = stdenv.lib.licenses.bsd3;
}
