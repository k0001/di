{ mkDerivation, attoparsec, base, bytestring, containers, mmorph
, mtl, optparse-applicative, QuickCheck, stdenv, stm, tasty
, tasty-hunit, tasty-quickcheck, text, time, transformers, unix
}:
mkDerivation {
  pname = "di";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers mmorph mtl QuickCheck stm
    text time transformers unix
  ];
  executableHaskellDepends = [
    base optparse-applicative QuickCheck text time
  ];
  testHaskellDepends = [
    base bytestring QuickCheck stm tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/di";
  description = "Easy, powerful, structured and typeful logging without monad towers";
  license = stdenv.lib.licenses.bsd3;
}
