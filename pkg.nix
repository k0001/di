{ mkDerivation, attoparsec, base, bytestring, containers, free
, lens-family, mmorph, mtl, optparse-applicative, pipes
, pipes-attoparsec, pipes-bytestring, pipes-group, pipes-parse
, QuickCheck, stdenv, stm, tasty, tasty-hunit, tasty-quickcheck
, text, time, transformers, unix
}:
mkDerivation {
  pname = "di";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers free lens-family mmorph mtl
    pipes pipes-attoparsec pipes-bytestring pipes-group pipes-parse
    QuickCheck stm text time transformers unix
  ];
  executableHaskellDepends = [
    attoparsec base optparse-applicative pipes pipes-attoparsec
    pipes-bytestring QuickCheck stm text time transformers
  ];
  testHaskellDepends = [
    base bytestring QuickCheck stm tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/k0001/di";
  description = "Easy, powerful, structured and typeful logging without monad towers";
  license = stdenv.lib.licenses.bsd3;
}
