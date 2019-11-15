{ mkDerivation, attoparsec, base, bytestring, containers
, QuickCheck, stdenv, tasty, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "df1";
  version = "0.3.2";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers text time
  ];
  testHaskellDepends = [
    attoparsec base bytestring QuickCheck tasty tasty-quickcheck text
    time
  ];
  homepage = "https://github.com/k0001/di";
  description = "Type, render and parse the df1 hierarchical structured log format";
  license = stdenv.lib.licenses.bsd3;
}
