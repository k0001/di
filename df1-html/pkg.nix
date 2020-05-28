{ mkDerivation, attoparsec, base, bytestring, containers, df1
, QuickCheck, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, time, xmlbf
}:
mkDerivation {
  pname = "df1-html";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers df1 text time xmlbf
  ];
  testHaskellDepends = [
    base containers df1 QuickCheck tasty tasty-hunit tasty-quickcheck
    text time xmlbf
  ];
  homepage = "https://github.com/k0001/di";
  description = "Render and parse df1 logs as HTML";
  license = stdenv.lib.licenses.bsd3;
}
