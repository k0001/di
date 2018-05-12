{ mkDerivation, base, df1, di, exceptions, optparse-applicative
, pipes, pipes-attoparsec, pipes-bytestring, QuickCheck, stdenv
, transformers
}:
mkDerivation {
  pname = "df1-cli";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base df1 di exceptions optparse-applicative pipes pipes-attoparsec
    pipes-bytestring QuickCheck transformers
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/k0001/di";
  description = "Command-line tools for the df1 logging format";
  license = stdenv.lib.licenses.bsd3;
}
