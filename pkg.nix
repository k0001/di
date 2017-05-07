{ mkDerivation, base, stdenv, stm, text, time, transformers }:
mkDerivation {
  pname = "di";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base stm text time transformers ];
  homepage = "https://github.com/k0001/di";
  description = "Easy and powerful typeful logging without monad towers";
  license = stdenv.lib.licenses.bsd3;
}
