# To be used as `packageSetConfig` for a Haskell pacakge set:
let
exceptions =
  { mkDerivation, base, fetchgit, mtl, QuickCheck, stdenv, stm
  , template-haskell, test-framework, test-framework-hunit
  , test-framework-quickcheck2, transformers, transformers-compat
  }:
  mkDerivation {
    pname = "exceptions";
    version = "0.10.0";
    src = fetchgit {
      url = "http://github.com/ekmett/exceptions/";
      sha256 = "0pqqc1dkhlki1c1i6zp8nlnf9hg1vmj5hlhjvcp6ad6h3iqwx7pc";
      rev = "14b2fab761abdc5c7a42d96859c5bf0723457a04";
    };
    libraryHaskellDepends = [
      base mtl stm template-haskell transformers transformers-compat
    ];
    testHaskellDepends = [
      base mtl QuickCheck stm template-haskell test-framework
      test-framework-hunit test-framework-quickcheck2 transformers
      transformers-compat
    ];
    homepage = "http://github.com/ekmett/exceptions/";
    description = "Extensible optionally-pure exceptions";
    license = stdenv.lib.licenses.bsd3;
  };
pipes =
  { mkDerivation, base, criterion, exceptions, fetchgit, mmorph, mtl
  , optparse-applicative, QuickCheck, semigroups, stdenv
  , test-framework, test-framework-quickcheck2, transformers, void
  }:
  mkDerivation {
    pname = "pipes";
    version = "4.3.9";
    src = fetchgit {
      url = "https://github.com/Gabriel439/Haskell-Pipes-Library";
      sha256 = "1g655p5ms4n3q0ay54ryr9jqp3nd1jaivnskzgikdpkrqa1rn26c";
      rev = "a89069e9dfcf177fe608c5837bda1eff1a4445f1";
    };
    libraryHaskellDepends = [
      base exceptions mmorph mtl semigroups transformers void
    ];
    testHaskellDepends = [
      base mtl QuickCheck test-framework test-framework-quickcheck2
      transformers
    ];
    benchmarkHaskellDepends = [
      base criterion mtl optparse-applicative transformers
    ];
    description = "Compositional pipelines";
    license = stdenv.lib.licenses.bsd3;
  };

in
self: super:
{
  exceptions = super.callPackage exceptions {};
  pipes = super.callPackage pipes {};
  df1 = super.callPackage ./df1/pkg.nix {};
  di = super.callPackage ./di/pkg.nix {};
  di-core = super.callPackage ./di-core/pkg.nix {};
  di-df1 = super.callPackage ./di-df1/pkg.nix {};
  di-handle = super.callPackage ./di-handle/pkg.nix {};
  di-monad = super.callPackage ./di-monad/pkg.nix {};
}
