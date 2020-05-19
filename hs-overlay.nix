{ pkgs }:

# To be used as `packageSetConfig` for a Haskell pacakge set:
let
hspec-discover =
  { mkDerivation, base, directory, filepath, hspec-meta, QuickCheck
  , stdenv
  }:
  mkDerivation {
    pname = "hspec-discover";
    version = "2.5.6";
    sha256 = "8c9689b51aa44b8278a5ff3059e0e8a609dce077df3781aad977c647a8c18a46";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [ base directory filepath ];
    executableHaskellDepends = [ base directory filepath ];
    testHaskellDepends = [
      base directory filepath hspec-meta QuickCheck
    ];
    homepage = "http://hspec.github.io/";
    description = "Automatically discover and run Hspec tests";
    license = stdenv.lib.licenses.mit;
  };
hspec =
  { mkDerivation, base, hspec-core, hspec-discover
  , hspec-expectations, QuickCheck, stdenv
  }:
  mkDerivation {
    pname = "hspec";
    version = "2.5.6";
    sha256 = "9ea6eb6ac6b49e1593e272707b760e125d3bdca2d8845d76e116c1ea8112da59";
    libraryHaskellDepends = [
      base hspec-core hspec-discover hspec-expectations QuickCheck
    ];
    homepage = "http://hspec.github.io/";
    description = "A Testing Framework for Haskell";
    license = stdenv.lib.licenses.mit;
  };
hspec-meta =
  { mkDerivation, ansi-terminal, array, base, call-stack, clock
  , deepseq, directory, filepath, hspec-expectations, HUnit
  , QuickCheck, quickcheck-io, random, setenv, stdenv, stm, time
  , transformers
  }:
  mkDerivation {
    pname = "hspec-meta";
    version = "2.5.6";
    sha256 = "440d3f09a9c88f5852fd654cb8f38be458c31bc0a571c612c1911eb899f2cda4";
    revision = "1";
    editedCabalFile = "0c7dq1vvk09fj6nljwwshgpkszg725hrpgnq9l2aka230sig9vz4";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
      ansi-terminal array base call-stack clock deepseq directory
      filepath hspec-expectations HUnit QuickCheck quickcheck-io random
      setenv stm time transformers
    ];
    executableHaskellDepends = [
      ansi-terminal array base call-stack clock deepseq directory
      filepath hspec-expectations HUnit QuickCheck quickcheck-io random
      setenv stm time transformers
    ];
    homepage = "http://hspec.github.io/";
    description = "A version of Hspec which is used to test Hspec itself";
    license = stdenv.lib.licenses.mit;
  };
hspec-core =
  { mkDerivation, ansi-terminal, array, base, call-stack, clock
  , deepseq, directory, filepath, hspec-expectations, hspec-meta
  , HUnit, process, QuickCheck, quickcheck-io, random, setenv
  , silently, stdenv, stm, temporary, tf-random, transformers
  }:
  mkDerivation {
    pname = "hspec-core";
    version = "2.5.6";
    sha256 = "6f188cf2322d0bafca7a9a11feb80a66631bdf6911d236ed16e4f4a22c1e455e";
    libraryHaskellDepends = [
      ansi-terminal array base call-stack clock deepseq directory
      filepath hspec-expectations HUnit QuickCheck quickcheck-io random
      setenv stm tf-random transformers
    ];
    testHaskellDepends = [
      ansi-terminal array base call-stack clock deepseq directory
      filepath hspec-expectations hspec-meta HUnit process QuickCheck
      quickcheck-io random setenv silently stm temporary tf-random
      transformers
    ];
    testTarget = "--test-option=--skip --test-option='Test.Hspec.Core.Runner.hspecResult runs specs in parallel'";
    homepage = "http://hspec.github.io/";
    description = "A Testing Framework for Haskell";
    license = stdenv.lib.licenses.mit;
    doCheck = false;
  };
QuickCheck =
  { mkDerivation, base, containers, deepseq, erf, process, random
  , stdenv, template-haskell, tf-random, transformers
  }:
  mkDerivation {
    pname = "QuickCheck";
    version = "2.12.4";
    sha256 = "c9f6b226b9f890ddf1506d9993c6b490aee66b598761e054f0de2a21b5ec4f5d";
    libraryHaskellDepends = [
      base containers deepseq erf random template-haskell tf-random
      transformers
    ];
    testHaskellDepends = [ base deepseq process ];
    homepage = "https://github.com/nick8325/quickcheck";
    description = "Automatic testing of Haskell programs";
    license = stdenv.lib.licenses.bsd3;
    jailbreak = true;
  };

inherit (pkgs.haskell.lib) doJailbreak;

in
self: super:
{
  # hspec-core = super.callPackage hspec-core {};
  # hspec-discover = super.callPackage hspec-discover {};
  # hspec-meta = super.callPackage hspec-meta {};
  # hspec = super.callPackage hspec {};
  # optparse-applicative = doJailbreak super.optparse-applicative;
  # QuickCheck = super.callPackage QuickCheck {};
  # safe-exceptions = doJailbreak super.safe-exceptions;

  df1 = super.callPackage ./df1/pkg.nix {};
  di = super.callPackage ./di/pkg.nix {};
  di-core = super.callPackage ./di-core/pkg.nix {};
  di-df1 = super.callPackage ./di-df1/pkg.nix {};
  di-handle = super.callPackage ./di-handle/pkg.nix {};
  di-monad = super.callPackage ./di-monad/pkg.nix {};

  _shell = self.shellFor {
    withHoogle = false; # hoogle dependencies don't compile
    packages = p: [
      p.df1
      p.di
      p.di-core
      p.di-df1
      p.di-handle
      p.di-monad
    ];
  };
}
