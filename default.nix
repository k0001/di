{ nixpkgs ? import ./nixpkgs-src.nix
, pkgs ? import nixpkgs {}
}:

let

hs = pkgs.haskell.lib;

packageSetConfig = self: super: {
  di = super.callPackage ./pkg.nix {};
};

ghc822 = pkgs.haskell.packages.ghc822.override { inherit packageSetConfig; };

drv = ghc822.di;

in if pkgs.stdenv.lib.inNixShell then drv.env else drv
