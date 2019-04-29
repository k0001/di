# This file is a nixpkgs package set overlay to be used as:
#   import nixpkgs { overlays = [ import ./pkgs-overlay.hs ]; }
self: super:
{
  _here = {
    ghc864 = super.haskell.packages.ghc864.override {
      packageSetConfig = import ./hs-overlay.nix { pkgs = self; };
    };
  };
}
