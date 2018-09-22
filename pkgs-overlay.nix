# This file is a nixpkgs package set overlay to be used as:
#   import nixpkgs { overlays = [ import ./pkgs-overlay.hs ]; }
self: super:
{
  _here = {
    ghc861 = super.haskell.packages.ghc861.override {
      packageSetConfig = import ./hs-overlay.nix { pkgs = self; };
    };
  };
}
