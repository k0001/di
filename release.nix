{ nixpkgs ? builtins.fetchTarball channel:nixos-18.03
}:

let
  pkgs = import nixpkgs {};
  ghc841 = pkgs.haskell.packages.ghc841.override {
    packageSetConfig = import ./hs-overlay.nix;
  };

in {
  inherit (ghc841) df1 di di-core di-df1 di-handle di-monad;
}
