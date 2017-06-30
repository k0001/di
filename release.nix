{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? builtins.fetchTarball
    https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.03.tar.gz
}:
let
pkgs = import nixpkgs {};
hsPackageSetConfig = self: super: {
  di = self.callPackage (import ./pkg.nix) {};
};
ghc802 = pkgs.haskell.packages.ghc802.override {
  packageSetConfig = hsPackageSetConfig;
};

in { inherit (ghc802) di; }
