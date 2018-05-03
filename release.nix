{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? builtins.fetchTarball channel:nixos-18.03
}:
let
pkgs = import nixpkgs {};
hsPackageSetConfig = self: super: {
  di = self.callPackage (import ./pkg.nix) {};
};
ghc822 = pkgs.haskell.packages.ghc822.override {
  packageSetConfig = hsPackageSetConfig;
};

in { inherit (ghc822) di; }
