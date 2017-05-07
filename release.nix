{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? (import nixpkgsBootstrap {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "b12aacc7c18fb3f29febc842aaa3d0c0e5622546"; # release-17.03
    sha256 = "1qklmrvfnlk1r6rqxpddllfkv3pihfms370qfsznm959i7hxcv2v"; }
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
