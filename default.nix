# This file exports every derivation introduced by this repository.
{ nixpkgs ? import ./nixpkgs.nix }:
let pkgs = import ./pkgs.nix { inherit nixpkgs; };
in
pkgs.releaseTools.aggregate {
  name = "everything";
  constituents = [
    pkgs._here.ghc864.df1
    pkgs._here.ghc864.di
    pkgs._here.ghc864.di-core
    pkgs._here.ghc864.di-df1
    pkgs._here.ghc864.di-handle
    pkgs._here.ghc864.di-monad
    pkgs._here.ghc864._shell
  ];
}

