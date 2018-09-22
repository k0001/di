# This file exports every derivation introduced by this repository.
{ nixpkgs ? import ./nixpkgs.nix }:
let pkgs = import ./pkgs.nix { inherit nixpkgs; };
in
pkgs.releaseTools.aggregate {
  name = "everything";
  constituents = [
    pkgs._here.ghc861.df1
    pkgs._here.ghc861.di
    pkgs._here.ghc861.di-core
    pkgs._here.ghc861.di-df1
    pkgs._here.ghc861.di-handle
    pkgs._here.ghc861.di-monad
    pkgs._here.ghc861._shell
  ];
}

