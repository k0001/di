{
  description = "di";
  inputs = {
    flakety.url = "github:k0001/flakety";
    nixpkgs.follows = "flakety/nixpkgs";
    flake-parts.follows = "flakety/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.flakety.overlays.default
        (final: prev:
          let
            hsLib = prev.haskell.lib;
            hsClean = drv:
              hsLib.overrideCabal drv
              (old: { src = prev.lib.sources.cleanSource old.src; });
          in {
            haskell = prev.haskell // {
              packageOverrides = prev.lib.composeExtensions
                (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
                  df1 = hsClean (hself.callPackage ./df1 { });
                  df1-html = hsClean (hself.callPackage ./df1-html { });
                  di = hsClean (hself.callPackage ./di { });
                  di-core = hsClean (hself.callPackage ./di-core { });
                  di-df1 = hsClean (hself.callPackage ./di-df1 { });
                  di-handle = hsClean (hself.callPackage ./di-handle { });
                  di-monad = hsClean (hself.callPackage ./di-monad { });
                });
            };
          })
      ];
      systems = [ "x86_64-linux" ];
      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.df1
              config.packages.df1-html
              config.packages.di
              config.packages.di-core
              config.packages.di-df1
              config.packages.di-handle
              config.packages.di-monad
              config.devShells.ghc
            ];
          };
          inherit (pkgs.haskell.packages.ghc98)
            df1 df1-html di di-core di-df1 di-handle di-monad;
        };
        devShells = {
          default = config.devShells.ghc;
          ghc = pkgs.haskell.packages.ghc98.shellFor {
            packages = p: [
              p.df1
              p.df1-html
              p.di
              p.di-core
              p.di-df1
              p.di-handle
              p.di-monad
            ];
            withHoogle = true;
            nativeBuildInputs =
              [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
          };
        };
      };
    };
}
