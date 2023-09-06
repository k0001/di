{
  description = "di";
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/389cc28963163614765721eda940fd5299f18458";
    };
    flake-parts = { url = "github:hercules-ci/flake-parts"; };
    hs_bsb-http-chunked = {
      url =
        "github:sjakobi/bsb-http-chunked/c0ecd72fe2beb1cf7de9340cc8b4a31045460532";
      flake = false;
    };
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = final: prev:
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

                # hoogle stuff
                bsb-http-chunked = hself.callCabal2nix "bsb-http-chunked"
                  inputs.hs_bsb-http-chunked { };
                warp = hsLib.dontCheck (hself.callHackage "warp" "3.3.25" { });
                warp-tls =
                  hsLib.dontCheck (hself.callHackage "warp-tls" "3.3.6" { });
                recv = hself.callHackage "recv" "0.1.0" { };
              });
          };
        };
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
          inherit (pkgs.haskell.packages.ghc962)
            df1 df1-html di di-core di-df1 di-handle di-monad;
        };
        devShells = {
          default = config.devShells.ghc;
          ghc = pkgs.haskell.packages.ghc962.shellFor {
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
