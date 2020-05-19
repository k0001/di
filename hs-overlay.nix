{ pkgs }:

# To be used as `packageSetConfig` for a Haskell pacakge set:
let inherit (pkgs.haskell.lib) doJailbreak;

in self: super: {
  df1 = super.callPackage ./df1/pkg.nix { };
  di = super.callPackage ./di/pkg.nix { };
  di-core = super.callPackage ./di-core/pkg.nix { };
  di-df1 = super.callPackage ./di-df1/pkg.nix { };
  di-handle = super.callPackage ./di-handle/pkg.nix { };
  di-monad = super.callPackage ./di-monad/pkg.nix { };

  _shell = self.shellFor {
    withHoogle = true; # hoogle dependencies don't compile
    packages = p: [
      p.df1 p.di p.di-core p.di-df1 p.di-handle p.di-monad
    ];
  };
}
