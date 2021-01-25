let
  # We are using lts-15.13 stack resolver which uses ghc883 (cf https://www.stackage.org/lts-15.13)
  compiler = "ghc884";

  # pin nixpkgs for reproducible build
  nixpkgsVersion = import nix/nixpkgs-version.nix;
  nixpkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };


  # overlays define packages we need to build our project
  allOverlays = import nix/overlays;
  overlays = [
    allOverlays.gitignore # helper to use gitignoreSource
    (allOverlays.haskell-packages { inherit compiler; })
  ];

  pkgs = import nixpkgs { inherit overlays; };

  # We define our packages by giving them names and a list of source files
  haskell-hole = {
    name = "haskell-hole";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./.)[ ".cabal" ".hs" "LICENSE" ];
  };

  # Some patches are unfortunately necessary to work with libpq
  patches = pkgs.callPackage nix/patches {};

  lib = pkgs.haskell.lib;

  # call our script which add our packages to nh2/static-haskell-nix project
  staticHaskellPackage = import nix/static-haskell-package.nix { inherit nixpkgs compiler patches allOverlays; };
in
rec {
  inherit nixpkgs pkgs;

  # if instead we want to generated a fully static executable we need:
  haskell-hole-static = lib.justStaticExecutables (lib.dontCheck (staticHaskellPackage haskell-hole.name haskell-hole.src));
}
