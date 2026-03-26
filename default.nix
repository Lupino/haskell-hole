{ compiler-nix-name ? "ghc9141", enableProfiling ? false }:
let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix
  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix
  # Import hackage index state from haskell.nix's hackage.nix
  hackageIndexState = import (haskellNix.sources.hackage + "/index-state.nix");

  # Get the latest index state by sorting the keys
  allIndexStates = builtins.attrNames hackageIndexState;
  sortedStates = builtins.sort (a: b: a > b) allIndexStates;
  latestState = builtins.head sortedStates;

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-2411
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.cabalProject {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      src = ./.;
      name = "haskell-hole";
    };
    # Use index state from hackage.nix
    index-state = latestState;
    index-sha256 = hackageIndexState.${latestState};
    sha256map = import ./nix/sha256map.nix;
    # Specify the GHC version to use.
    compiler-nix-name = compiler-nix-name;
    modules = [(
       {pkgs, ...}: {
         # Enable profiling globally for all packages if requested
         enableProfiling = enableProfiling;
      })];
  }
