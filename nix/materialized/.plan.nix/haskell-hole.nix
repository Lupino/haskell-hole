{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "haskell-hole"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "MIT";
      maintainer = "lmjubuntu@gmail.com";
      author = "Lupino";
      homepage = "https://github.com/Lupino/haskell-hole#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/Lupino/haskell-hole#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          (hsPkgs."map-io" or (errorHandler.buildDepError "map-io"))
          (hsPkgs."metro" or (errorHandler.buildDepError "metro"))
          (hsPkgs."metro-socket" or (errorHandler.buildDepError "metro-socket"))
          (hsPkgs."metro-transport-crypto" or (errorHandler.buildDepError "metro-transport-crypto"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [ "Paths_haskell_hole" ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "hole" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haskell-hole" or (errorHandler.buildDepError "haskell-hole"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          modules = [ "Paths_haskell_hole" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "hole-exe.hs" ];
          };
        "holed" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haskell-hole" or (errorHandler.buildDepError "haskell-hole"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          modules = [ "Paths_haskell_hole" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "holed.hs" ];
          };
        };
      tests = {
        "haskell-hole-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."haskell-hole" or (errorHandler.buildDepError "haskell-hole"))
            ];
          buildable = true;
          modules = [ "Paths_haskell_hole" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }) // {
    cabal-generator = "hpack";
    }