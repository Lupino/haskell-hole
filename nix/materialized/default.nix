{
  pkgs = hackage:
    {
      packages = {
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "old-time".revision = (((hackage."old-time")."1.1.0.3").revisions).default;
        "hslogger".revision = (((hackage."hslogger")."1.3.1.0").revisions).default;
        "hslogger".flags.network--gt-3_0_0 = true;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.11").revisions).default;
        "bifunctors".flags.tagged = true;
        "bifunctors".flags.semigroups = true;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.10.3").revisions).default;
        "ansi-terminal".flags.example = false;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.5").revisions).default;
        "time-compat".flags.old-locale = false;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.7").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "vector".revision = (((hackage."vector")."0.12.1.2").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.boundschecks = true;
        "vector".flags.wall = false;
        "network".revision = (((hackage."network")."3.1.1.1").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "async".revision = (((hackage."async")."2.2.3").revisions).default;
        "async".flags.bench = false;
        "process".revision = (((hackage."process")."1.6.9.0").revisions).default;
        "random".revision = (((hackage."random")."1.1").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.15.1.0").revisions).default;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.old-random = true;
        "QuickCheck".flags.templatehaskell = true;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.5").revisions).default;
        "scientific".revision = (((hackage."scientific")."0.3.6.2").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "distributive".revision = (((hackage."distributive")."0.6.2.1").revisions).default;
        "distributive".flags.tagged = true;
        "distributive".flags.semigroups = true;
        "base".revision = (((hackage."base")."4.14.2.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0.1").revisions).default;
        "text".revision = (((hackage."text")."1.2.4.1").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.13.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.1").revisions).default;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "unliftio".revision = (((hackage."unliftio")."0.2.15").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.12.0").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "basement".revision = (((hackage."basement")."0.0.12").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "containers".revision = (((hackage."containers")."0.6.4.1").revisions).default;
        "tagged".revision = (((hackage."tagged")."0.8.6.1").revisions).default;
        "tagged".flags.deepseq = true;
        "tagged".flags.transformers = true;
        "quickcheck-instances".revision = (((hackage."quickcheck-instances")."0.3.25.2").revisions).default;
        "quickcheck-instances".flags.bytestring-builder = false;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.4").revisions).default;
        "primitive".revision = (((hackage."primitive")."0.7.1.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.6").revisions).default;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.three = false;
        "transformers-compat".flags.mtl = true;
        "memory".revision = (((hackage."memory")."0.15.0").revisions).default;
        "memory".flags.support_basement = true;
        "memory".flags.support_bytestring = true;
        "memory".flags.support_foundation = true;
        "memory".flags.support_deepseq = true;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.4.2.0").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.5").revisions).default;
        "network-bsd".revision = (((hackage."network-bsd")."2.8.1.0").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.3").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "strict".revision = (((hackage."strict")."0.4.0.1").revisions).default;
        "strict".flags.assoc = true;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.examples = false;
        "comonad".revision = (((hackage."comonad")."5.0.8").revisions).default;
        "comonad".flags.distributive = true;
        "comonad".flags.indexed-traversable = true;
        "comonad".flags.containers = true;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "cryptonite".revision = (((hackage."cryptonite")."0.27").revisions).default;
        "cryptonite".flags.old_toolchain_inliner = false;
        "cryptonite".flags.support_pclmuldq = false;
        "cryptonite".flags.support_deepseq = true;
        "cryptonite".flags.support_aesni = true;
        "cryptonite".flags.use_target_attributes = true;
        "cryptonite".flags.support_sse = false;
        "cryptonite".flags.support_rdrand = true;
        "cryptonite".flags.integer-gmp = true;
        "cryptonite".flags.check_alignment = false;
        "indexed-traversable".revision = (((hackage."indexed-traversable")."0.1.1").revisions).default;
        "unix-time".revision = (((hackage."unix-time")."0.4.7").revisions).default;
        "old-locale".revision = (((hackage."old-locale")."1.0.0.7").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.1").revisions).default;
        };
      compiler = {
        version = "8.10.5";
        nix-name = "ghc8105";
        packages = {
          "binary" = "0.8.8.0";
          "ghc-prim" = "0.6.1";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          "template-haskell" = "2.16.0.0";
          "pretty" = "1.1.3.6";
          "process" = "1.6.9.0";
          "base" = "4.14.2.0";
          "rts" = "1.0.1";
          "text" = "1.2.4.1";
          "mtl" = "2.2.2";
          "time" = "1.9.3";
          "unix" = "2.7.2.2";
          "bytestring" = "0.10.12.0";
          "containers" = "0.6.4.1";
          "directory" = "1.3.6.0";
          "ghc-boot-th" = "8.10.5";
          "filepath" = "1.4.2.1";
          "deepseq" = "1.4.4.0";
          "transformers" = "0.5.6.2";
          "stm" = "2.5.0.1";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        map-io = ./.plan.nix/map-io.nix;
        haskell-hole = ./.plan.nix/haskell-hole.nix;
        metro = ./.plan.nix/metro.nix;
        metro-socket = ./.plan.nix/metro-socket.nix;
        metro-transport-crypto = ./.plan.nix/metro-transport-crypto.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "map-io" = { flags = {}; };
          "haskell-hole" = { flags = {}; };
          "metro" = { flags = {}; };
          "metro-socket" = { flags = {}; };
          "metro-transport-crypto" = { flags = {}; };
          };
        })
    ];
  }