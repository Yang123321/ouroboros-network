{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "non-integer"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Implementation decision for non-integer calculations";
      buildType = "Simple";
      };
    components = {
      "library" = { depends = [ (hsPkgs.base) ]; };
      exes = {
        "nonInt" = { depends = [ (hsPkgs.base) (hsPkgs.non-integer) ]; };
        };
      tests = {
        "non-integer-test" = {
          depends = (pkgs.lib).optionals (!flags.development) [
            (hsPkgs.base)
            (hsPkgs.non-integer)
            (hsPkgs.QuickCheck)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "cf339d59d09e24005a0b67babce013ed86330e30";
      sha256 = "0cjm2vccm0ya63n7xl965syd8qba7b7fd33zliry1j6pzakc511v";
      });
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/dependencies/non-integer; echo source root reset to \$sourceRoot";
    }