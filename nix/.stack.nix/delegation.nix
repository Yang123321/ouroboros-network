{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "delegation"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Metheds Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Delegation Executable Model";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.hedgehog)
          (hsPkgs.small-steps)
          (hsPkgs.microlens)
          (hsPkgs.microlens-th)
          (hsPkgs.non-integer)
          (hsPkgs.cs-ledger)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-class)
          (hsPkgs.cardano-prelude)
          ];
        };
      tests = {
        "delegation-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cryptonite)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.hedgehog)
            (hsPkgs.delegation)
            (hsPkgs.containers)
            (hsPkgs.multiset)
            (hsPkgs.text)
            (hsPkgs.microlens)
            (hsPkgs.cs-ledger)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto-class)
            (hsPkgs.cardano-prelude)
            (hsPkgs.small-steps)
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
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/executable-spec; echo source root reset to \$sourceRoot";
    }