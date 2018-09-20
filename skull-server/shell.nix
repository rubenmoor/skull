with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        skull-server = self.callPackage ./. {};
        # servant-purescript = haskellPackages.servant-purescript_0_8_0_0;
      };
    };
in modifiedHaskellPackages.skull-server.env
