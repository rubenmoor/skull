with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        diener = self.callPackage ../../diener {};
        servant-purescript = haskellPackages.servant-purescript_0_8_0_0; 
        skull-server = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.skull-server.env
