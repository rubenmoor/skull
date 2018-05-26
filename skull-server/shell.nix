with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        skull-server = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.skull-server.env
