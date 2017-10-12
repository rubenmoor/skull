with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        diener = self.callPackage ../../diener {};
        gerippe = self.callPackage ../../gerippe {};
        skull-server = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.skull-server.env
