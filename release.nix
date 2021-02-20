{ compiler ? "ghc865"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f0d8828b86c8105f722e9b1cceec50bcba1c9df6.tar.gz") {}
}:

let
  inherit (pkgs.haskell.lib) dontCheck;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # inherit (self) hspec hspec-core hspec-discover servant-client servant-server aeson;

      haskell-godaddy = self.callCabal2nix "haskell-godaddy" ./. { };

      # servant-client = dontCheck (self.callHackage "servant-client" "0.18.2" {});
      # servant-server = dontCheck (self.callHackage "servant-server" "0.18.2" {});
      # aeson = dontCheck (self.aeson);
      # QuickCheck = self.callHackage "QuickCheck" "2.13.2" {};
      # splitmix = self.callHackage "splitmix" "0.0.2" {};

      # hspec = self.callHackage "hspec" "2.7.1" {};
      # hspec-core = self.callHackage "hspec-core" "2.7.1" {};
      # hspec-discover = self.callHackage "hspec-discover" "2.7.1" {};
      # hspec-wai = self.callHackage "hspec-wai" "0.9.2" {};

      # hpack = dontCheck (self.callHackage "hpack" "0.34.1" {});
      Cabal = dontCheck (self.callHackage "Cabal" "3.2.1.0" {});

      # hoogle = dontCheck ((self.callHackage "hoogle" "5.0.18" {}).override {
      #   # example pinning a package only for hoogle
      #   extra = self.callHackage "extra" "1.7.4" {};
      # });
    };
  };

  shellHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # hpack = dontCheck (self.callHackage "hpack" "0.34.1" {});
      # Cabal = dontCheck (self.callHackage "Cabal" "3.0.2.0" {});
      Cabal = dontCheck (self.callHackage "Cabal" "3.2.1.0" {});

      # stack = dontCheck (self.callHackage "stack" "2.3.1" {});
      # hackage-security = dontCheck (self.callHackage "hackage-security" "0.6.0.0" {});
      # pantry = dontCheck (self.callHackage "pantry" "0.4.0.1" {});
      # haskell-language-server = self.haskell-language-server.override {
      #   supportedGhcVersions = [compiler];
      # };
    };
  };

  project = haskellPackages.haskell-godaddy;
in
{
  inherit project;

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      project
    ];
    buildInputs = with shellHaskellPackages; [
      Cabal
      stack
      haskell-language-server
    ];
    withHoogle = true;
  };
}
