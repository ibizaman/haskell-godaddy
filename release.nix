{ compiler ? "ghc865"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz") {}
}:

let
  inherit (pkgs.haskell.lib) dontCheck;

  commonOverrides = self: super: {
      Cabal = dontCheck (self.callHackage "Cabal" "3.2.0.0" {});
      hpack = dontCheck (self.callHackage "hpack" "0.34.2" {});
  };

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      haskell-godaddy = self.callCabal2nix "haskell-godaddy" ./. { };
      hspec-wai = dontCheck (self.callHackage "hspec-wai" "0.9.0" {});
    } // (commonOverrides self super);
  };

  shellHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
    } // (commonOverrides self super);
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
