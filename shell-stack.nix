{}:

let
  self = (import ./release.nix {});

  pkgs = self.pkgs;
  buildInputs = with self.shellHaskellPackages; [
    Cabal
    stack
    haskell-language-server
    pkgs.zlib
  ];
  ghc = self.shellHaskellPackages.ghc;
in

pkgs.haskell.lib.buildStackProject{
  name = "myEnv";
  inherit buildInputs ghc;
}
