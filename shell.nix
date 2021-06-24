let
  project = import ./default.nix;
in
project.shellFor {
  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    haskell-language-server = "latest";
  };

  exactDeps = true;
  withHoogle = true;
}
