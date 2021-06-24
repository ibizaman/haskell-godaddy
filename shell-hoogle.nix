let
  project = import ./default.nix {};
in
project.shellFor {
  packages = ps: [ps.haskell-godaddy];
  withHoogle = true;
}
