let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.shellFor {
      packages = ps: [ps.haskell-godaddy];
      withHoogle = true;
  }
