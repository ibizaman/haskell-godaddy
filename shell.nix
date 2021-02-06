(import ./release.nix { }).shell



# { }:

# let
#   src = builtins.path {
#     name = "haskell-godaddy";
#     path = ./.;
#     filter = path: type:
#       let
#         basePath = builtins.baseNameOf path;
#       in
#         basePath != "dist-newstyle" && basePath != ".stack-work";
#   };

#   hsPkgs = import ./default.nix { inherit src; };

#   haskell-language-server = hsPkgs.haskell-language-server.override { supportedGhcVersions = [ compiler ]; };
# in
#   hsPkgs.shellFor {
#     # Include only the *local* packages of your project.
#     packages = ps: with ps; [
#       # haskell-godaddy
#     ];

#     # Builds a Hoogle documentation index of all dependencies,
#     # and provides a "hoogle" command to search the index.
#     withHoogle = true;

#     # You might want some extra tools in the shell (optional).

#     # Some common tools can be added with the `tools` argument
#     # tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; haskell-language-server = "latest"; };
#     # See overlays/tools.nix for more details

#     # Some you may need to get some other way.
#     # buildInputs = with pkgs.haskellPackages;
#     #   [ ghcid, ];

#     # Prevents cabal from choosing alternate plans, so that
#     # *all* dependencies are provided by Nix.
#     exactDeps = true;
#   }
