let
  sources = {
    haskellNix = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/2b842f282c9b9001beab232d513636ec96718c7b.tar.gz";
  };

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix {};
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-2009
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-godaddy";
    src = ./.;
  };
  # Specify the GHC version to use. See
  # https://input-output-hk.github.io/haskell.nix/reference/supported-ghc-versions/
  # and
  # https://github.com/input-output-hk/haskell.nix/blob/master/ci.nix
  # for which versions are cached by CI.
  compiler-nix-name = "ghc8104";
}
