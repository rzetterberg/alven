# Descriptes what dependencies should be used when running nix-shell in the
# project dir. Sets relevant environment variables so that all Haskell tools
# and libraries can be found inside the shell.

let
  common = import ./common.nix;
  pkgs   = import <nixpkgs> {};
  env    = pkgs.haskellngPackages.ghcWithPackages (p: with p; [
    aeson base bytestring cabal-install classy-prelude classy-prelude-conduit
    classy-prelude-yesod conduit containers data-default directory esqueleto
    fast-logger file-embed hjsmin hspec http-conduit markdown monad-control
    monad-logger persistent persistent-postgresql persistent-template process
    resourcet safe shakespeare template-haskell text time unordered-containers
    vector wai-extra wai-logger warp yaml yesod yesod-auth yesod-bin yesod-core
    yesod-form yesod-text-markdown yesod-static yesod-test
  ]);
in
  pkgs.stdenv.mkDerivation {
    name        = common.name;
    buildInputs = [ env ];
    shellHook   = ''
      export NIX_GHC="${env}/bin/ghc"
      export NIX_GHCPKG="${env}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  }
