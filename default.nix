# Describes what Haskell dependencies are used and how the project should
# be built using cabal.
#
# Currently builds the project with all libraries statically compiled for easy
# deployment.
#
# Testing is disabled since running them requires the test database to be
# setup. Instead all testing defined in the source code will be performed
# in the nix-shell. Then hydra performs some system wide tests in a VM
# (see release.nix).

{ stdenv, mkDerivation, base, aeson, bytestring, cabal-install, classy-prelude,
  classy-prelude-conduit, classy-prelude-yesod, conduit, containers,
  data-default, directory, esqueleto, fast-logger, file-embed, hjsmin, hslua,
  hspec, http-conduit, markdown, monad-control, monad-logger, persistent,
  persistent-postgresql, persistent-template, process, resourcet, safe,
  shakespeare, template-haskell, text, time, transformers, unordered-containers,
  vector, wai-extra, wai-logger, warp, yaml, yesod, yesod-auth, yesod-bin,
  yesod-core, yesod-form, yesod-text-markdown, yesod-static, yesod-test
}:

let
  common = import ./common.nix;
in
  mkDerivation rec {
    pname   = common.name;
    version = common.version;
    src     = ./src;

    isLibrary    = true;
    isExecutable = true;
    doCheck      = false;

    enableSharedExecutables = false;

    buildDepends = [
      aeson base bytestring cabal-install classy-prelude classy-prelude-conduit
      classy-prelude-yesod conduit containers data-default directory esqueleto
      fast-logger file-embed hjsmin hslua http-conduit markdown monad-control
      monad-logger persistent persistent-postgresql persistent-template process
      resourcet safe shakespeare template-haskell text time unordered-containers
      vector wai-extra wai-logger warp yaml yesod yesod-auth yesod-bin yesod-core
      yesod-form yesod-text-markdown yesod-static
    ];

    testDepends = [
      aeson base classy-prelude classy-prelude-yesod hspec monad-logger
      persistent persistent-postgresql resourcet transformers wai-extra yesod
      yesod-core yesod-test
    ];

    postInstall = ''
      mkdir $out/bin/config -p
      mkdir $out/share/static -p

      function cpConfig {
        cp $src/config/$1 $out/bin/config/
      }

      function cpStatic {
        cp $src/static/$1 $out/share/static/ -R
      }

      cpConfig favicon.ico
      cpConfig robots.txt
      cpConfig settings.yml

      cpStatic combined
      cpStatic css
      cpStatic fonts
    '';

    license = stdenv.lib.licenses.gpl2;
  }
