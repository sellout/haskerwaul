{
  config,
  flaky,
  lib,
  pkgs,
  self,
  supportedSystems,
  ...
}: let
  githubSystems = ["macos-13" "ubuntu-22.04" "windows-2022"];
in {
  project = {
    name = "haskerwaul";
    summary = "Category-parametric programming";

    devPackages = [
      pkgs.cabal-install
      pkgs.graphviz
    ];
  };

  imports = [
    (import ./github-ci.nix githubSystems)
    ./hackage-publish.nix
    ./hlint.nix
  ];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git = {
      enable = true;
      ignores = [
        # Cabal build
        "dist-newstyle"
      ];
    };
  };

  ## formatting
  editorconfig.enable = true;

  programs = {
    treefmt = {
      enable = true;
      ## Haskell formatter
      programs.ormolu.enable = true;
    };
    vale = {
      enable = true;
      excludes = [
        "*.cabal"
        "*.hs"
        "*.lhs"
        "./cabal.project"
      ];
      vocab.${config.project.name}.accept = [
        "bugfix"
        "comonad"
        "conditionalize"
        "functor"
        "GADT"
        "Kleisli"
        "Kmett"
      ];
    };
  };

  ## CI
  services.garnix = {
    enable = true;
    builds = {
      exclude = [
        # TODO: Remove once garnix-io/garnix#285 is fixed.
        "homeConfigurations.x86_64-darwin-${config.project.name}-example"
      ];
      include = lib.mkForce (
        [
          "homeConfigurations.*"
          "nixosConfigurations.*"
        ]
        ++ lib.concatLists (
          flaky.lib.garnixChecks
          (
            sys:
              [
                "checks.${sys}.*"
                "devShells.${sys}.default"
                "packages.${sys}.default"
              ]
              ++ lib.concatMap (ghc: [
                "devShells.${sys}.${ghc}"
                "packages.${sys}.${ghc}_all"
              ])
              (self.lib.testedGhcVersions sys)
          )
        )
      );
    };
  };
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    (lib.concatMap (sys:
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      githubSystems
      ++ flaky.lib.forGarnixSystems supportedSystems (sys:
        lib.concatMap (ghc: [
          "devShell ghc${ghc} [${sys}]"
          "package ghc${sys}_all [${sys}]"
        ])
        (self.lib.testedGhcVersions sys)
        ++ [
          "homeConfig ${sys}-${config.project.name}-example"
          "package default [${sys}]"
          ## FIXME: These are duplicated from the base config
          "check formatter [${sys}]"
          "check project-manager-files [${sys}]"
          "check vale [${sys}]"
          "devShell default [${sys}]"
        ]));

  ## publishing
  # NB: Can’t use IFD on FlakeHub (see DeterminateSystems/flakehub-push#69), so
  #     this is disabled until we have a way to build Haskell without IFD.
  services.flakehub.enable = false;
  services.github.enable = true;
}
