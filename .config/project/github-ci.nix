## TODO: Map `systems` and `exclude` from Nixier values – perhaps flake-utils
##       systems, and a bool for `--prefer-oldest`?
{
  systems,
  packages,
  ## TODO: Prefer ignoring most known failures once
  ##       https://github.com/orgs/community/discussions/15452 is resolved.
  exclude ? [],
  defaultGhcVersion,
  latestGhcVersion,
}: {
  lib,
  pkgs,
  self,
  ...
}: let
  planName = "plan-\${{ matrix.os }}-\${{ matrix.ghc }}\${{ matrix.bounds }}";
  runs-on = "ubuntu-22.04";
in {
  services.github.workflow."build.yml".text = lib.generators.toYAML {} {
    name = "CI";
    on = {
      push.branches = ["main"];
      pull_request.types = [
        "opened"
        "synchronize"
      ];
    };
    jobs = {
      build = {
        strategy = {
          fail-fast = false;
          matrix = {
            bounds = ["--prefer-oldest" ""];
            ghc = self.lib.nonNixTestedGhcVersions;
            os = systems;
            exclude =
              [
                ## GHCup can’t find this version for Linux.
                {
                  ghc = "7.10.3";
                  os = "ubuntu-22.04";
                }
              ]
              ## GitHub can’t install GHC older than 9.4 on macos-14.
              ++ map (ghc: {
                inherit ghc;
                os = "macos-14";
              }) (builtins.filter (ghc: lib.versionOlder ghc "9.4")
                self.lib.nonNixTestedGhcVersions)
              ++ exclude;
          };
        };
        runs-on = "\${{ matrix.os }}";
        env.CONFIG = "--enable-tests --enable-benchmarks \${{ matrix.bounds }}";
        steps = [
          {uses = "actions/checkout@v4";}
          {
            uses = "haskell-actions/setup@v2";
            id = "setup-haskell-cabal";
            "with" = {
              cabal-version = pkgs.cabal-install.version;
              ghc-version = "\${{ matrix.ghc }}";
            };
          }
          {run = "cabal v2-freeze $CONFIG";}
          {
            uses = "actions/cache@v4";
            "with" = {
              path = ''
                ''${{ steps.setup-haskell-cabal.outputs.cabal-store }}
                dist-newstyle
              '';
              key = "\${{ matrix.os }}-\${{ matrix.ghc }}-\${{ hashFiles('cabal.project.freeze') }}";
            };
          }
          ## NB: The `doctests` suites don’t seem to get built without
          ##     explicitly doing so before running the tests.
          {run = "cabal v2-build all $CONFIG";}
          {run = "cabal v2-test all $CONFIG";}
          {run = "mv dist-newstyle/cache/plan.json ${planName}.json";}
          {
            name = "Upload build plan as artifact";
            uses = "actions/upload-artifact@v4";
            "with" = {
              name = planName;
              path = "${planName}.json";
            };
          }
        ];
      };
      check-bounds = {
        inherit runs-on;
        ## Some "build" jobs are a bit flaky. This can give us useful bounds
        ## information even without all of the build plans.
        "if" = "always()";
        needs = ["build"];
        steps = [
          {uses = "actions/checkout@v4";}
          {
            uses = "haskell-actions/setup@v2";
            id = "setup-haskell-cabal";
            "with" = {
              cabal-version = pkgs.cabal-install.version;
              ghc-version = defaultGhcVersion;
            };
          }
          {run = "cabal install cabal-plan-bounds";}
          {
            name = "download Cabal plans";
            uses = "actions/download-artifact@v4";
            "with" = {
              path = "plans";
              pattern = "plan-*";
              merge-multiple = true;
            };
          }
          {
            name = "Cabal plans considered in generated bounds";
            run = "find plans/";
          }
          {
            name = "check if bounds have changed";
            ## TODO: Simplify this once cabal-plan-bounds supports a `--check`
            ##       option.
            run = ''
              diffs="$(find . -name '*.cabal' -exec \
                cabal-plan-bounds \
                  --dry-run \
                  ${
                lib.concatMapStrings
                (pkg: "--also " + pkg + " ")
                self.lib.extraDependencyVersions or []
              } \
                  plans/*.json \
                  --cabal {} \;)"
              if [[ -n "$diffs" ]]; then
                echo "$diffs"
                exit 1
              fi
            '';
          }
        ];
      };
      check-licenses = {
        inherit runs-on;
        ## Some "build" jobs are a bit flaky. Since this only uses one of the
        ## jobs from the matrix, we run it regardless of build failures.
        "if" = "always()";
        needs = ["build"];
        steps = [
          {uses = "actions/checkout@v4";}
          {
            uses = "haskell-actions/setup@v2";
            id = "setup-haskell-cabal";
            "with" = {
              cabal-version = pkgs.cabal-install.version;
              ghc-version = defaultGhcVersion;
            };
          }
          {run = "cabal install cabal-plan -flicense-report";}
          {
            name = "download Cabal plans";
            uses = "actions/download-artifact@v4";
            "with" = {
              path = "plans";
              pattern = "plan-*";
              merge-multiple = true;
            };
          }
          {
            run = ''
              mkdir -p dist-newstyle/cache
              mv plans/plan-${runs-on}-${latestGhcVersion}.json dist-newstyle/cache/plan.json
            '';
          }
          {
            name = "check if licenses have changed";
            run = ''
              ${lib.toShellVar "packages" packages}
              for package in "''${!packages[@]}"; do
                {
                  echo "**NB**: This captures the licenses associated with a particular set of dependency versions. If your own build solves differently, it’s possible that the licenses may have changed, or even that the set of dependencies itself is different. Please make sure you run [\`cabal-plan license-report\`](https://hackage.haskell.org/package/cabal-plan) on your own components rather than assuming this is authoritative."
                  echo
                  cabal-plan license-report "$package:lib:$package"
                } >"''${packages[$package]}/docs/license-report.md"
              done
              git diff --exit-code */docs/license-report.md
            '';
          }
        ];
      };
    };
  };
}
