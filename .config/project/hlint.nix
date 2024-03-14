{
  lib,
  pkgs,
  ...
}: {
  ## Haskell linter
  programs.treefmt.programs.hlint.enable = true;
  ## TODO: Wrap this to find our generated hlint config in the store.
  project.devPackages = [pkgs.hlint];
  project.file.".hlint.yaml".text = lib.generators.toYAML {} [
    {
      group = {
        name = "dollar";
        enabled = true;
      };
    }
    {
      group = {
        name = "future";
        enabled = true;
      };
    }
    {
      group = {
        name = "generalise";
        enabled = true;
      };
    }

    {ignore = {name = "Eta reduce";};}
    {ignore = {name = "Evaluate";};}
    {ignore = {name = "Reduce duplication";};}
    ## TODO: Re-enable this one, and either adjust odd cases, or ignore this
    ##       locally.
    {ignore = {name = "Use camelCase";};}
    ## NB: In this project, we define a `map` that is more generic than `fmap`,
    ##     so this warning is misleading.
    {ignore = {name = "Use fmap";};}
    {ignore = {name = "Use list comprehension";};}
    {ignore = {name = "Use section";};}

    {
      package = {
        name = "monad";
        modules = ["import Control.Monad"];
      };
    }

    {
      package = {
        name = "traversable";
        modules = [
          "import Data.Foldable"
          "import Data.Traversable"
        ];
      };
    }

    {
      group = {
        name = "generalize";
        imports = [
          "package monad"
          "package traversable"
        ];
        rules = [
          {
            warn = {
              lhs = "forM";
              rhs = "for";
            };
          }
          {
            warn = {
              lhs = "forM_";
              rhs = "for_";
            };
          }
          {
            warn = {
              lhs = "mapM";
              rhs = "traverse";
            };
          }
          {
            warn = {
              lhs = "mapM_";
              rhs = "traverse_";
            };
          }
          {
            warn = {
              lhs = "return";
              rhs = "pure";
            };
          }
          {
            warn = {
              lhs = "sequence";
              rhs = "sequenceA";
            };
          }
          {
            warn = {
              lhs = "sequence_";
              rhs = "sequenceA_";
            };
          }
        ];
      };
    }

    {
      group = {
        name = "generalize";
        imports = ["package traversable"];
        rules = [
          {
            hint = {
              lhs = "maybe (pure ())";
              rhs = "traverse_";
              note = "IncreasesLaziness";
            };
          }
          {
            warn = {
              lhs = "mappend";
              rhs = "(<>)";
            };
          }
          {
            warn = {
              lhs = "(++)";
              rhs = "(<>)";
            };
          }
        ];
      };
    }
  ];
}
