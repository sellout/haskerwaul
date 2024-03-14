{
  lib,
  pkgs,
  self,
  ...
}: {
  services.github.workflow."hackage-publish.yml".text = lib.generators.toYAML {} {
    name = "Publish release to Hackage";
    on = {
      push.tags = ["v?[0-9]+.[0-9]+.[0-9]+*"];
      workflow_dispatch.inputs.tag = {
        description = "The existing version to publish to Hackage";
        type = "string";
        required = true;
      };
    };
    jobs.hackage-publish = {
      runs-on = "ubuntu-latest";
      permissions = {
        id-token = "write";
        contents = "read";
      };
      steps = [
        {
          uses = "actions/checkout@v4";
          "with".ref = "\${{ (inputs.tag != null) && format('refs/tags/{0}', inputs.tag) || '' }}";
        }
        {
          uses = "haskell-actions/hackage-publish@v1";
          "with" = {
            hackageToken = "\${{ secrets.HACKAGE_AUTH_TOKEN }}";
            packagesPath = "\${{ runner.temp }}/packages";
            publish = false;
          };
        }
      ];
    };
  };
}
