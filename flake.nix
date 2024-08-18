{ 
  nixConfig.extra-substituters = [ "https://pac-nix.cachix.org/" ];
  nixConfig.extra-trusted-public-keys = [ "pac-nix.cachix.org-1:l29Pc2zYR5yZyfSzk1v17uEZkhEw0gI4cXuOIsxIGpc=" ];

  inputs.pac-nix.url = "github:katrinafyi/pac-nix";

  outputs = {self, pac-nix}:
    let
      nixpkgs = pac-nix.inputs.nixpkgs;

      forAllSystems = f:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: f system pac-nix.legacyPackages.${system});

    in {
      packages = forAllSystems (sys: pac-nix:
        let pkgs = nixpkgs.legacyPackages.${sys};
        in {
          default = pkgs.haskellPackages.developPackage {
            root = pkgs.lib.cleanSourceWith {
              filter = name: type: !(baseNameOf name == "docs" || baseNameOf name == "site");
              src = ./.;
            };
          };
        }
      );
    };
}
