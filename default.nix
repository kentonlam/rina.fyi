let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = pkgs.nix-gitignore.gitignoreSource
    [ "/docs" ] ./.;
}
