{
  description = "Flake utils demo";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url =  "github:nixos/nixpkgs/23.11";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        devShell = pkgs.mkShellNoCC {
          name = "ocaml shell";
          packages = with pkgs; [
            ocaml
            dune_3
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
          ];
        };
      }
    );
}
