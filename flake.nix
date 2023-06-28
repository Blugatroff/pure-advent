{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  };

  outputs = { nixpkgs, flake-utils, easy-purescript-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        easy-ps = easy-purescript-nix.packages.${system};
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "purescript-custom-shell";
            buildInputs = [
              easy-ps.purs-0_15_9
              easy-ps.spago
              easy-ps.purescript-language-server
              easy-ps.purs-tidy
              easy-ps.purs-backend-es
              pkgs.nodejs_20
              pkgs.esbuild
              pkgs.just
              pkgs.nodePackages.purescript-psa
            ];
          shellHook = ''
            source <(spago --bash-completion-script `which spago`)
            source <(node --completion-bash)
            '';
          };
       };
     }
  );
}

