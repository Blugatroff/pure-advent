{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, flake-utils, purescript-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ purescript-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "purs-shell";
            buildInputs = with pkgs; [
              purs-bin.purs-0_15_10
              spago-unstable
              purescript-language-server
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
              esbuild
              just
              nodePackages.purescript-psa
              nodejs_20
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

