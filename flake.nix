{
  description = "A Haskell API for OpenAI";

  nixConfig.bash-prompt = "[nix:openai-haskell]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Import the Haskell environment from nixpkgs
        haskellEnv = pkgs.haskellPackages.developPackage {
          root = ./.;
          name = "openai-api";
        };
      in
      {
        devShell = haskellEnv.shellFor {
          # Add any additional packages you need for development here
          buildInputs = with pkgs; [
            cabal-install
            ghc
            git
          ];
        };
      }
    );
}
