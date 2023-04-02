{
  description = "Haskell API for OpenAI";

  nixConfig.bash-prompt = "[nix:openai-haskell]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
        };

        haskellEnv = pkgs.haskell.packages.ghc943;

        openai-api = haskellEnv.callCabal2nix "openai-api" ./. { };

      in {
        packages = {
          openai-api = openai-api;
        };

        defaultPackage = self.packages.${system}.openai-api;
        
        devShell = haskellEnv.shellFor {
          packages = p: [ openai-api ];
          buildInputs = with pkgs; [ cabal-install haskellEnv.dotenv ];
        };

        checks = {
          openai-api = openai-api;
        };
      }
    );
}
