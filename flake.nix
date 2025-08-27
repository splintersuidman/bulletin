{
  description = "A very basic flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc9102";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.${compiler};
      in {
        packages.bulletin = haskellPackages.callPackage ./bulletin.nix { };

        defaultPackage = self.packages.${system}.bulletin;

        devShell = self.packages.${system}.bulletin.env.overrideAttrs
          (oldAttrs: {
            buildInputs = (oldAttrs.buildInputs or [ ])
              ++ (with haskellPackages; [
                cabal2nix
                haskell-language-server
              ]);
          });
      });
}
