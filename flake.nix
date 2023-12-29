{
  inputs = { nixpkgs.url = "nixpkgs-unstable"; };
  outputs = { self, nixpkgs }: {
    devShells = builtins.mapAttrs (system: pkgs: {
      default = with pkgs;
        mkShell {
          buildInputs = [
            typescript
            cabal-install
            haskell.compiler.ghc96
            (haskell-language-server.override {
              supportedGhcVersions = [ "96" ];
            })
          ] ++ (with nodePackages; [
            typescript-language-server
            pnpm
            prettier
          ]);
        };
    }) nixpkgs.legacyPackages;
  };
}
