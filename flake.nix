{
  inputs = { nixpkgs.url = "nixpkgs-unstable"; };
  outputs = { self, nixpkgs }: {
    devShells = builtins.mapAttrs (system: pkgs: {
      default = with pkgs;
        mkShell {
          buildInputs = [ typescript ] ++ (with nodePackages; [
            typescript-language-server
            pnpm
            prettier
          ]);
        };
    }) nixpkgs.legacyPackages;
  };
}
