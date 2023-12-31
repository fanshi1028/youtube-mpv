{
  inputs = { nixpkgs.url = "nixpkgs-unstable"; };
  outputs = { self, nixpkgs }:
    let
      ghcVersion = "96";
      mkHsPackage = pkgs: pkgs.haskell.packages."ghc${ghcVersion}";
    in {
      packages = builtins.mapAttrs (system: pkgs: {
        default = ((mkHsPackage pkgs).developPackage {
          root = ./.;
          modifier = drv: pkgs.haskell.lib.appendConfigureFlag drv "-O2";
        }).overrideAttrs (_: oa: {
          # NOTE: https://stackoverflow.com/a/69395418
          nativeBuildInputs = (oa.nativeBuildInputs or [ ])
            ++ [ pkgs.makeWrapper ];
          postFixup = (oa.postFixup or "") + ''
            wrapProgram $out/bin/youtube-mpv-native-host --prefix PATH : ${
              pkgs.lib.makeBinPath [ pkgs.mpv ]
            }
          '';
        });
      }) nixpkgs.legacyPackages;
      devShells = builtins.mapAttrs (system: pkgs:
        with pkgs;
        let hsPackage = mkHsPackage pkgs;
        in {
          default = (mkHsPackage pkgs).shellFor {
            packages = _: [ self.packages.${system}.default ];
            nativeBuildInputs = with pkgs;
              [
                (haskell-language-server.override {
                  supportedGhcVersions = [ ghcVersion ];
                })
                cabal-install
                ghcid
                typescript
                mpv
              ] ++ (with nodePackages; [
                typescript-language-server
                pnpm
                prettier
              ]);
            withHoogle = true;
          };
        }) nixpkgs.legacyPackages;
    };
}
