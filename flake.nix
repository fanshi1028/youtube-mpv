{
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/6909e461de4ecafbfac42e7a7a9e53da01ebf244";
    nix-github-actions = {
      url = "github:nix-community/nix-github-actions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, nix-github-actions }:
    let
      ghcVersion = "96";
      mkHsPackage = pkgs: pkgs.haskell.packages."ghc${ghcVersion}";
    in {
      packages = builtins.mapAttrs (system: pkgs: {
        youtube-mpv-native-host = ((mkHsPackage pkgs).developPackage {
          root = with pkgs.lib.fileset;
            toSource {
              root = ./.;
              fileset = unions [
                ./youtube-mpv-native-host.cabal
                ./native-host
                ./LICENSE
                ./CHANGELOG.md
              ];
            };
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
        youtube-mpv-chrome-extension = with pkgs;
          buildNpmPackage {
            pname = "youtube-mpv-chrome-extension";
            version = "1.0.0";
            nativeBuildInputs = [ typescript ];
            src = with lib.fileset;
              toSource {
                root = ./.;
                fileset = unions [
                  ./extension/src
                  ./package.json
                  ./package-lock.json
                  ./tsconfig.json
                  (fileFilter (file: !(file.hasExt "js")) ./extension/dist)
                ];
              };
            npmDepsHash = "sha256-Z1IhQt2Yul1AjX3V1F0VHvjCOAxvDkxfOf3iNVfdI4E=";
            buildPhase = "tsc ";
            postInstall = ''
              mkdir -p $out/share/chrome/extensions
              cp -r extension/dist/* $out/share/chrome/extensions
              rm -r $out/lib
            '';
          };
        youtube-mpv = with pkgs;
          let
            inherit (self.packages.${system})
              youtube-mpv-native-host youtube-mpv-chrome-extension;
          in stdenvNoCC.mkDerivation {
            pname = "youtube-mpv";
            version = "1.0.0";
            buildInputs =
              [ youtube-mpv-native-host youtube-mpv-chrome-extension ];
            phases = [ "installPhase" ];
            installPhase = ''
              mkdir $out
              cp -r ${youtube-mpv-native-host}/bin $out
              cp -r ${youtube-mpv-chrome-extension}/share $out
            '';
          };
        default = self.packages.${system}.youtube-mpv;
      }) nixpkgs.legacyPackages;
      devShells = builtins.mapAttrs (system: pkgs:
        with pkgs;
        let hsPackage = mkHsPackage pkgs;
        in {
          default = (mkHsPackage pkgs).shellFor {
            packages = _: [ self.packages.${system}.youtube-mpv-native-host ];
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
                npm
              ]);
            withHoogle = true;
          };
        }) nixpkgs.legacyPackages;

      githubActions = nix-github-actions.lib.mkGithubMatrix {
        checks = builtins.mapAttrs (_: pkgs: { inherit (pkgs) youtube-mpv; })
          {
            inherit (self.packages)  "x86_64-linux" "x86_64-darwin" "mingwW64";
          };
      };
    };
}
