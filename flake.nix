{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hsPkgs = pkgs.haskellPackages;

        appDrv = pkgs.haskell.lib.justStaticExecutables
          (hsPkgs.callCabal2nix "hlox" ./. { });

        devEnv = hsPkgs.developPackage {
          returnShellEnv = true;
          root = ./.;
          modifier = with pkgs.haskell.lib;
            drv:
            dontHaddock (disableOptimization (disableLibraryProfiling drv));
        };
      in rec {
        apps.hlox = {
          type = "app";
          program = "${pkgs.haskell.lib.justStaticExecutables appDrv}/bin/hlox";
        };
        defaultApp = apps.hlox;

        packages.hlox = appDrv;
        defaultPackage = packages.hlox;

        devShell = pkgs.mkShell {
          inputsFrom = [ devEnv ];
          buildInputs = with hsPkgs; [ pkgs.just haskell-language-server implicit-hie ];
        };
      });
}
