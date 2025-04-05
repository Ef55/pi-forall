{
  inputs = {
   nixpkgs.url = "github:nixos/nixpkgs";
   utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };

      uninstall-local-autoenv = pkgs.writeShellApplication {
        name = "ul-autoenv";
        runtimeInputs = with pkgs; [
        ];
        text = ''
          stack exec -- ghc-pkg unregister --force autoenv
        '';
      };

    in
    {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          ghc
          haskellPackages.stack
          haskellPackages.haskell-language-server
          haskellPackages.hlint
          haskellPackages.ormolu
          uninstall-local-autoenv
        ];
      };
    }
  );
}
