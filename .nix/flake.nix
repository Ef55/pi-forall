{
  inputs = {
   nixpkgs.url = "github:nixos/nixpkgs";
   utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };

      pi-forall = pkgs.writeShellApplication {
        name = "pi-forall";
        runtimeInputs = with pkgs; [
          ghc
          haskellPackages.stack
        ];
        text = ''
          file=$(realpath "$1")
          cd "$PI_FORALL_ROOT/full" && stack build --profile && (stack exec --profile -- pi-forall "$file" +RTS -poperf -p -h)
        '';
      };

      autoenv = pkgs.writeShellApplication {
        name = "autoenv";
        runtimeInputs = with pkgs; [
          ghc
          haskellPackages.stack
        ];
        text = ''
          file=$(realpath "$1")
          cd "$AUTOENV_ROOT" && stack build --profile && (echo "goFilename \"$file\"" | stack ghci examples/PiForall/Modules.hs)
        '';
      };

    in
    {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          pi-forall
          autoenv

          ghc
          haskellPackages.stack
          haskellPackages.haskell-language-server
          haskellPackages.hlint
          haskellPackages.ormolu
        ];
      };
    }
  );
}
