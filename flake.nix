{
  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

  outputs = { nixpkgs, ... }:
    let pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in {
      devShells."x86_64-linux".default = pkgs.mkShell {
        packages = [
          (pkgs.writeShellScriptBin "run"
            "runhaskell -Wall -Wno-type-defaults -Wno-name-shadowing $1")
        ] ++ (with pkgs.haskellPackages; [
          ghc
          haskell-language-server
          ormolu
          stack
        ]);
      };
    };
}
