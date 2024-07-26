{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  };
  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" ];
    in
    builtins.foldl'
      (ss: s:
        let
          pkgs = nixpkgs.legacyPackages."${s}";
        in
        ss //
        {
          devShells."${s}".default = pkgs.mkShell {
            packages = [
              pkgs.zlib
              pkgs.darwin.apple_sdk.frameworks.CoreServices
              pkgs.darwin.apple_sdk.frameworks.Cocoa

              pkgs.haskell.compiler.ghc98
              pkgs.cabal-install
              (pkgs.haskell-language-server.override { supportedGhcVersions = [ "98" ]; })
            ];
          };
          formatter."${s}" = pkgs.nixpkgs-fmt;
        })
      { }
      systems;
}
