let
  pkgs = import <nixpkgs> { };
in pkgs.mkShell {
  packages = [ pkgs.haskell-language-server
             ];
  inputsFrom = [ (import ./release.nix).busboy.env ];
}
