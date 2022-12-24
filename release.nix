let
  pkgs = import <nixpkgs> { };

in
  { busboy = pkgs.haskellPackages.callPackage ./busboy.nix { };
  }
