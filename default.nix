{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  haskellPackages = nixpkgs.haskellPackages;
in

haskellPackages.callPackage ./project.nix {}
