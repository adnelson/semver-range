{ nixpkgs ? (import ./nix/17_09.nix) }:

let

  config   = { allowUnfree = true; };

  overlays = [
    (newPkgs: oldPkgs: {
      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          semver-range = 
            haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    })
  ];

  pkgs = import nixpkgs { inherit config overlays; };

in

  { inherit (pkgs.haskellPackages) semver-range; }
