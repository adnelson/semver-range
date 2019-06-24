let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

in
  fetchNixpkgs {
     rev          = "61f0936d1cd73760312712615233cd80195a9b47";
     sha256       = "1fkmp99lxd827km8mk3cqqsfmgzpj0rvaz5hgdmgzzyji70fa2f8";
  }
