let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

in
  fetchNixpkgs {
     rev          = "e1ad1a0aa2ce6f9fd951d18181ba850ca8e74133";
     sha256       = "0vk5sjmbq52xfrinrhvqry53asl6ppwbly1l7ymj8g0j4pkjf7p1";
  }
