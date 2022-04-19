# Provide almost the same arguments as the actual nixpkgs.
# This allows us to further configure this nixpkgs instantiation in places where we need it.
# In particular, `stack` needs this to be a function.
{ overlays ? [ ]  # additional overlays
, config ? { } # Imported configuration
}:
let
  sources = import ./sources.nix;

  nixpkgs = import sources.nixpkgs {
    overlays = [(import ./overlay.nix)] ++ overlays;
    config = {
      imports = [ config ];
    };
  };
in
nixpkgs
