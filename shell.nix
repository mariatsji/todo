let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/25.11.tar.gz") {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghc
  ];

}
