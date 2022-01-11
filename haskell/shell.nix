{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    GLUT
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    nixpkgs.haskellPackages.haskell-language-server
    nixpkgs.haskellPackages.hlint
    nixpkgs.haskellPackages.hoogle
    nixpkgs.haskellPackages.stack
    nixpkgs.haskellPackages.happy
    nixpkgs.haskellPackages.haskell-src-exts
    nixpkgs.haskellPackages.apply-refact
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
  LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";
}
