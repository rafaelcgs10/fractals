let
  pkgs = import
    (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/386234e2a61e1e8acf94dfa3a3d3ca19a6776efb.tar.gz) { };
  rustPackages = [
    pkgs.cargo
    pkgs.rustc
    pkgs.rustfmt
    pkgs.clippy
    pkgs.lldb
    pkgs.gdb
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = rustPackages;
}
