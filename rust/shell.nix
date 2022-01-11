let
  pkgs = import
    (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/93184413f2180ce79d53df91df4d43c2e8f931aa.tar.gz) { };
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
