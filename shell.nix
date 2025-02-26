let
  commit = "48dcbaf7fa799509cbec85d55b8d62dcf1477d57";
  link =  "https://github.com/NixOS/nixpkgs/archive/${commit}.zip";
  tarball = fetchTarball link;
in
{ pkgs ? import tarball {} }:

pkgs.mkShell rec {
  name = "steight-shell";

  buildInputs = [
    pkgs.ghc
  ];

  shellHook = ''
    export PS1="\n\[\033[1;32m\][${name}: \w]\n\$\[\033[0m\] "
    echo '>> Welcome to ${name}! <<'
    cd src
    echo Entering ghci "(in $(pwd)) and loading main.hs!"
    ghci -Wall -Werror main.hs
    exit $?
  '';
}
