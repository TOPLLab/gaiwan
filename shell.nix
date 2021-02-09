{ pkgs ? import <nixpkgs> { } }:
let
  needed = with pkgs; [
    stack
    ghc
    clang-tools
    nodePackages.prettier
    haskellPackages.profiteur
  ];
  stack-run = pkgs.writeShellScriptBin "stack-profile" ''
    PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
    if [ -z $1 ]; then echo "give a program path!";exit 1; fi
    set -ex
    stack build
    cat $1 | stack exec gaiwan-exe
  '';
  stack-profile = pkgs.writeShellScriptBin "stack-profile" ''
    PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
    if [ -z $1 ]; then echo "Give a program path" ;exit 1; fi
    set -ex
    stack clean
    stack build --profile
    cat $1 | stack exec --profile gaiwan-exe --RTS -- +RTS -p
    profiteur gaiwan-exe.prof
  '';
in
pkgs.mkShell {
  buildInputs = needed ++ [
    stack-profile
  ];
}
