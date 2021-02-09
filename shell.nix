{ pkgs ? import <nixpkgs> { } }:
let
  needed = with pkgs; [
    stack
    ghc
    clang-tools
    nodePackages.prettier
    haskellPackages.profiteur
  ];
  stack-profile = pkgs.writeShellScriptBin "stack-profile" ''
    PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
    if [ -z $1 ]; then exit 0; fi
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
