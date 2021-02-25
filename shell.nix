{ pkgs ? import <nixpkgs> { } }:
let
  needed = with pkgs; [
    stack
    ghc
    clang-tools # for C formatting
    nodePackages.prettier
    haskellPackages.profiteur # treemap of profile
  ];
in
pkgs.mkShell {
  buildInputs = needed ++ [
    (pkgs.writeShellScriptBin
      "stack-watch-test"
      ''
        PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
        stack test --file-watch
      '')
    (pkgs.writeShellScriptBin
      "stack-run"
      ''
        PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
        if [ -z $1 ]; then echo "give a program path!";exit 1; fi
        set -ex
        stack build
        stack exec gaiwan-exe eval $1
      '')
    (pkgs.writeShellScriptBin
      "stack-profile"
      ''
        PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
        if [ -z $1 ]; then echo "Give a program path" ;exit 1; fi
        set -ex
        stack clean
        stack build --profile
        stack exec --profile gaiwan-exe eval $1 --RTS -- +RTS -p
        profiteur gaiwan-exe.prof
      '')
  ];
}
