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
        stack test --file-watch "$@"
      '')
    (pkgs.writeShellScriptBin
      "stack-bench"
      ''
        PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
        set -ex
        stack build
        stack bench
      '')
    (pkgs.writeShellScriptBin
      "stack-run"
      ''
        PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
        if [ -z $1 ]; then echo "give a program path!";exit 1; fi
        set -ex
        stack build
        stack exec gaiwan-exe $@
      '')
    (pkgs.writeShellScriptBin
      "stack-profile"
      ''
        PATH=${pkgs.lib.makeBinPath needed}:"$PATH"
        if [ -z $1 ]; then echo "Give a action" ;exit 1; fi
        if [ -z $2 ]; then echo "Give a program path" ;exit 1; fi
        set -ex
        # stack clean
        stack build --profile gaiwan
        stack exec --profile gaiwan-exe $@ --RTS -- +RTS -p
        profiteur gaiwan-exe.prof
      '')
  ];
}
