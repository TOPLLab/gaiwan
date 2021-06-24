{ pkgs ? import <nixpkgs> { } }:
let
  ltpv = pkgs.stdenv.mkDerivation rec {
    pname = "LTPV";
    version = "0.0.1";

    src = pkgs.fetchFromGitHub {
      owner = "beardhatcode";
      repo = "LTPV";
      rev = "f93a8373d674b0e01744331c432ff19371c7d2bb";
      sha256 = "sha256-xfu0MSzOuk6uNTi7A0FYcOqtS1rVrpm8Q9S8mFXiwdw=";
    };

    buildPhase = ''
      echo $PATH
      cp -r $src/src .
      cd src
      make all
    '';

    installPhase = ''
      cd ..
      mkdir -p $out/{lib,share,bin}
      mv share/* $out/share
      mv src/libLTPV.so $out/lib/
      echo '#!/bin/sh' > $out/bin/ltpv
      echo "LD_PRELOAD=$out/lib/libLTPV.so "'"$@"' >> $out/bin/ltpv
      echo "echo -e '\n\nReport captured, see:\n$out/share/ltpv/index.html'" >> $out/bin/ltpv
      chmod u+x $out/bin/ltpv
    '';

    buildInputs = [
      pkgs.gnumake
      pkgs.gcc
      pkgs.opencl-clhpp # for cl2.hpp
      pkgs.python3
      pkgs.opencl-info
      pkgs.ocl-icd # for -lOpenCL
    ];



  };
  needed = with pkgs; [
    stack
    ghc
    clang-tools # for C formatting
    nodePackages.prettier
    haskellPackages.profiteur # treemap of profile
    pkgs.opencl-clhpp # for cl2.hpp
    pkgs.ormolu
    pkgs.python3
    pkgs.opencl-info
    pkgs.ocl-icd # for -lOpenCL
  ] ++ [ ltpv ];
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
