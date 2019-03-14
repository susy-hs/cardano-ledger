{ chainPackages ? import ../.. { }
, pkgs ? chainPackages._lib.pkgs
, buildTools ? with pkgs; [ git nix gnumake ]
}:

with pkgs.lib;
with pkgs;

let
  stackRebuild = runCommand "stack-rebuild" {} ''
    ${haskellPackages.ghcWithPackages (ps: [ps.turtle ps.safe ps.transformers])}/bin/ghc -o $out ${./rebuild.hs}
  '';

  buildTools' = buildTools ++ [
     chainPackages._lib.cache-s3
     chainPackages._lib.stack-hpc-coveralls
     stack gnused coreutils
  ];
in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath buildTools'}
    exec ${stackRebuild} "$@"
  ''
