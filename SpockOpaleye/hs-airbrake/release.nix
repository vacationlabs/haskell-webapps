{ supportedPlatforms ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCompilers ? ["ghc6104" "ghc6123" "ghc704" "ghc722" "ghc742" "ghc763" "ghc783" "ghcHEAD"]
}:

let
  genAttrs = (import <nixpkgs> {}).lib.genAttrs;
in
{
  airbrake = genAttrs supportedCompilers (ghcVer: genAttrs supportedPlatforms (system:
    let
      pkgs = import <nixpkgs> { inherit system; };
      haskellPackages = pkgs.lib.getAttrFromPath ["haskellPackages_${ghcVer}"] pkgs;
    in haskellPackages.callPackage ./default.nix {}
  ));
}
