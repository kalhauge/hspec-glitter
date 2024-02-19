{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
    nix-filter.url = github:numtide/nix-filter;
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    ...
  } @ inputs: let
    packages = final: p: {
      "hspec-glitter" =
        p.callCabal2nixWithOptions "hspec-glitter"
        (nix-filter.lib {root = self;}) "" {};
    };
    overlays = final: prev: {
      haskellPackages = prev.haskellPackages.extend (p: _: packages final p);
    };
  in
    {
      overlays.default = overlays;
    }
    // flake-utils.lib.eachDefaultSystem
    (system: let
      hpkgs =
        (import nixpkgs {
          inherit system;
          overlays = [overlays];
        })
        .haskellPackages;
    in rec {
      packages = {
        default = hpkgs.hspec-glitter;
        hspec-glitter = hpkgs.hspec-glitter;
      };
      devShells = let
        nativeBuildInputs = with hpkgs; [
          cabal-install
          ghcid
          haskell-language-server
          hpack
          fourmolu
          hspec-golden
        ];
        withHoogle = true;
      in {
        default =
          hpkgs.shellFor
          {
            name = "hspec-glitter-shells";
            packages = p: [p.hspec-glitter];
            doBenchmark = true;
            inherit nativeBuildInputs withHoogle;
          };
      };
    });
}
