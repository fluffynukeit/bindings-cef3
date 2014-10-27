{ haskellPackages ? (import <nixpkgs> {}).haskellPackages 
, gtk ? (import <nixpkgs> {}).gnome.gtk
, pkgconfig ? (import <nixpkgs> {}).pkgconfig
}:
let inherit (haskellPackages) cabal bindingsDSL;

in cabal.mkDerivation (self: {
  pname = "bindings-cef3";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ pkgconfig bindingsDSL gtk ];
  meta = {
    homepage = "https://github.com/fluffynukeit/bindings-cef3";
    description = "Raw bindings for Chromium Embedded Framework CEF3";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
