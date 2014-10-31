{ haskellPackages ? (import <nixpkgs> {}).haskellPackages 
, gtk ? (import <nixpkgs> {}).gnome.gtk
, pkgconfig ? (import <nixpkgs> {}).pkgconfig
, x11 ? (import <nixpkgs> {}).x11
, xlibs ? (import <nixpkgs> {}).xlibs
, nss ? (import <nixpkgs> {}).nss
, nspr ? (import <nixpkgs> {}).nspr
, expat ? (import <nixpkgs> {}).expat
, cups ? (import <nixpkgs> {}).cups
, gnome3 ? (import <nixpkgs> {}).gnome3
, alsaLib ? (import <nixpkgs> {}).alsaLib
, dbusLibs ? (import <nixpkgs> {}).dbus_glib
, udev ? (import <nixpkgs> {}).udev
, libgcrypt ? (import <nixpkgs> {}).libgcrypt
, libcap ? (import <nixpkgs> {}).libcap
}:
let inherit (haskellPackages) cabal bindingsDSL;

in cabal.mkDerivation (self: {
  pname = "bindings-cef3";
  version = "0.1.0";
  src = ./.;
  extraLibraries = [gtk x11 
                    xlibs.libXrender
                    xlibs.libXtst
                    xlibs.libXi
                    xlibs.libXcomposite
                    xlibs.libXfixes
                    xlibs.libXdamage
                    xlibs.libXcursor
                    xlibs.libXext
                    nss
                    nspr
                    expat
                    cups
                    gnome3.gconf
                    alsaLib
                    dbusLibs
                    udev
                    libgcrypt
                    libcap
                    ];

  buildDepends = [ pkgconfig bindingsDSL ];
  meta = {
    homepage = "https://github.com/fluffynukeit/bindings-cef3";
    description = "Raw bindings for Chromium Embedded Framework CEF3";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
