bindings-cef3
=============

Bindings for Chromium Embedded Framework CEF3 C API

The package mirrors the CEF3 C API, which mostly consists of groups of 
callback functions defined in structures.  Each such structure and callback
function is given its own type.

The package also provides an example executable that closely follows the one
provided by https://github.com/CzarekTomczak/cefcapi.  To build the executable,
use `cabal configure -fbuildExamples`.

Currently, only Linux is supported.  Adding Windows and MacOS support should 
be rather easy, especially for an experienced FFI practitioner, since CEF
itself is multiplatform.  More careful handling of `#defines` in `CEF3/Internal`
and testing is all I expect would be required.  I plan on adding Windows 
support eventually, but contributions are welcome!
