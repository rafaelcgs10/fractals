{ mkDerivation, base, GLUT, hpack, lib }:
mkDerivation {
  pname = "mandelbrot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base GLUT ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base GLUT ];
  testHaskellDepends = [ base GLUT ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/mandelbrot#readme";
  license = lib.licenses.bsd3;
}
