with import <nixpkgs> {} ;
stdenv.mkDerivation {
 name  = "nuxx";
 version = "0.1";
 src = ./.;
 nativeBuildInputs = [ pkgs.elmPackages.elm pkgs.nodejs ] ;
}
