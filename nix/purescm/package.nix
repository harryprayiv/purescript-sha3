{
  lib,
  buildNpmPackage,
  fetchNpmDeps,
  fetchurl,
  testers,
}:

let
  packageLock = builtins.fromJSON (builtins.readFile ./manifests/package-lock.json);

  pname = "purescm";
  version = packageLock.packages."node_modules/${pname}".version;

  # My bytevector.ss file — fetch directly from my fork
  bytevector-ss = fetchurl {
    url = "https://raw.githubusercontent.com/harryprayiv/purescm/bytevector/lib/purescm/bytevector.ss";
    hash = "sha256-sbf2CyJok+tiyf/hwOOaDcn3G9tixBf+wjsZUg9SZdE=";  # build once, get the real hash
  };

  package = buildNpmPackage {
    inherit pname version;

    src = ./manifests;
    dontNpmBuild = true;

    npmDeps = fetchNpmDeps {
      src = ./manifests;
      hash = "sha256-e8BDTCp7PsriJaUivdm5IdRcdVDf9urtKnTYu1Mr2oQ=";
    };

    installPhase = ''
      mkdir -p $out/share/${pname}
      cp -r node_modules/ $out/share/${pname}
      ln -s $out/share/${pname}/node_modules/.bin $out/bin

      # Patch hardcoded ICU version suffix to match nixpkgs ICU
      sed -i 's/_74"/_76"/g' $out/share/${pname}/node_modules/${pname}/lib/${pname}/pstring.ss

      # Add bytevector runtime library from my fork
      cp ${bytevector-ss} $out/share/${pname}/node_modules/${pname}/lib/${pname}/bytevector.ss
    '';

    passthru.tests = {
      version = testers.testVersion { inherit package; };
    };

    meta = {
      description = "Chez Scheme back-end for PureScript";
      homepage = "https://github.com/harryprayiv/purescm";
      license = lib.licenses.asl20;
      mainProgram = "purescm";
    };
  };
in
package