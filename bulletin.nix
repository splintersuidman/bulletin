{ mkDerivation, base, bytestring, containers, filepath, lib, mtl
, pandoc, pandoc-types, text, time, tomland, unordered-containers
}:
mkDerivation {
  pname = "bulletin";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers filepath mtl pandoc pandoc-types text
    time tomland unordered-containers
  ];
  license = lib.licenses.gpl3Plus;
  mainProgram = "bulletin";
}
