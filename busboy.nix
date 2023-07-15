{ mkDerivation, aeson, async, base, bytestring, co-log, containers
, generic-lens, http-client, http-client-tls, lens, lib, lucid
, optparse-applicative, scientific, servant, servant-client
, servant-lucid, servant-server, sqlite-simple, text, time, vector
, wai, warp
}:
mkDerivation {
  pname = "busboy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring co-log containers generic-lens
    http-client http-client-tls lens lucid scientific servant
    servant-client servant-lucid servant-server sqlite-simple text time
    vector wai warp
  ];
  executableHaskellDepends = [
    aeson base bytestring co-log containers http-client http-client-tls
    lucid optparse-applicative scientific servant servant-client
    servant-lucid servant-server text time vector wai warp
  ];
  testHaskellDepends = [
    aeson base bytestring containers http-client http-client-tls lucid
    scientific servant servant-client servant-lucid servant-server text
    time vector wai warp
  ];
  homepage = "https://github.com/undergroundquizscene/busboy#readme";
  license = lib.licenses.bsd3;
  mainProgram = "busboy-exe";
}
