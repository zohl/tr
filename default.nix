{ mkDerivation, aeson, base, bytestring, cmdargs, containers
, dictionaries, directory, filepath, hsyslog, ini, servant
, servant-server, stdenv, text, time, warp, warp-autoquit
, warp-socket-activation
}:
mkDerivation {
  pname = "tr";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring cmdargs containers dictionaries directory
    filepath hsyslog ini servant servant-server text time warp
    warp-autoquit warp-socket-activation
  ];
  license = stdenv.lib.licenses.bsd3;
}
