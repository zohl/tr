{ mkDerivation, aeson, base, blaze-html, blaze-markup, bytestring
, cmdargs, containers, data-default, dictionaries, directory
, filepath, hsyslog, ini, servant, servant-blaze, servant-server
, stdenv, text, time, unix, unordered-containers, warp
, warp-autoquit, warp-socket-activation
}:
mkDerivation {
  pname = "tr";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring cmdargs containers
    data-default dictionaries directory filepath hsyslog ini servant
    servant-blaze servant-server text time unix unordered-containers
    warp warp-autoquit warp-socket-activation
  ];
  license = stdenv.lib.licenses.bsd3;
}
