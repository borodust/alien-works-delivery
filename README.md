# alien-works-delivery

WIP system for delivering Common Lisp applications as executable bundles. For
now it only supports `AppImage` format for Linux and `MSIX` for
Windows, but `.APK` for Android and later MacOSX and iOS bundle formats are planned too.

Delivery process is split into two stages:
1. This system generates source bundle with required lisp sources to build an application
1. User invokes a simple command (e.g. `ccl --eval '(load
   "/path/to/source-bundle.lisp" :external-format :latin-1)'`) to build this
   source bundle into an actual executable bundle

Second step can possibly be performed anywhere appropriate architecture,
required build tools and lisp implementation are available.


## Signing

#### MSIX

Generating new self-signed certificate and installing it as a root cert:
```powershell
$cert = New-SelfSignedCertificate -DnsName "<this-domain>.org", "<that-domain>.io" -Type CodeSigning -Subject "CN=<Company-Name>" -CertStoreLocation "cert:\LocalMachine\My"

Move-Item (Join-Path "cert:\LocalMachine\My" $cert.Thumbprint) -Destination "cert:\LocalMachine\Root"
```

Signing MSIX bundle with the cert generated via above method:
```powershell
SignTool sign /v /fd sha256 /sm /s Root /n <Company-Name> C:\path-to\app.msix
```
