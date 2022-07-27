Required tools to make MSIX bundle from delivery one:
* Windows 10+ x64 OS (PowerShell)
* [MakeAppx](https://github.com/microsoft/msix-packaging)
* CL implementation: CCL, SBCL, ECL


```sh
SignTool sign /fd SHA256 /v /sm /s My /n Alien-Works <package>.msix
```