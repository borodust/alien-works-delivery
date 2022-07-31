###
### SBCL
###
Invoke-WebRequest -UseBasicParsing -Uri https://github.com/roswell/sbcl_bin/releases/download/2.2.6/sbcl-2.2.6-x86-64-windows-binary.msi -OutFile c:/sbcl.msi

$sbcl_install_args = "/i `"c:\sbcl.msi`" /quiet /passive"
Start-Process msiexec.exe -ArgumentList $sbcl_install_args -Wait
Remove-Item -Path C:/sbcl.msi

$SBCL = "C:/Program Files/Steel Bank Common Lisp/sbcl.exe"

###
### QUICKLISP
###
Invoke-WebRequest -UseBasicParsing -Uri https://beta.quicklisp.org/quicklisp.lisp -OutFile C:/quicklisp.lisp
