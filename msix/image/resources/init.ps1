# Add MakeAppx into path
$PathRegKey = 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment'
$PersistentPath = (Get-ItemProperty -Path $PathRegKey -Name PATH).path
$PersistentPath += ';C:\Program Files (x86)\Windows Kits\10\App Certification Kit\'
Set-ItemProperty -Path $PathRegKey -Name PATH -Value $PersistentPath

$PersistentPath = (Get-ItemProperty -Path $PathRegKey -Name PATH).path
echo "New PATH: $PersistentPath"

$SBCL = "C:/Program Files/Steel Bank Common Lisp/sbcl.exe"
& $SBCL --script "$PSScriptRoot/init.lisp"
