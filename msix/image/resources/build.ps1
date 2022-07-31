param(
    [Parameter(Mandatory)]
    [string]$Type,

    [Parameter(Mandatory)]
    [string]$Source,

    [Parameter(Mandatory)]
    [string]$Target
)

echo "Building artifact of type '$Type' from '$Source' into '$Target'"

$TmpWorkFile = New-TemporaryFile
$TmpWorkDir = (New-Item -Path ($TmpWorkFile.FullName + '.dir') -Type "directory").FullName

if ( "$Source" -like '*.zip' ) {
    Expand-Archive "$Source" "$TmpWorkDir"
} elseif ( "$Source" -like '*.tar.gz' ) {
    Push-Location -Path "$TmpWorkDir"
    tar -xf "$Source"
    Pop-Location
} else {
    echo "Unrecognized archive type: *.zip or *.tar.gz file expected, but got '$Source'"
    exit 1
}

$env:ALIEN_WORKS_DELIVERY_BUNDLE_TARGET_DIR=$Target
& 'C:/Program Files/Steel Bank Common Lisp/sbcl.exe' --script "$TmpWorkDir/delivery-bundle/deliver.lisp" $Type
