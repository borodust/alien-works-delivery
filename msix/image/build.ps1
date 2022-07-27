param(
    [Parameter(Mandatory)]
    [string]$Type,

    [Parameter(Mandatory)]
    [string]$Source,

    [Parameter(Mandatory)]
    [string]$Target
)

echo "Building artifact '$Target' of type '$Type' from '$Source'"

$env:ALIEN_WORKS_DELIVERY_BUNDLE_TARGET_FILE=$Target
& 'C:/Program Files/Steel Bank Common Lisp/sbcl.exe' --script $Source $Type
