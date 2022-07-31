param(
    [Parameter(Mandatory)]
    [string]$Target,

    [Parameter(Mandatory)]
    [string]$Cert,

    [Parameter(mandatory=$false,
               ValueFromRemainingArguments=$true)]
    $Remaining
)

echo "Signing '$Target' with key '$Cert'"


$ArgTable = @{}
$Remaining | foreach {
    if($_ -match '^-') {
        $Key = ($_ -replace '^-').ToLower()
    } else {
        $ArgTable[$Key] = $_
    }
}


$SignArgs = 'sign', '/fd', 'SHA256', '/v', '/f', "$Cert"

$Password = $ArgTable['password']
if ( "$Password" -ne '' ) {
    echo "Password provided: $Password"
    $SignArgs += "/p", "$Password"
}

echo "Params: $SignArgs $Target"

SignTool @SignArgs "$Target"
