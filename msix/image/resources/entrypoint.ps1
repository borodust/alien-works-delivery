param(
    [Parameter(Mandatory)]
    [string]$Operation,

    [Parameter(mandatory=$false,
               ValueFromRemainingArguments=$true)]
    $Remaining
)

switch ( $Operation ) {
    'build' { ./build.ps1 @Remaining }
    'sign' { ./sign.ps1 @Remaining }
    default {
        echo "Unexpected operation: '$Operation'"
        exit 1
    }
}
