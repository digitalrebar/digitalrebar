# -*- powershell -*-
# To run this script:
# powershell -executionpolicy bypass -file build-2012r2-iso.ps1  [Your Windows .iso name] [New Windows .iso name]
#
#
param(
    [Cmdletbinding(PositionalBinding = $false)]
    [string]$bootenv_name,
    [string]$destisoname
)

# Calculate the SHA256sum of the generated ISO.
Write-Host "Calculating SHA256 of ${destisoname}"
$hash = Get-Filehash -Algorithm SHA256 -Path $destisoname
$bootenv = (& .\rebar.exe provisioner bootenvs show $bootenv_name) |out-string |convertfrom-json

if (-not $bootenv -or -not $bootenv.OS.IsoSha256) {
    write-error "Unable to fetch boot environment for ${bootenv_name}."
    exit 1
}

if ($bootenv.OS.IsoSha256 -eq $hash.hash.ToLower()) {
     write-host "SHA256 unchanged, exiting early."
     exit 0
}

write-host "Uploading ${destisoname} to Rebar"
& .\rebar.exe provisioner isos upload $destisoname as $bootenv.OS.IsoFile
if ($LASTEXITCODE -ne 0) {
    write-error "Failed to upload ISO"
    exit 1
}

$hashVal = $hash.hash.ToLower()
write-host "Updating ${bootenv_name} ISO SHA256 to ${hashVal}"
@"
{ "OS": {"IsoSha256": "${hashVal}"}}
"@ | & .\rebar.exe provisioner bootenvs update $bootenv_name -

if ($LASTEXITCODE -ne 0) {
    write-error "Failed to update ${bootenv_name}"
    exit 1
}