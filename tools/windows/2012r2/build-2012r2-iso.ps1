# -*- powershell -*-
# To run this script:
# powershell -executionpolicy bypass -file build-2012r2-iso.ps1  [Your Windows .iso name] [New Windows .iso name]
#
#
param(
    [Cmdletbinding(PositionalBinding = $false)]
    [string]$srcisoname,
    [string]$destisoname
)

function test-administrator {
    $identity  = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($Identity)
    $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function get-currentdirectory {
    $thisName = $MyInvocation.MyCommand.Name
    [IO.Path]::GetDirectoryName((Get-Content function:$thisName).File)
}

function check-validisoname ([string] $isvalidiso) {
    $filename = [System.IO.Path]::GetFileName($isvalidiso)
    $extension = [System.IO.Path]::GetExtension($isvalidiso)
    $test = $extension.CompareTo('.iso')
    $test2 = $filename.Length
    if(($test -eq '0') -and ($test2 -ne '0') -and ($filename -ne '.iso')) {
        return $true
    } else {
        return $false
    }
}

if (-not (test-administrator)) {
    write-error @"
You must be running as administrator for this script to function.
Unfortunately, we can't reasonable elevate privileges ourselves
so you need to launch an administrator mode command shell and then
re-run this script yourself.  
"@
    exit 1
}

if(!$destisoname) {
    $destisoname = 'rebar-windows-2012r2.iso'
} 

if(-not (check-validisoname $destisoname)) {
    write-host 'Error: Final iso name entered ' $destisoname ' is not a valid iso name.'
    exit 1
}

# Check to see that the Windows 8.1 ADK is installed.
write-host "Looking for the Windows 8.1 ADK"
$adk = @([Environment]::GetFolderPath('ProgramFilesX86'),
         [Environment]::GetFolderPath('ProgramFiles')) |
           % { join-path $_ 'Windows Kits\8.1\Assessment and Deployment Kit\Windows Preinstallation Environment\amd64' } |
           ? { test-path  $_ } |
           select-object -First 1
if(!$adk) {
    write-error "No ADK found in default location."
    write-error "Please download and install the Windows ADK for Windows 8.1 from:"
    write-error "  https://www.microsoft.com/en-us/download/details.aspx?id=39982"
    exit 1
} else {
    write-host "Found ADK for Windows 8.1 in the default install location."
}


# Set some basic location information 
$cwd    = get-currentdirectory
$output = join-path $cwd "rebar-winpe"
$mount  = join-path $cwd "rebar-winpe-mount"
$srcisoloc = $cwd + '\' + $srcisoname

#Check for and mount the source iso
if(check-validisoname $srcisoname) {
    if(-not(test-path $srcisoloc)) {
        write-error 'The source iso location does not exist, ensure the iso is located in the same folder as this script'
        write-error 'Could not locate .iso: ' $srcisoloc
        exit 1
    }
} else {
    write-error 'The user iso name ' $srcisoname ' is not valid.'
    exit 1
}

Mount-DiskImage -ImagePath $srcisoloc -Verbose

# Copy needed files from the source iso
$ciminstance = Get-DiskImage -ImagePath $srcisoloc
$info = Get-Volume -DiskImage $ciminstance 
$driveletter = $info.DriveLetter
$isodirectory = $driveletter + ":\*"
Write-Host 'Iso mounted at: ' $driveletter ' and iso directory is: ' $isodirectory

$windowsdirectory = Join-Path $cwd "\workingisofiles"
if (test-path $windowsdirectory) {
    Remove-Item $windowsdirectory -Recurse -Force
}

New-Item $windowsdirectory -type directory
Copy-Item $isodirectory $windowsdirectory -Recurse

#Find boot.wim and install.wim, copy to where we need, and ensure writeable
$bootwimloc = $windowsdirectory + "\sources\boot.wim"
$installwimloc = $windowsdirectory + "\sources\install.wim"
$file = Get-Item $bootwimloc
$file.IsReadOnly = $false
$file = Get-Item $installwimloc
$file.IsReadOnly = $false
Copy-Item $bootwimloc $cwd
Copy-Item $installwimloc $cwd

#Unmount customer's .iso
Write-host 'Unmounting customer windows .iso'
Dismount-DiskImage -ImagePath $srcisoloc

# Path to the clean WinPE WIM file.
$wim = join-path $adk "en-us\winpe.wim"

# Root for the CAB files for optional features.
$packages = join-path $adk "WinPE_OCs"

#For windows 2008 we need to add the missing fonts for winpe
$fonts = join-path $adk '\Media\EFI\Microsoft\Boot\Fonts\*'
$fontsdir = Join-Path $windowsdirectory '\boot\fonts'
Copy-Item $fonts $fontsdir -Force

write-host "Make sure our working and output directories exist."

if (test-path -path $mount) {
    Dismount-WindowsImage -path $mount -discard
    Remove-Item $mount -Recurse -Force
}
new-item -type directory $mount

if (test-path -path $output) {
    Remove-Item $output -Recurse -Force
}
new-item -type directory $output

#Copy the clean ADK WinPE image into our output area.
copy-item $wim $output
# Update our wim location...
$wim = join-path $output "winpe.wim"

# Start hacking up the WinPE image
import-module dism
write-host "Mounting the winpe.wim image."
mount-windowsimage -imagepath $wim -index 1 -path $mount -erroraction stop

write-host "Adding powershell and dependencies to winpe.wim"
# This order is documented in http://technet.microsoft.com/library/hh824926.aspx
@('WinPE-WMI',
  'WinPE-NetFX',
  'WinPE-Scripting',
  'WinPE-PowerShell',
  'WinPE-StorageWMI',
  'WinPE-DismCmdlets') | foreach {
    $item = $_
    write-host "installing $item to image"
    $pkg = join-path $packages "$item.cab"
    add-windowspackage -packagepath $pkg -path $mount
    $pkg = join-path $packages "en-us\${item}_en-us.cab"
    add-windowspackage -packagepath $pkg -path $mount
}
# Copy bootmgr.exe to root of winpe.wim, this is fix for ADK 8.1 & iPXE
# not supporting compression
$bootmgrsource = Join-Path $mount "Windows\Boot\PXE\bootmgr.exe"
if(Test-path $bootmgrsource) {
    Copy-Item $bootmgrsource $mount
    Write-Host "Copying bootmgr.exe to output"
    Copy-Item $bootmgrsource $output
} else {
    Write-error "Bootmgr.exe was not successfully copied."
    Write-error "If using windows 2012 R2, deployment will not succeed."
}

write-host "Adding Drivers to the image"
$drivers  = join-path $cwd "Drivers"
Add-WindowsDriver -Path $mount -Driver "$drivers" -Recurse
Copy-Item $drivers $mount

# Download the Rebar CLI and place it in the winpe image
write-host "Adding Rebar CLI to the WinPE Image"
Invoke-WebRequest -Uri 'https://s3-us-west-2.amazonaws.com/rebar-bins/master/windows/amd64/rebar' -Outfile "${mount}\Windows\rebar.exe"

write-host "Writing stage0.ps1 startup script to winpe.wim"
$file = join-path $mount "stage0.ps1"
set-content $file @'
# -*- powershell -*-#

# This is arguably the wrong thing to do, but it will work well enough for
# initial prototyping.
$server = get-wmiobject win32_networkadapterconfiguration |
  where { $_.ipaddress -and
          $_.dhcpenabled -eq "true" -and
          $_.dhcpleaseobtained } |
            select -uniq -first 1 -expandproperty dhcpserver
$env:REBAR_ENDPOINT = "https://${server}"
$env:REBAR_KEY = "rebar:rebar1"

# Figure out who we are
$node = (& rebar whoami) |out-string |convertfrom-json
if (-Not $node -Or -Not $node.uuid) {
    write-host "Failed to figure out who we are.  Exiting"
    exit
}

$uuid = $node.uuid
$name = $node.name
$share = "\\${server}\tftpboot"
$install = "Q:\machines\${uuid}\stage1.ps1"
write-host "I am ${name}"
$shareMounted = false
write-host "Mounting share ${share} at Q:"
while (-Not $shareMounted) {
    New-SmbMapping -LocalPath Q: -RemotePath $share
    $shareMounted = $?
    Start-Sleep -Seconds 10
}
write-host "Running stage1 installer"
& $install
'@

write-host "Writing Windows\System32\startnet.cmd script to winpe.wim"
$file = join-path $mount "Windows\System32\startnet.cmd"
set-content $file @'
@echo off
echo starting wpeinit to detect and boot network hardware
wpeinit
echo starting the rebar client
powershell -executionpolicy bypass -noninteractive -file %SYSTEMDRIVE%\stage0.ps1
echo dropping to a command shell now...
'@

write-host "Unmounting and saving the Winpe.wim image"
dismount-windowsimage -save -path $mount -erroraction stop -loglevel WarningsInfo -logpath (Join-Path $cwd "winpe.log")
if (! $?) { exit 1 }

write-host "Starting to work with boot.wim and install.wim"
$bootwimsource = Join-Path $cwd "boot.wim"
$installwimsource = Join-Path $cwd "install.wim"

#test boot.wim and install.wim path and exit if not null
if(-not(Test-path $bootwimsource) -or -not(Test-path $installwimsource)) {
    Write-error "No Windows boot.wim or install.wim files present, exiting"
    Write-error "This should never happen"
    exit 1
} 

# Handle boot.wim first.
Copy-Item $bootwimsource $output
$wim = join-path $output "boot.wim"
write-host "mounting $wim image index 2 at $mount"
mount-windowsimage -imagepath $wim -index 2 -path $mount -erroraction stop
Add-WindowsDriver -Path $mount -Driver "$drivers" -Recurse
dismount-windowsimage -save -path $mount -erroraction stop

# Handle install.wim second.
Copy-Item $installwimsource $output
$wim = join-path $output "install.wim"
Get-WindowsImage -ImagePath $wim| foreach {
    $image = $_."ImageName"
    write-host "mounting $wim image $image at $mount"
    mount-windowsimage -imagepath $wim -name $image -path $mount -erroraction stop
    Add-WindowsDriver -Path $mount -Driver "$drivers" -Recurse
    dismount-windowsimage -save -path $mount -erroraction stop
    Write-Host "unmounted $wim image $image"
}

# Paranoia is the better part of virtue here.
Clear-WindowsCorruptMountPoint

#Copy our updated boot.wim, install.wim, winpe.wim, and bootmgr.exe to image
$originalpe = $cwd + "\rebar-winpe\winpe.wim"
$rebarpe = $cwd + "\rebar-winpe\rebar-winpe.wim"
$finalbootwimdir = $cwd + "\rebar-winpe\boot.wim"
$finalinstallwimdir = $cwd + "\rebar-winpe\install.wim"
$bootmgrdir = $cwd + "\rebar-winpe\bootmgr.exe"
Rename-Item $originalpe $rebarpe
Copy-Item $finalbootwimdir $bootwimloc
Copy-Item $finalinstallwimdir $installwimloc
Copy-Item $rebarpe $windowsdirectory
Copy-Item $bootmgrdir $windowsdirectory

#Build a new .iso
$destisodirectory = $cwd + '\' + $destisoname
$oscdimgloc = 'C:\Program Files (x86)\Windows Kits\8.1\Assessment and Deployment Kit\Deployment Tools\amd64\oscdimg\oscdimg.exe'
$arg1 = '-m'
$arg2 = '-h'
$arg3 = '-o'
$arg4 = '-u2'
$arg5 = '-udfver102'
$arg6 = '-l<Windows>'
$arg7 = '-bootdata:2#p0,e,b"' + $windowsdirectory + '\boot\ETFSBOOT.COM"#pEF,e,b"' + $windowsdirectory + '\efi\microsoft\boot\efisys.bin"' 
Write-Host "*************************"
Write-Host "Calling OSCDCMD: " + $oscdimgloc
& $oscdimgloc $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $windowsdirectory $destisodirectory

#Cleanup unneeded files
Write-Host 'Deleting files no longer needed.'
Remove-Item $output -recurse -force
Remove-Item $mount -recurse -force
Remove-Item $windowsdirectory -Recurse -Force
$bootwimloc = $cwd + '\boot.wim'
$installwimloc = $cwd + '\install.wim'
Remove-Item $bootwimloc
Remove-Item $installwimloc

# Calculate the SHA256sum of the generated ISO
Write-Host "Calculating SHA256 of ${destisoname}"
$hash = Get-Filehash -Algorithm SHA256 -Path $destisodirectory
$file = Join-Path $cwd "${destisoname}.sha256sum"
set-content $file $hash.hash.ToLower()