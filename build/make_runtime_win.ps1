# build/make_runtime_win.ps1  --  Makes ovid-runtime-win-x64: a portable R with every package build/renv.lock pins, that runs
# from any folder. "Open ovid.bat" uses it when it sits beside it as runtime\.
#
#   powershell -ExecutionPolicy Bypass -File build\make_runtime_win.ps1             (on Windows 10 or later)
#   ... -Install                                                                    also copies it to runtime\
#   ... -AllowNewer                                                                 accept a package newer than the lockfile
#
# How: CRAN's R installer for the lockfile's R version runs silently into the output folder (no Start menu, registry entry or
# file association; R on Windows finds its home from where it is). Its library gets the lockfile's packages as Windows binaries
# from the snapshot in build/snapshot.txt (build/runtime_packages.R), then it is checked to run on its own and zipped.
# Output in build\out\ (not in git): the folder and a .zip for a release.
param([switch]$Install, [switch]$AllowNewer)
$ErrorActionPreference = "Stop"
Set-Location (Split-Path -Parent $PSScriptRoot)
$Root = (Get-Location).Path
$RVer = (Get-Content build\renv.lock -Raw | ConvertFrom-Json).R.Version
$Name = "ovid-runtime-win-x64"
$Out = Join-Path $Root "build\out\$Name"; $R = Join-Path $Out "R"
$Rs = Join-Path $R "bin\x64\Rscript.exe"

Write-Host "1/5 R $RVer for Windows from CRAN"
$CranDir = Join-Path $Root "build\out\cran"; New-Item -ItemType Directory -Force $CranDir | Out-Null
$Exe = Join-Path $CranDir "R-$RVer-win.exe"
if (-not (Test-Path $Exe)) { Invoke-WebRequest "https://cloud.r-project.org/bin/windows/base/old/$RVer/R-$RVer-win.exe" -OutFile $Exe -UseBasicParsing }
Write-Host "   signature: $((Get-AuthenticodeSignature $Exe).Status)"

Write-Host "2/5 Installing it into $R (silent, this user only, nothing registered)"
if (Test-Path $Out) { Remove-Item -Recurse -Force $Out }
$p = Start-Process $Exe -Wait -PassThru -ArgumentList @("/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART", "/CURRENTUSER", "/NOICONS",
  "/DIR=`"$R`"", "/MERGETASKS=!desktopicon,!recordversion,!associate")
if ($p.ExitCode -ne 0 -or -not (Test-Path $Rs)) { throw "R's installer ended with $($p.ExitCode)" }
foreach ($d in @("doc", "tests")) { if (Test-Path "$R\$d") { Remove-Item -Recurse -Force "$R\$d" } }

# the runtime's own library only: no user or site library
$env:R_LIBS_USER = Join-Path $Out "none"; $env:R_LIBS_SITE = Join-Path $Out "none"; $env:R_HOME = $null

Write-Host "3/5 The packages build/renv.lock pins"
& $Rs build\runtime_packages.R install $Out; if ($LASTEXITCODE -ne 0) { throw "installing the packages failed" }
$a = @("build\runtime_packages.R", "check", $Out); if ($AllowNewer) { $a += "--allow-newer" }
& $Rs @a; if ($LASTEXITCODE -ne 0) { throw "the packages are not as the lockfile says" }

Write-Host "4/5 Checking it runs on its own (no user or site library)"
& $Rs build\runtime_packages.R smoke $Out
if ($LASTEXITCODE -ne 0) { throw "the runtime does not run on its own" }

Write-Host "5/5 Packing"
$Zip = Join-Path $Root "build\out\$Name.zip"; if (Test-Path $Zip) { Remove-Item -Force $Zip }
Push-Location (Join-Path $Root "build\out"); tar.exe -a -cf "$Name.zip" $Name; Pop-Location
$mb = { param($x) "{0:N0} MB" -f ($x / 1MB) }
Write-Host "   $Name $(& $mb ((Get-ChildItem $Out -Recurse -File | Measure-Object Length -Sum).Sum)), zip $(& $mb (Get-Item $Zip).Length)"
if ($Install) { $rt = Join-Path $Root "runtime"; if (Test-Path $rt) { Remove-Item -Recurse -Force $rt }; Copy-Item -Recurse $Out $rt; Write-Host "   installed as runtime\" }
