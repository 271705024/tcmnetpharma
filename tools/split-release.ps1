<#
.SYNOPSIS
  Split the RDesk-built Windows portable zip into fixed-size parts and emit complete checksums.
.DESCRIPTION
  1) Split into N parts: <base>.zip.001 ... <base>.zip.NNN
  2) Generate merge-parts.bat (copy /b merge + certutil SHA256 verify)
  3) Generate SHA256SUMS.txt containing the SHA256 of EVERY part AND the merged zip
     -- fixes the v0.2.0 issue where SHA256SUMS.txt had only the merged line (audit F3)
.PARAMETER ZipPath
  Full path to the zip to split (e.g. tcmnetpharma-0.2.1-windows-final.zip)
.PARAMETER OutDir
  Output directory for parts, default dist/
.PARAMETER PartBytes
  Bytes per part, default 83886080 (80 MiB)
.EXAMPLE
  powershell -ExecutionPolicy Bypass -File tools/split-release.ps1 `
    -ZipPath "tcmnetpharma-0.2.1-windows-final.zip" -OutDir dist
#>
param(
  [Parameter(Mandatory = $true)] [string]$ZipPath,
  [string]$OutDir = "dist",
  [long]$PartBytes = 83886080
)

$ErrorActionPreference = "Stop"

function Compute-Sha256File ($path) {
  $sha = [System.Security.Cryptography.SHA256]::Create()
  $s   = [System.IO.File]::OpenRead($path)
  try { ($sha.ComputeHash($s) | ForEach-Object { $_.ToString("x2") }) -join "" }
  finally { $s.Close(); $sha.Dispose() }
}

if (-not (Test-Path $ZipPath)) { Write-Error "zip not found: $ZipPath"; exit 1 }

$zip     = (Resolve-Path $ZipPath).Path
$zipName = Split-Path $zip -Leaf
$base    = [System.IO.Path]::GetFileNameWithoutExtension($zipName)

if (-not (Test-Path $OutDir)) { New-Item -ItemType Directory -Path $OutDir | Out-Null }

$reader = [System.IO.File]::OpenRead($zip)
$buf    = New-Object byte[] $PartBytes
$lines  = [System.Collections.Generic.List[string]]::new()
$parts  = [System.Collections.Generic.List[string]]::new()
$i = 1
try {
  while (($read = $reader.Read($buf, 0, $buf.Length)) -gt 0) {
    $partName = "{0}.zip.{1:D3}" -f $base, $i
    $partPath = Join-Path $OutDir $partName
    $writer   = [System.IO.File]::Create($partPath)
    try { $writer.Write($buf, 0, $read) } finally { $writer.Close() }
    $hash = Compute-Sha256File $partPath
    $lines.Add("$hash  $partName")
    $parts.Add($partName)
    Write-Host ("  wrote {0,-44} {1,12} B  sha256 {2}" -f $partName, $read, $hash.Substring(0, 16))
    $i++
  }
} finally { $reader.Close() }

# Whole-file hash of the merged zip (matches the last line of SHA256SUMS.txt)
$mergedHash = Compute-Sha256File $zip
$lines.Add("$mergedHash  $zipName")

# Write SHA256SUMS.txt: parts first, merged last
Set-Content -Path (Join-Path $OutDir "SHA256SUMS.txt") -Value $lines -Encoding ASCII
Write-Host ("  wrote SHA256SUMS.txt  ({0} lines = {1} parts + 1 merged)" -f $lines.Count, $parts.Count)

# Write merge-parts.bat
$mergeLine = "copy /b " + ($parts -join " + ") + " $zipName"
$merge = @"
@echo off
REM Merge all .zip.00X parts back into $zipName
REM Usage: keep this file together with all parts in one folder, then double-click
$mergeLine
certutil -hashfile $zipName SHA256
"@
Set-Content -Path (Join-Path $OutDir "merge-parts.bat") -Value $merge -Encoding ASCII
Write-Host ("  wrote merge-parts.bat  (merge {0} parts -> {1})" -f $parts.Count, $zipName)

Write-Host ""
Write-Host "Done. dist/ contents:"
Get-ChildItem $OutDir | ForEach-Object { Write-Host ("  {0,-44} {1,12} B" -f $_.Name, $_.Length) }
