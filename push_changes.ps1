# Push the current repository state safely.
# Usage:
#   .\\push_changes.ps1 -Message "describe the change" [-Remote origin] [-Branch main]

[CmdletBinding()]
param(
  [string]$Message = "Sync local changes",
  [string]$Remote = "origin",
  [string]$Branch = $(git rev-parse --abbrev-ref HEAD)
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# Work from the repo that contains this script.
$repoRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $repoRoot

if (Test-Path .git/index.lock) {
  Write-Host "Removing stale index.lock..."
  Remove-Item .git/index.lock -Force
}

Write-Host "Staging changes (respecting .gitignore)..."
git add -A

# If nothing is staged, bail out gracefully.
if (git diff --cached --quiet) {
  Write-Host "No staged changes to commit. Exiting."
  exit 0
}

Write-Host "Committing with message: $Message"
git commit -m $Message

Write-Host "Pushing to $Remote/$Branch..."
git push $Remote $Branch

Write-Host "Push complete."
