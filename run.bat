@echo off
rem Palaiz programmu no projekta mapes (dubultklikšķis vai run.bat terminālī).
rem Tikai testi: run.bat --test
chcp 65001 >nul
cd /d "%~dp0"

if not exist "project.scala" (
  echo project.scala not found - run this file from the project folder.
  pause
  exit /b 1
)

where scala-cli >nul 2>nul
if not errorlevel 1 (
  scala-cli run . -- %*
  goto done
)

where scala >nul 2>nul
if not errorlevel 1 (
  scala run . -- %*
  goto done
)

echo Scala CLI is not installed.
echo Install it with:  winget install -e --id VirtusLab.ScalaCLI
echo Then close and reopen the terminal and run this file again.

:done
pause
