@echo off
rem ============================================================================
rem  Ovid_RS.bat  --  starts ovid // RS in your browser.
rem  Double-click it. R is found in this order:
rem    1. R-portable\ inside this folder (made by Make_Portable.bat, so nothing needs installing)
rem    2. the R installed on this computer (registry, then Program Files, then PATH)
rem  Packages live in library\ inside this folder; the first start installs them.
rem ============================================================================
setlocal EnableExtensions
cd /d "%~dp0"
title ovid // RS
set "APPDIR=%~dp0"
set "RSCRIPT="

if exist "%APPDIR%R-portable\bin\x64\Rscript.exe" set "RSCRIPT=%APPDIR%R-portable\bin\x64\Rscript.exe"
if not defined RSCRIPT if exist "%APPDIR%R-portable\bin\Rscript.exe" set "RSCRIPT=%APPDIR%R-portable\bin\Rscript.exe"

if not defined RSCRIPT call :fromreg "HKCU\SOFTWARE\R-core\R64"
if not defined RSCRIPT call :fromreg "HKCU\SOFTWARE\R-core\R"
if not defined RSCRIPT call :fromreg "HKLM\SOFTWARE\R-core\R64"
if not defined RSCRIPT call :fromreg "HKLM\SOFTWARE\R-core\R"
if not defined RSCRIPT call :fromdir "%ProgramFiles%\R"
if not defined RSCRIPT call :fromdir "%LOCALAPPDATA%\Programs\R"
if not defined RSCRIPT for /f "delims=" %%p in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%p"

if not defined RSCRIPT (
  echo.
  echo  R was not found on this computer.
  echo  Install R from https://cran.r-project.org/bin/windows/base/ ^(no admin rights needed:
  echo  choose "Install for me only"^), or ask for the portable copy of this folder, then start again.
  echo.
  pause
  exit /b 1
)

set "MODE="
if /i "%~1"=="/portable" set "MODE=--portable"
"%RSCRIPT%" --vanilla "%APPDIR%launcher\run_app.R" %MODE%
if errorlevel 1 (
  echo.
  echo  Ovid RS stopped with an error ^(see above^).
  pause
)
exit /b 0

:fromreg
for /f "tokens=2,*" %%a in ('reg query "%~1" /v InstallPath 2^>nul ^| find /i "InstallPath"') do (
  if exist "%%b\bin\x64\Rscript.exe" ( set "RSCRIPT=%%b\bin\x64\Rscript.exe" ) else if exist "%%b\bin\Rscript.exe" set "RSCRIPT=%%b\bin\Rscript.exe"
)
exit /b 0

:fromdir
if not exist "%~1" exit /b 0
rem newest first (by date, so R-4.10 beats R-4.9)
for /f "delims=" %%d in ('dir /b /ad /o-d "%~1\R-*" 2^>nul') do (
  if not defined RSCRIPT if exist "%~1\%%d\bin\x64\Rscript.exe" set "RSCRIPT=%~1\%%d\bin\x64\Rscript.exe"
  if not defined RSCRIPT if exist "%~1\%%d\bin\Rscript.exe" set "RSCRIPT=%~1\%%d\bin\Rscript.exe"
)
exit /b 0
