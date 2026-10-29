@echo off
rem ============================================================================
rem  Open ovid.bat  --  opens the ovid launcher: one window that starts CS, WLR, GT, GS and RS.
rem  Double-click it. R is found in this order:
rem    1. runtime\ beside this file (the portable R, build\make_runtime_win.ps1: nothing needs installing)
rem    2. the R installed on this computer (registry, then Program Files, then the user's programs, then PATH)
rem  With the installed R, the first start installs the packages build\renv.lock lists (once).
rem ============================================================================
setlocal EnableExtensions
cd /d "%~dp0"
title ovid
set "ROOT=%~dp0"
if "%ROOT:~-1%"=="\" set "ROOT=%ROOT:~0,-1%"
set "RSCRIPT="

if exist "%ROOT%\runtime\R\bin\x64\Rscript.exe" set "RSCRIPT=%ROOT%\runtime\R\bin\x64\Rscript.exe"
if not defined RSCRIPT if exist "%ROOT%\runtime\R\bin\Rscript.exe" set "RSCRIPT=%ROOT%\runtime\R\bin\Rscript.exe"
if defined RSCRIPT (
  rem only the runtime's own library
  set "R_LIBS_USER=%ROOT%\runtime\none"
  set "R_LIBS_SITE=%ROOT%\runtime\none"
)

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
  echo  Put the portable runtime folder beside this file as "runtime", or install R from
  echo  https://cran.r-project.org/bin/windows/base/ ^(choose "Install for me only"^), then start again.
  echo.
  pause
  exit /b 1
)

set "OVID_ROOT=%ROOT%"
"%RSCRIPT%" --vanilla "%ROOT%\core\launcher_app\start.R"
if errorlevel 1 (
  echo.
  echo  The ovid launcher stopped with an error ^(see above^).
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
