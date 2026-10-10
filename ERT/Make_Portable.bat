@echo off
rem Make_Portable.bat  --  run once on a Windows computer that has R and the internet. It copies that R into R-portable\ and
rem every package into library\, so the whole folder can be zipped and run on computers without R (double-click
rem Ovid_ERT.bat there). Needs a few hundred MB of disk.
call "%~dp0Ovid_ERT.bat" /portable
