@echo off
setlocal
set "BASE=%~dp0"
for %%I in ("%BASE%..") do set "ROOT=%%~fI"
set "REXE=%ROOT%\R-4.5.1\bin\Rscript.exe"
set "SCRIPT=%BASE%\lcd_agent.R"
"%REXE%" --vanilla "%SCRIPT%"
endlocal
