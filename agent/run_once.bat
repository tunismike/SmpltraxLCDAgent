@echo off
setlocal
set REXE="C:\Users\Mike Tunis\Documents\SmpltraxLCDAgent\R-4.5.1\bin\Rscript.exe"
set SCRIPT="C:\Users\Mike Tunis\Documents\SmpltraxLCDAgent\agent\lcd_agent.R"
%REXE% --vanilla %SCRIPT%
endlocal
