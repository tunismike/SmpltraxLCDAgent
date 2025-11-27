@echo off
setlocal

set REXE="C:\Users\Mike Tunis\Documents\SmpltraxLCDAgent\R-4.5.1\bin\Rscript.exe"
set APP="C:\Users\Mike Tunis\Documents\SmpltraxLCDAgent\agent\configurator_app.R"

REM Run from the agent folder so relative paths work
pushd "C:\Users\Mike Tunis\Documents\SmpltraxLCDAgent\agent"
%REXE% --vanilla %APP%
echo.
echo If no browser opened, visit: http://127.0.0.1:31813/
echo Press any key to close this window...
pause >nul
popd

endlocal
