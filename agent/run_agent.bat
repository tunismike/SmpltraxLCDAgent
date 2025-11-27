@echo off
setlocal
set "BASE=%~dp0"
for %%I in ("%BASE%..") do set "ROOT=%%~fI"

rem optional jitter (5-25s) so multiple machines don't collide
set /a J=(%RANDOM% %% 21) + 5
timeout /t %J% >nul

"%ROOT%\R-4.5.1\bin\Rscript.exe" "%BASE%\lcd_agent.R" --once
endlocal
