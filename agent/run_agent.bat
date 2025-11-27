@echo off
setlocal
set BASE=%~dp0
rem optional jitter (5–25s) so multiple machines don’t collide
set /a J=(%RANDOM% %% 21) + 5
timeout /t %J% >nul

"%BASE%R-4.5.1\bin\Rscript.exe" "%BASE%agent\lcd_agent.R" --once
endlocal
