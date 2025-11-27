@echo on
setlocal enabledelayedexpansion

rem --- Resolve base folder to this .bat's directory (portable) ---
set "BASE=%~dp0"
rem If this .bat is in \agent, then AGENT=%BASE%
set "AGENT=%BASE%"
rem Project root one level up:
for %%I in ("%AGENT%..") do set "ROOT=%%~fI"

set "RSCRIPT=%ROOT%\R-4.5.1\bin\Rscript.exe"
set "LCD_AGENT=%AGENT%\lcd_agent.R"
set "LOG=%AGENT%\run_agent_debug.log"

echo =============================================================== >> "%LOG%"
echo [%date% %time%] START run_agent_verbose >> "%LOG%"

echo Checking Rscript: "%RSCRIPT%"
if not exist "%RSCRIPT%" (
  echo [ERROR] Rscript.exe not found at: "%RSCRIPT%"
  echo [ERROR] Rscript.exe not found at: "%RSCRIPT%" >> "%LOG%"
  goto :END
)

echo Checking agent script: "%LCD_AGENT%"
if not exist "%LCD_AGENT%" (
  echo [ERROR] lcd_agent.R not found at: "%LCD_AGENT%"
  echo [ERROR] lcd_agent.R not found at: "%LCD_AGENT%" >> "%LOG%"
  goto :END
)

echo Running agent once...
echo Command: "%RSCRIPT%" "%LCD_AGENT%" --once
echo [%date% %time%] EXEC >> "%LOG%"
"%RSCRIPT%" "%LCD_AGENT%" --once 1>>"%LOG%" 2>&1
set "RC=%ERRORLEVEL%"

echo Return code: %RC%
echo [%date% %time%] END RC=%RC% >> "%LOG%"

:END
echo.
echo --- Done. Press any key to close this window. ---
pause >nul
endlocal
