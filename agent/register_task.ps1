$R = "C:\Users\Mike Tunis\Documents\SmpltraxLCDAgent\R-4.5.1\bin\Rscript.exe"
$Script = "C:\Users\Mike Tunis\Documents\SmpltraxLCDAgent\agent\lcd_agent.R"
$Action = New-ScheduledTaskAction -Execute $R -Argument ("--vanilla `"" + $Script + "`"")
$Trigger = New-ScheduledTaskTrigger -Once -At (Get-Date).AddMinutes(1) -RepetitionInterval (New-TimeSpan -Minutes 2) -RepetitionDuration ([TimeSpan]::MaxValue)
$Principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType InteractiveToken
Register-ScheduledTask -TaskName "Smpltrax LCD Agent" -Action $Action -Trigger $Trigger -Principal $Principal -Description "Auto-convert Shimadzu LCD to CSV with metadata"
