Public Sub Sched_SystatReboot
   Cmd1 = "Reboot"
   Cmd2 = "RECORDING OFF"
   Cmd3 = "RECORDING ON"
   Response = Systat(Cmd2)
End Sub