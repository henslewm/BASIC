''''''''''''''''''''''''''''''''''''''''''''''
' FileName:GMTTime.bas
' Date:    04/16/2012
' Author: Winston Hensley
' Purpose: After polling the Iridium gateway 
'          for system time and converting to 
'          ms, this function syncs a Sutron
'          9210B's clock if it drifted 1s
'
'''''''''''''''''''''''''''''''''''''''''''''''

Public Declare Sub FlushPort(Handle)
Public Declare Function OpenPort(ComPort, RejectWhen)
Public Declare Sub ClosePort(Handle)
Public Declare Function ReadPort(Handle, BytesToRead, BytesRead, BytesRemain, TimeoutSec)
Public Declare Sub WritePort(Handle, Data)
Public Declare Function Trim(S)
Public Declare Function HayesCommand(Handle, Command, TimeoutSec)
Public Declare Sub SetDTRPort(Handle)
Public Declare Sub ClrDTRPort(Handle)
Public Declare Sub SetRTSPort(Handle)
Public Declare Sub ClrRTSPort(Handle)
Public Declare Sub SetBreakPort(Handle)
Public Declare Sub ClrBreakPort(Handle)
Public Declare Function CDPort(Handle)

Public Function GMTTime(RemainingMilliseconds)

' The remainder must be smaller than the 806277 (90ms)
' or there will be a carry digit to days.
iHH = 0
iMM = 0
iSS = 0

If RemainingMilliseconds < 796277 Then
		
	'Convert to Seconds
	RemainingSeconds = int(RemainingMilliseconds * .09)

	'Calculate HH:MM:SS since epoch.
	iHH = int(RemainingSeconds / 3600)
	RemainingSeconds = RemainingSeconds Mod 3600
	iMM = int(RemainingSeconds / 60)
	iSS = RemainingSeconds Mod 60

	'the following line for debugging
	TimeStamp = Format("%02d:%02d:%02d", iHH, iMM, iSS)
	StatusMsg "Iridium TimeStamp: " + TimeStamp

	' Now we must add the offset to the Iridium Epoch time
	' 03:50:35 GMT		
	' For some reason...this seems to set the time 13
 	' seconds fast...Adjustment made accordingly
	IridiumOffset = TimeSerial(03,50,35)
	IridiumTime = TimeSerial(iHH,iMM,iSS)
	CorrectorTime = TimeSerial(0,0,13)
		
	' This is what you should return: GMTTime
	GMTTime = IridiumTime + IridiumOffset - CorrectorTime

	Else
		StatusMsg "Can Not Sync Clock Now, Clock May Sync Later"	
End If

End Function

'********************* SUB Sync_DCP_Clock******************************************
Sub SCHED_Sync_DCP_Clock
	' Time Sync Added 04/13/2012 Winston Hensley
	' Here is where we check the Iridium System Time(IST) and update System Clock
	' Iridium's Epoch rolls over every 12 years and starts on March 8, 2007 03:50:35 GMT
	' Sutron's Epoch starts on January 1st, 1970 00:00:00 GMT
	' Get Iridium Time in 90ms values and convert to ms values
	txt = HayesCommand(IridiumHandle, "AT-MSSTM", 10)

	'trim down to the part we need and turn it into a hex number string
	txt = right(txt,10)
	txt = left(txt,8)
	txt = "&H" + txt
		' Print to screen (should be 8 byte Hex String)
	'StatusMsg "Iridium System Time(-MMSTM) = " & txt
		'Convert Hex String to integer
	hexInt = Val(txt)
		' Print to screen (should be 8 byte Hex String)
	StatusMsg "Iridium System Time Integer = " & hexInt
	
	'Divide by the number of 90ms intervals in 1 day...save the remainder
	RemainingMilliseconds = hexInt Mod 960000
	
	' Print to screen 
	StatusMsg "Leftover Milliseconds = " &RemainingMilliseconds 
		
	' The remainder must be smaller than the 806277
	' or there will be a carry digit to days.
	'NewGMTTime = 0
		'xxxxxxxxxxxxxx Sync the DCP Clock xxxxxxxxxxxxxx
	'The GMTTime function returns the TimeSerial for the ms remaining
	NewGMTTime = GMTTime(RemainingMilliseconds)
	
	If (NewGMTTime < 1) Or (Len(NewGMTTime) < 1) Then
		'Remainder of ms is too large includes a carry digit
		'for the day.
		StatusMsg "Can't Sync Right now"
		Else
			If ((Time-NewGMTTime) > 1) Or ((NewGMTTime-Time) > 1) Then
			Time = NewGMTTime
			StatusMsg "DCP Time Set to " &NewGMTTime
		Else
			StatusMsg "No Need to Sync Now"
		End If
	End If
End Sub
'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx