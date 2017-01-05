'-------------------------------------------------------------------------------
'     Iridium SBD transmit.BAS - BASIC code to push GOES messages out via SBD
'
'     This routine must be schedule to run after the GOES message has been sent
'     and the IRIDIUM.DAT file has been posted.  Typically you would schedule
'     this for about one minute after the GOES time.
'
'     Right now, this is set up for an Iridium modem on COM4:
'
'     NOTE:  ATE0 has been wired into the code as of 7/18/07 TNK
'	     This version deletes the Wallops-style header
'
'     Update: Iridium -> DCP clock sync Added
'             04/16/2012 WMH
'
'-------------------------------------------------------------------------------
Const True = -1
Const False = 0
Const LoggingIn=2
Const LoggedIn=4
Const LF = Chr(10)                 ' Line feed character
Const CR = Chr(13)                 ' Carriage return character
Const RegTimeout = 300             ' timeout to register
Const SendTimeout = 300            ' timeout to send
Const CharQuote = Chr(34)	   ' Quote (to send quote character inside of a string)

Public Declare Sub FlushPort(Handle)
Public Declare Function OpenPort(ComPort, RejectWhen)
Public Declare Sub ClosePort(Handle)
Public Declare Function ReadPort(Handle, BytesToRead, BytesRead, BytesRemain, TimeoutSec)
Public Declare Sub WritePort(Handle, Data)
Public Declare Function HayesCommand(Handle, Command, TimeoutSec)
Public Declare Sub SetDTRPort(Handle)
Public Declare Sub ClrDTRPort(Handle)
Public Declare Sub SetRTSPort(Handle)
Public Declare Sub ClrRTSPort(Handle)
Public Declare Sub SetBreakPort(Handle)
Public Declare Sub ClrBreakPort(Handle)
Public Declare Function CDPort(Handle)

Public Declare Function GMTTime(RemainingMilliseconds)
'-------------------------------------------------------------------------------
'     SCHED_SendSBD - scheduled routine that sends Iridium short burst
'                     data string
'
'     This subroutine is a modification of the code used by Sutron for the
'     Corps of Engineers in New Orleans
'
'-------------------------------------------------------------------------------
Public Sub Sched_SendSBD

	'Data transmission routine
	'This sets up the modem and pushes out a data string -- in this case,
	'IRIDIUM.DAT, which is recovered from the flash disk
	
	Handle = OpenPort("COM4:", LoggingIn Or LoggedIn) 'This needs to be set for the com port you are using
	'Handle = OpenPort("COM4:", 0) 'use this to openport even if in use. This needs to be set for the com port you are using

	If Handle Then
                'StatusMsg "Iridium transmission started"
		
    	        Call SetDTRPort(Handle)
		Sleep(5) ' Allow the modem time to wake up
		' wait for the modem to be registered
		'
		'Make sure Echo is off -- parser fails if echo is on
		'StatusMsg "ATE0"
		txt = HayesCommand(Handle, "ATE0", 2)

		StartTime = Time
		ModemRegistered = false
                iTry = 0

                StatusMsg "Iridium - Registering modem"
		Do
                   iTry = iTry + 1
                   StatusMsg "Iridium - Attempt #" & Format("%i",iTry) & " to register modem"
                   'NOTE:  This code is specific to the little 9601 style modem
		   '	   The little 9601 has its own registration
		   '	   command -- It's AT+SBDREG, and the return is not OK, but
		   '	   0,xx 1,xx or 2,xx -- 2 being success.

		   'Check the status -- see if it's already registered
		   	
		   txt = HayesCommand(Handle, "AT+SBDREG?", 10)
		   'trim down to the part we need
		   StatusMsg "Iridium - SBDREG status=" & txt
		   'trim down to the part we need
		   txt = left(txt,9)
		   strTestChar=Right(txt,1)
                   If (strTestChar = "2") or (strTestChar = "1") Then
                       	ModemRegistered = true
		   
		   Else 'It's not registered - But -- could be returning "wait 3 minutes"
			If(strTestChar = "0") Then 'It's not registered
		   		txt = HayesCommand(Handle, "AT+SBDREG", 10)
			Else 'It's hung - we need to wait three minutes
				'Sleep a bit -- Try to get wait status to expire
				SLEEP 10
			End If
 		   End If
                   
                   If iTry > 10 Then
		       StatusMsg "SBDREG retry count exceeded"
                       goto errorhandler
                   End If

		   If CDPort(Handle) <> 0 Then
		       StatusMsg "CD active on modem ... abort"
		       goto errorhandler
		   End If

		Loop Until (ModemRegistered OR ((Time - StartTime)> RegTimeout))

		If ((Time - StartTime) > RegTimeout) Then
		   ErrorMsg "Iridium - Timeout waiting for registration -- SBDREG STATUS: " &txt
		   goto ErrorHandler
		End If

                ' Check the signal strength and record with status
		' Note that book says not to do this until we are registered
                txt = HayesCommand(Handle, "AT+CSQ", 10)

		'The 10 second timeout is because it takes a while to get signal
                StatusMsg "Iridium - Signal Strength - starting = " & txt

		''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		' Time Sync Added 04/13/2012 Winston Hensley
		' Here is where we check the Iridium System Time(IST) and update System Clock
		' Iridium's Epoch rolls over every 12 years and starts on March 8, 2007 03:50:35 GMT
		' Sutron's Epoch starts on January 1st, 1970 00:00:00 GMT

		' Get Iridium Time in 90ms values and convert to ms values
		txt = HayesCommand(Handle, "AT-MSSTM", 10)

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
		'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
		

		'clear SBD Message Buffer
                iTry = 0
                BufferCleared = false
                Do
                     iTry = iTry + 1
		     'StatusMsg "AT+SBDD2"
		     txt = HayesCommand(Handle, "AT+SBDD2", 3)
                     If Right(txt,3) = "0OK" then
                       BufferCleared = true
		       StatusMsg "AT+SDD2 Cleared MOM/MTM Buffer"
                     Else
                        Sleep 1
                     End If
		     If iTry > 5 Then
		        ErrorMsg "Command AT+SBDD0 failed on retries " &txt
		        goto ErrorHandler
		     End If
                Loop Until (BufferCleared OR iTry > 5)

		' Conditional Below is for debugging and should not execute.
		If 0 = 1 Then
		   'send some text to the message buffer
		   StatusMsg "AT+SDBWT"
		   txt = HayesCommand(Handle, "AT+SBDWT=SBDWT Text Test", 2)
		   If txt="" OR txt <>"OK" Then
		      ErrorMsg "Command AT+SBDWT failed " &txt
		      goto ErrorHandler
		   End If
		Else

                   'Set the binary transmit string to the IRIDIUM.DAT value
                   'returned by GetMostRecent function

                   'Look for IRIDIUM.DAT
                   iDatFile = FreeFile
                   strMsg = "No recent data"
                   strDatFileName = "\Flash Disk\IRIDIUM.DAT"
                   Open strDatFileName for Input as iDatFile
                   On Error GoTo 100
                   Line Input iDatFile, strMsg
	           On Error Resume Next

100                Close iDatFile
		   'Close the damn iDatFile anyway!
		   Close iDatFile
	
		   ' Print the thing that was pulled from IRIDIUM.DAT
		   StatusMsg "String to Transmit: " &strMsg
                  
		   BinString = strMsg
		   LenData = Format("%d", Len(BinString))
		   Result = ComputeCRC(0,0,BinString) Mod 65536'
		   'StatusMsg "CRC " &(Result>>8) &" " &(Result Mod 256)
		   BinString = BinString & Chr(Result>>8) & Chr(Result Mod 256)
		   Cmd = "AT+SBDWB="&LenData
		   Txt = HayesCommand(Handle, Cmd, 2)
		   If Left(Txt, 5)="READY" then
		      Txt = HayesCommand(Handle, BinString, 2)
		      StatusMsg "SBDWB: " &txt
		      If Left(Txt,1) <> "0" then
		         ErrorMsg "Command SBDWB failed " &Txt
		         goto ErrorHandler
		      End If
		   End If
		End If
		SLEEP 10
		'Initiate SBD Session to send the data
		StartTime = Time
		DataSent = false
                iTry = 0
		Do
		   'statusmsg "AT+SBDI"
                   iTry = iTry + 1
                   StatusMsg "Iridium - Attempt #" & Format("%i",iTry) & " to send SBD Message" 
		   txt = HayesCommand(Handle, "AT+SBDI", 15)

		   'StatusMsg "SBDI=" &txt
		   If Left(txt, 8) = "+SBDI: 1" then
		      DataSent = true
                   Else
		       SLEEP 1
                       If iTry > 10 Then
		          StatusMsg "SBDI retry count exceeded"
                          goto errorhandler
                       End If
		   End If
		   If CDPort(Handle) <> 0 then
		        StatusMsg "CD active on modem ... abort"
		        goto errorhandler
		   End If
		Loop Until (DataSent OR ((Time - StartTime)> SendTimeout))
		If ((Time - StartTime) > SendTimeout) then
		   ErrorMsg "SBDI timeout " &txt
		   goto ErrorHandler
		End If
		StatusMsg "Iridium - SBD transmission done"

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
		'Check for text message in the Mobile Terminated Buffer		'X
		'Respond Appropriately at the end of this routine.		'X
		txt = HayesCommand(Handle, "AT+SBDRT", 20)			'X
		StatusMsg "Mobile Terminated Message:" &txt			'X
		'Determine what to do now					'X
		RebootDCP = 0							'X
		StatusMsg "Message Length =" &(Len(txt))		  	'X
										'X
		If Len(txt) > 0 Then						'X
										'X
			If StrComp(txt,"+SBDRT:RebootOK") = 0 Then		'X
				StatusMsg "Setting up DCP to Reboot"		'X
				Reboot						'X
			End If							'X
										'X
		End If								'X
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                ' Check the signal strength and record with status
                txt = HayesCommand(Handle, "AT+CSQ", 8)
                StatusMsg "Iridium - Signal Strength - ending = " & txt
		
ErrorHandler:   'Label for branch from errors

		'Call ClrDTRPort(Handle)
		Call ClosePort(Handle)

	Else    'End of check for valid handle to Iridium

		ErrorMsg "Could not access the Iridium Modem"

	End If

End Sub





