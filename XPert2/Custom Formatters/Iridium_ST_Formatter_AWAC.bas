'-------------------------------------------------------------------------------
'     NOAA / COOPS IridumSBDRoutines.BAS - 
'        BASIC code for scheduled ADCP Transmissions in NOAA/COOPS format
' 	      Modified code from Sutron software for Bhutan COE Concord project.
'        Designed to run on version Xpert OS version 3.19.0.10
'
'     Routines defined in this file are:
'
'     Sub SendSBD(TxMsg, MsgInSBD, MsgBytes, NumMsgQueued, SendOK)
'         This routine sends a message (TXMsg) via SBD.  The program first
'         sends at ATE0 to turn echo off.  Next AT+SBDD2 to clear the input and
'         output buffers of the iridium modem.  It send the message to the 
'         modem using AT+SBDWB command and then issues AT+SBDIX command.
'         The program parses the SBDIX response to see if there are any msgs
'         queued at the gateway.  The info is passed back out and handled by
'         the main SBDHandler program 
'
'     Sub ProcessSBDIX(txt, MsgInSBD, MsgBytes, NumMsgQueued)
'         This routine parses the response to an AT+SBDIX command.  This command is
'         issued when sending a message from the 9210 or in response to a SBDRING.
'         This will indicate if a message is received from the gateway (GSS) and 
'         how many are queued at the GSS
'
'     Sub ParseSBDMsg(RxMsg, TxMsg)
'         The routine parses the received msg/command after it has passed the CRC check.
'         The routine checks to see if it is a request or command.  Valid commands are:
'         LIMTS, SIREN, TIME, WX, REBOOT.
'
'     NOTE:  ATE0 has been wired into the code as of 7/18/07 TNK
'
'-------------------------------------------------------------------------------

Const LoggingIn=2
Const LoggedIn=4
Const LF = Chr(10)
Const CR = Chr(13)
Const CRLF = Chr(13) & Chr(10)
Const RegTimeout = 600              'timeout to register (seconds)
Const SendTimeout = 300             'timeout to send
Const CharQuote = Chr(34)           'Quote (to send quote character inside of a string)

Const IridiumPort = "COM4:"         'Com port iridium is connected to.  If com3 make sure NO 5 volts.
Const SecretPhrase = "How 'bout Dem Os?"


'These could be ignored since we are tranmitting a GOES message. NOAA/OSTEP 06/2012                             'XXXXX

Const TxOffsetTagName = "TXOFFSET"              'Comstag name for Tx offset time for normal transmission.  
Const TxIntervalTagName = "TXINTERVAL"          'Comstag name for Tx interval time for normal transmission.
Const RunIntervalTagName = "BASICRUNMIN"   	'Comstag name for SBDHandler Task interval.  Task checks for incoming 
                                                '   and outgoing SBD messages

'Offset to catch the WL value at the top of the hour.
TxOffset = TimeSerial(0,20,0)
TxInterval = TimeSerial(1,0,0)
TxWhileRecordingOff = False

Const MaxModemInitFail = 25          'Set max number of allowed failures it initialize modem 
Const RebootOnModemFail = True      'Set true if want to auto-reboot after max modem init failures reached


Static ModemRegistered
Static IridiumHandle
Static COMPortInUse 'This checks in Start_Recording to see if the Handler has control of the COM Port   'XXXXXXX
Static MsgInSBD            'indicates if there are messages are queued at the gateway
Static MsgBytes            'number of bytes in current message transferred from iridium modem
Static NumMsgQueued        'number of msgs queued at gateway as a result of SBDIXA cmd
Static ModemInitFail

'Init variable for first start recording
COMPortInUse = False


Static CurrentDay          'variables to track diagnostics
Static LastDay
Static NumBytesTxTotal     'number to total bytes transmitted by iridium
Static NumBytesTxToday     'number of total bytes transmitted today
Static NumBytesRxTotal     'number to total bytes received by Iridium
Static NumBytesRxToday     'number of total bytes received today
Static NumTxSuccessTotal   'number of total successful transmissions 
Static NumTxSuccessToday   'number of successul transmissions today
Static NumTxFailTotal      'number of total failed transmissions
Static NumTxFailToday      'number of failed transmissions today


Public Declare Sub SendSBD(TxMsg, MsgInSBD, MsgBytes, NumMsgQueued, SendOK)
Public Declare Function ReceiveSBD(RxMsg)
Public Declare Function CRCOK(RxMsg)
Public Declare Function AddCRC(TxMsg)
Public Declare Sub ProcessSBDIX(txt, MsgInSBD, MsgBytes, NumMsgQueued)
Public Declare Sub ReadSBD(RxMsg, TxMsg)
Public Declare Sub ParseSBDMsg(RxMsg, TxMsg)
Public Declare Sub RetrieveAndParseMsg(TxMsg)
Public Declare Function FormatIridiumData
Public Declare Function FormatIridiumWXData

Public Declare Sub FlushPort(Handle)
Public Declare Sub ClosePort(Handle)
Public Declare Sub WritePort(Handle, Data)
Public Declare Sub SetDTRPort(Handle)
Public Declare Sub ClrDTRPort(Handle)
Public Declare Sub SetRTSPort(Handle)
Public Declare Sub ClrRTSPort(Handle)
Public Declare Sub SetBreakPort(Handle)
Public Declare Sub ClrBreakPort(Handle)
Public Declare Function CDPort(Handle)
Public Declare Function OpenPort(ComPort, RejectWhen)
Public Declare Function Trim(S)
Public Declare Function HayesCommand(Handle, Command, TimeoutSec)
Public Declare Function ReadPort(Handle, BytesToRead, BytesRead, BytesRemain, TimeoutSec)

' extern "C" _declspec(dllexport) LPCTSTR GetSTCurrentMessage()
Public Declare Function GetSTCurrentMessage Lib "\Windows\Satlink.sll" As String
Public Declare Sub SyncDCPClock
Public Declare Function GMTTime(RemainingMilliseconds)

'********************* SUB Start_Recording *************************************
Sub START_RECORDING
   ' the iridium modem "looses" its baud rate whenever it is powered up
   ' or when DTR is dropped.  The solution is to send an AT command on power up
   ' and to not use DTR.  This routine sends the AT command and must be scheduled
   ' to run a few seconds after the modem is powered up. Note that with the changes
   ' for NOS, the modem does not need to be initialized prior to the transmissions.
   On Error Resume Next

   'Attempted to use Lock/Unlock and it failed so I will attempt to use a var to
   'see if the COM port is in use and just sleep until it's not in use. If it seems
   'to not respond I'll attempt to close the standard COM Port.
   ComPortCounter = 0
   Do While COMPortInUse
	Sleep 10
	StatusMsg "COM Port May Be In Use"
	ComPortCounter = ComPortCounter + 1
	If ComPortCounter > 9 Then
		Call ClosePort(IridiumHandle)
		COMPortInUse = False
	End If
   End Loop 								'XXXXXXX

   IridiumHandle = OpenPort(IridiumPort, LoggingIn Or LoggedIn)
   StatusMsg "Opening port (Start_Recording) - handle = " &(IridiumHandle)

   COMPortInUse = True							'XXXXXXX

   If IridiumHandle Then

      'Make sure Echo is off -- parser fails if echo is on
      StatusMsg "Iridium - Sending ATE0"
      txt = HayesCommand(IridiumHandle, "ATE0", 10)
      StatusMsg "ATE0 reply - "+txt

      ' Configure ISU to listen for ring alerts
      StatusMsg "Iridium - Sending AT+SBDMTA=1"
      txt = HayesCommand(IridiumHandle, "AT+SBDMTA=1", 10)
      StatusMsg "AT+SBDMTA=1 reply - "+txt
      If Right(txt,2) <> "OK" then
         'Try one more time
         txt = HayesCommand(IridiumHandle, "AT+SBDMTA=1", 10)
         StatusMsg "AT+SBDMTA=1 try #2 reply - "+txt
         If Right(txt,2) <> "OK" then
            ErrorMsg "Iridium - Failed to set enable SBD Ring Alerts = " & txt
         End If
      End If
               
      'Code Below may not be required by NOAA at this time 06/2012                              'XXXXXXX
      'Send the modem the PIN code
      'If it is, all the following registration code will fail
      'NOTE: CharQuote puts quotes in the string, 1111 is the default PIN
      'txt = HayesCommand(Handle, "AT+CPIN="&CharQuote&"1111"&CharQuote,3)
      'StatusMsg "Iridium - AT+CPIN="
      'StatusMsg "Iridium - PIN number sent"

      StartTime = Time
      ModemRegistered = False
      iTry = 0
      StatusMsg "Iridium - Registering modem"
      If Not ModemRegistered Then
         Do
           iTry = iTry + 1
           StatusMsg "Iridium - Attempt #" & Format("%i",iTry) & " to register modem"
           'NOTE:  This code is specific to the little 9601 style modem
           '     The little 9601 has its own registration
           '     command -- It's AT+SBDREG, and the return is not OK, but
           '     0,xx 1,xx or 2,xx -- 2 being success.

           'Check the status -- see if it's already registered

           txt = HayesCommand(IridiumHandle, "AT+SBDREG?", 20)
           'trim down to the part we need
           StatusMsg "Iridium - SBDREG status=" & txt
           'trim down to the part we need
           txt = left(txt,9)
           strTestChar=Right(txt,1)
           If (strTestChar = "2") Or (strTestChar = "1") Then
              ModemRegistered = True
           Else
              'It's not registered - But -- could be returning "wait 3 minutes"
              If (strTestChar = "0") Then 'It's not registered
                  txt = HayesCommand(IridiumHandle, "AT+SBDREG", 20)
              Else 'It's hung - we need to wait three minutes
                 'Sleep a bit -- Try to get wait status to expire
                 'NOTE:  experiments have shown that you don't want to go under
                 '       about 5 seconds here, and 10 is better.  Too short sleeps
                 '       cause the modem to get into the "wait 3 minutes between
                 '       registration retries mode.
                  Sleep 10
              End If
           End If

           If iTry > 10 Then
              StatusMsg "Iridium - SBDREG retry count exceeded"
           End If

         Loop Until (ModemRegistered OR ((Time - StartTime)> RegTimeout))
      End If

        'If ModemRegistered Then
        ' Check the signal strength and record with status
        ' Note that book says not to do this until we are registered
        txt = HayesCommand(IridiumHandle, "AT+CSQ", 10)
        'The 20 second timeout is because it takes a while to get signal
        StatusMsg "Iridium - Signal Strength - starting = " & txt
      'End If

      Call ClosePort(IridiumHandle)

      COMPortInUse = False									'XXXXXX
      
   End If  'Handle

   MsgInSBD = False

   If (CurrentDay = 0) and (LastDay = 0) Then
      StatusMsg "First time init of CurrentDay"
      CurrentDay = Day(Now)
      LastDay = CurrentDay
   End If

   'RunInterval = Tag(RunIntervalTagname, 3)

   StatusMsg "Starting Iridium_Handler"
   StartTask "Iridium_Handler", 0, TimeSerial(0,0,0), TimeSerial(0,2,0)

End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX STOP_RECORDING XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Sub STOP_RECORDING
'  On Error Resume Next
'  StatusMsg "Stopping Iridium_Handler"
'  For i = 1 To 60
'    StopTask "Iridium_Handler", 1.0
'  Next

  If IridiumHandle <> 0 Then 
     Call ClosePort(IridiumHandle)
     COMPortInUse = False
     Sleep 1
  End If

End Sub  

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Iridium Handler XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Public Function Iridium_Handler

   StatusMsg "In Iridium_handler"
   iRecOn = Systat(1)
   StatusMsg "Recording status - " +str(iRecOn)

      tNow = GetScheduledTime
  
      RxMsg = ""
      TxMsg = ""
      NumRxBytes = 0
      NumMsgQueued = 0
      SendOK = False
      CurrentDay = Day(Now)
      If CurrentDay <> LastDay Then
         'new day reset diagnostic stats
         StatusMsg "New day. Reset diagnostics"
         NumBytesTxToday = 0
         NumBytesRxToday = 0
         NumTxSuccessToday = 0
         NumTxFailToday = 0
         LastDay = CurrentDay
         ModemInitFail = 0									'XXXXXX
      End If

      On Error Resume Next

         IridiumHandle = OpenPort(IridiumPort, LoggingIn Or LoggedIn)
         StatusMsg "Opening port - handle = " &(IridiumHandle)

	 COMPortInUse = True 								'XXXXXX

      If IridiumHandle Then

         nOffset = tNow Mod TxInterval
         tNow = tNow - nOffset

         If ((tNow mod TxInterval) = 0) and (nOffset = TxOffset) Then
          StatusMsg "Time for normal transmission"
          'The GetSTCurrentMessageis the standard GOES message for NOS as of  10/2012
           TxMsg = GetSTCurrentMessage                           
           'TxMsg = TxMsg + AddCRC(TxMsg)
	   If (TxWhileRecordingOff AND (iRecOn = 0)) Or (iRecOn = -1) Then
		Call SendSBD(TxMsg, MsgInSBD, MsgBytes, NumMsgQueued, SendOK)
	   Else
		'This starts with +SBDIXA then +SBDRT
		StatusMsg "Not Recording, Checking Mailbox..."				 'XXXXXX
		Call ReadSBD(RxMsg, TxMsg)						 'XXXXXX 
	   End If

         End If

         If ReceiveSBD(RxMsg) Then
           Call ReadSBD(RxMsg, TxMsg)
           If TxMsg <> "" Then
              StatusMsg "Message to send after ReadSBD -"+TxMsg+"."
              'TxMsg = TxMsg + AddCRC(TxMsg)
              Call SendSBD(TxMsg, MsgInSBD, MsgBytes, NumMsgQueued, SendOK)
           End If
         End If
   
         'StatusMsg "Before Do While"
         Do While (NumMsgQueued > 0) Or MsgInSBD     						'XXXXXXX
           If MsgInSBD Then
              'If msg in MT buffer from last SBDIXA cmd, then just retrieve
              'msg using SBDRT command
              Call RetrieveAndParseMsg(TxMsg)
           Else
              'No msg transferred in buffer, start with SBDIXA then SBDRT cmds
              Call ReadSBD(RxMsg, TxMsg)
           End If
           If TxMsg <> "" Then
              StatusMsg "Message to send after ReadSBD -"+TxMsg+"."
              'TxMsg = TxMsg + AddCRC(TxMsg)
              Call SendSBD(TxMsg, MsgInSBD, MsgBytes, NumMsgQueued, SendOK)
           End If
         End Loop

         'StatusMsg "Closing port"

         Call ClosePort(IridiumHandle)

	COMPortInUse = False								'XXXXXXX

      Else
         ErrorMsg "ModemInit could not access the Modem"
         ModemInitFail = ModemInitFail + 1
	 If ModemInitFail > 5 Then
		Sleep 5
         Else 
	   If ModemInitFail > MaxModemInitFail Then

            		ModemInitFail = 0

		If RebootOnModemFail Then
			StatusMsg "Rebooting Due to Max Modem Init Fail"
            			Reboot
		End If
	   End If
         End If
      End If 
      StatusMsg "Leaving Iridium Handler"

End Function


'********************* SUB SendSBDData ******************************************
Public Sub SendSBD(TxMsg, MsgInSBD, MsgBytes, NumMsgQueued, SendOK)
  
  'Data transmission routine
  'This sets up the modem and pushes out a data string -- in this case,
  'it will be the GOES Message which would have been pushed out by the
  'SatLink2 

    On Error Resume Next
    NumMsg = 0
    StatusMsg "Iridium SendSBD - SBD transmission started"

    'Make sure Echo is off -- parser fails if echo is on
    'StatusMsg "Iridium - ATE0"
    txt = HayesCommand(IridiumHandle, "ATE0", 10)

    'Clear SBD Message Buffer
    iTry = 0
    BufferCleared = false
    Do
       iTry = iTry + 1
       StatusMsg "Iridium - AT+SBDD2 - Clears MO/MT Buffers"
       txt = HayesCommand(IridiumHandle, "AT+SBDD2", 3)   '0=MO, 1=MT, 2=both
       If Right(txt,3) = "0OK" then
         BufferCleared = true
       Else
          Sleep 5
       End If
       If iTry > 5 Then
          ErrorMsg "Iridium - Command AT+SBDD failed on retries " &txt
          goto ErrorHandler
       End If
    Loop Until (BufferCleared OR iTry > 5)


    StatusMsg "Tx Data = "&(TxMsg)
    BinString = TxMsg

    LenData = Format("%d", Len(BinString))
    Result = ComputeCRC(0,0,BinString) Mod 65536
    StatusMsg "Iridium - CRC " &(Result>>8) &" " &(Result Mod 256)
    BinString = BinString & Chr(Result>>8) & Chr(Result Mod 256)

    If Len(BinString) > 270 Then
       ErrorMsg "SBD message too long"
       TxMsg = ""
       GoTo ErrorHandler
    End If   

    StartTime = Time
    DataSent = False
    iTry = 0
    Do
       iTry = iTry + 1
       StatusMsg "Iridium - Attempt #" & Format("%i",iTry) & " to write SBD to buffer"
       Cmd = "AT+SBDWB="&LenData
       Txt = HayesCommand(IridiumHandle, Cmd, 5)
       StatusMsg "Iridium - SBDWB status=" & txt
       If Left(Txt, 5)="READY" then
          Txt = HayesCommand(IridiumHandle, BinString, 2)
          StatusMsg "Iridium - SBDWB: " &txt
          If Left(Txt,3) <> "0OK" then
             ErrorMsg "Iridium - Command SBDWB failed " &Txt
             goto ErrorHandler
          Else
             DataSent = True
             TxMsg = ""
          End If
       End If
       SLEEP 10
    Loop Until (DataSent OR ((Time - StartTime) > SendTimeout))

    SLEEP 1
    'Initiate SBD Session to send the data
    StartTime = Time
    DataSent = false
    iTry = 0
    Do
       Statusmsg "Iridium - AT+SBDIX"
       iTry = iTry + 1
       StatusMsg "Iridium - Attempt #" & Format("%i",iTry) & " to send SBD Message"
       txt = HayesCommand(IridiumHandle, "AT+SBDIX", 90)
       StatusMsg "Iridium - SBDIX=" &txt
       If (Left(txt, 10) = "+SBDIX: 0,") Or (Left(txt, 10) = "+SBDIX: 1,") Or (Left(txt, 10) = "+SBDIX: 2,") Then
          DataSent = True
          NumBytesTxTotal = NumBytesTxTotal + LenData
          NumBytesTxToday = NumBytesTxToday + LenData
          NumTxSuccessTotal = NumTxSuccessTotal + 1
          NumTxSuccessToday = NumTxSuccessToday + 1

          MsgBytes = 0
          NumMsgQueued = 0
          MsgInSBD = False

          Call SyncDCPClock							 	 'XXXXXX 

          Call ProcessSBDIX(txt, MsgInSBD, MsgBytes, NumMsgQueued)
       Else
          SLEEP 30
          nRxBytes = 0
          NumMsgQueued = 0
          If iTry > 10 Then
             StatusMsg "Iridium - SBDI retry count exceeded"
             NumTxFailTotal = NumTxFailTotal + 1
             NumTxFailToday = NumTxFailToday + 1

             goto ErrorHandler
          End If
       End If

    Loop Until (DataSent OR ((Time - StartTime)> SendTimeout))

    StatusMsg "Clear buffer after SBDIX"
    If MsgInSBD Then
       txt = HayesCommand(IridiumHandle, "AT+SBDD0", 3)   '0=MO, 1=MT, 2=both
       StatusMsg "SBDD0 response: " + txt
    Else
       txt = HayesCommand(IridiumHandle, "AT+SBDD2", 3)   '0=MO, 1=MT, 2=both
       StatusMsg "SBDD2 response: " + txt
    End If


    SendOK = DataSent
    StatusMsg "Iridium - SBD transmission done"

    ' Check the signal strength and record with status
    txt = HayesCommand(IridiumHandle, "AT+CSQ", 8)
    StatusMsg "Iridium - Signal Strength - ending = " & txt

ErrorHandler:   'Label for branch from errors

End Sub

'********************* FUNCTION CRCOK *******************************************
Public Function CRCOK(RxMsg)
  
  
  'Ind = InStr(1, RxMsg, "!")
  '   A = ASC(Mid(RxMsg, Ind+1, 1))
  '   B = ASC(Mid(RxMsg, Ind+2, 1))
  '   A = 69
  '   B = 7
  '   StatusMsg "A = "&(A)
  '   StatusMsg "B = "&(B)
  '   A = A << 8
  '   B = B Mod 256
  '   C = A Or B
  '   StatusMsg "Shifted Computed CRC: "&(c)
  '   hex  45 07

  On Error Resume Next
  Ind = InStr(1, RxMsg, "!")
  nLen = Len(RxMsg)
  If Ind+3 <= nLen Then
     Msg = Mid(RxMsg, 1, Ind-1)
     StatusMsg "Msg = "+Msg
     aCRC = Mid(RxMsg, Ind+1, 3)
     StatusMsg "Received CRC = "+aCRC
     nCRC = ComputeCRC(0, &h38005, Msg+SecretPhrase)
     StatusMsg "Computed SSP CRC: "+nCRC
     sCRC = Bin6(nCRC, 3)
     StatusMsg "6-bit CRC: "+sCRC

     If sCRC <> aCRC Then
        ErrorMsg "CRCs do not match"
        CRCOK = False
     Else
        StatusMsg "CRCs Ok"
        RxMsg = Msg
        CRCOK = True
     End If    
  Else
    ErrorMsg "Complete 2-byte CRC not present in msg"
    CRCOK = False  
  End If

End Function  

'********************* FUNCTION AddCRC ******************************************
Public Function AddCRC(TxMsg)
  
  StatusMsg "Computing CRC "
  nCRC = ComputeCRC(0, &h38005, TxMsg+SecretPhrase)
  StatusMsg "Computed SSP CRC: "+nCRC
  sCRC = Bin6(nCRC, 3)
  AddCRC = "!"+sCRC
  StatusMsg "6-bit CRC: "+AddCRC
  
End Function  


'********************* SUB Sched_SBDTest*********************************************
Public Sub Sched_SBDTest
    TxMsg = GetSTCurrentMessage
    SendOK = False
    StatusMsg "Tx Msg = "+TxMsg
    StatusMsg "Len TxMsg = "&(Len(TxMsg))
    Call SendSBD(TxMsg, MsgInSBD, MsgBytes, NumMsgQueued, SendOK)
End Sub  


'********************* SUB Sched_CRCTest*********************************************
Public Sub Sched_CRCTest
    RxMsg = "siren=1!E"
    If CRCOK(RxMsg) Then
       StatusMsg "CRC OK: "+RxMsg
    Else
       StatusMsg "CRC check failed: "+RxMsg
    End If
  
End Sub  


'********************* FUNCTION FormatIridiumData ******************************
Public Function FormatIridiumData

	'Set the binary transmit string to the IRIDIUM.DAT value
        'Look for IRIDIUM.DAT
        iDatFile = FreeFile
        strMsg = "No recent data"
        strDatFileName = "\Flash Disk\IRIDIUM.DAT"
        Open strDatFileName for Input as iDatFile
        On Error GoTo 100
        Line Input iDatFile, strMsg
	On Error Resume Next

100     Close iDatFile
	'Close the iDatFile anyway!
	Close iDatFile
	
	' Print the thing that was pulled from IRIDIUM.DAT
	StatusMsg "String to Transmit: " &strMsg

    FormatIridiumData = strMsg

End Function   