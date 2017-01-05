'-------------------------------------------------------------------------------
'     NOAA / COOPS IridumSBDRoutinesWITHSATLINK.BAS - 
'     BASIC code for Iridium hourly hydro project that handles Iridium tasks
' 	Modified code from Sutron software for Bhutan COE Concord project.
'                Designed to run on Xpert OS version 3.4.0.6 with associated
'                libraries, i.e. Iridium_SELFTIMED_STFormatter.bas
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
'     Function ReceiveSBD(RxMsg)
'         This routine check for the SBDRING on the port to indicated an 
'         incoming message to the modem.  Returns True or False.
'
'     Sub ProcessSBDIX(txt, MsgInSBD, MsgBytes, NumMsgQueued)
'         This routine parses the response to an AT+SBDIX command.  This command is
'         issued when sending a message from the 9210 or in response to a SBDRING.
'         This will indicate if a message is received from the gateway (GSS) and 
'         how many are queued at the GSS
'
'     Sub ReadSBD(RxMsg, TxMsg)
'         This routine is called after a SBDRING is received.  An SBDIX is issued to 
'         transfer the message from the GSS to the modem.  Next an AT+SBDRT is sent.
'         This command reads the message that has been transferred. After the message
'         is read it is parsed.  Then the AT+SBDD2 command is issued to clear the msg
'         from the buffer.
'
'     Sub ParseSBDMsg(RxMsg, TxMsg)
'         The routine parses the received msg/command after it has passed the CRC check.
'         The routine checks to see if it is a request or command.  Valid commands are:
'         LIMTS, SIREN, TIME, WX, REBOOT.
'
'     Function GMTTime(RemainingMilliseconds)
'         Routine added at NOAA in Chesapeake VA which is called when SendSBD transmits data.
'         Iridium system time is polled from the Gateway and then converted to GMT Time. If 
'         clock has drifted by a second or more the time is synced to the GMT Time from
'         the Gateway.
'
'     Sub SyncDCPClock
'         This is a function which contains an algorithm for converting the 90ms increments
'         from Iridium into ms values for comparing with the system time. It works with the
'         GMTTime function and it could be consolidated to one function I think.
'
'     Right now, this is set up for an Iridium modem on COM4: This is defined in 
' 	the Constants below
'
'     NOTE:  ATE0 has been wired into the code as of 7/18/07 TNK
'
'
'-------------------------------------------------------------------------------
Const True = -1
Const False = 0
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

Const MaxModemInitFail = 30          'Set max number of allowed failures it initialize modem 
Const RebootOnModemFail = True      'Set true if want to auto-reboot after max modem init failures reached

Static SBDStarted
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

   If IridiumHandle Then

      COMPortInUse = True							'XXXXXXX

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
         ModemInitFail = 0	 								'XXXXXX
      End If

      On Error Resume Next

         IridiumHandle = OpenPort(IridiumPort, LoggingIn Or LoggedIn)
         StatusMsg "Opening port - handle = " &(IridiumHandle)

      If IridiumHandle Then

         COMPortInUse = True 									'XXXXXX

         nOffset = tNow Mod TxInterval
         tNow = tNow - nOffset

         If ((tNow mod TxInterval) = 0) and (nOffset = TxOffset) Then
          StatusMsg "Time for normal transmission"
          'The FormatIridiumData is the standard GOES message for NOS as of  06/2012
           TxMsg = FormatIridiumData                                    
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
			Sleep 360							'XXXXXXX
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


'********************* SUB ReceiveSBD ******************************************
Public Function ReceiveSBD(RxMsg)

  On Error Resume Next
  ReceiveSBD = False
  RxMsg = ""

  StatusMsg "Iridium ReceiveSBD - Checking for SBD message"
  StatusMsg "Checking port - handle = " &(IridiumHandle)

  'check for +SBDRing alert
  BytesRead = 0
  BytesRemain = 0
  ' Wait up to the timeout for 1 character to come in

  sResult = ReadPort(IridiumHandle, 2, BytesRead, BytesRemain, 60)
  StatusMsg "sResult (2bytes)= "+sResult
  ' Now read in any additional characters but with a very short timeout
  If sResult <> "" Then
     sResult = sResult + ReadPort(IridiumHandle, 1000, BytesRead, BytesRemain, 0.01)

     StatusMsg "Received: " + sResult
     RxMsg = sResult
     If Instr(1, sResult, "SBDRING") > 0 Then
        StatusMsg "Ring detected"
        ReceiveSBD = True
     End If
  End If

ErrorHandler:   'Label for branch from errors

End Function


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

'********************* SUB ProcessSBDIX *****************************************
Public Sub ProcessSBDIX(txt, MsgInSBD, MsgBytes, NumMsgQueued)
     'Parse to see if any other messages queued from Gateway
     '+SBDIX: 0, 162, 1, 112, 23, 0OK
         '0 = Meesage transferred ok.
         '162 = sequence num
         '1 = SBD message received from GSS
         '112 = sequence number
         '23 = length of MT SBD message
         '0 = num messages queued
     Ind1 = InStr(1, txt, ",")
     Ind2 = Instr(Ind1+1, txt, ",")
     Ind3 = Instr(Ind2+1, txt, ",")
     stxt = Mid(txt, Ind2+1, Ind3-1-Ind2)
     nMsg = Val(Trim(stxt))
     StatusMsg "Msg in SBD = "+str(nMsg)
     If nMsg > 0  Then
        'Read msg
        MsgInSBD = True
        Ind4 = Instr(Ind3+1, txt, ",")
        Ind5 = Instr(Ind4+1, txt, ",")
        stxt = Mid(txt, Ind4+1, Ind5-1-Ind4)
        stxt = Trim(stxt)
        MsgBytes = Val(stxt)
        StatusMsg "Num bytes = "+str(MsgBytes)

        stxt = Mid(txt, Ind5+1, Len(txt) - Ind5+1-2)
        stxt = Trim(stxt)
        NumMsgQueued = Val(stxt)
        StatusMsg "Num msg queued = "+str(NumMsgQueued)
     Else
        MsgBytes = 0
        NumMsgQueued = 0
        MsgInSBD = False
        StatusMsg "No Messages"
     End If
End Sub

'********************* SUB ReadSBD *********************************************
Public Sub ReadSBD(RxMsg, TxMsg)

  On Error Resume Next

  StatusMsg "Iridium ReadSBD - Reading SBD message"

  MsgBytes = 0
  NumMsgQueued = 0
  MsgInSBD = False


  'Make sure Echo is off (necessary redundant check) -- fails if echo is on   		    	'XXXXXXXX
  'StatusMsg "Iridium - ATE0"
  txt = HayesCommand(IridiumHandle, "ATE0", 10)
  Sleep 1
  

  Cmd = "AT+SBDIXA"
  txt = HayesCommand(IridiumHandle, Cmd, 90)
  StatusMsg "Iridium - SBDIXA response =" & txt
  If (Left(txt, 10) = "+SBDIX: 0,") Or (Left(txt, 10) = "+SBDIX: 1,") Or (Left(txt, 10) = "+SBDIX: 2,") Then
      RxMsg = txt  

      NumBytesRxTotal = NumBytesRxTotal + MsgBytes
      NumBytesRxToday = NumBytesRxToday + MsgBytes

      Call ProcessSBDIX(RxMsg, MsgInSBD, MsgBytes, NumMsgQueued)
      If MsgInSBD Then
         Call RetrieveAndParseMsg(TxMsg)
      End If
  Else
     ErrorMsg "SBIDXA bad error code response"
  End If

ErrorHandler:   'Label for branch from errors

End Sub

'********************* SUB RetrieveAndParseMsg *********************************************
Public Sub RetrieveAndParseMsg(TxMsg)

  On Error Resume Next

    StatusMsg "Iridium RetrieveAndParseMsg"
    'Send read command
    Cmd = "AT+SBDRT"
    RxMsg = HayesCommand(IridiumHandle, Cmd, 20)
    StatusMsg "Iridium - SBDRT response =" & RxMsg
    If Len(RxMsg) > 0 Then
       TxMsg = ""
       'Reply will be +SBDRT:messageOK
       Ind1 = Instr(1, RxMsg, ":")
       Ind2 = Instr(1, RxMsg, "OK")
       If (Ind1 > 0) and (Ind2 > 0) and ((Ind2 - Ind1) > 1) Then
          RxMsg = Mid(RxMsg, Ind1+1, Ind2-Ind1+1)
          StatusMsg "RxMsg = "+RxMsg
          Call ParseSBDMsg(RxMsg, TxMsg)

          'If CRCOK(RxMsg) Then
          '   Call ParseSBDMsg(RxMsg, TxMsg)
          'Else
          '   StatusMsg "CRC check failed: "+RxMsg
          'End If

       End If
       'Clear message for next one
       Cmd = "AT+SBDD1"
       txt = HayesCommand(IridiumHandle, Cmd, 5)    '0=MO, 1=MT, 2=both
       StatusMsg "Iridium - SBDD1 response =" & txt
       MsgInSBD = False
    End If

ErrorHandler:   'Label for branch from errors

End Sub


'********************* SUB ParseSBDMsg ******************************************
Public Sub ParseSBDMsg(RxMsg, TxMsg)
  
  'Check if request or set command (i.e. LIMITS? or LIMITS,xx.,xx,xx)
  StatusMsg "Parsing rx msg 1: " + RxMsg
  On Error Resume Next
  Ind = InStr(1, RxMsg, "OK")
  If Ind > 0 Then
    'Remove trailing OK response code.
     RxMsg = Mid(RxMsg, 1, Ind-1)
  End If
  StatusMsg "Parsing rx msg 2: " + RxMsg
  TxMsg = ""
  rLen = Len(RxMsg)

  'Check if message is a Systat or custom command
  'If ! is first then Systat command
  Ind = InStr(1,RxMsg, "!")
  If Ind = 1 Then
     SystatCmd = True
     StatusMsg "Systat command received"
  Else
     SystatCmd = False
     StatusMsg "Basic data command received"
  End If

  If SystatCmd Then
 
     'Pass command to systat command

     Cmd = Mid(RxMsg, 2, rLen - 1)
     StatusMsg "Systat cmd = "+Cmd
  
     TxMsg = Systat(Cmd)

  Else
     'Check if message is a request or set command
     Ind = InStr(1,RxMsg, "?")
     If Ind > 0 Then
        Request = True
     Else
        Request = False
        Ind = InStr(1, RxMsg, "=")
     End If
          If Ind > 0 Then
            Cmd = Mid(RxMsg, 1, Ind - 1)
            Cmd = UCase(Cmd)
            StatusMsg "Cmd = "+Cmd
            'level threshold command 
            If Cmd = "LIMITS" Then
               If Request Then
               Else 
               End If
            ElseIf Cmd = "TXSCHED" Then
               'TxSched = offset,interval  (00:00:00,01:00:00)
               If Request Then
                  TxMsg = "TXSCHED="
                  
                  iHr = (TxOffset - (TxOffset mod 3600)) / 3600
                  nVal = TxOffset - (iHr*3600)
                  iMin = (nVal - (nVal mod 60)) / 60
                  iSec = TxOffset - (iHr*3600) - (iMin*60)
                  sMsg = Format("%02:%02:%02", iHr, iMin, iSec)
                  TxMsg = TxMsg+sMsg+","

                  iHr = (TxInterval - (TxInterval mod 3600)) / 3600
                  nVal = TxInterval - (iHr*3600)
                  iMin = (nVal - (nVal mod 60)) / 60
                  iSec = TxInterval - (iHr*3600) - (iMin*60)
                  sMsg = Format("%02:%02:%02", iHr, iMin, iSec)
                  TxMsg = TxMsg+sMsg
                  
                  StatusMsg "TxSched="+TxMsg
               Else 
               'TxSched = offset,interval  (00:00:00,01:00:00)
                  iHr = Val(Mid(RxMsg, 1, 2))
                  iMin = Val(Mid(RxMsg, 4, 2))
                  iSec = Val(Mid(RxMsg, 7, 2))
                  nVal = (iHr*3600)+(iMin*60)+iSec
                  Tag(TxOffsetTagName) = nVal
                
                  iHr = Val(Mid(RxMsg, 10, 2))
                  iMin = Val(Mid(RxMsg, 13, 2))
                  iSec = Val(Mid(RxMsg, 16, 2))
                  nVal = (iHr*3600)+(iMin*60)+iSec
                  Tag(TxIntervalTagName) = nVal

               End If
            ElseIf Cmd = "TIME" Then
              'time format - yyyymmddhhmmss
               If Request Then
                  CurTime = Now
                  Y = Format("%04d", Year(CurTime))
                  M = Format("%02d", Month(CurTime))
                  D = Format("%02d", Day(CurTime))
                  Y = Format("%04d", Year(CurTime))
                  H = Format("%02d", Hour(CurTime))
                  MM = Format("%02d", Minute(CurTime))
                  S = Format("%02d", Minute(CurTime))

                  TxMsg = Y+M+D+H+MM+S
               End If
            ElseIf Cmd = "WX" Then
               If Request Then
                  TxMsg = FormatIridiumWXData
               End If
            ElseIf Cmd = "REBOOT" Then
	StatusMsg "Rebooting Due to MTM"
               Reboot
               TxMsg = ""
            ElseIf Cmd = "DIAG" Then
               'Form diagnostic message
                TxMsg = "Bytes Tx Total/Today - "&(NumBytesTxTotal)+"/"&(NumBytesTxToday)+CRLF
                TxMsg = TxMsg + "Tx Success Total/Today - "&(NumTxSuccessTotal)+"/"&(NumTxSuccessToday)+CRLF   
                TxMsg = TxMsg + "Tx Fail Total/Today - "&(NumBytesTxTotal)+"/"&(NumBytesTxToday)+CRLF   
                TxMsg = TxMsg + "Bytes Rx Total/Today - "&(NumBytesRxTotal)+"/"&(NumBytesRxToday)
            End If
          Else
            StatusMsg "Invalid command: " & RxMsg
          End If
  End If

  If TxMsg <> "" Then
     StatusMsg "TxMsg - " + TxMsg
  End If

End Sub

'********************* SUB Sched_SBDTest*********************************************
Public Sub Sched_SBDTest
    TxMsg = FormatIridiumData
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

'********************* SUB Sched_ATTest*********************************************
Public Sub Sched_ATTest
   IridiumHandle = OpenPort(IridiumPort, LoggingIn Or LoggedIn)
   StatusMsg "Opening port - handle = " &(IridiumHandle)

   If IridiumHandle Then
      StatusMsg "Iridium - SBD Registration started"

      'Power 1, 1
      'Sleep 10 ' Allow the modem time to wake up
      ' wait for the modem to be registered

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
   End If
  If IridiumHandle <> 0 Then
     Call ClosePort(IridiumHandle)
     Sleep 1
     'Power 1, 0
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

End Function

'********************* SUB Sync_DCP_Clock******************************************

Public Sub SyncDCPClock

	' Time Sync Added 04/13/2012 - Winston Hensley
	' We check the Iridium System Time(IST) and update System Clock
	' Iridium's Epoch rolls over every 12 years and starts on March 8, 2007 03:50:35 GMT
	' Sutron's Epoch starts on January 1st, 1970 00:00:00 GMT
	' Get Iridium Time in 90ms increments and convert to milliseconds
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
		
	' The remainder must be at least smaller than the 806277
	' or there will be a carry digit to days which complicates the code.
	'The GMTTime function returns the TimeSerial object for the current time in GMT
	'based on the remainder of milliseconds from Iridium's Date & Time
	If RemainingMilliseconds < 796277 Then

	NewGMTTime = GMTTime(RemainingMilliseconds)

		If ((Time-NewGMTTime) > 1) Or ((NewGMTTime-Time) > 1) Then
			Time = NewGMTTime
			StatusMsg "DCP Time Set to " &NewGMTTime
		Else
			StatusMsg "No Need to Sync Now"
		End If

	Else
		StatusMsg "Can Not Sync Clock Now, Clock May Sync Later"	
	End If
End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


'XXXXXXXXXXXXXXXXXXXXXXXXXXXX GMT TIME XXXXXXXXXXXXXXXXXXXXXXXXXXXX

Public Function GMTTime(RemainingMilliseconds)

' The remainder must be smaller than the 806277 (90ms)
' or there will be a carry digit to days.
iHH = 0
iMM = 0
iSS = 0
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

End Function
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX