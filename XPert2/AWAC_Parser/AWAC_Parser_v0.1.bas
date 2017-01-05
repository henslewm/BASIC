'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Filename: 	AWAC_Parser_v0.1.bas
' Author:   	Winston Hensley
' Date:     	08/25/2016
' Decription: 	This code is meant to be run as a BASIC sesor block every 30
'              minutes, open a temporary file to contain the latest AWAC data and parse
'              out the 3rd line of current magnitude & direction along with 
'              significant wave height & direction.
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'    The checksum calculation is part of the NMEA standard. It is the representation of two
'    hexadecimal characters of an XOR if all characters in the sentence between – but not
'    including – the $ and the * character.
'
'    MAX RESPONSE LENGTH-103 Bytes
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' INFORMATION (SENSOR CONFIGURATION)
' Field    Description          Form
'   0      Identifier           "$PNORI" 
'   1      Instrument Type      0=Aquadopp, 2=Aquadopp Profiler 3=AWAC
'   2      Head ID              aaannnn
'   3      # of beams           N
'   4      # of cells           N
'   5      Blanking(m)          dd.dd
'   6      Cell Size(m)         dd.dd
'   7      Coordinate System    ENU=0,XYZ=1,Beam=2
'   8      Checksum(hex)        *hh
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' SENSOR DATA
'    $PNORS,073010,050000,00,B0,13.4,1520.6,114.9,-0.5,1.6,22.314,18.92,1039,0*0B
' Field    Description          Form
'   0       Identifier           "$PNORS" 
'   1       Date                 MMDDYY
'   2       Time                 hhmmss
'   3       Error Code(hex)      hh
'   4       Status Code(hex)     hh
'   5       Battery Volt(V)      dd.d
'   6       Sound Speed(m/s)     dddd.d
'   7       Heading(deg)         ddd.d
'   8       Pitch(deg)           dd.d
'   9       Roll(deg)            dd.d
'   10      Pressure(dbar)       ddd.ddd
'   11      Temp(deg C)          dd.dd
'   12      Analog Input #1(cts) nnnnn
'   13      Analog Input #2(cts) nnnnn
'   14      Checksum(hex)        *hh
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' CURRENT VELOCITY DATA
'           $PNORC,073010,050000,1,0.10,-0.11,-0.01,0.15,137.2,C,88,83,87,,,*37
' Field    Description          Form
'   0       Identifier           "$PNORC" 
'   1       Date                 MMDDYY
'   2       Time                 hhmmss
'   3       Cell number          N
'   4       Velocity 1(m/s)      dd.dd
'   5       Velocity 2(m/s)      dd.dd
'   6       Velocity 3(m/s)      dd.dd
'   7       Speed(m/s)           dd.dd
'   8       Direction(deg)       ddd.d
'   9       Amplitude units      "C" counts
'   10      Amplitude 1          Nnn
'   11      Amplitude 2          Nnn
'   12      Amplitude 3          Nnn
'   13      Correlation 1(%)     Nn
'   14      Correlation 2(%)     Nn
'   15      Correlation 3(%)     Nn
'   16      Checksum(hex)        *hh
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' WAVE PARAMETERS
'    $PNORW,073010,051001,3,4,0.55,0.51,0.63,0.82,2.76,3.33,2.97,55.06,78.91,337.62,
'            0.48,22.35,0,1,0.27,129.11,0000*4E
' Field    Description          Form
'   0       Identifier           "$PNORW" 
'   1       Date                 MMDDYY
'   2       Time                 hhmmss
'   3       Spectrum basis type  n
'         0-dbar,1-m/s,3-AST
'   4       Processing Method    n
'           1-PUV,2-SUV,3-MLM
'           4-MLMST
'   5       Hm0(m)               dd.dd
'   6       H3(m)                dd.dd
'   7       H10(m)               dd.dd
'   8       Hmax(m)              dd.dd
'   9       Tm02(s)              dd.dd
'   10      Tp(s)                dd.dd
'   11      Tz(s)                dd.dd
'   12      DirTp(deg)           ddd.dd
'   13      SprTp(deg)           ddd.dd
'   14      Main Direction(deg)  ddd.dd
'   15      Unidirectivity Idx   dd.dd
'   16      Mean Pressure(dbar)  dd.dd
'   17      # of no detects      n
'   18      # of bad detects     n
'   19      Near surface current dd.dd
'            speed(m/s)
'   20      Near surface current ddd.dd
'             direction(m/s) 
'   21      Error Code           hhhh
'   22      Checksum(hex)        *hh

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This program supports measuring Nortek AWAC sensor and logging 30 minute
' files to the \SD Card\AWAC
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'******* Initialize Variables
CONST AWAC_CONNECTED = 1    ' Easily disable code for testing.
CONST CHAR_SH = Chr(1)           ' Start of heading character
CONST CHAR_SX = Chr(2)           ' Start of text character
CONST CHAR_EX = Chr(3)           ' End of text character
CONST CHAR_ET = Chr(4)           ' End of transmission character
CONST CHAR_LF = Chr(10)          ' Line feed character
CONST CHAR_CR = Chr(13)          ' Carriage return character
CONST ENTER = CHAR_CR + CHAR_LF
CONST MAXBYTES = 100             ' Max num bytes in sensor response. See manual.


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Port Configuration Parameters
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
CONST PORT = "COM4:"
CONST PORT_TIMEOUT = 0.5
CONST SENSORNAME = "AWAC"        ' Name used on block output and sensors page
Const BAUD = 9600
Const NOPARITY = 0
Const NOHANDSHAKE = 0


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' INITIALIZE SENSOR
' This code runs at recording start. Here we simply open the port.
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Sub Start_Recording
	If AWAC_CONNECTED THEN
	 '*** Open port, abort if recording stopped
	  If Abort Then Goto ErrorHandler
	  On Error Resume Next
	  StatusMsg "Opening AWAC port"
	  Open PORT as #1  'iFilenum
	  If Err <> 0 Then
	     ErrorMsg "Failed to open port, err " &Err
	     Goto ErrorHandler
	  End If
	  
	  
	  StatusMsg "AWAC port opened"
	  SetPort #1, BAUD, NOPARITY, 8, 1, NOHANDSHAKE
	  StartTime = Time
	  TimeValid = false
	  SetTimeout #1,35
	  
	  'Flush any previous messages from input buffer.
	  FlushInput PORT
	Else
	ErrorHandler:
	      ErrorMsg Format("AWAC: Open port %s failed.", PORT)
	End If
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Measure Sensor
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Public Sub SENSOR_AIRMAR

'Initialize Variables
NMEA_Messages = 8
Message_Location = 0
'Number of empty strings below corresponds to 
'number of messages in variable above.
sInBuf 	= Array("","","","","","","","")
MSGID 	= ""      'NMEA Identifier
CURMAG  	= 0.0
CDIR  	= 0.0
SIGH  	= 0.0
PERIOD 	= 0.0
Asterisk = ""
NMEA_message = ""

' NMEA CheckSum Variable 
Cksum 	= ""
On Error Resume Next
  
'------------------------------------------------------------------------------------------
'Start Time
'------------------------------------------------------------------------------------------
tStart = Now

'Typical string from the AWAC for Currents:
'$PNORC,29.9286,I,1.0135,B,30.5,C,,,,,,,147.3,T,158.0,M,1.1,N,0.6,M*2F
'
'Typical string from the AWAC for Waves:
'$PNORW,29.9286,I,1.0135,B,30.5,C,,,,,,,147.3,T,158.0,M,1.1,N,0.6,M*2F
'123451234512345123451234512345123451234512345123451234512345
'----------------10-------------20--------------30--------------40-------------50-------------60

F1 = FreeFile         
Open "DataFile.txt" For Output As F1
	For i = 1 To NMEA_Messages
		WaitFor #1, "$"
		Line Input #1, sInBuf(i) 'Should be a complete line
		Print F1, sInBuf(i)
	Next
Close F1

' Copy the datafile to a backup directory on SD Card
FileCopy "DataFile.txt" ["\SD Card\AWAC\" Date Time ".txt"]

' Open the file back up for parsing. Non-elegant but necessary
Open "Datafile.txt" for Input as F1

	' Parse data after reading from instrument to minimize timing conflicts.
	For i = 1 To NMEA_Messages
		
		'Get NMEA message location, pull message, Read the ID, move pointer 
		'back to beginning of line so Line Input can pull the line for 
		'parsing into variables.
		Message_Location = Seek(F1)

		'Read line from file to find MsgID then reset position to start of msg
		Line Input F1, NMEA_message
		Seek F1, Message_Location
		
		' Read Message ID for Further Parsing 
		MsgID = Mid(NMEA_message,1,5)
				
		''''''''''''''''''''''''''''''''''''' QC & Record Currents '''''''''''''''''''''''''''''''''
		If MsgID = "PNORC" Then
			' Bob only wants the 3rd of 6 bins in this application
			If i = 5
				Input F1, MsgID, , , , , , , ,CURMAG,CDIR, , , , , , ,  Cksum
				
				' Don't understand why variables re-defined here.
				dMag = CURMAG 'numerical current magnitude
				dDir = CDIR 'numerical current direction
				
				  'Set the #1 output to dMag and set the quality
					  SetOutputData 1, dMag
					  If dMag = 0.0 Then
					     SetOutputQuality 2, "B"
					  Else
					     SetOutputName 2, "Baro"
					     SetOutputUnits 2, "mb"
					     SetOutputQuality 2, "G"
					  End If
					  
					'Set the #2 output to dDir and set the quality
					  SetOutputData 2, dDir
					  If dDir = 0.0 Then
					     SetOutputQuality 2, "B"
					  Else
					     SetOutputName 2, "Baro"
					     SetOutputUnits 2, "mb"
					     SetOutputQuality 2, "G"
					  End If	  
					 
			Else
			End If
		Else
			If MsgID = "PNORC" Then
			Else
			End If
		End If
	Next
Close F1
End Sub

Sub Stop_Recording
	Close #1
	StatusMsg "Closing AWAC COM Port"
End Sub
