'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Filename: 	AIRMAR.bas
' Author:   	Philip Libraro
' Date:     	Long long ago.
' Modified: 	Winston Hensley - 02/23/2015
' Decription: 	This code assumes that AIRMAR_INIT has modified the default 
'      			settings for the AirMar WX150 All-in-One Weather Station. 
'        		All output is suppressed except Wind/Baro/Temp and GPS Lat/Long
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This program supports measuring AIRMAR WEATHER sensor. Assumes sensor has been
' initialized (all strings aside from weather&GPS have been shut off) and measures
' sensor at schedule user assigns to measure blocks.
' On power up, Wind speed and direction will not be available until GPS lock
' is achieved.  This could take a minute or two.
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'***'Variables initilized
CONST AIRMAR_CONNECTED = 1       ' Easily disable code for testing.
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
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
CONST PORT = "COM4:"
CONST PORT_TIMEOUT = 0.5
CONST SENSORNAME = "AIRMAR"        ' Name used on block output and sensors page
Const BAUD = 4800
Const NOPARITY = 0
Const NOHANDSHAKE = 0


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Initialize Sensor
'
' This code runs at recording start. Assumes we have disabled all lines aside
' from the wind, baro, and temperature output line. The Port is opened
' and left open for better performance.

IF AIRMAR_CONNECTED THEN
 '*** Open port, abort if recording stopped
  If Abort Then Goto ErrorHandler
  On Error Resume Next
  StatusMsg "Opening AIRMAR port"
  Open PORT as #1  'iFilenum
  If Err <> 0 Then
     ErrorMsg "Failed to open com port, err " &Err
     Goto ErrorHandler
  End If
  StatusMsg "AIRMAR port opened"
  SetPort #1, BAUD, NOPARITY, 8, 1, NOHANDSHAKE
  StartTime = Time
  TimeValid = false
  SetTimeout #1,35
ELSE
ErrorHandler:
      ErrorMsg Format("AIRMAR: Open port %s failed.", PORT)
END IF
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' Measure Sensor
'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Public Sub SENSOR_AIRMAR

'Initialize Variables
NMEA_Messages = 2
Message_Location = 0
'Number of empty strings below corresponds to 
'number of messages in variable above.
sInBuf 	= Array("","")
MSGID 	= ""
TEMPC 	= 0
PRESIN 	= 0.0
PRESMB 	= 0.0
Q1 		= "b"
Q2 		= "b"
Q3 		= "b"
Q4 		= "b"
Q5 		= "b"
Q6 		= "b"
Q7 		= "b"
Q8 		= "b"
Q9 		= "b"
NG 		= ""
N 			= 0.0
COG 		= 0.0
DIRT 		= 0.0
DIRM 		= 0.0
SPDN 		= 0.0
SPDM 		= 0.0
Asterisk = ""
GPSLat  	= 0.0
GPSLong 	= 0.0
NMEA_message = ""

' Output from GPGGA
GGA_UTC 		= 0 
GGA_lat		= 0
GGA_ns		= "N"
GGA_long		= 0
GGA_ew		= "E"
GGA_quality = 0.0
GGA_num 		= 0
GGA_HDOP		= ""
GGA_Alt  	= 0
GGA_M    	= ""
GGA_geoid	= 0

' NMEA CheckSum Variable 
Cksum 	= ""
On Error Resume Next
  
'------------------------------------------------------------------------------------------
'Start Time
'------------------------------------------------------------------------------------------
tStart = Now

'Synchronization string is $
'Typical string from the AirMar for Weather:
'$WIMDA,29.9286,I,1.0135,B,30.5,C,,,,,,,147.3,T,158.0,M,1.1,N,0.6,M*2F

'Typical string from the AirMar for GPS:
'$GPGGA,140503.99,3645.8079,N,07615.7310,W,1,5,3.8,628.7,M,-37.4,M,,*67

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

' Open the file back up for parsing. Non-elegant but necessary.
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
				
		''''''''''''''''''''''''''''''''''''' QC & Record Winds'''''''''''''''''''''''''''''''''
		If MsgID = "WIMDA" Then
			Input F1, MsgID, PRESIN, Q1, PRESMB, Q2, TEMPC, Q3, NG, NG, NG, NG, NG, NG, DIRT, Q4, DIRM, Q5, SPDN, Q6, SPDM, Cksum
			
			' Don't understand why variables re-defined here.
			dPRES = PRESMB 'numerical PRESSURE
			dTEMP = TEMPC 'numerical LONG
			dDIR = DIRT 'numerical wind direction true
			dSPD = SPDM 'numerical wind speed
			dPRES = dPRES * 1000
			
			  'Set the #2 output to PRESSURE and set the quality
				  SetOutputData 2, dPRES
				  If dPRES = 0.0 Then
				     SetOutputQuality 2, "B"
				  Else
				     SetOutputName 2, "Baro"
				     SetOutputUnits 2, "mb"
				     SetOutputQuality 2, "G"
				  End If
			
			  'Set the #3 output to TEMPERATURE and set the quality
				  SetOutputData 3, dTEMP
				  If dTEMP = 0.0 Then
				     SetOutputQuality 3, "B"
				  Else
				     SetOutputName 3, "ATemp"
				     SetOutputUnits 3, "C"
				     SetOutputQuality 3, "G"
				  End If
			
			  'Set the #4 output to WIND DIRECTION TRUE and set the quality
			     SetOutputName 4, "WDirTrue"
			     SetOutputData 4, dDIR
			     SetOutputUnits 4, "Deg"
			
			  'Set the #5 output to WIND SPEED and set the quality
			     SetOutputName 5, "WSpd"
			     SetOutputData 5, dSPD
			     SetOutputUnits 5, "M/S"
			
		Else
			
			''''''''''''''''''''''''''''''''''''' QC & Record GPS''''''''''''''''''''''''''''''''		
				If MsgID = "GPGGA" Then
					Input F1, MsgID, GGA_UTC, GGA_lat, NG, GGA_long, GGA_ew, GGA_quality, NG, NG, NG, GGA_ns, NG, NG, NG, Cksum
				    
				    'Unable to set the GGA_lat and GGA_long directions. The string output N/S/E/W 
				    'seems to come out in a '0 0' form. Perhaps BASIC forces all arguments to be 
				    'same data type as first found when parsing?
			  'Set the #6 output to Longitude
			     SetOutputName 6, "Lat"
			     SetOutputData 6, GGA_lat
			     SetOutputUnits 6, "min"
			
			  'Set the #7 output to Latitude
			     SetOutputName 7, "Long"
			     SetOutputData 7, GGA_long
			     SetOutputUnits 7, "min"
					
				Else
				End If
		End If
	Next
Close F1
End Sub

