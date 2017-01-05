'  Recent.DatStringBuilder BASIC - November, 2007
'
'  This file is used to build Recent.DAT files for transmission by Iridium
'  Modified for AGWL and Laser January 2008
'
'  xxxxxxxxxxxxxxxx  GLOBAL VARIABLES xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
STATIC PreviousDay = 0      'Day used to reference file names
STATIC PreviousFile = "xxxx"    'Place to hold open file name
' Declarations of functions and subroutines
DECLARE FUNCTION ReadLog(LogName, Sensor, TimeStamp)

' Global variables
' Variables used to store results of ReadLog function.
RLData = 0.0
RLQuality = "U"
RLUnits = ""
TimeFound = 0
DataLog = "SSP.log"         'Should be name of log where data are stored
sParam1 = "SNS"     'This is the 1ST parameter the display will output
sParam2 = "DAT"     'This is the 2ND parameter the display will output
sParam3 = "BAT"     'This is the 3RD parameter the display will output
sParam4 = "PAROS1"     'This is the 4TH parameter the display will output
sParam5 = "P1STD"     'This is the 5TH parameter the display will output
sParam6 = "P1OUT"     'This is the 6TH parameter the display will output
sParam7 = "WT"     'This is the 7TH parameter the display will output
sParam8 = "BP"     'This is the 8TH parameter the display will output
sParam9 = "PWS"     'This is the 9TH parameter the display will output
sParam10 = "PWD"    'This is the 10TH parameter the display will output
sParam11 = "PWG"    'This is the 11TH parameter the display will output
iLookBack = 60      'Number of seconds delay between "NOW" and posting of log data
iLookBack2 = 150     'Number of seconds delay between "NOW" and posting of log data
iLookBack3 = 110    'Number of seconds delay between "NOW" and posting of log data


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Public SUB SCHED_RecentDatStringBuilder


    'Set up for the Recent.Dat file
    iDatFile = FREEFILE
    strDatFileName = "\Flash Disk\RECENT.DAT"
    OPEN strDatFileName FOR OUTPUT AS iDatFile

    'Get the time, and figure out what the day is
    DateNow = Date
    DayNow = Day(DateNow)

    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    'Build a time string from the XPert clock
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    dnow = Date
    iYear = Year(Date) - 2000 'to get in same format as ADCP
    iMonth = Month(Date)
    iDay = Day(Date)
    tnow = Now
    iHH = Hour(tnow)

	' WMH Note: I don't know why 1 and 50 are subtracted from these values.
    iMM = Minute(tnow) '- 1
    iSS = Second(tnow) '- 50
    TimeStamp = Format("%02d/%02d/%02d %02d:%02d:%02d", iMonth, iDay, iYear, iHH, iMM, iSS)
    PRINT iDatFile, TimeStamp;
    SLEEP 0
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    'Build a string to put in Recent.Dat
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    tStart = DateSerial(Year(tNow), Month(tNow), Day(tNow)) + TimeSerial(Hour(tNow), Minute(tNow), Second(tNow))
    t = tStart - iLookBack 'back off now by iLookBack seconds (set at top of code)
IF (ReadLog(DataLog,sParam1, t) = 1) THEN
    'add to output string
    sValue = Format("%1.3f",RLData)
    PRINT iDatFile, "   SNS: " & sValue;
END IF
IF (ReadLog(DataLog,sParam2, t) = 1) THEN
    'add to output string
    sValue = Format("%1.3f",RLData)
    PRINT iDatFile, "   DAT: " & sValue;
END IF
IF (ReadLog(DataLog,sParam3, t) = 1) THEN
    'add to output string
    sValue = Format("%2.1f",RLData)
    PRINT iDatFile, "   BAT: " & sValue;
END IF
    t = tStart - iLookBack2 'back off now by iLookBack2 seconds (set at top of code)
IF (ReadLog(DataLog,sParam4, t) = 1) THEN
    'add to output string
    sValue = Format("%2.3f",RLData)
    PRINT iDatFile, "    PAROS1: " & sValue;
END IF
IF (ReadLog(DataLog,sParam5, t) = 1) THEN
    'add to output string
    sValue = Format("%2.3f",RLData)
    PRINT iDatFile, "    P1STD: " & sValue;
END IF
IF (ReadLog(DataLog,sParam6, t) = 1) THEN
    'add to output string
    sValue = Format("%2.0f",RLData)
    PRINT iDatFile, "      P1OUT: " & sValue;
END IF

IF (ReadLog(DataLog,sParam7, t) = 1) THEN
    'add to output string
    sValue = Format("%2.1f",RLData)
    PRINT iDatFile, "      WT: " & sValue;
END IF
IF (ReadLog(DataLog,sParam8, t) = 1) THEN
    'add to output string
    sValue = Format("%4.1f",RLData)
    PRINT iDatFile, "      BP: " & sValue;
END IF
    t = tStart - iLookBack3 'back off now by iLookBack2 seconds (set at top of code)
IF (ReadLog(DataLog,sParam9, t) = 1) THEN
    'add to output string
    sValue = Format("%2.1f",RLData)
    PRINT iDatFile, "     PWS: " & sValue;
END IF
IF (ReadLog(DataLog,sParam10, t) = 1) THEN
    'add to output string
    sValue = Format("%3.0f",RLData)
    PRINT iDatFile, "      PWD: " & sValue;
END IF
IF (ReadLog(DataLog,sParam11, t) = 1) THEN
    'add to output string
    sValue = Format("%2.1f",RLData)
    PRINT iDatFile, "      PWG: " & sValue
END IF
    CLOSE iDatFile
END SUB

'Run on start
'
CALL SCHED_RecentDatStringBuilder



' Function to read a specific sensor from log.
'
FUNCTION ReadLog(LogName, Sensor, TimeStamp)
   ' LogName, Sensor, and TimeStamp are inputs. RLData, RLQuality, and RLUnits
   ' are global variables that receive this function's outputs.
   ' If Sensor at Timestamp is found, 1 is returned. Otherwise, 0.
   ReadLog = 0
   TYPE = 0
   TimeFound = 0
   SensorFound = ""
   FileNum = FREEFILE
   OPEN LogName FOR LOG AS FileNum
   SEEK FileNum, TimeStamp
   IF NOT EOF(FileNum) THEN
      INPUT FileNum, TYPE, TimeFound, SensorFound, RLData, RLQuality, RLUnits
      DO WHILE TimeFound = TimeStamp AND NOT EOF(FileNum)
         IF SensorFound = Sensor THEN
            IF RLQuality = "G" THEN
               ReadLog = 1
            END IF
            EXIT DO
         ELSE
            ' Log may contain multiple entries for this time-slot so keep looking.
            ' Original Seek finds last entry for specified time, so move to previous.
            SEEK FileNum, NEXT
            INPUT FileNum, TYPE, TimeFound, SensorFound, RLData, RLQuality, RLUnits
         END IF
      END LOOP
   END IF
   CLOSE FileNum
END FUNCTION
