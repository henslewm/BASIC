Public Sub SCHED_GARMINSOG
    ' this subroutine runs periodically to read out the 
    ' GPS and write the SOG/TimeStamp to a file.
    ' the routine turns on the GPS using Output 4 and 
    ' leaves it on the routine expects the GPS to be 
    ' connected to COM2 the routine expects the data 
    ' in NEMA $GPRMC format
    ' *************************************************
    ' * Modified by: Winston Hensley
    ' *          on: 08/31/2011
    ' *
    ' *************************************************


    Const BAUD = 4800
    Const NOPARITY = 0
    Const NOHANDSHAKE = 0
    Const GPSTimeout = 900 'allow 60*15minutes to get the GPS signal

    '***'Variables initilized
    '*** Open port, abort if recording stopped
    Port = FreeFile

    If Abort Then Goto ErrorHandler
    On Error Resume Next
    StatusMsg "Opening GPS port"
    Open "Com2:" as Port nowait

    If Err <> 0 Then
        ErrorMsg "Failed to open com port, err " &Err
        Goto ErrorHandler
    End If

    StatusMsg "GPS port opened"
    SetPort Port, BAUD, NOPARITY, 8, 1, NOHANDSHAKE
    StartTime = Time
    TimeValid = false
    Digital 1, 4, -1 'turn on the GPS if its control is connected to COM4
    Do
    SetTimeout Port,35
    '*** port opened and initialized
    StatusMsg "GPS port Initialized"    


    'GPS sends data out every second in the format:
    '$GPRMC,hhmmss,s,ddmm.mmmm,h,ddmm.mmmm,h,000.0,000.0,ddmmyy,000.0,a*hh<CR><LF>
    FlushInput Port ' get rid of any data in the input buffer

    if Err then
        ErrorMsg "FlushInput failed " &Err
    end if


    WaitFor Port, "$GPRMC," ' waitfor message identifier   

    if Err then
        ErrorMsg "Failed to get $GPRMC"
        Goto ErrorHandler
    end if

    ' Whatever is in the following StatusMsg doesn't carry through to the Input Port line below.
    StatusMsg "Got $GPRMC,"

    UTC = 0
    Status = ""
    Latitude = 0.0
    LatH = ""
    Longitude = 0.0
    LongH = ""
    SOG = 0.0
    COG = 0.0
    UTCDate = 0
    MagVar = 0.0
    MagVarDir = ""
    ModeInd = ""

    'Two following lines for debug.
    'Line Input Port, strdata
    'StatusMsg strdata

    Input Port, UTC, Status, Latitude, LatH, Longitude, LongH, SOG, COG, UTCDate, MagVar, MagVarDir, ModeInd


     LogFile=FreeFile
       Open "System.Log" For Log As LogFile
       Log LogFile, Now, "LAT", Latitude, LatH
       Log LogFile, Now, "LONG", Longitude, LongH
       Log LogFile, Now, "SOG", SOG,"KTS"
       Log LogFile, Now, "COG", COG,"DEG"
     Close LogFile


    if Err then
        ' couldn't get data
        ErrorMsg "Input error " &Err
        goto ErrorHandler
    end if

    ' we got data, now see if it is any good
    ' check to see if we have GPS lock
    StatusMsg "MSG " &UTC &" " &latitude &" " &SOG &" " &COG

    if (UTC > 0) AND (Status = "A") then
        SOGValid = true
    end if

    loop until (SOGValid OR ((Time - StartTime)> GPSTimeout))

    if ((Time - StartTime) > GPSTimeout) then
        ErrorMsg "Timeout waiting for GPS --"
        
        goto ErrorHandler
    end if

    Hr = int(UTC / 10000)
    Min = int((UTC - (Hr * 10000))/100)
    Sec = UTC Mod 100
    GPSTime=TimeSerial(Hr, Min, Sec)

    ErrorHandler:
    Close Port

End Sub
