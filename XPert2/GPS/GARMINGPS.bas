Public Sub SCHED_GARMINGPS

    ' this subroutine runs periodically (e.g. every hour) to
    ' read the GPS and set the Xpert clock.
    ' the routine turns on the GPS using Output 4
    ' the routine expects the GPS to be connected to COM2
    ' the routine expects the data in NEMA $GPGGA format
    ' *************************************************
    ' * Modified by: Winston Hensley
    ' *          on: 08/30/2011
    ' *
    ' *************************************************


    Const BAUD = 4800
    Const NOPARITY = 0
    Const NOHANDSHAKE = 0
    Const GPSTimeout = 900 'allow 60*15 minutes to get the GPS signal

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
    Digital 1, 4, -1 'turn on the GPS if its control is connected to DIO4
    Do
    SetTimeout Port,35
    '*** port opened and initialized
    StatusMsg "GPS port Initialized"    


    'GPS sends data out every second in the format:
    '$GPGGA,hhmmss,llll.lll,a,nnnnn.nnn,b,t,uu,v.v,w.w,M,x.x,M,y.y,zzzz&hh<CR><LF>
    FlushInput Port ' get rid of any data in the input buffer

    if Err then
        ErrorMsg "FlushInput failed " &Err
    end if


    WaitFor Port, "$GPGGA," ' waitfor message identifier   

    if Err then
        ErrorMsg "Failed to get $GPGGA"
        Goto ErrorHandler
    end if

    ' Whatever is in the following StatusMsg doesn't carry through to the Input Port line below.
    StatusMsg "Got $GPGGA,"

    MSGID = 0
    UTC = 0
    Latitude = 0.0
    LatH = 0
    Longitude = 0.0
    LongH = 0
    GPSQual = 0
    NumSats = 0
    HorDil = 0.0
    AntAlt = 0.0
    Meters = ""
    AgeDiff = 0.0
    DiffRef = 0
    Asterisk = ""
    Cksum = 0
    strdata = ""

    'Two following lines for debug.
    'Line Input Port, strdata
    'StatusMsg strdata

    Input Port, UTC, Latitude, LatH, Longitude, LongH, GPSQual, NumSats, HorDil, AntAlt, Meters, AgeDiff, DiffRef, Asterisk, Cksum

    if Err then
        ' couldn't get data
        ErrorMsg "Input error " &Err
        goto ErrorHandler
    end if

    ' we got data, now see if it is any good
    ' check to see if we have GPS lock
    StatusMsg "MSG " &UTC &" " &latitude &" " &gpsqual &" " &numsats

    if (NumSats > 0) AND (GPSQual > 0) then
        TimeValid = true
    end if

    loop until (TimeValid OR ((Time - StartTime)> GPSTimeout))

    if ((Time - StartTime) > GPSTimeout) then
        ErrorMsg "Timeout waiting for GPS -- NumSats: " &NumSats &" GPSQUAL: "
        &GPSQual
        goto ErrorHandler
    end if

    Hr = int(UTC / 10000)
    Min = int((UTC - (Hr * 10000))/100)
    Sec = UTC Mod 100
    GPSTime=TimeSerial(Hr, Min, Sec)

    if ((Time-GPSTime)>1) Or ((GPSTime-Time)>1) then
        Time = GPSTime
        StatusMsg "Time set to " &GPSTime
    else
        StatusMsg "Time too close " &GPSTime
    end if

    ErrorHandler:
    Close Port
    Digital 1, 4, 0 ' turn off GPS

End Sub
