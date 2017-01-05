'----- G "PORTS" Message Formatter -----------------------------------------------
'
'     This basic code is the self timed formatter for storing GOES messages
'     We make use of the XPert's ability to handle custom GOES formatters
'     written by the user in BASIC.  A fundamental part of this capability is
'     appending data to a message built by the C++ XPert code (e.g. binary interleaved).
'     In this case, we capture the message and add the last message.  The capture
'     is done by writing the message to RECENT.DAT.  That way, the user can define a
'     special log in name and password and retrieve the message that way.
'
'     The function SELFTIMED_STFormatter handles the message capture.  Most of this
'     routine is straight from the XPert BASIC manual.
' ------------------------------------------------------------------------------
' Declare external function used to get current satlink self-timed message
' extern "C" _declspec(dllexport) LPCTSTR GetSTCurrentMessage()
' 
' At some point after 3.4.0.6 it became necessary to add this external call.
Declare Function GetSTCurrentMessage Lib "\Windows\Satlink.sll" As String

'------------------------------------------------------------------------------
'
Public FUNCTION SELFTIMED_STFormatter
   'Note here that Selftime_STFormatter is the current message built by the C code
    sTr1 = ""
    sTr2 = ""
    F1 = FreeFile
    F2 = FreeFile
    F3 = FreeFile
   Open "Recent.Dat" For Output As F3
   OPEN "\Flash Disk\Sat.Dat" FOR OUTPUT AS F1
   ' At some point after 3.4.0.6 it became necessary to add the external call below.
   Selftimed_STFormatter = GetSTCurrentMessage
   PRINT F1,  Selftimed_STFormatter
   Close F1
   OPEN "\Flash Disk\Sat.Dat" FOR Input AS F1
   Line Input F1, sTr1
   Print F3, sTr1
   Close F1
   On Error Goto 10   'When routine is first run, if Satold.Dat does not exist it does not crash the routine
   Open "\Flash Disk\Satold.Dat" For Input As F2
   Line Input F2, sTr2
   Print F3, sTr2
   CLOSE F2
10 Close F3
   Open "\Flash Disk\Satold.Dat" For Output As F2
   Print F2, sTr1      'Replace the old message with the new message
   Close F2
END FUNCTION
