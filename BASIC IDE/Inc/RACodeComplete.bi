
' CodeCompleteListBox Styles
#Define STYLE_USEIMAGELIST		1
' CodeCompleteListBox Messages
#Define CCM_ADDITEM			WM_USER+0			' wParam=Type, lParam=lpString, Returns nothing
#Define CCM_DELITEM			WM_USER+1			' wParam=Index, lParam=0, Returns nothing
#Define CCM_GETITEM			WM_USER+2			' wParam=Index, lParam=0, Returns pointer to string or NULL
#Define CCM_GETCOUNT			WM_USER+3			' wParam=0, lParam=0, Returns count
#Define CCM_CLEAR				WM_USER+4			' wParam=0, lParam=0, Returns nothing
#Define CCM_SETCURSEL		WM_USER+5			' wParam=Index, lParam=0, Returns nothing
#Define CCM_GETCURSEL		WM_USER+6			' wParam=0, lParam=0, Returns Index
#Define CCM_GETTOPINDEX		WM_USER+7			' wParam=0, lParam=0, Returns TopIndex
#Define CCM_SETTOPINDEX		WM_USER+8			' wParam=TopIndex, lParam=0, Returns nothing
#Define CCM_GETITEMRECT		WM_USER+9			' wParam=Index, lParam=lpRECT, Returns nothing
#Define CCM_SETVISIBLE		WM_USER+10			' wParam=0, lParam=0, Returns nothing
#Define CCM_FINDSTRING		WM_USER+11			' wParam=Start Index, lParam=lpString, Returns Index or -1
#Define CCM_SORT				WM_USER+12			' wParam=Descending (TRUE/FALSE), lParam=delete equal (TRUE/FALSE), Returns nothing
#Define CCM_GETCOLOR			WM_USER+13			' wParam=0, lParam=lpCC_COLOR, Returns nothing
#Define CCM_SETCOLOR			WM_USER+14			' wParam=0, lParam=lpCC_COLOR, Returns nothing
#Define CCM_ADDLIST			WM_USER+15			' wParam=0, lParam=lpCC_ADDLIST, Returns nothing
#Define CCM_GETMAXWIDTH		WM_USER+16			' wParam=0, lParam=0, Returns length of longest Item

Type CC_COLOR
	back As Integer
	text As Integer
End Type

Type CC_ADDLIST
	lpszList As ZString Ptr
	lpszFilter As ZString Ptr
	nType As Integer
End Type

' CodeCompleteToolTip Styles
#Define STYLE_USEPARANTESES	1
' CodeCompleteToolTip Messages
#Define TTM_SETITEM			WM_USER+0			' wParam=0, lParam=lpTTITEM, Returns x-offset
#Define TTM_GETCOLOR			WM_USER+1			' wParam=0, lParam=lpTT_COLOR, Returns nothing
#Define TTM_SETCOLOR			WM_USER+2			' wParam=0, lParam=lpTT_COLOR, Returns nothing
#Define TTM_GETITEMNAME		WM_USER+3			' wParam=0, lParam=lpTTITEM, Returns pointer to item string
#Define TTM_SCREENFITS		WM_USER+4			' wParam=0, lParam=lpPOINT, Returns nothing

Type TT_COLOR
	back As Integer
	text As Integer
	api As Integer
	hilite As Integer
End Type

Type TTITEM
	lpszApi As ZString Ptr							' Pointer to api string
	lpszParam As ZString Ptr						' Pointer to comma separated parameters string
	nitem As Integer									' Item to hilite
	lpszRetType As ZString Ptr						' Pointer to return type string
	lpszDesc As ZString Ptr							' Pointer to item description
	novr As Integer									' Totals of functions
	nsel As Integer									' Actual function
	nwidth As Integer									' Width of tooltip
End Type

Const szCCLBClassName = "RACodeComplete"
Const szCCTTClassName = "RAToolTip"
