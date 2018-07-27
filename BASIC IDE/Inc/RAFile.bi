' Messages
#Define FBM_SETPATH				WM_USER+1	' wParam=FALSE/TRUE(Refresh), lParam=Pointer to path string
#Define FBM_GETPATH				WM_USER+2	' wParam=0, lParam=Pointer to path string
#Define FBM_SETFILTERSTRING	WM_USER+3	' wParam=FALSE/TRUE(Refresh), lParam=Pointer to filter string
#Define FBM_GETFILTERSTRING	WM_USER+4	' wParam=0, lParam=Pointer to filter string
#Define FBM_SETFILTER			WM_USER+5	' wParam=FALSE/TRUE(Refresh), lParam=FALSE/TRUE(Filter On/Off)
#Define FBM_GETFILTER			WM_USER+6	' wParam=0, lParam=0
#Define FBM_SETSELECTED			WM_USER+7	' wParam=0, lParam=lpFile
#Define FBM_GETSELECTED			WM_USER+8	' wParam=0, lParam=lpFile
#Define FBM_SETBACKCOLOR		WM_USER+9	' wParam=0, lParam=nColor
#Define FBM_GETBACKCOLOR		WM_USER+10	' wParam=0, lParam=0
#Define FBM_SETTEXTCOLOR		WM_USER+11	' wParam=0, lParam=nColor
#Define FBM_GETTEXTCOLOR		WM_USER+12	' wParam=0, lParam=0

' Notifications
#Define FBN_PATHCHANGE			1
#Define FBN_DBLCLICK				2

Type FBNOTIFY
	nmhdr As NMHDR
	lpfile As ZString Ptr
End Type

' Styles
#Define FBSTYLE_FLATTOOLBAR	1
#Define FBSTYLE_DIVIDERLINE	2

Const szFBClassName As String="RAFileBrowser"
