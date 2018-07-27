#Define GN_HEADERCLICK		1				'User clicked header
#Define GN_BUTTONCLICK		2				'Sendt when user clicks the button in a button cell
#Define GN_CHECKCLICK		3				'Sendt when user double clicks the checkbox in a checkbox cell
#Define GN_IMAGECLICK		4				'Sendt when user double clicks the image in an image cell
#Define GN_BEFORESELCHANGE	5				'Sendt when user request a selection change
#Define GN_AFTERSELCHANGE	6				'Sendt after a selection change
#Define GN_BEFOREEDIT		7				'Sendt before the cell edit control shows
#Define GN_AFTEREDIT			8				'Sendt when the cell edit control is about to close
#Define GN_BEFOREUPDATE		9				'Sendt before a cell updates grid data
#Define GN_AFTERUPDATE		10				'Sendt after grid data has been updated
#Define GN_USERCONVERT		11				'Sendt when user cell needs to be converted.

'Messages
#Define GM_ADDCOL				WM_USER+1	'wParam=0, lParam=lpCOLUMN
#Define GM_ADDROW				WM_USER+2	'wParam=0, lParam=lpROWDATA (can be NULL)
#Define GM_INSROW				WM_USER+3	'wParam=nRow, lParam=lpROWDATA (can be NULL)
#Define GM_DELROW				WM_USER+4	'wParam=nRow, lParam=0
#Define GM_MOVEROW			WM_USER+5	'wParam=nFromRow, lParam=nToRow
#Define GM_COMBOADDSTRING	WM_USER+6	'wParam=nCol, lParam=lpszString
#Define GM_COMBOCLEAR		WM_USER+7	'wParam=nCol, lParam=0
#Define GM_GETCURSEL			WM_USER+8	'wParam=0, lParam=0
#Define GM_SETCURSEL			WM_USER+9	'wParam=nCol, lParam=nRow
#Define GM_GETCURCOL			WM_USER+10	'wParam=0, lParam=0
#Define GM_SETCURCOL			WM_USER+11	'wParam=nCol, lParam=0
#Define GM_GETCURROW			WM_USER+12	'wParam=0, lParam=0
#Define GM_SETCURROW			WM_USER+13	'wParam=nRow, lParam=0
#Define GM_GETCOLCOUNT		WM_USER+14	'wParam=0, lParam=0
#Define GM_GETROWCOUNT		WM_USER+15	'wParam=0, lParam=0
#Define GM_GETCELLDATA		WM_USER+16	'wParam=nRowCol, lParam=lpData
#Define GM_SETCELLDATA		WM_USER+17	'wParam=nRowCol, lParam=lpData
#Define GM_GETCELLRECT		WM_USER+18	'wParam=nRowCol, lParam=lpRECT
#Define GM_SCROLLCELL		WM_USER+19	'wParam=0, lParam=0
#Define GM_GETBACKCOLOR		WM_USER+20	'wParam=0, lParam=0
#Define GM_SETBACKCOLOR		WM_USER+21	'wParam=nColor, lParam=0
#Define GM_GETGRIDCOLOR		WM_USER+22	'wParam=0, lParam=0
#Define GM_SETGRIDCOLOR		WM_USER+23	'wParam=nColor, lParam=0
#Define GM_GETTEXTCOLOR		WM_USER+24	'wParam=0, lParam=0
#Define GM_SETTEXTCOLOR		WM_USER+25	'wParam=nColor, lParam=0
#Define GM_ENTEREDIT			WM_USER+26	'wParam=nCol, lParam=nRow
#Define GM_ENDEDIT			WM_USER+27	'wParam=nRowCol, lParam=fCancel
#Define GM_GETCOLWIDTH		WM_USER+28	'wParam=nCol, lParam=0
#Define GM_SETCOLWIDTH		WM_USER+29	'wParam=nCol, lParam=nWidth
#Define GM_GETHDRHEIGHT		WM_USER+30	'wParam=0, lParam=0
#Define GM_SETHDRHEIGHT		WM_USER+31	'wParam=0, lParam=nHeight
#Define GM_GETROWHEIGHT		WM_USER+32	'wParam=0, lParam=0
#Define GM_SETROWHEIGHT		WM_USER+33	'wParam=0, lParam=nHeight
#Define GM_RESETCONTENT		WM_USER+34	'wParam=0, lParam=0
#Define GM_COLUMNSORT		WM_USER+35	'wParam=nCol, lParam=0=Ascending, 1=Descending, 2=Invert
#Define GM_GETHDRTEXT		WM_USER+36	'wParam=nCol, lParam=lpBuffer
#Define GM_SETHDRTEXT		WM_USER+37	'wParam=nCol, lParam=lpszText
#Define GM_GETCOLFORMAT		WM_USER+38	'wParam=nCol, lParam=lpBuffer
#Define GM_SETCOLFORMAT		WM_USER+39	'wParam=nCol, lParam=lpszText
#Define GM_CELLCONVERT		WM_USER+40	'wParam=nRowCol, lParam=lpBuffer
#Define GM_RESETCOLUMNS		WM_USER+41	'wParam=0, lParam=0
#Define GM_GETROWCOLOR		WM_USER+42	'wParam=nRow, lParam=lpROWCOLOR
#Define GM_SETROWCOLOR		WM_USER+43	'wParam=nRow, lParam=lpROWCOLOR
#Define GM_GETCOLDATA		WM_USER+44	'wParam=nCol, lParam=lpCOLUMN

'Column alignment
#Define GA_ALIGN_LEFT		0
#Define GA_ALIGN_CENTER		1
#Define GA_ALIGN_RIGHT		2

'Column types
#Define TYPE_EDITTEXT		0				'String
#Define TYPE_EDITLONG		1				'Long
#Define TYPE_CHECKBOX		2				'Long
#Define TYPE_COMBOBOX		3				'Long
#Define TYPE_HOTKEY			4				'Long
#Define TYPE_BUTTON			5				'String
#Define TYPE_IMAGE			6				'Long
#Define TYPE_DATE				7				'Long
#Define TYPE_TIME				8				'Long
#Define TYPE_USER				9				'0=String, 1 to 512 bytes binary data
#Define TYPE_EDITBUTTON		10				'String

'Column sorting
#Define SORT_ASCENDING		0
#Define SORT_DESCENDING		1
#Define SORT_INVERT			2

'Window styles
#Define STYLE_NOSEL			01
#Define STYLE_NOFOCUS		02
#Define STYLE_HGRIDLINES	04
#Define STYLE_VGRIDLINES	08
#Define STYLE_GRIDFRAME		10

#Define ODT_GRID				6

Type COLUMN
	colwt As Integer							'Column width
	lpszhdrtext As PTSTR						'Pointer to header text.
	halign As Integer							'Header text alignment.
	calign As Integer							'Column text alignment.
	ctype As Integer							'Column data type.
	ctextmax As Integer						'Max text lenght for TYPE_EDITTEXT and TYPE_EDITLONG.
	lpszformat As Integer					'Format string for TYPE_EDITLONG.
	himl As Integer							'Handle of image list. For the image columns and combobox only.
	hdrflag As Integer						'Header flags.
	colxp As Integer							'Column position. Internally used.
	edthwnd As Integer						'Column control handle. Internally used.
	lParam As Integer							'User defined 32 bit value.
End Type

Type ROWCOLOR
	backcolor As Integer
	textcolor As Integer
End Type

'Notifications
Type GRIDNOTIFY
	nmhdr As NMHDR
	col As Integer								'Column
	row As Integer								'Row
	hwnd As Integer							'Handle of column edit control
	lpdata As Integer							'Pointer to data
	fcancel As Integer						'Set to TRUE to cancel operation
End Type


