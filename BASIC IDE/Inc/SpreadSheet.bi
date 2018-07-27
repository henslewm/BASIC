'Messages
Const SPRM_SPLITTHOR				= WM_USER+100		'Create horizontal splitt in current splitt at current row. wParam=0, lParam=0
Const SPRM_SPLITTVER				= WM_USER+101		'Create vertical splitt in current splitt at current col. wParam=0, lParam=0
Const SPRM_SPLITTCLOSE			= WM_USER+102		'Close the current splitt. wParam=0, lParam=0
Const SPRM_SPLITTSYNC			= WM_USER+103		'Syncronizez a splitt window witit's parent. wParam=0, lParam=TRUE/FALSE
Const SPRM_GETSPLITTSTATE		= WM_USER+104		'*Get splitt state. wParam=nWin(0-7), if nWin=-1 active split window, lParam=0
Const SPRM_GETCELLRECT			= WM_USER+105		'Get the current cells rect in active splitt. wParam=0, lParam=pointer to RECT struct. Returns handle of active splitt window.
Const SPRM_GETLOCKCOL			= WM_USER+106		'Get lock cols in active splitt. wParam=0, lParam=0
Const SPRM_SETLOCKCOL			= WM_USER+107		'Lock cols in active splitt. wParam=0, lParam=cols
Const SPRM_GETLOCKROW			= WM_USER+108		'Get lock rows in active splitt. wParam=0, lParam=0
Const SPRM_SETLOCKROW			= WM_USER+109		'Lock rows in active splitt. wParam=0, lParam=rows
Const SPRM_DELETECOL				= WM_USER+110		'Delete col. wParam=col, lParam=0
Const SPRM_INSERTCOL				= WM_USER+111		'Insert col. wParam=col, lParam=0
Const SPRM_DELETEROW				= WM_USER+112		'Delete row. wParam=row, lParam=0
Const SPRM_INSERTROW				= WM_USER+113		'Insert row. wParam=row, lParam=0
Const SPRM_GETCOLCOUNT			= WM_USER+114		'Get number of columns. wParam=0, lParam=0
Const SPRM_SETCOLCOUNT			= WM_USER+115		'Set number of columns. wParam=nCols, lParam=0
Const SPRM_GETROWCOUNT			= WM_USER+116		'Get number of rows. wParam=0, lParam=0
Const SPRM_SETROWCOUNT			= WM_USER+117		'Set number of rows. wParam=nRows, lParam=0
Const SPRM_RECALC					= WM_USER+118		'Recalculates the sheet
Const SPRM_BLANKCELL				= WM_USER+119		'Blank a cell. wParam=col, lParam=row
Const SPRM_GETCURRENTWIN		= WM_USER+120		'Get active splitt window. wParam=0, lParam=0
Const SPRM_SETCURRENTWIN		= WM_USER+121		'Set active splitt window. wParam=0, lParam=nWin (0-7)
Const SPRM_GETCURRENTCELL		= WM_USER+122		'Get current col/row in active window. wParam=0, lParam=0. Returns Hiword=row, Loword=col
Const SPRM_SETCURRENTCELL		= WM_USER+123		'Set current col/row in active window. wParam=col, lParam=row
Const SPRM_GETCELLSTRING		= WM_USER+124		'*Get content of current cell. wParam=0, lParam=0. Returns a pointer to a null terminated string.
Const SPRM_SETCELLSTRING		= WM_USER+125		'Set content of current cell. wParam=type, lParam=pointer to string.
Const SPRM_GETCOLWIDT		  	= WM_USER+126		'Get column width. wParam=col, lParam=0. Returns column width.
Const SPRM_SETCOLWIDT		  	= WM_USER+127		'Set column width. wParam=col, lParam=width.
Const SPRM_GETROWHEIGHT		  	= WM_USER+128		'Get row height. wParam=row, lParam=0. Returns row height.
Const SPRM_SETROWHEIGHT		  	= WM_USER+129		'Set row height. wParam=row, lParam=height.
Const SPRM_GETCELLDATA			= WM_USER+130		'Get cell data. wParam=0, lParam=Pointer to SPR_ITEM struct
Const SPRM_SETCELLDATA			= WM_USER+131		'Set cell data. wParam=0, lParam=Pointer to SPR_ITEM struct
Const SPRM_GETMULTISEL			= WM_USER+132		'Get multiselection. wParam=0, lParam=pointer to a RECT struct. Returns handle of active split window
Const SPRM_SETMULTISEL			= WM_USER+133		'Set multiselection. wParam=0, lParam=pointer to a RECT struct. Returns handle of active split window
Const SPRM_GETFONT				= WM_USER+134		'Get font. wParam=index(0-15), lParam=pointer to FONT struct. Returns font handle
Const SPRM_SETFONT				= WM_USER+135		'Set font. wParam=index(0-15), lParam=pointer to FONT struct. Returns font handle
Const SPRM_GETGLOBAL				= WM_USER+136		'Get global. wParam=0, lParam=pointer to GLOBAL struct.
Const SPRM_SETGLOBAL				= WM_USER+137		'Set global. wParam=0, lParam=pointer to GLOBAL struct.
Const SPRM_IMPORTLINE			= WM_USER+138		'Import a line of data. wParam=SepChar, lParam=pointer to data line.
Const SPRM_LOADFILE				= WM_USER+139		'Load a file. wParam=0, lParam=pointer to filename
Const SPRM_SAVEFILE				= WM_USER+140		'Save a file. wParam=0, lParam=pointer to filename
Const SPRM_NEWSHEET			  	= WM_USER+141		'Clears the sheet. wParam=0, lParam=0
Const SPRM_EXPANDCELL			= WM_USER+142		'Expand a cell to cover more than one cell. wParam=0, lParam=pointer to RECT struct
Const SPRM_GETCELLTYPE			= WM_USER+143		'Get cell data type. wParam=col, lParam=row. Returns cell type.
Const SPRM_ADJUSTCELLREF		= WM_USER+144		'Adjust cell refs in formula. wParam=pointer to cell, lParam=pointer to RECT.
Const SPRM_CREATECOMBO			= WM_USER+145		'Creates a ComboBox. wPatam=0, lParam=0
Const SPRM_SCROLLCELL			= WM_USER+146		'Scrolls current cell into view
Const SPRM_DELETECELL			= WM_USER+147		'Deletes a cell. wParam=col, lParam=row
Const SPRM_GETDATEFORMAT		= WM_USER+148		'Returns date format string. wParam=0, lParam=0
Const SPRM_SETDATEFORMAT		= WM_USER+149		'Sets date format string. wParam=0, lParam=lpDateFormat (yyyy-MM-dd)

'Styles
Const SPS_VSCROLL			  		= &h0001				'Vertical scrollbar
Const SPS_HSCROLL			  		= &h0002				'Horizontal scrollbar
Const SPS_STATUS			  		= &h0004				'Show status window
Const SPS_GRIDLINES			  	= &h0008				'Show grid lines
Const SPS_ROWSELECT			  	= &h0010				'Selection by row
Const SPS_CELLEDIT			  	= &h0020				'Cell editing
Const SPS_GRIDMODE			  	= &h0040				'Inserting and deleting row/col adjusts max row/col
Const SPS_COLSIZE			  		= &h0080				'Allow col widt sizeing by mouse
Const SPS_ROWSIZE		      	= &h0100				'Allow row height sizeing by mouse
Const SPS_WINSIZE			  		= &h0200				'Allow splitt window sizeing by mouse
Const SPS_MULTISELECT		  	= &h0400				'Allow multiselect

'Cell data types
Const TPE_EMPTY					= &h000				'The cell contains formatting only
Const TPE_COLHDR					= &h001				'Column header
Const TPE_ROWHDR					= &h002				'Row header
Const TPE_WINHDR					= &h003				'Window (splitt) header
Const TPE_TEXT						= &h004				'Text cell
Const TPE_TEXTMULTILINE			= &h005				'Text cell, text is multiline
Const TPE_INTEGER					= &h006				'Double word integer
Const TPE_FLOAT					= &h007				'80 bit float
Const TPE_FORMULA					= &h008				'Formula
Const TPE_GRAP						= &h009				'Graph
Const TPE_HYPERLINK				= &h00A				'Hyperlink
Const TPE_CHECKBOX				= &h00B				'Checkbox
Const TPE_COMBOBOX				= &h00C				'Combobox
Const TPE_OWNERDRAWBLOB			= &h00D				'Owner drawn blob, first word is lenght of blob
Const TPE_OWNERDRAWINTEGER		= &h00E				'Owner drawn integer

Const TPE_EXPANDED				= &h00F				'Part of expanded cell, internally used

Const TPE_BUTTON					= &h010				'Small button
Const TPE_WIDEBUTTON				= &h020				'Button, covers the cell
Const TPE_DATE						= &h030				'Combine with type integer
Const TPE_FORCETYPE				= &h040				'Forced type
Const TPE_FIXEDSIZE				= &h080				'Fixed size for CheckBox, ComboBox and Button image

'Format Alignment & Decimals
Const FMTA_AUTO					= &h000				'Text left middle, numbers right middle
Const FMTA_LEFT					= &h010
Const FMTA_CENTER					= &h020
Const FMTA_RIGHT					= &h030
Const FMTA_TOP						= &h000
Const FMTA_MIDDLE					= &h040
Const FMTA_BOTTOM					= &h080
Const FMTA_GLOBAL					= &h0F0
Const FMTA_MASK					= &h0F0				'Alignment mask
Const FMTA_XMASK					= &h030				'Alignment x-mask
Const FMTA_YMASK					= &h0C0				'Alignment y-mask

Const FMTD_0						= &h00
Const FMTD_1						= &h01
Const FMTD_2						= &h02
Const FMTD_3						= &h03
Const FMTD_4						= &h04
Const FMTD_5						= &h05
Const FMTD_6						= &h06
Const FMTD_7						= &h07
Const FMTD_8						= &h08
Const FMTD_9						= &h09
Const FMTD_10						= &h0A
Const FMTD_11						= &h0B
Const FMTD_12						= &h0C
Const FMTD_ALL						= &h0D
Const FMTD_SCI						= &h0E
Const FMTD_GLOBAL					= &h0F
Const FMTD_MASK					= &h0F

Type FORMAT
	bckcol			As Integer							'Back color
	txtcol			As Integer							'Text color
	txtal				As Byte								'Text alignment and decimals
	imgal				As Byte								'Image alignment and imagelist/control index
	fnt				As Byte								'Font index (0-15)
	tpe				As Byte								'Cell type
End Type

Type GLOBAL
	colhdrbtn		As Integer
	rowhdrbtn		As Integer
	winhdrbtn		As Integer
	lockcol			As Integer							'Back color of locked cell
	hdrgrdcol		As Integer							'Header grid color
	grdcol			As Integer							'Cell grid color
	bcknfcol			As Integer							'Back color of active cell, lost focus
	txtnfcol			As Integer							'Text color of active cell, lost focus
	bckfocol			As Integer							'Back color of active cell, has focus
	txtfocol			As Integer							'Text color of active cell, has focus
	ncols				As Integer
	nrows				As Integer
	ghdrwt			As Integer
	ghdrht			As Integer
	gcellwt			As Integer
	gcellht			As Integer
	colhdr			As FORMAT							'Column header formatting
	rowhdr			As FORMAT 							'Row header formatting
	winhdr			As FORMAT							'Window header formatting
	cell				As FORMAT							'Cell formatting
End Type

Type FONT
	hfont				As Integer							'Font handle
	face				As ZString*LF_FACESIZE			'Face name
	fsize				As Integer							'Point size
	ht					As Integer							'Height
	bold				As Byte								'Bold
	italic			As Byte								'Italics
	underline		As Byte								'Underline
	strikeout		As Byte								'Strikeout
End Type

Const STATE_LOCKED				= &h001				'Cell is locked for editing
Const STATE_HIDDEN				= &h002				'Cell content is not displayed
Const STATE_REDRAW				= &h008
Const STATE_ERROR					= &h010
Const STATE_DIV0					= &h020
Const STATE_UNDERFLOW			= &h030
Const STATE_OVERFLOW				= &h040
Const STATE_RECALC				= &h080
Const STATE_ERRMASK				= &h0F0

Const SPRIF_BACKCOLOR			= &h00000001		'Back color is valid
Const SPRIF_TEXTCOLOR			= &h00000002		'Text color is valid
Const SPRIF_TEXTALIGN			= &h00000004
Const SPRIF_IMAGEALIGN			= &h00000008
Const SPRIF_FONT					= &h00000010
Const SPRIF_STATE					= &h00000020
Const SPRIF_TYPE					= &h00000040
Const SPRIF_WIDTH					= &h00000080
Const SPRIF_HEIGHT				= &h00000100
Const SPRIF_DATA					= &h00000200
Const SPRIF_DOUBLE				= &h00000400		'Converts to / from double
Const SPRIF_SINGLE				= &h00000800		'Converts to / from single
Const SPRIF_COMPILE				= &h80000000		'Compile the formula

Type SPR_ITEM
	flag				As Integer
	col				As Integer
	row				As Integer
	expx				As Byte								'Expanded columns
	expy				As Byte								'Expanded rows
	state				As Byte
	fmt				As FORMAT
	wt					As Integer
	ht					As Integer
	lpdta				As Any ptr
End Type

'Notification messages (WM_NOTIFY)
Const SPRN_SELCHANGE				= 1					'Splitt, col or row changed.
Const SPRN_BEFOREEDIT			= 2					'Before the editbox is shown
Const SPRN_AFTEREDIT				= 3					'After the editbox is closed
Const SPRN_BEFOREUPDATE			= 4					'Before cell is updated
Const SPRN_AFTERUPDATE			= 5					'After cell is updated
Const SPRN_HYPERLINKENTER		= 6					'Hyperlink entered
Const SPRN_HYPERLINKLEAVE		= 7					'Hyperlink leaved
Const SPRN_HYPERLINKCLICK		= 8					'Hyperlink clicked
Const SPRN_BUTTONCLICK			= 9					'Button clicked

'on structs
Type SPR_SELCHANGE
	hdr				As NMHDR
	nwin				As Integer
	col				As Integer
	row				As Integer
	fcancel			As Integer
End Type

Type SPR_EDIT
	hdr				As NMHDR
	lpspri			As SPR_ITEM ptr
	fcancel			As Integer
End Type

Type SPR_HYPERLINK
	hdr				As NMHDR
	lpspri			As SPR_ITEM ptr
End Type

Type SPR_BUTTON
	hdr				As NMHDR
	lpspri			As SPR_ITEM ptr
End Type
