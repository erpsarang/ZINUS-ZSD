*&---------------------------------------------------------------------*
*& Include          ZSDR0200F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_FRAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_FRAME.

  CREATE OBJECT GO_CUSTOM
    EXPORTING
      CONTAINER_NAME = 'GO_CON'.

  CREATE OBJECT GO_GRID
    EXPORTING
      I_PARENT = GO_CUSTOM.


ENDFORM. " SET_FRAME

*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_VARIANT.
  CLEAR: GS_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-USERNAME = SY-UNAME.
ENDFORM. " SET_VARIANT

*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_SORT.
  CLEAR: GS_SORT, GT_SORT[].

ENDFORM. " SET_SORT

*&---------------------------------------------------------------------*
*&      Form  SET_TOOLBAR_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_TOOLBAR_EXCLUDE.
  CLEAR: GT_EXCLUDE[].
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO GT_EXCLUDE. "Undo
  APPEND CL_GUI_ALV_GRID=>MC_FC_HELP              TO GT_EXCLUDE. "Help
  APPEND CL_GUI_ALV_GRID=>MC_FC_INFO              TO GT_EXCLUDE. "Info
  APPEND CL_GUI_ALV_GRID=>MC_FC_GRAPH             TO GT_EXCLUDE. "그래픽
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO GT_EXCLUDE. "Local: Copy
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO GT_EXCLUDE. "Local: Copy Row
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO GT_EXCLUDE. "Local: Cut
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO GT_EXCLUDE. "Local: InsertRow
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW      TO GT_EXCLUDE.
*  IF P_DIS EQ 'X'.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO GT_EXCLUDE. "Local: DeleteRow
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO GT_EXCLUDE. "Local: AppendRow
*  ENDIF.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO GT_EXCLUDE. "Local: Paste
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO GT_EXCLUDE. "Refresh
  APPEND CL_GUI_ALV_GRID=>MC_FC_WORD_PROCESSOR    TO GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_HTML              TO GT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL          TO GT_EXCLUDE. "Exclude
ENDFORM. " SET_TOOLBAR_EXCLUDE

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_LAYOUT.

  CLEAR GS_LAYOUT.
  GS_LAYOUT-SEL_MODE   = 'A'.
  GS_LAYOUT-ZEBRA      = 'X'.
  GS_LAYOUT-CWIDTH_OPT = 'X'.
  GS_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
  GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

ENDFORM. " SET_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  SET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_EVENT USING PS_GRID TYPE REF TO LCL_ALV_GRID.

  SET HANDLER
    PS_GRID->HANDLE_DATA_CHANGED   "ALV 변경사항 관리
    PS_GRID->HANDLE_DATA_CHANGED_FINISHED   "ALV 변경사항 관리
    PS_GRID->HANDLE_HOTSPOT_CLICK  "HOTSPOT click event
    PS_GRID->HANDLE_DOUBLE_CLICK   "DOUBLE click event
    PS_GRID->HANDLE_TOP_OF_PAGE
    PS_GRID->ON_F4
    PS_GRID->HANDLE_TOOLBAR
    PS_GRID->HANDLE_USER_COMMAND
  FOR PS_GRID.

  CALL METHOD GO_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.    "변경시

  CALL METHOD GO_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.       "엔터

ENDFORM. " SET_EVENT

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_FIELDCAT.

  CLEAR: GT_FCAT[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ICON'.
  GS_FCAT-COLTEXT    = TEXT-F11.
  GS_FCAT-JUST       = 'C'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MESSAGE'.
  GS_FCAT-COLTEXT    = TEXT-F12.
  GS_FCAT-REF_TABLE  = 'EAM_WF_PROC_MSG'.
  GS_FCAT-REF_FIELD  = 'MESSAGE'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'EBELN'.
  GS_FCAT-REF_TABLE  = 'EKPO'.
  GS_FCAT-REF_FIELD  = 'EBELN'.
  GS_FCAT-COLTEXT    = TEXT-F01.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'EBELP'.
  GS_FCAT-REF_TABLE  = 'EKPO'.
  GS_FCAT-REF_FIELD  = 'EBELP'.
  GS_FCAT-COLTEXT    = TEXT-F02.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VBELN'.
  GS_FCAT-REF_TABLE  = 'VBAP'.
  GS_FCAT-REF_FIELD  = 'VBELN'.
  GS_FCAT-COLTEXT    = TEXT-F03.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'POSNR'.
  GS_FCAT-REF_TABLE  = 'VBAP'.
  GS_FCAT-REF_FIELD  = 'POSNR'.
  GS_FCAT-COLTEXT    = TEXT-F04.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-REF_TABLE  = 'VBAP'.
  GS_FCAT-REF_FIELD  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F05.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR_TXT'.
  GS_FCAT-REF_TABLE  = 'MAKT'.
  GS_FCAT-REF_FIELD  = 'MAKTX'.
  GS_FCAT-COLTEXT    = TEXT-F06.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR_C'.
  GS_FCAT-CURRENCY   = '2'.
  GS_FCAT-EDIT       = 'X'.
  GS_FCAT-COLTEXT    = TEXT-F07.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR_HQ'.
  GS_FCAT-CFIELDNAME = 'KONWA'.
  GS_FCAT-EMPHASIZE  = 'C200'.
  GS_FCAT-COLTEXT    = TEXT-F08.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR_US'.
  GS_FCAT-CFIELDNAME = 'KONWA'.
  GS_FCAT-EMPHASIZE  = 'C400'.
  GS_FCAT-COLTEXT    = TEXT-F09.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KONWA'.
  GS_FCAT-REF_TABLE  = 'KONP'.
  GS_FCAT-REF_FIELD  = 'KONWA'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.


ENDFORM. " SET_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  SET_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_DISPLAY.
  FIELD-SYMBOLS: <GT_TAB> TYPE TABLE.
  UNASSIGN: <GT_TAB>.

  ASSIGN: GT_LIST[] TO <GT_TAB>.

  CALL METHOD GO_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

  CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_VARIANT
      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
      I_SAVE               = 'A'
    CHANGING
      IT_OUTTAB            = <GT_TAB>[]
      IT_FIELDCATALOG      = GT_FCAT
      IT_SORT              = GT_SORT.


ENDFORM. " SET_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  REFRESH_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REFRESH_LIST.

  CALL METHOD GO_GRID->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = GS_LAYOUT.

  GS_STBL-ROW = 'X'.
  GS_STBL-COL = 'X'.

  CALL METHOD GO_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = GS_STBL.

ENDFORM. " REFRESH_LIST
