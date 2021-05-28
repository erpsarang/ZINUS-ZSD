*&---------------------------------------------------------------------*
*& Include          ZSDR0130F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_FRAME
*&---------------------------------------------------------------------*
FORM SET_FRAME.

  CREATE OBJECT GO_CUSTOM
    EXPORTING
      CONTAINER_NAME = 'GO_CON'.

  CREATE OBJECT GO_DOCUMENT
    EXPORTING
      STYLE = 'TOP_OF_PAGE'.

  CREATE OBJECT GO_SPLITTER
    EXPORTING
      PARENT  = GO_CUSTOM
      ROWS    = 2
      COLUMNS = 1.

  CALL METHOD GO_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = G_PARENT_HTML.

  CALL METHOD GO_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 2
      COLUMN    = 1
    RECEIVING
      CONTAINER = GO_CON.

  CALL METHOD GO_SPLITTER->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 15.

  CREATE OBJECT GO_GRID
    EXPORTING
      I_PARENT = GO_CON.


ENDFORM. " SET_FRAME
*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT
*&---------------------------------------------------------------------*
FORM SET_VARIANT.
  CLEAR: GS_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-USERNAME = SY-UNAME.
ENDFORM. " SET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
FORM SET_SORT.
  CLEAR: GS_SORT, GT_SORT[].

ENDFORM. " SET_SORT
*&---------------------------------------------------------------------*
*&      Form  SET_TOOLBAR_EXCLUDE
*&---------------------------------------------------------------------*
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
FORM SET_FIELDCAT.

  CLEAR: GT_FCAT[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'DOCNUM'.
  GS_FCAT-COLTEXT    = TEXT-F01."'Idoc NO'.
  GS_FCAT-REF_FIELD  = 'DOCNUM'.
  GS_FCAT-REF_TABLE  = 'EDIDC'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ICON'.
  GS_FCAT-COLTEXT    = TEXT-F02."'STATUS'.
  GS_FCAT-JUST       = 'C'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'CREDAT'.
  GS_FCAT-REF_FIELD = 'CREDAT'.
  GS_FCAT-REF_TABLE = 'EDIDC'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'MESTYP'.
  GS_FCAT-REF_FIELD = 'MESTYP'.
  GS_FCAT-REF_TABLE = 'EDIDC'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'MESSAGE'.
  GS_FCAT-COLTEXT   = TEXT-F03.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'EBELN'.
  GS_FCAT-REF_FIELD = 'EBELN'.
  GS_FCAT-REF_TABLE = 'EKKO'.
  GS_FCAT-EMPHASIZE = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'BEDAT'.
  GS_FCAT-REF_FIELD = 'BEDAT'.
  GS_FCAT-REF_TABLE = 'EKKO'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'VBELN'.
  GS_FCAT-REF_FIELD = 'VBELN'.
  GS_FCAT-REF_TABLE = 'VBAK'.
  GS_FCAT-EMPHASIZE = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'OPEN_PO'.
  GS_FCAT-COLTEXT   = 'OPEN_PO'.
  GS_FCAT-CHECKBOX  = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'LIFNR'.
  GS_FCAT-COLTEXT    = TEXT-F04.
  GS_FCAT-REF_FIELD = 'LIFNR'.
  GS_FCAT-REF_TABLE = 'VBPA'.
  GS_FCAT-NO_ZERO   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME = 'LIFNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F05.
  GS_FCAT-REF_FIELD = 'NAME_ORG1'.
  GS_FCAT-REF_TABLE = 'BUT000'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.


ENDFORM. " SET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_DISPLAY
*&---------------------------------------------------------------------*
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

  CALL METHOD GO_DOCUMENT->INITIALIZE_DOCUMENT
    EXPORTING
      BACKGROUND_COLOR = CL_DD_AREA=>COL_TEXTAREA.


  CALL METHOD GO_GRID->LIST_PROCESSING_EVENTS
    EXPORTING
      I_EVENT_NAME = 'TOP_OF_PAGE'
      I_DYNDOC_ID  = GO_DOCUMENT.




ENDFORM. " SET_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  REFRESH_LIST
*&---------------------------------------------------------------------*
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
