*&---------------------------------------------------------------------*
*& Include          ZSDB0010F02
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
  GS_FCAT-COLTEXT    = TEXT-F01."'STATUS'.
  GS_FCAT-JUST       = 'C'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MESSAGE'.
  GS_FCAT-COLTEXT    = TEXT-F19."'Message'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-REF_TABLE  = 'EAM_WF_PROC_MSG'.
  GS_FCAT-REF_FIELD  = 'MESSAGE'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'PRICE'.
  GS_FCAT-COLTEXT    = 'Price'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'PARTNER'.
  GS_FCAT-COLTEXT    = 'Partner'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'QTY'.
  GS_FCAT-COLTEXT    = 'Qty.'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VBELN '.
  GS_FCAT-COLTEXT    = TEXT-F02."'Sales Order'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-EMPHASIZE =  'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'POSNR'.
  GS_FCAT-COLTEXT    = TEXT-F03."'Item'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-EMPHASIZE =  'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VTWEG'.
  GS_FCAT-COLTEXT    = TEXT-F06."'Distribution Chan.'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'SPART'.
  GS_FCAT-COLTEXT    = TEXT-F07."'Division'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'WERKS'.
  GS_FCAT-COLTEXT    = TEXT-F08."'Plant'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR'.
  GS_FCAT-COLTEXT    = TEXT-F09."'Sold-to-Party'.
  GS_FCAT-NO_ZERO    = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F20."'Sold-to text'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR2'.
  GS_FCAT-COLTEXT    = TEXT-F10."'Ship-to-Party'.
  GS_FCAT-NO_ZERO    = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR2_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F21."'Ship-to text'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR3'.
  GS_FCAT-COLTEXT    = TEXT-F25."'Godds supplier'.
  GS_FCAT-NO_ZERO    = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR3_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F26."'SGodds supplier text'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'BSTKD'.
  GS_FCAT-COLTEXT    = TEXT-F11."'Customer Ref.'.
  GS_FCAT-EMPHASIZE  = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'BSTDK'.
  GS_FCAT-COLTEXT    = TEXT-F12."'Req. Delivery date'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'PRSDT'.
  GS_FCAT-COLTEXT    = TEXT-F13."'Pricing Date'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ETD'.
  GS_FCAT-COLTEXT    = 'ETD'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ETA'.
  GS_FCAT-COLTEXT    = 'ETA'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F15."'Material (SKU)'.
  GS_FCAT-REF_TABLE = 'MARA'.
  GS_FCAT-REF_FIELD = 'MATNR'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F22."'Material (SKU)'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KWMENG'.
  GS_FCAT-COLTEXT    = TEXT-F16."'Order QTY.'.
  GS_FCAT-QFIELDNAME = 'MEINS'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'WERKS'.
  GS_FCAT-COLTEXT    = TEXT-F18.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'WERKS_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F24.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LGORT'.
  GS_FCAT-COLTEXT    = TEXT-F17."'Storage Location'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LGORT_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F23."'Sloc text'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR'.
  GS_FCAT-COLTEXT    = TEXT-F29."'price'.
  GS_FCAT-CURRENCY   = GV_WAERS.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR_CURRENT'.
  GS_FCAT-COLTEXT    = TEXT-F38."'SAP Sales price'.
  GS_FCAT-CURRENCY   = GV_WAERS.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  IF P_OPENPO = ' '.
    GS_FCAT-FIELDNAME  = 'KBETR_SD'.
    GS_FCAT-COLTEXT    = TEXT-F30."'SAP Sales price'.
    GS_FCAT-CURRENCY   = GV_WAERS.
    APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.
  ENDIF.

  GS_FCAT-FIELDNAME  = 'POSEX'.
  GS_FCAT-COLTEXT    = TEXT-F28. "'Cust ref Item
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZASIN'.
  GS_FCAT-COLTEXT    =  'ASIN'. "'Cust ref Item
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
