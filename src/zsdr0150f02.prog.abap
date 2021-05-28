*&---------------------------------------------------------------------*
*& Include          ZSDR0150F02
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
      HEIGHT = 10.

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

  GS_FCAT-FIELDNAME  = 'ICON'.
  GS_FCAT-COLTEXT    = 'STATUS' .
  GS_FCAT-JUST       = 'C'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MESSAGE'.
  GS_FCAT-COLTEXT    = 'MESSAGE' .
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'SEND'.
  GS_FCAT-COLTEXT    = 'SEND' .
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZUPDATE'.
  GS_FCAT-COLTEXT    = 'UPDATE' .
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VBELN_VF'.
  GS_FCAT-COLTEXT    = TEXT-F01."BILLING
  GS_FCAT-REF_FIELD  = 'VBELN'.
  GS_FCAT-REF_TABLE  = 'VBRK'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'POSNR_VF'.
  GS_FCAT-COLTEXT    = TEXT-F02."'Billing item'.
  GS_FCAT-REF_FIELD  = 'POSNR'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VBELN_VL'.
  GS_FCAT-COLTEXT    = TEXT-F03."Delivery
  GS_FCAT-REF_FIELD  = 'VBELN'.
  GS_FCAT-REF_TABLE  = 'LIPS'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'POSNR_VL'.
  GS_FCAT-COLTEXT    = TEXT-F04."Delivery item
  GS_FCAT-REF_FIELD  = 'POSNR'.
  GS_FCAT-REF_TABLE  = 'LIPS'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VBELN'.
  GS_FCAT-COLTEXT    = TEXT-F05."SO
  GS_FCAT-REF_FIELD  = 'VBELN'.
  GS_FCAT-REF_TABLE  = 'VBAP'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'POSNR'.
  GS_FCAT-COLTEXT    = TEXT-F06."SO ITEM
  GS_FCAT-REF_FIELD  = 'POSNR'.
  GS_FCAT-REF_TABLE  = 'VBAP'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'BSTKD'.
  GS_FCAT-COLTEXT    = TEXT-F19."Cust Ref.
  GS_FCAT-REF_FIELD  = 'BSTKD'.
  GS_FCAT-REF_TABLE  = 'VBKD'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZZKOINV'.
  GS_FCAT-COLTEXT    = TEXT-F28."Invoice
  GS_FCAT-REF_FIELD  = 'XBLNR'.
  GS_FCAT-REF_TABLE  = 'VBRK'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'FKDAT'. "Billing date
  GS_FCAT-REF_FIELD  = 'FKDAT'.
  GS_FCAT-REF_TABLE  = 'VBRK'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'AUART'.
  GS_FCAT-REF_FIELD  = 'AUART'.
  GS_FCAT-REF_TABLE  = 'VBAK'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  GS_FCAT-JUST       = 'C'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VBELN2'.
  GS_FCAT-COLTEXT    = TEXT-F07."CR DR SO
  GS_FCAT-REF_FIELD  = 'VBELN'.
  GS_FCAT-REF_TABLE  = 'VBAP'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'POSNR2'.
  GS_FCAT-COLTEXT    = TEXT-F08."CR DR SO ITEM
  GS_FCAT-REF_FIELD  = 'POSNR'.
  GS_FCAT-REF_TABLE  = 'VBAP'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VBELN_VF2'.
  GS_FCAT-COLTEXT    = TEXT-F09."CR DR BILLING
  GS_FCAT-REF_FIELD  = 'VBELN'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'POSNR_VF2'.
  GS_FCAT-COLTEXT    = TEXT-F10."CR DR BILLING ITEM
  GS_FCAT-REF_FIELD  = 'VBELN'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNAG'.
  GS_FCAT-COLTEXT    = TEXT-F11."Sold-to party
  GS_FCAT-REF_FIELD  = 'KUNAG'.
  GS_FCAT-REF_TABLE  = 'VBRK'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNAG_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F18."Sold-to party NAME
  GS_FCAT-REF_FIELD  = 'NAME_ORG1'.
  GS_FCAT-REF_TABLE  = 'BUT000'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNWE'.
  GS_FCAT-COLTEXT    = TEXT-F26."Ship-to party
  GS_FCAT-REF_FIELD  = 'KUNAG'.
  GS_FCAT-REF_TABLE  = 'VBRK'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNWE_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F27."Ship-to party NAME
  GS_FCAT-REF_FIELD  = 'NAME_ORG1'.
  GS_FCAT-REF_TABLE  = 'BUT000'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR'.
  GS_FCAT-COLTEXT    = TEXT-F22."Good Supplier
  GS_FCAT-REF_FIELD  = 'LIFNR'.
  GS_FCAT-REF_TABLE  = 'VBPA'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F23."Good Supplier Name
  GS_FCAT-REF_FIELD  = 'NAME_ORG1'.
  GS_FCAT-REF_TABLE  = 'BUT000'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'BWTAR'.
  GS_FCAT-COLTEXT    = TEXT-F24."Val. Type
  GS_FCAT-REF_FIELD  = 'BWTAR'.
  GS_FCAT-REF_TABLE  = 'LIPS'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F12."SKU
  GS_FCAT-REF_FIELD  = 'MATNR'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ARKTX'.
  GS_FCAT-COLTEXT    = TEXT-F13."SKU Description
  GS_FCAT-REF_FIELD  = 'ARKTX'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME   = 'FKIMG'.
  GS_FCAT-COLTEXT     = TEXT-F14."qty
  GS_FCAT-QFIELDNAME  = 'VRKME'.
  GS_FCAT-REF_FIELD   = 'FKIMG'.
  GS_FCAT-REF_TABLE   = 'VBRP'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VRKME'.
  GS_FCAT-COLTEXT    = TEXT-F15."unit
  GS_FCAT-REF_FIELD  = 'VRKME'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'CMPRE'.
  GS_FCAT-COLTEXT    = TEXT-F21."BILLING UNIT PRICE
  GS_FCAT-CFIELDNAME  = 'WAERK'.
  GS_FCAT-REF_FIELD  = 'CMPRE'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-EMPHASIZE  = 'C700'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'NETWR1'.
  GS_FCAT-COLTEXT    = TEXT-F16."billing amount
  GS_FCAT-CFIELDNAME  = 'WAERK'.
  GS_FCAT-REF_FIELD  = 'NETWR'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-EMPHASIZE  = 'C700'.
  GS_FCAT-DO_SUM     = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR_AD'.
  GS_FCAT-COLTEXT    = TEXT-F25."adjust unit price
  GS_FCAT-CFIELDNAME  = 'WAERK'.
  GS_FCAT-REF_FIELD  = 'CMPRE'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-EMPHASIZE  = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'NETWR_AD'.
  GS_FCAT-COLTEXT    = TEXT-F30."adjust amount
  GS_FCAT-CFIELDNAME  = 'WAERK'.
  GS_FCAT-REF_FIELD  = 'CMPRE'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-EMPHASIZE  = 'X'.
  GS_FCAT-DO_SUM     = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR2'.
  GS_FCAT-COLTEXT    = TEXT-F29."Special unit price
  GS_FCAT-CFIELDNAME  = 'WAERK'.
  GS_FCAT-REF_FIELD  = 'CMPRE'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'NETWR2'.
  GS_FCAT-COLTEXT    = TEXT-F17."Special amount
  GS_FCAT-CFIELDNAME  = 'WAERK'.
  GS_FCAT-REF_FIELD  = 'NETWR'.
  GS_FCAT-REF_TABLE  = 'VBRP'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  GS_FCAT-DO_SUM     = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'WAERK'.
  GS_FCAT-COLTEXT    = TEXT-F20. "Currency
  GS_FCAT-REF_FIELD  = 'WAERK'.
  GS_FCAT-REF_TABLE  = 'VBRK'.
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