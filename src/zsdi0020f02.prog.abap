*&---------------------------------------------------------------------*
*& Include          ZSDI0020F02
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
  GS_LAYOUT-CWIDTH_OPT   = 'X'.
  GS_LAYOUT-ZEBRA      = 'X'.
  GS_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
  GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

*  PERFORM CELL_STYLE.

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

  CALL METHOD PS_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.    "변경시

  CALL METHOD PS_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.       "엔터

ENDFORM. " SET_EVENT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCAT
*&---------------------------------------------------------------------*
FORM SET_FIELDCAT.

  CLEAR: GT_FCAT[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ICON'.
  GS_FCAT-COLTEXT    = TEXT-F11. "'STATUS'.
  GS_FCAT-JUST       = 'C'.
  GS_FCAT-OUTPUTLEN  = '4'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MESSAGE'.
  GS_FCAT-COLTEXT    = TEXT-F12. "'Message'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR'.
  GS_FCAT-COLTEXT    = TEXT-f01.
  GS_FCAT-REF_TABLE  = 'LFA1'.
  GS_FCAT-REF_FIELD  = 'LIFNR'.
  GS_FCAT-NO_ZERO    = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZBUKRS_JDE'.
  GS_FCAT-REF_TABLE  = 'ZSDT0040'.
  GS_FCAT-REF_FIELD  = 'ZBUKRS_JDE'.
  GS_FCAT-COLTEXT    = TEXT-f02.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR'.
  GS_FCAT-COLTEXT    = TEXT-f03.
  GS_FCAT-REF_TABLE  = 'KNA1'.
  GS_FCAT-REF_FIELD  = 'KUNNR'.
  GS_FCAT-NO_ZERO    = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR_JDE'.
  GS_FCAT-COLTEXT    = TEXT-f04.
  GS_FCAT-NO_ZERO    = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-f05.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-f06.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'START_DATE'.
  GS_FCAT-COLTEXT    = TEXT-f07.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'END_DATE'.
  GS_FCAT-COLTEXT    = TEXT-f08.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR'.
  GS_FCAT-COLTEXT    = TEXT-f09.
  GS_FCAT-CFIELDNAME = 'KONWA'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KONWA'.
  GS_FCAT-COLTEXT    = TEXT-f10.
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

  PERFORM SET_ON_F4.

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
*&---------------------------------------------------------------------*
*& Form HANDLE_ON_F4
*&---------------------------------------------------------------------*
FORM HANDLE_ON_F4 USING       P_FIELDNAME   TYPE LVC_FNAME
                              P_FIELDVALUE  TYPE LVC_VALUE
                              P_ROW_ID      TYPE LVC_S_ROID
                              PR_EVENT_DATA TYPE REF TO  CL_ALV_EVENT_DATA
                              PT_BAD_CELLS  TYPE LVC_T_MODI
                              P_E_DISPLAY   TYPE CHAR01.

  FIELD-SYMBOLS: <L_ITAB> TYPE LVC_T_MODI.
  DATA : LS_MODI TYPE LVC_S_MODI.
  DATA : LV_SELECTFIELD  LIKE  HELP_INFO-FIELDNAME,
         LT_FIELDS       LIKE  HELP_VALUE OCCURS 0 WITH HEADER LINE,
         LV_SELECT_VALUE LIKE  HELP_INFO-FLDVALUE,
         LD_TABIX        LIKE  SY-TABIX.

  CASE P_FIELDNAME.
    WHEN 'ZBUKRS'.

*      CLEAR: LV_SELECTFIELD, LT_FIELDS, LV_SELECT_VALUE, LD_TABIX.
*      REFRESH: LT_FIELDS.
*
*      LT_FIELDS-TABNAME = 'ZSDT0040'.
*      LT_FIELDS-FIELDNAME = 'ZBUKRS'.
*      LT_FIELDS-SELECTFLAG = 'X'.
*      APPEND LT_FIELDS.
*      LT_FIELDS-TABNAME = 'ZSDT0040'.
*      LT_FIELDS-FIELDNAME = 'ZBUKRS_TXT'.
*      LT_FIELDS-SELECTFLAG = ' '.
*      APPEND LT_FIELDS.
*
*      CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
*        EXPORTING
*          SELECTFIELD                  = LV_SELECTFIELD
*        IMPORTING
*          IND                          = LD_TABIX
*          SELECT_VALUE                 = LV_SELECT_VALUE
*        TABLES
*          FIELDS                       = LT_FIELDS
*          FULL_TABLE                   = GT_0040
*        EXCEPTIONS
*          FULL_TABLE_EMPTY             = 1
*          NO_TABLESTRUCTURE_GIVEN      = 2
*          NO_TABLEFIELDS_IN_DICTIONARY = 3
*          MORE_THEN_ONE_SELECTFIELD    = 4
*          NO_SELECTFIELD               = 5
*          OTHERS                       = 6.
*      CHECK NOT LD_TABIX IS INITIAL.
*      READ TABLE GT_0040 INDEX LD_TABIX.
*
*      ASSIGN PR_EVENT_DATA->M_DATA->* TO <L_ITAB>.
*      CLEAR GT_LIST.
*      READ TABLE GT_LIST INDEX P_ROW_ID-ROW_ID.
*      GT_LIST-ZBUKRS = LV_SELECT_VALUE.
*      MODIFY GT_LIST INDEX P_ROW_ID-ROW_ID.
*
*      LS_MODI-ROW_ID   = P_ROW_ID-ROW_ID.
*      LS_MODI-FIELDNAME = 'ZBUKRS'.
*      LS_MODI-VALUE     = LV_SELECT_VALUE.
*      APPEND LS_MODI TO <L_ITAB>.
*
*      PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.


  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ON_F4
*&---------------------------------------------------------------------*
FORM SET_ON_F4 .

  DATA: LS_F4 TYPE LVC_S_F4,
        LT_F4 TYPE LVC_T_F4.

*  CLEAR LS_F4.
*  LS_F4-FIELDNAME  = 'KUNNR'.
*  LS_F4-REGISTER   = 'X'.
*  APPEND LS_F4 TO LT_F4.

  CALL METHOD GO_GRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = LT_F4.

ENDFORM.
