*&---------------------------------------------------------------------*
*& Include          ZSDR0011F02
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
  GS_LAYOUT-STYLEFNAME = 'CELLSTYLE'.
  GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

  PERFORM CELL_STYLE.

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
*       text
*----------------------------------------------------------------------*
FORM SET_FIELDCAT.

  CLEAR: GT_FCAT[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ICON'.
  GS_FCAT-COLTEXT    = TEXT-F01. "'STATUS'.
  GS_FCAT-JUST       = 'C'.
  GS_FCAT-OUTPUTLEN  = '4'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MESSAGE'.
  GS_FCAT-COLTEXT    = TEXT-F02. "'Message'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKUNNR_IC'.
  GS_FCAT-COLTEXT    = TEXT-F03. "Intercompany.
  GS_FCAT-REF_TABLE  = 'ZSDT0020'.
  GS_FCAT-REF_FIELD  = 'ZKUNNR_IC'.
  GS_FCAT-NO_ZERO    = 'X'.
  GS_FCAT-F4AVAILABL = 'X'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKUNNR_IC_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F04. "Intercompany Desc.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR'.
  GS_FCAT-COLTEXT    = TEXT-F05. "Customer.
  GS_FCAT-REF_TABLE  = 'KNA1'.
  GS_FCAT-REF_FIELD  = 'KUNNR'.
  GS_FCAT-NO_ZERO    = 'X'.
  GS_FCAT-F4AVAILABL = 'X'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F06. "Customer Desc.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR'.
  GS_FCAT-COLTEXT    = TEXT-F07. "Vendor.
  GS_FCAT-REF_TABLE  = 'LFA1'.
  GS_FCAT-REF_FIELD  = 'LIFNR'.
  GS_FCAT-F4AVAILABL = 'X'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F08. "Vendor Desc.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZPRODH_GROUP'.
  GS_FCAT-COLTEXT    = TEXT-F09. "Middle Category.
  GS_FCAT-F4AVAILABL = 'X'.
  GS_FCAT-OUTPUTLEN  = '5'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZPRODH_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F10. "Middle cate Desc.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F11. "SKU.
  GS_FCAT-REF_FIELD  = 'MATNR'.
  GS_FCAT-REF_TABLE  = 'MARA'.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F12. "SKU Desc.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZMARGIN'.
  GS_FCAT-COLTEXT    = TEXT-F13. "Margin(%).
  GS_FCAT-CURRENCY   = '2'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZSTART'.
  GS_FCAT-COLTEXT    = TEXT-F14. "Start Date.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZEXCEPT'.
  GS_FCAT-COLTEXT    = TEXT-F15. "Except.
  GS_FCAT-CHECKBOX   = 'X'.
  GS_FCAT-OUTPUTLEN  = '5'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZTYPE'.
  GS_FCAT-COLTEXT    = TEXT-F16. "TYPE.
  GS_FCAT-OUTPUTLEN  = '6'.
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
*&---------------------------------------------------------------------*
*& Form DELETE_LINE
*&---------------------------------------------------------------------*
FORM DELETE_LINE .

  DATA : LV_CHECK,
         LV_ERR.

  CLEAR: GT_ROWS[], GS_ROWS.
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004  DISPLAY LIKE 'E'.
  ELSE.

    CLEAR : LV_ERR, LV_CHECK.
    PERFORM POPUP_MSG USING TEXT-P01 "
                            TEXT-P02 "
                             LV_CHECK.

    CHECK LV_CHECK EQ '1'.

    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
      IF GT_LIST-ZCONFIRM IS INITIAL.
        GT_LIST-DEL = 'X'.
        MODIFY GT_LIST  INDEX GS_ROWS-INDEX TRANSPORTING DEL.
      ELSE.
        LV_ERR = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF LV_ERR = 'X'.
      MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
    ELSE.
      DELETE GT_LIST WHERE DEL = 'X'.
      PERFORM CELL_STYLE.
      PERFORM REFRESH_LIST.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CELL_STYLE
*&---------------------------------------------------------------------*
FORM CELL_STYLE.

  LOOP AT GT_LIST.
    CHECK GT_LIST-ZCONFIRM IS INITIAL.
    CLEAR GT_STYL.
    IF GT_LIST-FLAG = 'X'. "ZSDT0020에 존재하던 데이터는 마진과 예외만 수정허용
      PERFORM :  SET_STYLE USING 'ZMARGIN'  ' ',
                 SET_STYLE USING 'ZEXCEPT'  ' '.
    ELSE.
      PERFORM : SET_STYLE USING 'ZKUNNR_IC'   ' ',
                SET_STYLE USING 'KUNNR'       ' ',
                SET_STYLE USING 'LIFNR'       ' ',
                SET_STYLE USING 'ZMARGIN'     ' ',
                SET_STYLE USING 'ZEXCEPT'     ' ',
                SET_STYLE USING 'MATNR'       ' ',
                SET_STYLE USING 'ZPRODH_GROUP'        ' '.

      IF GT_LIST-MATNR IS NOT INITIAL.
        PERFORM SET_STYLE USING 'ZPRODH_GROUP'  'DISP'.
      ENDIF.
      IF GT_LIST-ZPRODH_GROUP IS NOT INITIAL.
        PERFORM SET_STYLE USING 'MATNR' 'DISP'.
      ENDIF.
      IF GV_ZTYPE EQ 'M' OR GV_ZTYPE EQ 'I'.
        PERFORM SET_STYLE USING 'ZPRODH_GROUP'  'DISP'.
      ENDIF.
    ENDIF.
    CLEAR GT_LIST-CELLSTYLE[].
    INSERT LINES OF GT_STYL INTO TABLE GT_LIST-CELLSTYLE.
    MODIFY GT_LIST.  CLEAR GT_LIST.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_STYLE
*&---------------------------------------------------------------------*
FORM SET_STYLE  USING P_NAME
                      P_MODE.

**해당 필드에 설정된 셀은 지우고 새로 넣어줘야한다.
  CLEAR GS_STYL.
  READ TABLE GT_STYL INTO GS_STYL WITH KEY FIELDNAME = P_NAME.
  IF SY-SUBRC = 0.
    DELETE GT_STYL INDEX SY-TABIX.
  ENDIF.

  CLEAR GS_STYL.
  GS_STYL-FIELDNAME = P_NAME.
  IF P_MODE EQ 'DISP'.
    GS_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  ELSE.
    GS_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  ENDIF.
  INSERT GS_STYL INTO TABLE GT_STYL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_LINE
*&---------------------------------------------------------------------*
FORM ADD_LINE .

  CLEAR GT_LIST.
  GT_LIST-ZSTART = GV_DATE.

  PERFORM GET_DOMAIN_ZTYPE USING GV_ZTYPE
                      CHANGING GT_LIST-ZTYPE.

  APPEND GT_LIST.

  PERFORM CELL_STYLE.
  PERFORM REFRESH_LIST.

ENDFORM.
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
    WHEN 'ZKUNNR_IC'.

      CLEAR: LV_SELECTFIELD, LT_FIELDS, LV_SELECT_VALUE, LD_TABIX.
      REFRESH: LT_FIELDS.

      LT_FIELDS-TABNAME = 'ZSDT0040'.
      LT_FIELDS-FIELDNAME = 'ZKUNNR_IC'.
      LT_FIELDS-SELECTFLAG = 'X'.
      APPEND LT_FIELDS.
      LT_FIELDS-TABNAME = 'BUT000'.
      LT_FIELDS-FIELDNAME = 'NAME_ORG1'.
      LT_FIELDS-SELECTFLAG = ' '.
      APPEND LT_FIELDS.

      CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
        EXPORTING
          SELECTFIELD                  = LV_SELECTFIELD
        IMPORTING
          IND                          = LD_TABIX
          SELECT_VALUE                 = LV_SELECT_VALUE
        TABLES
          FIELDS                       = LT_FIELDS
          FULL_TABLE                   = GT_0040
        EXCEPTIONS
          FULL_TABLE_EMPTY             = 1
          NO_TABLESTRUCTURE_GIVEN      = 2
          NO_TABLEFIELDS_IN_DICTIONARY = 3
          MORE_THEN_ONE_SELECTFIELD    = 4
          NO_SELECTFIELD               = 5
          OTHERS                       = 6.
      CHECK NOT LD_TABIX IS INITIAL.
      READ TABLE GT_0040 INDEX LD_TABIX.

      ASSIGN PR_EVENT_DATA->M_DATA->* TO <L_ITAB>.
      CLEAR GT_LIST.
      READ TABLE GT_LIST INDEX P_ROW_ID-ROW_ID.
      GT_LIST-ZKUNNR_IC = LV_SELECT_VALUE.
      MODIFY GT_LIST INDEX P_ROW_ID-ROW_ID TRANSPORTING ZKUNNR_IC.

      LS_MODI-ROW_ID   = P_ROW_ID-ROW_ID.
      LS_MODI-FIELDNAME = 'ZKUNNR_IC'.
      LS_MODI-VALUE     = LV_SELECT_VALUE.
      APPEND LS_MODI TO <L_ITAB>.

      PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.

    WHEN 'ZPRODH_GROUP'.

      CLEAR: LV_SELECTFIELD, LT_FIELDS, LV_SELECT_VALUE, LD_TABIX.
      REFRESH: LT_FIELDS.

      LT_FIELDS-TABNAME = 'ZSDT0100'.
      LT_FIELDS-FIELDNAME = 'ZPRODH_GROUP'.
      LT_FIELDS-SELECTFLAG = 'X'.
      APPEND LT_FIELDS.
      LT_FIELDS-TABNAME = 'ZSDT0100'.
      LT_FIELDS-FIELDNAME = 'VTEXT'.
      LT_FIELDS-SELECTFLAG = ' '.
      APPEND LT_FIELDS.

      CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
        EXPORTING
          SELECTFIELD                  = LV_SELECTFIELD
        IMPORTING
          IND                          = LD_TABIX
          SELECT_VALUE                 = LV_SELECT_VALUE
        TABLES
          FIELDS                       = LT_FIELDS
          FULL_TABLE                   = GT_0100
        EXCEPTIONS
          FULL_TABLE_EMPTY             = 1
          NO_TABLESTRUCTURE_GIVEN      = 2
          NO_TABLEFIELDS_IN_DICTIONARY = 3
          MORE_THEN_ONE_SELECTFIELD    = 4
          NO_SELECTFIELD               = 5
          OTHERS                       = 6.
      CHECK NOT LD_TABIX IS INITIAL.
      READ TABLE GT_0100 INDEX LD_TABIX.
      ASSIGN PR_EVENT_DATA->M_DATA->* TO <L_ITAB>.
      CLEAR GT_LIST.
      READ TABLE GT_LIST INDEX P_ROW_ID-ROW_ID.
      GT_LIST-ZPRODH_GROUP = LV_SELECT_VALUE.
      MODIFY GT_LIST INDEX P_ROW_ID-ROW_ID TRANSPORTING ZPRODH_GROUP.

      LS_MODI-ROW_ID   = P_ROW_ID-ROW_ID.
      LS_MODI-FIELDNAME = 'ZPRODH_GROUP'.
      LS_MODI-VALUE     = LV_SELECT_VALUE.
      APPEND LS_MODI TO <L_ITAB>.

      PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.

    WHEN 'KUNNR'.

      CLEAR: LV_SELECTFIELD, LT_FIELDS, LV_SELECT_VALUE, LD_TABIX.
      REFRESH: LT_FIELDS.

      LT_FIELDS-TABNAME = 'KNA1'.
      LT_FIELDS-FIELDNAME = 'KUNNR'.
      LT_FIELDS-SELECTFLAG = 'X'.
      APPEND LT_FIELDS.
      LT_FIELDS-TABNAME = 'KNA1'.
      LT_FIELDS-FIELDNAME = 'NAME1'.
      LT_FIELDS-SELECTFLAG = ' '.
      APPEND LT_FIELDS.

      CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
        EXPORTING
          SELECTFIELD                  = LV_SELECTFIELD
        IMPORTING
          IND                          = LD_TABIX
          SELECT_VALUE                 = LV_SELECT_VALUE
        TABLES
          FIELDS                       = LT_FIELDS
          FULL_TABLE                   = GT_KNA1
        EXCEPTIONS
          FULL_TABLE_EMPTY             = 1
          NO_TABLESTRUCTURE_GIVEN      = 2
          NO_TABLEFIELDS_IN_DICTIONARY = 3
          MORE_THEN_ONE_SELECTFIELD    = 4
          NO_SELECTFIELD               = 5
          OTHERS                       = 6.
      CHECK NOT LD_TABIX IS INITIAL.
      READ TABLE GT_KNA1 INDEX LD_TABIX.
      ASSIGN PR_EVENT_DATA->M_DATA->* TO <L_ITAB>.
      CLEAR GT_LIST.
      READ TABLE GT_LIST INDEX P_ROW_ID-ROW_ID.
      GT_LIST-KUNNR = LV_SELECT_VALUE.
      PERFORM ALPHA_OUTPUT USING GT_LIST-KUNNR.
      MODIFY GT_LIST INDEX P_ROW_ID-ROW_ID TRANSPORTING KUNNR.

      LS_MODI-ROW_ID   = P_ROW_ID-ROW_ID.
      LS_MODI-FIELDNAME = 'KUNNR'.
      LS_MODI-VALUE     = LV_SELECT_VALUE.
      APPEND LS_MODI TO <L_ITAB>.

      PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.

    WHEN 'LIFNR'.

      CLEAR: LV_SELECTFIELD, LT_FIELDS, LV_SELECT_VALUE, LD_TABIX.
      REFRESH: LT_FIELDS.

      LT_FIELDS-TABNAME = 'LFA1'.
      LT_FIELDS-FIELDNAME = 'LIFNR'.
      LT_FIELDS-SELECTFLAG = 'X'.
      APPEND LT_FIELDS.
      LT_FIELDS-TABNAME = 'LFA1'.
      LT_FIELDS-FIELDNAME = 'NAME1'.
      LT_FIELDS-SELECTFLAG = ' '.
      APPEND LT_FIELDS.

      CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
        EXPORTING
          SELECTFIELD                  = LV_SELECTFIELD
        IMPORTING
          IND                          = LD_TABIX
          SELECT_VALUE                 = LV_SELECT_VALUE
        TABLES
          FIELDS                       = LT_FIELDS
          FULL_TABLE                   = GT_LFA1
        EXCEPTIONS
          FULL_TABLE_EMPTY             = 1
          NO_TABLESTRUCTURE_GIVEN      = 2
          NO_TABLEFIELDS_IN_DICTIONARY = 3
          MORE_THEN_ONE_SELECTFIELD    = 4
          NO_SELECTFIELD               = 5
          OTHERS                       = 6.
      CHECK NOT LD_TABIX IS INITIAL.
      READ TABLE GT_KNA1 INDEX LD_TABIX.
      ASSIGN PR_EVENT_DATA->M_DATA->* TO <L_ITAB>.
      CLEAR GT_LIST.
      READ TABLE GT_LIST INDEX P_ROW_ID-ROW_ID.
      GT_LIST-LIFNR = LV_SELECT_VALUE.
      MODIFY GT_LIST INDEX P_ROW_ID-ROW_ID TRANSPORTING LIFNR.

      LS_MODI-ROW_ID   = P_ROW_ID-ROW_ID.
      LS_MODI-FIELDNAME = 'LIFNR'.
      LS_MODI-VALUE     = LV_SELECT_VALUE.
      APPEND LS_MODI TO <L_ITAB>.

      PR_EVENT_DATA->M_EVENT_HANDLED = 'X'.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ON_F4
*&---------------------------------------------------------------------*
FORM SET_ON_F4 .

  DATA: LS_F4 TYPE LVC_S_F4,
        LT_F4 TYPE LVC_T_F4.

  CLEAR LS_F4.
  LS_F4-FIELDNAME  = 'KUNNR'.
  LS_F4-REGISTER   = 'X'.
  APPEND LS_F4 TO LT_F4.

  CLEAR LS_F4.
  LS_F4-FIELDNAME  = 'LIFNR'.
  LS_F4-REGISTER   = 'X'.
  APPEND LS_F4 TO LT_F4.

  CLEAR LS_F4.
  LS_F4-FIELDNAME  = 'ZKUNNR_IC'.
  LS_F4-REGISTER   = 'X'.
  APPEND LS_F4 TO LT_F4.

  CLEAR LS_F4.
  LS_F4-FIELDNAME  = 'ZPRODH_GROUP'.
  LS_F4-REGISTER   = 'X'.
  APPEND LS_F4 TO LT_F4.

  CALL METHOD GO_GRID->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = LT_F4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCAT_9100
*&---------------------------------------------------------------------*
FORM SET_FIELDCAT_9100 .

  CLEAR: GT_FCAT1[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKUNNR_IC'.
  GS_FCAT-COLTEXT    = TEXT-F03. "Intercompany.
  GS_FCAT-F4AVAILABL = 'X'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKUNNR_IC_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F04. "Intercompany Desc.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR'.
  GS_FCAT-COLTEXT    = TEXT-F05. "Customer.
  GS_FCAT-REF_FIELD  = 'KUNNR'.
  GS_FCAT-REF_TABLE  = 'KNA1'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F06. "Customer Desc.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR'.
  GS_FCAT-COLTEXT    = TEXT-F07. "Vendor.
  GS_FCAT-REF_FIELD  = 'LIFNR'.
  GS_FCAT-REF_TABLE  = 'LFA1'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F08. "Vendor Desc.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZPRODH_GROUP'.
  GS_FCAT-COLTEXT    = TEXT-F09. " Group Product hierarchy
  GS_FCAT-F4AVAILABL = 'X'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZPRODH_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F10. "Group Product hierarchy DESC
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F11. "SKU.
  GS_FCAT-REF_FIELD  = 'MATNR'.
  GS_FCAT-REF_TABLE  = 'MARA'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F12. "SKU Desc.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZMARGIN'.
  GS_FCAT-COLTEXT    = TEXT-F13. "Margin(%).
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZSTART'.
  GS_FCAT-COLTEXT    = TEXT-F14. "Start Date.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZEXCEPT'.
  GS_FCAT-COLTEXT    = TEXT-F15. "Except.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZCONFIRM'.
  GS_FCAT-COLTEXT    = TEXT-F16. "CONFIRM.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_DATA
*&---------------------------------------------------------------------*
FORM UPLOAD_DATA .

  DATA: LT_INTERN LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA: L_TYPE    TYPE C.
  DATA: LV_COL    TYPE KCD_EX_COL_N.
  FIELD-SYMBOLS <FS> TYPE ANY.

  CLEAR: LT_INTERN, LT_INTERN[].
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = GV_PATH
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 256
      I_END_ROW               = 20000
    TABLES
      INTERN                  = LT_INTERN
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC NE 0.
    LEAVE LIST-PROCESSING.
  ELSE.
    CLEAR: GT_UPLOAD, GT_UPLOAD[].
    LOOP AT LT_INTERN.
      AT NEW ROW.
        CLEAR GT_UPLOAD.
      ENDAT.

      LV_COL = LT_INTERN-COL.

      ASSIGN COMPONENT LV_COL OF STRUCTURE GT_UPLOAD TO <FS>.
      DESCRIBE FIELD <FS> TYPE L_TYPE.

      <FS> = LT_INTERN-VALUE.
      AT END OF ROW.
        APPEND GT_UPLOAD.
      ENDAT.
    ENDLOOP.
  ENDIF.

  PERFORM CHECK_UPLOAD_DATA.

  PERFORM CELL_STYLE.
  PERFORM REFRESH_LIST.

ENDFORM.
