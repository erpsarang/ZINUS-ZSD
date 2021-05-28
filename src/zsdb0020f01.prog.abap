*&---------------------------------------------------------------------*
*& Include          ZSDB0020F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT .

  PERFORM GET_AUTHORITY.

  IF P_WERKS IS NOT INITIAL.
    SELECT SINGLE NAME1
    INTO T_WERKS
    FROM T001W
    WHERE WERKS EQ P_WERKS
      AND SPRAS EQ SY-LANGU.
  ENDIF.

ENDFORM. " SET_INIT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA : LS_MOD_CELLS TYPE LVC_S_MODI.

  CLEAR: LS_MOD_CELLS.

  LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
    CASE LS_MOD_CELLS-FIELDNAME.
*      WHEN 'TRKORR'.

    ENDCASE.
  ENDLOOP.


ENDFORM. " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING P_UCOMM.

  CASE P_UCOMM.

  ENDCASE.

ENDFORM. " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& Form POPUP_MSG
*&---------------------------------------------------------------------*
FORM POPUP_MSG USING P_MSG1 P_MSG2 PV_CHECK.

  CLEAR PV_CHECK.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = P_MSG1
      TEXT_QUESTION  = P_MSG2
      TEXT_BUTTON_1  = 'YES'
      TEXT_BUTTON_2  = 'NO'
    IMPORTING
      ANSWER         = PV_CHECK
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING P_ROW_ID
                                 P_COLUMN_ID.

  CLEAR GT_LIST.
  READ TABLE GT_LIST INDEX P_ROW_ID.
  CASE P_COLUMN_ID.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILENAME_INPUT_HELP_EXCEL
*&---------------------------------------------------------------------*
FORM FILENAME_INPUT_HELP_EXCEL  CHANGING P_FNAME.

  DATA : L_TITLE TYPE STRING,
         L_RC    TYPE SY-SUBRC,
         L_LEN   TYPE I,
         LT_FILE TYPE FILETABLE WITH HEADER LINE.

  CLEAR   : L_RC, LT_FILE, P_FNAME, L_LEN.
  REFRESH : LT_FILE.
  L_TITLE = TEXT-003.

*-- File open dialog
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = L_TITLE
      FILE_FILTER             = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
    CHANGING
      FILE_TABLE              = LT_FILE[]
      RC                      = L_RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF NOT LT_FILE[] IS INITIAL.
*-- The length of file path should be in 128
      READ TABLE LT_FILE INDEX 1.
      IF SY-SUBRC EQ 0.
        L_LEN = STRLEN( LT_FILE ).
        IF L_LEN GE 128.
          MESSAGE S002 DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ELSE.
          P_FNAME = LT_FILE.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE S003.
    ENDIF.
  ENDIF.

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
      FILENAME                = P_FILE
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

      <FS> = LT_INTERN-VALUE.
      AT END OF ROW.
        REPLACE ALL OCCURRENCES OF ',' IN GT_UPLOAD-BMENG WITH SPACE.
        CONDENSE GT_UPLOAD-BMENG NO-GAPS.
        REPLACE ALL OCCURRENCES OF ',' IN GT_UPLOAD-MENGE WITH SPACE.
        CONDENSE GT_UPLOAD-MENGE NO-GAPS.
        APPEND GT_UPLOAD.
      ENDAT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DATA .

  DATA : BEGIN OF LT_MARA OCCURS 0,
           MATNR LIKE MARA-MATNR,
           MEINS LIKE MARA-MEINS,
         END OF LT_MARA.

  CHECK GT_UPLOAD[] IS NOT INITIAL.

  SELECT A~MATNR A~MEINS
    INTO TABLE LT_MARA
    FROM MARA AS A INNER JOIN MARC AS B
                           ON A~MATNR = B~MATNR
   FOR ALL ENTRIES IN GT_UPLOAD
   WHERE A~MATNR EQ GT_UPLOAD-IDNRK
     AND B~WERKS EQ P_WERKS.

  SORT LT_MARA BY MATNR.

  _CLEAR : GT_LIST.
  LOOP AT GT_UPLOAD.
    MOVE-CORRESPONDING GT_UPLOAD TO GT_LIST.

    PERFORM REQUIRED_FIELD_CHECK USING : GT_LIST-MATNR TEXT-F01, "'Material(SKU)'.
                                         GT_LIST-WERKS TEXT-F02, "'Plant'.
                                         GT_LIST-IDNRK TEXT-F03, "'BOM component'.
                                         GT_LIST-BMENG TEXT-F04, "'Base quantity'.
                                         GT_LIST-MENGE TEXT-F05. "'Component quantity'.

    IF GT_LIST-MESSAGE IS INITIAL.
      IF GT_LIST-WERKS NE P_WERKS.
        GT_LIST-ICON    = ICON_RED_LIGHT.
        GT_LIST-MESSAGE = TEXT-E01. "'Please check plant'.
      ENDIF.

      CLEAR LT_MARA.
      READ TABLE LT_MARA WITH KEY MATNR = GT_LIST-IDNRK BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-MEINS = LT_MARA-MEINS.
      ELSE.
        GT_LIST-ICON    = ICON_RED_LIGHT.
        GT_LIST-MESSAGE = TEXT-E02. "'Please check BOM component'.
      ENDIF.

      IF GT_LIST-ICON IS INITIAL.
        GT_LIST-ICON = ICON_YELLOW_LIGHT.
      ENDIF.
    ENDIF.
    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_BOM
*&---------------------------------------------------------------------*
FORM CREATE_BOM.

  DATA: LT_STPO    LIKE STPO_API01 OCCURS 0 WITH HEADER LINE,
        LS_STKO    LIKE STKO_API01,
        LV_DATUM   LIKE SY-DATUM,
        LV_WARNING LIKE CAPIFLAG-FLWARNING,
        LV_BOM     LIKE STKO_API02-BOM_NO.

  CLEAR: GT_ROWS[], GS_ROWS.
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.
    CLEAR : GV_SUCCESS, GV_FAILURE.
    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.

      CHECK GT_LIST-ICON = ICON_YELLOW_LIGHT.

      _CLEAR LT_STPO.
      CLEAR : LS_STKO, GV_SUCCESS, GV_FAILURE.
      LOOP AT GT_COMP WHERE MATNR = GT_LIST-MATNR.
        LS_STKO-BASE_QUAN = GT_COMP-BMENG.
        REPLACE ALL OCCURRENCES OF ',' IN LS_STKO-BASE_QUAN WITH SPACE.
        CONDENSE LS_STKO-BASE_QUAN.

        ADD 10 TO LT_STPO-ITEM_NO.
        LT_STPO-ITEM_CATEG = 'L'.
        LT_STPO-COMPONENT  = GT_COMP-IDNRK.
        LT_STPO-COMP_QTY   = GT_COMP-MENGE.
        LT_STPO-COMP_UNIT  = GT_COMP-MEINS.
        APPEND LT_STPO.
      ENDLOOP.

      LV_DATUM = SY-DATLO+0(6) && '01'.

      CALL FUNCTION 'CSAP_MAT_BOM_CREATE'
        EXPORTING
          MATERIAL   = GT_LIST-MATNR
          PLANT      = P_WERKS
          BOM_USAGE  = '5'
          VALID_FROM = LV_DATUM
          I_STKO     = LS_STKO
        IMPORTING
          FL_WARNING = LV_WARNING
          BOM_NO     = LV_BOM
        TABLES
          T_STPO     = LT_STPO
        EXCEPTIONS
          ERROR      = 1
          OTHERS     = 2.

      IF SY-SUBRC EQ 0.
        GT_LIST-MESSAGE = text-s01. "'BOM is created!'.
        GT_LIST-ICON = ICON_GREEN_LIGHT.
        ADD 1 TO GV_SUCCESS.
      ELSE.
        GT_LIST-MESSAGE = text-e04. "'BOM is not created!'.
        GT_LIST-ICON = ICON_RED_LIGHT.
        MODIFY GT_LIST TRANSPORTING MESSAGE WHERE MATNR = GT_LIST-MATNR .
        ADD 1 TO GV_FAILURE.
      ENDIF.
    ENDLOOP.

    FREE GO_DOCUMENT.
    CREATE OBJECT GO_DOCUMENT
      EXPORTING
        STYLE = 'TOP_OF_PAGE'.

    PERFORM HANDLE_TOP_OF_PAGE USING GO_DOCUMENT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_INPUT
*&---------------------------------------------------------------------*
FORM ALPHA_INPUT  USING    P_DATA.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_DATA
    IMPORTING
      OUTPUT = P_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM HANDLE_TOOLBAR   USING PE_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                            PE_INTERACTIVE.

  DATA: LS_TOOLBAR TYPE STB_BUTTON.
  DATA: LS_UPLOAD  TYPE STB_BUTTON.

  CLEAR LS_UPLOAD.
  MOVE 3 TO LS_UPLOAD-BUTN_TYPE.
  APPEND LS_UPLOAD TO PE_OBJECT->MT_TOOLBAR.

*    LS_TOOLBAR-FUNCTION  = SPACE.
*    LS_TOOLBAR-ICON      = SPACE.
*    LS_TOOLBAR-BUTN_TYPE = '3'.
*    LS_TOOLBAR-DISABLED  = SPACE.
*    LS_TOOLBAR-TEXT      = SPACE.
*    LS_TOOLBAR-QUICKINFO = SPACE.
*    LS_TOOLBAR-CHECKED   = SPACE.
*    APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.
*
*    LS_TOOLBAR-FUNCTION  = 'DELETE'.
*    LS_TOOLBAR-ICON      = ICON_DELETE.
*    LS_TOOLBAR-BUTN_TYPE = SPACE.
*    LS_TOOLBAR-DISABLED  = SPACE.
*    LS_TOOLBAR-TEXT      = 'DELETE'.
*    LS_TOOLBAR-QUICKINFO = 'DELETE'.
*    LS_TOOLBAR-CHECKED   = SPACE.
*    APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    PE_ROW
                                   PE_COLUMN.

  CASE PE_COLUMN.
*    WHEN 'VBELN'.
*      CLEAR : GT_LIST.
*      READ TABLE GT_LIST INDEX PE_ROW.
*      CHECK SY-SUBRC EQ 0.
*      SET PARAMETER ID 'AUN' FIELD GT_LIST-VBELN.
*      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HANDLE_TOP_OF_PAGE  USING PE_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT.

  DATA : LV_TEXT(255) TYPE C,
         LT_MAKT      LIKE TABLE OF MAKT WITH HEADER LINE.

  CLEAR : LV_TEXT.
  SELECT SINGLE NAME1 INTO LV_TEXT
  FROM T001W
  WHERE WERKS = P_WERKS
  AND SPRAS = SY-LANGU."Plant
  CONCATENATE TEXT-001 ' :' P_WERKS '('  LV_TEXT  ')'
  INTO LV_TEXT SEPARATED BY SPACE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'MEDIUM'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  CLEAR : LV_TEXT.
  DESCRIBE TABLE GT_LIST LINES DATA(LV_ROWS).
  LV_TEXT = LV_ROWS.
  CONDENSE LV_TEXT. "Uploaded Data
  CONCATENATE TEXT-004 ' :' LV_TEXT INTO LV_TEXT SEPARATED BY SPACE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'MEDIUM'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  _CLEAR LT_MAKT.
  MOVE-CORRESPONDING GT_LIST[] TO LT_MAKT[].
  SORT LT_MAKT BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_MAKT COMPARING MATNR.
  DESCRIBE TABLE LT_MAKT LINES DATA(LV_ROWS2).
  LV_TEXT = LV_ROWS2.
  CONDENSE LV_TEXT. "Can be created BOM
  CONCATENATE TEXT-005 ' :' LV_TEXT INTO LV_TEXT SEPARATED BY SPACE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'MEDIUM'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  IF GV_SUCCESS IS NOT INITIAL.
    CLEAR : LV_TEXT.
    LV_TEXT = TEXT-006 && ' :' && GV_SUCCESS.

    CALL METHOD PE_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = LV_TEXT
        SAP_EMPHASIS = CL_DD_AREA=>HEADING
        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF GV_FAILURE IS NOT INITIAL.
    CLEAR : LV_TEXT.
    LV_TEXT = TEXT-007 && ' :' && GV_FAILURE.

    CALL METHOD PE_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = LV_TEXT
        SAP_EMPHASIS = CL_DD_AREA=>HEADING
        SAP_COLOR    = CL_DD_AREA=>LIST_NEGATIVE_INT.

    CALL METHOD PE_DYNDOC_ID->NEW_LINE.
  ENDIF.

  IF GO_HEADER IS INITIAL.

    CREATE OBJECT GO_HEADER
      EXPORTING
        PARENT = G_PARENT_HTML.

  ENDIF.

  CALL METHOD PE_DYNDOC_ID->MERGE_DOCUMENT.
  PE_DYNDOC_ID->HTML_CONTROL = GO_HEADER.

  CALL METHOD PE_DYNDOC_ID->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = G_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TEMPLATE_BUTTON
*&---------------------------------------------------------------------*
FORM TEMPLATE_BUTTON .

  DATA : L_DYNTXT     TYPE SMP_DYNTXT.

 CLEAR : L_DYNTXT.
  MOVE : TEXT-008     TO L_DYNTXT-TEXT,
         ICON_XLS     TO L_DYNTXT-ICON_ID,
         TEXT-008     TO L_DYNTXT-ICON_TEXT,
         TEXT-008     TO L_DYNTXT-QUICKINFO,
         'C:\TEMP'    TO L_DYNTXT-PATH.      "C:\TEMP

  SSCRFIELDS-FUNCTXT_01 = L_DYNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TEMPLATE_DOWNLOAD
*&---------------------------------------------------------------------*
FORM TEMPLATE_DOWNLOAD .

  DATA : WWWDATATAB LIKE WWWDATATAB.

  DATA : LV_FILENAME TYPE STRING,
         LV_PATH     TYPE STRING,
         LV_FULLPATH TYPE STRING.

  DATA : FILENAME TYPE RLGRAP-FILENAME.

  CLEAR : WWWDATATAB.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF WWWDATATAB
    FROM WWWDATA
   WHERE OBJID EQ 'ZSDB0020'.

  CHECK SY-SUBRC = 0.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE      = 'Excel Format'
      DEFAULT_EXTENSION = 'xls'
      DEFAULT_FILE_NAME = 'Upload Format'
      FILE_FILTER       = 'Only Excel Files (*.xls;*.xlsx)'
      INITIAL_DIRECTORY = 'C:\'
    CHANGING
      FILENAME          = LV_FILENAME
      PATH              = LV_PATH
      FULLPATH          = LV_FULLPATH.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  FILENAME = LV_FULLPATH.

  CHECK FILENAME IS NOT INITIAL .

  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = WWWDATATAB
      DESTINATION = FILENAME.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>EXECUTE
    EXPORTING
      DOCUMENT               = LV_FULLPATH
    EXCEPTIONS
      CNTL_ERROR             = 1
      ERROR_NO_GUI           = 2
      BAD_PARAMETER          = 3
      FILE_NOT_FOUND         = 4
      PATH_NOT_FOUND         = 5
      FILE_EXTENSION_UNKNOWN = 6
      ERROR_EXECUTE_FAILED   = 7
      SYNCHRONOUS_FAILED     = 8
      NOT_SUPPORTED_BY_GUI   = 9
      OTHERS                 = 10.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORIZATION_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORIZATION_CHECK .

  DATA : LS_RETURN LIKE BAPIRETURN1 .

  CALL FUNCTION 'ZBC_AUTHORIZATION_CHECK'
    EXPORTING
*     I_BUKRS   = P_BUKRS
*     I_KOKRS   = P_KOKRS
      I_WERKS   = P_WERKS
*     I_VKORG   =
*     I_EKORG   =
    IMPORTING
      ES_RETURN = LS_RETURN.

  IF LS_RETURN-TYPE = 'E'.
    MESSAGE E000 WITH LS_RETURN-MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_AUTHORITY
*&---------------------------------------------------------------------*
FORM GET_AUTHORITY .

  DATA : LT_WERKS LIKE TABLE OF RANGE_WERKS WITH HEADER LINE.

  _CLEAR LT_WERKS.
  CALL FUNCTION 'ZSD_GET_AUTHORITY'
    EXPORTING
      I_UNAME = SY-UNAME
    TABLES
      T_WERKS = LT_WERKS.

  READ TABLE LT_WERKS INDEX 1.
  P_WERKS = LT_WERKS-LOW.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SELECTION_SCREEN .

  IF P_WERKS IS NOT INITIAL.
    SELECT SINGLE NAME1
    INTO T_WERKS
    FROM T001W
    WHERE WERKS EQ P_WERKS
      AND SPRAS EQ SY-LANGU.
  ENDIF.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'A'.
      SCREEN-INPUT = 0.
      SCREEN-INTENSIFIED = 1.
      SCREEN-DISPLAY_3D = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REQUIRED_FIELD_CHECK
*&---------------------------------------------------------------------*
FORM REQUIRED_FIELD_CHECK  USING  PV_DATA PV_TEXT.

  CHECK PV_DATA IS INITIAL.

  IF GT_LIST-MESSAGE IS INITIAL.
    GT_LIST-MESSAGE = PV_TEXT && text-e03.
    GT_LIST-ICON = ICON_RED_LIGHT.
  ELSE.
    GT_LIST-MESSAGE = GT_LIST-MESSAGE && '/' && PV_TEXT && text-e03.
  ENDIF.

ENDFORM.
