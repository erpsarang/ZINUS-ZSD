*&---------------------------------------------------------------------*
*& Include          ZSDB0040F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT .

  PERFORM GET_AUTHORITY.

  IF P_VKORG IS NOT INITIAL.
    SELECT SINGLE VTEXT   "20자리
      INTO T_VKORG
      FROM TVKOT
      WHERE VKORG EQ P_VKORG
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
    IF P_VKORG = '1001'.
      CLEAR: GT_UPLOAD2, GT_UPLOAD2[].
      LOOP AT LT_INTERN.
        AT NEW ROW.
          CLEAR GT_UPLOAD2.
        ENDAT.

        LV_COL = LT_INTERN-COL.

        ASSIGN COMPONENT LV_COL OF STRUCTURE GT_UPLOAD2 TO <FS>.
        <FS> = LT_INTERN-VALUE.
        AT END OF ROW.
          PERFORM ADJUST_DATA USING GT_UPLOAD2-DATAB.
          PERFORM ADJUST_DATA USING GT_UPLOAD2-DATBI.

          APPEND GT_UPLOAD2.
        ENDAT.
      ENDLOOP.
    ELSE.
      CLEAR: GT_UPLOAD1, GT_UPLOAD1[].
      LOOP AT LT_INTERN.
        AT NEW ROW.
          CLEAR GT_UPLOAD1.
        ENDAT.

        LV_COL = LT_INTERN-COL.

        ASSIGN COMPONENT LV_COL OF STRUCTURE GT_UPLOAD1 TO <FS>.
        <FS> = LT_INTERN-VALUE.
        AT END OF ROW.
          PERFORM ADJUST_DATA USING GT_UPLOAD1-DATAB.
          PERFORM ADJUST_DATA USING GT_UPLOAD1-DATBI.

          APPEND GT_UPLOAD1.
        ENDAT.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DATA .

  DATA : LV_KBETR     TYPE C LENGTH 15,
         LV_KONWA     LIKE TCURC-WAERS,
         LV_KONWA_CON TYPE C LENGTH 15.
  RANGES LR_KUNNR FOR BUT000-PARTNER.

  _CLEAR GT_0040.
  SELECT ZKUNNR_IC
  INTO CORRESPONDING FIELDS OF TABLE GT_0040
  FROM ZSDT0040.
  SORT GT_0040 BY ZKUNNR_IC.

  IF P_VKORG EQ '1001'.
    MOVE-CORRESPONDING GT_UPLOAD2[] TO GT_LIST[].
  ELSE.
    MOVE-CORRESPONDING GT_UPLOAD1[] TO GT_LIST[].
  ENDIF.

  LOOP AT GT_LIST.
    PERFORM ALPHA_INPUT USING GT_LIST-KUNNR1.
    PERFORM ALPHA_INPUT USING GT_LIST-KUNNR2.
    _RANGE LR_KUNNR 'I' 'EQ' GT_LIST-KUNNR1 ''.
    _RANGE LR_KUNNR 'I' 'EQ' GT_LIST-KUNNR2 ''.
  ENDLOOP.
  DELETE LR_KUNNR WHERE LOW EQ SPACE.


  SELECT MATNR,
         MAKTX
    FROM MAKT
    INTO TABLE @DATA(LT_MAKT)
    FOR ALL ENTRIES IN @GT_LIST
    WHERE MATNR = @GT_LIST-MATNR
      AND SPRAS = @SY-LANGU.
  SORT LT_MAKT BY MATNR.

  SELECT PARTNER,
         NAME_ORG1
    FROM BUT000
    INTO TABLE @DATA(LT_BP)
    WHERE PARTNER IN @LR_KUNNR.
  SORT LT_BP BY PARTNER.

  LOOP AT GT_LIST.
    PERFORM ALPHA_INPUT USING GT_LIST-KUNNR1.
    PERFORM ALPHA_INPUT USING GT_LIST-KUNNR2.

    LV_KBETR = GT_LIST-KBETR.
    LV_KONWA = GT_LIST-KONWA.

    CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
      EXPORTING
        CURRENCY    = LV_KONWA
        IDOC_AMOUNT = LV_KBETR
      IMPORTING
        SAP_AMOUNT  = LV_KONWA_CON.
    CONDENSE LV_KONWA_CON.
    GT_LIST-KBETR = LV_KONWA_CON.

    PERFORM REQUIRED_FIELD_CHECK USING : GT_LIST-KSCHL  TEXT-F02, "'Condition type'.
                                         GT_LIST-VKORG  TEXT-F03, "'Sales Organization.'.
                                         GT_LIST-VTWEG  TEXT-F04, "'Distribution Channel.'.
                                         GT_LIST-KUNNR1 TEXT-F05. "'Customer number.'.

    READ TABLE LT_BP INTO DATA(LS_BP1) WITH KEY PARTNER = GT_LIST-KUNNR1 BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-KUNNR1_TXT = LS_BP1-NAME_ORG1.
    ELSE.
      PERFORM ERROR_FIELD USING TEXT-E02.
    ENDIF.

    IF P_VKORG EQ '1001'.
      READ TABLE GT_0040 WITH KEY ZKUNNR_IC = GT_LIST-KUNNR2 BINARY SEARCH.
      IF SY-SUBRC = 0.
        PERFORM REQUIRED_FIELD_CHECK USING GT_LIST-KUNNR2 TEXT-F06. "'Ship-to number'.
        READ TABLE LT_BP INTO DATA(LS_BP2) WITH KEY PARTNER = GT_LIST-KUNNR2 BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-KUNNR2_TXT = LS_BP2-NAME_ORG1.
        ELSE.
          PERFORM ERROR_FIELD USING TEXT-E01.
        ENDIF.
      ENDIF.
    ENDIF.
    PERFORM REQUIRED_FIELD_CHECK USING : GT_LIST-MATNR TEXT-F07, "'Material Number.'.
                                         GT_LIST-KBETR TEXT-F08, "'Amount.'.
                                         GT_LIST-KONWA TEXT-F09, "'Currency.'.
                                         GT_LIST-KPEIN TEXT-F10, "'Condition Pricing Unit.'.
                                         GT_LIST-KMEIN TEXT-F11, "'Condition Unit.'.
                                         GT_LIST-DATAB TEXT-F12, "'Validity start date.'.
                                         GT_LIST-DATBI TEXT-F13. "'Validity end date.'.

    READ TABLE LT_MAKT INTO DATA(LS_MAKT) WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-MATNR_TXT = LS_MAKT-MAKTX.
    ELSE.
      PERFORM ERROR_FIELD USING TEXT-E03.
    ENDIF.

    IF GT_LIST-ICON IS INITIAL.
      GT_LIST-ICON = ICON_YELLOW_LIGHT.
    ENDIF.
    MODIFY GT_LIST TRANSPORTING ICON MESSAGE KUNNR1 KUNNR2 KBETR
                                MATNR_TXT KUNNR1_TXT KUNNR2_TXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONDITION
*&---------------------------------------------------------------------*
FORM CREATE_CONDITION.

  DATA: LV_KBETR    TYPE CHAR14,
        LV_KPEIN    TYPE CHAR5,
        LV_DATAB    TYPE RV13A-DATAB,
        LV_DATBI    TYPE RV13A-DATBI,
        LV_TABIX    TYPE SY-TABIX,
        LV_MSG(255).

  CLEAR: GT_ROWS[], GS_ROWS.
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.

    CLEAR : GS_CTU_PARAMS, GV_FAILURE, GV_SUCCESS.

** BDC MODE
    GS_CTU_PARAMS-DISMODE  = P_MODE.
    GS_CTU_PARAMS-UPDMODE  = 'S'.
    GS_CTU_PARAMS-RACOMMIT = 'X'.
    GS_CTU_PARAMS-NOBINPT  = 'X'.

    CLEAR : GV_SUCCESS, GV_FAILURE.
    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.

      CHECK GT_LIST-ICON EQ ICON_YELLOW_LIGHT.

      _CLEAR : GT_BDCDATA, GT_BDCMSG.

      PERFORM BDCDATA_SET USING :
          'X'  'SAPMV13A'         '0100',
          ' '  'RV13A-KSCHL'      GT_LIST-KSCHL,
          ' '  'BDC_OKCODE'       '/00',
          'X'  'SAPLV14A'         '0100'.

      IF P_VKORG = '1001'.
        READ TABLE GT_0040 WITH KEY ZKUNNR_IC = GT_LIST-KUNNR1 BINARY SEARCH.
        IF SY-SUBRC = 0.
          PERFORM BDCDATA_SET USING :
                 ' '  'RV130-SELKZ(01)'   ' ',
                 ' '  'RV130-SELKZ(04)'   'X',
                 ' '  'BDC_OKCODE'       '=WEIT',
                 'X'  'SAPMV13A'         '1903',
                 ' '  'KOMG-VKORG'       P_VKORG,
                 ' '  'KOMG-VTWEG'       GT_LIST-VTWEG,
                 ' '  'KOMG-KUNNR'       GT_LIST-KUNNR1,
                 ' '  'KOMG-KUNWE'       GT_LIST-KUNNR2.
        ELSE.
          PERFORM BDCDATA_SET USING :
               ' '  'RV130-SELKZ(01)'   'X',
               ' '  'BDC_OKCODE'       '=WEIT',
               'X'  'SAPMV13A'         '1305',
               ' '  'KOMG-VKORG'       P_VKORG,
               ' '  'KOMG-VTWEG'       GT_LIST-VTWEG,
               ' '  'KOMG-KUNNR'       GT_LIST-KUNNR1.
        ENDIF.
      ELSE.
        PERFORM BDCDATA_SET USING :
             ' '  'RV130-SELKZ(01)'   'X',
             ' '  'BDC_OKCODE'       '=WEIT',
             'X'  'SAPMV13A'         '1305',
             ' '  'KOMG-VKORG'       P_VKORG,
             ' '  'KOMG-VTWEG'       GT_LIST-VTWEG,
             ' '  'KOMG-KUNNR'       GT_LIST-KUNNR1.
      ENDIF.

      CLEAR LV_KBETR.
      WRITE GT_LIST-KBETR TO LV_KBETR CURRENCY GT_LIST-KONWA.
      CLEAR LV_KPEIN.
      WRITE GT_LIST-KPEIN TO LV_KPEIN UNIT GT_LIST-KMEIN.

      PERFORM DATE_SETTING USING GT_LIST-DATAB
                        CHANGING LV_DATAB.

      PERFORM DATE_SETTING USING GT_LIST-DATBI
                        CHANGING LV_DATBI.
      PERFORM BDCDATA_SET USING :
          ' '  'KOMG-MATNR(01)'  GT_LIST-MATNR,
          ' '  'KONP-KBETR(01)'  LV_KBETR,
          ' '  'KONP-KONWA(01)'  GT_LIST-KONWA,
          ' '  'KONP-KPEIN(01)'  LV_KPEIN,
          ' '  'KONP-KMEIN(01)'  GT_LIST-KMEIN,
          ' '  'RV13A-DATAB(01)' LV_DATAB,
          ' '  'RV13A-DATBI(01)' LV_DATBI,
          ' '  'BDC_OKCODE'      '=SICH'.

** CALL TRANSACTION
      CLEAR : GT_BDCMSG, GT_BDCMSG[].
      CALL TRANSACTION 'VK11'
                 USING GT_BDCDATA
                 OPTIONS FROM GS_CTU_PARAMS
                 MESSAGES INTO GT_BDCMSG.

      READ TABLE GT_BDCMSG WITH KEY MSGTYP = 'E'.
      IF SY-SUBRC = 0.
        CLEAR LV_MSG.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = GT_BDCMSG-MSGID
            MSGNR               = GT_BDCMSG-MSGNR
            MSGV1               = GT_BDCMSG-MSGV1
            MSGV2               = GT_BDCMSG-MSGV2
            MSGV3               = GT_BDCMSG-MSGV3
            MSGV4               = GT_BDCMSG-MSGV4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = LV_MSG.
        ADD 1 TO GV_FAILURE.
        GT_LIST-ICON = ICON_RED_LIGHT.
        GT_LIST-MESSAGE = LV_MSG.
      ELSE.
        ADD 1 TO GV_SUCCESS.
        GT_LIST-ICON = ICON_GREEN_LIGHT.
      ENDIF.

      MODIFY GT_LIST INDEX GS_ROWS-INDEX TRANSPORTING ICON MESSAGE.
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

  DATA : LV_TEXT(255) TYPE C.

  CLEAR : LV_TEXT.
  SELECT SINGLE VTEXT INTO LV_TEXT
  FROM TVKOT
  WHERE VKORG = P_VKORG
  AND SPRAS = SY-LANGU. "Sales Organization
  CONCATENATE TEXT-F03 ' :' P_VKORG '('  LV_TEXT  ')'
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

  IF GV_SUCCESS IS NOT INITIAL.
    CLEAR : LV_TEXT.
    LV_TEXT = TEXT-005 && ' :' && GV_SUCCESS.

    CALL METHOD PE_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = LV_TEXT
        SAP_EMPHASIS = CL_DD_AREA=>HEADING
        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF GV_FAILURE IS NOT INITIAL.
    CLEAR : LV_TEXT.
    LV_TEXT = TEXT-006 && ' :' && GV_FAILURE.

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
  MOVE : TEXT-007     TO L_DYNTXT-TEXT,
          ICON_XLS    TO L_DYNTXT-ICON_ID,
         TEXT-007     TO L_DYNTXT-ICON_TEXT,
         TEXT-007     TO L_DYNTXT-QUICKINFO,
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
         LV_FULLPATH TYPE STRING,
         LV_ID(40).

  DATA : FILENAME TYPE RLGRAP-FILENAME.

  CLEAR : WWWDATATAB, LV_ID.

  IF P_VKORG EQ '1001'.
    LV_ID = 'ZSDB0040_02'.
  ELSE.
    LV_ID = 'ZSDB0040_01'.
  ENDIF.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF WWWDATATAB
    FROM WWWDATA
   WHERE OBJID EQ LV_ID.

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
*     I_BUKRS   =
*     I_KOKRS   =
*     I_WERKS   =
      I_VKORG   = P_VKORG
*     I_EKORG   =
    IMPORTING
      ES_RETURN = LS_RETURN.

  IF LS_RETURN-TYPE = 'E'.
    MESSAGE E000 WITH LS_RETURN-MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDCDATA_SET
*&---------------------------------------------------------------------*
FORM BDCDATA_SET  USING P_START P_OBJECT P_VALUE.

  CLEAR GT_BDCDATA.

  IF P_START = 'X'.
    GT_BDCDATA-DYNBEGIN = P_START.
    GT_BDCDATA-PROGRAM = P_OBJECT.
    GT_BDCDATA-DYNPRO = P_VALUE.
  ELSE.
    GT_BDCDATA-FNAM = P_OBJECT.
    GT_BDCDATA-FVAL = P_VALUE.
  ENDIF.

  APPEND GT_BDCDATA.
  CLEAR GT_BDCDATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADJUST_DATA
*&---------------------------------------------------------------------*
FORM ADJUST_DATA  USING  PV_DATA.

  REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:]]'
                                IN PV_DATA WITH ''.
  CONDENSE PV_DATA NO-GAPS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATE_SETTING
*&---------------------------------------------------------------------*
FORM DATE_SETTING  USING PV_S_DATUM
                CHANGING PV_T_DATUM.
  DATA: LV_STRING TYPE CHAR10,
        LV_FORMAT TYPE CHAR10.

  CALL FUNCTION 'ZCM_DATE_FORMAT_CONVERSION'
    EXPORTING
      I_DATE        = PV_S_DATUM
      I_USER        = SY-UNAME
    IMPORTING
      E_DATE_STRING = LV_STRING
      E_DATA_FORMAT = LV_FORMAT.

  REPLACE ALL OCCURRENCES OF '.' IN LV_STRING WITH SPACE.
  REPLACE ALL OCCURRENCES OF '/' IN LV_STRING WITH SPACE.
  REPLACE ALL OCCURRENCES OF '-' IN LV_STRING WITH SPACE.

  PV_T_DATUM = LV_STRING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_AUTHORITY
*&---------------------------------------------------------------------*
FORM GET_AUTHORITY .

  DATA : LT_VKORG LIKE TABLE OF RANGE_VKORG WITH HEADER LINE.

  _CLEAR LT_VKORG.
  CALL FUNCTION 'ZSD_GET_AUTHORITY'
    EXPORTING
      I_UNAME = SY-UNAME
    TABLES
      T_VKORG = LT_VKORG.

  READ TABLE LT_VKORG INDEX 1.
  P_VKORG = LT_VKORG-LOW.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SELECTION_SCREEN .

  IF P_VKORG IS NOT INITIAL.
    SELECT SINGLE VTEXT   "20자리
      INTO T_VKORG
      FROM TVKOT
      WHERE VKORG EQ P_VKORG
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
FORM REQUIRED_FIELD_CHECK USING  PV_DATA PV_TEXT.

  CHECK PV_DATA IS INITIAL.

  IF GT_LIST-MESSAGE IS INITIAL.
    GT_LIST-MESSAGE = PV_TEXT && TEXT-008.
    GT_LIST-ICON = ICON_RED_LIGHT.
  ELSE.
    GT_LIST-MESSAGE = GT_LIST-MESSAGE && '/' && PV_TEXT && TEXT-008.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ERROR_FIELD
*&---------------------------------------------------------------------*
FORM ERROR_FIELD  USING    PV_TEXT.

  IF GT_LIST-MESSAGE IS INITIAL.
    GT_LIST-MESSAGE = PV_TEXT.
    GT_LIST-ICON = ICON_RED_LIGHT.
  ELSE.
    GT_LIST-MESSAGE = GT_LIST-MESSAGE && '/' && PV_TEXT.
  ENDIF.

ENDFORM.
