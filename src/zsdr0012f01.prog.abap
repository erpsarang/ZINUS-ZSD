*&---------------------------------------------------------------------*
*& Include          ZSDR0012F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT .

  SELECT A~ZKUNNR_IC
         B~NAME_ORG1 AS ZKUNNR_IC_TXT
  INTO CORRESPONDING FIELDS OF TABLE GT_0040
  FROM ZSDT0040 AS A INNER JOIN BUT000 AS B ON A~ZKUNNR_IC = B~PARTNER
  WHERE A~BUKRS = SPACE.

  SORT GT_0040 BY ZKUNNR_IC.
  DELETE ADJACENT DUPLICATES FROM GT_0040 COMPARING ZKUNNR_IC.

  SELECT B~ZKUNNR
         B~ZKUNNR_DESC AS ZKUNNR_TXT
  INTO CORRESPONDING FIELDS OF TABLE GT_0041
  FROM ZSDT0040 AS A INNER JOIN ZSDT0041 AS B ON A~ZKUNNR_IC = B~ZKUNNR_IC
  WHERE A~BUKRS = SPACE.

  SORT GT_0041 BY ZKUNNR.
  DELETE ADJACENT DUPLICATES FROM GT_0041 COMPARING ZKUNNR.


ENDFORM. " SET_INIT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA : LS_MOD_CELLS     TYPE LVC_S_MODI,
         LV_GETVALUE(30),
         LV_KBETR         LIKE ZSDT0070-KBETR,
         LV_KBETR2        TYPE BAPICURR-BAPICURR,
         LV_CURR          LIKE TCURC-WAERS,
         LV_MODIVALUE(30).

  CLEAR: LS_MOD_CELLS.

  LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'VKORG'.
        CLEAR LV_GETVALUE.
        PERFORM GET_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'VKORG' LV_GETVALUE.

        IF LV_GETVALUE IS INITIAL.
          PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'VKORG_TXT' SPACE.
        ENDIF.

        CLEAR GT_TVTWT.
        READ TABLE GT_TVTWT WITH KEY VTWEG = LV_GETVALUE BINARY SEARCH.
        LV_MODIVALUE = GT_TVTWT-VTEXT.
        IF LV_MODIVALUE IS INITIAL.
          PERFORM MODI_CELL USING : PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'VTWEG' SPACE.
          MESSAGE S000 WITH TEXT-E02  '(Distribution Channel)' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
        PERFORM MODI_CELL USING : PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'MESSAGE' SPACE.

      WHEN 'ZKUNNR'.
        CLEAR LV_GETVALUE.
        PERFORM GET_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ZKUNNR' LV_GETVALUE.

        IF LV_GETVALUE IS INITIAL.
          PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ZKUNNR_TXT' SPACE.
        ENDIF.

        CLEAR GT_0041.
        READ TABLE GT_0041 WITH KEY ZKUNNR = LV_GETVALUE BINARY SEARCH.
        LV_MODIVALUE = GT_0041-ZKUNNR_TXT.
        IF LV_MODIVALUE IS INITIAL.
          PERFORM MODI_CELL USING : PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ZKUNNR' SPACE,
                                    PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ZKUNNR_TXT' SPACE.
          MESSAGE S000 WITH TEXT-E02  '(Customer)' DISPLAY LIKE 'E'.
          EXIT.
        ELSE.
          PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ZKUNNR_TXT' LV_MODIVALUE.
        ENDIF.
        PERFORM MODI_CELL USING : PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'MESSAGE' SPACE.

      WHEN 'MATNR'.
        CLEAR LV_GETVALUE.
        PERFORM GET_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'MATNR' LV_GETVALUE.

        IF LV_GETVALUE IS INITIAL.
          PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'MATNR_TXT' SPACE.
        ENDIF.

        CLEAR LV_MODIVALUE.
        SELECT SINGLE MAKTX INTO LV_MODIVALUE
        FROM MAKT WHERE MATNR = LV_GETVALUE.
        IF LV_MODIVALUE IS INITIAL.
          PERFORM MODI_CELL USING : PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'MATNR' SPACE,
                                    PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'MATNR_TXT' SPACE.
          MESSAGE S000 WITH TEXT-E02  '(SKU)' DISPLAY LIKE 'E'.
          EXIT.
        ELSE.
          PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'MATNR_TXT' LV_MODIVALUE.
        ENDIF.
        PERFORM MODI_CELL USING : PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'MESSAGE' SPACE.
      WHEN 'KONWA'.
        CLEAR LV_GETVALUE.
        PERFORM GET_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'KBETR' LV_GETVALUE.
        PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'KBETR' LV_GETVALUE.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.


ENDFORM. " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING P_UCOMM.

  CASE P_UCOMM.
    WHEN 'ADD'.
      PERFORM ADD_LINE.
    WHEN 'DELETE'.
      PERFORM DELETE_LINE.
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

  LS_TOOLBAR-FUNCTION  = 'ADD'.
  LS_TOOLBAR-ICON      = ICON_INSERT_ROW.
  LS_TOOLBAR-BUTN_TYPE = SPACE.
  LS_TOOLBAR-DISABLED  = SPACE.
  LS_TOOLBAR-TEXT      = 'Add Line'.
  LS_TOOLBAR-QUICKINFO = 'Add Line'.
  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.

  CLEAR LS_UPLOAD.
  MOVE 3 TO LS_UPLOAD-BUTN_TYPE.
  APPEND LS_UPLOAD TO PE_OBJECT->MT_TOOLBAR.

  LS_TOOLBAR-FUNCTION  = 'DELETE'.
  LS_TOOLBAR-ICON      = ICON_DELETE_ROW.
  LS_TOOLBAR-BUTN_TYPE = SPACE.
  LS_TOOLBAR-DISABLED  = SPACE.
  LS_TOOLBAR-TEXT      = 'Delete Line'.
  LS_TOOLBAR-QUICKINFO = 'Delete Line'.
  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.

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
         LV_TEXT2(10).

  CLEAR : LV_TEXT.

  IF P_CRE = 'X'.
    LV_TEXT = TEXT-S01.
  ELSEIF P_DIS = 'X'.
    LV_TEXT = TEXT-S02.
  ENDIF.

  LV_TEXT = LV_TEXT && ' Mode'.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'LARGE'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  CLEAR : LV_TEXT, GT_0040.
  READ TABLE GT_0040 WITH KEY ZKUNNR_IC = P_INCOMP.
  LV_TEXT = GT_0040-ZKUNNR_IC_TXT.
  LV_TEXT2 = P_INCOMP.
  PERFORM ALPHA_OUTPUT USING LV_TEXT2.
  LV_TEXT = LV_TEXT2 && ' :　' && LV_TEXT.
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
*& Form AUTHORIZATION_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORIZATION_CHECK .

*  DATA : LS_RETURN LIKE BAPIRETURN1 .
*
*  CALL FUNCTION 'ZBC_AUTHORIZATION_CHECK'
*    EXPORTING
**     I_BUKRS   =
**     I_KOKRS   =
**     I_WERKS   =
**      I_VKORG   = P_VKORG
**     I_EKORG   =
*    IMPORTING
*      ES_RETURN = LS_RETURN.
*
*  IF LS_RETURN-TYPE = 'E'.
*    MESSAGE E000 WITH LS_RETURN-MESSAGE.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_AUTHORITY
*&---------------------------------------------------------------------*
FORM GET_AUTHORITY .

*  DATA : LT_VKORG LIKE TABLE OF RANGE_VKORG WITH HEADER LINE.
*
*  _CLEAR LT_VKORG.
*  CALL FUNCTION 'ZSD_GET_AUTHORITY'
*    EXPORTING
*      I_UNAME = SY-UNAME
*    TABLES
*      T_VKORG = LT_VKORG.
*
*  READ TABLE LT_VKORG INDEX 1.
*  P_VKORG = LT_VKORG-LOW.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SELECTION_SCREEN .

  LOOP AT SCREEN.
    IF P_CRE = 'X'.
      IF SCREEN-GROUP1 = 'Z01'.
        SCREEN-INPUT = 0.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .

  _CLEAR : GT_TVTWT.

  SELECT VTWEG VTEXT INTO CORRESPONDING FIELDS OF TABLE GT_TVTWT
  FROM TVTWT.
  SORT GT_TVTWT BY VTWEG.

  _CLEAR GT_LIST.
  SELECT A~ZKUNNR_IC
         A~VTWEG
         A~ZKUNNR
         A~MATNR
         A~DATAB
         A~DATBI
         A~KBETR
         A~KONWA
         A~KPEIN
         A~KMEIN
         A~LOEVM_KO
         B~ZKUNNR_DESC AS ZKUNNR_TXT
         C~MAKTX       AS MATNR_TXT
  INTO CORRESPONDING FIELDS OF TABLE GT_LIST
  FROM ZSDT0070 AS A INNER JOIN ZSDT0041 AS B ON A~ZKUNNR = B~ZKUNNR
                     INNER JOIN MAKT     AS C ON A~MATNR  = C~MATNR
  WHERE A~ZKUNNR_IC = P_INCOMP
    AND A~ZKUNNR IN S_ZKUNNR
    AND A~VTWEG IN S_VTWEG.

  LOOP AT GT_LIST.
    GT_LIST-ICON = ICON_YELLOW_LIGHT.
    GT_LIST-FLAG = 'X' .
    MODIFY GT_LIST TRANSPORTING ICON FLAG.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA .

  DATA : LT_SAVE      LIKE TABLE OF ZSDT0070 WITH HEADER LINE,
         LV_ERR,
         LV_MSG(100),
         LV_CHECK,
         LV_ZKUNNR_IC LIKE ZSDT0020-ZKUNNR_IC.


  PERFORM POPUP_MSG USING 'Data save'
                          'Do you want to save? For the same Intercompany and Dchl and Customer and SKU and start date, it is overwritten.'
                           LV_CHECK.

  CHECK LV_CHECK EQ '1'.


  CLEAR LV_CHECK.
  LOOP AT GT_LIST.
    CLEAR : GT_LIST-ICON, GT_LIST-MESSAGE.
    PERFORM REQUIRED_FIELD_CHECK USING : GT_LIST-VTWEG   TEXT-F05,
                                         GT_LIST-ZKUNNR  TEXT-F06,
                                         GT_LIST-MATNR   TEXT-F08,
                                         GT_LIST-KBETR   TEXT-F10,
                                         GT_LIST-KONWA   TEXT-F11,
                                         GT_LIST-KPEIN   TEXT-F12,
                                         GT_LIST-KMEIN   TEXT-F03,
                                         GT_LIST-DATAB   TEXT-F13,
                                         GT_LIST-DATBI   TEXT-F14.

    IF GT_LIST-ICON = ICON_RED_LIGHT.
      MODIFY GT_LIST TRANSPORTING ICON MESSAGE.
      LV_CHECK = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF LV_CHECK = 'X'.
    MESSAGE S000 WITH TEXT-E03 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE GT_LIST INDEX 1.
  SELECT SINGLE ZKUNNR_IC INTO LV_ZKUNNR_IC
  FROM ZSDT0070
  WHERE ZKUNNR_IC = P_INCOMP
    AND VTWEG     = GT_LIST-VTWEG
    AND ZKUNNR    = GT_LIST-ZKUNNR
    AND MATNR     = GT_LIST-MATNR
    AND DATAB     = GT_LIST-DATAB.
  IF SY-SUBRC = 0.
    DELETE FROM ZSDT0070   WHERE ZKUNNR_IC = P_INCOMP
                             AND VTWEG     = GT_LIST-VTWEG
                             AND ZKUNNR    = GT_LIST-ZKUNNR
                             AND MATNR     = GT_LIST-MATNR
                             AND DATAB     = GT_LIST-DATAB.
    COMMIT WORK.
  ELSE.
    DELETE FROM ZSDT0070   WHERE ZKUNNR_IC = P_INCOMP
                             AND DATAB     = GT_LIST-DATAB.
    COMMIT WORK.
  ENDIF.

  _CLEAR LT_SAVE.
  LOOP AT GT_LIST.
    MOVE-CORRESPONDING GT_LIST TO LT_SAVE.
    LT_SAVE-ZKUNNR_IC = P_INCOMP.
    LT_SAVE-ERNAM = SY-UNAME.
    LT_SAVE-ERDAT = SY-DATUM.
    LT_SAVE-ERZET = SY-UZEIT.
    APPEND LT_SAVE. CLEAR LT_SAVE.
  ENDLOOP.
  CHECK LT_SAVE[] IS NOT INITIAL.

  MODIFY ZSDT0070 FROM TABLE LT_SAVE.
  IF SY-SUBRC = 0.
    COMMIT WORK.
    MESSAGE S010.
    GT_LIST-ICON = ICON_GREEN_LIGHT.
    GT_LIST-MESSAGE = SPACE.
    MODIFY GT_LIST TRANSPORTING ICON MESSAGE WHERE VTWEG     = GT_LIST-VTWEG
                                               AND ZKUNNR    = GT_LIST-ZKUNNR
                                               AND MATNR     = GT_LIST-MATNR
                                               AND DATAB     = GT_LIST-DATAB.

  ELSE.
    ROLLBACK WORK.
    MESSAGE S011  DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT
*&---------------------------------------------------------------------*
FORM CHECK_INPUT .

  IF P_INCOMP IS INITIAL.
    MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CELL
*&---------------------------------------------------------------------*
FORM GET_CELL   USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                      PV_ROW
                      PV_CELL
                      PV_VALUE.

  CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
    EXPORTING
      I_ROW_ID    = PV_ROW
      I_FIELDNAME = PV_CELL
    IMPORTING
      E_VALUE     = PV_VALUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODI_CELL
*&---------------------------------------------------------------------*
FORM MODI_CELL USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                     PV_ROW
                     PV_CELL
                     PV_VALUE.

  CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = PV_ROW
      I_FIELDNAME = PV_CELL
      I_VALUE     = PV_VALUE.

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
   WHERE OBJID EQ 'ZSDR0012'.

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
*& Form TEMPLATE_BUTTON
*&---------------------------------------------------------------------*
FORM TEMPLATE_BUTTON .

  DATA : L_DYNTXT     TYPE SMP_DYNTXT.

  CLEAR : L_DYNTXT.
  MOVE : TEXT-002     TO L_DYNTXT-TEXT,
         ICON_XLS     TO L_DYNTXT-ICON_ID,
         TEXT-002     TO L_DYNTXT-ICON_TEXT,
         TEXT-002     TO L_DYNTXT-QUICKINFO,
         'C:\TEMP'    TO L_DYNTXT-PATH.      "C:\TEMP

  SSCRFIELDS-FUNCTXT_01 = L_DYNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEARCH_HELP_INTERCOMPANY
*&---------------------------------------------------------------------*
FORM SEARCH_HELP_INTERCOMPANY USING PV_FIELD.

  DATA : BEGIN OF LT_LINE OCCURS 0,
           ZKUNNR_IC     LIKE ZSDT0040-ZKUNNR_IC,
           ZKUNNR_IC_TXT LIKE BUT000-NAME_ORG1,
         END OF LT_LINE.

  _CLEAR LT_LINE.

  LT_LINE[] = GT_0040[].

  CLEAR : GT_RETURNTAB[], GT_RETURNTAB.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZKUNNR_IC'
      DYNPROFIELD     = PV_FIELD
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      WINDOW_TITLE    = 'Intercompany'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = LT_LINE
      RETURN_TAB      = GT_RETURNTAB
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCEL_FILE_PATH
*&---------------------------------------------------------------------*
FORM EXCEL_FILE_PATH .

  CLEAR GV_PATH.
  GV_PATH = 'C:\'.
  CALL SCREEN '9200' STARTING AT 10 5.

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
*&---------------------------------------------------------------------*
*& Form CHECK_UPLOAD_DATA
*&---------------------------------------------------------------------*
FORM CHECK_UPLOAD_DATA .

  DATA : LT_DUP LIKE TABLE OF GT_LIST WITH HEADER LINE.
  DATA : LT_MAKT     LIKE TABLE OF MAKT WITH HEADER LINE,
         LT_MAKT_KEY LIKE TABLE OF MAKT WITH HEADER LINE.
  DATA : LV_CNT       TYPE I,
         LV_KBETR     TYPE C LENGTH 15,
         LV_KONWA     LIKE TCURC-WAERS,
         LV_KONWA_CON TYPE C LENGTH 15.
  _CLEAR : LT_MAKT_KEY, LT_MAKT.
  MOVE-CORRESPONDING GT_UPLOAD[] TO LT_MAKT_KEY[].
  SORT LT_MAKT_KEY BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_MAKT_KEY COMPARING MATNR.

  IF LT_MAKT_KEY[] IS NOT INITIAL.
    SELECT A~MATNR
           B~MAKTX
    INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
    FROM MARA AS A INNER JOIN MAKT AS B
                                   ON A~MATNR = B~MATNR
    FOR ALL ENTRIES IN LT_MAKT_KEY
    WHERE A~MATNR = LT_MAKT_KEY-MATNR.
    SORT LT_MAKT BY MATNR.
  ENDIF.

  LOOP AT GT_UPLOAD.
    CLEAR GT_LIST.
    MOVE-CORRESPONDING GT_UPLOAD TO GT_LIST.
    PERFORM ALPHA_INPUT USING GT_LIST-ZKUNNR.


    PERFORM REQUIRED_FIELD_CHECK USING : GT_LIST-VTWEG   TEXT-F05,
                                         GT_LIST-ZKUNNR  TEXT-F06,
                                         GT_LIST-MATNR   TEXT-F08,
                                         GT_LIST-KBETR   TEXT-F10,
                                         GT_LIST-KONWA   TEXT-F11,
                                         GT_LIST-KPEIN   TEXT-F12,
                                         GT_LIST-KMEIN   TEXT-F03,
                                         GT_LIST-DATAB   TEXT-F13,
                                         GT_LIST-DATBI   TEXT-F14.

    IF GT_LIST-ICON IS INITIAL.
      CLEAR : GT_TVTWT.
      READ TABLE GT_TVTWT WITH KEY VTWEG = GT_LIST-VTWEG.
      IF SY-SUBRC = 0.
      ELSE.
        PERFORM ERROR_MSG USING '(Distribution Channel)'.
      ENDIF.

      CLEAR : GT_0041.
      PERFORM ALPHA_INPUT USING GT_LIST-ZKUNNR.
      READ TABLE GT_0041 WITH KEY ZKUNNR = GT_LIST-ZKUNNR.
      IF SY-SUBRC = 0.
        GT_LIST-ZKUNNR_TXT = GT_0041-ZKUNNR_TXT.
      ELSE.
        PERFORM ERROR_MSG USING '(Customer)'.
      ENDIF.

      IF GT_LIST-MATNR IS NOT INITIAL.
        CLEAR LT_MAKT.
        READ TABLE LT_MAKT WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-MATNR_TXT = LT_MAKT-MAKTX.
        ELSE.
          PERFORM ERROR_MSG USING '(SKU)'.
        ENDIF.
      ENDIF.

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

    ENDIF.

    IF GT_LIST-ICON IS INITIAL.
      GT_LIST-ICON = ICON_YELLOW_LIGHT.
    ENDIF.

    GT_LIST-UP = 'X'.

    APPEND GT_LIST.
  ENDLOOP.

  _CLEAR LT_DUP.
  LT_DUP[] = GT_LIST[].


  LOOP AT GT_LIST WHERE UP = 'X'.
    CLEAR LV_CNT.
    LOOP AT LT_DUP WHERE VTWEG    = GT_LIST-VTWEG
                     AND ZKUNNR     = GT_LIST-ZKUNNR
                     AND MATNR      = GT_LIST-MATNR
                     AND DATAB      = GT_LIST-DATAB.
      ADD 1 TO LV_CNT.
    ENDLOOP.

    IF LV_CNT > 1.
      IF GT_LIST-MESSAGE IS INITIAL.
        GT_LIST-MESSAGE = TEXT-E02.
        GT_LIST-ICON = ICON_RED_LIGHT.
        MODIFY GT_LIST TRANSPORTING MESSAGE ICON.
      ELSE.
        GT_LIST-MESSAGE = GT_LIST-MESSAGE && '/' && TEXT-E02.
        MODIFY GT_LIST TRANSPORTING MESSAGE.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REQUIRED_FIELD_CHECK
*&---------------------------------------------------------------------*
FORM REQUIRED_FIELD_CHECK USING  PV_DATA PV_TEXT.

  CHECK PV_DATA IS INITIAL.

  IF GT_LIST-MESSAGE IS INITIAL.
    GT_LIST-MESSAGE = '(' && PV_TEXT && ')' && TEXT-E03.
    GT_LIST-ICON = ICON_RED_LIGHT.
  ELSE.
    GT_LIST-MESSAGE = GT_LIST-MESSAGE && '/' && '(' && PV_TEXT && ')'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ERROR_MSG
*&---------------------------------------------------------------------*
FORM ERROR_MSG USING PV_FIELD.

  IF GT_LIST-MESSAGE IS INITIAL.
    GT_LIST-MESSAGE = TEXT-E04 && PV_FIELD.
    GT_LIST-ICON = ICON_RED_LIGHT.
  ELSE.
    GT_LIST-MESSAGE = GT_LIST-MESSAGE && '/' && '(' && PV_FIELD && ')'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEARCH_HELP_SHIP-TOPARTY
*&---------------------------------------------------------------------*
FORM SEARCH_HELP_SHIP-TOPARTY  USING PV_FIELD.

  DATA : LT_LINE LIKE TABLE OF GT_0041 WITH HEADER LINE.

  _CLEAR LT_LINE.

  LT_LINE[] = GT_0041[].

  CLEAR : GT_RETURNTAB[], GT_RETURNTAB.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZKUNNR'
      DYNPROFIELD     = PV_FIELD
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      WINDOW_TITLE    = 'SHIP-TO PARTY'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = LT_LINE
      RETURN_TAB      = GT_RETURNTAB
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_OUTPUT
*&---------------------------------------------------------------------*
FORM ALPHA_OUTPUT  USING    P_DATA.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = P_DATA
    IMPORTING
      OUTPUT = P_DATA.

ENDFORM.
