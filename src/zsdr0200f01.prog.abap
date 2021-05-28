*&---------------------------------------------------------------------*
*& Include          ZSDR0200F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT.

  P_VKORG = '1001'.

ENDFORM. " SET_INIT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA : LS_MOD_CELLS     TYPE LVC_S_MODI,
         LV_GETVALUE(30),
         LV_GETVALUE2(30),
         LV_GETVALUE3(30),
         LV_MODIVALUE(30).

  CLEAR: LS_MOD_CELLS.

  LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'KBETR_C'.
        CLEAR : LV_GETVALUE, LV_GETVALUE2, LV_GETVALUE3.
        PERFORM GET_CELL  USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'KBETR_C'  LV_GETVALUE.
        PERFORM GET_CELL  USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'KBETR_HQ' LV_GETVALUE2.
        PERFORM GET_CELL  USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'KBETR_US' LV_GETVALUE3.

        IF LV_GETVALUE3 IS INITIAL.
          IF LV_GETVALUE EQ LV_GETVALUE2.
            PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ICON' ICON_LED_INACTIVE.
          ELSE.
            PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ICON' ICON_LED_YELLOW.
          ENDIF.
        ELSE.
          IF LV_GETVALUE NE LV_GETVALUE2 OR LV_GETVALUE NE LV_GETVALUE3.
            PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ICON' ICON_LED_YELLOW.
          ELSE.
            PERFORM MODI_CELL USING PR_DATA_CHANGED LS_MOD_CELLS-ROW_ID 'ICON' ICON_LED_INACTIVE.
          ENDIF.
        ENDIF.
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
*& Form SET_SELECTION
*&---------------------------------------------------------------------*
FORM SET_SELECTION .

  CASE 'X'.
    WHEN P_DISP.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 EQ 'M2'.
          SCREEN-ACTIVE = '0'.
          SCREEN-INPUT  = '0'.
        ENDIF.
        IF SCREEN-NAME EQ 'P_VKORG'.
          SCREEN-INPUT  = '0'.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.

    WHEN P_UPLOAD.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 EQ 'M1'.
          SCREEN-ACTIVE = '0'.
          SCREEN-INPUT  = '0'.
        ENDIF.
        IF SCREEN-NAME EQ 'P_VKORG'.
          SCREEN-INPUT  = '0'.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.

ENDFORM.
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

  L_TITLE = TEXT-001.

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
      I_END_COL               = 3
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DATA .

  DATA : BEGIN OF LT_CUST OCCURS 0,
           BSTNK LIKE VBAK-BSTNK,
         END OF LT_CUST.

  SORT GT_UPLOAD BY BSTNK MATNR.

  MOVE-CORRESPONDING GT_UPLOAD[] TO LT_CUST[].
  SORT LT_CUST.
  DELETE ADJACENT DUPLICATES FROM LT_CUST COMPARING ALL FIELDS.

  _CLEAR S_BSTNK.
  LOOP AT LT_CUST.
    _RANGE S_BSTNK 'I' 'EQ' LT_CUST-BSTNK ''.
  ENDLOOP.

  DATA: LV_EBELP LIKE EKPO-EBELP.

  SELECT A~EBELN, A~EBELP, A~NETPR, A~LOEKZ, A~ELIKZ,
         B~BUZEI, B~MATNR,
         C~BELNR, C~GJAHR, C~STBLG,
         D~MBLNR, D~MJAHR, D~ZEILE, D~SMBLN, D~SJAHR, D~SMBLP
    INTO TABLE @DATA(LT_EKPO)
    FROM EKPO AS A LEFT OUTER JOIN RSEG AS B ON A~EBELN = B~EBELN
                                            AND A~EBELP = B~EBELP
                   LEFT OUTER JOIN RBKP AS C ON B~BELNR = C~BELNR
                                            AND B~GJAHR = C~GJAHR
                                            AND C~STBLG = @SPACE
                   LEFT OUTER JOIN MSEG AS D ON A~EBELN = D~EBELN
                                            AND A~EBELP = D~EBELP
    WHERE A~EBELN IN @S_BSTNK.

  DELETE LT_EKPO WHERE STBLG IS NOT INITIAL.

  DATA : LT_CANC LIKE LT_EKPO WITH HEADER LINE.

  SORT LT_EKPO BY MJAHR MBLNR ZEILE.

  _CLEAR LT_CANC.
  LOOP AT LT_EKPO INTO DATA(LS_CANC) WHERE SMBLN IS NOT INITIAL.
    MOVE-CORRESPONDING LS_CANC TO LT_CANC.
    APPEND LT_CANC. CLEAR LT_CANC.
    DELETE LT_EKPO.
  ENDLOOP.

  LOOP AT LT_CANC.
    READ TABLE LT_EKPO WITH KEY MJAHR = LT_CANC-SJAHR
                                MBLNR = LT_CANC-SMBLN
                                ZEILE = LT_CANC-SMBLP BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      DELETE LT_EKPO INDEX SY-INDEX.
    ENDIF.
  ENDLOOP.

  SORT LT_EKPO BY EBELN MATNR.

  SELECT A~VBELN, A~BSTNK,
         B~POSNR, B~NETPR, B~POSEX, B~WAERK, B~ABGRU, B~MATNR,
         C~VBELN AS VBELN_VL,
         C~POSNR AS POSNR_VL,
         D~MAKTX AS MATNR_TXT
    INTO TABLE @DATA(LT_VBAP)
    FROM VBAK AS A INNER JOIN VBAP AS B ON A~VBELN = B~VBELN
              LEFT OUTER JOIN LIPS AS C ON B~VBELN = C~VGBEL
                                       AND B~POSNR = C~VGPOS
              LEFT OUTER JOIN MAKT AS D ON B~MATNR = D~MATNR
                                       AND D~SPRAS = @SY-LANGU
    WHERE A~BSTNK IN @S_BSTNK
      AND A~VKORG EQ @P_VKORG.

  SORT LT_VBAP BY BSTNK MATNR.

  _CLEAR GT_LIST.

  LOOP AT GT_UPLOAD.

    PERFORM CHECK_FIELD USING : GT_UPLOAD-BSTNK 'Customer PO',
                                GT_UPLOAD-MATNR 'SKU',
                                GT_UPLOAD-KBETR 'Change Price'.
    IF GT_LIST-ICON = ICON_RED_LIGHT.
    ELSE.

      READ TABLE LT_VBAP INTO DATA(LS_VBAP) WITH KEY BSTNK = GT_UPLOAD-BSTNK
                                                     MATNR = GT_UPLOAD-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING LS_VBAP TO GT_LIST.
        GT_LIST-KBETR_C  = GT_UPLOAD-KBETR.
        GT_LIST-KBETR_HQ = LS_VBAP-NETPR.
        GT_LIST-KONWA    = LS_VBAP-WAERK.

        CLEAR LV_EBELP.
        PERFORM ALPHA_OUTPUT USING LS_VBAP-POSEX.
        LV_EBELP = LS_VBAP-POSEX.

        READ TABLE LT_EKPO INTO DATA(LS_EKPO) WITH KEY EBELN = GT_UPLOAD-BSTNK
                                                       EBELP = LV_EBELP BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-EBELN    = LS_EKPO-EBELN.
          GT_LIST-EBELP    = LS_EKPO-EBELP.
          GT_LIST-KBETR_US = LS_EKPO-NETPR.
          GT_LIST-MBLNR    = LS_EKPO-MBLNR.
          GT_LIST-ELIKZ    = LS_EKPO-ELIKZ.
          GT_LIST-LOEKZ    = LS_EKPO-LOEKZ.

          IF GT_LIST-KBETR_HQ EQ GT_LIST-KBETR_US.
            GT_LIST-ICON = ICON_LED_INACTIVE.
          ELSE.
            GT_LIST-ICON = ICON_LED_YELLOW.
          ENDIF.
        ELSE.
          GT_LIST-EBELN = LS_VBAP-BSTNK.
          GT_LIST-ICON  = ICON_LED_INACTIVE.
        ENDIF.

        IF GT_LIST-BELNR IS NOT INITIAL.
          GT_LIST-ICON = ICON_LED_INACTIVE.
          GT_LIST-DISP = 'X'.
          GT_LIST-MESSAGE = TEXT-E01.
        ENDIF.

        IF GT_LIST-MBLNR IS NOT INITIAL.
          GT_LIST-ICON = ICON_LED_INACTIVE.
          GT_LIST-DISP = 'X'.
          GT_LIST-MESSAGE = TEXT-E02.
        ENDIF.
        IF GT_LIST-VBELN_VL IS NOT INITIAL.
          GT_LIST-ICON = ICON_LED_INACTIVE.
          GT_LIST-DISP = 'X'.
          GT_LIST-MESSAGE = TEXT-E03.
        ENDIF.
        IF GT_LIST-LOEKZ IS NOT INITIAL.
          GT_LIST-ICON = ICON_LED_INACTIVE.
          GT_LIST-DISP = 'X'.
          GT_LIST-MESSAGE = TEXT-E04.
        ENDIF.
        IF GT_LIST-ELIKZ IS NOT INITIAL.
          GT_LIST-ICON = ICON_LED_INACTIVE.
          GT_LIST-DISP = 'X'.
          GT_LIST-MESSAGE = TEXT-E05.
        ENDIF.
        IF GT_LIST-ABGRU IS NOT INITIAL.
          GT_LIST-ICON = ICON_LED_INACTIVE.
          GT_LIST-DISP = 'X'.
          GT_LIST-MESSAGE = TEXT-E06.
        ENDIF.

      ENDIF.
    ENDIF.
    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  SORT GT_LIST BY VBELN POSNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPLY_PRICE
*&---------------------------------------------------------------------*
FORM APPLY_PRICE .
  DATA : LT_SO_KEY LIKE VBAP-VBELN OCCURS 0  WITH HEADER LINE,
         LT_PO_KEY LIKE EKPO-EBELN OCCURS 0  WITH HEADER LINE.
  DATA : LV_CHECK.

  CLEAR: GT_ROWS[], GS_ROWS.
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM POPUP_MSG USING TEXT-P01
                          TEXT-P02
                          LV_CHECK.

  CHECK LV_CHECK = '1'.

  _CLEAR : LT_SO_KEY, LT_PO_KEY.

  LOOP AT GT_ROWS INTO GS_ROWS.
    READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
    CHECK GT_LIST-ICON = ICON_LED_YELLOW.

    IF GT_LIST-KBETR_C NE GT_LIST-KBETR_HQ.
      LT_SO_KEY = GT_LIST-VBELN.
      COLLECT LT_SO_KEY.
      GT_LIST-APPLYSO = 'X'.
    ENDIF.
    IF GT_LIST-KBETR_C NE GT_LIST-KBETR_US.
      LT_PO_KEY = GT_LIST-EBELN.
      COLLECT LT_PO_KEY.
      GT_LIST-APPLYPO = 'X'.
    ENDIF.

    MODIFY GT_LIST INDEX GS_ROWS-INDEX TRANSPORTING APPLYSO APPLYPO.

  ENDLOOP.

  SORT LT_SO_KEY.
  SORT LT_PO_KEY .

  IF LT_SO_KEY[] IS INITIAL AND LT_PO_KEY[] IS INITIAL.
    MESSAGE S021 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT LT_SO_KEY.
    PERFORM CLEAR_PARAMETERS.
    GV_SALESDOCUMENT = GT_LIST-VBELN.
    GS_HEADER_INX-UPDATEFLAG = 'U'.

    LOOP AT GT_LIST WHERE VBELN = LT_SO_KEY
                      AND APPLYSO = 'X'.
      PERFORM ITEM_DATA_UPDATE.
    ENDLOOP.
    PERFORM CALL_BAPI_CHANGE USING LT_SO_KEY.
  ENDLOOP.

  LOOP AT LT_PO_KEY.
    PERFORM CLEAR_PARAMETERS_PO.
    GS_POHEADER-PO_NUMBER = LT_PO_KEY.
    GS_POHEADERX-PO_NUMBER = 'X'.

    LOOP AT GT_LIST WHERE EBELN = LT_PO_KEY
                      AND APPLYPO = 'X'.
      PERFORM PO_ITEM_DATA_UPDATE.
    ENDLOOP.
    PERFORM CALL_BAPI_CHANGE_PO USING LT_PO_KEY.
  ENDLOOP.


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
    WHEN 'VBELN'.
      CLEAR : GT_LIST.
      READ TABLE GT_LIST INDEX PE_ROW.
      CHECK SY-SUBRC EQ 0.
      SET PARAMETER ID 'AUN' FIELD GT_LIST-VBELN.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    WHEN 'EBELN'.
      CLEAR : GT_LIST.
      READ TABLE GT_LIST INDEX PE_ROW.
      CHECK SY-SUBRC EQ 0.
      SET PARAMETER ID 'BES' FIELD GT_LIST-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HANDLE_TOP_OF_PAGE  USING PE_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT.

*  DATA : LV_TEXT(255) TYPE C.

*  CONCATENATE P_AUART '('  LV_TEXT  ')' "Order type
*  INTO LV_TEXT SEPARATED BY SPACE.
*  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
*    EXPORTING
*      TEXT         = LV_TEXT
*      SAP_FONTSIZE = 'MEDIUM'.
*
*  CALL METHOD PE_DYNDOC_ID->NEW_LINE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form TEMPLATE_BUTTON
*&---------------------------------------------------------------------*
FORM TEMPLATE_BUTTON .

  DATA : L_DYNTXT     TYPE SMP_DYNTXT.
  CLEAR : L_DYNTXT.
  MOVE : 'Upload Template'     TO L_DYNTXT-TEXT,
         ICON_XLS     TO L_DYNTXT-ICON_ID,
         'Upload Template'     TO L_DYNTXT-ICON_TEXT,
         'Upload Template'     TO L_DYNTXT-QUICKINFO,
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
   WHERE OBJID EQ 'ZSDR0200'.

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
FORM AUTHORIZATION_CHECK.

  DATA : LS_RETURN LIKE BAPIRETURN1,
         LV_VKORG  TYPE VKORG.

  CLEAR LV_VKORG.
  LV_VKORG = P_VKORG.

  CALL FUNCTION 'ZBC_AUTHORIZATION_CHECK'
    EXPORTING
      I_VKORG   = LV_VKORG
    IMPORTING
      ES_RETURN = LS_RETURN.

  IF LS_RETURN-TYPE = 'E'.
    MESSAGE E000 WITH LS_RETURN-MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_AMOUNT
*&---------------------------------------------------------------------*
FORM CONV_AMOUNT  USING    PV_KBETR
                   CHANGING PV_KONWA_CON.


  DATA : LV_KBETR     TYPE C LENGTH 15,
         LV_KONWA_CON TYPE C LENGTH 15.

  CLEAR : LV_KBETR, LV_KONWA_CON.

  LV_KBETR = PV_KBETR.
  CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
    EXPORTING
      CURRENCY    = GV_WAERS
      IDOC_AMOUNT = LV_KBETR
    IMPORTING
      SAP_AMOUNT  = LV_KONWA_CON.

  CONDENSE LV_KONWA_CON.
  PV_KONWA_CON = LV_KONWA_CON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT
*&---------------------------------------------------------------------*
FORM CHECK_INPUT .

  CASE 'X'.
    WHEN P_UPLOAD.
      IF P_VKORG IS INITIAL.
        MESSAGE S018 WITH 'Sales Org.' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF P_FILE IS INITIAL.
        MESSAGE S018 WITH 'File Path' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.

    WHEN P_DISP.
      IF P_VKORG IS INITIAL.
        MESSAGE S018 WITH 'Sales Org.' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

  DATA: LV_EBELP LIKE EKPO-EBELP.

  SELECT A~EBELN, A~EBELP, A~NETPR, A~LOEKZ, A~ELIKZ,
         B~BUZEI,
         C~BELNR, C~GJAHR, C~STBLG,
         D~MBLNR, D~MJAHR, D~ZEILE, D~SMBLN, D~SJAHR, D~SMBLP
    INTO TABLE @DATA(LT_EKPO)
    FROM EKPO AS A LEFT OUTER JOIN RSEG AS B ON A~EBELN = B~EBELN
                                            AND A~EBELP = B~EBELP
                   LEFT OUTER JOIN RBKP AS C ON B~BELNR = C~BELNR
                                            AND B~GJAHR = C~GJAHR
                                            AND C~STBLG = @SPACE
                   LEFT OUTER JOIN MSEG AS D ON A~EBELN = D~EBELN
                                            AND A~EBELP = D~EBELP
    WHERE A~EBELN IN @S_BSTNK.

  DELETE LT_EKPO WHERE STBLG IS NOT INITIAL.

  DATA : LT_CANC LIKE LT_EKPO WITH HEADER LINE.

  SORT LT_EKPO BY MJAHR MBLNR ZEILE.

  _CLEAR LT_CANC.
  LOOP AT LT_EKPO INTO DATA(LS_CANC) WHERE SMBLN IS NOT INITIAL.
    MOVE-CORRESPONDING LS_CANC TO LT_CANC.
    APPEND LT_CANC. CLEAR LT_CANC.
    DELETE LT_EKPO.
  ENDLOOP.

  LOOP AT LT_CANC.
    READ TABLE LT_EKPO WITH KEY MJAHR = LT_CANC-SJAHR
                                MBLNR = LT_CANC-SMBLN
                                ZEILE = LT_CANC-SMBLP BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      DELETE LT_EKPO INDEX SY-INDEX.
    ENDIF.
  ENDLOOP.

  SORT LT_EKPO BY EBELN EBELP.

  SELECT A~VBELN, A~BSTNK,
         B~POSNR, B~NETPR, B~POSEX, B~WAERK, B~ABGRU, B~MATNR,
         C~VBELN AS VBELN_VL,
         C~POSNR AS POSNR_VL,
         D~MAKTX AS MATNR_TXT
    INTO TABLE @DATA(LT_VBAP)
    FROM VBAK AS A INNER JOIN VBAP AS B ON A~VBELN = B~VBELN
              LEFT OUTER JOIN LIPS AS C ON B~VBELN = C~VGBEL
                                       AND B~POSNR = C~VGPOS
              LEFT OUTER JOIN MAKT AS D ON B~MATNR = D~MATNR
                                       AND D~SPRAS = @SY-LANGU
    WHERE A~BSTNK IN @S_BSTNK
      AND A~VKORG EQ @P_VKORG.

  SORT LT_VBAP BY VBELN POSNR.

  _CLEAR GT_LIST.

  LOOP AT LT_VBAP INTO DATA(LS_VBAP).
    MOVE-CORRESPONDING LS_VBAP TO GT_LIST.
    GT_LIST-KBETR_C  = LS_VBAP-NETPR.
    GT_LIST-KBETR_HQ = LS_VBAP-NETPR.
    GT_LIST-KONWA    = LS_VBAP-WAERK.

    CLEAR LV_EBELP.
    PERFORM ALPHA_OUTPUT USING LS_VBAP-POSEX.
    LV_EBELP = LS_VBAP-POSEX.

    READ TABLE LT_EKPO INTO DATA(LS_EKPO) WITH KEY EBELN = LS_VBAP-BSTNK
                                                   EBELP = LV_EBELP BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-EBELN    = LS_EKPO-EBELN.
      GT_LIST-EBELP    = LS_EKPO-EBELP.
      GT_LIST-KBETR_US = LS_EKPO-NETPR.
      GT_LIST-MBLNR    = LS_EKPO-MBLNR.
      GT_LIST-ELIKZ    = LS_EKPO-ELIKZ.
      GT_LIST-LOEKZ    = LS_EKPO-LOEKZ.

      IF GT_LIST-KBETR_HQ EQ GT_LIST-KBETR_US.
        GT_LIST-ICON = ICON_LED_INACTIVE.
      ELSE.
        GT_LIST-ICON = ICON_LED_YELLOW.
      ENDIF.
    ELSE.
      GT_LIST-EBELN = LS_VBAP-BSTNK.
      GT_LIST-ICON  = ICON_LED_INACTIVE.
    ENDIF.

    IF GT_LIST-BELNR IS NOT INITIAL.
      GT_LIST-ICON = ICON_LED_INACTIVE.
      GT_LIST-DISP = 'X'.
      GT_LIST-MESSAGE = TEXT-E01.
    ENDIF.

    IF GT_LIST-MBLNR IS NOT INITIAL.
      GT_LIST-ICON = ICON_LED_INACTIVE.
      GT_LIST-DISP = 'X'.
      GT_LIST-MESSAGE = TEXT-E02.
    ENDIF.
    IF GT_LIST-VBELN_VL IS NOT INITIAL.
      GT_LIST-ICON = ICON_LED_INACTIVE.
      GT_LIST-DISP = 'X'.
      GT_LIST-MESSAGE = TEXT-E03.
    ENDIF.
    IF GT_LIST-LOEKZ IS NOT INITIAL.
      GT_LIST-ICON = ICON_LED_INACTIVE.
      GT_LIST-DISP = 'X'.
      GT_LIST-MESSAGE = TEXT-E04.
    ENDIF.
    IF GT_LIST-ELIKZ IS NOT INITIAL.
      GT_LIST-ICON = ICON_LED_INACTIVE.
      GT_LIST-DISP = 'X'.
      GT_LIST-MESSAGE = TEXT-E05.
    ENDIF.
    IF GT_LIST-ABGRU IS NOT INITIAL.
      GT_LIST-ICON = ICON_LED_INACTIVE.
      GT_LIST-DISP = 'X'.
      GT_LIST-MESSAGE = TEXT-E06.
    ENDIF.

    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  SORT GT_LIST BY VBELN POSNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_OUTPUT
*&---------------------------------------------------------------------*
FORM ALPHA_OUTPUT   USING    P_DATA.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = P_DATA
    IMPORTING
      OUTPUT = P_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CELL
*&---------------------------------------------------------------------*
FORM GET_CELL  USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
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
FORM MODI_CELL  USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
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
*& Form CELL_STYLE
*&---------------------------------------------------------------------*
FORM CELL_STYLE .

  LOOP AT GT_LIST.
    CLEAR GT_STYL.
    IF P_UPLOAD = 'X'.
      PERFORM SET_STYLE USING 'KBETR_C' 'DISP'.
    ELSE.
      IF GT_LIST-DISP IS INITIAL.
        PERFORM SET_STYLE USING 'KBETR_C' 'EDIT'.
      ELSE.
        PERFORM SET_STYLE USING 'KBETR_C' 'DISP'.
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
FORM SET_STYLE   USING P_NAME
                       P_MODE.

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
*& Form CLEAR_PARAMETERS
*&---------------------------------------------------------------------*
FORM CLEAR_PARAMETERS .

* "---- clear ---
  CLEAR: GS_ORDER_HEADER_IN,
         GS_ORDER_HEADER_INX,
         GS_HEADER_INX,
         GT_ORDER_ITEMS_IN,       GT_ORDER_ITEMS_IN[],
         GT_ORDER_ITEMS_INX,      GT_ORDER_ITEMS_INX[],
         GT_ORDER_SCHEDULES_IN,   GT_ORDER_SCHEDULES_IN[],
         GT_ORDER_SCHEDULES_INX,  GT_ORDER_SCHEDULES_INX[],
         GT_ORDER_PARTNERS,       GT_ORDER_PARTNERS[],
         GT_ORDER_CONDITIONS_IN,  GT_ORDER_CONDITIONS_IN[],
         GT_ORDER_CONDITIONS_INX, GT_ORDER_CONDITIONS_INX[],
         GT_RETURN,               GT_RETURN[],
         GT_EXTENSION,            GT_EXTENSION[],
         GT_TEXT,                 GT_TEXT[],
         GT_CHANGE_PARTNERS,      GT_CHANGE_PARTNERS[],
         GV_SALESDOCUMENT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ITEM_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM ITEM_DATA_UPDATE .

  GT_ORDER_ITEMS_IN-ITM_NUMBER  = GT_LIST-POSNR.
  GT_ORDER_ITEMS_INX-ITM_NUMBER = GT_LIST-POSNR.

  GT_ORDER_ITEMS_INX-UPDATEFLAG = 'U'.

  APPEND GT_ORDER_ITEMS_IN.      CLEAR GT_ORDER_ITEMS_IN.
  APPEND GT_ORDER_ITEMS_INX.     CLEAR GT_ORDER_ITEMS_INX.

  GT_ORDER_CONDITIONS_IN-ITM_NUMBER  = GT_LIST-POSNR.
  GT_ORDER_CONDITIONS_INX-ITM_NUMBER = 'X'.

  GT_ORDER_CONDITIONS_INX-UPDATEFLAG = 'U'.
  IF P_VKORG = '1002'.
    GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR01'.
  ELSE.
    GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR00'.
  ENDIF.
  GT_ORDER_CONDITIONS_INX-COND_TYPE  = 'X'.

  GT_ORDER_CONDITIONS_IN-COND_VALUE  = GT_LIST-KBETR_C.
  GT_ORDER_CONDITIONS_INX-COND_VALUE = 'X'.       "조건 금액

  GT_ORDER_CONDITIONS_IN-CURRENCY    = GT_LIST-KONWA. "통화 단위.
  GT_ORDER_CONDITIONS_INX-CURRENCY   = 'X'.       "통화 단위.

  APPEND GT_ORDER_CONDITIONS_IN.  CLEAR GT_ORDER_CONDITIONS_IN.
  APPEND GT_ORDER_CONDITIONS_INX. CLEAR GT_ORDER_CONDITIONS_INX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_BAPI_CHANGE
*&---------------------------------------------------------------------*
FORM CALL_BAPI_CHANGE  USING  PV_VBELN.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      SALESDOCUMENT    = GV_SALESDOCUMENT
      ORDER_HEADER_INX = GS_HEADER_INX
    TABLES
      RETURN           = GT_RETURN
      ORDER_ITEM_IN    = GT_ORDER_ITEMS_IN
      ORDER_ITEM_INX   = GT_ORDER_ITEMS_INX
      SCHEDULE_LINES   = GT_ORDER_SCHEDULES_IN
      SCHEDULE_LINESX  = GT_ORDER_SCHEDULES_INX
      PARTNERS         = GT_ORDER_PARTNERS
      PARTNERCHANGES   = GT_CHANGE_PARTNERS
      CONDITIONS_IN    = GT_ORDER_CONDITIONS_IN
      CONDITIONS_INX   = GT_ORDER_CONDITIONS_INX.

  READ TABLE GT_RETURN WITH KEY TYPE = 'S'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    GT_LIST-ICON = ICON_LED_GREEN.
    CLEAR GT_LIST-APPLYSO.
    MODIFY GT_LIST TRANSPORTING ICON APPLYSO WHERE VBELN = PV_VBELN AND APPLYSO EQ 'X'.

    UPDATE VBAK SET BSTZD = 'X'
              WHERE VBELN = GT_LIST-VBELN.
    IF SY-SUBRC = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    READ TABLE GT_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      GT_LIST-MESSAGE = GT_RETURN-MESSAGE.
      GT_LIST-ICON    = ICON_LED_RED.
      CLEAR GT_LIST-APPLYSO.
      MODIFY GT_LIST TRANSPORTING MESSAGE ICON APPLYSO WHERE VBELN = PV_VBELN AND APPLYSO EQ 'X'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_PARAMETERS_PO
*&---------------------------------------------------------------------*
FORM CLEAR_PARAMETERS_PO.

  CLEAR : GS_POHEADER,
          GS_POHEADERX,
          GT_RETURN,  GT_RETURN[],
          GT_POITEM,  GT_POITEM[],
          GT_POITEMX, GT_POITEMX[],
          GT_POCOND,  GT_POCOND[],
          GT_POCONDX, GT_POCONDX[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_ITEM_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM PO_ITEM_DATA_UPDATE .

  GT_POITEM-PO_ITEM    = GT_LIST-EBELP.
  GT_POITEMX-PO_ITEM   = GT_LIST-EBELP.

  GT_POITEM-NET_PRICE  = GT_LIST-KBETR_C.
  PERFORM BAPI_CURRENCY_CONV_TO_EXTERNAL USING    GT_LIST-KONWA
                                         CHANGING GT_POITEM-NET_PRICE.
  GT_POITEMX-NET_PRICE  = 'X'.

  APPEND GT_POITEM. CLEAR: GT_POITEM.
  APPEND GT_POITEMX. CLEAR: GT_POITEMX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BAPI_CURRENCY_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
FORM BAPI_CURRENCY_CONV_TO_EXTERNAL USING    PV_CURRENCY
                                     CHANGING PV_AMOUNT.

  DATA : LV_INTERNAL TYPE BAPICURR-BAPICURR,
         LV_EXTERNAL TYPE BAPICURR-BAPICURR.

  CLEAR : LV_INTERNAL, LV_EXTERNAL.

  LV_INTERNAL = PV_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      CURRENCY        = PV_CURRENCY
      AMOUNT_INTERNAL = LV_INTERNAL
    IMPORTING
      AMOUNT_EXTERNAL = LV_EXTERNAL.

  PV_AMOUNT = LV_EXTERNAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_BAPI_CHANGE_PO
*&---------------------------------------------------------------------*
FORM CALL_BAPI_CHANGE_PO  USING PV_EBELN.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      PURCHASEORDER = PV_EBELN
      POHEADER      = GS_POHEADER
      POHEADERX     = GS_POHEADERX
    TABLES
      RETURN        = GT_RETURN
      POITEM        = GT_POITEM
      POITEMX       = GT_POITEMX
      POCOND        = GT_POCOND
      POCONDX       = GT_POCONDX.

  READ TABLE GT_RETURN WITH KEY TYPE = 'E'.

  IF SY-SUBRC EQ 0.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    GT_LIST-MESSAGE = GT_RETURN-MESSAGE.
    GT_LIST-ICON    = ICON_LED_RED.
    CLEAR GT_LIST-APPLYPO.
    MODIFY GT_LIST TRANSPORTING MESSAGE ICON APPLYPO WHERE EBELN = PV_EBELN AND APPLYPO = 'X'.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    GT_LIST-ICON = ICON_LED_GREEN.
    CLEAR GT_LIST-APPLYPO.
    MODIFY GT_LIST TRANSPORTING ICON APPLYPO WHERE EBELN = PV_EBELN AND APPLYPO = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_FIELD
*&---------------------------------------------------------------------*
FORM CHECK_FIELD  USING  PV_DATA PV_TEXT.

  CHECK PV_DATA IS INITIAL.

  IF GT_LIST-MESSAGE IS INITIAL.
    GT_LIST-MESSAGE = PV_TEXT && TEXT-E07.
    GT_LIST-ICON = ICON_LED_RED.
  ELSE.
    GT_LIST-MESSAGE = GT_LIST-MESSAGE && '/' && PV_TEXT && TEXT-E07.
  ENDIF.

ENDFORM.
