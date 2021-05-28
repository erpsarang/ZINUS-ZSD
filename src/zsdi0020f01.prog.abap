*&---------------------------------------------------------------------*
*& Include          ZSDI0020F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT .

  CLEAR : GV_DATE, GV_ZTYPE.
  GET PARAMETER ID 'Z01' FIELD GV_DATE.
  GET PARAMETER ID 'Z02' FIELD GV_ZTYPE.

  IF GV_DATE IS NOT INITIAL AND
     P_DATE  IS INITIAL.
    P_DATE = GV_DATE.
  ENDIF.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR  = '9999'
      I_MONMIT = 00
      I_PERIV  = '24'
      I_POPER  = '024'
    IMPORTING
      E_DATE   = P_END.

ENDFORM. " SET_INIT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA : LS_MOD_CELLS     TYPE LVC_S_MODI,
         LV_GETVALUE(30),
         LV_MODIVALUE(30).

  CLEAR: LS_MOD_CELLS.

  LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
    CASE LS_MOD_CELLS-FIELDNAME.
*      WHEN 'MATNR'.

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

*  LS_TOOLBAR-FUNCTION  = 'ADD'.
*  LS_TOOLBAR-ICON      = ICON_INSERT_ROW.
*  LS_TOOLBAR-BUTN_TYPE = SPACE.
*  LS_TOOLBAR-DISABLED  = SPACE.
*  LS_TOOLBAR-TEXT      = 'Add Line'.
*  LS_TOOLBAR-QUICKINFO = 'Add Line'.
*  LS_TOOLBAR-CHECKED   = SPACE.
*  APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.
*
*  CLEAR LS_UPLOAD.
*  MOVE 3 TO LS_UPLOAD-BUTN_TYPE.
*  APPEND LS_UPLOAD TO PE_OBJECT->MT_TOOLBAR.
*
*  LS_TOOLBAR-FUNCTION  = 'DELETE'.
*  LS_TOOLBAR-ICON      = ICON_DELETE_ROW.
*  LS_TOOLBAR-BUTN_TYPE = SPACE.
*  LS_TOOLBAR-DISABLED  = SPACE.
*  LS_TOOLBAR-TEXT      = 'Delete Line'.
*  LS_TOOLBAR-QUICKINFO = 'Delete Line'.
*  LS_TOOLBAR-CHECKED   = SPACE.
*  APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.

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


  LV_TEXT =  'Start date :' && '　' && P_DATE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'LARGE'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  DESCRIBE TABLE GT_LIST LINES DATA(LV_CNT).

  LV_TEXT =  'Total data :' && '　' && LV_CNT.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'LARGE'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  CLEAR LV_TEXT.

  PERFORM GET_DOMAIN_ZTYPE USING GV_ZTYPE CHANGING LV_TEXT.
  CONCATENATE 'Process type : ' LV_TEXT INTO LV_TEXT SEPARATED BY SPACE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_EMPHASIS = CL_DD_AREA=>HEADING
      SAP_COLOR    = CL_DD_AREA=>LIST_BACKGROUND_INV.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.


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
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

  RANGES : LR_SKU FOR MARA-MATNR,
           LR_INT_SKU FOR MARA-MATNR.


  CLEAR : GV_ERROR.
  SELECT SINGLE MANDT INTO SY-MANDT
         FROM ZSDT0021
         WHERE ZSTART EQ P_DATE
         AND   ZTYPE  EQ GV_ZTYPE
         AND   ZINTERCAL_I EQ 'X'
         AND   ZINTERCAL_S EQ 'X'.
  IF SY-SUBRC NE 0.
    GV_ERROR = 'X'.
  ENDIF.
  CHECK GV_ERROR IS INITIAL.

*1. 판매단가 생성
*1.1  매입단가 생성을 위한 기준단가 정보 가져오기
*   ZSDT0040-ZSYSTEM = 'JDE'
*   위의 조건에 해당하는 ZKUNNR_IC 값이 생성대상 법인임
*   위의 조건으로 생성된 HQ 매입단가 가져오기

  SELECT  ZKUNNR_IC,
          ZBUKRS_JDE
  INTO TABLE @DATA(LT_DATA)
  FROM ZSDT0040
  WHERE ZSYSTEM = 'JDE'.
  SORT LT_DATA BY ZKUNNR_IC.

  CHECK LT_DATA IS NOT INITIAL.

  IF GV_ZTYPE NE 'N'.
    PERFORM MAKE_MATNR_RANGES.
  ENDIF.

  SELECT A~KAPPL, A~KSCHL, A~LIFNR, B~NAME1 AS LIFNR_NM,
         A~MATNR, C~MAKTX AS MATNR_TXT,
         A~EKORG, D~EKOTX, D~BUKRS, F~BUTXT,
         A~WERKS, E~NAME1 AS WERKS_NM,
         A~ESOKZ, A~DATBI, A~DATAB, A~KNUMH,
         K~KBETR, K~KONWA, K~KPEIN,
         K~KMEIN
   INTO TABLE @DATA(LT_A017)
   FROM  A017 AS A INNER JOIN KONP AS K
   ON   A~KNUMH EQ K~KNUMH
   LEFT JOIN LFA1 AS B
   ON    A~LIFNR EQ B~LIFNR
   LEFT  JOIN MAKT AS C
   ON    A~MATNR EQ C~MATNR
   AND   C~SPRAS EQ @SY-LANGU
   LEFT  JOIN T024E AS D
   ON    A~EKORG EQ D~EKORG
   LEFT  JOIN T001W AS E
   ON    A~WERKS EQ E~WERKS
   LEFT  JOIN T001 AS F
   ON    D~BUKRS EQ F~BUKRS
   FOR ALL ENTRIES IN @LT_DATA
   WHERE A~KSCHL EQ 'PB00'
   AND   A~LIFNR EQ @LT_DATA-ZKUNNR_IC
   AND   A~WERKS EQ '1001'
   AND   A~DATBI GE @P_DATE
   AND   A~DATAB LE @P_DATE.

  IF GV_ZTYPE NE 'N'.
    DELETE LT_A017 WHERE MATNR NOT IN GR_MATNR.
  ENDIF.

  _CLEAR : LR_SKU, LR_INT_SKU.
  LOOP AT LT_A017 INTO DATA(LS_A017).
    IF LS_A017-EKORG+0(2) = '1Y'. "NON INTERCOMPANY
      _RANGE LR_SKU 'I' 'EQ' LS_A017-MATNR ''.
    ELSE. "INTERCOMPANY
      _RANGE LR_INT_SKU 'I' 'EQ' LS_A017-MATNR ''.
    ENDIF.
  ENDLOOP.

  SORT : LR_SKU BY LOW,
         LR_INT_SKU BY LOW.

*1.2 판매단가 생성을 위한 데이터 생성
*위에서 찾은 데이터를 가지고 아래와 같이 ship-to code를 찾기 위해 HQ 판매단가 정보를 찾는다

*--EKORG <> '1011' 이면 관계사
  SELECT A~VKORG, A~MATNR, A~KUNNR, A~KUNWE, C~NAME1,
         T~MAKTX, A~KSCHL, K~KBETR, K~KONWA, K~KPEIN,
         K~KMEIN, A~DATAB, A~DATBI, A~VTWEG,
         D~TITLE_LET
         INTO TABLE @DATA(LT_A903)
         FROM A903 AS A INNER JOIN KONP AS K
         ON   A~KNUMH EQ K~KNUMH
         LEFT JOIN MAKT AS T
         ON   A~MATNR EQ T~MATNR
         LEFT JOIN KNA1 AS C
         ON   A~KUNNR EQ C~KUNNR
         LEFT JOIN BUT000 AS D
         ON A~KUNNR = D~PARTNER
         WHERE A~VKORG = '1001'
         AND   A~MATNR IN @LR_INT_SKU
         AND   A~DATBI GE @P_DATE
         AND   A~DATAB LE @P_DATE
         AND   A~KSCHL EQ 'PR00'
         AND   T~SPRAS EQ @SY-LANGU
         AND   K~LOEVM_KO = ''.

  SORT LT_A903 BY MATNR.

*--EKORG = '1011' 이면 관계사외
  SELECT A~VKORG, A~MATNR, A~KUNNR, C~NAME1,
         T~MAKTX, A~KSCHL, K~KBETR, K~KONWA, K~KPEIN,
         K~KMEIN, A~DATAB, A~DATBI, A~VTWEG,
         D~TITLE_LET
         INTO TABLE @DATA(LT_A305)
         FROM A305 AS A INNER JOIN KONP AS K
         ON   A~KNUMH EQ K~KNUMH
         LEFT JOIN MAKT AS T
         ON   A~MATNR EQ T~MATNR
         LEFT JOIN KNA1 AS C
         ON   A~KUNNR EQ C~KUNNR
         LEFT JOIN BUT000 AS D
         ON A~KUNNR = D~PARTNER
         WHERE A~VKORG = '1001'
         AND   A~MATNR IN @LR_SKU
         AND   A~DATBI GE @P_DATE
         AND   A~DATAB LE @P_DATE
         AND   A~KSCHL EQ 'PR00'
         AND   T~SPRAS EQ @SY-LANGU
         AND   K~LOEVM_KO = ''.

  SORT LT_A305 BY MATNR.

*-DI인지 확인
  SELECT A~ZKUNNR_IC, A~VTWEG, A~KUNWE, A~KUNNR,
         B~PARTNER, B~TITLE_LET
    FROM ZSDT0060 AS A INNER JOIN BUT000 AS B ON A~KUNNR = B~PARTNER
    INTO TABLE @DATA(LT_0060)
   WHERE VTWEG EQ '10'.

  SORT LT_0060 BY ZKUNNR_IC KUNWE.

  LOOP AT LT_A017 INTO DATA(LS_A017_2).
    MOVE-CORRESPONDING LS_A017_2 TO GT_LIST.
    READ TABLE LT_DATA INTO DATA(LS_DATA) WITH KEY ZKUNNR_IC = GT_LIST-LIFNR BINARY SEARCH.
    IF LS_DATA-ZBUKRS_JDE IS INITIAL.
      GT_LIST-ICON = ICON_LED_RED.
      GT_LIST-MESSAGE = TEXT-M03.
    ELSE.
      GT_LIST-ZBUKRS_JDE = LS_DATA-ZBUKRS_JDE.
    ENDIF.

    IF LS_A017_2-EKORG+0(2) = '1Y'. "NON INTERCOMPANY
      READ TABLE LT_A305 INTO DATA(LS_A305) WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-KUNNR = LS_A305-KUNNR.
        GT_LIST-KUNNR_JDE = LS_A305-TITLE_LET.
      ENDIF.
    ELSE. "INTERCOMPANY
      READ TABLE LT_A903 INTO DATA(LS_A903) WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-KUNNR = LS_A903-KUNNR.
        GT_LIST-KUNNR_JDE = LS_A903-TITLE_LET.
      ENDIF.
    ENDIF.

*-DI인지 확인
    READ TABLE LT_0060 INTO DATA(LS_0060) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR
                                                   KUNWE     = LS_A903-KUNWE BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-KUNNR = LS_0060-KUNNR.
      GT_LIST-KUNNR_JDE = LS_0060-TITLE_LET.
      CLEAR : LS_0060, LS_A903.
    ENDIF.

    IF GT_LIST-KUNNR_JDE IS INITIAL.
      GT_LIST-ICON = ICON_LED_RED.
      GT_LIST-MESSAGE = TEXT-M04.
    ENDIF.

    IF GT_LIST-KUNNR IS INITIAL.
      GT_LIST-ICON = ICON_LED_RED.
      GT_LIST-MESSAGE = TEXT-M05.
    ENDIF.

    GT_LIST-START_DATE = P_DATE.
    GT_LIST-END_DATE = P_END.

    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  SORT GT_LIST BY LIFNR ZBUKRS_JDE KUNNR KUNNR_JDE MATNR.
  DELETE ADJACENT DUPLICATES FROM GT_LIST COMPARING LIFNR ZBUKRS_JDE KUNNR KUNNR_JDE MATNR.

  CLEAR : GV_NOT_CONFIRM.
  SELECT SINGLE MANDT INTO SY-MANDT
                FROM ZSDT0021
                WHERE ZSTART EQ P_DATE
                AND   ZTYPE  EQ GV_ZTYPE
                AND   ZCONFIRM EQ SPACE.
  IF SY-SUBRC = 0.
    GV_NOT_CONFIRM = 'X'.
  ENDIF.

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
*& Form SEARCH_HELP_DATE
*&---------------------------------------------------------------------*
FORM SEARCH_HELP_DATE USING PV_FIELD.

  DATA : BEGIN OF LT_LINE OCCURS 0,
           ZSTART   LIKE ZSDT0020-ZSTART,
           ZTYPE    LIKE ZSDT0020-ZTYPE,
           ZCONFIRM LIKE ZSDT0021-ZCONFIRM,
         END OF LT_LINE.

  RANGES : LR_ZTYPE FOR ZSDT0020-ZTYPE.

  _CLEAR LR_ZTYPE.
  IF GV_ZTYPE IS NOT INITIAL.
    _RANGE : LR_ZTYPE 'I' 'EQ' GV_ZTYPE ''.
  ENDIF.

  _CLEAR LT_LINE.
  SELECT ZSTART ZTYPE ZCONFIRM
  INTO CORRESPONDING FIELDS OF TABLE LT_LINE
  FROM ZSDT0021
  WHERE ZTYPE IN LR_ZTYPE.

  SORT LT_LINE BY ZSTART DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_LINE COMPARING ZSTART.

  CLEAR : GT_RETURNTAB[], GT_RETURNTAB.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZSTART'
      DYNPROFIELD     = PV_FIELD
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      WINDOW_TITLE    = 'START DATE'
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
*& Form SEND_DATA_TO_JDE
*&---------------------------------------------------------------------*
FORM SEND_DATA_TO_JDE .

  DATA : LV_CON LIKE DBCON-CON_NAME VALUE 'JDEDB'.
  DATA: LT_ROWS  TYPE LVC_T_ROW,
        LS_ROWS  TYPE LVC_S_ROW,
        LV_CHECK,
        LV_ERR.

  CLEAR: LT_ROWS[], LS_ROWS.
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS.

  IF LT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM POPUP_MSG USING 'Send Data'
                          'Do you want to send selected data?'
                          LV_CHECK.

  CHECK LV_CHECK = '1'.

  CLEAR LV_ERR.
  _CLEAR GT_INTERFACE.
  LOOP AT LT_ROWS INTO LS_ROWS.
    READ TABLE GT_LIST INDEX LS_ROWS-INDEX.
    CHECK GT_LIST-ICON NE ICON_LED_RED.
    MOVE-CORRESPONDING GT_LIST TO GT_INTERFACE.
    APPEND GT_INTERFACE. CLEAR GT_INTERFACE.
  ENDLOOP.

  CHECK GT_INTERFACE[] IS NOT INITIAL.

  EXEC SQL.
    CONNECT TO :LV_CON AS :LV_CON
  ENDEXEC.

  CASE SY-SUBRC.
    WHEN '0'.
**       Connect...
      EXEC SQL.
        SET CONNECTION :LV_CON
      ENDEXEC.

      IF SY-SUBRC = 0.
        MESSAGE S000 WITH 'Connection Success!' DISPLAY LIKE 'S'.

        PERFORM INSERT_DATA.

        IF SY-SUBRC EQ 0.
          MESSAGE S000 WITH 'Data Transfer Processing OK' DISPLAY LIKE 'S'.

          UPDATE ZSDT0021
          SET    ZINTERFACE      = 'X'
                 ZINTERFACE_DATE = SY-DATLO
                 ZCONFIRM        = SPACE
                 ZCONFIRM_DATE   = SPACE
          WHERE ZSTART EQ P_DATE
            AND ZTYPE  EQ GV_ZTYPE.
        ENDIF.

**       Disconnect.
        EXEC SQL.
          DISCONNECT :LV_CON
        ENDEXEC.

        IF SY-SUBRC EQ 0.
          MESSAGE S000 WITH 'Disconnect Success!' DISPLAY LIKE 'S'.
        ENDIF.

      ELSE.
        MESSAGE S000 WITH 'Connection Error' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INSERT_DATA
*&---------------------------------------------------------------------*
FORM INSERT_DATA .

  DATA : LCL_NATIVE_SQL TYPE  REF TO CX_SY_NATIVE_SQL_ERROR,
         LV_NATIVE_ERR  TYPE STRING.

  DATA : LV_CHECK TYPE C.
  CLEAR LV_CHECK.

  LOOP AT GT_INTERFACE.
    PERFORM SAPGUI_PROGRESS_INDICATOR USING 50 'M'.

    CLEAR : GV_MESSAGE, LV_CHECK.

    TRY.
        EXEC SQL.
          MERGE INTO F56SA060
              USING DUAL
                     ON ( S6KCO = :GT_INTERFACE-ZBUKRS_JDE AND S6AN8 = :GT_INTERFACE-KUNNR_JDE
                     AND  S6SRTX = :GT_INTERFACE-MATNR AND S6GDC5 = :GT_INTERFACE-START_DATE )
            WHEN MATCHED THEN
                UPDATE SET
                  S6GDC6 = :GT_INTERFACE-END_DATE,
                  S6NETPR = :GT_INTERFACE-KBETR,
                  S6CRCD = :GT_INTERFACE-KONWA


            WHEN NOT MATCHED THEN
                 INSERT (
             S6KCO,
             S6AN8,
             S6SRTX,
             S6GDC5,
             S6GDC6,
             S6NETPR,
             S6CRCD )


             VALUES(
               :GT_INTERFACE-ZBUKRS_JDE,
               :GT_INTERFACE-KUNNR_JDE,
               :GT_INTERFACE-MATNR,
               :GT_INTERFACE-START_DATE,
               :GT_INTERFACE-END_DATE,
               :GT_INTERFACE-KBETR,
               :GT_INTERFACE-KONWA

              )
        ENDEXEC.

      CATCH CX_SY_NATIVE_SQL_ERROR INTO LCL_NATIVE_SQL.
        LV_NATIVE_ERR = LCL_NATIVE_SQL->GET_TEXT( ).
        GV_MESSAGE = LV_NATIVE_ERR.
        ROLLBACK WORK.
        EXIT.
        LV_CHECK = 'X'.
    ENDTRY.

    CLEAR GT_INTERFACE.

  ENDLOOP.

  IF LV_CHECK EQ SPACE.
    COMMIT WORK.
    LOOP AT GT_INTERFACE.
      GT_LIST-MESSAGE = TEXT-M01.
      GT_LIST-ICON    = ICON_LED_GREEN.
      MODIFY GT_LIST TRANSPORTING MESSAGE ICON
                            WHERE ZBUKRS_JDE = GT_INTERFACE-ZBUKRS_JDE
                              AND KUNNR_JDE = GT_INTERFACE-KUNNR_JDE
                              AND MATNR = GT_INTERFACE-MATNR.

    ENDLOOP.
  ELSE.
    LOOP AT GT_INTERFACE.
      GT_LIST-MESSAGE = TEXT-M02.
      GT_LIST-ICON    = ICON_LED_RED.
      MODIFY GT_LIST TRANSPORTING MESSAGE ICON
                            WHERE ZBUKRS_JDE = GT_INTERFACE-ZBUKRS_JDE
                              AND KUNNR_JDE = GT_INTERFACE-KUNNR_JDE
                              AND MATNR = GT_INTERFACE-MATNR.

    ENDLOOP.
  ENDIF.

  WAIT UP TO 1 SECONDS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM SAPGUI_PROGRESS_INDICATOR   USING PV_PER
                                      PV_VAL TYPE C.

  DATA : LV_TEXT(32) TYPE C.
  CLEAR  LV_TEXT.

  CASE PV_VAL.
    WHEN 'M'.
      LV_TEXT = 'Data insert SAP to JDE...'.
  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = PV_PER
      TEXT       = LV_TEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SELECTION
*&---------------------------------------------------------------------*
FORM SET_SELECTION .

  LOOP AT SCREEN.
    IF GV_DATE IS NOT INITIAL.
      IF SCREEN-NAME = 'P_DATE'.
        SCREEN-INPUT = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DOMAIN_ZTYPE
*&---------------------------------------------------------------------*
FORM GET_DOMAIN_ZTYPE  USING  PV_ZTYPE
                       CHANGING PV_TEXT.

  DATA: LT_VAL TYPE TABLE OF DD07V.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME         = 'ZDTYPE'
      TEXT            = 'X'
    TABLES
      VALUES_TAB      = LT_VAL
    EXCEPTIONS
      NO_VALUES_FOUND = 1
      OTHERS          = 2.

  READ TABLE LT_VAL INTO DATA(LS_VAL) WITH KEY DOMVALUE_L = PV_ZTYPE.
  IF SY-SUBRC = 0.
    PV_TEXT = LS_VAL-DDTEXT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_MATNR_RANGES
*&---------------------------------------------------------------------*
FORM MAKE_MATNR_RANGES .

  SELECT ZKUNNR_IC,
  KUNNR,
  LIFNR,
  ZPRODH_GROUP,
  MATNR,
  ZSTART,
  ZTYPE
  INTO TABLE @DATA(LT_0020)
  FROM ZSDT0020
  WHERE ZSTART = @P_DATE
    AND ZTYPE  = @GV_ZTYPE.

  _CLEAR GR_MATNR.
  LOOP AT LT_0020 INTO DATA(LS_0020).
    _RANGE GR_MATNR 'I' 'EQ' LS_0020-MATNR ''.
  ENDLOOP.

ENDFORM.
