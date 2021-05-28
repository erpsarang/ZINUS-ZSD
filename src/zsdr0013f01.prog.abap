*&---------------------------------------------------------------------*
*& Include          ZSDR0011F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT .

  CLEAR : GV_DATE, GV_ZTYPE, GV_DATE2.
  GET PARAMETER ID 'Z01' FIELD GV_DATE.
  GET PARAMETER ID 'Z02' FIELD GV_ZTYPE.

  IF GV_ZTYPE = 'P' OR GV_ZTYPE = 'S'.
    GET PARAMETER ID 'Z03' FIELD GV_DATE2.
    _RANGE S_DATE 'I' 'EQ' GV_DATE GV_DATE2.
  ELSE.
    P_DATE  = GV_DATE.
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
  DATA : LV_DATE TYPE CHAR10.

  CLEAR : LV_TEXT.

  CLEAR : LV_DATE.
  WRITE GV_DATE TO LV_DATE.
  IF S_DATE[] IS INITIAL.
    LV_TEXT =  'Start date :' && '　' && LV_DATE.
  ELSE.
    CONCATENATE 'Start date :' S_DATE-LOW '~'
                'End Date :' S_DATE-HIGH
    INTO LV_TEXT SEPARATED BY SPACE.
  ENDIF.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'MIDIUM'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  CLEAR LV_TEXT.
  DESCRIBE TABLE GT_LIST LINES DATA(LS_LINE).
  LV_TEXT =  'Number of entries : '  && LS_LINE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'MIDIUM'.

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

*1. ZSDT0020(Management margin) TABLE 기준으로 데이터를 가지고 온다.
  CLEAR : GV_ERROR.

  SELECT SINGLE MANDT INTO SY-MANDT
                FROM ZSDT0021
                WHERE ZSTART  EQ GV_DATE
                AND   ZTYPE   EQ GV_ZTYPE
                AND   ZMARGIN EQ 'X'.
  IF SY-SUBRC NE 0.
    GV_ERROR = 'X'.
  ENDIF.

  CHECK GV_ERROR IS INITIAL.

  _CLEAR GT_DATA.
  SELECT A~ZKUNNR_IC,
         A~KUNNR,
         A~LIFNR,
         A~ZPRODH_GROUP,
         A~MATNR,
         A~ZSTART,
         A~ZTYPE,
         A~ZMARGIN,
         A~ZEXCEPT,
         F~ZCONFIRM,
         B~VKORG,
         B~VTWEG,
         B~ZKUNNR_S,
         B~ZKUNNR_SUB,
         C~ZKUNNR,
         C~ZKUNNR_DESC,
         E~EKORG
  INTO CORRESPONDING FIELDS OF TABLE @GT_DATA
  FROM ZSDT0020 AS A LEFT JOIN ZSDT0042 AS B ON A~ZKUNNR_IC  = B~ZKUNNR_IC
                                            AND A~KUNNR      = B~KUNNR
                     LEFT JOIN ZSDT0041 AS C ON A~ZKUNNR_IC  = C~ZKUNNR_IC
                                            AND A~KUNNR      = C~ZKUNNR
                     LEFT JOIN ZSDT0060 AS E ON B~ZKUNNR_SUB = E~ZKUNNR_IC
                                            AND B~VTWEG      = E~VTWEG
                                            AND A~KUNNR      = E~KUNNR
                     LEFT JOIN ZSDT0021 AS F ON A~ZSTART     = F~ZSTART
  WHERE A~ZSTART = @GV_DATE
    AND A~ZTYPE  = @GV_ZTYPE.

  CLEAR : GV_NOT_CONFIRM.
  LOOP AT GT_DATA WHERE ZCONFIRM = SPACE.
    GV_NOT_CONFIRM = 'X'.
    EXIT.
  ENDLOOP.

*GR_PRODH
  _CLEAR GT_0090.
  SELECT ZPRODH_GROUP PRODH
  INTO CORRESPONDING FIELDS OF TABLE GT_0090
  FROM ZSDT0090.
  SORT GT_0090 BY PRODH.

  _CLEAR GR_PRODH.
  CLEAR GT_DATA.
  READ TABLE GT_DATA WITH KEY ZPRODH_GROUP = 'ALL'.
  IF SY-SUBRC = 0.
    LOOP AT GT_0090.
      _RANGE GR_PRODH 'I' 'EQ' GT_0090-PRODH ''.
    ENDLOOP.
  ELSE.
    LOOP AT GT_DATA.
      IF GT_DATA-ZPRODH_GROUP IS NOT INITIAL AND
         GT_DATA-ZEXCEPT      IS INITIAL.
        CHECK GT_DATA-ZPRODH_GROUP NE 'ALL'.
        LOOP AT GT_0090 WHERE ZPRODH_GROUP = GT_DATA-ZPRODH_GROUP.
          _RANGE GR_PRODH 'I' 'EQ' GT_0090-PRODH ''.
          CLEAR : GT_0090.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-마진 확인용도
  _CLEAR GT_0020.
  MOVE-CORRESPONDING GT_DATA[] TO GT_0020[].

  _CLEAR GT_0040.
  SELECT ZKUNNR_IC
         BUKRS
         ZSYSTEM
         ZSEND_SALES
         ZSEND_INFO
         ZHQ
  INTO CORRESPONDING FIELDS OF TABLE GT_0040
  FROM ZSDT0040.
  SORT GT_0040 BY ZKUNNR_IC.

*-ITALY
  _CLEAR GT_0050.
  SELECT ZKUNNR_IC,
         WERKS,
         LIFNR,
         VTWEG,
         ZKUNNR_S
  INTO  CORRESPONDING FIELDS OF TABLE @GT_0050
  FROM ZSDT0050.
  SORT GT_0050 BY ZKUNNR_IC LIFNR VTWEG.


  _CLEAR : GT_0070_KEY, GT_MDDP_KEY, GT_FOB_305_KEY, GT_FOB_903_KEY.
  LOOP AT GT_DATA.
    CHECK GT_DATA-ZEXCEPT IS INITIAL.
*1)ZSDT0042에만 있는경우 SAP에서 기준금액 가져온다.
    IF GT_DATA-ZKUNNR_SUB IS NOT INITIAL AND GT_DATA-ZKUNNR IS INITIAL.
      MOVE-CORRESPONDING GT_DATA TO GT_MDDP_KEY.
      APPEND GT_MDDP_KEY. CLEAR GT_MDDP_KEY.
    ENDIF.
*2)ZSDT0042,ZSDT0041(Manage Exception Customer)에 있는 경우 ZSDT0070에서 데이터 가져온다.
    IF GT_DATA-ZKUNNR_SUB IS NOT INITIAL AND GT_DATA-ZKUNNR IS NOT INITIAL.
      MOVE-CORRESPONDING GT_DATA TO GT_0070_KEY.
      APPEND GT_0070_KEY. CLEAR GT_0070_KEY.
    ENDIF.
*3)ZSDT0042,ZSDT0041 둘다 없는경우 INTERCOMPANY(ZSDT0040)인지 확인.
    IF GT_DATA-ZKUNNR_SUB IS INITIAL AND GT_DATA-ZKUNNR IS INITIAL.
      READ TABLE GT_0040 WITH KEY ZKUNNR_IC = GT_DATA-KUNNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING GT_DATA TO GT_FOB_903_KEY.
        APPEND GT_FOB_903_KEY. CLEAR GT_FOB_903_KEY.
      ELSE.
*4)Intercompany 가 아닌 경우 납품처에 대한 정보는 포함하지 않는다
        MOVE-CORRESPONDING GT_DATA TO GT_FOB_305_KEY.
        APPEND GT_FOB_305_KEY. CLEAR GT_FOB_305_KEY.
      ENDIF.
    ENDIF.
  ENDLOOP.

*-1)ZSDT0042에만 있는경우 SAP에서 기준금액 가져온다.
  PERFORM READ_MDDP.
*-2)ZSDT0042,ZSDT0041(Manage Exception Customer)에 있는 경우 ZSDT0070에서 데이터 가져온다.
  PERFORM READ_MDDP_EXCEPTION.
*-3)ZSDT0042,ZSDT0041 둘다 없는경우 INTERCOMPANY(ZSDT0040)인지 확인.
  PERFORM READ_FOB_INTERCOM.
*-4)Intercompany 가 아닌 경우 납품처에 대한 정보는 포함하지 않는다.
  PERFORM READ_FOB.


  PERFORM MERGE_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form Create_price
*&---------------------------------------------------------------------*
FORM CREATE_PRICE .

  DATA : LT_SAVE     LIKE TABLE OF ZSDT0110 WITH HEADER LINE,
         LV_ERR,
         LV_MSG(100),
         LV_CHECK    TYPE C.

  DATA : LS_0021 LIKE ZSDT0021.

  CLEAR : LV_ERR, LS_0021.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF LS_0021
         FROM ZSDT0021
         WHERE ZSTART EQ GV_DATE
           AND ZTYPE  EQ GV_ZTYPE.
  IF SY-SUBRC = 0.
    IF LS_0021-ZMARGIN IS INITIAL.
      LV_ERR = 'X'.
      MESSAGE S015 DISPLAY LIKE 'E'.
    ENDIF.

    IF LS_0021-ZHQCAL IS NOT INITIAL.
      LV_ERR = 'X'.
      PERFORM POPUP_MSG USING 'Create price'
                              'Do you want to create price again?'
                              LV_CHECK.
      IF LV_CHECK = '1'.
        CLEAR : LV_ERR.
      ENDIF.
    ENDIF.

  ENDIF.

  CHECK LV_ERR IS INITIAL.

  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.

    _CLEAR GT_CHK_PRICE.
    SELECT A~VKORG, A~MATNR, A~VTWEG, A~KUNNR,
           A~KUNWE, A~KSCHL, A~DATAB, A~DATBI,
           B~KBETR, B~KONWA, B~KPEIN, B~KMEIN
           INTO CORRESPONDING FIELDS OF TABLE @GT_CHK_PRICE
           FROM A903 AS A INNER JOIN KONP AS B
           ON   A~KNUMH EQ B~KNUMH
           WHERE A~VKORG    EQ '1001'
           AND   A~DATAB    EQ @GV_DATE
           AND   A~KSCHL    EQ 'PR00'
           AND   B~LOEVM_KO EQ @ABAP_OFF.

    SORT GT_CHK_PRICE BY VKORG VTWEG KUNNR KUNWE MATNR.

    IF LV_CHECK IS INITIAL.
      PERFORM POPUP_MSG USING 'Create price'
                              'Do you want to create price?'
                              LV_CHECK.
    ENDIF.
    CHECK LV_CHECK = '1'.

    CLEAR : GV_SUCCESS, GV_FAILURE.
    _CLEAR LT_SAVE.

    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
      IF SY-SUBRC = 0.
*오류건 선택시 에러
        IF GT_LIST-ICON EQ ICON_LED_RED.
          CONTINUE.
        ENDIF.

        MOVE-CORRESPONDING GT_LIST TO LT_SAVE.
        LT_SAVE-ZTYPE = GV_ZTYPE.

** BDC MODE
        GS_CTU_PARAMS-DISMODE  = 'N'.
        GS_CTU_PARAMS-UPDMODE  = 'S'.
        GS_CTU_PARAMS-RACOMMIT = 'X'.
        GS_CTU_PARAMS-NOBINPT  = 'X'.

        IF GT_LIST-FLAG = 'FOB'.
*INFO 금액이 있는경우에만 생성
          IF GT_LIST-ZKBETR_MM IS NOT INITIAL.
            PERFORM MAKE_INFO_RECORD.
          ENDIF.
          READ TABLE GT_INFO INDEX 1.
          IF GT_INFO-TYPE EQ 'S'. "성공한경우에만 진행
            LT_SAVE-ZSUCCESS_MM = 'O'.
            GT_LIST-ICON = ICON_LED_GREEN.
            IF GT_LIST-ZKBETR_SD IS NOT INITIAL.
              READ TABLE GT_0040 WITH KEY ZKUNNR_IC = GT_LIST-ZKUNNR_SUB BINARY SEARCH.
              IF SY-SUBRC = 0.
                READ TABLE GT_CHK_PRICE WITH KEY VKORG = GT_LIST-VKORG
                                                 VTWEG = GT_LIST-VTWEG
                                                 KUNNR = GT_LIST-ZKUNNR_SUB
                                                 KUNWE = GT_LIST-KUNWE
                                                 MATNR = GT_LIST-MATNR BINARY SEARCH.
                IF SY-SUBRC = 0.
                  PERFORM MAKE_PRICE_BDC_VK12 USING LT_SAVE-ZSUCCESS_SD.
                ELSE.
                  PERFORM MAKE_PRICE_BDC1 USING LT_SAVE-ZSUCCESS_SD.
                ENDIF.
              ELSE.
                READ TABLE GT_CHK_PRICE WITH KEY VKORG = GT_LIST-VKORG
                                                 VTWEG = GT_LIST-VTWEG
                                                 KUNNR = GT_LIST-ZKUNNR_SUB
                                                 KUNWE = GT_LIST-KUNWE
                                                 MATNR = GT_LIST-MATNR BINARY SEARCH.
                IF SY-SUBRC = 0.
                  PERFORM MAKE_PRICE_BDC2_VK12 USING LT_SAVE-ZSUCCESS_SD.
                ELSE.
                  PERFORM MAKE_PRICE_BDC2 USING LT_SAVE-ZSUCCESS_SD.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            LT_SAVE-ZSUCCESS_MM = 'X'.
            GT_LIST-MESSAGE = GT_INFO-MESSAGE.
            GT_LIST-ICON = ICON_LED_RED.
          ENDIF.


        ELSEIF GT_LIST-FLAG = 'MDDP'.
*INFO 금액이 있는경우에만 생성
          IF GT_LIST-ZKBETR_MM IS NOT INITIAL.
            PERFORM MAKE_INFO_RECORD.
          ENDIF.
          READ TABLE GT_INFO INDEX 1.
          IF GT_INFO-TYPE EQ 'S'. "성공여부 FLAG만 남기고 실패해도 판가생성
            LT_SAVE-ZSUCCESS_MM = 'O'.
            GT_LIST-ICON = ICON_LED_GREEN.
          ELSE.
            LT_SAVE-ZSUCCESS_MM = 'X'.
            GT_LIST-MESSAGE = GT_INFO-MESSAGE.
            GT_LIST-ICON = ICON_LED_RED.
          ENDIF.
          IF GT_LIST-ZKBETR_SD IS NOT INITIAL.
            READ TABLE GT_0040 WITH KEY ZKUNNR_IC = GT_LIST-ZKUNNR_SUB BINARY SEARCH.
            IF SY-SUBRC = 0.
              READ TABLE GT_CHK_PRICE WITH KEY VKORG = GT_LIST-VKORG
                                               VTWEG = GT_LIST-VTWEG
                                               KUNNR = GT_LIST-ZKUNNR_SUB
                                               KUNWE = GT_LIST-KUNWE
                                               MATNR = GT_LIST-MATNR BINARY SEARCH.
              IF SY-SUBRC = 0.
                PERFORM MAKE_PRICE_BDC_VK12 USING LT_SAVE-ZSUCCESS_SD.
              ELSE.
                PERFORM MAKE_PRICE_BDC1 USING LT_SAVE-ZSUCCESS_SD.
              ENDIF.
            ELSE.
              READ TABLE GT_CHK_PRICE WITH KEY VKORG = GT_LIST-VKORG
                                               VTWEG = GT_LIST-VTWEG
                                               KUNNR = GT_LIST-ZKUNNR_SUB
                                               KUNWE = GT_LIST-KUNWE
                                               MATNR = GT_LIST-MATNR BINARY SEARCH.
              IF SY-SUBRC = 0.
                PERFORM MAKE_PRICE_BDC2_VK12 USING LT_SAVE-ZSUCCESS_SD.
              ELSE.
                PERFORM MAKE_PRICE_BDC2 USING LT_SAVE-ZSUCCESS_SD.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        MODIFY GT_LIST  INDEX GS_ROWS-INDEX TRANSPORTING ICON MESSAGE.

        LT_SAVE-ERNAM = SY-UNAME.
        LT_SAVE-ERDAT = SY-DATUM.
        LT_SAVE-ERZET = SY-UZEIT.
        APPEND LT_SAVE. CLEAR LT_SAVE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF LT_SAVE[] IS NOT INITIAL.
    MODIFY ZSDT0110 FROM TABLE LT_SAVE.

    UPDATE ZSDT0021
       SET ZHQCAL  = 'X'
           ZHQCAL_DATE     = SY-DATLO
           ZINTERCAL_I     = SPACE
           ZINTERCAL_IDATE = SPACE
           ZINTERCAL_S     = SPACE
           ZINTERCAL_SDATE = SPACE
           ZINTERFACE      = SPACE
           ZINTERFACE_DATE = SPACE
           ZCONFIRM        = SPACE
           ZCONFIRM_DATE   = SPACE
     WHERE ZSTART EQ GV_DATE
       AND ZTYPE  EQ GV_ZTYPE.
  ENDIF.

  FREE GO_DOCUMENT.
  CREATE OBJECT GO_DOCUMENT
    EXPORTING
      STYLE = 'TOP_OF_PAGE'.
  PERFORM HANDLE_TOP_OF_PAGE USING GO_DOCUMENT.

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
*& Form READ_MDDP
*&---------------------------------------------------------------------*
FORM READ_MDDP .

  DATA : LT_DATA LIKE TABLE OF GT_MDDP_KEY WITH HEADER LINE.
  DATA : LT_MDDP LIKE TABLE OF GT_MDDP WITH HEADER LINE.
  DATA : LV_CHK.

  CHECK GT_MDDP_KEY[] IS NOT INITIAL.

*STEP 1 기준금액 조회
  SELECT A~VKORG, A~MATNR, A~KUNNR, A~KSCHL,
         A~DATAB, A~DATBI, A~VTWEG,
         B~KBETR, B~KONWA,B~KPEIN, B~KMEIN,
         C~PRDHA AS PRODH
         INTO TABLE @DATA(LT_A305)
         FROM A305 AS A INNER JOIN KONP AS B
                                        ON A~KNUMH EQ B~KNUMH
                        INNER JOIN MARA AS C
                                        ON A~MATNR EQ C~MATNR
*                        INNER JOIN ZSDT0090 AS D
*                                        ON C~PRDHA EQ D~PRODH
         FOR ALL ENTRIES IN @GT_MDDP_KEY
         WHERE A~VKORG    EQ @GT_MDDP_KEY-VKORG
         AND   A~KUNNR    EQ @GT_MDDP_KEY-KUNNR
         AND   A~VTWEG    EQ @GT_MDDP_KEY-VTWEG
         AND   A~DATBI    GE @GV_DATE
         AND   A~DATAB    LE @GV_DATE
         AND   A~KSCHL    EQ 'PR00'
         AND   B~LOEVM_KO EQ @ABAP_OFF
         AND   C~LVORM    EQ @ABAP_OFF
         AND   C~PRDHA    IN @GR_PRODH.
  IF SY-SUBRC = 0.
    SORT LT_A305 BY VKORG
                    KUNNR
                    VTWEG.
  ENDIF.

  SORT GT_0090 BY PRODH.
*-기준금액이 있는 SKU별로 인터널 테이블 구성
  _CLEAR GT_MDDP.
  LOOP AT GT_MDDP_KEY.

*ITALY SHIP TO로 변경
    CLEAR GT_0050.
    READ TABLE GT_0050 WITH KEY ZKUNNR_IC = GT_MDDP_KEY-ZKUNNR_SUB
                                LIFNR     = GT_MDDP_KEY-LIFNR
                                VTWEG     = GT_MDDP_KEY-VTWEG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_MDDP_KEY-ZKUNNR_S = GT_0050-ZKUNNR_S.
    ELSE.
      CLEAR GT_0050.
      READ TABLE GT_0050 WITH KEY ZKUNNR_IC = GT_MDDP_KEY-ZKUNNR_SUB
                                  LIFNR     = SPACE
                                  VTWEG     = GT_MDDP_KEY-VTWEG BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_MDDP_KEY-ZKUNNR_S = GT_0050-ZKUNNR_S.
      ENDIF.
    ENDIF.


    READ TABLE LT_A305 WITH KEY VKORG = GT_MDDP_KEY-VKORG
                                KUNNR = GT_MDDP_KEY-KUNNR
                                VTWEG = GT_MDDP_KEY-VTWEG
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      LOOP AT LT_A305 INTO DATA(LS_A305) FROM SY-TABIX.

        IF LS_A305-VKORG NE GT_MDDP_KEY-VKORG OR
           LS_A305-KUNNR NE GT_MDDP_KEY-KUNNR OR
           LS_A305-VTWEG NE GT_MDDP_KEY-VTWEG.
          EXIT.
        ENDIF.

        PERFORM CHECK_DATA TABLES GT_MDDP_KEY
                            USING LS_A305-MATNR LS_A305-PRODH
                            CHANGING LV_CHK.

        CHECK LV_CHK IS INITIAL.

        MOVE-CORRESPONDING GT_MDDP_KEY TO GT_MDDP.
        GT_MDDP-ZBASE_PRICE = LS_A305-KBETR / LS_A305-KPEIN.
        GT_MDDP-KONWA_1 = LS_A305-KONWA.
        GT_MDDP-KPEIN_1 = LS_A305-KPEIN.
        GT_MDDP-KMEIN_1 = LS_A305-KMEIN.
        GT_MDDP-MATNR   = LS_A305-MATNR.

        IF GT_MDDP-ZPRODH_GROUP IS INITIAL.
          CLEAR GT_0090.
          READ TABLE GT_0090 WITH KEY PRODH = LS_A305-PRODH
                                      BINARY SEARCH.
          GT_MDDP-ZPRODH_GROUP = GT_0090-ZPRODH_GROUP.
        ENDIF.

        APPEND GT_MDDP.
        CLEAR : GT_MDDP, LS_A305.
      ENDLOOP.
    ENDIF.
    CLEAR GT_MDDP_KEY.
  ENDLOOP.


  PERFORM GET_INFO_RECORD_PRICE TABLES GT_MDDP.

  SORT GT_MDDP BY ZKUNNR_IC KUNNR LIFNR ZPRODH_GROUP MATNR.
  DELETE ADJACENT DUPLICATES FROM GT_MDDP COMPARING ZKUNNR_IC KUNNR LIFNR MATNR.

*STEP 3 : STEP 2을 바탕으로 HQ 판매단가를 가져온다
*HQ이고 매입단가가 있는경우에만 판매단가 대상
  _CLEAR LT_DATA.
  LOOP AT GT_MDDP.
    MOVE-CORRESPONDING GT_MDDP TO LT_DATA.
    APPEND LT_DATA. CLEAR LT_DATA.
  ENDLOOP.

  SORT LT_DATA BY ZKUNNR_IC ZKUNNR_S VTWEG MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING ZKUNNR_IC ZKUNNR_S VTWEG MATNR.

  IF LT_DATA[] IS NOT INITIAL.
    SELECT A~VKORG, A~MATNR, A~VTWEG, A~KUNNR,
           A~KUNWE, A~KSCHL, A~DATAB, A~DATBI,
           B~KBETR, B~KONWA, B~KPEIN, B~KMEIN
           INTO TABLE @DATA(LT_A903)
           FROM A903 AS A INNER JOIN KONP AS B
           ON   A~KNUMH EQ B~KNUMH
           FOR ALL ENTRIES IN @LT_DATA
           WHERE A~VKORG    EQ '1001'
           AND   A~KUNNR    EQ @LT_DATA-ZKUNNR_IC
           AND   A~KUNWE    EQ @LT_DATA-ZKUNNR_S
           AND   A~VTWEG    EQ @LT_DATA-VTWEG
           AND   A~DATBI    GE @GV_DATE
           AND   A~DATAB    LE @GV_DATE
           AND   A~KSCHL    EQ 'PR00'
           AND   B~LOEVM_KO EQ @ABAP_OFF
           AND   A~MATNR    EQ @LT_DATA-MATNR.
    IF SY-SUBRC = 0.
      SORT LT_A903 BY KUNNR KUNWE VTWEG MATNR.
    ENDIF.

    LOOP AT GT_MDDP.

      DATA(LV_TABIX) = SY-TABIX.

      READ TABLE LT_A903 INTO DATA(LS_A903) WITH KEY KUNNR = GT_MDDP-ZKUNNR_SUB
                                                     KUNWE = GT_MDDP-ZKUNNR_S
                                                     VTWEG = GT_MDDP-VTWEG
                                                     MATNR = GT_MDDP-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_MDDP-ZKBETR_SD_OLD = LS_A903-KBETR / LS_A903-KPEIN.
        GT_MDDP-ZKONWA_SD     = LS_A903-KONWA.
        GT_MDDP-ZKMEIN_SD     = LS_A903-KPEIN.
        GT_MDDP-KMEIN_3       = LS_A903-KMEIN.

        MODIFY GT_MDDP INDEX LV_TABIX
               TRANSPORTING ZKBETR_SD_OLD ZKONWA_SD ZKMEIN_SD KMEIN_3.
      ENDIF.

      CLEAR :LS_A903, GT_MDDP.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_MDDP_Exception
*&---------------------------------------------------------------------*
FORM READ_MDDP_EXCEPTION .

  DATA : LT_DATA LIKE TABLE OF GT_0070_KEY WITH HEADER LINE,
         LV_CHK.

  CHECK GT_0070_KEY[] IS NOT INITIAL.

*STEP 1 기준금액 조회
  SELECT A~MATNR, A~ZKUNNR, A~DATAB, A~DATBI,
         A~VTWEG, A~KBETR, A~KONWA, A~KPEIN,
         A~KMEIN,
         B~PRDHA AS PRODH
         INTO TABLE @DATA(LT_0070)
         FROM ZSDT0070 AS A INNER JOIN MARA AS B
                                            ON A~MATNR EQ B~MATNR
         FOR ALL ENTRIES IN @GT_0070_KEY
         WHERE A~ZKUNNR_IC = @GT_0070_KEY-ZKUNNR_SUB
         AND   A~ZKUNNR = @GT_0070_KEY-KUNNR
         AND   A~VTWEG  = @GT_0070_KEY-VTWEG
         AND   A~DATBI GE @GV_DATE
         AND   A~DATAB LE @GV_DATE
         AND   B~PRDHA IN @GR_PRODH
         AND   B~LVORM EQ @ABAP_OFF.

*-기준금액이 있는 SKU별로 인터널 테이블 구성
  _CLEAR GT_0070.
  LOOP AT GT_0070_KEY.
*ITALY SHIP TO
    CLEAR GT_0050.
    READ TABLE GT_0050 WITH KEY ZKUNNR_IC = GT_0070_KEY-ZKUNNR_SUB
                                LIFNR     = GT_0070_KEY-LIFNR
                                VTWEG     = GT_0070_KEY-VTWEG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_0070_KEY-ZKUNNR_S = GT_0050-ZKUNNR_S.
    ELSE.
      CLEAR GT_0050.
      READ TABLE GT_0050 WITH KEY ZKUNNR_IC = GT_0070_KEY-ZKUNNR_SUB
                                  LIFNR     = SPACE
                                  VTWEG     = GT_0070_KEY-VTWEG BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_0070_KEY-ZKUNNR_S = GT_0050-ZKUNNR_S.
      ENDIF.
    ENDIF.

    LOOP AT LT_0070 INTO DATA(LS_0070) WHERE ZKUNNR =  GT_0070_KEY-KUNNR
                                         AND VTWEG  =  GT_0070_KEY-VTWEG.

      PERFORM CHECK_DATA TABLES GT_0070_KEY
                          USING LS_0070-MATNR LS_0070-PRODH
                          CHANGING LV_CHK.

      CHECK LV_CHK IS INITIAL.

      MOVE-CORRESPONDING GT_0070_KEY TO GT_0070.
      GT_0070-ZBASE_PRICE = LS_0070-KBETR / LS_0070-KPEIN.
      GT_0070-KONWA_1 = LS_0070-KONWA.
      GT_0070-KPEIN_1 = LS_0070-KPEIN.
      GT_0070-KMEIN_1 = LS_0070-KMEIN.
      GT_0070-MATNR   = LS_0070-MATNR.

* HQ 매입단가를 가져온다
*      PERFORM GET_INFO_RECORD TABLES GT_0070.

      IF GT_0070-ZPRODH_GROUP IS INITIAL.
        CLEAR GT_0090.
        READ TABLE GT_0090 WITH KEY PRODH = LS_0070-PRODH
                                    BINARY SEARCH.
        GT_0070-ZPRODH_GROUP = GT_0090-ZPRODH_GROUP.
      ENDIF.

      APPEND GT_0070.
      CLEAR : GT_0070, LS_0070.
    ENDLOOP.
    CLEAR GT_0070_KEY.
  ENDLOOP.

  PERFORM GET_INFO_RECORD_PRICE TABLES GT_0070.

*STEP 3 : STEP 2을 바탕으로 HQ 판매단가를 가져온다
*HQ이고 매입단가가 있는경우에만 판매단가 대상
  _CLEAR LT_DATA.
  LOOP AT GT_0070.
    MOVE-CORRESPONDING GT_0070 TO LT_DATA.
    APPEND LT_DATA. CLEAR LT_DATA.
  ENDLOOP.

  IF LT_DATA[] IS NOT INITIAL.
    SELECT A~VKORG, A~MATNR, A~VTWEG, A~KUNNR,
           A~KUNWE, A~KSCHL, A~DATAB, A~DATBI,
           B~KBETR, B~KONWA, B~KPEIN, B~KMEIN
           INTO TABLE @DATA(LT_A903)
           FROM A903 AS A INNER JOIN KONP AS B
           ON   A~KNUMH EQ B~KNUMH
           FOR ALL ENTRIES IN @LT_DATA
           WHERE A~VKORG    EQ '1001'
           AND   A~KUNNR    EQ @LT_DATA-ZKUNNR_IC
           AND   A~KUNWE    EQ @LT_DATA-ZKUNNR_S
           AND   A~VTWEG    EQ @LT_DATA-VTWEG
           AND   A~DATBI    GE @GV_DATE
           AND   A~DATAB    LE @GV_DATE
           AND   A~KSCHL    EQ 'PR00'
           AND   B~LOEVM_KO EQ @ABAP_OFF
           AND   A~MATNR    EQ @LT_DATA-MATNR.

    SORT LT_A903 BY KUNNR KUNWE VTWEG MATNR.

    LOOP AT GT_0070.
      READ TABLE LT_A903 INTO DATA(LS_A903) WITH KEY KUNNR = GT_0070-ZKUNNR_SUB
                                                     KUNWE = GT_0070-ZKUNNR_S
                                                     VTWEG = GT_0070-VTWEG
                                                     MATNR = GT_0070-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_0070-ZKBETR_SD_OLD = LS_A903-KBETR / LS_A903-KPEIN.
        GT_0070-ZKONWA_SD = LS_A903-KONWA.
        GT_0070-ZKMEIN_SD = LS_A903-KPEIN.
        GT_0070-KMEIN_3 = LS_A903-KMEIN.
        MODIFY GT_0070 TRANSPORTING ZKBETR_SD_OLD ZKONWA_SD ZKMEIN_SD KMEIN_3 ZKUNNR_S.
      ENDIF.
      CLEAR LS_A903.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_FOB
*&---------------------------------------------------------------------*
FORM READ_FOB .

  DATA : LV_CHK.

  CHECK GT_FOB_305_KEY[] IS NOT INITIAL.

*STEP 1 기준금액 조회
  SELECT A~VKORG, A~MATNR, A~KUNNR, A~KSCHL,
         A~DATAB, A~DATBI, A~VTWEG,
         B~KBETR, B~KONWA,B~KPEIN, B~KMEIN,
         C~PRDHA AS PRODH
         INTO TABLE @DATA(LT_A305)
         FROM A305 AS A INNER JOIN KONP AS B
                                        ON A~KNUMH EQ B~KNUMH
                        INNER JOIN MARA AS C
                                        ON A~MATNR EQ C~MATNR
         FOR ALL ENTRIES IN @GT_FOB_305_KEY
         WHERE A~VKORG    EQ '1001'
         AND   A~KUNNR    EQ @GT_FOB_305_KEY-KUNNR
         AND   A~VTWEG    EQ '30'
         AND   A~DATBI    GE @GV_DATE
         AND   A~DATAB    LE @GV_DATE
         AND   A~KSCHL    EQ 'PR00'
         AND   B~LOEVM_KO EQ @ABAP_OFF
         AND   C~LVORM    EQ @ABAP_OFF
         AND   C~PRDHA    IN @GR_PRODH.

  SELECT ZKUNNR_IC,
         VTWEG,
         KUNWE,
         EKORG
  INTO TABLE @DATA(LT_0060)
  FROM ZSDT0060.
  SORT LT_0060 BY ZKUNNR_IC VTWEG.


*-기준금액이 있는 SKU별로 인터널 테이블 구성
  _CLEAR GT_FOB_305.
  LOOP AT GT_FOB_305_KEY.
    LOOP AT LT_A305 INTO DATA(LS_A305) WHERE KUNNR = GT_FOB_305_KEY-KUNNR.

      PERFORM CHECK_DATA TABLES GT_FOB_305_KEY
                          USING LS_A305-MATNR LS_A305-PRODH
                          CHANGING LV_CHK.

      CHECK LV_CHK IS INITIAL.

      MOVE-CORRESPONDING GT_FOB_305_KEY TO GT_FOB_305.
      GT_FOB_305-ZBASE_PRICE = LS_A305-KBETR / LS_A305-KPEIN.
      GT_FOB_305-KONWA_1 = LS_A305-KONWA.
      GT_FOB_305-KPEIN_1 = LS_A305-KPEIN.
      GT_FOB_305-KMEIN_1 = LS_A305-KMEIN.
      GT_FOB_305-MATNR   = LS_A305-MATNR.

      READ TABLE LT_0060 INTO DATA(LS_0060) WITH KEY ZKUNNR_IC = LS_A305-KUNNR
                                                     VTWEG     = LS_A305-VTWEG BINARY SEARCH.
      GT_FOB_305-EKORG = LS_0060-EKORG.

      IF GT_FOB_305-ZPRODH_GROUP IS INITIAL.
        CLEAR GT_0090.
        READ TABLE GT_0090 WITH KEY PRODH = LS_A305-PRODH
                                    BINARY SEARCH.
        GT_FOB_305-ZPRODH_GROUP = GT_0090-ZPRODH_GROUP.
      ENDIF.

      APPEND GT_FOB_305.
      CLEAR : GT_FOB_305, LS_A305, LS_0060.
    ENDLOOP.
    CLEAR GT_FOB_305_KEY.
  ENDLOOP.

  PERFORM GET_INFO_RECORD_PRICE TABLES GT_FOB_305.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_FOB_INTERCOM
*&---------------------------------------------------------------------*
FORM READ_FOB_INTERCOM .

  DATA : LV_CHK.

  RANGES : LR_KUNWE FOR A903-KUNWE.

  CHECK GT_FOB_903_KEY[] IS NOT INITIAL.

*ITALY SHIP TO 제거
  _CLEAR LR_KUNWE.
  LOOP AT GT_0050.
    CHECK GT_0050-ZKUNNR_S IS NOT INITIAL.

    CHECK ( GT_0050-VTWEG EQ '10' AND GT_0050-ZKUNNR_IC EQ '0000002000' )
     OR  GT_0050-LIFNR EQ '0000040020'.
    _RANGE LR_KUNWE 'I' 'EQ' GT_0050-ZKUNNR_S ''.
  ENDLOOP.

*STEP 1 기준금액 조회
  SELECT A~VKORG, A~MATNR, A~VTWEG, A~KUNNR,
         A~KUNWE, A~KSCHL, A~DATAB, A~DATBI,
         B~KBETR, B~KONWA, B~KPEIN, B~KMEIN,
         C~PRDHA AS PRODH
         INTO TABLE @DATA(LT_A903)
         FROM A903 AS A INNER JOIN KONP AS B
         ON   A~KNUMH EQ B~KNUMH
                        INNER JOIN MARA AS C
         ON   A~MATNR EQ C~MATNR
*         INNER JOIN ZSDT0090 AS D
*         ON   C~PRDHA EQ D~PRODH
         FOR ALL ENTRIES IN @GT_FOB_903_KEY
         WHERE A~VKORG    EQ '1001'
         AND   A~KUNNR    EQ @GT_FOB_903_KEY-KUNNR
         AND   A~KUNWE    NOT IN  @LR_KUNWE
         AND   A~DATBI    GE @GV_DATE
         AND   A~DATAB    LE @GV_DATE
         AND   A~KSCHL    EQ 'PR00'
         AND   B~LOEVM_KO EQ @ABAP_OFF
         AND   C~LVORM    EQ @ABAP_OFF
         AND   C~PRDHA    IN @GR_PRODH.
  IF SY-SUBRC = 0.
    SORT LT_A903 BY KUNNR.
  ENDIF.

  SELECT ZKUNNR_IC,
         VTWEG,
         KUNWE,
         EKORG
  INTO TABLE @DATA(LT_0060)
  FROM ZSDT0060
  WHERE KUNNR EQ @SPACE.
  SORT LT_0060 BY ZKUNNR_IC VTWEG KUNWE.

  SELECT ZKUNNR_IC,
         KUNNR,
         VTWEG,
         VKORG,
         ZKUNNR_S,
         ZKUNNR_SUB
  INTO TABLE @DATA(LT_0042)
  FROM ZSDT0042.
  SORT LT_0042 BY ZKUNNR_IC ZKUNNR_SUB ZKUNNR_S VTWEG.

*-기준금액이 있는 SKU별로 인터널 테이블 구성
  _CLEAR GT_FOB_903.
  LOOP AT GT_FOB_903_KEY.
    READ TABLE LT_A903 WITH KEY KUNNR = GT_FOB_903_KEY-KUNNR
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      LOOP AT LT_A903 INTO DATA(LS_A903) FROM SY-TABIX.

        IF LS_A903-KUNNR NE GT_FOB_903_KEY-KUNNR.
          EXIT.
        ENDIF.
*마스터(ZSDT0020)에 있는 대상인지 비교
        PERFORM CHECK_DATA TABLES GT_FOB_903_KEY
                            USING LS_A903-MATNR LS_A903-PRODH
                            CHANGING LV_CHK.

        CHECK LV_CHK IS INITIAL.
*-mDDP용 판매단가 제외 로직
        READ TABLE LT_0042 INTO DATA(LS_0042) WITH KEY ZKUNNR_IC  = GT_FOB_903_KEY-ZKUNNR_IC
                                                       ZKUNNR_SUB = GT_FOB_903_KEY-KUNNR
                                                       ZKUNNR_S   = LS_A903-KUNWE
                                                       VTWEG      = LS_A903-VTWEG BINARY SEARCH.
        CHECK SY-SUBRC NE 0.
        CLEAR GT_FOB_903.
        MOVE-CORRESPONDING GT_FOB_903_KEY TO GT_FOB_903.
        GT_FOB_903-ZBASE_PRICE  = LS_A903-KBETR / LS_A903-KPEIN.
        GT_FOB_903-KONWA_1  = LS_A903-KONWA.
        GT_FOB_903-KPEIN_1  = LS_A903-KPEIN.
        GT_FOB_903-KMEIN_1  = LS_A903-KMEIN.
        GT_FOB_903-MATNR    = LS_A903-MATNR.
        GT_FOB_903-ZKUNNR_S = LS_A903-KUNWE.
        GT_FOB_903-VTWEG    = LS_A903-VTWEG.
        READ TABLE LT_0060 INTO DATA(LS_0060) WITH KEY ZKUNNR_IC = GT_FOB_903-KUNNR
                                                        VTWEG    = GT_FOB_903-VTWEG
                                                        KUNWE    = GT_FOB_903-ZKUNNR_S BINARY SEARCH.
        CHECK SY-SUBRC EQ 0.
        GT_FOB_903-EKORG = LS_0060-EKORG.
* HQ 매입단가를 가져온다
*        PERFORM GET_INFO_RECORD TABLES GT_FOB_903.

        IF GT_FOB_903-ZPRODH_GROUP IS INITIAL.
          CLEAR GT_0090.
          READ TABLE GT_0090 WITH KEY PRODH = LS_A903-PRODH
                                      BINARY SEARCH.
          GT_FOB_903-ZPRODH_GROUP = GT_0090-ZPRODH_GROUP.
        ENDIF.

        APPEND GT_FOB_903.
        CLEAR : GT_FOB_903, LS_A903, LS_0042, LS_0060.
      ENDLOOP.
    ENDIF.
    CLEAR GT_FOB_903_KEY.
  ENDLOOP.

  PERFORM GET_INFO_RECORD_PRICE TABLES GT_FOB_903.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MERGE_DATA
*&---------------------------------------------------------------------*
FORM MERGE_DATA .

  RANGES: LR_MATNR FOR MARA-MATNR,
          LR_LIFNR FOR LFA1-LIFNR,
          LR_KUNNR FOR KNA1-KUNNR,
          LR_EKORG FOR EKKO-EKORG,
          LR_VKORG FOR VBAK-VKORG,
          LR_VTWEG FOR VBAK-VTWEG,
          LR_WERKS FOR T001W-WERKS.

  _CLEAR GT_LIST.
  LOOP AT GT_MDDP.
    MOVE-CORRESPONDING GT_MDDP TO GT_LIST.
    GT_LIST-VKORG = '1001'.
    GT_LIST-WERKS = '1001'.
    GT_LIST-KUNWE = GT_MDDP-ZKUNNR_S.
    GT_LIST-ZKUNNR_SUB = GT_MDDP-ZKUNNR_SUB.
    GT_LIST-FLAG  = 'MDDP'.
    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  LOOP AT GT_0070.
    MOVE-CORRESPONDING GT_0070 TO GT_LIST.
    GT_LIST-VKORG = '1001'.
    GT_LIST-WERKS = '1001'.
    GT_LIST-KUNWE = GT_0070-ZKUNNR_S.
    GT_LIST-KUNNR_TXT = GT_0070-ZKUNNR_DESC.
    GT_LIST-ZKUNNR_SUB = GT_0070-ZKUNNR_SUB.
    GT_LIST-FLAG  = 'MDDP'.
    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.


  LOOP AT GT_FOB_903.
    MOVE-CORRESPONDING GT_FOB_903 TO GT_LIST.
    GT_LIST-WERKS = '1001'.
    GT_LIST-FLAG  = 'FOB'.
    CLEAR : GT_LIST-VKORG, GT_LIST-VTWEG.
    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  LOOP AT GT_FOB_305.
    MOVE-CORRESPONDING GT_FOB_305 TO GT_LIST.
    GT_LIST-WERKS = '1001'.
    GT_LIST-FLAG  = 'FOB'.
    CLEAR : GT_LIST-VKORG, GT_LIST-VTWEG.
    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  SORT GT_LIST.

  DATA : BEGIN OF LT_WMATNR_C OCCURS 0,
           MATNR LIKE MARC-MATNR,
           WERKS LIKE MARC-WERKS,
         END OF LT_WMATNR_C.


  _CLEAR : LR_MATNR, LR_LIFNR, LR_KUNNR, LR_KUNNR,
           LR_EKORG, LR_VKORG, LR_VTWEG.
  LOOP AT GT_LIST.
    _RANGE : LR_MATNR  'I' 'EQ' GT_LIST-MATNR '',
             LR_LIFNR  'I' 'EQ' GT_LIST-LIFNR '',
             LR_KUNNR  'I' 'EQ' GT_LIST-ZKUNNR_IC '',
             LR_KUNNR  'I' 'EQ' GT_LIST-KUNNR '',
             LR_KUNNR  'I' 'EQ' GT_LIST-KUNWE '',
             LR_KUNNR  'I' 'EQ' GT_LIST-ZKUNNR_SUB '',
             LR_EKORG  'I' 'EQ' GT_LIST-EKORG '',
             LR_VKORG  'I' 'EQ' GT_LIST-VKORG '',
             LR_VTWEG  'I' 'EQ' GT_LIST-VTWEG ''.

    CLEAR : LT_WMATNR_C.
    LT_WMATNR_C-MATNR = GT_LIST-MATNR.
    LT_WMATNR_C-WERKS = GT_LIST-WERKS.
    COLLECT LT_WMATNR_C.
  ENDLOOP.


  SELECT MATNR, MAKTX FROM MAKT INTO TABLE @DATA(LT_MAKT)
  WHERE MATNR IN @LR_MATNR AND SPRAS EQ @SY-LANGU.
  SORT LT_MAKT BY MATNR.

  SELECT MATNR FROM MARA INTO TABLE @DATA(LT_MARA)
  WHERE MATNR IN @LR_MATNR AND MSTAE EQ '03'.
  SORT LT_MARA BY MATNR.

  SELECT MATNR, WERKS FROM MARC INTO TABLE @DATA(LT_MARC)
  FOR ALL ENTRIES IN @LT_WMATNR_C
  WHERE MATNR EQ @LT_WMATNR_C-MATNR
    AND WERKS EQ @LT_WMATNR_C-WERKS
    AND MMSTA EQ '03'.
  SORT LT_MARC BY MATNR WERKS.

  SELECT LIFNR, NAME1 FROM LFA1 INTO TABLE @DATA(LT_LFA1)
  WHERE LIFNR IN @LR_LIFNR.
  SORT LT_LFA1 BY LIFNR.

  SELECT EKORG, EKOTX FROM T024E INTO TABLE @DATA(LT_T024E)
  WHERE EKORG IN @LR_EKORG.
  SORT LT_T024E BY EKORG.

  SELECT VKORG, VTEXT FROM TVKOT INTO TABLE @DATA(LT_TVKOT)
  WHERE VKORG IN @LR_VKORG AND SPRAS = @SY-LANGU.
  SORT LT_TVKOT BY VKORG.

  SELECT VTWEG, VTEXT FROM TVTWT INTO TABLE @DATA(LT_TVTWT)
  WHERE VTWEG IN @LR_VTWEG AND SPRAS = @SY-LANGU.
  SORT LT_TVTWT BY VTWEG.

  SELECT WERKS, NAME1 FROM T001W INTO TABLE @DATA(LT_T001W)
  WHERE WERKS IN @LR_WERKS.
  SORT LT_T001W BY WERKS.

  SELECT KUNNR, NAME1 FROM KNA1 INTO TABLE @DATA(LT_KNA1)
  WHERE KUNNR IN @LR_KUNNR.
  SORT LT_KNA1 BY KUNNR.

  LOOP AT GT_LIST.
*--MARGIN RATE은 SKU가 입력되었으면 우선-> 없으면 ZPRODH_GROUP기준으로 읽으면됨.
    IF GT_LIST-FLAG = 'FOB'.
      CLEAR GT_0020.
      READ TABLE GT_0020 WITH KEY KUNNR     = GT_LIST-KUNNR
                                  ZKUNNR_IC = GT_LIST-ZKUNNR_IC
                                  LIFNR     = GT_LIST-LIFNR
                                  MATNR     = GT_LIST-MATNR.
      GT_LIST-ZMARGIN_MM = GT_0020-ZMARGIN.
      IF GT_LIST-ZMARGIN_MM IS INITIAL.
        CLEAR GT_0020.
        READ TABLE GT_0020 WITH KEY KUNNR        = GT_LIST-KUNNR
                                    ZKUNNR_IC    = GT_LIST-ZKUNNR_IC
                                    LIFNR        = GT_LIST-LIFNR
                                    ZPRODH_GROUP = GT_LIST-ZPRODH_GROUP
                                    MATNR        = SPACE.
        GT_LIST-ZMARGIN_MM = GT_0020-ZMARGIN.
      ENDIF.
    ENDIF.
    IF GT_LIST-FLAG = 'MDDP'.
      CLEAR GT_0020.
      READ TABLE GT_0020 WITH KEY KUNNR     = GT_LIST-KUNNR
                                  ZKUNNR_IC = '0000001000'
                                  LIFNR     = GT_LIST-LIFNR
                                  MATNR     = GT_LIST-MATNR.
      GT_LIST-ZMARGIN_MM = GT_0020-ZMARGIN.
      IF GT_LIST-ZMARGIN_MM IS INITIAL.
        CLEAR GT_0020.
        READ TABLE GT_0020 WITH KEY KUNNR        = GT_LIST-KUNNR
                                    ZKUNNR_IC    = '0000001000'
                                    LIFNR        = GT_LIST-LIFNR
                                    ZPRODH_GROUP = GT_LIST-ZPRODH_GROUP
                                    MATNR        = SPACE.
        GT_LIST-ZMARGIN_MM = GT_0020-ZMARGIN.
      ENDIF.
      CLEAR GT_0020.
      READ TABLE GT_0020 WITH KEY KUNNR = GT_LIST-KUNNR
                              ZKUNNR_IC = GT_LIST-ZKUNNR_SUB
                              LIFNR     = GT_LIST-LIFNR
                              MATNR     = GT_LIST-MATNR.

      GT_LIST-ZMARGIN_SD = GT_0020-ZMARGIN.
      IF GT_LIST-ZMARGIN_SD IS INITIAL.
        CLEAR GT_0020.
        READ TABLE GT_0020 WITH KEY KUNNR        = GT_LIST-KUNNR
                                    ZKUNNR_IC    = GT_LIST-ZKUNNR_SUB
                                    LIFNR        = GT_LIST-LIFNR
                                    ZPRODH_GROUP = GT_LIST-ZPRODH_GROUP
                                    MATNR        = SPACE.
        GT_LIST-ZMARGIN_SD = GT_0020-ZMARGIN.
      ENDIF.
      IF GT_LIST-ZMARGIN_SD IS INITIAL.
        CLEAR : GT_LIST-VKORG, GT_LIST-VTWEG, GT_LIST-ZKUNNR_SUB, GT_LIST-KUNWE.
      ELSE.
        GT_LIST-ZKBETR_SD = GT_LIST-ZBASE_PRICE * GT_LIST-ZMARGIN_SD / 100.
      ENDIF.
    ENDIF.

    GT_LIST-ZKBETR_MM = GT_LIST-ZBASE_PRICE * GT_LIST-ZMARGIN_MM / 100.

    READ TABLE LT_MAKT  INTO DATA(LS_MAKT) WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-MATNR_TXT = LS_MAKT-MAKTX.
    ENDIF.
*    READ TABLE LT_MARA  INTO DATA(LS_MARA) WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      GT_LIST-LVORM = 'X'.
*    ENDIF.

* Check Discontinued material
    READ TABLE LT_MARC WITH KEY MATNR = GT_LIST-MATNR
                                WERKS = GT_LIST-WERKS
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      GT_LIST-LVORM = 'X'.
      GT_LIST-ICON  = ICON_LED_RED.
      GT_LIST-MESSAGE = 'Discontinue can not be processed.'.
    ELSE.
      GT_LIST-ICON  = ICON_LED_YELLOW.
    ENDIF.

    READ TABLE LT_LFA1  INTO DATA(LS_LFA1) WITH KEY LIFNR = GT_LIST-LIFNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-LIFNR_TXT = LS_LFA1-NAME1.
    ENDIF.
    READ TABLE LT_T024E INTO DATA(LS_T024E) WITH KEY EKORG = GT_LIST-EKORG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-EKORG_TXT = LS_T024E-EKOTX.
    ENDIF.
    READ TABLE LT_TVKOT INTO DATA(LS_TVKOT) WITH KEY VKORG = GT_LIST-VKORG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-VKORG_TXT = LS_TVKOT-VTEXT.
    ENDIF.
    READ TABLE LT_TVTWT INTO DATA(LS_TVTWT) WITH KEY VTWEG = GT_LIST-VTWEG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-VTWEG_TXT = LS_TVTWT-VTEXT.
    ENDIF.
    READ TABLE LT_T001W INTO DATA(LS_T001W) WITH KEY WERKS = GT_LIST-WERKS BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-WERKS_TXT = LS_T001W-NAME1.
    ENDIF.
    READ TABLE LT_KNA1  INTO DATA(LS_KNA1) WITH KEY KUNNR = GT_LIST-KUNNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-KUNNR_TXT = LS_KNA1-NAME1.
    ENDIF.
    READ TABLE LT_KNA1  INTO DATA(LS_KNA1B) WITH KEY KUNNR = GT_LIST-KUNWE BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-KUNWE_TXT = LS_KNA1B-NAME1.
    ENDIF.
    READ TABLE LT_KNA1  INTO DATA(LS_KNA1C) WITH KEY KUNNR = GT_LIST-ZKUNNR_SUB BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-ZKUNNR_IC_TXT = LS_KNA1C-NAME1.
    ENDIF.
    GT_LIST-ZKONWA_MM = GT_LIST-KONWA_1.
    GT_LIST-ZKONWA_SD = GT_LIST-KONWA_1.

    MODIFY GT_LIST TRANSPORTING ICON LVORM MESSAGE VKORG VTWEG ZKUNNR_SUB KUNWE
                                ZKBETR_MM ZKBETR_SD ZMARGIN_MM ZMARGIN_SD
                                MATNR_TXT LIFNR_TXT EKORG_TXT VKORG_TXT
                                VTWEG_TXT WERKS_TXT KUNNR_TXT KUNWE_TXT
                                ZKUNNR_IC_TXT ZKONWA_MM ZKONWA_SD.
  ENDLOOP.

*ZEXCEPT(제외) 로직 적용,
*MATNR(SKU) 단위 마진율 적용
  LOOP AT GT_DATA.
    IF GT_DATA-ZEXCEPT = 'X'.
      IF GT_DATA-MATNR IS NOT INITIAL.
        DELETE GT_LIST WHERE ZKUNNR_IC = GT_DATA-ZKUNNR_IC
                         AND KUNNR     = GT_DATA-KUNNR
                         AND LIFNR     = GT_DATA-LIFNR
                         AND MATNR     = GT_DATA-MATNR.
        CONTINUE.
      ENDIF.
      IF GT_DATA-ZPRODH_GROUP IS NOT INITIAL.
        DELETE GT_LIST WHERE ZKUNNR_IC    = GT_DATA-ZKUNNR_IC
                         AND KUNNR        = GT_DATA-KUNNR
                         AND LIFNR        = GT_DATA-LIFNR
                         AND ZPRODH_GROUP = GT_DATA-ZPRODH_GROUP.
        CONTINUE.
      ENDIF.
    ELSE.
      CHECK GV_ZTYPE NE 'I'.
      CHECK GV_ZTYPE NE 'M'.
      IF GT_DATA-MATNR IS NOT INITIAL.
        DELETE GT_LIST WHERE ZKUNNR_IC  = GT_DATA-ZKUNNR_IC
                         AND KUNNR      = GT_DATA-KUNNR
                         AND LIFNR      = GT_DATA-LIFNR
                         AND MATNR      = GT_DATA-MATNR
                         AND ZMARGIN_MM  <> GT_DATA-ZMARGIN.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT GT_LIST BY KUNNR ZPRODH_GROUP MATNR LIFNR EKORG ZKBETR_SD_OLD DESCENDING.
  DELETE ADJACENT DUPLICATES FROM GT_LIST COMPARING KUNNR ZPRODH_GROUP MATNR LIFNR EKORG.

*  SKU로 등록한 대상 표시
  LOOP AT GT_DATA WHERE MATNR IS NOT INITIAL.
    LOOP AT GT_LIST WHERE ZKUNNR_IC = GT_DATA-ZKUNNR_IC
                      AND KUNNR     = GT_DATA-KUNNR
                      AND LIFNR     = GT_DATA-LIFNR
                      AND MATNR     = GT_DATA-MATNR.
      GT_LIST-CHK_MATNR = 'X'.
      MODIFY GT_LIST TRANSPORTING CHK_MATNR.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_PRICE_BDC1
*&---------------------------------------------------------------------*
FORM MAKE_PRICE_BDC1 USING PV_ZSUCCESS_SD.

  DATA: LV_KBETR    TYPE CHAR14,
        LV_KPEIN    TYPE CHAR5,
        LV_DATAB    TYPE RV13A-DATAB,
        LV_DATBI    TYPE RV13A-DATBI,
        LV_MSG(255).

  _CLEAR : GT_BDCDATA, GT_BDCMSG.
  PERFORM BDCDATA_SET USING :
      'X'  'SAPMV13A'         '0100',
      ' '  'RV13A-KSCHL'      'PR00',
      ' '  'BDC_OKCODE'       '/00',
      'X'  'SAPLV14A'         '0100'.

  PERFORM BDCDATA_SET USING :
         ' '  'RV130-SELKZ(01)'   ' ',
         ' '  'RV130-SELKZ(04)'   'X',
         ' '  'BDC_OKCODE'       '=WEIT',
         'X'  'SAPMV13A'         '1903',
         ' '  'KOMG-VKORG'       '1001',
         ' '  'KOMG-VTWEG'       GT_LIST-VTWEG,
         ' '  'KOMG-KUNNR'       GT_LIST-ZKUNNR_SUB,
         ' '  'KOMG-KUNWE'       GT_LIST-KUNWE.

  CLEAR LV_KBETR.
  WRITE GT_LIST-ZKBETR_SD TO LV_KBETR CURRENCY GT_LIST-KONWA_1.
  CLEAR LV_KPEIN.
  WRITE GT_LIST-KPEIN_1 TO LV_KPEIN UNIT GT_LIST-KMEIN_1.

  PERFORM DATE_SETTING USING GV_DATE
                    CHANGING LV_DATAB.
  PERFORM DATE_SETTING USING P_END
                    CHANGING LV_DATBI.

  PERFORM BDCDATA_SET USING :
      ' '  'KOMG-MATNR(01)'  GT_LIST-MATNR,
      ' '  'KONP-KBETR(01)'  LV_KBETR,
      ' '  'KONP-KONWA(01)'  GT_LIST-KONWA_1,
      ' '  'KONP-KPEIN(01)'  LV_KPEIN,
      ' '  'KONP-KMEIN(01)'  GT_LIST-KMEIN_1,
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
    GT_LIST-ICON = ICON_LED_RED.
    GT_LIST-MESSAGE = LV_MSG.
    PV_ZSUCCESS_SD = 'X'.
  ELSE.
    ADD 1 TO GV_SUCCESS.
    GT_LIST-ICON = ICON_LED_GREEN.
    PV_ZSUCCESS_SD = 'O'.

*동일 조건의 가격정보가 존재하는지 확인을 위한 테이블에 APPEND
    CLEAR GT_CHK_PRICE.
    GT_CHK_PRICE-VKORG = GT_LIST-VKORG.
    GT_CHK_PRICE-VTWEG = GT_LIST-VTWEG.
    GT_CHK_PRICE-KUNNR = GT_LIST-ZKUNNR_SUB.
    GT_CHK_PRICE-KUNWE = GT_LIST-KUNWE.
    GT_CHK_PRICE-MATNR = GT_LIST-MATNR.
    APPEND   GT_CHK_PRICE.
    SORT GT_CHK_PRICE BY VKORG VTWEG KUNNR KUNWE MATNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_PRICE_BDC2
*&---------------------------------------------------------------------*
FORM MAKE_PRICE_BDC2 USING PV_ZSUCCESS_SD.

  DATA: LV_KBETR    TYPE CHAR14,
        LV_KPEIN    TYPE CHAR5,
        LV_DATAB    TYPE RV13A-DATAB,
        LV_DATBI    TYPE RV13A-DATBI,
        LV_MSG(255).

  _CLEAR : GT_BDCDATA, GT_BDCMSG.
  PERFORM BDCDATA_SET USING :
      'X'  'SAPMV13A'         '0100',
      ' '  'RV13A-KSCHL'      'PR00',
      ' '  'BDC_OKCODE'       '/00',
      'X'  'SAPLV14A'         '0100'.

  PERFORM BDCDATA_SET USING :
         ' '  'RV130-SELKZ(01)'   'X',
         ' '  'BDC_OKCODE'       '=WEIT',
         'X'  'SAPMV13A'         '1305',
         ' '  'KOMG-VKORG'       '1001',
         ' '  'KOMG-VTWEG'       GT_LIST-VTWEG,
         ' '  'KOMG-KUNNR'       GT_LIST-ZKUNNR_SUB.


  CLEAR LV_KBETR.
  WRITE GT_LIST-ZKBETR_SD TO LV_KBETR CURRENCY GT_LIST-ZKONWA_SD.
  CLEAR LV_KPEIN.
  WRITE GT_LIST-ZKMEIN_SD TO LV_KPEIN UNIT GT_LIST-KMEIN_3.

  PERFORM DATE_SETTING USING GV_DATE
                    CHANGING LV_DATAB.
  PERFORM DATE_SETTING USING P_END
                    CHANGING LV_DATBI.

  PERFORM BDCDATA_SET USING :
      ' '  'KOMG-MATNR(01)'  GT_LIST-MATNR,
      ' '  'KONP-KBETR(01)'  LV_KBETR,
      ' '  'KONP-KONWA(01)'  GT_LIST-ZKONWA_SD,
      ' '  'KONP-KPEIN(01)'  LV_KPEIN,
      ' '  'KONP-KMEIN(01)'  GT_LIST-KMEIN_3,
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
    GT_LIST-ICON = ICON_LED_RED.
    GT_LIST-MESSAGE = LV_MSG.
    PV_ZSUCCESS_SD = 'X'.
  ELSE.
    ADD 1 TO GV_SUCCESS.
    GT_LIST-ICON = ICON_LED_GREEN.
    PV_ZSUCCESS_SD = 'O'.

*동일 조건의 가격정보가 존재하는지 확인을 위한 테이블에 APPEND
    CLEAR GT_CHK_PRICE.
    GT_CHK_PRICE-VKORG = GT_LIST-VKORG.
    GT_CHK_PRICE-VTWEG = GT_LIST-VTWEG.
    GT_CHK_PRICE-KUNNR = GT_LIST-ZKUNNR_SUB.
    GT_CHK_PRICE-KUNWE = GT_LIST-KUNWE.
    GT_CHK_PRICE-MATNR = GT_LIST-MATNR.
    APPEND   GT_CHK_PRICE.
    SORT GT_CHK_PRICE BY VKORG VTWEG KUNNR KUNWE MATNR.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_PRICE_BDC2_VK12
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SAVE_ZSUCCESS_SD
*&---------------------------------------------------------------------*
FORM MAKE_PRICE_BDC_VK12  USING    PV_ZSUCCESS_SD.

  DATA: LV_KBETR    TYPE CHAR14,
        LV_KPEIN    TYPE CHAR5,
        LV_DATAB    TYPE RV13A-DATAB,
        LV_DATBI    TYPE RV13A-DATBI,
        LV_MSG(255).

  _CLEAR : GT_BDCDATA, GT_BDCMSG.
  PERFORM BDCDATA_SET USING :
      'X'  'SAPMV13A'         '0100',
      ' '  'RV13A-KSCHL'      'PR00',
      ' '  'BDC_OKCODE'       '/00'.

  PERFORM BDCDATA_SET USING :
      'X'  'SAPLV14A'         '0100',
      ' '  'BDC_OKCODE'       '=WEIT',
      ' '  'RV130-SELKZ(04)'   'X'.

  PERFORM DATE_SETTING USING GV_DATE
                    CHANGING LV_DATAB.
  PERFORM DATE_SETTING USING P_END
                    CHANGING LV_DATBI.

  PERFORM BDCDATA_SET USING :
         'X'  'RV13A903'         '1000',
         ' '  'BDC_OKCODE'       '/00',
         ' '  'F001'             GT_LIST-VKORG,      " Sales org.
         ' '  'F002'             GT_LIST-VTWEG,      " Distribution Channel
         ' '  'F003'             GT_LIST-ZKUNNR_SUB, " Sold-to party
         ' '  'F004'             GT_LIST-KUNWE,      " Ship-to party
         ' '  'F005-LOW'         GT_LIST-MATNR,      " Material
         ' '  'SEL_DATE'         LV_DATAB.           " Valid On


  PERFORM BDCDATA_SET USING :
         'X'  'RV13A903'         '1000',
         ' '  'BDC_OKCODE'       '=ONLI',
         ' '  'F001'             GT_LIST-VKORG,      " Sales org.
         ' '  'F002'             GT_LIST-VTWEG,      " Distribution Channel
         ' '  'F003'             GT_LIST-ZKUNNR_SUB, " Sold-to party
         ' '  'F004'             GT_LIST-KUNWE,      " Ship-to party
         ' '  'F005-LOW'         GT_LIST-MATNR,      " Material
         ' '  'SEL_DATE'         LV_DATAB.           " Valid On

  CLEAR LV_KBETR.
  WRITE GT_LIST-ZKBETR_SD TO LV_KBETR CURRENCY GT_LIST-ZKONWA_SD.
  PERFORM BDCDATA_SET USING :
         'X'  'SAPMV13A'         '1903',
         ' '  'BDC_OKCODE'       '/00',
         ' '  'KONP-KBETR(01)'   LV_KBETR,
         ' '  'RV13A-DATAB(01)'  LV_DATAB,
         ' '  'RV13A-DATBI(01)'  LV_DATBI,
         ' '  'BDC_OKCODE'       '=SICH'.

** CALL TRANSACTION
  CLEAR : GT_BDCMSG, GT_BDCMSG[].
  CALL TRANSACTION 'VK12'
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
    GT_LIST-ICON = ICON_LED_RED.
    GT_LIST-MESSAGE = LV_MSG.
    PV_ZSUCCESS_SD = 'X'.
  ELSE.
    ADD 1 TO GV_SUCCESS.
    GT_LIST-ICON = ICON_LED_GREEN.
    PV_ZSUCCESS_SD = 'O'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDCDATA_SET
*&---------------------------------------------------------------------*
FORM BDCDATA_SET   USING P_START P_OBJECT P_VALUE.

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
*& Form DATE_SETTING
*&---------------------------------------------------------------------*
FORM DATE_SETTING USING PV_S_DATUM
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
*& Form GET_INFO_RECORD
*&---------------------------------------------------------------------*
FORM GET_INFO_RECORD  TABLES PT_TAB STRUCTURE GT_MDDP.

  DATA : LT_DATA LIKE TABLE OF ZSDS0050 WITH HEADER LINE.

  _CLEAR LT_DATA.
  CALL FUNCTION 'ZSD_VENDOR_PRICE_BY_MATERIAL'
    EXPORTING
      IV_WERKS     = '1001'
      IV_EKORG     = PT_TAB-EKORG
      IV_KSCHL     = 'PB00'
      IV_MATNR     = PT_TAB-MATNR
      IV_LIFNR     = PT_TAB-LIFNR
      IV_VALIDDATE = GV_DATE
    TABLES
      ET_DATA      = LT_DATA.

  READ TABLE LT_DATA INDEX 1.
  IF LT_DATA-KBETR IS NOT INITIAL.
    PT_TAB-ZKBETR_MM_OLD = LT_DATA-KBETR.
    PT_TAB-ZKONWA_MM     = LT_DATA-WAERS.
    PT_TAB-ZKMEIN_MM     = LT_DATA-KPEIN.
    PT_TAB-KMEIN_2       = LT_DATA-KMEIN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA  TABLES   PT_TAB STRUCTURE GT_FOB_305_KEY
                 USING    PV_MATNR
                          PV_PRODH
                          PV_CHK.

  DATA : LV_CHK,
             LV_GROUP LIKE ZSDT0090-ZPRODH_GROUP.

  CLEAR : PV_CHK, LV_CHK, LV_GROUP.

  CASE PT_TAB-ZPRODH_GROUP.
*ALL일 경우 SORG, CUTOMER, DCHL만 같으면 모두 대상
    WHEN 'ALL'.
*빈값일 경우 SKU로 등록한 마진, 비교해서 같으면 대상
    WHEN SPACE.
      IF PT_TAB-MATNR EQ PV_MATNR.
        CLEAR GT_0090.
        READ TABLE GT_0090 WITH KEY PRODH = PV_PRODH
                                    BINARY SEARCH.
        IF SY-SUBRC = 0.
        ELSE.
          PV_CHK = 'X'.
        ENDIF.
      ELSE.
        PV_CHK = 'X'.
      ENDIF.
*값이 있을경우 같은 대상인지 확인
    WHEN OTHERS.
      CLEAR GT_0090.
      READ TABLE GT_0090 WITH KEY PRODH = PV_PRODH
                                  BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF PT_TAB-ZPRODH_GROUP EQ GT_0090-ZPRODH_GROUP.
        ELSE.
          PV_CHK = 'X'.
        ENDIF.
      ELSE.
        PV_CHK = 'X'.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INFO_RECORD_PRICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_INFO_RECORD_PRICE TABLES PT_TAB STRUCTURE GT_MDDP..

  CONSTANTS : LC_WERKS TYPE WERKS_D VALUE '1001'.

  DATA : LS_DATA LIKE ZSDS0050,
         LT_DATA LIKE TABLE OF LS_DATA.

  DATA : BEGIN OF LT_MDDP_C OCCURS 0,
           EKORG TYPE EKKO-EKORG,
           MATNR TYPE MARA-MATNR,
           LIFNR TYPE LFA1-LIFNR,
         END OF LT_MDDP_C.

  CLEAR : LT_MDDP_C, LT_MDDP_C[].
  MOVE-CORRESPONDING PT_TAB[] TO LT_MDDP_C[].
  SORT LT_MDDP_C BY EKORG MATNR LIFNR.
  DELETE ADJACENT DUPLICATES FROM LT_MDDP_C COMPARING ALL FIELDS.


  DATA : BEGIN OF LS_A017,
           MATNR    LIKE A017-MATNR,
           MAKTX    TYPE MAKT-MAKTX,
           EKORG    LIKE A017-EKORG,
           EKOTX    LIKE T024E-EKOTX,
           BUKRS    LIKE T001-BUKRS,
           BUTXT    LIKE T001-BUTXT,
           LIFNR    LIKE A017-LIFNR,
           LIFNR_NM LIKE LFA1-NAME1,
           WERKS    LIKE A017-WERKS,
           WERKS_NM LIKE T001W-NAME1,
           ESOKZ    LIKE A017-ESOKZ,
           DATBI    LIKE A017-DATBI,
           DATAB    LIKE A017-DATAB,
           KNUMH    LIKE A017-KNUMH,
         END OF LS_A017.
  DATA : LT_A017 LIKE TABLE OF LS_A017.

  DATA : BEGIN OF LT_KNUMH_C OCCURS 0,
           KNUMH LIKE KONP-KNUMH,
           KSCHL LIKE KONP-KSCHL,
         END OF LT_KNUMH_C.

  DATA : LV_VALIDFR TYPE SY-DATUM.
  DATA : LV_TABIX TYPE SY-TABIX.
  DATA : LV_CONDITION TYPE STRING.

  CLEAR : LT_KNUMH_C, LT_KNUMH_C[].
  CLEAR : LT_A017,    LT_A017[].
  CHECK LT_MDDP_C[] IS NOT INITIAL.
  SELECT A~KAPPL, A~KSCHL, A~LIFNR, B~NAME1 AS LIFNR_NM,
         A~MATNR, C~MAKTX,
         A~EKORG, D~EKOTX, D~BUKRS, F~BUTXT,
         A~WERKS, E~NAME1 AS WERKS_NM,
         A~ESOKZ, A~DATBI, A~DATAB, A~KNUMH
         INTO TABLE @DATA(LT_A017_TMP)
         FROM  A017 AS A LEFT JOIN LFA1 AS B
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
         FOR ALL ENTRIES IN @LT_MDDP_C
         WHERE A~KSCHL EQ 'PB00'
         AND   A~MATNR EQ @LT_MDDP_C-MATNR
         AND   A~EKORG EQ @LT_MDDP_C-EKORG
         AND   A~WERKS EQ @LC_WERKS
         AND   A~LIFNR EQ @LT_MDDP_C-LIFNR
         AND   A~DATBI GE @GV_DATE
         AND   A~DATAB LE @GV_DATE.
  IF SY-SUBRC = 0.

    LT_A017[] = CORRESPONDING #( LT_A017_TMP[] ).

    LT_KNUMH_C[] = CORRESPONDING #( LT_A017_TMP[] ).
    SORT LT_KNUMH_C BY KNUMH KSCHL.
    DELETE ADJACENT DUPLICATES FROM LT_KNUMH_C COMPARING ALL FIELDS.

    IF LT_KNUMH_C[] IS NOT INITIAL.

      SELECT KNUMH, KSCHL, KOPOS, KBETR, KONWA AS WAERS, KMEIN, KPEIN
             INTO TABLE @DATA(LT_KONP)
             FROM  KONP
             FOR ALL ENTRIES IN @LT_KNUMH_C
             WHERE KNUMH EQ @LT_KNUMH_C-KNUMH
             AND   KSCHL EQ @LT_KNUMH_C-KSCHL.
      IF SY-SUBRC = 0.
        SORT LT_KONP BY KNUMH KSCHL.
      ENDIF.

    ENDIF.

    LOOP AT LT_A017 INTO LS_A017.

      MOVE-CORRESPONDING LS_A017 TO LS_DATA.


      LS_DATA-VALID_FR = LS_A017-DATAB.
      LS_DATA-VALID_TO = LS_A017-DATBI.

      READ TABLE LT_KONP WITH KEY KNUMH = LS_A017-KNUMH
                                  KSCHL = 'PB00'
                                  TRANSPORTING NO FIELDS
                                  BINARY SEARCH.
      IF SY-SUBRC = 0.
        CLEAR : LV_TABIX.
        LV_TABIX = SY-TABIX.
        LOOP AT LT_KONP INTO DATA(LS_KONP) FROM LV_TABIX.
          IF LS_A017-KNUMH NE LS_KONP-KNUMH.
            CLEAR : LS_KONP.
            EXIT.
          ENDIF.

          MOVE-CORRESPONDING LS_KONP TO LS_DATA.

          CLEAR : LS_KONP.

        ENDLOOP.
      ENDIF.

      APPEND LS_DATA TO LT_DATA.

      CLEAR : LS_A017.

    ENDLOOP.


    SORT LT_DATA BY EKORG MATNR LIFNR.
    DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING EKORG MATNR LIFNR.


    LOOP AT PT_TAB.

      CLEAR : LV_TABIX.
      LV_TABIX = SY-TABIX.

      CLEAR : LS_DATA.
      READ TABLE LT_DATA INTO LS_DATA WITH KEY EKORG = PT_TAB-EKORG
                                               MATNR = PT_TAB-MATNR
                                               LIFNR = PT_TAB-LIFNR
                                               BINARY SEARCH.
      IF SY-SUBRC = 0.
        PT_TAB-ZKBETR_MM_OLD = LS_DATA-KBETR.
        PT_TAB-ZKONWA_MM     = LS_DATA-WAERS.
        PT_TAB-ZKMEIN_MM     = LS_DATA-KPEIN.
        PT_TAB-KMEIN_2       = LS_DATA-KMEIN.
        PT_TAB-DATAB         = LS_DATA-VALID_FR.

        MODIFY PT_TAB INDEX LV_TABIX TRANSPORTING ZKBETR_MM_OLD ZKONWA_MM
                                                  ZKMEIN_MM     KMEIN_2
                                                  DATAB.

      ENDIF.

      CLEAR : PT_TAB.

    ENDLOOP.


  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_PRICE_BDC2_VK12
*&---------------------------------------------------------------------*
FORM MAKE_PRICE_BDC2_VK12  USING    PV_ZSUCCESS_SD.

  DATA: LV_KBETR    TYPE CHAR14,
        LV_KPEIN    TYPE CHAR5,
        LV_DATAB    TYPE RV13A-DATAB,
        LV_DATBI    TYPE RV13A-DATBI,
        LV_MSG(255).

  _CLEAR : GT_BDCDATA, GT_BDCMSG.
  PERFORM BDCDATA_SET USING :
      'X'  'SAPMV13A'         '0100',
      ' '  'RV13A-KSCHL'      'PR00',
      ' '  'BDC_OKCODE'       '/00'.

  PERFORM BDCDATA_SET USING :
      'X'  'SAPLV14A'         '0100',
      ' '  'BDC_OKCODE'       '=WEIT',
      ' '  'RV130-SELKZ(01)'   'X'.

  PERFORM DATE_SETTING USING P_DATE
                    CHANGING LV_DATAB.
  PERFORM DATE_SETTING USING P_END
                    CHANGING LV_DATBI.

  PERFORM BDCDATA_SET USING :
         'X'  'RV13A903'         '1000',
         ' '  'BDC_OKCODE'       '/00',
         ' '  'F001'             GT_LIST-VKORG,      " Sales org.
         ' '  'F002'             GT_LIST-VTWEG,      " Distribution Channel
         ' '  'F003'             GT_LIST-ZKUNNR_SUB, " Sold-to party
         ' '  'F004-LOW'         GT_LIST-MATNR,      " Material
         ' '  'SEL_DATE'         LV_DATAB.           " Valid On


  PERFORM BDCDATA_SET USING :
         'X'  'RV13A903'         '1000',
         ' '  'BDC_OKCODE'       '=ONLI',
         ' '  'F001'             GT_LIST-VKORG,      " Sales org.
         ' '  'F002'             GT_LIST-VTWEG,      " Distribution Channel
         ' '  'F003'             GT_LIST-ZKUNNR_SUB, " Sold-to party
         ' '  'F004-LOW'         GT_LIST-MATNR,      " Material
         ' '  'SEL_DATE'         LV_DATAB.           " Valid On

  CLEAR LV_KBETR.
  WRITE GT_LIST-ZKBETR_SD TO LV_KBETR CURRENCY GT_LIST-ZKONWA_SD.
  PERFORM BDCDATA_SET USING :
         'X'  'SAPMV13A'         '1903',
         ' '  'BDC_OKCODE'       '/00',
         ' '  'KONP-KBETR(01)'   LV_KBETR,
         ' '  'RV13A-DATAB(01)'  LV_DATAB,
         ' '  'RV13A-DATBI(01)'  LV_DATBI,
         ' '  'BDC_OKCODE'       '=SICH'.

** CALL TRANSACTION
  CLEAR : GT_BDCMSG, GT_BDCMSG[].
  CALL TRANSACTION 'VK12'
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
    GT_LIST-ICON = ICON_LED_RED.
    GT_LIST-MESSAGE = LV_MSG.
    PV_ZSUCCESS_SD = 'X'.
  ELSE.
    ADD 1 TO GV_SUCCESS.
    GT_LIST-ICON = ICON_LED_GREEN.
    PV_ZSUCCESS_SD = 'O'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SELECTION
*&---------------------------------------------------------------------*
FORM SET_SELECTION .

  IF GV_ZTYPE = 'P' OR GV_ZTYPE = 'S'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'Z01'.
        SCREEN-INPUT = 0.
        SCREEN-ACTIVE = 0.
      ENDIF.
      IF SCREEN-GROUP1 = 'Z02'.
        SCREEN-INPUT = 0.
        SCREEN-ACTIVE = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'Z01'.
        SCREEN-INPUT = 0.
        SCREEN-ACTIVE = 1.
      ENDIF.
      IF SCREEN-GROUP1 = 'Z02'.
        SCREEN-INPUT = 0.
        SCREEN-ACTIVE = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_INFO_RECORD
*&---------------------------------------------------------------------*
FORM MAKE_INFO_RECORD .

  DATA LV_TCODE    LIKE SY-TCODE.

* STEP 1이 있는 경우에만 대상.
  _CLEAR GT_INFO.
  GT_INFO-MATNR = GT_LIST-MATNR.
  GT_INFO-LIFNR = GT_LIST-LIFNR.
  GT_INFO-EKORG = GT_LIST-EKORG.
  GT_INFO-WERKS = '1001'.
  GT_INFO-DATAB = GV_DATE.
  IF S_DATE[] IS INITIAL.
    GT_INFO-DATBI = P_END.
  ELSE.
    GT_INFO-DATBI = S_DATE-HIGH.
  ENDIF.
  GT_INFO-KBETR = GT_LIST-ZKBETR_MM.
  GT_INFO-KONWA = GT_LIST-KONWA_1.
  GT_INFO-KPEIN = GT_LIST-KPEIN_1.
  GT_INFO-KMEIN = GT_LIST-KMEIN_1.
  APPEND GT_INFO. CLEAR GT_INFO .

  IF GT_LIST-ZKBETR_MM_OLD IS INITIAL.
    CLEAR LV_TCODE. LV_TCODE = 'ME11'.
  ELSE.
    CLEAR LV_TCODE. LV_TCODE = 'ME12'.
  ENDIF.

*STEP 2 : HQ 매입단가를 생성한다
  CALL FUNCTION 'ZMM_MGNT_INFO_RECORD'
    EXPORTING
      I_BDC_MODE = 'N'
      I_TCODE    = LV_TCODE
    TABLES
      IT_DATA    = GT_INFO.

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
*& Form SET_FRAME_9100
*&---------------------------------------------------------------------*
FORM SET_FRAME_9100 .

  CREATE OBJECT GO_CUSTOM1
    EXPORTING
      CONTAINER_NAME = 'GO_CON'.

  CREATE OBJECT GO_DOCUMENT
    EXPORTING
      STYLE = 'TOP_OF_PAGE'.

  CREATE OBJECT GO_SPLITTER
    EXPORTING
      PARENT  = GO_CUSTOM1
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

  CREATE OBJECT GO_GRID1
    EXPORTING
      I_PARENT = GO_CON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCAT_9100
*&---------------------------------------------------------------------*
FORM SET_FIELDCAT_9100 .

  CLEAR: GT_FCAT[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ICON'.
  GS_FCAT-COLTEXT    = TEXT-F28. "'STATUS'.
  GS_FCAT-JUST       = 'C'.
  GS_FCAT-OUTPUTLEN  = '4'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MESSAGE'.
  GS_FCAT-COLTEXT    = TEXT-F29. "'Message'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR'.
  GS_FCAT-COLTEXT    = TEXT-F01.
  GS_FCAT-REF_FIELD  = 'KUNNR'.
  GS_FCAT-REF_TABLE  = 'KNA1'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '4'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F26.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZBASE_PRICE'.
  GS_FCAT-COLTEXT    = TEXT-F02.
  GS_FCAT-CFIELDNAME = 'KONWA_1'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KONWA_1'.
  GS_FCAT-COLTEXT    = TEXT-F14.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'FLAG'.
  GS_FCAT-COLTEXT    = TEXT-F03.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '4'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZPRODH_GROUP'.
  GS_FCAT-COLTEXT    = TEXT-F27.
  GS_FCAT-OUTPUTLEN  = 10.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'CHK_MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F35.
  GS_FCAT-CHECKBOX   = 'X'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '5'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F04.
  GS_FCAT-REF_FIELD  = 'MATNR'.
  GS_FCAT-REF_TABLE  = 'MARA'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '18'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F05.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '20'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZMARGIN_MM'.
  GS_FCAT-COLTEXT    = TEXT-F31.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR'.
  GS_FCAT-COLTEXT    = TEXT-F06.
  GS_FCAT-REF_FIELD  = 'LIFNR'.
  GS_FCAT-REF_TABLE  = 'LFA1'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  GS_FCAT-OUTPUTLEN  = '7'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F07.
  GS_FCAT-EMPHASIZE  = 'C100'.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'EKORG'.
  GS_FCAT-COLTEXT    = TEXT-F08.
  GS_FCAT-EMPHASIZE  = 'C100'.
  GS_FCAT-OUTPUTLEN  = '5'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'EKORG_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F09.
  GS_FCAT-EMPHASIZE  = 'C100'.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'WERKS'.
  GS_FCAT-COLTEXT    = TEXT-F10.
  GS_FCAT-EMPHASIZE  = 'C100'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'WERKS_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F11.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKBETR_MM_OLD'.
  GS_FCAT-COLTEXT    = TEXT-F13.
  GS_FCAT-CFIELDNAME = 'ZKONWA_MM'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'DATAB'.
  GS_FCAT-COLTEXT    = TEXT-F36.
  GS_FCAT-EMPHASIZE  = 'C100'.
  GS_FCAT-NO_OUT     = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKBETR_MM'.
  GS_FCAT-COLTEXT    = TEXT-F33.
  GS_FCAT-CFIELDNAME = 'ZKONWA_MM'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKONWA_MM'.
  GS_FCAT-COLTEXT    = TEXT-F14.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LVORM'.
  GS_FCAT-COLTEXT    = TEXT-F25.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCAT_9200
*&---------------------------------------------------------------------*
FORM SET_FIELDCAT_9200 .

  CLEAR: GT_FCAT[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ICON'.
  GS_FCAT-COLTEXT    = TEXT-F28. "'STATUS'.
  GS_FCAT-JUST       = 'C'.
  GS_FCAT-OUTPUTLEN  = '4'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MESSAGE'.
  GS_FCAT-COLTEXT    = TEXT-F29. "'Message'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR'.
  GS_FCAT-COLTEXT    = TEXT-F01.
  GS_FCAT-REF_FIELD  = 'KUNNR'.
  GS_FCAT-REF_TABLE  = 'KNA1'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '4'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F26.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZBASE_PRICE'.
  GS_FCAT-COLTEXT    = TEXT-F02.
  GS_FCAT-CFIELDNAME = 'KONWA_1'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KONWA_1'.
  GS_FCAT-COLTEXT    = TEXT-F14.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'FLAG'.
  GS_FCAT-COLTEXT    = TEXT-F03.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '4'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZPRODH_GROUP'.
  GS_FCAT-COLTEXT    = TEXT-F27.
  GS_FCAT-OUTPUTLEN  = 10.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'CHK_MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F35.
  GS_FCAT-CHECKBOX   = 'X'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '5'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F04.
  GS_FCAT-REF_FIELD  = 'MATNR'.
  GS_FCAT-REF_TABLE  = 'MARA'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '18'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F05.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-OUTPUTLEN  = '20'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZMARGIN_SD'.
  GS_FCAT-COLTEXT    = TEXT-F32.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VKORG'.
  GS_FCAT-COLTEXT    = TEXT-F15.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VKORG_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F16.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VTWEG'.
  GS_FCAT-COLTEXT    = TEXT-F17.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VTWEG_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F18.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKUNNR_SUB '.
  GS_FCAT-COLTEXT    = TEXT-F19.
  GS_FCAT-NO_ZERO    = 'X'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKUNNR_IC_TXT '.
  GS_FCAT-COLTEXT    = TEXT-F20.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNWE'.
  GS_FCAT-COLTEXT    = TEXT-F21.
  GS_FCAT-REF_FIELD  = 'KUNNR'.
  GS_FCAT-REF_TABLE  = 'KNA1'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNWE_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F22.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKBETR_SD_OLD'.
  GS_FCAT-COLTEXT    = TEXT-F23.
  GS_FCAT-CFIELDNAME = 'ZKONWA_SD'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKBETR_SD'.
  GS_FCAT-COLTEXT    = TEXT-F34.
  GS_FCAT-CFIELDNAME = 'ZKONWA_SD'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZKONWA_SD'.
  GS_FCAT-COLTEXT    = TEXT-F24.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LVORM'.
  GS_FCAT-COLTEXT    = TEXT-F25.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PRICE_2
*&---------------------------------------------------------------------*
FORM CREATE_PRICE_2 .

  DATA : LT_SAVE     LIKE TABLE OF ZSDT0110 WITH HEADER LINE,
         LV_ERR,
         LV_MSG(100),
         LV_CHECK    TYPE C.

  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.

    IF LV_CHECK IS INITIAL.
      PERFORM POPUP_MSG USING 'Create price'
                              'Do you want to create price?'
                              LV_CHECK.
    ENDIF.
    CHECK LV_CHECK = '1'.

    _CLEAR LT_SAVE.
    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING GT_LIST TO LT_SAVE.
        IF GV_ZTYPE = 'S'.
          CLEAR : LT_SAVE-ZMARGIN_MM,
                  LT_SAVE-WERKS,
                  LT_SAVE-ZKMEIN_MM,
                  LT_SAVE-ZKBETR_MM,
                  LT_SAVE-ZKBETR_MM_OLD,
                  LT_SAVE-ZKONWA_MM,
                  LT_SAVE-ZSUCCESS_MM.
        ELSEIF GV_ZTYPE = 'P'.
          CLEAR : LT_SAVE-ZMARGIN_SD,
                  LT_SAVE-VKORG,
                  LT_SAVE-VTWEG,
                  LT_SAVE-KUNWE,
                  LT_SAVE-ZKMEIN_SD,
                  LT_SAVE-ZKBETR_SD,
                  LT_SAVE-ZKBETR_SD_OLD,
                  LT_SAVE-ZKONWA_SD,
                  LT_SAVE-ZSUCCESS_SD.
        ENDIF.

        LT_SAVE-ZTYPE = GV_ZTYPE.
        LT_SAVE-ZSTART = GV_DATE.
        LT_SAVE-END_DATE = GV_DATE2.
        LT_SAVE-ERNAM = SY-UNAME.
        LT_SAVE-ERDAT = SY-DATUM.
        LT_SAVE-ERZET = SY-UZEIT.

        GT_LIST-ICON = ICON_LED_GREEN.
        MODIFY GT_LIST  INDEX GS_ROWS-INDEX TRANSPORTING ICON.
        APPEND LT_SAVE. CLEAR LT_SAVE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF LT_SAVE[] IS NOT INITIAL.
    MODIFY ZSDT0110 FROM TABLE LT_SAVE.

    UPDATE ZSDT0021
       SET ZHQCAL  = 'X'
           ZHQCAL_DATE     = SY-DATLO
           ZINTERCAL_I     = SPACE
           ZINTERCAL_IDATE = SPACE
           ZINTERCAL_S     = SPACE
           ZINTERCAL_SDATE = SPACE
           ZINTERFACE      = SPACE
           ZINTERFACE_DATE = SPACE
           ZCONFIRM        = SPACE
           ZCONFIRM_DATE   = SPACE
     WHERE ZSTART EQ GV_DATE
       AND ZTYPE  EQ GV_ZTYPE.
  ENDIF.

  FREE GO_DOCUMENT.
  CREATE OBJECT GO_DOCUMENT
    EXPORTING
      STYLE = 'TOP_OF_PAGE'.
  PERFORM HANDLE_TOP_OF_PAGE USING GO_DOCUMENT.

ENDFORM.
