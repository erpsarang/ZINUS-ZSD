*&---------------------------------------------------------------------*
*& Include          ZSDR0015F01
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
  DATA : LV_DATE(10)  TYPE C.

  CLEAR : LV_TEXT.

  CLEAR : LV_DATE.
  WRITE P_DATE TO LV_DATE.
  LV_TEXT =  'Start Date :' && '　' && LV_DATE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'MIDIUM'.

  CALL METHOD PE_DYNDOC_ID->NEW_LINE.

  CLEAR LV_TEXT.
  DESCRIBE TABLE GT_LIST LINES DATA(LS_LINE).
  LV_TEXT =  'Total Data : '  && LS_LINE.
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

* 판매법인 매입단가 정보
  SELECT A~ZKUNNR_IC,
         A~BUKRS,
         A~ZSYSTEM,
         B~WERKS,
         B~LIFNR,
         B~VTWEG,
         B~ZKUNNR_S,
         B~ZBUKRS_INFO,
         B~ZWERKS_INFO,
         B~ZEKORG_INFO
    FROM ZSDT0040 AS A INNER JOIN ZSDT0050 AS B ON A~ZKUNNR_IC = B~ZKUNNR_IC
    INTO TABLE @DATA(LT_0050)
    WHERE A~ZSYSTEM EQ 'SAP'.

  SORT LT_0050 BY ZKUNNR_IC VTWEG ZKUNNR_S.

  SELECT A~ZKUNNR_IC,
         A~BUKRS,
         A~ZSYSTEM,
         B~VTWEG,
         B~KUNWE,
         B~EKORG,
         B~ZVKORG_SALES,
         B~ZVTWEG_SALES,
         B~ZKUNNR_SALES,
         B~KUNNR
    FROM ZSDT0040 AS A INNER JOIN ZSDT0060 AS B ON A~ZKUNNR_IC = B~ZKUNNR_IC
    INTO TABLE @DATA(LT_0060)
    WHERE A~ZSYSTEM EQ 'SAP'.

  SORT LT_0060 BY ZKUNNR_IC VTWEG KUNWE.

  DATA : LT_0060_K LIKE TABLE OF ZSDT0060 WITH HEADER LINE.

  LOOP AT LT_0060 INTO DATA(WA_0060) WHERE ZVKORG_SALES IS NOT INITIAL
                                       AND ZVTWEG_SALES IS NOT INITIAL
                                       AND ZKUNNR_SALES IS NOT INITIAL.
    MOVE-CORRESPONDING WA_0060 TO LT_0060_K.
    APPEND LT_0060_K. CLEAR LT_0060_K.
  ENDLOOP.


*1. Start date 기준으로 HQ 판매단가 정보를 가져온다
* - A305 : HQ 직접매출일 경우
  SELECT A~VKORG, A~MATNR, A~KUNNR, A~KSCHL,
         A~DATAB, A~DATBI, A~VTWEG,
         B~KBETR, B~KONWA, B~KPEIN, B~KMEIN,
         C~PRDHA AS PRODH,
         D~MAKTX AS MATNR_TXT,
         E~VTEXT AS PRODH_TXT,
         F~NAME_ORG1 AS KUNNR_TXT
         INTO TABLE @DATA(LT_A305)
         FROM A305 AS A INNER JOIN KONP  AS B
                                         ON A~KNUMH EQ B~KNUMH
                        INNER JOIN MARA  AS C
                                         ON A~MATNR EQ C~MATNR
                        INNER JOIN MAKT  AS D
                                         ON A~MATNR EQ D~MATNR
                                        AND D~SPRAS EQ @SY-LANGU
                        INNER JOIN T179T AS E
                                         ON C~PRDHA EQ E~PRODH
                                        AND E~SPRAS EQ @SY-LANGU
                        INNER JOIN BUT000 AS F
                                          ON A~KUNNR EQ F~PARTNER
           WHERE A~DATBI    GE @P_DATE
           AND   A~DATAB    LE @P_DATE
           AND   A~KSCHL    EQ 'PR00'
           AND   A~VKORG    EQ '1001'
           AND   A~VTWEG    EQ '30'
           AND   B~LOEVM_KO EQ @ABAP_OFF
           AND   A~KUNNR    IN @S_KUNNR1
           AND   A~MATNR    IN @S_MATNR1.

* - A903 : 관계사 매출일 경우
  SELECT A~VKORG, A~MATNR, A~VTWEG, A~KUNNR,
         A~KUNWE, A~KSCHL, A~DATAB, A~DATBI,
         B~KBETR, B~KONWA, B~KPEIN, B~KMEIN,
         C~PRDHA AS PRODH,
         D~MAKTX AS MATNR_TXT,
         E~VTEXT AS PRODH_TXT,
         F~NAME_ORG1 AS KUNNR_TXT,
         G~NAME_ORG1 AS KUNWE_TXT
         INTO TABLE @DATA(LT_A903)
         FROM A903 AS A INNER JOIN KONP  AS B
                                         ON A~KNUMH EQ B~KNUMH
                        INNER JOIN MARA  AS C
                                         ON A~MATNR EQ C~MATNR
                        INNER JOIN MAKT  AS D
                                         ON A~MATNR EQ D~MATNR
                                        AND D~SPRAS EQ @SY-LANGU
                        INNER JOIN T179T AS E
                                         ON C~PRDHA EQ E~PRODH
                                        AND E~SPRAS EQ @SY-LANGU
                        INNER JOIN BUT000 AS F
                                         ON A~KUNNR EQ F~PARTNER
                        INNER JOIN BUT000 AS G
                                         ON A~KUNWE EQ G~PARTNER
         WHERE A~DATBI    GE @P_DATE
         AND   A~DATAB    LE @P_DATE
         AND   A~KSCHL    EQ 'PR00'
         AND   B~LOEVM_KO EQ @ABAP_OFF
         AND   A~KUNNR    IN @S_KUNNR1
         AND   A~KUNWE    IN @S_KUNWE1
         AND   A~VTWEG    IN @S_VTWEG1
         AND   A~MATNR    IN @S_MATNR1.

*2. HQ 매입단가 정보를 가져온다
* - ZSDT0060의 정보(판매처, 유통경로, ship-to)를 활용해서 매입단가를 가져온다 (EKORG)
* - A017
  SELECT A~KAPPL, A~KSCHL, A~LIFNR, A~MATNR,
         A~EKORG, A~WERKS, A~ESOKZ, A~DATBI,
         A~DATAB, A~KNUMH,
         B~KBETR, B~KONWA, B~KPEIN, B~KMEIN,
         C~PRDHA AS PRODH,
         D~MAKTX AS MATNR_TXT,
         E~VTEXT AS PRODH_TXT,
         F~NAME_ORG1 AS LIFNR_TXT
     INTO TABLE @DATA(LT_A017)
     FROM  A017 AS A INNER JOIN KONP  AS B
                                      ON A~KNUMH EQ B~KNUMH
                     INNER JOIN MARA  AS C
                                      ON A~MATNR EQ C~MATNR
                     INNER JOIN MAKT  AS D
                                      ON A~MATNR EQ D~MATNR
                                     AND D~SPRAS EQ @SY-LANGU
                     INNER JOIN T179T AS E
                                      ON C~PRDHA EQ E~PRODH
                                     AND E~SPRAS EQ @SY-LANGU
                      INNER JOIN BUT000 AS F
                                       ON A~LIFNR EQ F~PARTNER
     WHERE A~KSCHL EQ 'PB00'
     AND   A~DATBI GE @P_DATE
     AND   A~DATAB LE @P_DATE
     AND   A~MATNR IN @S_MATNR2
     AND   A~LIFNR IN @S_LIFNR2
     AND   A~EKORG IN @S_EKORG2
     AND   A~WERKS IN @S_WERKS2.

  SORT LT_A017 BY MATNR EKORG.

*3. 판매법인 매입단가 정보를 가져온다
* - ZSDT0040 에서 매입단가 생성 대상인 법인을 찾는다
* - ZSDT0050 에서 판매법인 매핑정보에 따라서 조직정보를 찾는다
* - A914에서 해당하는 매입단가 정보를 가지고 온다

  SELECT A~KAPPL, A~KSCHL, A~BUKRS,  A~EKORG,
         A~LLIEF, A~WERKS, A~MATNR,  A~KFRST,
         A~DATBI, A~DATAB, A~KBSTAT, A~KNUMH,
         B~KBETR, B~KONWA, B~KPEIN,  B~KMEIN,
         C~PRDHA AS PRODH,
         D~MAKTX AS MATNR_TXT,
         E~VTEXT AS PRODH_TXT,
         F~NAME_ORG1 AS LLIEF_TXT
      INTO TABLE @DATA(LT_A914)
      FROM  A914 AS A INNER JOIN KONP AS B
                                      ON A~KNUMH EQ B~KNUMH
                      INNER JOIN MARA  AS C
                                       ON A~MATNR EQ C~MATNR
                      INNER JOIN MAKT  AS D
                                       ON A~MATNR EQ D~MATNR
                                      AND D~SPRAS EQ @SY-LANGU
                      INNER JOIN T179T AS E
                                       ON C~PRDHA EQ E~PRODH
                                      AND E~SPRAS EQ @SY-LANGU
                      INNER JOIN BUT000 AS F
                                       ON A~LLIEF EQ F~PARTNER
      WHERE A~KSCHL EQ 'PB00'
        AND A~BUKRS IN @S_BUKRS4
        AND A~LLIEF IN @S_LLIEF4
        AND A~WERKS IN @S_WERKS4
        AND A~EKORG IN @S_EKORG4
        AND A~MATNR IN @S_MATNR4
        AND A~DATBI GE @P_DATE
        AND A~DATAB LE @P_DATE.

  SORT LT_A914 BY BUKRS EKORG LLIEF WERKS MATNR.

*4. 제조법인 판매단가 정보를 가져온다 (SAP)
* - ZSDT0040 에서 판매단가 생성 대상인 법인을 찾는다 (대상 시스템이 SAP)
* - ZSDT0060 에서 제조법인 매핑정보에 따라서 조직정보를 찾는다
* - A305에서 해당하는 판매단가 정보를 가지고 온다

  SELECT  A~VKORG, A~MATNR, A~KUNNR, A~KSCHL,
          A~DATAB, A~DATBI, A~VTWEG,
          B~KBETR, B~KONWA, B~KPEIN, B~KMEIN,
          C~PRDHA AS PRODH,
          D~MAKTX AS MATNR_TXT,
          E~VTEXT AS PRODH_TXT,
          F~NAME_ORG1 AS KUNNR_TXT
          INTO TABLE @DATA(LT_A305_S)
          FROM A305 AS A INNER JOIN KONP  AS B
                                          ON A~KNUMH EQ B~KNUMH
                         INNER JOIN MARA  AS C
                                          ON A~MATNR EQ C~MATNR
                         INNER JOIN MAKT  AS D
                                          ON A~MATNR EQ D~MATNR
                                         AND D~SPRAS EQ @SY-LANGU
                         INNER JOIN T179T AS E
                                          ON C~PRDHA EQ E~PRODH
                                         AND E~SPRAS EQ @SY-LANGU
                        INNER JOIN BUT000 AS F
                                          ON A~KUNNR EQ F~PARTNER
         FOR ALL ENTRIES IN @LT_0060_K
          WHERE A~DATBI    GE @P_DATE
          AND   A~DATAB    LE @P_DATE
          AND   A~VKORG    EQ @LT_0060_K-ZVKORG_SALES
          AND   A~VTWEG    EQ @LT_0060_K-ZVTWEG_SALES
          AND   A~KUNNR    EQ @LT_0060_K-ZKUNNR_SALES
          AND   A~MATNR    IN @S_MATNR3
          AND   A~KSCHL    EQ 'PR00'
          AND   B~LOEVM_KO EQ @ABAP_OFF.

  SORT LT_A305_S BY VTWEG MATNR.

  _CLEAR GT_LIST.

*1. 1)A305 : HQ 직접매출일 경우
*   2)A017 EKORG = 1011' WERKS '1001' 대상과 MAPPING
  LOOP AT LT_A305 INTO DATA(LS_A305).
    CLEAR GT_LIST.
    MOVE : LS_A305-MATNR     TO GT_LIST-MATNR,
           LS_A305-MATNR_TXT TO GT_LIST-MATNR_TXT,
           LS_A305-PRODH     TO GT_LIST-PRODH,
           LS_A305-PRODH_TXT TO GT_LIST-PRODH_TXT,
           LS_A305-VKORG     TO GT_LIST-VKORG1,
           LS_A305-VTWEG     TO GT_LIST-VTWEG1,
           LS_A305-KUNNR     TO GT_LIST-KUNNR1,
           LS_A305-KUNNR_TXT TO GT_LIST-KUNNR1_TXT,
           LS_A305-KPEIN     TO GT_LIST-KPEIN1,
           LS_A305-KMEIN     TO GT_LIST-KMEIN1,
           LS_A305-KBETR     TO GT_LIST-KBETR1,
           LS_A305-KONWA     TO GT_LIST-KONWA1.

    READ TABLE LT_A017 WITH KEY MATNR = LS_A305-MATNR
                                EKORG = '1011' BINARY SEARCH
                                               TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      LOOP AT LT_A017 INTO DATA(LS_A017) FROM SY-TABIX.
        IF LS_A017-MATNR NE LS_A305-MATNR OR
           LS_A017-EKORG NE '1011'.
          EXIT.
        ENDIF.
        MOVE : LS_A017-LIFNR     TO GT_LIST-LIFNR,
               LS_A017-LIFNR_TXT TO GT_LIST-LIFNR_TXT,
               LS_A017-EKORG     TO GT_LIST-EKORG2,
               LS_A017-WERKS     TO GT_LIST-WERKS,
               LS_A017-KPEIN     TO GT_LIST-KPEIN2,
               LS_A017-KMEIN     TO GT_LIST-KMEIN2,
               LS_A017-KBETR     TO GT_LIST-KBETR2,
               LS_A017-KONWA     TO GT_LIST-KONWA2.

        APPEND GT_LIST.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

*2. 1)A903 : 관계사매출일 경우->
*   2)A017- ZSDT0060의 정보(sold-to, 유통경로, ship-to)를 활용해서 매입단가의 구매조직 (EKORG) 을 가져온다
*   3) 판매법인 매입단가 정보를 가져온다  - ZSDT0050 에서 판매법인 매핑정보에 따라서 조직정보를 찾는다
*      A914에서 해당하는 매입단가 정보를 가지고 온다
  LOOP AT LT_A903 INTO DATA(LS_A903).
    CLEAR GT_LIST.
    MOVE : LS_A903-MATNR     TO GT_LIST-MATNR,
           LS_A903-MATNR_TXT TO GT_LIST-MATNR_TXT,
           LS_A903-PRODH     TO GT_LIST-PRODH,
           LS_A903-PRODH_TXT TO GT_LIST-PRODH_TXT,
           LS_A903-VKORG     TO GT_LIST-VKORG1,
           LS_A903-VTWEG     TO GT_LIST-VTWEG1,
           LS_A903-KUNNR     TO GT_LIST-KUNNR1,
           LS_A903-KUNNR_TXT TO GT_LIST-KUNNR1_TXT,
           LS_A903-KUNWE     TO GT_LIST-KUNWE1,
           LS_A903-KUNWE_TXT TO GT_LIST-KUNWE1_TXT,
           LS_A903-KPEIN     TO GT_LIST-KPEIN1,
           LS_A903-KMEIN     TO GT_LIST-KMEIN1,
           LS_A903-KBETR     TO GT_LIST-KBETR1,
           LS_A903-KONWA     TO GT_LIST-KONWA1.

    READ TABLE LT_0060 INTO DATA(LS_0060) WITH KEY ZKUNNR_IC = LS_A903-KUNNR
                                                   VTWEG     = LS_A903-VTWEG
                                                   KUNWE     = LS_A903-KUNWE BINARY SEARCH.

    READ TABLE LT_A017 WITH KEY MATNR = LS_A903-MATNR
                                EKORG = LS_0060-EKORG BINARY SEARCH
                                                      TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      LOOP AT LT_A017 INTO DATA(LS_A017_IC) FROM SY-TABIX.
        IF LS_A017_IC-MATNR NE LS_A903-MATNR OR
           LS_A017_IC-EKORG NE LS_0060-EKORG.
          EXIT.
        ENDIF.
        MOVE : LS_A017_IC-LIFNR     TO GT_LIST-LIFNR,
               LS_A017_IC-LIFNR_TXT TO GT_LIST-LIFNR_TXT,
               LS_A017_IC-EKORG     TO GT_LIST-EKORG2,
               LS_A017_IC-WERKS     TO GT_LIST-WERKS,
               LS_A017_IC-KPEIN     TO GT_LIST-KPEIN2,
               LS_A017_IC-KMEIN     TO GT_LIST-KMEIN2,
               LS_A017_IC-KBETR     TO GT_LIST-KBETR2,
               LS_A017_IC-KONWA     TO GT_LIST-KONWA2.

        READ TABLE LT_0050 INTO DATA(LS_0050) WITH KEY ZKUNNR_IC = LS_A903-KUNNR
                                                       VTWEG     = LS_A903-VTWEG
                                                       ZKUNNR_S  = LS_A903-KUNWE BINARY SEARCH.

        READ TABLE LT_A914 INTO DATA(LS_A914) WITH KEY BUKRS = LS_0050-ZBUKRS_INFO
                                                       EKORG = LS_0050-ZEKORG_INFO
                                                       LLIEF = LS_A017_IC-LIFNR
                                                       WERKS = LS_0050-ZWERKS_INFO
                                                       MATNR = LS_A017_IC-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          MOVE : LS_A914-BUKRS     TO GT_LIST-BUKRS3,
                 LS_A914-EKORG     TO GT_LIST-EKORG3,
                 LS_A914-WERKS     TO GT_LIST-WERKS3,
                 LS_A914-LLIEF     TO GT_LIST-LLIEF,
                 LS_A914-LLIEF_TXT TO GT_LIST-LLIEF_TXT,
                 LS_A914-KPEIN     TO GT_LIST-KPEIN3,
                 LS_A914-KMEIN     TO GT_LIST-KMEIN3,
                 LS_A914-KBETR     TO GT_LIST-KBETR3,
                 LS_A914-KONWA     TO GT_LIST-KONWA3.
        ENDIF.

        READ TABLE LT_A305_S INTO DATA(LS_A305_S) WITH KEY VTWEG = LS_A903-VTWEG
                                                           MATNR = LS_A903-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          MOVE : LS_A305_S-VKORG     TO GT_LIST-VKORG4,
                 LS_A305_S-VTWEG     TO GT_LIST-VTWEG4,
                 LS_A305_S-KUNNR     TO GT_LIST-KUNNR4,
                 LS_A305_S-KUNNR_TXT TO GT_LIST-KUNNR4_TXT,
                 LS_A305_S-KPEIN     TO GT_LIST-KPEIN4,
                 LS_A305_S-KMEIN     TO GT_LIST-KMEIN4,
                 LS_A305_S-KBETR     TO GT_LIST-KBETR4,
                 LS_A305_S-KONWA     TO GT_LIST-KONWA4.
        ENDIF.
        APPEND GT_LIST.
        CLEAR LS_0050.
      ENDLOOP.
    ENDIF.

  ENDLOOP.


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
           ZCONFIRM LIKE ZSDT0020-ZCONFIRM,
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
FORM GET_DOMAIN_ZTYPE   USING  PV_ZTYPE
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
