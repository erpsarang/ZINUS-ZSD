*&---------------------------------------------------------------------*
*& Include          ZSDR0014F01
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

  RANGES: LR_MATNR FOR MARA-MATNR.
  RANGES: LR_EKORG FOR EKKO-EKORG.

  CLEAR : LR_MATNR, LR_MATNR[].
  CASE PE_COLUMN.
    WHEN 'MATNR' OR 'MATNR_TXT'.

      IF P_INFO IS NOT INITIAL.
        READ TABLE GT_LIST INDEX PE_ROW.
        IF SY-SUBRC = 0.
          LR_MATNR = 'IEQ'.
          LR_MATNR-LOW = GT_LIST-MATNR.
          APPEND LR_MATNR.

          SUBMIT RM06IM00 WITH IF_MATNR IN LR_MATNR
                          AND RETURN.
        ENDIF.

      ELSE.

        CLEAR: GS_BDC_OPT.
        GS_BDC_OPT-DISMODE  = 'N'.
        GS_BDC_OPT-UPDMODE  = 'S'.
        GS_BDC_OPT-RACOMMIT = 'X'.
        GS_BDC_OPT-NOBINPT  = 'X'.

        PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '0100'.
        PERFORM BDC_FIELD  USING: 'BDC_OKCODE'     '/00',
                                  'RV13A-KSCHL'    'PR00'.

        PERFORM BDC_DYNPRO USING: 'SAPLV14A'       '0100'.
        PERFORM BDC_FIELD  USING: 'BDC_CURSOR'     'RV130-SELKZ(01)',
                                  'RV130-SELKZ(01)' 'X',
                                  'BDC_OKCODE'     '=WEIT'.

        CALL TRANSACTION 'VK13'
                     USING GT_BDCDATA
                     OPTIONS FROM  GS_BDC_OPT
                     MESSAGES INTO GT_BDCMSGE.

        LR_MATNR = 'IEQ'.
        LR_MATNR-LOW = GT_LIST-MATNR.
        APPEND LR_MATNR.

        SUBMIT RV13A305 WITH KSCHL EQ 'PR00'
                        WITH F001  EQ GT_LIST-VKORG
                        WITH F002  EQ GT_LIST-VTWEG
                        WITH F003  EQ GT_LIST-KUNNR
                        WITH F004  IN LR_MATNR
                        WITH SEL_DATE EQ P_DATE
                        AND RETURN.



      ENDIF.



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

  DATA : LV_EKORG TYPE EKKO-EKORG.
  DATA : LT_VPRICE LIKE TABLE OF ZSDS0050 WITH HEADER LINE.

  DATA : LV_TABIX TYPE SY-TABIX.

  DATA : BEGIN OF LT_EXCEPT OCCURS 0,
           ZKUNNR_IC TYPE ZSDT0050-ZKUNNR_IC,
           WERKS     TYPE ZSDT0050-WERKS,
           LIFNR     TYPE ZSDT0050-LIFNR,
           VTWEG     TYPE ZSDT0050-VTWEG,
         END OF LT_EXCEPT.

  DATA : BEGIN OF LT_A914_C OCCURS 0,
           BUKRS    TYPE A914-BUKRS,
           EKORG    TYPE A914-EKORG,
           KUNNR_WL TYPE A914-LLIEF,
           WERKS    TYPE A914-WERKS,
           MATNR    TYPE A914-MATNR,
         END OF LT_A914_C.

  CLEAR : GV_ERROR.
  SELECT SINGLE MANDT INTO SY-MANDT
         FROM ZSDT0021
         WHERE ZSTART EQ P_DATE
         AND   ZTYPE  EQ GV_ZTYPE
         AND   ZHQCAL EQ 'X'.
  IF SY-SUBRC NE 0.
    GV_ERROR = 'X'.
  ENDIF.

  CHECK GV_ERROR IS INITIAL.
  CLEAR : GT_LIST, GT_LIST[].
* Price management master data - Assign Base Price(mDDP)
  SELECT ZKUNNR_IC, KUNNR, VTWEG, VKORG,
         ZKUNNR_S,  ZKUNNR_SUB
         INTO TABLE @DATA(LT_0042)
         FROM ZSDT0042.
  IF SY-SUBRC = 0.
    SORT LT_0042 BY ZKUNNR_IC KUNNR VTWEG VKORG.
  ENDIF.

* Price management master data - Mapping ship-to party
  CLEAR : LT_EXCEPT, LT_EXCEPT[].
  SELECT A~ZKUNNR_IC,
         A~WERKS,
         A~LIFNR,
         A~VTWEG,
         A~ZKUNNR_S,
         A~ZBUKRS_INFO,
         A~ZWERKS_INFO,
         A~ZEKORG_INFO,
         D~NAME1     AS LIFNR_NM,
         C~EKOTX     AS EKORG_TXT,
         B~BUTXT     AS BUKRS_TXT,
         E~NAME1     AS WERKS_TXT
         INTO TABLE @DATA(LT_0050)
         FROM ZSDT0050 AS A LEFT JOIN T001 AS B
         ON   A~ZBUKRS_INFO EQ B~BUKRS
         LEFT JOIN T024E  AS C
         ON   A~ZEKORG_INFO EQ C~EKORG
         LEFT JOIN LFA1 AS D
         ON   A~LIFNR EQ D~LIFNR
         LEFT JOIN T001W AS E
         ON   A~ZWERKS_INFO EQ E~WERKS.
  IF SY-SUBRC = 0.
    SORT LT_0050 BY ZKUNNR_IC VTWEG ZKUNNR_S WERKS LIFNR.

    LOOP AT LT_0050 INTO DATA(LS_0050) WHERE LIFNR IS NOT INITIAL.

      MOVE-CORRESPONDING LS_0050 TO LT_EXCEPT.
      COLLECT LT_EXCEPT. CLEAR : LT_EXCEPT.

      CLEAR : LS_0050.
    ENDLOOP.

    SORT LT_EXCEPT BY ZKUNNR_IC WERKS VTWEG LIFNR.
    DELETE ADJACENT DUPLICATES FROM LT_EXCEPT COMPARING ALL FIELDS.

  ENDIF.

* Price management master data - Mapping info record
  SELECT A~ZKUNNR_IC,
         A~VTWEG,
         A~KUNWE,
         A~EKORG,
         B~EKOTX AS EKORG_TXT
   INTO TABLE @DATA(LT_0060)
    FROM ZSDT0060 AS A LEFT JOIN T024E  AS B
    ON  A~EKORG EQ B~EKORG.
  IF SY-SUBRC = 0.
    SORT LT_0060 BY ZKUNNR_IC VTWEG KUNWE EKORG.
  ENDIF.

  IF GV_ZTYPE NE 'N'.
    PERFORM MAKE_MATNR_RANGES.
  ENDIF.

**HQ의 판가가 기준
  SELECT A~VKORG, A~MATNR, A~VTWEG, A~KUNNR,
         A~KUNWE, A~KSCHL, A~DATAB, A~DATBI,
         B~KBETR, B~KONWA, B~KPEIN, B~KMEIN,
         D~VTEXT     AS VKORG_TXT,
         E~MAKTX     AS MATNR_TXT,
         F~VTEXT     AS VTWEG_TXT,
         G~NAME_ORG1 AS KUNNR_TXT,
         H~NAME_ORG1 AS KUNWE_TXT,
         I~BSTME, I~MEINS,
         J~VRKME, K~BUTXT,
         L~EKORG,
         M~ZCONFIRM
         INTO TABLE @DATA(LT_A903)
         FROM A903 AS A INNER JOIN KONP AS B
         ON A~KNUMH EQ B~KNUMH
         LEFT JOIN ZSDT0040 AS C
         ON A~KUNNR EQ C~ZKUNNR_IC

         INNER JOIN MARA AS I
         ON  A~MATNR EQ I~MATNR

         LEFT JOIN TVKOT AS D
         ON  A~VKORG EQ D~VKORG
         AND D~SPRAS EQ @SY-LANGU

         LEFT JOIN MAKT  AS E
         ON  A~MATNR EQ E~MATNR
         AND E~SPRAS EQ @SY-LANGU

         LEFT JOIN MVKE AS J
         ON  A~MATNR EQ J~MATNR
         AND A~VKORG EQ J~VKORG
         AND A~VTWEG EQ J~VTWEG

         LEFT JOIN TVTWT AS F
         ON  A~VTWEG EQ F~VTWEG
         AND F~SPRAS EQ @SY-LANGU

         LEFT JOIN T001 AS K
         ON  C~BUKRS EQ K~BUKRS

         LEFT JOIN BUT000 AS G
         ON  A~KUNNR EQ G~PARTNER

         LEFT JOIN BUT000 AS H
         ON  A~KUNWE EQ H~PARTNER

         LEFT JOIN ZSDT0060 AS L
         ON   A~KUNNR EQ L~ZKUNNR_IC
         AND  A~VTWEG EQ L~VTWEG
         AND  A~KUNWE EQ L~KUNWE

         LEFT JOIN ZSDT0021 AS M
         ON   M~ZSTART EQ @P_DATE
         AND  M~ZTYPE  EQ @GV_ZTYPE

         WHERE A~DATBI    GE @P_DATE
         AND   A~DATAB    LE @P_DATE
         AND   A~KSCHL    EQ 'PR00'
         AND   B~LOEVM_KO EQ @SPACE
         AND   I~LVORM    EQ @SPACE
         AND   C~ZHQ      EQ @SPACE
         AND   C~ZSYSTEM  EQ 'SAP'.
  IF SY-SUBRC = 0.

    IF GV_ZTYPE NE 'N'.
      DELETE LT_A903 WHERE MATNR NOT IN GR_MATNR.
    ENDIF.

    MOVE-CORRESPONDING LT_A903[] TO GT_MAT_C[].
    SORT GT_MAT_C BY EKORG MATNR.
    DELETE ADJACENT DUPLICATES FROM GT_MAT_C COMPARING ALL FIELDS.

* Get vendor & price from info record
    PERFORM GET_INFO_DATA_PRICE.


    LOOP AT LT_A903 INTO DATA(LS_A903).

      CLEAR : LS_0050, GT_LIST.
      READ TABLE LT_0050 INTO LS_0050 WITH KEY ZKUNNR_IC = LS_A903-KUNNR
                                               VTWEG     = LS_A903-VTWEG
                                               ZKUNNR_S  = LS_A903-KUNWE
                                               BINARY SEARCH.
      CHECK SY-SUBRC = 0.

      MOVE-CORRESPONDING LS_A903 TO GT_LIST.
      GT_LIST-WERKS     = LS_0050-ZWERKS_INFO.
      GT_LIST-WERKS_TXT = LS_0050-WERKS_TXT.

      IF LS_A903-VRKME IS INITIAL.
        GT_LIST-KMEIN_1     = LS_A903-MEINS.
      ELSE.
        GT_LIST-KMEIN_1     = LS_A903-VRKME.
      ENDIF.

      IF LS_A903-BSTME IS INITIAL.
        GT_LIST-ZKMEIN_MM   = LS_A903-MEINS.
      ELSE.
        GT_LIST-ZKMEIN_MM   = LS_A903-BSTME.
      ENDIF.

      GT_LIST-ZBASE_PRICE = LS_A903-KBETR.
      GT_LIST-KONWA_1     = LS_A903-KONWA.

      CLEAR : LV_EKORG.
      READ TABLE LT_0060 INTO DATA(LS_0060) WITH KEY ZKUNNR_IC = LS_A903-KUNNR
                                                     VTWEG     = LS_A903-VTWEG
                                                     KUNWE     = LS_A903-KUNWE
                                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_EKORG = LS_0060-EKORG.
      ENDIF.

      CLEAR : GS_VPRICE.
      READ TABLE GT_VPRICE INTO GS_VPRICE
                                WITH KEY EKORG = LV_EKORG
                                         MATNR = GT_LIST-MATNR
                                         BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF LS_0050-LIFNR IS INITIAL.
          CLEAR : GS_VPRICE.
          LOOP AT GT_VPRICE INTO GS_VPRICE FROM SY-TABIX.

            IF GS_VPRICE-EKORG NE LV_EKORG OR
               GS_VPRICE-MATNR NE  GT_LIST-MATNR.
              EXIT.
            ENDIF.

            READ TABLE LT_EXCEPT WITH KEY ZKUNNR_IC = LS_0050-ZKUNNR_IC
                                          WERKS     = LS_0050-WERKS
                                          VTWEG     = LS_0050-VTWEG
                                          LIFNR     = GS_VPRICE-LIFNR
                                          BINARY SEARCH
                                          TRANSPORTING NO FIELDS.
            IF SY-SUBRC = 0.
              CLEAR : GS_VPRICE.
              CLEAR : GT_LIST-LIFNR,     GT_LIST-LIFNR_TXT,
                      GT_LIST-EKORG,     GT_LIST-EKORG_TXT,
                      GT_LIST-BUKRS,     GT_LIST-BUKRS_TXT,
                      GT_LIST-WERKS,     GT_LIST-WERKS_TXT,
                      GT_LIST-KUNNR_WL,  GT_LIST-KUNNR_WL_TXT,
                      GT_LIST-ZKMEIN_MM, GT_LIST-ZKBETR_MM,
                      GT_LIST-ZKONWA_MM, GT_LIST-ZKPEIN_MM.
              CONTINUE.
            ELSE.
              GT_LIST-ICON = ICON_LED_YELLOW.
              GT_LIST-EKORG        = LS_0050-ZEKORG_INFO.
              GT_LIST-EKORG_TXT    = LS_0050-EKORG_TXT.
              GT_LIST-BUKRS        = LS_0050-ZBUKRS_INFO.
              GT_LIST-BUKRS_TXT    = LS_0050-BUKRS_TXT.

              GT_LIST-WERKS        = LS_0050-ZWERKS_INFO.
              GT_LIST-WERKS_TXT    = LS_0050-WERKS_TXT.

              GT_LIST-KUNNR_WL     = GS_VPRICE-LIFNR.
              GT_LIST-KUNNR_WL_TXT = GS_VPRICE-LIFNR_NM.

              GT_LIST-ZKMEIN_MM    = GS_VPRICE-KMEIN.
              GT_LIST-ZKBETR_MM    = GT_LIST-ZBASE_PRICE.
              GT_LIST-ZKPEIN_MM    = GS_VPRICE-KPEIN.
              GT_LIST-ZKONWA_MM    = GT_LIST-KONWA_1.
              APPEND GT_LIST. CLEAR : GS_VPRICE.
            ENDIF.
          ENDLOOP.
        ELSE.

          GT_LIST-ICON         = ICON_LED_YELLOW.
          GT_LIST-EKORG        = LS_0050-ZEKORG_INFO.
          GT_LIST-EKORG_TXT    = LS_0050-EKORG_TXT.
          GT_LIST-BUKRS        = LS_0050-ZBUKRS_INFO.
          GT_LIST-BUKRS_TXT    = LS_0050-BUKRS_TXT.

          GT_LIST-KUNNR_WL     = LS_0050-LIFNR.
          GT_LIST-KUNNR_WL_TXT = LS_0050-LIFNR_NM.

          GT_LIST-ZKMEIN_MM    = GS_VPRICE-KMEIN.
          GT_LIST-ZKBETR_MM    = GT_LIST-ZBASE_PRICE / GS_VPRICE-KPEIN.
          GT_LIST-ZKPEIN_MM    = GS_VPRICE-KPEIN.
          GT_LIST-ZKONWA_MM    = GT_LIST-KONWA_1.
          APPEND GT_LIST. CLEAR : GS_VPRICE.

        ENDIF.
      ELSE.
        GT_LIST-ICON = ICON_LED_RED.
        GT_LIST-MESSAGE = 'Not exist info. record.'.
        CLEAR : GT_LIST-EKORG,     GT_LIST-EKORG_TXT,
                GT_LIST-WERKS,     GT_LIST-WERKS_TXT,
                GT_LIST-KUNNR_WL,  GT_LIST-KUNNR_WL_TXT,
                GT_LIST-ZKMEIN_MM, GT_LIST-ZKBETR_MM,
                GT_LIST-ZKONWA_MM, GT_LIST-BUKRS,
                GT_LIST-BUKRS_TXT.

        APPEND GT_LIST. CLEAR GT_LIST.
      ENDIF.

      CLEAR : LS_A903, LS_0050, GT_LIST.
    ENDLOOP.

    DATA : BEGIN OF LT_WMATNR_C OCCURS 0,
             MATNR TYPE MARC-MATNR,
             WERKS TYPE MARC-WERKS,
           END OF LT_WMATNR_C.

    CLEAR : LT_WMATNR_C, LT_WMATNR_C[].
    LT_WMATNR_C[] = CORRESPONDING #( GT_LIST[] ).
    SORT LT_WMATNR_C BY MATNR WERKS.
    DELETE ADJACENT DUPLICATES FROM LT_WMATNR_C COMPARING ALL FIELDS.

    IF LT_WMATNR_C[] IS NOT INITIAL.
      SELECT MATNR, WERKS
             FROM MARC INTO TABLE @DATA(LT_MARC)
             FOR ALL ENTRIES IN @LT_WMATNR_C
             WHERE MATNR EQ @LT_WMATNR_C-MATNR
             AND   WERKS EQ @LT_WMATNR_C-WERKS
             AND  MMSTA EQ '03'.
      IF SY-SUBRC = 0.
        SORT LT_MARC BY MATNR WERKS.
      ENDIF.


      LOOP AT GT_LIST.

        CLEAR : LV_TABIX.
        LV_TABIX = SY-TABIX.

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
*          GT_LIST-ICON  = ICON_LED_YELLOW.
        ENDIF.

        MODIFY GT_LIST INDEX LV_TABIX TRANSPORTING LVORM ICON MESSAGE.

      ENDLOOP.
    ENDIF.

* check exist data. A914- 가격생성시 입력한 날짜로 가격이 있는지 확인 필요함
    SELECT KAPPL, KSCHL, BUKRS, EKORG, LLIEF AS KUNNR_WL,
           WERKS, MATNR, KFRST, DATAB, DATBI
           INTO TABLE @DATA(LT_A914_TMP)
           FROM A914
           WHERE   DATAB EQ @P_DATE.
    IF SY-SUBRC = 0.

      SORT LT_A914_TMP BY  BUKRS EKORG KUNNR_WL WERKS MATNR.
      DELETE ADJACENT DUPLICATES FROM LT_A914_TMP COMPARING BUKRS EKORG KUNNR_WL WERKS MATNR.

      SORT GT_LIST BY BUKRS EKORG KUNNR_WL WERKS MATNR.

    ENDIF.


    CLEAR : GV_NOT_CONFIRM.
    LOOP AT GT_LIST.
      READ TABLE LT_A914_TMP INTO DATA(LS_A914) WITH KEY  BUKRS = GT_LIST-BUKRS
                                                          EKORG = GT_LIST-EKORG
                                                          KUNNR_WL = GT_LIST-KUNNR_WL
                                                          WERKS = GT_LIST-WERKS
                                                          MATNR = GT_LIST-MATNR BINARY SEARCH.

      IF SY-SUBRC = 0.
        GT_LIST-DATAB = LS_A914-DATAB.
      ELSE.
        CLEAR GT_LIST-DATAB.
      ENDIF.

      MODIFY GT_LIST TRANSPORTING DATAB.

    ENDLOOP.

    LOOP AT GT_LIST WHERE  ZCONFIRM = SPACE.
      GV_NOT_CONFIRM = 'X'.
      EXIT.
    ENDLOOP.

    SORT GT_LIST BY VKORG VTWEG KUNNR KUNWE MATNR.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form Create_price
*&---------------------------------------------------------------------*
FORM CREATE_PRICE .

  DATA : LT_SAVE     LIKE TABLE OF ZSDT0110 WITH HEADER LINE,
         LV_ERR,
         LV_MSG(100),
         LV_CHECK,
         LV_TCODE    LIKE SY-TCODE.

  PERFORM POPUP_MSG USING 'Create price'
                          'Do you want to create price?'
                           LV_CHECK.

  CHECK LV_CHECK EQ '1'.

  CLEAR : GV_SUCCESS, GV_FAILURE.
  _CLEAR LT_SAVE.
  LOOP AT GT_LIST.
    CLEAR LT_SAVE.
    MOVE-CORRESPONDING GT_LIST TO LT_SAVE.

* STEP 1이 있는 경우에만 대상.
    IF GT_LIST-KUNNR_WL IS INITIAL.
      CLEAR : GT_LIST.
      CONTINUE.
    ENDIF.

    _CLEAR GT_INFO.
    GT_INFO-MATNR = GT_LIST-MATNR.
    GT_INFO-LIFNR = GT_LIST-KUNNR_WL.
    GT_INFO-EKORG = GT_LIST-EKORG.
    GT_INFO-WERKS = GT_LIST-WERKS.
    GT_INFO-DATAB = P_DATE.
    GT_INFO-DATBI = P_END.
    GT_INFO-KBETR = GT_LIST-ZKBETR_MM. "GT_LIST-ZBASE_PRICE.
    GT_INFO-KONWA = GT_LIST-ZKONWA_MM.
    GT_INFO-KPEIN = GT_LIST-ZKPEIN_MM.
    GT_INFO-KMEIN = GT_LIST-ZKMEIN_MM.
    APPEND GT_INFO. CLEAR GT_INFO .

*    IF GT_LIST-ZKBETR_MM_OLD IS INITIAL.
    CLEAR LV_TCODE. LV_TCODE = 'ME11'.
*    ELSE.
*      CLEAR LV_TCODE. LV_TCODE = 'ME12'.
*    ENDIF..

*STEP 2 : HQ 매입단가를 생성한다
    CALL FUNCTION 'ZMM_MGNT_INFO_RECORD'
      EXPORTING
        I_BDC_MODE = 'E'
        I_TCODE    = LV_TCODE
      TABLES
        IT_DATA    = GT_INFO.

    READ TABLE GT_INFO INDEX 1.
    IF GT_INFO-TYPE EQ 'S'.
      GT_LIST-ICON = ICON_GREEN_LIGHT.
    ELSE.
      GT_LIST-MESSAGE = GT_INFO-MESSAGE.
      GT_LIST-ICON = ICON_RED_LIGHT.
    ENDIF.

    MODIFY GT_LIST TRANSPORTING ICON MESSAGE.
    APPEND LT_SAVE. CLEAR LT_SAVE.
  ENDLOOP.

  IF LT_SAVE[] IS NOT INITIAL.
    MODIFY ZSDT0110 FROM TABLE LT_SAVE.
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
*& Form CREATE_PRICE_MEK1_BDC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_PRICE_MEK1_BDC .

  CONSTANTS : LC_PB00 TYPE RV13A-KSCHL VALUE 'PB00'.
  DATA : LV_CHECK    TYPE C.
  DATA : LV_DATE     TYPE SY-DATUM,
         LV_KBETR    TYPE CHAR18,
         LV_KPEIN    TYPE CHAR5,
         LV_DATUM    TYPE CHAR10,
         LV_DATUM_TO TYPE CHAR10,
         LV_MSG(255).

  DATA LV_TRANSACTION TYPE SY-TCODE.

  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.

    PERFORM POPUP_MSG USING TEXT-P01
                            TEXT-P02
                            LV_CHECK.

    CHECK LV_CHECK = '1'.

    CLEAR : GV_SUCCESS, GV_FAILURE, GV_ERR.

    CLEAR: GS_BDC_OPT.

    GS_BDC_OPT-DISMODE  = 'N'.
    GS_BDC_OPT-UPDMODE  = 'S'.
    GS_BDC_OPT-RACOMMIT = 'X'.
    GS_BDC_OPT-NOBINPT  = 'X'.

    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
      IF SY-SUBRC = 0.
*오류건 제외
        IF GT_LIST-ICON EQ ICON_LED_RED.
          GV_ERR = 'X'.
          CONTINUE.
        ENDIF.
        _CLEAR: GT_BDCDATA, GT_BDCMSGE.
        IF P_DATE NE GT_LIST-DATAB.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '0100'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'     '/00',
                                    'RV13A-KSCHL'    LC_PB00.

          PERFORM BDC_DYNPRO USING: 'SAPLV14A'       '0100'.
          PERFORM BDC_FIELD  USING: 'BDC_CURSOR'     'RV130-SELKZ(01)',
                                    'RV130-SELKZ(01)' 'X',
                                    'BDC_OKCODE'     '=WEIT'.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '1914'.
          PERFORM BDC_FIELD  USING: 'KOMG-BUKRS'     GT_LIST-BUKRS,
                                    'KOMG-EKORG'     GT_LIST-EKORG,
                                    'KOMG-LLIEF'     GT_LIST-KUNNR_WL,
                                    'KOMG-WERKS'     GT_LIST-WERKS.


          CLEAR : LV_KBETR, LV_KPEIN, LV_DATUM.
          WRITE GT_LIST-ZKBETR_MM TO LV_KBETR CURRENCY GT_LIST-ZKONWA_MM.
          CONDENSE LV_KBETR.
          WRITE GT_LIST-ZKPEIN_MM TO LV_KPEIN UNIT GT_LIST-ZKMEIN_MM.
          CONDENSE LV_KPEIN.

          CLEAR : LV_DATUM_TO.
          LV_DATE = '99991231'.
          WRITE LV_DATE TO LV_DATUM_TO.
          CONDENSE LV_DATUM_TO.
          CLEAR : LV_DATUM.
          WRITE P_DATE TO LV_DATUM.
          CONDENSE LV_DATUM.


          PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '1914'.
          PERFORM BDC_FIELD  USING: 'KOMG-MATNR(01)'  GT_LIST-MATNR,
                                    'KONP-KBETR(01)'  LV_KBETR,
                                    'KONP-KONWA(01)'  GT_LIST-ZKONWA_MM,
                                    'KONP-KPEIN(01)'  LV_KPEIN,
                                    'KONP-KMEIN(01)'  GT_LIST-ZKMEIN_MM,
                                    'RV13A-DATAB(01)'  LV_DATUM,
                                    'RV13A-DATBI(01)'  LV_DATUM_TO,
                                    'BDC_OKCODE'     '/00'.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'     '1914'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'   '=SICH'.

          CLEAR : GT_BDCMSGE, GT_BDCMSGE[].
          CALL TRANSACTION 'MEK1'
                     USING GT_BDCDATA
                     OPTIONS FROM  GS_BDC_OPT
                     MESSAGES INTO GT_BDCMSGE.


        ELSE.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '0100'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'     '/00',
                                    'RV13A-KSCHL'    LC_PB00.

          PERFORM BDC_DYNPRO USING: 'SAPLV14A'       '0100'.
          PERFORM BDC_FIELD  USING: 'BDC_CURSOR'     'RV130-SELKZ(01)',
                                    'RV130-SELKZ(01)' 'X',
                                    'BDC_OKCODE'     '=WEIT'.

          CLEAR : LV_DATUM.
          WRITE P_DATE TO LV_DATUM.
          CONDENSE LV_DATUM.

          PERFORM BDC_DYNPRO USING: 'RV13A914'       '1000'.
          PERFORM BDC_FIELD  USING: 'BDC_CURSOR'     'F001',
                                    'F001'           GT_LIST-BUKRS, "Company Code
                                    'F002'           GT_LIST-EKORG, "Purch. organization
                                    'F003'           GT_LIST-KUNNR_WL, "Goods Supplier
                                    'F004'           GT_LIST-WERKS, "Plant
                                    'F005-LOW'       GT_LIST-MATNR, "Material
                                    'SEL_DATE'       LV_DATUM, " Valid On
                                    'BDC_OKCODE'     '=ONLI'.

          CLEAR : LV_KBETR, LV_KPEIN, LV_DATUM.
          WRITE GT_LIST-ZKBETR_MM TO LV_KBETR CURRENCY GT_LIST-ZKONWA_MM.
          CONDENSE LV_KBETR.
          WRITE GT_LIST-ZKPEIN_MM TO LV_KPEIN UNIT GT_LIST-ZKMEIN_MM.
          CONDENSE LV_KPEIN.

          CLEAR : LV_DATUM_TO.
          LV_DATE = '99991231'.
          WRITE LV_DATE TO LV_DATUM_TO.
          CONDENSE LV_DATUM_TO.
          CLEAR : LV_DATUM.
          WRITE P_DATE TO LV_DATUM.
          CONDENSE LV_DATUM.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '1914'.
          PERFORM BDC_FIELD  USING: "'KOMG-MATNR(01)'  GT_LIST-MATNR,
                                    'KONP-KBETR(01)'  LV_KBETR,
*                                    'KONP-KONWA(01)'  GT_LIST-ZKONWA_MM,
*                                    'KONP-KPEIN(01)'  LV_KPEIN,
*                                    'KONP-KMEIN(01)'  GT_LIST-ZKMEIN_MM,
                                    'RV13A-DATAB(01)'  LV_DATUM,
                                    'RV13A-DATBI(01)'  LV_DATUM_TO,
                                    'BDC_OKCODE'     '/00'.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '1914'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'     '=SICH'.

          CLEAR : GT_BDCMSGE, GT_BDCMSGE[].
          CALL TRANSACTION 'MEK2'
                     USING GT_BDCDATA
                     OPTIONS FROM  GS_BDC_OPT
                     MESSAGES INTO GT_BDCMSGE.

        ENDIF.

        READ TABLE GT_BDCMSGE WITH KEY MSGTYP = 'E'.
        IF SY-SUBRC = 0.
          CLEAR LV_MSG.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              MSGID               = GT_BDCMSGE-MSGID
              MSGNR               = GT_BDCMSGE-MSGNR
              MSGV1               = GT_BDCMSGE-MSGV1
              MSGV2               = GT_BDCMSGE-MSGV2
              MSGV3               = GT_BDCMSGE-MSGV3
              MSGV4               = GT_BDCMSGE-MSGV4
            IMPORTING
              MESSAGE_TEXT_OUTPUT = LV_MSG.
          ADD 1 TO GV_FAILURE.
          GT_LIST-ICON = ICON_LED_RED.
          GT_LIST-MESSAGE = LV_MSG.
        ELSE.
          ADD 1 TO GV_SUCCESS.
          GT_LIST-ICON = ICON_LED_GREEN.
        ENDIF.

        MODIFY GT_LIST INDEX GS_ROWS-INDEX TRANSPORTING ICON MESSAGE.

      ENDIF.
    ENDLOOP.
  ENDIF.

  IF GV_SUCCESS IS NOT INITIAL.
    UPDATE ZSDT0021
    SET    ZINTERCAL_I     = 'X'
           ZINTERCAL_IDATE = SY-DATLO
           ZINTERFACE      = SPACE
           ZINTERFACE_DATE = SPACE
           ZCONFIRM        = SPACE
           ZCONFIRM_DATE   = SPACE
     WHERE ZSTART EQ P_DATE
       AND ZTYPE  EQ GV_ZTYPE.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  form bdc_dynpro
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR GT_BDCDATA.
  GT_BDCDATA-PROGRAM  = PROGRAM.
  GT_BDCDATA-DYNPRO   = DYNPRO.
  GT_BDCDATA-DYNBEGIN = 'X'.
  APPEND GT_BDCDATA.

ENDFORM. " BDC_DYNPRO
*----------------------------------------------------------------------*
*  form  bdc_field
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR GT_BDCDATA.
  GT_BDCDATA-FNAM = FNAM.
  GT_BDCDATA-FVAL = FVAL.
  APPEND GT_BDCDATA.

ENDFORM. " BDC_FIELD
*&---------------------------------------------------------------------*
*& Form GET_DATA_SALES_PRICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_SALES_PRICE .


  DATA : BEGIN OF LT_A305_C OCCURS 0,
           VKORG TYPE A305-VKORG,
           KUNNR TYPE A305-KUNNR,
           VTWEG TYPE A305-VTWEG,
           MATNR TYPE A305-MATNR,
         END OF LT_A305_C.

  CLEAR : GV_ERROR.
  SELECT SINGLE MANDT INTO SY-MANDT
         FROM ZSDT0021
         WHERE ZSTART EQ P_DATE
         AND   ZTYPE  EQ GV_ZTYPE
         AND   ZHQCAL EQ 'X'.
  IF SY-SUBRC NE 0.
    GV_ERROR = 'X'.
  ENDIF.

  CHECK GV_ERROR IS INITIAL.

  CLEAR : GT_LIST, GT_LIST[].
*2. 판매단가 생성
*2.1 매입단가 생성을 위한 기준단가 정보 가져오기
*   ZSDT0040-ZSYSTEM = 'SAP' and ZSEND_SALES = 'X'
*   조건에 해당하는 ZKUNNR_IC 값이 생성대상 법인임

  SELECT A~ZKUNNR_IC, A~BUKRS, A~ZSYSTEM, A~ZSEND_SALES, A~ZSEND_INFO, A~ZHQ, A~REMARK,
         B~VTWEG, B~EKORG, B~ZVKORG_SALES, B~ZVTWEG_SALES, B~ZKUNNR_SALES,
         C~VTEXT AS VKORG_TXT, D~VTEXT AS VTWEG_TXT,
         E~NAME1 AS KUNNR_TXT
         INTO TABLE @DATA(LT_0040)
         FROM ZSDT0040 AS A LEFT JOIN ZSDT0060 AS B
         ON   A~ZKUNNR_IC EQ B~ZKUNNR_IC
         LEFT JOIN TVKOT AS C
         ON   B~ZVKORG_SALES = C~VKORG
         AND  C~SPRAS        = @SY-LANGU
         LEFT JOIN TVTWT AS D
         ON   B~ZVTWEG_SALES EQ D~VTWEG
         LEFT JOIN KNA1 AS E
         ON   B~ZKUNNR_SALES EQ E~KUNNR
         WHERE ZSYSTEM   = 'SAP'
         AND ZSEND_SALES = 'X'.
  CHECK LT_0040[] IS NOT INITIAL.

  SORT LT_0040 BY ZKUNNR_IC EKORG.
  DELETE ADJACENT DUPLICATES FROM LT_0040 COMPARING ALL FIELDS.

  IF GV_ZTYPE NE 'N'.
    PERFORM MAKE_MATNR_RANGES.
  ENDIF.

  SELECT A~KAPPL, A~KSCHL, A~LIFNR, B~NAME1 AS LIFNR_NM,
         A~MATNR, C~MAKTX,
         A~EKORG, D~EKOTX, D~BUKRS, F~BUTXT,
         A~WERKS, E~NAME1 AS WERKS_NM,
         A~ESOKZ, A~DATBI, A~DATAB, A~KNUMH,
         P~KBETR, P~KONWA, P~KPEIN, P~KMEIN,
         G~ZCONFIRM
         INTO TABLE @DATA(LT_A017_TMP)
         FROM  A017 AS A INNER JOIN KONP AS P
         ON    A~KNUMH EQ P~KNUMH
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
         LEFT  JOIN ZSDT0021 AS G
         ON    G~ZSTART EQ @P_DATE
         AND   G~ZTYPE  EQ @GV_ZTYPE
         FOR ALL ENTRIES IN @LT_0040
         WHERE A~KSCHL EQ 'PB00'
         AND   A~LIFNR EQ @LT_0040-ZKUNNR_IC
         AND   A~EKORG EQ @LT_0040-EKORG
         AND   A~WERKS EQ '1001'
         AND   A~DATBI GE @P_DATE
         AND   A~DATAB LE @P_DATE.

  IF GV_ZTYPE NE 'N'.
    DELETE LT_A017_TMP WHERE MATNR NOT IN GR_MATNR.
  ENDIF.

*2.2 판매단가 생성을 위한 데이터 생성
*위에서 찾은 데이터를 가지고 아래와 같이 판매단가를 생성하기 위한 데이터를 만든다
*ZSDT0060-ZKUNNR_IC = ZKUNNR_IC
*                 -EKORG = 2.1에서 가져온 EKORG
*ZVKORG_SALES, ZVTWEG_SALES, ZKUNNR_SALES

  LOOP AT LT_A017_TMP INTO DATA(LS_A017).

    GT_LIST-START_DATE = P_DATE.
    GT_LIST-ZCONFIRM   = LS_A017-ZCONFIRM.
    GT_LIST-LIFNR      = LS_A017-LIFNR.
    GT_LIST-LIFNR_TXT  = LS_A017-LIFNR_NM.
    GT_LIST-EKORG      = LS_A017-EKORG.
    GT_LIST-EKORG_TXT  = LS_A017-EKOTX.
    GT_LIST-WERKS      = LS_A017-WERKS.
    GT_LIST-WERKS_TXT  = LS_A017-WERKS_NM.
    GT_LIST-ZKMEIN_MM  = LS_A017-KMEIN.
    GT_LIST-ZKBETR_MM  = LS_A017-KBETR.
    GT_LIST-ZKONWA_MM  = LS_A017-KONWA.
    GT_LIST-ZKPEIN_MM  = LS_A017-KPEIN.

    GT_LIST-MATNR     = LS_A017-MATNR.
    GT_LIST-MATNR_TXT = LS_A017-MAKTX.

    READ TABLE LT_0040 WITH KEY ZKUNNR_IC = LS_A017-LIFNR
                                EKORG     = LS_A017-EKORG
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      LOOP AT LT_0040 INTO DATA(LS_0040) FROM SY-TABIX.

        IF LS_0040-ZKUNNR_IC NE LS_A017-LIFNR OR
           LS_0040-EKORG     NE LS_A017-EKORG.
          EXIT.
        ENDIF.

        GT_LIST-ICON      = ICON_LED_YELLOW.
        GT_LIST-VKORG     = LS_0040-ZVKORG_SALES.
        GT_LIST-VKORG_TXT = LS_0040-VKORG_TXT.
        GT_LIST-VTWEG     = LS_0040-ZVTWEG_SALES.
        GT_LIST-VTWEG_TXT = LS_0040-VTWEG_TXT.
        GT_LIST-KUNNR     = LS_0040-ZKUNNR_SALES.
        GT_LIST-KUNNR_TXT = LS_0040-KUNNR_TXT.
        GT_LIST-ZKMEIN_SO = GT_LIST-ZKMEIN_MM.
        GT_LIST-ZKBETR_SO = GT_LIST-ZKBETR_MM.
        GT_LIST-ZKPEIN_SO = GT_LIST-ZKPEIN_MM.
        GT_LIST-ZKONWA_SO = GT_LIST-ZKONWA_MM.
        APPEND GT_LIST.

        CLEAR : GT_LIST-ICON,
                GT_LIST-VKORG,     GT_LIST-VKORG_TXT,
                GT_LIST-VTWEG,     GT_LIST-VTWEG_TXT,
                GT_LIST-KUNNR,     GT_LIST-KUNNR_TXT,
                GT_LIST-ZKMEIN_SO, GT_LIST-ZKBETR_SO,
                GT_LIST-ZKONWA_SO, GT_LIST-ZKPEIN_SO.

        CLEAR : LS_0040.

      ENDLOOP.
    ENDIF.

    CLEAR : LS_A017, GT_LIST.
  ENDLOOP.


  CLEAR : LT_A305_C, LT_A305_C[].
  LT_A305_C[] = CORRESPONDING #( GT_LIST[] ).
  SORT LT_A305_C BY VKORG KUNNR VTWEG MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_A305_C COMPARING ALL FIELDS.

  DATA : LV_TABIX TYPE SY-TABIX.

  IF LT_A305_C[] IS NOT INITIAL.
    SELECT A~MATNR, A~VKORG, A~VTWEG, A~KUNNR, A~KSCHL, K~KBETR, K~KONWA, K~KPEIN,
           K~KMEIN, A~DATAB, A~DATBI, K~LOEVM_KO AS LOEVM
           INTO TABLE @DATA(LT_A305)
           FROM A305 AS A INNER JOIN KONP AS K
           ON   A~KNUMH EQ K~KNUMH
           FOR ALL ENTRIES IN @LT_A305_C
           WHERE A~VKORG    EQ @LT_A305_C-VKORG
           AND   A~KUNNR    EQ @LT_A305_C-KUNNR
           AND   A~VTWEG    EQ @LT_A305_C-VTWEG
           AND   A~MATNR    EQ @LT_A305_C-MATNR
           AND   A~DATAB    EQ @P_DATE
           AND   A~KSCHL    EQ 'PR00'
           AND   K~LOEVM_KO EQ @SPACE.
    IF SY-SUBRC = 0.

      SORT GT_LIST BY VKORG VTWEG KUNNR MATNR.
      SORT LT_A305 BY VKORG VTWEG KUNNR MATNR.

      LOOP AT LT_A305 INTO DATA(LS_A305).

        READ TABLE GT_LIST WITH KEY VKORG = LS_A305-VKORG
                                    VTWEG = LS_A305-VTWEG
                                    KUNNR = LS_A305-KUNNR
                                    MATNR = LS_A305-MATNR
                                    BINARY SEARCH.
        IF SY-SUBRC = 0.
          CLEAR : LV_TABIX.
          LV_TABIX = SY-TABIX.

          GT_LIST-DATAB = LS_A305-DATAB.
          MODIFY GT_LIST INDEX LV_TABIX TRANSPORTING DATAB.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CLEAR : GV_NOT_CONFIRM.
  LOOP AT GT_LIST WHERE ZCONFIRM = SPACE.
    GV_NOT_CONFIRM = 'X'.
    EXIT.
  ENDLOOP.

  SORT GT_LIST BY LIFNR EKORG WERKS MATNR VKORG VTWEG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INFO_DATA_PRICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_INFO_DATA_PRICE .

  CONSTANTS : LC_WERKS TYPE WERKS_D VALUE '1001'.

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
  CHECK GT_MAT_C[] IS NOT INITIAL.
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
         FOR ALL ENTRIES IN @GT_MAT_C
         WHERE A~KSCHL EQ 'PB00'
         AND   A~MATNR EQ @GT_MAT_C-MATNR
         AND   A~EKORG EQ @GT_MAT_C-EKORG
         AND   A~WERKS EQ @LC_WERKS
         AND   A~DATBI GE @P_DATE
         AND   A~DATAB LE @P_DATE.
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

      MOVE-CORRESPONDING LS_A017 TO GS_VPRICE.


      GS_VPRICE-VALID_FR = LS_A017-DATAB.
      GS_VPRICE-VALID_TO = LS_A017-DATBI.

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

          MOVE-CORRESPONDING LS_KONP TO GS_VPRICE.

          CLEAR : LS_KONP.

        ENDLOOP.
      ENDIF.

      APPEND GS_VPRICE TO GT_VPRICE.

      CLEAR : LS_A017, GS_VPRICE.

    ENDLOOP.

  ENDIF.

  SORT GT_VPRICE BY EKORG MATNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PRICE_VK11_BDC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_PRICE_VK11_BDC .

  CONSTANTS : LC_PR00 TYPE RV13A-KSCHL VALUE 'PR00'.
  DATA : LV_CHECK    TYPE C.
  DATA : LV_DATE     TYPE SY-DATUM,
         LV_KBETR    TYPE CHAR18,
         LV_KPEIN    TYPE CHAR5,
         LV_DATUM    TYPE CHAR10,
         LV_DATUM_TO TYPE CHAR10,
         LV_MSG(255).

  DATA: LV_DATAB TYPE RV13A-DATAB,
        LV_DATBI TYPE RV13A-DATBI.


  DATA LV_TRANSACTION TYPE SY-TCODE.

  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  CLEAR : GV_SUCCESS, GV_FAILURE, GV_ERR.
  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.

    PERFORM POPUP_MSG USING TEXT-P01
                            TEXT-P02
                            LV_CHECK.

    CHECK LV_CHECK = '1'.

    CLEAR: GS_BDC_OPT.
    GS_BDC_OPT-DISMODE  = 'N'.
    GS_BDC_OPT-UPDMODE  = 'S'.
    GS_BDC_OPT-RACOMMIT = 'X'.
    GS_BDC_OPT-NOBINPT  = 'X'.

    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
      IF SY-SUBRC = 0.
*오류건 제외
        IF GT_LIST-ICON EQ ICON_LED_RED.
          GV_ERR = 'X'.
          CONTINUE.
        ENDIF.

        _CLEAR: GT_BDCDATA, GT_BDCMSGE.
        IF P_DATE NE GT_LIST-DATAB.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '0100'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'     '/00',
                                    'RV13A-KSCHL'    LC_PR00.

          PERFORM BDC_DYNPRO USING: 'SAPLV14A'         '0100'.
          PERFORM BDC_FIELD  USING: 'RV130-SELKZ(01)'  ' ',
                                    'RV130-SELKZ(01)'  'X',
                                    'BDC_OKCODE'       '=WEIT'.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'         '1305'.
          PERFORM BDC_FIELD  USING: 'KOMG-VKORG'       GT_LIST-VKORG,
                                    'KOMG-VTWEG'       GT_LIST-VTWEG,
                                    'KOMG-KUNNR'       GT_LIST-KUNNR.

          CLEAR LV_KBETR.
          WRITE GT_LIST-ZKBETR_SO TO LV_KBETR CURRENCY GT_LIST-ZKONWA_SO.
          CONDENSE LV_KBETR.


          CLEAR LV_KPEIN.
          WRITE GT_LIST-ZKPEIN_SO TO LV_KPEIN UNIT GT_LIST-ZKMEIN_SO.
          CONDENSE LV_KPEIN.

          PERFORM DATE_SETTING USING P_DATE
                            CHANGING LV_DATAB.
          PERFORM DATE_SETTING USING P_END
                            CHANGING LV_DATBI.

          PERFORM BDC_FIELD USING : 'KOMG-MATNR(01)'  GT_LIST-MATNR,
                                    'KONP-KBETR(01)'  LV_KBETR,
                                    'KONP-KONWA(01)'  GT_LIST-ZKONWA_SO,
                                    'KONP-KPEIN(01)'  LV_KPEIN,
                                    'KONP-KMEIN(01)'  GT_LIST-ZKMEIN_SO,
                                    'RV13A-DATAB(01)' LV_DATAB,
                                    'RV13A-DATBI(01)' LV_DATBI,
                                    'BDC_OKCODE'      '=SICH'.

          CLEAR : GT_BDCMSGE, GT_BDCMSGE[].
          CALL TRANSACTION 'VK11'
                     USING GT_BDCDATA
                     OPTIONS FROM  GS_BDC_OPT
                     MESSAGES INTO GT_BDCMSGE.

        ELSE.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'       '0100'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'     '/00',
                                    'RV13A-KSCHL'    LC_PR00.

          PERFORM BDC_DYNPRO USING: 'SAPLV14A'        '0100'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'      '=WEIT',
                                    'RV130-SELKZ(01)' 'X'.


          PERFORM DATE_SETTING USING P_DATE
                              CHANGING LV_DATAB.
          PERFORM DATE_SETTING USING P_END
                            CHANGING LV_DATBI.

          PERFORM BDC_DYNPRO USING: 'RV13A305'        '1000'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'      '=ONLI',
                                    'F001'             GT_LIST-VKORG,      " Sales org.
                                    'F002'             GT_LIST-VTWEG,      " Distribution Channel
                                    'F003'             GT_LIST-KUNNR,      " Sold-to party
                                    'F004-LOW'         GT_LIST-MATNR,      " Material
                                    'SEL_DATE'         LV_DATAB.           " Valid On

          CLEAR LV_KBETR.
          WRITE GT_LIST-ZKBETR_SO TO LV_KBETR CURRENCY GT_LIST-ZKONWA_SO.
          CONDENSE LV_KBETR.

          CLEAR LV_KPEIN.
          WRITE GT_LIST-ZKPEIN_SO TO LV_KPEIN UNIT GT_LIST-ZKMEIN_SO.
          CONDENSE LV_KPEIN.

          PERFORM BDC_DYNPRO USING: 'SAPMV13A'        '1305'.
          PERFORM BDC_FIELD  USING: 'BDC_OKCODE'       '/00',
                                    'KONP-KBETR(01)'   LV_KBETR,
                                    'RV13A-DATAB(01)'  LV_DATAB,
                                    'RV13A-DATBI(01)'  LV_DATBI,
                                    'BDC_OKCODE'       '=SICH'.

          CLEAR : GT_BDCMSGE, GT_BDCMSGE[].
          CALL TRANSACTION 'VK12'
                     USING GT_BDCDATA
                     OPTIONS FROM  GS_BDC_OPT
                     MESSAGES INTO GT_BDCMSGE.

        ENDIF.

        READ TABLE GT_BDCMSGE WITH KEY MSGTYP = 'E'.
        IF SY-SUBRC = 0.
          CLEAR LV_MSG.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              MSGID               = GT_BDCMSGE-MSGID
              MSGNR               = GT_BDCMSGE-MSGNR
              MSGV1               = GT_BDCMSGE-MSGV1
              MSGV2               = GT_BDCMSGE-MSGV2
              MSGV3               = GT_BDCMSGE-MSGV3
              MSGV4               = GT_BDCMSGE-MSGV4
            IMPORTING
              MESSAGE_TEXT_OUTPUT = LV_MSG.
          ADD 1 TO GV_FAILURE.
          GT_LIST-ICON = ICON_LED_RED.
          GT_LIST-MESSAGE = LV_MSG.
        ELSE.
          ADD 1 TO GV_SUCCESS.
          GT_LIST-ICON = ICON_LED_GREEN.
        ENDIF.

        MODIFY GT_LIST INDEX GS_ROWS-INDEX TRANSPORTING ICON MESSAGE.

      ENDIF.
    ENDLOOP.

  ENDIF.

  IF GV_SUCCESS IS NOT INITIAL.
    UPDATE ZSDT0021
     SET   ZINTERCAL_S     = 'X'
           ZINTERCAL_SDATE = SY-DATLO
           ZINTERFACE = SPACE
           ZINTERFACE_DATE = SPACE
           ZCONFIRM = SPACE
           ZCONFIRM_DATE = SPACE
    WHERE ZSTART EQ P_DATE
      AND ZTYPE  EQ GV_ZTYPE.
  ENDIF.

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
*&---------------------------------------------------------------------*
*& Form CONFIRM
*&---------------------------------------------------------------------*
FORM CONFIRM .

  DATA : LV_CHECK    TYPE C.

  PERFORM POPUP_MSG USING 'Confirmation'
                          'Do you want to confirm?'
                          LV_CHECK.

  CHECK LV_CHECK = '1'.

  UPDATE ZSDT0021
     SET ZINTERCAL_S     = 'X'
         ZINTERCAL_SDATE = SY-DATLO
         ZINTERFACE      = SPACE
         ZINTERFACE_DATE = SPACE
         ZCONFIRM        = SPACE
         ZCONFIRM_DATE   = SPACE
   WHERE ZSTART          = P_DATE
     AND ZTYPE           = GV_ZTYPE.

  IF SY-SUBRC = 0.
    MESSAGE S000 WITH 'Confirmation is completed'.
  ENDIF.

ENDFORM.
