*&---------------------------------------------------------------------*
*& Include          ZSDR0180F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT .

  _CLEAR S_INC.
  _RANGE S_INC 'I' 'EQ' '0000001000' ''.


ENDFORM. " SET_INIT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA : LS_MOD_CELLS TYPE LVC_S_MODI.

  CLEAR: LS_MOD_CELLS.

  LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'TRKORR'.

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

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'Z01'.
      SCREEN-INPUT       = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

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

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HANDLE_TOP_OF_PAGE  USING PE_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT.

  DATA : LV_TEXT(255) TYPE C.

  CLEAR : LV_TEXT.
  DESCRIBE TABLE GT_LIST LINES DATA(LV_ROWS).
  LV_TEXT = LV_ROWS.
  CONDENSE LV_TEXT.
  CONCATENATE TEXT-001 ' :' LV_TEXT INTO LV_TEXT SEPARATED BY SPACE.
  CALL METHOD PE_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_FONTSIZE = 'MEDIUM'.

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
FORM GET_DATA .

  DATA : LT_MARC_KEY LIKE TABLE OF MARC WITH HEADER LINE,
         LT_A305_KEY LIKE TABLE OF A305 WITH HEADER LINE,
         LT_A903_KEY LIKE TABLE OF A903 WITH HEADER LINE,
         LT_A017_KEY LIKE TABLE OF A017 WITH HEADER LINE,
         BEGIN OF LT_PLANT OCCURS 0,
           WERKS LIKE T001W-WERKS,
           NAME1 LIKE T001W-NAME1,
         END OF LT_PLANT.

  RANGES : LR_KUNWE FOR A903-KUNWE,
           LR_WERKS FOR T001W-WERKS.

*기준 플랜트
  SELECT C~WERKS, D~NAME1
  INTO TABLE @LT_PLANT
  FROM ZSDT0040 AS A INNER JOIN ZSDT0042 AS B ON A~ZKUNNR_IC  = B~ZKUNNR_IC
                     INNER JOIN ZSDT0050 AS C ON B~ZKUNNR_SUB = C~ZKUNNR_IC
                                             AND B~VTWEG      = C~VTWEG
                     INNER JOIN T001W    AS D ON C~WERKS      = D~WERKS
  WHERE A~ZKUNNR_IC IN @S_INC
    AND B~KUNNR     IN @S_KUNNR.

  IF SY-SUBRC = 0.
    SORT LT_PLANT BY  WERKS.
    DELETE ADJACENT DUPLICATES FROM LT_PLANT COMPARING ALL FIELDS.
  ELSE.

*제외 플랜트
    SELECT B~WERKS
    INTO TABLE @DATA(LT_WERKS_EX)
    FROM ZSDT0042 AS A INNER JOIN ZSDT0050 AS B ON A~ZKUNNR_SUB = B~ZKUNNR_IC
                                               AND A~VTWEG      = B~VTWEG.

    SORT LT_WERKS_EX BY WERKS.
    DELETE ADJACENT DUPLICATES FROM LT_WERKS_EX COMPARING ALL FIELDS.

    _CLEAR LR_WERKS.

    LOOP AT LT_WERKS_EX INTO DATA(LS_EX).
      CHECK LS_EX-WERKS IS NOT INITIAL.
      _RANGE LR_WERKS 'I' 'EQ' LS_EX-WERKS ''.
    ENDLOOP.

    SELECT A~WERKS, B~NAME1
    INTO TABLE @DATA(LT_PLANT2)
    FROM ZSDT0050 AS A INNER JOIN T001W AS B ON A~WERKS = B~WERKS
    WHERE A~ZKUNNR_IC IN @S_KUNNR.


    IF SY-SUBRC = 0.
      DELETE LT_PLANT2 WHERE WERKS IN LR_WERKS.
      SORT LT_PLANT2 BY  WERKS.
      DELETE ADJACENT DUPLICATES FROM LT_PLANT2 COMPARING ALL FIELDS.
      LT_PLANT[] = LT_PLANT2[].
    ELSE.
      LT_PLANT-WERKS = '1001'.
      SELECT SINGLE NAME1 INTO LT_PLANT-NAME1
      FROM T001W
      WHERE WERKS = LT_PLANT-WERKS.
      APPEND LT_PLANT. CLEAR LT_PLANT.
    ENDIF.
  ENDIF.


  SELECT ZKUNNR_IC,
         WERKS,
         LIFNR,
         VTWEG,
         ZKUNNR_S
  INTO TABLE @DATA(LT_0050)
  FROM ZSDT0050.
  SORT LT_0050 BY ZKUNNR_IC WERKS LIFNR.

*-ITALY SHIP TO 제거를 위함----S
  _CLEAR LR_KUNWE.
  LOOP AT LT_0050 INTO DATA(LS_0050).
    CHECK LS_0050-ZKUNNR_S IS NOT INITIAL.

    CHECK ( LS_0050-VTWEG EQ '10' AND LS_0050-ZKUNNR_IC EQ '0000002000' )
     OR  LS_0050-LIFNR EQ '0000040020'.
    _RANGE LR_KUNWE 'I' 'EQ' LS_0050-ZKUNNR_S ''.
  ENDLOOP.
*-ITALY SHIP TO 제거를 위함----E

  SELECT ZKUNNR_IC,
         VTWEG,
         KUNWE,
         EKORG
  INTO TABLE @DATA(LT_0060)
  FROM ZSDT0060.
  SORT LT_0060 BY ZKUNNR_IC VTWEG KUNWE.


  SELECT A~ZKUNNR_IC,
         A~KUNNR,
         A~LIFNR,
         A~ZPRODH_GROUP,
         A~MATNR AS MATNO,
         A~ZSTART,
         A~ZTYPE,
         A~ZMARGIN,
         C~MATNR,
         C~LVORM,
         E~NAME_ORG1 AS ZKUNNR_IC_TXT,
         F~NAME_ORG1 AS KUNNR_TXT,
         G~NAME_ORG1 AS LIFNR_TXT,
         H~VKORG,
         H~VTWEG,
         H~ZKUNNR_S,
         H~ZKUNNR_SUB,
         I~ZKUNNR,
         I~ZKUNNR_DESC,
         J~ZKUNNR_IC AS ZKUNNR_INC,
         K~EKORG,
         L~MAKTX AS MATNR_TXT,
         M~ZKUNNR_S AS KUNWE,
         N~ZKUNNR_S AS KUNWE2
 INTO TABLE @DATA(LT_DATA)
 FROM ZSDT0020 AS A INNER JOIN ZSDT0090 AS B ON A~ZPRODH_GROUP = B~ZPRODH_GROUP
                    INNER JOIN MARA     AS C ON C~PRDHA        = B~PRODH
                    INNER JOIN ZSDT0021 AS D ON A~ZSTART       = D~ZSTART
                                            AND A~ZTYPE        = D~ZTYPE
               LEFT OUTER JOIN BUT000   AS E ON A~ZKUNNR_IC    = E~PARTNER
               LEFT OUTER JOIN BUT000   AS F ON A~KUNNR        = F~PARTNER
               LEFT OUTER JOIN BUT000   AS G ON A~LIFNR        = G~PARTNER

               LEFT OUTER JOIN ZSDT0042 AS H ON A~ZKUNNR_IC    = H~ZKUNNR_IC
                                            AND A~KUNNR        = H~KUNNR
               LEFT OUTER JOIN ZSDT0041 AS I ON H~ZKUNNR_SUB   = I~ZKUNNR_IC
                                            AND A~KUNNR        = I~ZKUNNR
               LEFT OUTER JOIN ZSDT0040 AS J ON A~KUNNR        = J~ZKUNNR_IC
               LEFT OUTER JOIN ZSDT0060 AS K ON H~ZKUNNR_SUB   = K~ZKUNNR_IC
                                            AND H~VTWEG        = K~VTWEG
                                            AND A~KUNNR        = K~KUNNR
               LEFT OUTER JOIN MAKT     AS L ON C~MATNR        = L~MATNR
                                            AND L~SPRAS        = @SY-LANGU
               LEFT OUTER JOIN ZSDT0050 AS M ON H~ZKUNNR_SUB   = M~ZKUNNR_IC
                                            AND H~VTWEG        = M~VTWEG
                                            AND A~LIFNR        = M~LIFNR
               LEFT OUTER JOIN ZSDT0050 AS N ON H~ZKUNNR_SUB   = N~ZKUNNR_IC
                                            AND H~VTWEG        = N~VTWEG
                                            AND N~LIFNR        = @SPACE
 WHERE A~ZKUNNR_IC IN @S_INC
   AND A~KUNNR     IN @S_KUNNR
   AND A~LIFNR     IN @S_LIFNR
   AND C~MATNR     IN @S_MATNR
   AND A~ZTYPE     EQ 'N'
   AND D~ZCONFIRM  EQ 'X'.

  SORT LT_DATA BY ZKUNNR_IC    ASCENDING
                  KUNNR        ASCENDING
                  LIFNR        ASCENDING
                  ZPRODH_GROUP ASCENDING
                  ZSTART       DESCENDING
                  MATNR        ASCENDING.

  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING ZKUNNR_IC
                                                    KUNNR
                                                    LIFNR
                                                    ZPRODH_GROUP
                                                    MATNR.

  MOVE-CORRESPONDING LT_DATA[] TO LT_MARC_KEY[].
  SORT LT_MARC_KEY BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_MARC_KEY COMPARING MATNR.

  SELECT MATNR, WERKS, MMSTA
  INTO TABLE @DATA(LT_MARC)
  FROM MARC
  FOR ALL ENTRIES IN @LT_MARC_KEY
  WHERE WERKS IN @S_WERKS
    AND MATNR EQ @LT_MARC_KEY-MATNR.

  SORT LT_MARC BY MATNR WERKS.

  _CLEAR LT_A305_KEY.
  LOOP AT LT_DATA INTO DATA(LS_DATA1).
    LT_A305_KEY-DATAB = LS_DATA1-ZSTART.
    LT_A305_KEY-MATNR = LS_DATA1-MATNR.
    _COLLECT LT_A305_KEY.
    LT_A903_KEY-DATAB = LS_DATA1-ZSTART.
    LT_A903_KEY-MATNR = LS_DATA1-MATNR.
    _COLLECT LT_A903_KEY.
    LT_A017_KEY-DATAB = LS_DATA1-ZSTART.
    LT_A017_KEY-LIFNR = LS_DATA1-LIFNR.
    LT_A017_KEY-MATNR = LS_DATA1-MATNR.
    _COLLECT LT_A017_KEY.
  ENDLOOP.


  SELECT A~VKORG, A~MATNR, A~KUNNR, A~KSCHL,
         A~DATAB, A~DATBI, A~VTWEG,
         B~KBETR, B~KONWA,B~KPEIN, B~KMEIN
         INTO TABLE @DATA(LT_A305)
         FROM A305 AS A INNER JOIN KONP AS B
                                        ON A~KNUMH EQ B~KNUMH
         FOR ALL ENTRIES IN @LT_A305_KEY
         WHERE A~DATBI    GE @LT_A305_KEY-DATAB
         AND   A~DATAB    LE @LT_A305_KEY-DATAB
         AND   A~KSCHL    EQ 'PR00'
         AND   A~MATNR    EQ @LT_A305_KEY-MATNR
         AND   B~LOEVM_KO EQ @ABAP_OFF.

  SORT LT_A305 BY VKORG KUNNR VTWEG MATNR.

  SELECT  MATNR, ZKUNNR, DATAB, DATBI,
          VTWEG, KBETR,  KONWA, KPEIN,
          KMEIN
         INTO TABLE @DATA(LT_0070)
         FROM ZSDT0070
         FOR ALL ENTRIES IN @LT_A305_KEY
         WHERE DATBI GE @LT_A305_KEY-DATAB
         AND   DATAB LE @LT_A305_KEY-DATAB
         AND   MATNR EQ @LT_A305_KEY-MATNR.

  SORT LT_0070 BY ZKUNNR VTWEG MATNR.

  SELECT A~VKORG, A~MATNR, A~VTWEG, A~KUNNR,
         A~KUNWE, A~KSCHL, A~DATAB, A~DATBI,
         B~KBETR, B~KONWA, B~KPEIN, B~KMEIN
         INTO TABLE @DATA(LT_A903)
         FROM A903 AS A INNER JOIN KONP AS B
         ON   A~KNUMH EQ B~KNUMH
         FOR ALL ENTRIES IN @LT_A903_KEY
         WHERE A~VKORG    EQ '1001'
         AND   A~DATBI    GE @LT_A903_KEY-DATAB
         AND   A~DATAB    LE @LT_A903_KEY-DATAB
         AND   A~MATNR    EQ @LT_A903_KEY-MATNR
*         AND   A~KUNWE    NOT IN  @LR_KUNWE
         AND   A~KSCHL    EQ 'PR00'
         AND   B~LOEVM_KO EQ @ABAP_OFF.

  SORT LT_A903 BY KUNNR KUNWE MATNR.

  SELECT A~KAPPL, A~KSCHL, A~LIFNR, A~MATNR,
         A~EKORG, A~WERKS, A~ESOKZ, A~DATBI,
         A~DATAB, A~KNUMH,
         B~KBETR, B~KONWA, B~KPEIN, B~KMEIN
         INTO TABLE @DATA(LT_A017)
         FROM  A017 AS A INNER JOIN KONP AS B
         ON A~KNUMH EQ B~KNUMH
         FOR ALL ENTRIES IN @LT_A017_KEY
         WHERE A~KSCHL EQ 'PB00'
         AND   A~MATNR EQ @LT_A017_KEY-MATNR
         AND   A~LIFNR EQ @LT_A017_KEY-LIFNR
         AND   A~DATBI GE @LT_A017_KEY-DATAB
         AND   A~DATAB LE @LT_A017_KEY-DATAB.

  SORT LT_A017 BY WERKS EKORG LIFNR MATNR.

  _CLEAR GT_LIST.
  LOOP AT LT_PLANT.
    LOOP AT LT_DATA INTO DATA(LS_DATA).
      MOVE-CORRESPONDING LS_DATA TO  GT_LIST.
      GT_LIST-WERKS     = LT_PLANT-WERKS.
      GT_LIST-WERKS_TXT = LT_PLANT-NAME1.
      IF LS_DATA-KUNWE IS INITIAL.
        LS_DATA-ZKUNNR_S = LS_DATA-KUNWE2.
      ELSE.
        LS_DATA-ZKUNNR_S = LS_DATA-KUNWE.
      ENDIF.

      READ TABLE LT_MARC INTO DATA(LS_MARC) WITH KEY MATNR = GT_LIST-MATNR
                                                     WERKS = GT_LIST-WERKS BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF LS_MARC-MMSTA = '03'.
          GT_LIST-LVORM = 'X'.
        ENDIF.
      ELSE.
        GT_LIST-CHK_PLANT = 'X'.
      ENDIF.

      IF LS_DATA-ZKUNNR_SUB IS NOT INITIAL AND LS_DATA-ZKUNNR IS INITIAL.
        GT_LIST-FLAG = 'MDDP'.
        READ TABLE LT_A305 INTO DATA(LS_A305) WITH KEY VKORG = LS_DATA-VKORG
                                                       KUNNR = LS_DATA-KUNNR
                                                       VTWEG = LS_DATA-VTWEG
                                                       MATNR = LS_DATA-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-KBETR_BASE = LS_A305-KBETR / LS_A305-KPEIN.
          GT_LIST-KONWA = LS_A305-KONWA.
        ENDIF.
      ENDIF.

      IF LS_DATA-ZKUNNR_SUB IS NOT INITIAL AND LS_DATA-ZKUNNR IS NOT INITIAL.
        GT_LIST-FLAG = 'MDDP'.
        READ TABLE LT_0070 INTO DATA(LS_0070) WITH KEY ZKUNNR = LS_DATA-KUNNR
                                                        VTWEG = LS_DATA-VTWEG
                                                        MATNR = LS_DATA-MATNR  BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-KBETR_BASE = LS_0070-KBETR / LS_0070-KPEIN.
          GT_LIST-KONWA = LS_0070-KONWA.
        ENDIF.
      ENDIF.

      IF GT_LIST-FLAG = 'MDDP'.
        READ TABLE LT_A903 INTO DATA(LS_A903_1) WITH KEY KUNNR = LS_DATA-ZKUNNR_SUB
                                                         KUNWE = LS_DATA-ZKUNNR_S
                                                         VTWEG = LS_DATA-VTWEG
                                                         MATNR = LS_DATA-MATNR.
        IF SY-SUBRC = 0.
          GT_LIST-KBETR_COND = LS_A903_1-KBETR / LS_A903_1-KPEIN.
        ENDIF.
      ENDIF.

      IF LS_DATA-ZKUNNR_SUB IS INITIAL AND LS_DATA-ZKUNNR IS INITIAL.
        GT_LIST-FLAG = 'FOB'.
        IF LS_DATA-ZKUNNR_INC IS INITIAL.
          READ TABLE LT_A305 INTO DATA(LS_A305_1) WITH KEY VKORG = '1001'
                                                           KUNNR = LS_DATA-KUNNR
                                                           VTWEG = '30'
                                                           MATNR = LS_DATA-MATNR BINARY SEARCH.
          IF SY-SUBRC = 0.
            GT_LIST-KBETR_BASE = LS_A305_1-KBETR / LS_A305_1-KPEIN.
            GT_LIST-KONWA = LS_A305_1-KONWA.

            READ TABLE LT_0060 INTO DATA(LS_0060) WITH KEY ZKUNNR_IC = LS_DATA-KUNNR
                                                           VTWEG     = '30' BINARY SEARCH.
            IF SY-SUBRC = 0.
              GT_LIST-EKORG = LS_0060-EKORG.
            ENDIF.
          ENDIF.
        ELSE.

          READ TABLE LT_0050 INTO DATA(LS_0050_1) WITH KEY ZKUNNR_IC = LS_DATA-KUNNR
                                                           WERKS     = GT_LIST-WERKS
                                                           LIFNR     = LS_DATA-LIFNR BINARY SEARCH.
          IF SY-SUBRC = 0.
            GT_LIST-KUNWE = LS_0050_1-ZKUNNR_S.

          ELSE.
            READ TABLE LT_0050 INTO DATA(LS_0050_2) WITH KEY ZKUNNR_IC = LS_DATA-KUNNR
                                                             WERKS     = GT_LIST-WERKS
                                                             LIFNR     = ' ' BINARY SEARCH.
            IF SY-SUBRC = 0.
              GT_LIST-KUNWE = LS_0050_2-ZKUNNR_S.
            ENDIF.
          ENDIF.

          READ TABLE LT_A903 INTO DATA(LS_A903) WITH KEY KUNNR = LS_DATA-KUNNR
                                                         KUNWE = GT_LIST-KUNWE
                                                         MATNR = LS_DATA-MATNR BINARY SEARCH.
          IF SY-SUBRC = 0.
            GT_LIST-KBETR_BASE = LS_A903-KBETR / LS_A903-KPEIN.
            GT_LIST-KONWA = LS_A903-KONWA.

            READ TABLE LT_0060 INTO DATA(LS_0060_1) WITH KEY ZKUNNR_IC = LS_DATA-KUNNR
                                                             VTWEG     = LS_A903-VTWEG
                                                             KUNWE     = GT_LIST-KUNWE BINARY SEARCH.
            IF SY-SUBRC = 0.
              GT_LIST-EKORG = LS_0060_1-EKORG.
            ENDIF.
          ENDIF.
        ENDIF.
        GT_LIST-KBETR_COND = GT_LIST-KBETR_BASE.
        GT_LIST-KBETR_REQ  = GT_LIST-KBETR_BASE.
      ENDIF.

      READ TABLE LT_A017 INTO DATA(LS_A017_1) WITH KEY WERKS = GT_LIST-WERKS
                                                       EKORG = GT_LIST-EKORG
                                                       LIFNR = LS_DATA-LIFNR
                                                       MATNR = LS_DATA-MATNR
                                                       BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-KBETR_REQ = LS_A017_1-KBETR.
      ENDIF.

      READ TABLE LT_A017 INTO DATA(LS_A017) WITH KEY WERKS = '1001'
                                                     EKORG = GT_LIST-EKORG
                                                     LIFNR = LS_DATA-LIFNR
                                                     MATNR = LS_DATA-MATNR
                                                     BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-KBETR_INFO = LS_A017-KBETR.
      ENDIF.

      _APPEND GT_LIST.
    ENDLOOP.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MESSAGE
*&---------------------------------------------------------------------*
FORM GET_MESSAGE  USING    PV_MSG
                           PV_STAPA1
                           PV_STAPA2
                           PV_STAPA3
                           PV_STAPA4
                           PV_STAMID
                           PV_STAMNO.

  CLEAR PV_MSG.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = PV_STAMID
      MSGNR               = PV_STAMNO
      MSGV1               = PV_STAPA1
      MSGV2               = PV_STAPA2
      MSGV3               = PV_STAPA3
      MSGV4               = PV_STAPA4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = PV_MSG.

ENDFORM.
