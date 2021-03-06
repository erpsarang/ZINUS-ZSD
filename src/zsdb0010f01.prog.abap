*&---------------------------------------------------------------------*
*& Include          ZSDB0010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT.

  DATA: LT_PARAMETER TYPE TABLE OF BAPIPARAM WITH HEADER LINE,
        LT_RETURN    TYPE TABLE OF BAPIRET2  WITH HEADER LINE.

  P_SPART = '00'.
  S_SPART-LOW = '00'.
  S_SPART-SIGN = 'I'.
  S_SPART-OPTION = 'EQ'.
  APPEND S_SPART.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      USERNAME  = SY-UNAME
    TABLES
      PARAMETER = LT_PARAMETER
      RETURN    = LT_RETURN.
  READ TABLE LT_PARAMETER WITH KEY PARID = 'VKO'.
  IF SY-SUBRC EQ 0.
    P_VKORG = LT_PARAMETER-PARVA.
    S_VKORG-LOW = LT_PARAMETER-PARVA.
    S_VKORG-OPTION = 'EQ'.
    S_VKORG-SIGN = 'I'.
    APPEND S_VKORG.
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

  IF P_UPLOAD = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'M2'.
        SCREEN-ACTIVE  = '0'.
        SCREEN-INPUT  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'M1'.
        SCREEN-ACTIVE  = '0'.
        SCREEN-INPUT  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

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
      I_END_COL               = 15
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
      IF L_TYPE = 'D'.
        REPLACE ALL OCCURRENCES OF '-' IN LT_INTERN-VALUE WITH ' '.
        REPLACE ALL OCCURRENCES OF '/' IN LT_INTERN-VALUE WITH ' '.
        REPLACE ALL OCCURRENCES OF '.' IN LT_INTERN-VALUE WITH ' '.
        CONDENSE LT_INTERN-VALUE NO-GAPS.
      ENDIF.

      <FS> = LT_INTERN-VALUE.
      AT END OF ROW.
        PERFORM ALPHA_INPUT USING : GT_UPLOAD-KUNNR,
                                    GT_UPLOAD-KUNNR2,
                                    GT_UPLOAD-KUNNR3.
        APPEND GT_UPLOAD.
        _RANGE GR_KUNNR 'I' 'EQ' GT_UPLOAD-KUNNR '' .
        _RANGE GR_KUNNR 'I' 'EQ' GT_UPLOAD-KUNNR2 '' .
        _RANGE GR_KUNNR 'I' 'EQ' GT_UPLOAD-KUNNR3 '' .
      ENDAT.
    ENDLOOP.

    SORT GT_UPLOAD BY BSTKD.

    DELETE GR_KUNNR WHERE LOW EQ SPACE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DATA .

  DATA : LT_TEMP LIKE TABLE OF GT_LIST WITH HEADER LINE.

  DATA : BEGIN OF LT_A305_KEY OCCURS 0,
           KUNNR      LIKE A305-KUNNR,
           MATNR      LIKE A305-MATNR,
           PRICE_DATE LIKE A305-DATBI,
         END OF LT_A305_KEY.

  DATA : BEGIN OF LT_903_KEY OCCURS 0,
           KUNNR      LIKE A903-KUNNR,
           KUNWE      LIKE A903-KUNWE,
           MATNR      LIKE A903-MATNR,
           PRICE_DATE LIKE A903-DATBI,
         END OF LT_903_KEY.
  DATA : LV_KSCHL LIKE A305-KSCHL.

  DATA : LV_POSNR  LIKE VBAP-POSNR,
         LV_CHANGE,
         LV_END.

  DATA : BEGIN OF LT_ITEMS OCCURS 0,
           BSTKD  LIKE VBKD-BSTKD,
           MATNR  LIKE VBAP-MATNR,
           KUNNR3 LIKE KNA1-KUNNR,
           KWMENG LIKE VBAP-KWMENG,
         END OF LT_ITEMS,
         LT_DUP LIKE TABLE OF GT_DUP WITH HEADER LINE.

  CHECK GT_UPLOAD[] IS NOT INITIAL.

*-PO Duplication Check
  SELECT A~BSTKD,
         B~VBELN,
         C~POSNR,
         C~MATNR,
         C~KWMENG,
         C~NETPR,
         D~KUNNR AS KUNWE_H,
         E~LIFNR AS LIFNR_H,
         F~KUNNR AS KUNWE_I,
         G~LIFNR AS LIFNR_I,
         H~VBELN AS VBELN_VL,
         H~POSNR AS POSNR_VL,
         I~SO_VBELN AS IF_VBELN,
         K~EBELN,
         K~EBELP
     INTO CORRESPONDING FIELDS OF TABLE @GT_DUP
     FROM VBKD AS A INNER JOIN VBAK AS B ON A~VBELN      = B~VBELN
                    INNER JOIN VBAP AS C ON A~VBELN      = C~VBELN
                    INNER JOIN VBPA AS D ON A~VBELN      = D~VBELN
                                        AND D~POSNR      = '000000'
                                        AND D~PARVW      = 'WE'
                    INNER JOIN VBPA AS E ON A~VBELN      = E~VBELN
                                        AND E~POSNR      = '000000'
                                        AND E~PARVW      = 'WL'
               LEFT OUTER JOIN VBPA AS F ON A~VBELN      = F~VBELN
                                        AND F~POSNR      = C~POSNR
                                        AND F~PARVW      = 'WE'
               LEFT OUTER JOIN VBPA AS G ON A~VBELN      = G~VBELN
                                        AND G~POSNR      = C~POSNR
                                        AND G~PARVW      = 'ZS'
               LEFT OUTER JOIN LIPS AS H ON C~VBELN      = H~VGBEL
                                        AND C~POSNR      = H~VGPOS
           LEFT OUTER JOIN ZMMT0021 AS I ON C~VBELN      = I~SO_VBELN
                                        AND C~POSNR      = I~SO_POSNR
                                        AND I~ZSAP_RTYPE <> 'E'
               LEFT OUTER JOIN VBEP AS J ON C~VBELN      = J~VBELN
                                        AND C~POSNR      = J~POSNR
                                        AND J~BANFN      <> @SPACE
               LEFT OUTER JOIN EKPO AS K ON J~BANFN      = K~BANFN
                                        AND J~BNFPO      = K~BNFPO
                                        AND K~LOEKZ      = @SPACE
      FOR ALL ENTRIES IN @GT_UPLOAD
       WHERE A~BSTKD EQ @GT_UPLOAD-BSTKD
       AND   B~VBTYP NOT IN ('K', 'L').
  SORT GT_DUP BY BSTKD.

  _CLEAR LT_DUP.
  LT_DUP[] = GT_DUP[].
  SORT LT_DUP  BY BSTKD MATNR.

*SKU check
  SELECT A~MATNR,
         A~MTART,
         A~MEINS,
         B~MAKTX
  INTO TABLE @DATA(LT_MAKT)
  FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
  FOR ALL ENTRIES IN @GT_UPLOAD
  WHERE A~MATNR = @GT_UPLOAD-MATNR
    AND B~SPRAS = @SY-LANGU.
  SORT LT_MAKT BY MATNR.

*BP check
  SELECT PARTNER, NAME_ORG1
  INTO TABLE @DATA(LT_BP)
  FROM BUT000
  WHERE PARTNER IN @GR_KUNNR.
  SORT LT_BP BY PARTNER.

*Goods supplier
  SELECT LIFNR, NAME1, EMNFR
  INTO TABLE @DATA(LT_LFA1)
  FROM LFA1
  FOR ALL ENTRIES IN @GT_UPLOAD
  WHERE EMNFR EQ @GT_UPLOAD-KUNNR3.
  SORT LT_LFA1 BY EMNFR.

*PLANT check
  SELECT WERKS, NAME1
  INTO TABLE @DATA(LT_T001W)
  FROM T001W
  FOR ALL ENTRIES IN @GT_UPLOAD
  WHERE WERKS EQ @GT_UPLOAD-WERKS.
  SORT LT_T001W BY WERKS.

*SLOC check
  SELECT LGORT, LGOBE
  INTO TABLE @DATA(LT_T001L)
  FROM T001L
  FOR ALL ENTRIES IN @GT_UPLOAD
  WHERE LGORT EQ @GT_UPLOAD-LGORT.
  SORT LT_T001L BY LGORT.

*Valuation Type ??????
  SELECT A~PARTNER,
         B~COUNTRY
  INTO TABLE @DATA(LT_ADRC)
  FROM BUT020 AS A INNER JOIN ADRC AS B ON A~ADDRNUMBER = B~ADDRNUMBER
  FOR ALL ENTRIES IN @GT_UPLOAD
  WHERE A~PARTNER EQ @GT_UPLOAD-KUNNR3.

  SELECT A~PARTNER,
         B~COUNTRY
  APPENDING CORRESPONDING FIELDS OF TABLE @LT_ADRC
  FROM BUT020 AS A INNER JOIN ADRC AS B ON A~ADDRNUMBER = B~ADDRNUMBER
  FOR ALL ENTRIES IN @LT_LFA1
  WHERE A~PARTNER EQ @LT_LFA1-LIFNR.

  SORT LT_ADRC BY PARTNER.

*Material Valuation
  SELECT MATNR,
         BWKEY,
         BWTAR
  INTO TABLE @DATA(LT_MBEW)
  FROM MBEW
  FOR ALL ENTRIES IN @GT_UPLOAD
    WHERE MATNR EQ @GT_UPLOAD-MATNR
      AND BWKEY EQ @GT_UPLOAD-WERKS
      AND BKLAS NE ''.
  SORT LT_MBEW BY MATNR BWKEY BWTAR.

*SHIP-TO PARTY
  SELECT A~ZKUNNR_IC,
         A~WERKS,
         A~LIFNR,
         A~VTWEG,
         A~ZKUNNR_S,
         B~NAME_ORG1
  INTO TABLE @DATA(LT_0050)
  FROM ZSDT0050 AS A INNER JOIN BUT000 AS B ON A~ZKUNNR_S EQ B~PARTNER.
  SORT LT_0050 BY ZKUNNR_IC LIFNR VTWEG.

*check Intercompany
  SELECT ZKUNNR_IC
  INTO TABLE @DATA(LT_0040)
  FROM ZSDT0040.
  SORT LT_0040 BY ZKUNNR_IC.

*-CURRENCY
  CLEAR GV_WAERS.
  SELECT SINGLE WAERS
  INTO GV_WAERS
  FROM TVKO
  WHERE VKORG = P_VKORG.

*ASIN ?????????????????? ????????? ??????
  SELECT ZKUNNR_IC,
         VTWEG,
         KUNNR
    FROM ZSDT0060 INTO CORRESPONDING FIELDS OF TABLE @GT_0060
    WHERE KUNNR NE @SPACE.
  SORT GT_0060 BY ZKUNNR_IC VTWEG.


  _CLEAR : LT_A305_KEY, LT_903_KEY.
  SORT GT_UPLOAD BY BSTKD.
*-Check Condition
  LOOP AT GT_UPLOAD.
*SHIP TO ???????????? ??????
    IF GT_UPLOAD-KUNNR2 IS INITIAL.
      READ TABLE LT_0040 INTO DATA(LS_0040A) WITH KEY ZKUNNR_IC = GT_UPLOAD-KUNNR BINARY SEARCH.
      "???????????? ????????? ??????????????? 30??? ?????? SOLD-TO = SHIP TO
      IF SY-SUBRC <> 0 AND P_VTWEG = '30'.
        GT_UPLOAD-KUNNR2     = GT_UPLOAD-KUNNR.
      ELSE.
        READ TABLE LT_0050 INTO DATA(LS_0050A) WITH KEY ZKUNNR_IC = GT_UPLOAD-KUNNR
                                                        LIFNR     = GT_UPLOAD-KUNNR3
                                                        VTWEG     = P_VTWEG BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_UPLOAD-KUNNR2 = LS_0050A-ZKUNNR_S.
        ELSE.
          READ TABLE LT_0050 INTO DATA(LS_0050B) WITH KEY ZKUNNR_IC = GT_UPLOAD-KUNNR
                                                           LIFNR    = SPACE
                                                           VTWEG    = P_VTWEG BINARY SEARCH.
          IF SY-SUBRC = 0.
            GT_UPLOAD-KUNNR2  = LS_0050B-ZKUNNR_S.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*?????????????????? A903/ ???????????? A305?????? CONDITION DATA?????????
    READ TABLE LT_0040 INTO DATA(LS_0040B) WITH KEY ZKUNNR_IC = GT_UPLOAD-KUNNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      LT_903_KEY-KUNNR      = GT_UPLOAD-KUNNR.
      LT_903_KEY-KUNWE      = GT_UPLOAD-KUNNR2.
      LT_903_KEY-MATNR      = GT_UPLOAD-MATNR.
      LT_903_KEY-PRICE_DATE = GT_UPLOAD-PRSDT.
      COLLECT LT_903_KEY. CLEAR LT_903_KEY.
    ELSE.
      LT_A305_KEY-KUNNR      = GT_UPLOAD-KUNNR.
      LT_A305_KEY-MATNR      = GT_UPLOAD-MATNR.
      LT_A305_KEY-PRICE_DATE = GT_UPLOAD-PRSDT.
      COLLECT LT_A305_KEY. CLEAR LT_A305_KEY.
    ENDIF.
  ENDLOOP.

  CLEAR LV_KSCHL.
  IF P_VKORG = '1002'.
    LV_KSCHL   = 'PR01'.
  ELSE.
    LV_KSCHL   = 'PR00'.
  ENDIF.

  IF LT_A305_KEY[] IS NOT INITIAL.
    SELECT A~VKORG, A~MATNR, A~KUNNR, A~KSCHL,
           A~DATAB, A~DATBI, A~VTWEG, B~KBETR, B~KONWA
           INTO TABLE @DATA(LT_A305)
           FROM A305 AS A INNER JOIN KONP AS B
                                          ON A~KNUMH EQ B~KNUMH
           FOR ALL ENTRIES IN @LT_A305_KEY
           WHERE A~VKORG    EQ @P_VKORG
           AND   A~KUNNR    EQ @LT_A305_KEY-KUNNR
           AND   A~MATNR    EQ @LT_A305_KEY-MATNR
           AND   A~VTWEG    EQ @P_VTWEG
           AND   A~DATBI    GE @LT_A305_KEY-PRICE_DATE
           AND   A~DATAB    LE @LT_A305_KEY-PRICE_DATE
           AND   A~KSCHL    EQ @LV_KSCHL
           AND   B~LOEVM_KO EQ @ABAP_OFF.

    SORT LT_A305 BY KUNNR MATNR.
  ENDIF.

  IF LT_903_KEY[] IS NOT INITIAL.
    SELECT A~VKORG, A~MATNR, A~KUNNR, A~KUNWE, A~KSCHL,
           A~DATAB, A~DATBI, A~VTWEG, B~KBETR, B~KONWA
           INTO TABLE @DATA(LT_A903)
           FROM A903 AS A INNER JOIN KONP AS B
                                          ON A~KNUMH EQ B~KNUMH
           FOR ALL ENTRIES IN @LT_903_KEY
           WHERE A~VKORG    EQ @P_VKORG
           AND   A~KUNNR    EQ @LT_903_KEY-KUNNR
           AND   A~KUNWE    EQ @LT_903_KEY-KUNWE
           AND   A~MATNR    EQ @LT_903_KEY-MATNR
           AND   A~VTWEG    EQ @P_VTWEG
           AND   A~DATBI    GE @LT_903_KEY-PRICE_DATE
           AND   A~DATAB    LE @LT_903_KEY-PRICE_DATE
           AND   A~KSCHL    EQ @LV_KSCHL
           AND   B~LOEVM_KO EQ @ABAP_OFF.

    SORT LT_A903 BY KUNNR KUNWE MATNR.
  ENDIF.

  CLEAR : GT_LIST , GT_LIST[].
  LOOP AT GT_UPLOAD.
    MOVE-CORRESPONDING GT_UPLOAD TO GT_LIST.
    GT_LIST-VTWEG = P_VTWEG.
    GT_LIST-SPART = P_SPART.

*-MATERIAL CHECK
    READ TABLE LT_MAKT INTO DATA(LS_MAKT) WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-MEINS     = LS_MAKT-MEINS.
      GT_LIST-MATNR_TXT = LS_MAKT-MAKTX.
      IF LS_MAKT-MTART = 'SET' OR LS_MAKT-MTART = 'SETV'.
        CLEAR GT_LIST-LGORT.
        PERFORM GET_COMPONENT USING GT_LIST-MATNR.
      ENDIF.
    ELSE.
      PERFORM ERROR_FIELD USING TEXT-E02.
    ENDIF.

*BP CHECK
    READ TABLE LT_BP INTO DATA(LS_BP) WITH KEY PARTNER = GT_LIST-KUNNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-KUNNR_TXT = LS_BP-NAME_ORG1.
    ELSE.
      PERFORM ERROR_FIELD USING TEXT-E03.
    ENDIF.

*UPLOAD??? Goods supplier
*1 BP??? ????????? ??????
* ?????????
*2 LFA1-EMNFR ??? ??????
    READ TABLE LT_BP INTO DATA(LS_BP3) WITH KEY PARTNER = GT_LIST-KUNNR3 BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-KUNNR3_TXT = LS_BP3-NAME_ORG1.

      READ TABLE LT_ADRC INTO DATA(LS_ADRC) WITH KEY PARTNER = GT_LIST-KUNNR3.
      GT_LIST-COUNTRY = LS_ADRC-COUNTRY.
      CLEAR LS_ADRC.

    ELSE.

      READ TABLE LT_LFA1 INTO DATA(LS_LFA1) WITH KEY EMNFR = GT_LIST-KUNNR3.
      IF SY-SUBRC = 0.
        GT_LIST-KUNNR3_TXT = LS_LFA1-NAME1.
        GT_LIST-GOODSUP = LS_LFA1-LIFNR.

        READ TABLE LT_ADRC INTO DATA(LS_ADRC2) WITH KEY PARTNER = LS_LFA1-LIFNR.
        GT_LIST-COUNTRY = LS_ADRC2-COUNTRY.
        CLEAR LS_ADRC.

      ELSE.
        PERFORM ERROR_FIELD USING TEXT-E08.
      ENDIF.
    ENDIF.

*ship to party??? ???????????? ????????? ??????,
*????????? ????????? ??????
*????????? ZSDT0050??????????????? ????????????
*1)sold to , Goods supplier, ??????????????? ?????? ?????????
*2)sold to , ??????????????? ????????? ??????
    IF GT_LIST-KUNNR2 IS INITIAL.
      READ TABLE LT_0040 INTO DATA(LS_0040) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR BINARY SEARCH.
      "???????????? ????????? ??????????????? 30??? ?????? SOLD-TO = SHIP TO
      IF SY-SUBRC <> 0 AND P_VTWEG = '30'.
        GT_LIST-KUNNR2     = GT_LIST-KUNNR.
        GT_LIST-KUNNR2_TXT = GT_LIST-KUNNR_TXT.
      ELSE.
        READ TABLE LT_0050 INTO DATA(LS_0050) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR
                                                       LIFNR     = GT_LIST-KUNNR3
                                                       VTWEG     = P_VTWEG BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-KUNNR2 = LS_0050-ZKUNNR_S.
          GT_LIST-KUNNR2_TXT = LS_0050-NAME_ORG1.
        ELSE.
          READ TABLE LT_0050 INTO DATA(LS_0050_2) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR
                                                           LIFNR     = SPACE
                                                           VTWEG     = P_VTWEG BINARY SEARCH.
          IF SY-SUBRC = 0.
            GT_LIST-KUNNR2     = LS_0050_2-ZKUNNR_S.
            GT_LIST-KUNNR2_TXT = LS_0050_2-NAME_ORG1.
          ELSE.
            PERFORM ERROR_FIELD USING TEXT-E10.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE LT_BP INTO DATA(LS_BP2) WITH KEY PARTNER = GT_LIST-KUNNR2 BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-KUNNR2_TXT = LS_BP2-NAME_ORG1.
      ELSE.
        PERFORM ERROR_FIELD USING TEXT-E04.
      ENDIF.
    ENDIF.

    READ TABLE LT_T001W INTO DATA(LS_T001W) WITH KEY WERKS = GT_LIST-WERKS BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-WERKS_TXT = LS_T001W-NAME1.
    ELSE.
      PERFORM ERROR_FIELD USING TEXT-E05.
    ENDIF.

    READ TABLE LT_MBEW INTO DATA(LS_MBEW) WITH KEY MATNR = GT_LIST-MATNR
                                                   BWKEY = GT_LIST-WERKS
                                                   BWTAR = GT_LIST-COUNTRY BINARY SEARCH.
    IF SY-SUBRC = 0.
    ELSE.
      CLEAR GT_LIST-COUNTRY.
    ENDIF.

    IF LS_MAKT-MTART = 'SET' OR LS_MAKT-MTART = 'SETV'.
    ELSE.
      READ TABLE LT_T001L INTO DATA(LS_T001L) WITH KEY LGORT = GT_LIST-LGORT BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-LGORT_TXT = LS_T001L-LGOBE.
      ELSE.
        PERFORM ERROR_FIELD USING TEXT-E06.
      ENDIF.
    ENDIF.

    IF GT_LIST-KBETR IS NOT INITIAL.
      GT_LIST-KBETR_CONV = GT_LIST-KBETR.
      PERFORM CONV_AMOUNT USING    GT_LIST-KBETR
                          CHANGING GT_LIST-KBETR.

    ENDIF.

    IF GT_LIST-ETD IS INITIAL.
      PERFORM ERROR_FIELD USING TEXT-E09.
    ENDIF.

*PRICE CHECK LOGIC
    READ TABLE LT_0040 INTO DATA(LS_0040D) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE LT_A903  WITH KEY KUNNR = GT_LIST-KUNNR
                                   KUNWE = GT_LIST-KUNNR2
                                   MATNR = GT_LIST-MATNR BINARY SEARCH
                                   TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        LOOP AT LT_A903 INTO DATA(LS_A903) FROM SY-TABIX.
          IF LS_A903-KUNNR NE GT_LIST-KUNNR  OR
             LS_A903-KUNWE NE GT_LIST-KUNNR2 OR
             LS_A903-MATNR NE GT_LIST-MATNR.
            EXIT.
          ENDIF.
          IF  GT_LIST-PRSDT >= LS_A903-DATAB AND GT_LIST-PRSDT <= LS_A903-DATBI.
            GT_LIST-KBETR_SD = LS_A903-KBETR.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ELSE.
      READ TABLE LT_A305 WITH KEY KUNNR = GT_LIST-KUNNR
                                  MATNR = GT_LIST-MATNR BINARY SEARCH
                                  TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        LOOP AT LT_A305 INTO DATA(LS_A305) FROM SY-TABIX.
          IF LS_A305-KUNNR NE GT_LIST-KUNNR OR
             LS_A305-MATNR NE GT_LIST-MATNR.
            EXIT.
          ENDIF.
          IF  GT_LIST-PRSDT >= LS_A305-DATAB AND GT_LIST-PRSDT <= LS_A305-DATBI.
            GT_LIST-KBETR_SD = LS_A305-KBETR.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF GT_LIST-ICON IS INITIAL.
      GT_LIST-ICON  = ICON_LIGHT_OUT.
      GT_LIST-KONWA = GV_WAERS.
    ENDIF.

    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  SORT GT_LIST BY BSTKD.
  SORT GT_SET  BY MATNR IDNRK.
  DELETE ADJACENT DUPLICATES FROM GT_SET COMPARING ALL FIELDS.

  _CLEAR LT_TEMP.
  LT_TEMP[] = GT_LIST[].
*--?????? CUST REF??? ????????? ?????? ????????????
  LOOP AT LT_TEMP WHERE ICON = ICON_RED_LIGHT.
    LOOP AT GT_LIST WHERE BSTKD = LT_TEMP-BSTKD
                      AND ICON = ICON_LIGHT_OUT.
      GT_LIST-ICON    = ICON_RED_LIGHT.
      GT_LIST-MESSAGE = TEXT-E07.
      MODIFY GT_LIST TRANSPORTING ICON MESSAGE.
    ENDLOOP.
  ENDLOOP.

  _CLEAR : LT_ITEMS.
  LOOP AT GT_LIST.
*-????????? ????????? ???????????????.
* PO/SKU/GS/??? ????????? ????????????????????? ?????? ???????????? ????????? ????????? ????????? ???????????????.
    MOVE-CORRESPONDING GT_LIST TO LT_ITEMS.
    COLLECT LT_ITEMS. CLEAR LT_ITEMS.
  ENDLOOP.
  SORT LT_ITEMS BY BSTKD.


*- ??????SKU??? ???????????? ??? PO??? ???????????? ????????? ????????? ????????? ?????????
*  (GS??? ??????????????? ????????? ??????????????? ????????????)
  LOOP AT LT_ITEMS.
    ON CHANGE OF LT_ITEMS-BSTKD.
      CLEAR : LV_POSNR.
    ENDON.

*-1. ???????????? ????????? ?????? ?????????
    ADD 10 TO LV_POSNR.
    LOOP AT GT_LIST WHERE BSTKD  = LT_ITEMS-BSTKD
                     AND  MATNR  = LT_ITEMS-MATNR
                     AND  KUNNR3 = LT_ITEMS-KUNNR3.
      GT_LIST-POSNR      = LV_POSNR.
      GT_LIST-KWMENG_SUM = LT_ITEMS-KWMENG.
      MODIFY GT_LIST TRANSPORTING POSNR KWMENG_SUM.
    ENDLOOP.
  ENDLOOP.

*-2. ???????????? SO????????? ?????? ?????????????????? ???????????????,
*    ????????? ????????? ????????? ??????(??????, Sales price??? ????????????)
*    ????????? ????????? ?????? ???????????? ??????
*    ?????? ????????? ??????????????? ??????????????? ???????????????.

  _CLEAR LT_TEMP.
  LT_TEMP[] = GT_LIST[].
  SORT LT_TEMP BY BSTKD MESSAGE.

  SORT GT_LIST BY BSTKD MATNR.
  LOOP AT GT_LIST.
    ON CHANGE OF GT_LIST-BSTKD.
      CLEAR : LV_CHANGE, LV_END.
    ENDON.
    READ TABLE LT_DUP  WITH KEY BSTKD = GT_LIST-BSTKD BINARY SEARCH TRANSPORTING NO FIELDS.
    CHECK SY-SUBRC = 0.
    CLEAR GT_LIST-POSNR.
    MODIFY GT_LIST TRANSPORTING POSNR.

    READ TABLE LT_DUP INTO DATA(LS_DUP) WITH KEY BSTKD = GT_LIST-BSTKD
                                                 MATNR = GT_LIST-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-VBELN = LS_DUP-VBELN.
      GT_LIST-POSNR = LS_DUP-POSNR.
      GT_LIST-KBETR_CURRENT = LS_DUP-NETPR.
      MODIFY GT_LIST TRANSPORTING VBELN POSNR KBETR_CURRENT.
*??????/??????/?????????????????? ?????????????????? ??????
**      ????????? ???????????? ????????? CUST.REF DUP ERROR
      IF GT_LIST-KWMENG_SUM NE LS_DUP-KWMENG.
        LV_CHANGE = 'X'.
        GT_LIST-QTY = 'X'.
        MODIFY GT_LIST TRANSPORTING QTY.
      ENDIF.
      IF GT_LIST-KBETR IS INITIAL.
        IF GT_LIST-KBETR_SD NE LS_DUP-NETPR.
          LV_CHANGE = 'X'.
          GT_LIST-PRICE = 'X'.
          MODIFY GT_LIST TRANSPORTING PRICE.
        ENDIF.
      ELSE.
        IF GT_LIST-KBETR NE LS_DUP-NETPR.
          LV_CHANGE = 'X'.
          GT_LIST-PRICE = 'X'.
          MODIFY GT_LIST TRANSPORTING PRICE.
        ENDIF.
      ENDIF.
*SHIP TO
      IF LS_DUP-KUNWE_I IS INITIAL.
        IF GT_LIST-KUNNR2 NE LS_DUP-KUNWE_H.
          LV_CHANGE = 'X'.
          GT_LIST-PARTNER = 'X'.
          MODIFY GT_LIST TRANSPORTING PARTNER.
        ENDIF.
      ELSE.
        IF GT_LIST-KUNNR2 NE LS_DUP-KUNWE_I.
          LV_CHANGE = 'X'.
          GT_LIST-PARTNER = 'X'.
          MODIFY GT_LIST TRANSPORTING PARTNER.
        ENDIF.
      ENDIF.
*GOODS SUPPLIER
      IF LS_DUP-LIFNR_I IS INITIAL.
        IF GT_LIST-KUNNR3 NE LS_DUP-LIFNR_H AND GT_LIST-GOODSUP NE LS_DUP-LIFNR_H.
          LV_CHANGE = 'X'.
          GT_LIST-PARTNER = 'X'.
          MODIFY GT_LIST TRANSPORTING PARTNER.
        ENDIF.
      ELSE.
        IF GT_LIST-KUNNR3 NE LS_DUP-LIFNR_I AND GT_LIST-GOODSUP NE LS_DUP-LIFNR_I.
          LV_CHANGE = 'X'.
          GT_LIST-PARTNER = 'X'.
          MODIFY GT_LIST TRANSPORTING PARTNER.
        ENDIF.
      ENDIF.

      AT END OF BSTKD.
        LV_END = 'X'.
      ENDAT.
      IF  LV_END = 'X'.
        IF LV_CHANGE IS INITIAL.
          GT_LIST-ICON = ICON_RED_LIGHT.
          GT_LIST-MESSAGE = TEXT-E01.
          MODIFY GT_LIST TRANSPORTING MESSAGE ICON WHERE BSTKD = GT_LIST-BSTKD.
        ELSE.
          READ TABLE LT_TEMP WITH KEY BSTKD = GT_LIST-BSTKD
                                    MESSAGE = TEXT-E12.
          IF SY-SUBRC NE 0.
            GT_LIST-ICON = ICON_LIGHT_OUT.
            MODIFY GT_LIST TRANSPORTING ICON WHERE BSTKD = GT_LIST-BSTKD.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    CLEAR GT_LIST.
  ENDLOOP.

  _CLEAR LT_TEMP.
  LT_TEMP[] = GT_LIST[].
  DELETE LT_TEMP WHERE POSNR IS NOT INITIAL.
  SORT LT_TEMP BY BSTKD MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING BSTKD MATNR.
  SORT LT_TEMP BY BSTKD MATNR.
  SORT LT_DUP BY VBELN ASCENDING
                 POSNR DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_DUP COMPARING VBELN.
  SORT LT_DUP  BY BSTKD.

  CLEAR LV_POSNR.
  LOOP AT LT_TEMP.
    ON CHANGE OF LT_TEMP-BSTKD.
      CLEAR : LV_POSNR.
    ENDON.
    IF LV_POSNR IS INITIAL.
      READ TABLE LT_DUP INTO DATA(LS_DUP2) WITH KEY BSTKD = LT_TEMP-BSTKD BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_POSNR = LS_DUP2-POSNR+0(5) && 0.
        LV_POSNR = LV_POSNR + 10.
*        LV_POSNR = LS_DUP2-POSNR + 10.
      ENDIF.
    ELSE.
      LV_POSNR = LV_POSNR+0(5) + 10.
    ENDIF.

    READ TABLE GT_LIST WITH KEY BSTKD = LT_TEMP-BSTKD
                                ICON = ICON_RED_LIGHT.
    IF SY-SUBRC = 0 AND GT_LIST-MESSAGE NE TEXT-E01.
      GT_LIST-ICON = ICON_RED_LIGHT.
    ELSE.
      GT_LIST-ICON = ICON_LIGHT_OUT.
      CLEAR GT_LIST-MESSAGE.
    ENDIF.

    GT_LIST-VBELN    = LS_DUP2-VBELN.
    GT_LIST-POSNR    = LV_POSNR.
    GT_LIST-NEW_ITEM = 'X'.
    MODIFY GT_LIST TRANSPORTING VBELN POSNR ICON  NEW_ITEM WHERE BSTKD = LT_TEMP-BSTKD
                                                             AND MATNR = LT_TEMP-MATNR.

    MODIFY GT_LIST TRANSPORTING ICON MESSAGE WHERE BSTKD = LT_TEMP-BSTKD.

  ENDLOOP.

  SORT GT_DUP BY BSTKD MATNR.
  LOOP AT GT_LIST WHERE PARTNER = 'X' AND ICON <> ICON_RED_LIGHT.
    CLEAR GT_DUP.
    READ TABLE GT_DUP WITH KEY BSTKD = GT_LIST-BSTKD
                               MATNR = GT_LIST-MATNR.
    IF SY-SUBRC = 0.
      IF GT_DUP-EBELN IS NOT INITIAL.
        PERFORM ERROR_FIELD USING TEXT-E15.
        MODIFY GT_LIST TRANSPORTING ICON MESSAGE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT GT_LIST WHERE QTY = 'X' AND ICON <> ICON_RED_LIGHT.
    CLEAR GT_DUP.
    READ TABLE GT_DUP WITH KEY BSTKD = GT_LIST-BSTKD
                               MATNR = GT_LIST-MATNR.
    IF SY-SUBRC = 0.
      IF GT_DUP-IF_VBELN IS NOT INITIAL.
        PERFORM ERROR_FIELD USING TEXT-E16.
        MODIFY GT_LIST TRANSPORTING ICON MESSAGE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT GT_LIST WHERE ICON <> ICON_RED_LIGHT.
    IF P_OPENPO = ' ' AND GT_LIST-KBETR IS NOT INITIAL.
* UPLOAD PRICE??? ?????????????????? ????????? ???????????? ??????

      IF GT_LIST-KBETR_CURRENT IS INITIAL.
        IF GT_LIST-KBETR NE GT_LIST-KBETR_SD.
          PERFORM ERROR_FIELD USING TEXT-E12.
          GT_LIST-PRICE = 'X'.
        ENDIF.
      ELSE.
        IF GT_LIST-KBETR_CURRENT NE GT_LIST-KBETR.
          IF GT_LIST-KBETR NE GT_LIST-KBETR_SD.
            PERFORM ERROR_FIELD USING TEXT-E12.
            GT_LIST-PRICE = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*?????????????????? ????????? ???????????? ??????
    IF GT_LIST-KBETR_SD IS INITIAL.
      GT_LIST-MESSAGE = TEXT-E19.
      GT_LIST-ICON = ICON_RED_LIGHT.
    ENDIF.

*DELIVERY??? ?????????
*????????? ???????????? ??????
*????????? ??????????????????
    CLEAR GT_DUP.
    READ TABLE GT_DUP WITH KEY BSTKD = GT_LIST-BSTKD
                               MATNR = GT_LIST-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF GT_DUP-VBELN_VL IS NOT INITIAL.
        IF    GT_LIST-NEW_ITEM EQ 'X' OR
              GT_LIST-PRICE    EQ 'X' OR
              GT_LIST-PARTNER  EQ 'X' OR
              GT_LIST-QTY      EQ 'X'.
          PERFORM ERROR_FIELD USING TEXT-E14.
        ELSE.
          GT_LIST-MESSAGE = TEXT-E14.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY GT_LIST TRANSPORTING ICON MESSAGE PRICE.
  ENDLOOP.

  _CLEAR LT_TEMP.
  LT_TEMP[] = GT_LIST[].
*--?????? CUST REF??? ????????? ?????? ????????????
  LOOP AT LT_TEMP WHERE ICON = ICON_RED_LIGHT.
    LOOP AT GT_LIST WHERE BSTKD = LT_TEMP-BSTKD
                      AND ICON = ICON_LIGHT_OUT.
      GT_LIST-ICON    = ICON_RED_LIGHT.
      GT_LIST-MESSAGE = TEXT-E07.
      MODIFY GT_LIST TRANSPORTING ICON MESSAGE.
    ENDLOOP.
  ENDLOOP.

  SORT GT_LIST BY BSTKD POSNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_SALES_ORDER
*&---------------------------------------------------------------------*
FORM CREATE_SALES_ORDER .

  DATA : LT_BSTKD   LIKE TABLE OF GT_BSTKD WITH HEADER LINE,
         LT_BSTKD_M LIKE TABLE OF GT_BSTKD WITH HEADER LINE,
         LV_CHECK,
         LV_ITEM    LIKE VBAP-POSNR.

  CLEAR: GT_ROWS[], GS_ROWS.
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.

    _CLEAR: LT_BSTKD, GT_0120.
    CLEAR : GV_SUCCESS, GV_FAILURE.

    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
      CHECK GT_LIST-ICON EQ ICON_LIGHT_OUT.
      LT_BSTKD-BSTKD = GT_LIST-BSTKD.
      COLLECT LT_BSTKD. CLEAR LT_BSTKD.
    ENDLOOP.

    IF LT_BSTKD[] IS INITIAL.
      MESSAGE S019 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    PERFORM POPUP_MSG USING TEXT-P01
                            TEXT-P02
                            LV_CHECK.

    CHECK LV_CHECK = '1'.

*?????? ?????? ????????? ?????? ????????? ??????
    LOOP AT LT_BSTKD.
      READ TABLE GT_LIST WITH KEY BSTKD    = LT_BSTKD-BSTKD
                                  VBELN    = SPACE TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
        LT_BSTKD_M-BSTKD = LT_BSTKD-BSTKD.
        COLLECT LT_BSTKD_M. CLEAR LT_BSTKD_M.
        DELETE LT_BSTKD.
      ENDIF.
    ENDLOOP.

    _CLEAR GT_0150.
    CLEAR :GV_TIME, GV_DATE.
    GV_TIME = SY-UZEIT.
    GV_DATE = SY-DATUM.

*?????????
    LOOP AT LT_BSTKD.
      PERFORM CLEAR_PARAMETERS.

      LOOP AT GT_LIST WHERE BSTKD EQ LT_BSTKD-BSTKD
                        AND ICON  NE ICON_RED_LIGHT.
        EXIT.
      ENDLOOP.
      PERFORM HEADER_DATA.
      PERFORM PARTNER_DATA.
      CLEAR LV_ITEM.
      LOOP AT GT_LIST WHERE BSTKD = LT_BSTKD-BSTKD.
        CHECK GT_LIST-ICON NE ICON_RED_LIGHT.
        IF LV_ITEM IS INITIAL.
          LV_ITEM = GT_LIST-POSNR.
          PERFORM ITEM_DATA.
          IF GT_LIST-LGORT IS INITIAL. "SET OR SETV
            PERFORM COMPONENT_ITEM_DATA.
          ENDIF.
        ELSE.
          IF LV_ITEM = GT_LIST-POSNR.
          ELSE.
            PERFORM ITEM_DATA.
            IF GT_LIST-LGORT IS INITIAL. "SET OR SETV
              PERFORM COMPONENT_ITEM_DATA.
            ENDIF.
            LV_ITEM = GT_LIST-POSNR.
          ENDIF.
        ENDIF.
      ENDLOOP.
      PERFORM CALL_BAPI USING LT_BSTKD-BSTKD.
    ENDLOOP.

    SORT GT_DUP BY BSTKD MATNR NETPR.

*?????????
    LOOP AT LT_BSTKD_M.
      PERFORM CLEAR_PARAMETERS.
      LOOP AT GT_LIST WHERE BSTKD EQ LT_BSTKD_M-BSTKD.
        EXIT.
      ENDLOOP.
      GV_SALESDOCUMENT = GT_LIST-VBELN.
      GS_HEADER_INX-UPDATEFLAG = 'U'.
      CLEAR LV_ITEM.
      LOOP AT GT_LIST WHERE BSTKD = LT_BSTKD_M-BSTKD.

        CHECK GT_LIST-ICON NE ICON_RED_LIGHT.
*?????????????????? ??????
        CHECK GT_LIST-NEW_ITEM EQ 'X' OR
              GT_LIST-PRICE    EQ 'X' OR
              GT_LIST-PARTNER  EQ 'X' OR
              GT_LIST-QTY      EQ 'X'.

        IF LV_ITEM IS INITIAL.
          LV_ITEM = GT_LIST-POSNR.
          PERFORM ITEM_DATA_UPDATE.
          IF GT_LIST-LGORT IS INITIAL. "SET OR SETV
            PERFORM COMPONENT_ITEM_UPDATE.
          ENDIF.
        ELSE.
          IF LV_ITEM = GT_LIST-POSNR.
          ELSE.
            PERFORM ITEM_DATA_UPDATE.
            IF GT_LIST-LGORT IS INITIAL. "SET OR SETV
              PERFORM COMPONENT_ITEM_UPDATE.
            ENDIF.
            LV_ITEM = GT_LIST-POSNR.
          ENDIF.
        ENDIF.
      ENDLOOP.
      PERFORM CALL_BAPI_CHANGE USING LT_BSTKD_M-BSTKD.
    ENDLOOP.

    IF GT_0120[] IS NOT INITIAL.
      MODIFY ZSDT0120 FROM TABLE GT_0120.
    ENDIF.

    IF GT_0150[] IS NOT INITIAL.
      MODIFY ZSDT0150 FROM TABLE GT_0150.
    ENDIF.

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
         GT_CHANGE_PARTNERS,      GT_CHANGE_PARTNERS[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HEADER_DATA
*&---------------------------------------------------------------------*
FORM HEADER_DATA .

  DATA : LT_BSTKD(20) OCCURS 0 WITH HEADER LINE.

  GS_ORDER_HEADER_IN-DOC_TYPE   = P_AUART.
  GS_ORDER_HEADER_INX-DOC_TYPE  = 'X'.

  GS_ORDER_HEADER_IN-SALES_ORG   = P_VKORG.     "?????? ??????
  GS_ORDER_HEADER_INX-SALES_ORG  = 'X'.

  GS_ORDER_HEADER_IN-DISTR_CHAN  = P_VTWEG. "?????? ??????.
  GS_ORDER_HEADER_INX-DISTR_CHAN = 'X'.

  GS_ORDER_HEADER_IN-DIVISION    = P_SPART.       "Division
  GS_ORDER_HEADER_INX-DIVISION   = 'X'.

  GS_ORDER_HEADER_IN-REQ_DATE_H  = GT_LIST-BSTDK."?????? ?????????
  GS_ORDER_HEADER_INX-REQ_DATE_H = 'X'.

  GS_ORDER_HEADER_IN-PURCH_DATE  = GT_LIST-PRSDT. "CUST REF DATE date
  GS_ORDER_HEADER_INX-PURCH_DATE = 'X'.

  GS_ORDER_HEADER_IN-PRICE_DATE  = GT_LIST-PRSDT."?????? ?????????
  GS_ORDER_HEADER_INX-PRICE_DATE = 'X'.

  GS_ORDER_HEADER_IN-PURCH_NO_C  = GT_LIST-BSTKD. "??????????????????.
  GS_ORDER_HEADER_INX-PURCH_NO_C = 'X'.

  IF P_OPENPO = 'X'.
    GS_ORDER_HEADER_IN-REF_1   = 'OPEN_PO'.
    GS_ORDER_HEADER_INX-REF_1 = 'X'.

    FIND '^' IN GT_LIST-BSTKD IN CHARACTER MODE.
    IF  SY-SUBRC = 0.
      _CLEAR LT_BSTKD.
      SPLIT GT_LIST-BSTKD  AT '^'  INTO TABLE LT_BSTKD.
      READ TABLE LT_BSTKD INDEX 1.
      GS_ORDER_HEADER_IN-PURCH_NO_S   = LT_BSTKD.
      GS_ORDER_HEADER_INX-PURCH_NO_S = 'X'.
    ENDIF.
  ENDIF.

*& TEXT
  GT_TEXT-TEXT_ID   = '0002'.
  GT_TEXT-LANGU     = SY-LANGU.
  GT_TEXT-TEXT_LINE     = 'ETD :???' &&  GT_LIST-ETD.
  APPEND GT_TEXT.  CLEAR GT_TEXT.

  IF GT_LIST-ETA IS NOT INITIAL.
    GT_TEXT-FORMAT_COL   = '/'.
    GT_TEXT-TEXT_ID   = '0002'.
    GT_TEXT-LANGU     = SY-LANGU.
    GT_TEXT-TEXT_LINE = 'ETA :???' && GT_LIST-ETA.
    APPEND GT_TEXT.  CLEAR GT_TEXT.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form PARTNER_DATA
*&---------------------------------------------------------------------*
FORM PARTNER_DATA .

  GT_ORDER_PARTNERS-PARTN_ROLE   = 'AG'.     "????????? (Sold-to party)
  GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR.
  APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.

  GT_ORDER_PARTNERS-PARTN_ROLE   = 'WE'.     "????????? (Ship-to party)
  GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR2.
  APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.

  GT_ORDER_PARTNERS-PARTN_ROLE   = 'WL'.     "Goods supplier
  IF GT_LIST-GOODSUP IS INITIAL.
    GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR3.
  ELSE.
    GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-GOODSUP.
  ENDIF.
  APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ITEM_DATA
*&---------------------------------------------------------------------*
FORM ITEM_DATA .

  GT_ORDER_ITEMS_IN-ITM_NUMBER      = GT_LIST-POSNR.  "??????
  GT_ORDER_ITEMS_INX-ITM_NUMBER     = GT_LIST-POSNR.

  GT_ORDER_ITEMS_IN-MATERIAL_LONG   = GT_LIST-MATNR.  "????????????
  GT_ORDER_ITEMS_INX-MATERIAL_LONG  = 'X'.

  GT_ORDER_ITEMS_IN-PLANT           = GT_LIST-WERKS.  "?????????.
  GT_ORDER_ITEMS_INX-PLANT          = 'X'.

  GT_ORDER_ITEMS_IN-STORE_LOC       = GT_LIST-LGORT.  "????????????.
  GT_ORDER_ITEMS_INX-STORE_LOC      = 'X'.

  GT_ORDER_ITEMS_IN-PURCH_NO_C      = GT_LIST-BSTKD.  "Cust. Reference.
  GT_ORDER_ITEMS_INX-PURCH_NO_C     = 'X'.

  IF GT_LIST-POSEX IS INITIAL.
    GT_ORDER_ITEMS_IN-PO_ITM_NO      = GT_LIST-POSNR.    "Cust. Reference. ITEM
  ELSE.
    GT_ORDER_ITEMS_IN-PO_ITM_NO      = GT_LIST-POSEX.   "Cust. Reference. ITEM
  ENDIF.
  PERFORM ALPHA_INPUT USING GT_ORDER_ITEMS_IN-PO_ITM_NO.
  GT_ORDER_ITEMS_INX-PO_ITM_NO     = 'X'.

  GT_ORDER_ITEMS_IN-PURCH_DATE      = GT_LIST-BSTDK.  "Cust. Reference. DATE
  GT_ORDER_ITEMS_INX-PURCH_DATE     = 'X'.

  APPEND GT_ORDER_ITEMS_IN.      CLEAR GT_ORDER_ITEMS_IN.
  APPEND GT_ORDER_ITEMS_INX.     CLEAR GT_ORDER_ITEMS_INX.

  GT_ORDER_SCHEDULES_IN-ITM_NUMBER  = GT_LIST-POSNR.
  GT_ORDER_SCHEDULES_INX-ITM_NUMBER = GT_LIST-POSNR.

  GT_ORDER_SCHEDULES_IN-REQ_DATE    = GT_LIST-BSTDK. "?????? ????????? .
  GT_ORDER_SCHEDULES_INX-REQ_DATE   = 'X'.

  GT_ORDER_SCHEDULES_IN-REQ_QTY     = GT_LIST-KWMENG_SUM.  "?????? .
  GT_ORDER_SCHEDULES_INX-REQ_QTY    = 'X'.

  APPEND GT_ORDER_SCHEDULES_IN.  CLEAR GT_ORDER_SCHEDULES_IN.
  APPEND GT_ORDER_SCHEDULES_INX. CLEAR GT_ORDER_SCHEDULES_INX.

  GT_ORDER_CONDITIONS_IN-ITM_NUMBER  = GT_LIST-POSNR.  "??????????????????
  GT_ORDER_CONDITIONS_INX-ITM_NUMBER = 'X'.       "??????????????????

  IF P_VKORG = '1002'.
    GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR01'.
  ELSE.
    GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR00'.
  ENDIF.

  GT_ORDER_CONDITIONS_INX-COND_TYPE  = 'X'.       "????????????
  IF GT_LIST-KBETR_CONV IS NOT INITIAL.
    GT_ORDER_CONDITIONS_IN-COND_VALUE  = GT_LIST-KBETR_CONV. "?????? ??????.
    GT_ORDER_CONDITIONS_IN-CURRENCY    = GV_WAERS. "?????? ??????.

    GT_ORDER_CONDITIONS_INX-COND_VALUE = 'X'.       "?????? ??????
    GT_ORDER_CONDITIONS_INX-CURRENCY   = 'X'.       "?????? ??????.
  ENDIF.
  APPEND GT_ORDER_CONDITIONS_IN.  CLEAR GT_ORDER_CONDITIONS_IN.
  APPEND GT_ORDER_CONDITIONS_INX. CLEAR GT_ORDER_CONDITIONS_INX.


  GT_ORDER_PARTNERS-ITM_NUMBER   = GT_LIST-POSNR.  "Goods supplier
  GT_ORDER_PARTNERS-PARTN_ROLE   = 'ZS'.           "Goods supplier
  IF GT_LIST-GOODSUP IS INITIAL.
    GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR3.
  ELSE.
    GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-GOODSUP.
  ENDIF.
  APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.


  GT_ORDER_PARTNERS-ITM_NUMBER   = GT_LIST-POSNR.  "SHIP TO
  GT_ORDER_PARTNERS-PARTN_ROLE   = 'WE'.           "SHIP TO
  GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR2.

  APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_BAPI
*&---------------------------------------------------------------------*
FORM CALL_BAPI USING PV_BSTKD.

  CLEAR GV_SALESDOCUMENT.
  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
    EXPORTING
      ORDER_HEADER_IN      = GS_ORDER_HEADER_IN
      ORDER_HEADER_INX     = GS_ORDER_HEADER_INX
    IMPORTING
      SALESDOCUMENT        = GV_SALESDOCUMENT
    TABLES
      RETURN               = GT_RETURN
      ORDER_TEXT           = GT_TEXT
      ORDER_ITEMS_IN       = GT_ORDER_ITEMS_IN
      ORDER_ITEMS_INX      = GT_ORDER_ITEMS_INX
      ORDER_SCHEDULES_IN   = GT_ORDER_SCHEDULES_IN
      ORDER_SCHEDULES_INX  = GT_ORDER_SCHEDULES_INX
      ORDER_PARTNERS       = GT_ORDER_PARTNERS
      ORDER_CONDITIONS_IN  = GT_ORDER_CONDITIONS_IN
      ORDER_CONDITIONS_INX = GT_ORDER_CONDITIONS_INX
      EXTENSIONIN          = GT_EXTENSION.

  IF GV_SALESDOCUMENT IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    GT_LIST-MESSAGE = TEXT-S03 && '(' && GV_SALESDOCUMENT && ')'.
    GT_LIST-VBELN   =  GV_SALESDOCUMENT.
    GT_LIST-ICON    = ICON_GREEN_LIGHT.
    MODIFY GT_LIST TRANSPORTING VBELN MESSAGE ICON WHERE BSTKD = PV_BSTKD AND ICON NE ICON_RED_LIGHT.
    ADD 1 TO GV_SUCCESS.

*ASIN CODE ????????????, ???????????? ???????????? ??????
    CLEAR GT_LIST.
    READ TABLE GT_LIST WITH KEY BSTKD = PV_BSTKD.
    READ TABLE GT_0060 WITH KEY ZKUNNR_IC = GT_LIST-KUNNR
                                VTWEG     = GT_LIST-VTWEG BINARY SEARCH.
    IF SY-SUBRC = 0.
      LOOP AT GT_LIST WHERE BSTKD = PV_BSTKD
                        AND ZASIN IS NOT INITIAL.
        GT_0120-KUNNR = GT_0060-KUNNR.
        GT_0120-MATNR = GT_LIST-MATNR.
        GT_0120-ZASIN = GT_LIST-ZASIN.
        COLLECT GT_0120. CLEAR GT_0120.
      ENDLOOP.
    ENDIF.

*Save log.
*    LOOP AT GT_LIST WHERE BSTKD = PV_BSTKD.
*      MOVE-CORRESPONDING GT_LIST TO GT_0150.
*      GT_0150-ZUPLOAD_DATE = GV_DATE.
*      GT_0150-ZUPLOAD_TIME = GV_TIME.
*      GT_0150-VRKME        = GT_LIST-MEINS.
*      GT_0150-KWMENG       = GT_LIST-KWMENG_SUM.
*      IF GT_LIST-KBETR IS INITIAL.
*        GT_0150-KBETR       = GT_LIST-KBETR_SD.
*      ELSE.
*        GT_0150-KBETR       = GT_LIST-KBETR.
*      ENDIF.
*      GT_0150-LIFNR        = GT_LIST-KUNNR3.
*      GT_0150-KUNWE        = GT_LIST-KUNNR2.
*      GT_0150-ERNAM        = SY-UNAME.
*      GT_0150-ERDAT        = SY-DATUM.
*      GT_0150-ERZET        = SY-UZEIT.
*      APPEND GT_0150. CLEAR GT_0150.
*    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CLEAR GT_RETURN.
    READ TABLE GT_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      GT_LIST-MESSAGE = GT_RETURN-MESSAGE.
      GT_LIST-ICON = ICON_RED_LIGHT.
      CLEAR GT_LIST-POSNR.
      MODIFY GT_LIST TRANSPORTING MESSAGE ICON POSNR WHERE BSTKD = PV_BSTKD.
    ENDIF.
    ADD 1 TO GV_FAILURE.
  ENDIF.

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
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HANDLE_TOP_OF_PAGE  USING PE_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT.

  DATA : LV_TEXT(255) TYPE C.
  IF P_UPLOAD = 'X'.
    CLEAR : LV_TEXT.
    SELECT SINGLE BEZEI INTO LV_TEXT
    FROM TVAKT
    WHERE AUART = P_AUART
      AND SPRAS = SY-LANGU.
    CONCATENATE TEXT-F04 P_AUART '('  LV_TEXT  ')' "Order type
    INTO LV_TEXT SEPARATED BY SPACE.
    CALL METHOD PE_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = LV_TEXT
        SAP_FONTSIZE = 'MEDIUM'.

    CALL METHOD PE_DYNDOC_ID->NEW_LINE.

    CLEAR : LV_TEXT.
    SELECT SINGLE VTEXT INTO LV_TEXT
    FROM TVKOT
    WHERE VKORG = P_VKORG
    AND SPRAS = SY-LANGU.
    CONCATENATE TEXT-F05 ' :' P_VKORG '(' LV_TEXT ')' "Sales Org.
    INTO LV_TEXT SEPARATED BY SPACE.
    CALL METHOD PE_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = LV_TEXT
        SAP_FONTSIZE = 'MEDIUM'.

    CALL METHOD PE_DYNDOC_ID->NEW_LINE.

    CLEAR : LV_TEXT.
    DESCRIBE TABLE GT_LIST LINES DATA(LV_ROWS).
    LV_TEXT = LV_ROWS.
    CONDENSE LV_TEXT. "uploaded data
    CONCATENATE TEXT-002 ' :' LV_TEXT INTO LV_TEXT SEPARATED BY SPACE.
    CALL METHOD PE_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = LV_TEXT
        SAP_FONTSIZE = 'MEDIUM'.

    CALL METHOD PE_DYNDOC_ID->NEW_LINE.


    IF GV_SUCCESS IS NOT INITIAL.
      CLEAR : LV_TEXT.
      LV_TEXT = TEXT-004 && ' :' && GV_SUCCESS.

      CALL METHOD PE_DYNDOC_ID->ADD_TEXT
        EXPORTING
          TEXT         = LV_TEXT
          SAP_EMPHASIS = CL_DD_AREA=>HEADING
          SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

    ENDIF.

    IF GV_FAILURE IS NOT INITIAL.
      CLEAR : LV_TEXT.
      LV_TEXT = TEXT-005 && ' :' && GV_FAILURE.

      CALL METHOD PE_DYNDOC_ID->ADD_TEXT
        EXPORTING
          TEXT         = LV_TEXT
          SAP_EMPHASIS = CL_DD_AREA=>HEADING
          SAP_COLOR    = CL_DD_AREA=>LIST_NEGATIVE_INT.


      CALL METHOD PE_DYNDOC_ID->NEW_LINE.
    ENDIF.


  ELSE.
    CLEAR : LV_TEXT.
    LV_TEXT = 'Display sales order change history'.
    CALL METHOD PE_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = LV_TEXT
        SAP_FONTSIZE = 'MEDIUM'.

    CALL METHOD PE_DYNDOC_ID->NEW_LINE.

    CLEAR : LV_TEXT.
    SELECT SINGLE VTEXT INTO LV_TEXT
    FROM TVKOT
    WHERE VKORG = S_VKORG-LOW
      AND SPRAS = SY-LANGU.
    CONCATENATE TEXT-F05 S_VKORG-LOW '('  LV_TEXT  ')' "Order type
    INTO LV_TEXT SEPARATED BY SPACE.
    CALL METHOD PE_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = LV_TEXT
        SAP_FONTSIZE = 'MEDIUM'.

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
  MOVE : TEXT-006     TO L_DYNTXT-TEXT,
         ICON_XLS     TO L_DYNTXT-ICON_ID,
         TEXT-006     TO L_DYNTXT-ICON_TEXT,
         TEXT-006     TO L_DYNTXT-QUICKINFO,
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
   WHERE OBJID EQ 'ZSDB0010'.

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
*&---------------------------------------------------------------------*
*& Form AUTHORIZATION_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORIZATION_CHECK.

  DATA : LS_RETURN LIKE BAPIRETURN1,
         LV_VKORG  TYPE VKORG.

  CLEAR LV_VKORG.

  IF P_UPLOAD = 'X'.
    LV_VKORG = P_VKORG.
  ELSE.
    LV_VKORG = S_VKORG-LOW.
  ENDIF.

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
*& Form APPROVE_PRICE
*&---------------------------------------------------------------------*
FORM APPROVE_PRICE .

  DATA : LT_BSTKD LIKE TABLE OF GT_BSTKD WITH HEADER LINE,
         LV_CHECK,
         LV_ITEM  LIKE VBAP-POSNR,
         LT_TEMP  LIKE TABLE OF GT_LIST WITH HEADER LINE.

  CLEAR: GT_ROWS[], GS_ROWS.
  CALL METHOD GO_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ELSE.

    _CLEAR: LT_BSTKD.

    LOOP AT GT_ROWS INTO GS_ROWS.
      READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
      CHECK GT_LIST-ICON EQ ICON_RED_LIGHT AND GT_LIST-MESSAGE EQ TEXT-E12.
      LT_BSTKD-BSTKD = GT_LIST-BSTKD.
      COLLECT LT_BSTKD. CLEAR LT_BSTKD.
      GT_LIST-APPROVE = 'X'.
      MODIFY GT_LIST INDEX GS_ROWS-INDEX TRANSPORTING APPROVE.
    ENDLOOP.

    IF LT_BSTKD[] IS INITIAL.
      MESSAGE S000 WITH TEXT-E13 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    PERFORM POPUP_MSG USING TEXT-P03
                            TEXT-P04
                            LV_CHECK.

    CHECK LV_CHECK = '1'.

    LOOP AT LT_BSTKD.
      GT_LIST-ICON = ICON_LIGHT_OUT.
      CLEAR GT_LIST-MESSAGE.
      MODIFY GT_LIST TRANSPORTING ICON MESSAGE WHERE BSTKD = LT_BSTKD-BSTKD
                                                 AND MESSAGE = TEXT-E07.
      MODIFY GT_LIST TRANSPORTING ICON MESSAGE WHERE BSTKD = LT_BSTKD-BSTKD
                                                 AND APPROVE = 'X'.
    ENDLOOP.


    _CLEAR LT_TEMP.
    LT_TEMP[] = GT_LIST[].
*--?????? CUST REF??? ????????? ?????? ????????????
    LOOP AT LT_TEMP WHERE ICON = ICON_RED_LIGHT.
      LOOP AT GT_LIST WHERE BSTKD = LT_TEMP-BSTKD
                        AND ICON = ICON_LIGHT_OUT.
        GT_LIST-ICON    = ICON_RED_LIGHT.
        GT_LIST-MESSAGE = TEXT-E07.
        MODIFY GT_LIST TRANSPORTING ICON MESSAGE.
      ENDLOOP.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CELLCOLOR
*&---------------------------------------------------------------------*
FORM CELLCOLOR .

  LOOP AT GT_LIST WHERE NEW_ITEM = 'X'.

    _SET_ALV_COLOR   3 1 0.
*>> ALV
    INSERT LINES OF GT_COLOR INTO TABLE GT_LIST-CELLCOLOR.

    MODIFY GT_LIST TRANSPORTING CELLCOLOR.
    CLEAR : GT_LIST.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ITEM_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM ITEM_DATA_UPDATE.

  DATA LV_LFAG.

  GT_ORDER_ITEMS_IN-ITM_NUMBER  = GT_LIST-POSNR.
  GT_ORDER_ITEMS_INX-ITM_NUMBER = GT_LIST-POSNR.


  CLEAR LV_LFAG.
  IF GT_LIST-NEW_ITEM = 'X'.
    LV_LFAG = 'I'.

    GT_ORDER_ITEMS_IN-PLANT       = GT_LIST-WERKS.
    GT_ORDER_ITEMS_INX-PLANT      = 'X'.
    GT_ORDER_ITEMS_IN-STORE_LOC   = GT_LIST-LGORT.
    GT_ORDER_ITEMS_INX-STORE_LOC  = 'X'.

    GT_ORDER_ITEMS_IN-PURCH_NO_C  = GT_LIST-BSTKD.
    GT_ORDER_ITEMS_INX-PURCH_NO_C = 'X'.

    IF GT_LIST-POSEX IS INITIAL.
      GT_ORDER_ITEMS_IN-PO_ITM_NO = GT_LIST-POSNR.    "Cust. Reference. ITEM
    ELSE.
      GT_ORDER_ITEMS_IN-PO_ITM_NO = GT_LIST-POSEX.   "Cust. Reference. ITEM
    ENDIF.

    PERFORM ALPHA_INPUT USING GT_ORDER_ITEMS_IN-PO_ITM_NO.
    GT_ORDER_ITEMS_INX-PO_ITM_NO   = 'X'.

    GT_ORDER_ITEMS_IN-PURCH_DATE   = GT_LIST-BSTDK .
    GT_ORDER_ITEMS_INX-PURCH_DATE  = 'X'.

  ELSE.
    LV_LFAG = 'U'.
  ENDIF.

  GT_ORDER_ITEMS_INX-UPDATEFLAG    = LV_LFAG.

  GT_ORDER_ITEMS_IN-MATERIAL    = GT_LIST-MATNR.
  GT_ORDER_ITEMS_INX-MATERIAL   = 'X'.

  GT_ORDER_ITEMS_IN-TARGET_QTY   = GT_LIST-KWMENG_SUM.
  GT_ORDER_ITEMS_INX-TARGET_QTY  = 'X'.

  GT_ORDER_ITEMS_IN-TARGET_QU   = 'EA'.
  GT_ORDER_ITEMS_INX-TARGET_QU  = 'X'.


  APPEND GT_ORDER_ITEMS_IN.      CLEAR GT_ORDER_ITEMS_IN.
  APPEND GT_ORDER_ITEMS_INX.     CLEAR GT_ORDER_ITEMS_INX.

  GT_ORDER_SCHEDULES_IN-ITM_NUMBER  = GT_LIST-POSNR.
  GT_ORDER_SCHEDULES_INX-ITM_NUMBER = GT_LIST-POSNR.

  GT_ORDER_SCHEDULES_IN-SCHED_LINE  = 1.
  GT_ORDER_SCHEDULES_INX-SCHED_LINE = 1.

  GT_ORDER_SCHEDULES_INX-UPDATEFLAG = LV_LFAG.

  IF LV_LFAG = 'I'.
    GT_ORDER_SCHEDULES_IN-REQ_DATE    = GT_LIST-BSTDK. "?????? ????????? .
    GT_ORDER_SCHEDULES_INX-REQ_DATE   = 'X'.
  ENDIF.

  GT_ORDER_SCHEDULES_IN-REQ_QTY     = GT_LIST-KWMENG_SUM.  "?????? .
  GT_ORDER_SCHEDULES_INX-REQ_QTY    = 'X'.

  APPEND GT_ORDER_SCHEDULES_IN.  CLEAR GT_ORDER_SCHEDULES_IN.
  APPEND GT_ORDER_SCHEDULES_INX. CLEAR GT_ORDER_SCHEDULES_INX.

  GT_ORDER_CONDITIONS_IN-ITM_NUMBER  = GT_LIST-POSNR.  "??????????????????
  GT_ORDER_CONDITIONS_INX-ITM_NUMBER = GT_LIST-POSNR.  "??????????????????

*??????????????? ????????? ??????
  IF GT_LIST-PRICE = 'X' OR  GT_LIST-PARTNER = 'X'.
    GT_ORDER_CONDITIONS_INX-UPDATEFLAG = LV_LFAG.

    IF P_VKORG = '1002'.
      GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR01'.
    ELSE.
      GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR00'.
    ENDIF.
    GT_ORDER_CONDITIONS_INX-COND_TYPE  = 'X'.       "????????????

    IF GT_LIST-KBETR_CONV IS INITIAL.
      GT_ORDER_CONDITIONS_IN-COND_VALUE  = GT_LIST-KBETR_SD. "?????? ??????.
    ELSE.
      GT_ORDER_CONDITIONS_IN-COND_VALUE  = GT_LIST-KBETR_CONV. "?????? ??????.
    ENDIF.
    GT_ORDER_CONDITIONS_INX-COND_VALUE = 'X'.       "?????? ??????

    GT_ORDER_CONDITIONS_IN-CURRENCY    = GV_WAERS. "?????? ??????.
    GT_ORDER_CONDITIONS_INX-CURRENCY   = 'X'.       "?????? ??????.

    APPEND GT_ORDER_CONDITIONS_IN.  CLEAR GT_ORDER_CONDITIONS_IN.
    APPEND GT_ORDER_CONDITIONS_INX. CLEAR GT_ORDER_CONDITIONS_INX.
  ENDIF.
*PARNTER
  IF LV_LFAG = 'I'.
    GT_ORDER_PARTNERS-ITM_NUMBER   = GT_LIST-POSNR.  "Goods supplier
    GT_ORDER_PARTNERS-PARTN_ROLE   = 'ZS'.           "Goods supplier
    IF GT_LIST-GOODSUP IS INITIAL.
      GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR3.
    ELSE.
      GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-GOODSUP.
    ENDIF.
    APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.

    GT_ORDER_PARTNERS-ITM_NUMBER   = GT_LIST-POSNR.  "SHIP TO
    GT_ORDER_PARTNERS-PARTN_ROLE   = 'WE'.           "SHIP TO
    GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR2.

    APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.
  ELSE.
    IF GT_LIST-PARTNER = 'X'.
      GT_CHANGE_PARTNERS-DOCUMENT   = GT_LIST-VBELN.
      GT_CHANGE_PARTNERS-ITM_NUMBER = GT_LIST-POSNR.
      GT_CHANGE_PARTNERS-UPDATEFLAG = LV_LFAG.
      GT_CHANGE_PARTNERS-PARTN_ROLE = 'WE'.

      CLEAR GT_DUP.
      READ TABLE GT_DUP WITH KEY BSTKD = GT_LIST-BSTKD
                                 MATNR = GT_LIST-MATNR BINARY SEARCH.
      IF GT_DUP-KUNWE_I IS INITIAL.
        GT_CHANGE_PARTNERS-P_NUMB_OLD = GT_DUP-KUNWE_H.
      ELSE.
        GT_CHANGE_PARTNERS-P_NUMB_OLD = GT_DUP-KUNWE_I.
      ENDIF.
      GT_CHANGE_PARTNERS-P_NUMB_NEW = GT_LIST-KUNNR2.
      APPEND GT_CHANGE_PARTNERS. CLEAR GT_CHANGE_PARTNERS.

      GT_CHANGE_PARTNERS-DOCUMENT   = GT_LIST-VBELN.
      GT_CHANGE_PARTNERS-ITM_NUMBER = GT_LIST-POSNR.
      GT_CHANGE_PARTNERS-UPDATEFLAG = 'U'.
      GT_CHANGE_PARTNERS-PARTN_ROLE = 'ZS'.

      IF GT_DUP-LIFNR_I IS INITIAL.
        GT_CHANGE_PARTNERS-P_NUMB_OLD = GT_DUP-LIFNR_H.
      ELSE.
        GT_CHANGE_PARTNERS-P_NUMB_OLD = GT_DUP-LIFNR_I.
      ENDIF.
      GT_CHANGE_PARTNERS-P_NUMB_NEW = GT_LIST-KUNNR3.
      APPEND GT_CHANGE_PARTNERS. CLEAR GT_CHANGE_PARTNERS.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_BAPI_CHANGE
*&---------------------------------------------------------------------*
FORM CALL_BAPI_CHANGE  USING  PV_BSTKD.

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
    GT_LIST-ICON = ICON_GREEN_LIGHT.
    GT_LIST-MESSAGE = TEXT-S04 && '(' && GV_SALESDOCUMENT && ')'.
    MODIFY GT_LIST TRANSPORTING ICON MESSAGE WHERE BSTKD = PV_BSTKD AND ICON NE ICON_RED_LIGHT.
    ADD 1 TO GV_SUCCESS.

*ASIN CODE ????????????, ???????????? ???????????? ??????
    CLEAR GT_LIST.
    READ TABLE GT_LIST WITH KEY BSTKD = PV_BSTKD.
    READ TABLE GT_0060 WITH KEY ZKUNNR_IC = GT_LIST-KUNNR
                                VTWEG     = GT_LIST-VTWEG BINARY SEARCH.
    IF SY-SUBRC = 0.
      LOOP AT GT_LIST WHERE BSTKD = PV_BSTKD
                        AND ZASIN IS NOT INITIAL.
        GT_0120-KUNNR = GT_0060-KUNNR.
        GT_0120-MATNR = GT_LIST-MATNR.
        GT_0120-ZASIN = GT_LIST-ZASIN.
        COLLECT GT_0120. CLEAR GT_0120.
      ENDLOOP.
    ENDIF.

*Save log.
    LOOP AT GT_ORDER_ITEMS_IN.
      LOOP AT GT_LIST WHERE BSTKD = PV_BSTKD
                        AND POSNR = GT_ORDER_ITEMS_IN-ITM_NUMBER.
        MOVE-CORRESPONDING GT_LIST TO GT_0150.
        GT_0150-ZUPLOAD_DATE = GV_DATE.
        GT_0150-ZUPLOAD_TIME = GV_TIME.
        GT_0150-KWMENG       = GT_LIST-KWMENG_SUM.
        IF GT_LIST-KBETR IS INITIAL.
          GT_0150-KBETR       = GT_LIST-KBETR_SD.
        ELSE.
          GT_0150-KBETR       = GT_LIST-KBETR.
        ENDIF.
        GT_0150-LIFNR        = GT_LIST-KUNNR3.
        GT_0150-KUNWE        = GT_LIST-KUNNR2.
        GT_0150-VRKME        = GT_LIST-MEINS.


        CLEAR GT_DUP.
        READ TABLE GT_DUP WITH KEY BSTKD = GT_LIST-BSTKD
                                   MATNR = GT_LIST-MATNR BINARY SEARCH.
        IF GT_LIST-PARTNER = 'X'.
          IF GT_DUP-KUNWE_I IS INITIAL.
            GT_0150-KUNWE_OLD = GT_DUP-KUNWE_H.
          ELSE.
            GT_0150-KUNWE_OLD = GT_DUP-KUNWE_I.
          ENDIF.
          IF GT_0150-KUNWE_OLD EQ GT_0150-KUNWE.
            CLEAR GT_0150-KUNWE_OLD.
          ENDIF.

          IF GT_DUP-LIFNR_I IS INITIAL.
            GT_0150-LIFNR_OLD = GT_DUP-LIFNR_H.
          ELSE.
            GT_0150-LIFNR_OLD = GT_DUP-LIFNR_I.
          ENDIF.

          IF GT_0150-LIFNR_OLD EQ GT_0150-LIFNR.
            CLEAR GT_0150-LIFNR_OLD.
          ENDIF.
        ENDIF.
        IF GT_LIST-QTY = 'X'.
          GT_0150-KWMENG_OLD = GT_DUP-KWMENG.
        ENDIF.
        IF GT_LIST-PRICE = 'X'.
          GT_0150-KBETR_OLD = GT_DUP-NETPR.
        ENDIF.

        GT_0150-ERNAM        = SY-UNAME.
        GT_0150-ERDAT        = SY-DATUM.
        GT_0150-ERZET        = SY-UZEIT.
        APPEND GT_0150. CLEAR GT_0150.
      ENDLOOP.
    ENDLOOP.

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
      GT_LIST-ICON = ICON_RED_LIGHT.
      MODIFY GT_LIST TRANSPORTING MESSAGE ICON WHERE BSTKD = PV_BSTKD.
    ENDIF.
    ADD 1 TO GV_FAILURE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT
*&---------------------------------------------------------------------*
FORM CHECK_INPUT .

  CASE 'X'.
    WHEN P_UPLOAD.
      IF P_AUART IS INITIAL.
        MESSAGE S018 WITH 'Order Type' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF P_VKORG IS INITIAL.
        MESSAGE S018 WITH 'Sales Org.' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF P_VTWEG IS INITIAL.
        MESSAGE S018 WITH 'Distribution Channel' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF P_SPART IS INITIAL.
        MESSAGE S018 WITH 'Division' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF P_FILE IS INITIAL.
        MESSAGE S018 WITH 'File Path' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.

    WHEN P_DISP.

      IF S_VKORG[] IS INITIAL.
        MESSAGE S018 WITH 'Sales Org.' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF S_VTWEG[] IS INITIAL.
        MESSAGE S018 WITH 'Distribution Channel' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF S_SPART[] IS INITIAL.
        MESSAGE S018 WITH 'Division' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
      IF S_DATE[] IS INITIAL.
        MESSAGE S018 WITH 'Upload date' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

  SELECT A~VBELN,
         A~POSNR,
         A~ZUPLOAD_DATE,
         A~ZUPLOAD_TIME,
         A~BSTKD,
         A~LIFNR AS KUNNR3,
         A~MATNR,
         A~KWMENG,
         A~VRKME AS MEINS,
         A~KBETR,
         A~KONWA,
         A~KUNWE AS KUNNR2,
         A~KUNWE_OLD,
         A~LIFNR_OLD,
         A~KWMENG_OLD,
         A~KBETR_OLD,
         B~VKORG,
         B~VTWEG,
         B~SPART,
         B~KUNNR,
         C~MAKTX AS MATNR_TXT,
         D~NAME_ORG1 AS KUNNR_TXT,
         E~NAME_ORG1 AS KUNNR2_TXT,
         F~NAME_ORG1 AS KUNNR3_TXT

  INTO TABLE @DATA(LT_DATA)
  FROM ZSDT0150 AS A INNER JOIN VBAK AS B ON A~VBELN = B~VBELN
                     INNER JOIN MAKT AS C ON A~MATNR = C~MATNR
                                         AND C~SPRAS = @SY-LANGU
                     INNER JOIN BUT000 AS D ON B~KUNNR = D~PARTNER
                     INNER JOIN BUT000 AS E ON A~KUNWE = E~PARTNER
                     INNER JOIN BUT000 AS F ON A~LIFNR = F~PARTNER
  WHERE B~VKORG IN @S_VKORG
    AND B~VTWEG IN @S_VTWEG
    AND B~SPART IN @S_SPART
    AND A~ZUPLOAD_DATE IN @S_DATE
    AND B~KUNNR IN @S_KUNNR
    AND A~VBELN IN @S_VBELN
    AND A~BSTKD IN @S_BSTKD.

  _CLEAR GT_LIST.
  LOOP AT LT_DATA INTO DATA(LS_DATA).
    MOVE-CORRESPONDING LS_DATA TO GT_LIST.

    IF LS_DATA-LIFNR_OLD IS NOT INITIAL.
      IF LS_DATA-LIFNR_OLD NE LS_DATA-KUNNR3.
        GT_LIST-PARTNER = 'X'.
      ENDIF.
    ENDIF.
    IF LS_DATA-KWMENG_OLD IS NOT INITIAL.
      IF LS_DATA-KWMENG_OLD NE LS_DATA-KWMENG.
        GT_LIST-QTY = 'X'.
      ENDIF.
    ENDIF.
    IF LS_DATA-KBETR_OLD IS NOT INITIAL.
      IF LS_DATA-KBETR_OLD NE LS_DATA-KBETR.
        GT_LIST-PRICE = 'X'.
      ENDIF.
    ENDIF.
    IF LS_DATA-KUNWE_OLD IS NOT INITIAL.
      IF LS_DATA-KUNWE_OLD NE LS_DATA-KUNNR2.
        GT_LIST-PARTNER = 'X'.
      ENDIF.
    ENDIF.

    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  SORT GT_LIST BY VBELN POSNR ZUPLOAD_DATE ZUPLOAD_TIME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCAT_DISP
*&---------------------------------------------------------------------*
FORM SET_FIELDCAT_DISP.

  CLEAR: GT_FCAT[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'PARTNER'.
  GS_FCAT-COLTEXT    = 'Partner'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'QTY'.
  GS_FCAT-COLTEXT    = 'Qty.'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'PRICE'.
  GS_FCAT-COLTEXT    = 'Price'.
  GS_FCAT-FIX_COLUMN = 'X'.
  GS_FCAT-CHECKBOX   = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VBELN '.
  GS_FCAT-COLTEXT    = TEXT-F02."'Sales Order'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'POSNR'.
  GS_FCAT-COLTEXT    = TEXT-F03."'Item'.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZUPLOAD_DATE '.
  GS_FCAT-COLTEXT    = TEXT-F31.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZUPLOAD_TIME'.
  GS_FCAT-COLTEXT    = TEXT-F32.
  GS_FCAT-FIX_COLUMN = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'BSTKD'.
  GS_FCAT-COLTEXT    = TEXT-F11."'Customer Ref.'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR'.
  GS_FCAT-COLTEXT    = TEXT-F15."'Material (SKU)'.
  GS_FCAT-REF_TABLE = 'MARA'.
  GS_FCAT-REF_FIELD = 'MATNR'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MATNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F22."'Material (SKU)'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'LIFNR_OLD'.
  GS_FCAT-COLTEXT    = TEXT-F34."'Godds supplier OLD'.
  GS_FCAT-NO_ZERO    = 'X'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR3'.
  GS_FCAT-COLTEXT    = TEXT-F25."'Godds supplier'.
  GS_FCAT-NO_ZERO    = 'X'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR3_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F26."'SGodds supplier text'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KWMENG_OLD'.
  GS_FCAT-COLTEXT    = TEXT-F36."'qty old'.
  GS_FCAT-QFIELDNAME = 'MEINS'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KWMENG'.
  GS_FCAT-COLTEXT    = TEXT-F16."'Order QTY.'.
  GS_FCAT-QFIELDNAME = 'MEINS'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'MEINS'.
  GS_FCAT-COLTEXT    = TEXT-F33.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR_OLD'.
  GS_FCAT-COLTEXT    = TEXT-F37."'price old'.
  GS_FCAT-CFIELDNAME = 'KONWA'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KBETR'.
  GS_FCAT-COLTEXT    = TEXT-F29."'price'.
  GS_FCAT-CFIELDNAME = 'KONWA'.
  GS_FCAT-EMPHASIZE  = 'C100'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KONWA'.
  GS_FCAT-REF_TABLE  = 'KONP'.
  GS_FCAT-REF_FIELD  = 'KONWA'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VKORG'.
  GS_FCAT-REF_TABLE  = 'VBAK'.
  GS_FCAT-REF_FIELD  = 'VKORG'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'VTWEG'.
  GS_FCAT-REF_TABLE  = 'VBAK'.
  GS_FCAT-REF_FIELD  = 'VTWEG'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'SPART'.
  GS_FCAT-COLTEXT    = TEXT-F07."'Division'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR'.
  GS_FCAT-COLTEXT    = TEXT-F09."'Sold-to-Party'.
  GS_FCAT-NO_ZERO    = 'X'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F20."'Sold-to text'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNWE_OLD'.
  GS_FCAT-COLTEXT    = TEXT-F35."'ship to OLD'.
  GS_FCAT-NO_ZERO    = 'X'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR2'.
  GS_FCAT-COLTEXT    = TEXT-F10."'Ship-to-Party'.
  GS_FCAT-NO_ZERO    = 'X'.
  GS_FCAT-EMPHASIZE  = 'C500'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'KUNNR2_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F21."'Ship-to text'.
  APPEND GS_FCAT TO GT_FCAT. CLEAR: GS_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_COMPONENT
*&---------------------------------------------------------------------*
FORM GET_COMPONENT  USING    PV_MATNR.

  DATA : LT_STB    TYPE TABLE OF STPOX  WITH HEADER LINE,
         LT_MATCAT TYPE TABLE OF CSCMAT WITH HEADER LINE.

  _CLEAR : LT_STB, LT_MATCAT.
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      CAPID                 = 'SD01'
      DATUV                 = SY-DATLO
      MEHRS                 = 'X'
      MTNRV                 = PV_MATNR
      WERKS                 = '1001'
    TABLES
      STB                   = LT_STB
      MATCAT                = LT_MATCAT
    EXCEPTIONS
      ALT_NOT_FOUND         = 1
      CALL_INVALID          = 2
      MATERIAL_NOT_FOUND    = 3
      MISSING_AUTHORIZATION = 4
      NO_BOM_FOUND          = 5
      NO_PLANT_DATA         = 6
      NO_SUITABLE_BOM_FOUND = 7
      OTHERS                = 8.
  IF SY-SUBRC = 0.
    LOOP AT LT_STB.
      GT_SET-MATNR = PV_MATNR.
      GT_SET-IDNRK = LT_STB-IDNRK.
      APPEND GT_SET. CLEAR GT_SET.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPONENT_ITEM_DATA
*&---------------------------------------------------------------------*
FORM COMPONENT_ITEM_DATA .

  DATA : LV_SEQ TYPE POSNR_VA.

  CLEAR LV_SEQ.
  READ TABLE GT_SET WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
  IF SY-SUBRC = 0.
    LOOP AT GT_SET FROM SY-TABIX.
      IF GT_SET-MATNR NE GT_LIST-MATNR.
        CLEAR LV_SEQ.
        EXIT.
      ENDIF.

      ADD 1 TO LV_SEQ.

      GT_ORDER_ITEMS_IN-ITM_NUMBER      = GT_LIST-POSNR + LV_SEQ.
      GT_ORDER_ITEMS_INX-ITM_NUMBER     = GT_LIST-POSNR + LV_SEQ.

      GT_ORDER_ITEMS_IN-MATERIAL_LONG   = GT_SET-IDNRK.
      GT_ORDER_ITEMS_INX-MATERIAL_LONG  = 'X'.

      GT_ORDER_ITEMS_IN-PLANT           = GT_LIST-WERKS.  "?????????.
      GT_ORDER_ITEMS_INX-PLANT          = 'X'.

      GT_ORDER_ITEMS_IN-STORE_LOC       = GT_LIST-LGORT.  "????????????.
      GT_ORDER_ITEMS_INX-STORE_LOC      = 'X'.

      GT_ORDER_ITEMS_IN-PURCH_NO_C      = GT_LIST-BSTKD.  "Cust. Reference.
      GT_ORDER_ITEMS_INX-PURCH_NO_C     = 'X'.

      IF GT_LIST-POSEX IS INITIAL.
        GT_ORDER_ITEMS_IN-PO_ITM_NO      = GT_LIST-POSNR.    "Cust. Reference. ITEM
      ELSE.
        GT_ORDER_ITEMS_IN-PO_ITM_NO      = GT_LIST-POSEX.   "Cust. Reference. ITEM
      ENDIF.
      PERFORM ALPHA_INPUT USING GT_ORDER_ITEMS_IN-PO_ITM_NO.
      GT_ORDER_ITEMS_INX-PO_ITM_NO     = 'X'.

      GT_ORDER_ITEMS_IN-PURCH_DATE      = GT_LIST-BSTDK.  "Cust. Reference. DATE
      GT_ORDER_ITEMS_INX-PURCH_DATE     = 'X'.

      APPEND GT_ORDER_ITEMS_IN.      CLEAR GT_ORDER_ITEMS_IN.
      APPEND GT_ORDER_ITEMS_INX.     CLEAR GT_ORDER_ITEMS_INX.

      GT_ORDER_CONDITIONS_IN-ITM_NUMBER  = GT_LIST-POSNR + LV_SEQ.
      GT_ORDER_CONDITIONS_INX-ITM_NUMBER = 'X'.       "??????????????????

      IF P_VKORG = '1002'.
        GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR01'.
      ELSE.
        GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR00'.
      ENDIF.

      GT_ORDER_CONDITIONS_INX-COND_TYPE  = 'X'.       "????????????
      IF GT_LIST-KBETR_CONV IS NOT INITIAL.
        GT_ORDER_CONDITIONS_IN-COND_VALUE  = GT_LIST-KBETR_CONV. "?????? ??????.
        GT_ORDER_CONDITIONS_IN-CURRENCY    = GV_WAERS. "?????? ??????.

        GT_ORDER_CONDITIONS_INX-COND_VALUE = 'X'.       "?????? ??????
        GT_ORDER_CONDITIONS_INX-CURRENCY   = 'X'.       "?????? ??????.
      ENDIF.
      APPEND GT_ORDER_CONDITIONS_IN.  CLEAR GT_ORDER_CONDITIONS_IN.
      APPEND GT_ORDER_CONDITIONS_INX. CLEAR GT_ORDER_CONDITIONS_INX.


      GT_ORDER_PARTNERS-ITM_NUMBER   = GT_LIST-POSNR + LV_SEQ.
      GT_ORDER_PARTNERS-PARTN_ROLE   = 'ZS'.           "Goods supplier
      IF GT_LIST-GOODSUP IS INITIAL.
        GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR3.
      ELSE.
        GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-GOODSUP.
      ENDIF.
      APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.


      GT_ORDER_PARTNERS-ITM_NUMBER   = GT_LIST-POSNR + LV_SEQ.
      GT_ORDER_PARTNERS-PARTN_ROLE   = 'WE'.           "SHIP TO
      GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR2.

      APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPONENT_ITEM_UPDATE
*&---------------------------------------------------------------------*
FORM COMPONENT_ITEM_UPDATE .

  DATA : LV_SEQ  TYPE POSNR_VA,
         LV_LFAG.

  CLEAR LV_SEQ.
  READ TABLE GT_SET WITH KEY MATNR = GT_LIST-MATNR BINARY SEARCH.
  IF SY-SUBRC = 0.
    LOOP AT GT_SET FROM SY-TABIX.
      IF GT_SET-MATNR NE GT_LIST-MATNR.
        CLEAR LV_SEQ.
        EXIT.
      ENDIF.

      ADD 1 TO LV_SEQ.

      GT_ORDER_ITEMS_IN-ITM_NUMBER  = GT_LIST-POSNR + LV_SEQ.
      GT_ORDER_ITEMS_INX-ITM_NUMBER = GT_LIST-POSNR + LV_SEQ.

      CLEAR LV_LFAG.
      IF GT_LIST-NEW_ITEM = 'X'.
        LV_LFAG = 'I'.

        GT_ORDER_ITEMS_IN-PLANT       = GT_LIST-WERKS.
        GT_ORDER_ITEMS_INX-PLANT      = 'X'.
        GT_ORDER_ITEMS_IN-STORE_LOC   = GT_LIST-LGORT.
        GT_ORDER_ITEMS_INX-STORE_LOC  = 'X'.

        GT_ORDER_ITEMS_IN-PURCH_NO_C  = GT_LIST-BSTKD.
        GT_ORDER_ITEMS_INX-PURCH_NO_C = 'X'.

        IF GT_LIST-POSEX IS INITIAL.
          GT_ORDER_ITEMS_IN-PO_ITM_NO = GT_LIST-POSNR + LV_SEQ.
        ELSE.
          GT_ORDER_ITEMS_IN-PO_ITM_NO = GT_LIST-POSEX.   "Cust. Reference. ITEM
        ENDIF.

        PERFORM ALPHA_INPUT USING GT_ORDER_ITEMS_IN-PO_ITM_NO.
        GT_ORDER_ITEMS_INX-PO_ITM_NO   = 'X'.

        GT_ORDER_ITEMS_IN-PURCH_DATE   = GT_LIST-BSTDK .
        GT_ORDER_ITEMS_INX-PURCH_DATE  = 'X'.

      ELSE.
        LV_LFAG = 'U'.
      ENDIF.

      GT_ORDER_ITEMS_INX-UPDATEFLAG    = LV_LFAG.

      GT_ORDER_ITEMS_IN-MATERIAL    = GT_SET-IDNRK.
      GT_ORDER_ITEMS_INX-MATERIAL   = 'X'.

      GT_ORDER_ITEMS_IN-TARGET_QTY   = GT_LIST-KWMENG_SUM.
      GT_ORDER_ITEMS_INX-TARGET_QTY  = 'X'.

      GT_ORDER_ITEMS_IN-TARGET_QU   = 'EA'.
      GT_ORDER_ITEMS_INX-TARGET_QU  = 'X'.

      APPEND GT_ORDER_ITEMS_IN.      CLEAR GT_ORDER_ITEMS_IN.
      APPEND GT_ORDER_ITEMS_INX.     CLEAR GT_ORDER_ITEMS_INX.
      IF LV_LFAG = 'U'.
        GT_ORDER_SCHEDULES_IN-ITM_NUMBER  = GT_LIST-POSNR + LV_SEQ.
        GT_ORDER_SCHEDULES_INX-ITM_NUMBER = GT_LIST-POSNR + LV_SEQ.

        GT_ORDER_SCHEDULES_IN-SCHED_LINE  = 1.
        GT_ORDER_SCHEDULES_INX-SCHED_LINE = 1.

        GT_ORDER_SCHEDULES_INX-UPDATEFLAG = LV_LFAG.

        GT_ORDER_SCHEDULES_IN-REQ_QTY     = GT_LIST-KWMENG_SUM.  "?????? .
        GT_ORDER_SCHEDULES_INX-REQ_QTY    = 'X'.

        APPEND GT_ORDER_SCHEDULES_IN.  CLEAR GT_ORDER_SCHEDULES_IN.
        APPEND GT_ORDER_SCHEDULES_INX. CLEAR GT_ORDER_SCHEDULES_INX.
      ENDIF.
      GT_ORDER_CONDITIONS_IN-ITM_NUMBER  = GT_LIST-POSNR + LV_SEQ.
      GT_ORDER_CONDITIONS_INX-ITM_NUMBER = GT_LIST-POSNR + LV_SEQ.

*??????????????? ????????? ??????
      IF GT_LIST-PRICE = 'X' OR  GT_LIST-PARTNER = 'X'.
        GT_ORDER_CONDITIONS_INX-UPDATEFLAG = LV_LFAG.

        IF P_VKORG = '1002'.
          GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR01'.
        ELSE.
          GT_ORDER_CONDITIONS_IN-COND_TYPE   = 'PR00'.
        ENDIF.
        GT_ORDER_CONDITIONS_INX-COND_TYPE  = 'X'.       "????????????

        IF GT_LIST-KBETR_CONV IS INITIAL.
          GT_ORDER_CONDITIONS_IN-COND_VALUE  = GT_LIST-KBETR_SD. "?????? ??????.
        ELSE.
          GT_ORDER_CONDITIONS_IN-COND_VALUE  = GT_LIST-KBETR_CONV. "?????? ??????.
        ENDIF.
        GT_ORDER_CONDITIONS_INX-COND_VALUE = 'X'.       "?????? ??????

        GT_ORDER_CONDITIONS_IN-CURRENCY    = GV_WAERS. "?????? ??????.
        GT_ORDER_CONDITIONS_INX-CURRENCY   = 'X'.       "?????? ??????.

        APPEND GT_ORDER_CONDITIONS_IN.  CLEAR GT_ORDER_CONDITIONS_IN.
        APPEND GT_ORDER_CONDITIONS_INX. CLEAR GT_ORDER_CONDITIONS_INX.
      ENDIF.
*PARNTER
      IF LV_LFAG = 'I'.
        GT_ORDER_PARTNERS-ITM_NUMBER   = GT_LIST-POSNR + LV_SEQ.
        GT_ORDER_PARTNERS-PARTN_ROLE   = 'ZS'.           "Goods supplier
        IF GT_LIST-GOODSUP IS INITIAL.
          GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR3.
        ELSE.
          GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-GOODSUP.
        ENDIF.
        APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.

        GT_ORDER_PARTNERS-ITM_NUMBER   = GT_LIST-POSNR + LV_SEQ.
        GT_ORDER_PARTNERS-PARTN_ROLE   = 'WE'.           "SHIP TO
        GT_ORDER_PARTNERS-PARTN_NUMB   = GT_LIST-KUNNR2.

        APPEND GT_ORDER_PARTNERS. CLEAR GT_ORDER_PARTNERS.
      ELSE.
        IF GT_LIST-PARTNER = 'X'.
          GT_CHANGE_PARTNERS-DOCUMENT   = GT_LIST-VBELN.
          GT_CHANGE_PARTNERS-ITM_NUMBER = GT_LIST-POSNR + LV_SEQ.
          GT_CHANGE_PARTNERS-UPDATEFLAG = LV_LFAG.
          GT_CHANGE_PARTNERS-PARTN_ROLE = 'WE'.

          CLEAR GT_DUP.
          READ TABLE GT_DUP WITH KEY BSTKD = GT_LIST-BSTKD
                                     MATNR = GT_LIST-MATNR BINARY SEARCH.
          IF GT_DUP-KUNWE_I IS INITIAL.
            GT_CHANGE_PARTNERS-P_NUMB_OLD = GT_DUP-KUNWE_H.
          ELSE.
            GT_CHANGE_PARTNERS-P_NUMB_OLD = GT_DUP-KUNWE_I.
          ENDIF.
          GT_CHANGE_PARTNERS-P_NUMB_NEW = GT_LIST-KUNNR2.
          APPEND GT_CHANGE_PARTNERS. CLEAR GT_CHANGE_PARTNERS.

          GT_CHANGE_PARTNERS-DOCUMENT   = GT_LIST-VBELN.
          GT_CHANGE_PARTNERS-ITM_NUMBER = GT_LIST-POSNR + LV_SEQ.
          GT_CHANGE_PARTNERS-UPDATEFLAG = 'U'.
          GT_CHANGE_PARTNERS-PARTN_ROLE = 'ZS'.

          IF GT_DUP-LIFNR_I IS INITIAL.
            GT_CHANGE_PARTNERS-P_NUMB_OLD = GT_DUP-LIFNR_H.
          ELSE.
            GT_CHANGE_PARTNERS-P_NUMB_OLD = GT_DUP-LIFNR_I.
          ENDIF.
          GT_CHANGE_PARTNERS-P_NUMB_NEW = GT_LIST-KUNNR3.
          APPEND GT_CHANGE_PARTNERS. CLEAR GT_CHANGE_PARTNERS.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.

ENDFORM.
