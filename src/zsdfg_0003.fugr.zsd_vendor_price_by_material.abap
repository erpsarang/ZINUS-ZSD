FUNCTION ZSD_VENDOR_PRICE_BY_MATERIAL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_WERKS) TYPE  WERKS_D DEFAULT '1001'
*"     REFERENCE(IV_EKORG) TYPE  EKORG
*"     REFERENCE(IV_KSCHL) TYPE  KSCHL DEFAULT 'PB00'
*"     REFERENCE(IV_MATNR) TYPE  MATNR
*"     VALUE(IV_LIFNR) TYPE  LIFNR OPTIONAL
*"     REFERENCE(IV_VALIDDATE) TYPE  KODATAB DEFAULT SY-DATUM
*"  EXPORTING
*"     REFERENCE(EX_RETURN)
*"  TABLES
*"      ET_DATA STRUCTURE  ZSDS0050 OPTIONAL
*"----------------------------------------------------------------------

  DATA : LS_DATA LIKE ZSDS0050,
         LT_DATA LIKE TABLE OF LS_DATA.

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

  RANGES : LR_LIFNR FOR LFA1-LIFNR.

  IF IV_LIFNR IS NOT INITIAL.
    LR_LIFNR = 'IEQ'.
    LR_LIFNR-LOW = IV_LIFNR.
    APPEND LR_LIFNR.
  ENDIF.

  IF IV_VALIDDATE IS INITIAL.
    LV_VALIDFR = SY-DATUM.
  ELSE.
    LV_VALIDFR = IV_VALIDDATE.
  ENDIF.

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
         WHERE A~KSCHL EQ @IV_KSCHL
         AND   A~MATNR EQ @IV_MATNR
         AND   A~EKORG EQ @IV_EKORG
         AND   A~WERKS EQ @IV_WERKS
         AND   A~LIFNR IN @LR_LIFNR
         AND   A~DATBI GE @LV_VALIDFR
         AND   A~DATAB LE @LV_VALIDFR.
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

    CLEAR : ET_DATA.
    ET_DATA[] = LT_DATA[].

  ELSE.

  ENDIF.


ENDFUNCTION.
