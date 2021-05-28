*&---------------------------------------------------------------------*
*& Include          ZSDR0100_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INIT
*&---------------------------------------------------------------------*
FORM INIT.
  DATA : LV_DAT TYPE SY-DATLO.

  P_VKORG = '2011'. " Sales org.
  P_SPART = '00'.   " Division
  P_AUART = 'ZICW'. " Type.

  CLEAR: LV_DAT.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATLO
      DAYS      = '00'
      MONTHS    = '06'
      SIGNUM    = '-'
      YEARS     = '00'
    IMPORTING
      CALC_DATE = LV_DAT.

  S_ERDAT-LOW    = LV_DAT.
  S_ERDAT-HIGH   = SY-DATLO.
  S_ERDAT-SIGN   = 'I'.
  S_ERDAT-OPTION = 'BT'.
  APPEND S_ERDAT.

*S_2021/03/15 BY E00064
  S_BIDAT-LOW    = SY-DATLO.
  S_BIDAT-LOW+6(2)   = '01'.
  S_BIDAT-HIGH   = SY-DATLO.
  S_BIDAT-SIGN   = 'I'.
  S_BIDAT-OPTION = 'BT'.
  APPEND S_BIDAT.
*E_2021/03/15 BY E00064

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
*{   INSERT         ZUQK901230                                        2

*}   INSERT
  DATA: LT_SORD    LIKE TABLE OF GT_DATA WITH HEADER LINE,
        WA_SORD    LIKE GT_DATA,
        LT_TEMP    LIKE TABLE OF GT_DATA WITH HEADER LINE,
        LT_COMWA   LIKE TABLE OF VBCO6 WITH HEADER LINE,
        LT_VBFA    LIKE TABLE OF VBFAS WITH HEADER LINE,
        LT_VBFAB   LIKE TABLE OF VBFAS WITH HEADER LINE,
*S_2021/3/4 add BY E00064
        LT_VBFAB_A LIKE TABLE OF VBFAS WITH HEADER LINE,
*E_2021/3/4 add BY E00064
        LV_INDEX   TYPE SY-TABIX,
        LV_TABIX   TYPE SY-TABIX,
        LT_MAKT    LIKE TABLE OF MAKT WITH HEADER LINE,
        LT_MAKT_S  LIKE TABLE OF MAKT WITH HEADER LINE,
        LV_BSTNK   LIKE VBAK-BSTNK,
        LV_CNT     TYPE I,
        LV_UNIT    TYPE VBAP-VRKME.

  DATA: LT_STTRG TYPE TABLE OF DD07V WITH HEADER LINE,
        LT_WBSTA TYPE TABLE OF DD07V WITH HEADER LINE,
        LT_KNA1  TYPE TABLE OF KNA1  WITH HEADER LINE,
        LT_KUNWE TYPE TABLE OF KNA1  WITH HEADER LINE,
        LT_ORGS  TYPE TABLE OF KNA1  WITH HEADER LINE,
        LT_ABGRU TYPE TABLE OF TVAGT WITH HEADER LINE,
        LT_VTTK  TYPE TABLE OF VTTK  WITH HEADER LINE,
        LT_VBRK  TYPE TABLE OF VBRK  WITH HEADER LINE,
        LT_VBEP  TYPE TABLE OF VBEP  WITH HEADER LINE.

  DATA: LS_VBEP LIKE GT_VBEP.
  DATA: LV_EDATU(32),
        LV_DATE(10),
        LV_BMENG TYPE VBEP-BMENG.

  DATA: LV_EBELN     TYPE EKBE-EBELN,
        LV_EBELP     TYPE EKBE-EBELP,
        LV_XBLNR(16).

  DATA: LV_FIRST        TYPE STRING,
        LV_SECOND       TYPE STRING,
        LV_VBELN_VL(14).

  RANGES:  R_VBTYP FOR VBFAS-VBTYP_N. "flow
  R_VBTYP-LOW = 'N'.
  R_VBTYP-SIGN = 'I'.
  R_VBTYP-OPTION = 'EQ'.
  APPEND R_VBTYP. CLEAR R_VBTYP.
  R_VBTYP-LOW = 'M'.
  R_VBTYP-SIGN = 'I'.
  R_VBTYP-OPTION = 'EQ'.
  APPEND R_VBTYP. CLEAR R_VBTYP.

  CLEAR: GT_DATA, GT_DATA[], GT_VBEP, GT_VBEP[], GT_MAIN, GT_MAIN[],
         LT_SORD, LT_SORD[], LT_TEMP, LT_TEMP[], LT_COMWA, LT_COMWA[],
         LT_VBFA, LT_VBFA[], LT_VBFAB, LT_VBFAB[], LT_MAKT, LT_MAKT[],
         LT_MAKT_S, LT_MAKT_S[], LT_STTRG, LT_STTRG[], LT_WBSTA, LT_WBSTA[],
         LT_KNA1, LT_KNA1[], LT_KUNWE, LT_KUNWE[], LT_ORGS, LT_ORGS[],
         LT_ABGRU, LT_ABGRU[], LT_VTTK, LT_VTTK[], LT_VBRK, LT_VBRK[],
         LT_VBEP, LT_VBEP[].

*--Quotation
  SELECT AK~VBELN,
    AP~POSNR, AP~MATNR, AP~KWMENG, AP~VKAUS AS VKAUS_Q,
    AK~BSTNK, AP~VRKME AS VRKME_Q,
    AK~GBSTK, AK~VTWEG, AK~VDATU
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA
    FROM VBAK AS AK INNER JOIN VBAP AS AP ON AK~VBELN EQ AP~VBELN
                    INNER JOIN VBPA AS B ON AK~VBELN = B~VBELN
                                       AND B~POSNR = '00000'
                                       AND B~PARVW = 'WE'
    WHERE AK~AUART EQ 'ZQT'
      AND AK~BSTNK IN @S_BSTNK
      AND AK~VKORG EQ @P_VKORG
      AND AK~VTWEG IN @S_VTWEG
      AND AK~SPART EQ @P_SPART
      AND AP~MATNR IN @S_MATNR
      AND B~KUNNR  IN @S_KUNNR
      AND AK~ERDAT IN @S_ERDAT.

  SORT GT_DATA BY VBELN POSNR.

  IF GT_DATA[] IS NOT INITIAL.
    SELECT VBELN, POSNR, EDATU AS EDATU_Q, BMENG
      FROM VBEP
      INTO CORRESPONDING FIELDS OF TABLE @GT_VBEP
      FOR ALL ENTRIES IN @GT_DATA
      WHERE VBELN EQ @GT_DATA-VBELN
        AND BMENG > 0.

    SORT GT_VBEP BY VBELN POSNR EDATU_Q.

    LV_TABIX = 1.
    LOOP AT GT_VBEP.
      "Confirmed Qty
      LV_BMENG = LV_BMENG + GT_VBEP-BMENG.

      "user date format
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          INPUT  = GT_VBEP-EDATU_Q
        IMPORTING
          OUTPUT = LV_DATE.

      "Schedule line date
      IF LV_EDATU IS INITIAL.
        LV_EDATU = LV_DATE.
      ELSE.
        CONCATENATE LV_EDATU LV_DATE INTO LV_EDATU SEPARATED BY ','.
      ENDIF.

*      LV_TABIX = LV_TABIX + 1.
*
*      READ TABLE GT_VBEP INTO LS_VBEP INDEX LV_TABIX. "READ NEXT ROW
*      IF LS_VBEP-VBELN NE GT_VBEP-VBELN OR
*         LS_VBEP-POSNR NE GT_VBEP-POSNR.
*
*        READ TABLE GT_DATA WITH KEY VBELN = GT_VBEP-VBELN
*                                    POSNR = GT_VBEP-POSNR.
*        IF SY-SUBRC EQ 0.
*          GT_DATA-BMENG = LV_BMENG.
*          GT_DATA-VDATU = LV_EDATU.
*
*          MODIFY GT_DATA TRANSPORTING BMENG VDATU WHERE VBELN EQ GT_VBEP-VBELN
*                                                    AND POSNR EQ GT_VBEP-POSNR .
*
*          CLEAR : GT_DATA, LV_BMENG, LV_EDATU.
*        ENDIF.
*
*      ENDIF.

      " quotation , item 이 바뀌면 gt_data modify
      AT END OF POSNR.
        READ TABLE GT_DATA WITH KEY VBELN = GT_VBEP-VBELN
                                    POSNR = GT_VBEP-POSNR.
        IF SY-SUBRC EQ 0.
          GT_DATA-BMENG = LV_BMENG.
          GT_DATA-EDATU_Q = LV_EDATU.

          MODIFY GT_DATA TRANSPORTING BMENG EDATU_Q WHERE VBELN EQ GT_VBEP-VBELN
                                                      AND POSNR EQ GT_VBEP-POSNR .

          CLEAR : GT_DATA, LV_BMENG, LV_EDATU.
        ENDIF.
      ENDAT.

      CLEAR : GT_VBEP, LS_VBEP.
    ENDLOOP.
  ENDIF.

  IF GT_DATA[] IS NOT INITIAL.
    LT_TEMP[] = GT_DATA[].
    SORT LT_TEMP BY BSTNK.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING BSTNK.
    SORT LT_TEMP BY BSTNK.
*--S/O
    IF LT_TEMP[] IS NOT INITIAL.
      SELECT A~VBELN AS VBELN_S,   "S/O #
             A~KUNNR,              "Sold-to party
             A~VTWEG AS VTWEG_S,   "Distr. Channel
             B~KUNNR AS KUNWE,     "Sold-to party
             C~KUNNR AS ORGSOLDTO, "org-sold to
             K~BSTKD_E,            "AZ PO#
             K~IHREZ,              "HQ PO
             K~BSTKD,              "AZ PO#
             P~POSNR AS POSNR_S,   "Item #
             P~MATNR AS MATNR_S,   "Material
             P~WERKS,              "Plant
             P~LGORT,              "Storage location
             P~NETPR,              "Net Price
             P~KWMENG AS KWMENG_S, "Qty
             P~VRKME,              "Sales Unit
             P~NETPR,              "Net Price
             P~NETWR,              "Net Value
             P~WAERK,              "Currency
             P~LFGSA,              "Status
             P~ABGRU,              "Reason for Rejection
             P~VKAUS,              "Usage Indicator
             L~VBELN AS VBELN_VL,  "Delivery
             L~POSNR AS POSNR_VL,  "Delivery Item
             I~WADAT_IST,          "Qty delivered
             L~LFIMG,              "Movement Status
             L~WBSTA,              "Actual goods movement date
             I~WADAT,              "Plan GI date
             L~MBDAT AS MBDAT_D,   "Mat.avail.date
             I~LFDAT               "Delivery date
      INTO CORRESPONDING FIELDS OF TABLE @LT_SORD
      FROM VBAK AS A INNER JOIN VBPA AS B ON A~VBELN = B~VBELN
                                         AND B~POSNR = '00000'
                                         AND B~PARVW = 'WE'
                LEFT OUTER JOIN VBPA AS C ON A~VBELN = C~VBELN
                                         AND C~POSNR = '00000'
                                         AND C~PARVW = 'SX'
                     INNER JOIN VBKD AS K ON A~VBELN = K~VBELN
                                         AND K~POSNR = '00000'
                     INNER JOIN VBAP AS P ON A~VBELN = P~VBELN
                LEFT OUTER JOIN LIPS AS L ON P~VBELN = L~VGBEL
                                         AND P~POSNR = L~VGPOS
                LEFT OUTER JOIN LIKP AS I ON I~VBELN = L~VBELN
      FOR ALL ENTRIES IN @LT_TEMP
      WHERE A~AUART EQ @P_AUART
        AND P~MATNR IN @S_MATNR
        AND A~VKORG EQ @P_VKORG
        AND A~VTWEG IN @S_VTWEG
        AND A~SPART EQ @P_SPART
        AND B~KUNNR IN @S_KUNNR
        AND K~BSTKD_E EQ @LT_TEMP-BSTNK.

      DELETE LT_SORD WHERE BSTKD_E IS INITIAL.
    ENDIF.

    CLEAR: LT_TEMP[], LT_TEMP.
    LT_TEMP[] = GT_DATA[].
    SORT LT_TEMP BY MATNR.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING MATNR.

    IF LT_TEMP[] IS NOT INITIAL.
      SELECT MATNR MAKTX FROM MAKT INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
        FOR ALL ENTRIES IN LT_TEMP WHERE MATNR = LT_TEMP-MATNR
                                     AND SPRAS = SY-LANGU.
      SORT LT_MAKT BY MATNR.
    ENDIF.

    CLEAR: LT_TEMP[], LT_TEMP.
    LT_TEMP[] = LT_SORD[].
    SORT LT_TEMP BY MATNR_S.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING MATNR_S.

    IF LT_TEMP[] IS NOT INITIAL.
      SELECT MATNR MAKTX FROM MAKT INTO CORRESPONDING FIELDS OF TABLE LT_MAKT_S
        FOR ALL ENTRIES IN LT_TEMP WHERE MATNR = LT_TEMP-MATNR_S
                                     AND SPRAS = SY-LANGU.
      SORT LT_MAKT_S BY MATNR.
    ENDIF.

    CLEAR: LT_TEMP[], LT_TEMP.
    LT_TEMP[] = LT_SORD[].
    SORT LT_TEMP BY KUNNR.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING KUNNR.

    IF LT_TEMP[] IS NOT INITIAL.
      SELECT KUNNR NAME1 FROM KNA1 INTO CORRESPONDING FIELDS OF TABLE LT_KNA1 "soldto
        FOR ALL ENTRIES IN LT_TEMP WHERE KUNNR = LT_TEMP-KUNNR
                                     AND SPRAS = SY-LANGU.
      SORT LT_KNA1 BY KUNNR.
    ENDIF.

    CLEAR: LT_TEMP[], LT_TEMP.
    LT_TEMP[] = LT_SORD[].
    SORT LT_TEMP BY KUNWE.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING KUNWE.

    IF LT_TEMP[] IS NOT INITIAL.
      SELECT KUNNR NAME1 FROM KNA1 INTO CORRESPONDING FIELDS OF TABLE LT_KUNWE "shipto
        FOR ALL ENTRIES IN LT_TEMP WHERE KUNNR = LT_TEMP-KUNWE
                                     AND SPRAS = SY-LANGU.
      SORT LT_KUNWE BY KUNNR.
    ENDIF.

    CLEAR: LT_TEMP[], LT_TEMP.
    LT_TEMP[] = LT_SORD[].
    SORT LT_TEMP BY ORGSOLDTO.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING ORGSOLDTO.

    IF LT_TEMP[] IS NOT INITIAL.
      SELECT KUNNR NAME1 FROM KNA1 INTO CORRESPONDING FIELDS OF TABLE LT_ORGS "orgsoldto
        FOR ALL ENTRIES IN LT_TEMP WHERE KUNNR = LT_TEMP-ORGSOLDTO
                                     AND SPRAS = SY-LANGU.
      SORT LT_ORGS BY KUNNR.
    ENDIF.

    CLEAR: LT_TEMP[], LT_TEMP.
    LT_TEMP[] = LT_SORD[].
    SORT LT_TEMP BY ABGRU.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING ABGRU.

    IF LT_TEMP[] IS NOT INITIAL.
      SELECT ABGRU BEZEI FROM TVAGT INTO CORRESPONDING FIELDS OF TABLE LT_ABGRU "rj
        FOR ALL ENTRIES IN LT_TEMP WHERE ABGRU = LT_TEMP-ABGRU
                                     AND SPRAS = SY-LANGU.
      SORT LT_ABGRU BY ABGRU.
    ENDIF.

    CLEAR: LT_TEMP[], LT_TEMP.
    LT_TEMP[] = LT_SORD[].
    SORT LT_TEMP BY VBELN_S POSNR_S.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING VBELN_S POSNR_S.

    IF LT_TEMP[] IS NOT INITIAL.
      SELECT VBELN POSNR EDATU MBDAT FROM VBEP INTO CORRESPONDING FIELDS OF TABLE LT_VBEP "Schedule line date
        FOR ALL ENTRIES IN LT_TEMP WHERE VBELN = LT_TEMP-VBELN_S
                                     AND POSNR = LT_TEMP-POSNR_S.
      SORT LT_VBEP BY VBELN POSNR.
    ENDIF.

    CLEAR: LT_TEMP[], LT_TEMP.
    LT_TEMP[] = LT_SORD[].
    SORT LT_TEMP BY BSTKD.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING BSTKD.

    CALL FUNCTION 'DD_DD07V_GET'
      EXPORTING
        DOMAIN_NAME    = 'STTRG'
        LANGU          = SY-LANGU
        WITHTEXT       = 'X'
      TABLES
        DD07V_TAB      = LT_STTRG
      EXCEPTIONS
        ACCESS_FAILURE = 1
        OTHERS         = 2.
    SORT LT_STTRG BY DOMVALUE_L.

    CALL FUNCTION 'DD_DD07V_GET'
      EXPORTING
        DOMAIN_NAME    = 'STATV'
        LANGU          = SY-LANGU
        WITHTEXT       = 'X'
      TABLES
        DD07V_TAB      = LT_WBSTA
      EXCEPTIONS
        ACCESS_FAILURE = 1
        OTHERS         = 2.
    SORT LT_WBSTA BY DOMVALUE_L.

    SORT LT_SORD BY VBELN_S.

    LOOP AT LT_SORD.
      LV_INDEX = SY-TABIX.

      AT NEW VBELN_S.

        CLEAR: LT_COMWA, LT_COMWA[], LT_VBFA, LT_VBFA[], LT_VBFAB, LT_VBFAB[].

        LT_COMWA-VBELN = LT_SORD-VBELN_S.
        APPEND LT_COMWA.

        CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
          EXPORTING
            COMWA         = LT_COMWA
          TABLES
            VBFA_TAB      = LT_VBFA
          EXCEPTIONS
            NO_VBFA       = 1
            NO_VBUK_FOUND = 2
            OTHERS        = 3.

        LT_VBFAB[] = LT_VBFA[].
*S_2021/3/4 add BY E00064
        LT_VBFAB_A[] = LT_VBFA[].
*E_2021/3/4 add BY E00064

        DELETE LT_VBFA WHERE VBTYP_N NE '8'.
        SORT LT_VBFA BY VBELV.

        IF LT_VBFA[] IS NOT INITIAL.
          CLEAR : LT_VTTK, LT_VTTK[].
          SELECT TKNUM STTRG DTABF EXTI1 FROM VTTK INTO CORRESPONDING FIELDS OF TABLE LT_VTTK "Shipment
            FOR ALL ENTRIES IN LT_VBFA
            WHERE TKNUM EQ LT_VBFA-VBELN.
          SORT LT_VTTK BY TKNUM.
        ENDIF.

        DELETE LT_VBFAB WHERE VBTYP_N NOT IN R_VBTYP. " M , N
        SORT LT_VBFAB BY VBELV POSNV ERDAT DESCENDING ERZET DESCENDING.

        IF LT_VBFAB[] IS NOT INITIAL.
          CLEAR : LT_VBRK, LT_VBRK[].
          SELECT VBELN FKDAT FROM VBRK INTO CORRESPONDING FIELDS OF TABLE LT_VBRK "Billing
            FOR ALL ENTRIES IN LT_VBFAB
            WHERE VBELN EQ LT_VBFAB-VBELN.
          SORT LT_VBRK BY VBELN.
        ENDIF.
      ENDAT.

      IF LT_VBFA[] IS NOT INITIAL.

        CLEAR LT_VBFA.
        READ TABLE LT_VBFA WITH KEY VBELV = LT_SORD-VBELN_VL BINARY SEARCH.

        IF SY-SUBRC = 0.
          LT_SORD-TKNUM = LT_VBFA-VBELN.  "Shipment

          CLEAR LT_VTTK.
          READ TABLE LT_VTTK WITH KEY TKNUM = LT_VBFA-VBELN BINARY SEARCH.
          IF SY-SUBRC = 0.
            LT_SORD-STTRG = LT_VTTK-STTRG. "shmt status
            LT_SORD-DTABF = LT_VTTK-DTABF. "compl date
            LT_SORD-EXTI1 = LT_VTTK-EXTI1. "BL No.
          ENDIF.

          CLEAR LT_STTRG.
          READ TABLE LT_STTRG WITH KEY DOMVALUE_L = LT_SORD-STTRG.
          IF SY-SUBRC = 0.
            LT_SORD-STTRG_T = LT_STTRG-DDTEXT. "shmt sts text
          ENDIF.
        ENDIF.
      ENDIF.


      IF LT_VBFAB[] IS NOT INITIAL.
        CLEAR LT_VBFAB.
        READ TABLE LT_VBFAB WITH KEY VBELV = LT_SORD-VBELN_VL
                                     POSNV = LT_SORD-POSNR_VL BINARY SEARCH.
        IF LT_VBFAB-VBTYP_N = 'M'.
          LT_SORD-VBELN_B = LT_VBFAB-VBELN.  "Billing doc
          LT_SORD-POSNR_B = LT_VBFAB-POSNN.  "item
*S_2021/3/4 add BY E00064
*위의 FUNC RV_ORDER_FLOW_INFORMATION로 가져온 vbfa_tab 빌링문서에 금액필드도 가져와 display
          LT_SORD-RFWRT   = LT_VBFAB-RFWRT.  "Billing amount
          LT_SORD-WAERS   = LT_VBFAB-WAERS.  "Billing amt currency
*E_2021/3/4 add BY E00064

          CLEAR LT_VBRK.
          READ TABLE LT_VBRK WITH KEY VBELN = LT_VBFAB-VBELN BINARY SEARCH.
          IF SY-SUBRC = 0.
            LT_SORD-FKDAT = LT_VBRK-FKDAT.  "date
          ENDIF.
        ENDIF.
      ENDIF.

*S_2021/3/4 add BY E00064
*빌링의 accounting Doc을 display
*가져온 빌링문서, 아이템번호를 선행문서필드(vbelv, posnv) 에 넣고 vbtyp_n이 '+'인 vbeln
      IF LT_SORD-VBELN_B IS NOT INITIAL.
        SORT LT_VBFAB_A BY VBELV POSNV VBTYP_N.
        CLEAR LT_VBFAB_A.
        READ TABLE LT_VBFAB_A WITH KEY VBELV = LT_SORD-VBELN_B
                                       POSNV = LT_SORD-POSNR_B
                                       VBTYP_N = '+' BINARY SEARCH.
        IF SY-SUBRC = 0.
          LT_SORD-VBELN_A = LT_VBFAB_A-VBELN.
        ENDIF.
      ENDIF.
*E_2021/3/4 add BY E00064

      CLEAR LT_KNA1.
      READ TABLE LT_KNA1 WITH KEY KUNNR = LT_SORD-KUNNR BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_SORD-KUNNR_T = LT_KNA1-NAME1. "Shipto
      ENDIF.

      CLEAR LT_KUNWE.
      READ TABLE LT_KUNWE WITH KEY KUNNR = LT_SORD-KUNWE BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_SORD-KUNWE_T = LT_KUNWE-NAME1. "Soldto
      ENDIF.

      CLEAR LT_ORGS.
      READ TABLE LT_ORGS WITH KEY KUNNR = LT_SORD-ORGSOLDTO BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_SORD-ORGS_T = LT_ORGS-NAME1. "orgsoldto
      ENDIF.

      CLEAR LT_WBSTA.
      READ TABLE LT_WBSTA WITH KEY DOMVALUE_L = LT_SORD-LFGSA BINARY SEARCH.
      IF SY-SUBRC = 0.
        LT_SORD-LFGSA_T = LT_WBSTA-DDTEXT.  "delv status
      ENDIF.

      CLEAR LT_ABGRU.
      READ TABLE LT_ABGRU WITH KEY ABGRU = LT_SORD-ABGRU BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_SORD-ABGRU_T = LT_ABGRU-BEZEI. "reason for rej
      ENDIF.

      IF LT_SORD-WBSTA IS NOT INITIAL.
        CLEAR LT_WBSTA.
        READ TABLE LT_WBSTA WITH KEY DOMVALUE_L = LT_SORD-WBSTA BINARY SEARCH.
        IF SY-SUBRC = 0.
          LT_SORD-WBSTA_T = LT_WBSTA-DDTEXT.  "movmt status
        ENDIF.
      ENDIF.

      CLEAR LT_MAKT_S.
      READ TABLE LT_MAKT_S WITH KEY MATNR = LT_SORD-MATNR_S BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_SORD-MAKTX_S = LT_MAKT_S-MAKTX.
      ENDIF.

      CLEAR LT_VBEP.
      READ TABLE LT_VBEP WITH KEY VBELN = LT_SORD-VBELN_S
                                  POSNR = LT_SORD-POSNR_S BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_SORD-EDATU = LT_VBEP-EDATU. "Schedule line date
        LT_SORD-MBDAT = LT_VBEP-MBDAT. "Mat.avail.date
      ENDIF.

      IF LT_SORD-WBSTA EQ 'C'. "출고 완료된 경우
        CLEAR : LV_EBELN, LV_EBELP, LV_XBLNR.

        LV_EBELN = LT_SORD-BSTKD(10).
        LV_EBELP = LT_SORD-POSNR_S+1(5).
        CONCATENATE 'ZIVN' LT_SORD-VBELN_VL INTO LV_XBLNR.

*        CALL FUNCTION 'ME_READ_HISTORY'
*          EXPORTING
*            EBELN              = LV_EBELN
*            EBELP              = LV_EBELP
*            WEBRE              = ''
*            I_BYPASSING_BUFFER = 'X'
*            I_REFRESH_BUFFER   = 'X'
*          TABLES
*            XEKBE              = LT_EKBE.

        "HQ GR
        SELECT A~VGABE, A~CPUDT, A~CPUTM, A~SHKZG, A~MENGE, A~WRBTR,
               B~FRBNR, B~MBLNR, B~MJAHR
          INTO TABLE @DATA(LT_EKBE)
          FROM EKBE AS A INNER JOIN MKPF AS B
                                 ON A~BELNR EQ B~MBLNR
                                AND A~GJAHR EQ B~MJAHR
          WHERE A~EBELN EQ @LV_EBELN
            AND A~EBELP EQ @LV_EBELP
            AND A~VGABE EQ '1'.
*          AND B~FRBNR EQ @LT_SORD-EXTI1.

        "Invoice
        SELECT A~VGABE, A~CPUDT, A~CPUTM, A~SHKZG, A~MENGE, A~WRBTR
          INTO TABLE @DATA(LT_EKBE_I)
          FROM EKBE AS A INNER JOIN RBKP AS B
                                 ON A~BELNR EQ B~BELNR
                                AND A~GJAHR EQ B~GJAHR
          WHERE A~EBELN EQ @LV_EBELN
            AND A~EBELP EQ @LV_EBELP
            AND A~VGABE EQ '2'
            AND B~XBLNR EQ @LV_XBLNR. " ZIVN+delivery

        IF LT_EKBE[] IS NOT INITIAL.
          DATA: LT_HQGR LIKE LT_EKBE.
          DATA: LT_RESULT LIKE TABLE OF MSEG.
*{   INSERT         ZUQK901230                                        1
          "this point.
          SORT LT_EKBE BY CPUDT DESCENDING CPUTM DESCENDING. "날짜, 시간 역순 정렬 처리
          LOOP AT LT_EKBE INTO DATA(LS_EKBE) .
            LV_TABIX = SY-TABIX.
            CLEAR : LT_RESULT[].
            CALL FUNCTION 'MB_READ_MATERIAL_POSITION'
              EXPORTING
                MBLNR  = LS_EKBE-MBLNR
                MJAHR  = LS_EKBE-MJAHR
                TRTYP  = 'A'
              TABLES
                SEQTAB = LT_RESULT.

            READ TABLE LT_RESULT INTO DATA(LX_RESULT) INDEX 1.
            IF SY-SUBRC EQ 0 AND LX_RESULT-SMBLN IS NOT INITIAL.
              DELETE LT_EKBE WHERE MBLNR = LX_RESULT-SMBLN AND MJAHR = LX_RESULT-SJAHR .
              DELETE LT_EKBE INDEX LV_TABIX.
            ENDIF.
          ENDLOOP.
*}   INSERT

          "HQ G/R
          LT_HQGR[] = LT_EKBE[].
*          DELETE LT_HQGR WHERE VGABE NE '1'. "1 제외 다 삭제 처리
          SORT LT_HQGR BY CPUDT DESCENDING CPUTM DESCENDING. "날짜, 시간 역순 정렬 처리
*          READ TABLE LT_HQGR INTO DATA(LS_HQGR) WITH KEY FRBNR = LT_SORD-EXTI1.
*          IF SY-SUBRC EQ 0.
*            IF LS_HQGR-SHKZG = 'S'.
*              LT_SORD-MENGE_GR = LS_HQGR-MENGE.
*
*              IF LT_SORD-LFIMG NE LS_HQGR-MENGE. "딜리버리 수량과 다르면 ERROR
*                LT_SORD-RET_ICON = ICON_LED_RED.
*                LT_SORD-RET_MSG  = 'Different with HQ G/R'.
*              ENDIF.
*            ELSE.
*              LT_SORD-RET_ICON = ICON_LED_RED.
*              LT_SORD-RET_MSG  = 'Different with HQ G/R'.
*            ENDIF.
*          ENDIF.

          "2021/05/14 S E00035
          LOOP AT LT_HQGR INTO DATA(LS_HQGR) WHERE FRBNR = LT_SORD-EXTI1.
            CLEAR : LT_RESULT[].
            CALL FUNCTION 'MB_READ_MATERIAL_POSITION'
              EXPORTING
                MBLNR  = LS_HQGR-MBLNR
                MJAHR  = LS_HQGR-MJAHR
                TRTYP  = 'A'
              TABLES
                SEQTAB = LT_RESULT.

            READ TABLE LT_RESULT INTO DATA(LS_RESULT) INDEX 1.
            IF SY-SUBRC EQ 0.
              CLEAR : LV_FIRST, LV_SECOND, LV_VBELN_VL.
              SPLIT LS_RESULT-SGTXT AT '/' INTO LV_FIRST LV_SECOND LV_VBELN_VL.

              IF LV_VBELN_VL+4(10) = LT_SORD-VBELN_VL.
                "기존 로직 S
                IF LS_HQGR-SHKZG = 'S'.
                  LT_SORD-MENGE_GR = LS_HQGR-MENGE.

                  IF LT_SORD-LFIMG NE LS_HQGR-MENGE. "딜리버리 수량과 다르면 ERROR
                    LT_SORD-RET_ICON = ICON_LED_RED.
                    LT_SORD-RET_MSG  = 'Different with HQ G/R'.
                  ENDIF.
                ELSE.
                  LT_SORD-RET_ICON = ICON_LED_RED.
                  LT_SORD-RET_MSG  = 'Different with HQ G/R'.
                ENDIF.
                "기존 로직 E
                EXIT.
              ELSE.
                CONTINUE.
*                LT_SORD-RET_ICON = ICON_LED_RED.
*                LT_SORD-RET_MSG  = 'Different with HQ G/R'.
              ENDIF.
              CLEAR LS_RESULT.
            ENDIF.
          ENDLOOP.

          IF LT_SORD-LFIMG NE LT_SORD-MENGE_GR.
            LT_SORD-RET_ICON = ICON_LED_RED.
            LT_SORD-RET_MSG  = 'Different with HQ G/R'.
          ENDIF.
          "2021/05/14 E E00035
        ELSE.
          LT_SORD-RET_ICON = ICON_LED_RED.
          LT_SORD-RET_MSG  = 'Different with HQ G/R'.
        ENDIF.

        IF LT_EKBE_I[] IS NOT INITIAL.
          DATA: LT_HQIV LIKE LT_EKBE_I.

          " HQ Invoice
          LT_HQIV[] = LT_EKBE_I[].
*          DELETE LT_HQIV WHERE VGABE NE '2'. "2 제외 다 삭제 처리
          SORT LT_HQIV BY CPUDT DESCENDING CPUTM DESCENDING. "날짜, 시간 역순 정렬 처리

          READ TABLE LT_HQIV INTO DATA(LS_HQIV) INDEX 1.
          IF SY-SUBRC EQ 0.
            IF LS_HQIV-SHKZG = 'S'.
              LT_SORD-MENGE_IV = LS_HQIV-MENGE.

              IF LT_SORD-RET_ICON IS INITIAL AND LT_SORD-LFIMG NE LS_HQIV-MENGE. "딜리버리 수량과 다르면 ERROR
                LT_SORD-RET_ICON = ICON_LED_RED.
                LT_SORD-RET_MSG  = 'Different with HQ Invoice'.
              ENDIF.

              " Billing Doc 있는 경우
              IF LT_SORD-VBELN_B IS NOT INITIAL.
                LT_SORD-WRBTR = LS_HQIV-WRBTR.

                IF LT_SORD-RET_ICON IS INITIAL AND LT_SORD-RFWRT NE LS_HQIV-WRBTR. "빌링 금액과 다르면 ERROR
                  LT_SORD-RET_ICON = ICON_LED_RED.
                  LT_SORD-RET_MSG  = 'Different with HQ Invoice amount'.
                ENDIF.
              ENDIF.
            ELSE. " H 인 경우
              IF LT_SORD-RET_ICON IS INITIAL. "기존 에러메세지가 없는 경우
                LT_SORD-RET_ICON = ICON_LED_RED.
                LT_SORD-RET_MSG  = 'Different with HQ Invoice'.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSEIF LT_EKBE_I[] IS INITIAL AND LT_SORD-RET_ICON IS INITIAL.
          LT_SORD-RET_ICON = ICON_LED_RED.
          LT_SORD-RET_MSG  = 'Different with HQ Invoice'.
        ENDIF.
      ENDIF.

      MODIFY LT_SORD INDEX LV_INDEX.
      CLEAR : LT_SORD.
    ENDLOOP.

    SORT LT_SORD BY BSTKD_E MATNR_S POSNR_S.
*---
    LOOP AT GT_DATA.
      CLEAR: WA_SORD, LV_TABIX.
      READ TABLE LT_SORD INTO WA_SORD WITH KEY BSTKD_E = GT_DATA-BSTNK
                                               MATNR_S = GT_DATA-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_TABIX = SY-TABIX.
      ELSE.
        CLEAR LT_MAKT.
        READ TABLE LT_MAKT WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          GT_DATA-MAKTX = LT_MAKT-MAKTX.
        ENDIF.
        IF GT_DATA-GBSTK IS NOT INITIAL.
          CLEAR LT_WBSTA.
          READ TABLE LT_WBSTA WITH KEY DOMVALUE_L = GT_DATA-GBSTK BINARY SEARCH.
          IF SY-SUBRC = 0.
            GT_DATA-GBSTK_T = LT_WBSTA-DDTEXT.  "quot status
          ENDIF.
        ENDIF.
        APPEND GT_DATA TO GT_MAIN. CLEAR GT_DATA.
        CONTINUE.
      ENDIF.

      IF GT_DATA-BSTNK IS INITIAL.
        CLEAR LT_MAKT.
        READ TABLE LT_MAKT WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          GT_DATA-MAKTX = LT_MAKT-MAKTX.
        ENDIF.

        IF GT_DATA-GBSTK IS NOT INITIAL.
          CLEAR LT_WBSTA.
          READ TABLE LT_WBSTA WITH KEY DOMVALUE_L = GT_DATA-GBSTK BINARY SEARCH.
          IF SY-SUBRC = 0.
            GT_DATA-GBSTK_T = LT_WBSTA-DDTEXT.  "quot status
          ENDIF.
        ENDIF.
        APPEND GT_DATA TO GT_MAIN. CLEAR GT_DATA.
        CONTINUE.
      ENDIF.

      CLEAR LV_CNT.
      LOOP AT LT_SORD FROM LV_TABIX.
        LV_INDEX = SY-TABIX.
        LV_CNT = LV_CNT + 1.
        IF LT_SORD-BSTKD_E NE WA_SORD-BSTKD_E OR
           LT_SORD-MATNR_S NE WA_SORD-MATNR_S.
          EXIT.
        ENDIF.

        IF LV_CNT = '1'.
          CLEAR LT_MAKT.
          READ TABLE LT_MAKT WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GT_DATA-MAKTX = LT_MAKT-MAKTX.
          ENDIF.

          IF GT_DATA-GBSTK IS NOT INITIAL.
            CLEAR LT_WBSTA.
            READ TABLE LT_WBSTA WITH KEY DOMVALUE_L = GT_DATA-GBSTK BINARY SEARCH.
            IF SY-SUBRC = 0.
              GT_DATA-GBSTK_T = LT_WBSTA-DDTEXT.  "quot status
            ENDIF.
          ENDIF.
          GT_DATA-VBELN_S   = LT_SORD-VBELN_S.
          GT_DATA-VTWEG_S   = LT_SORD-VTWEG_S.
          GT_DATA-VKAUS     = LT_SORD-VKAUS.
          GT_DATA-KUNNR     = LT_SORD-KUNNR.
          GT_DATA-KUNNR_T   = LT_SORD-KUNNR_T.
          GT_DATA-KUNWE     = LT_SORD-KUNWE.
          GT_DATA-KUNWE_T   = LT_SORD-KUNWE_T.
          GT_DATA-ORGSOLDTO = LT_SORD-ORGSOLDTO.
          GT_DATA-ORGS_T    = LT_SORD-ORGS_T.
          GT_DATA-BSTKD_E   = LT_SORD-BSTKD_E.
          GT_DATA-EDATU     = LT_SORD-EDATU.
          GT_DATA-MBDAT     = LT_SORD-MBDAT.

          GT_DATA-IHREZ     = LT_SORD-IHREZ.
          GT_DATA-BSTKD     = LT_SORD-BSTKD.
          GT_DATA-POSNR_S   = LT_SORD-POSNR_S.
          GT_DATA-MATNR_S   = LT_SORD-MATNR_S.
          GT_DATA-MAKTX_S   = LT_SORD-MAKTX_S.
          GT_DATA-WERKS     = LT_SORD-WERKS.
          GT_DATA-LGORT     = LT_SORD-LGORT.
          GT_DATA-NETPR     = LT_SORD-NETPR.
          GT_DATA-KWMENG_S  = LT_SORD-KWMENG_S.
          GT_DATA-VRKME     = LT_SORD-VRKME.
          GT_DATA-NETPR     = LT_SORD-NETPR.
          GT_DATA-NETWR     = LT_SORD-NETWR.

          GT_DATA-WAERK     = LT_SORD-WAERK.
          GT_DATA-LFGSA     = LT_SORD-LFGSA.
          GT_DATA-LFGSA_T   = LT_SORD-LFGSA_T.
          GT_DATA-ABGRU     = LT_SORD-ABGRU.
          GT_DATA-ABGRU_T   = LT_SORD-ABGRU_T.

          GT_DATA-VBELN_VL  = LT_SORD-VBELN_VL.
          GT_DATA-POSNR_VL  = LT_SORD-POSNR_VL.
          GT_DATA-LFIMG     = LT_SORD-LFIMG.
          GT_DATA-WBSTA     = LT_SORD-WBSTA.
          GT_DATA-WBSTA_T   = LT_SORD-WBSTA_T.
          GT_DATA-WADAT     = LT_SORD-WADAT.
          GT_DATA-MBDAT_D   = LT_SORD-MBDAT_D.
          GT_DATA-LFDAT     = LT_SORD-LFDAT.
          GT_DATA-WADAT_IST = LT_SORD-WADAT_IST.

          GT_DATA-TKNUM     = LT_SORD-TKNUM.
          GT_DATA-EXTI1     = LT_SORD-EXTI1.
          GT_DATA-STTRG     = LT_SORD-STTRG .
          GT_DATA-STTRG_T   = LT_SORD-STTRG_T.
          GT_DATA-DTABF     = LT_SORD-DTABF.

          GT_DATA-MENGE_GR  = LT_SORD-MENGE_GR.
          GT_DATA-MENGE_IV  = LT_SORD-MENGE_IV.
          GT_DATA-VBELN_B   = LT_SORD-VBELN_B.
          GT_DATA-FKDAT     = LT_SORD-FKDAT.
          GT_DATA-POSNR_B   = LT_SORD-POSNR_B.

*S_2021/3/4 add BY E00064
          GT_DATA-RFWRT     = LT_SORD-RFWRT.    "Billing amount
          GT_DATA-VBELN_A   = LT_SORD-VBELN_A.  "Accounting Doc.
*E_2021/3/4 add BY E00064

          GT_DATA-WRBTR     = LT_SORD-WRBTR.

          GT_DATA-RET_ICON  = LT_SORD-RET_ICON.
          GT_DATA-RET_MSG   = LT_SORD-RET_MSG.

          APPEND GT_DATA TO GT_MAIN.
          DELETE LT_SORD INDEX LV_INDEX.
        ELSE.
          GT_DATA-KWMENG    = ' '.
          GT_DATA-BMENG     = ' '.

          GT_DATA-VBELN_S   = LT_SORD-VBELN_S.
          GT_DATA-KUNNR     = LT_SORD-KUNNR.
          GT_DATA-KUNNR_T   = LT_SORD-KUNNR_T.
          GT_DATA-KUNWE     = LT_SORD-KUNWE.
          GT_DATA-KUNWE_T   = LT_SORD-KUNWE_T.
          GT_DATA-ORGSOLDTO = LT_SORD-ORGSOLDTO.
          GT_DATA-ORGS_T    = LT_SORD-ORGS_T.
          GT_DATA-BSTKD_E   = LT_SORD-BSTKD_E.
          GT_DATA-EDATU     = LT_SORD-EDATU.
          GT_DATA-MBDAT     = LT_SORD-MBDAT.

          GT_DATA-IHREZ     = LT_SORD-IHREZ.
          GT_DATA-BSTKD     = LT_SORD-BSTKD.
          GT_DATA-POSNR_S   = LT_SORD-POSNR_S.
          GT_DATA-MATNR_S   = LT_SORD-MATNR_S.
          GT_DATA-MAKTX_S   = LT_SORD-MAKTX_S.
          GT_DATA-WERKS     = LT_SORD-WERKS.
          GT_DATA-LGORT     = LT_SORD-LGORT.
          GT_DATA-NETPR     = LT_SORD-NETPR.
          GT_DATA-KWMENG_S  = LT_SORD-KWMENG_S.
          GT_DATA-VRKME     = LT_SORD-VRKME.
          GT_DATA-NETPR     = LT_SORD-NETPR.
          GT_DATA-NETWR     = LT_SORD-NETWR.

          GT_DATA-WAERK     = LT_SORD-WAERK.
          GT_DATA-LFGSA     = LT_SORD-LFGSA.
          GT_DATA-LFGSA_T   = LT_SORD-LFGSA_T.
          GT_DATA-ABGRU     = LT_SORD-ABGRU.
          GT_DATA-ABGRU_T   = LT_SORD-ABGRU_T.

          GT_DATA-VBELN_VL  = LT_SORD-VBELN_VL.
          GT_DATA-POSNR_VL  = LT_SORD-POSNR_VL.
          GT_DATA-LFIMG     = LT_SORD-LFIMG.
          GT_DATA-WBSTA     = LT_SORD-WBSTA.
          GT_DATA-WBSTA_T   = LT_SORD-WBSTA_T.
          GT_DATA-WADAT     = LT_SORD-WADAT.
          GT_DATA-MBDAT_D   = LT_SORD-MBDAT_D.
          GT_DATA-LFDAT     = LT_SORD-LFDAT.
          GT_DATA-WADAT_IST = LT_SORD-WADAT_IST.

          GT_DATA-TKNUM     = LT_SORD-TKNUM.
          GT_DATA-EXTI1     = LT_SORD-EXTI1.
          GT_DATA-STTRG     = LT_SORD-STTRG .
          GT_DATA-STTRG_T   = LT_SORD-STTRG_T.
          GT_DATA-DTABF     = LT_SORD-DTABF.

          GT_DATA-MENGE_GR  = LT_SORD-MENGE_GR.
          GT_DATA-MENGE_IV  = LT_SORD-MENGE_IV.
          GT_DATA-VBELN_B   = LT_SORD-VBELN_B.
          GT_DATA-FKDAT     = LT_SORD-FKDAT.
          GT_DATA-POSNR_B   = LT_SORD-POSNR_B.

*S_2021/3/4 add BY E00064
          GT_DATA-RFWRT     = LT_SORD-RFWRT.    "Billing amount
          GT_DATA-VBELN_A   = LT_SORD-VBELN_A.  "Accounting Doc.
*E_2021/3/4 add BY E00064

          GT_DATA-WRBTR     = LT_SORD-WRBTR.

          GT_DATA-RET_ICON  = LT_SORD-RET_ICON.
          GT_DATA-RET_MSG   = LT_SORD-RET_MSG.

          APPEND GT_DATA TO GT_MAIN.
          DELETE LT_SORD INDEX LV_INDEX.
        ENDIF.
        CLEAR: LV_BSTNK, LV_UNIT.
        LV_BSTNK = GT_DATA-BSTNK.
        LV_UNIT = GT_DATA-VRKME_Q.
      ENDLOOP.

      AT END OF VBELN.
        CLEAR LV_TABIX.
        READ TABLE LT_SORD INTO WA_SORD WITH KEY BSTKD_E = LV_BSTNK BINARY SEARCH.
        IF SY-SUBRC = 0.
          LV_TABIX = SY-TABIX.
          LOOP AT LT_SORD FROM LV_TABIX.
            LV_INDEX = SY-TABIX.
            IF LT_SORD-BSTKD_E NE WA_SORD-BSTKD_E.
              EXIT.
            ENDIF.
            GT_DATA-BSTNK     = LV_BSTNK.
            GT_DATA-POSNR     = ' '.
            GT_DATA-MATNR     = ' '.
            GT_DATA-MAKTX     = ' '.

            GT_DATA-KWMENG    = ' '.
            GT_DATA-BMENG     = ' '.
            GT_DATA-VRKME_Q   = LV_UNIT.
            GT_DATA-GBSTK = ' '.
            GT_DATA-GBSTK_T = ' '.
            GT_DATA-VBELN_S   = LT_SORD-VBELN_S.
            GT_DATA-KUNNR     = LT_SORD-KUNNR.
            GT_DATA-KUNNR_T   = LT_SORD-KUNNR_T.
            GT_DATA-KUNWE     = LT_SORD-KUNWE.
            GT_DATA-KUNWE_T   = LT_SORD-KUNWE_T.
            GT_DATA-ORGSOLDTO = LT_SORD-ORGSOLDTO.
            GT_DATA-ORGS_T    = LT_SORD-ORGS_T.
            GT_DATA-BSTKD_E   = LT_SORD-BSTKD_E.
            GT_DATA-EDATU     = LT_SORD-EDATU.
            GT_DATA-MBDAT     = LT_SORD-MBDAT.

            GT_DATA-IHREZ     = LT_SORD-IHREZ.
            GT_DATA-BSTKD     = LT_SORD-BSTKD.
            GT_DATA-POSNR_S   = LT_SORD-POSNR_S.
            GT_DATA-MATNR_S   = LT_SORD-MATNR_S.
            GT_DATA-MAKTX_S   = LT_SORD-MAKTX_S.
            GT_DATA-WERKS     = LT_SORD-WERKS.
            GT_DATA-LGORT     = LT_SORD-LGORT.
            GT_DATA-NETPR     = LT_SORD-NETPR.
            GT_DATA-KWMENG_S  = LT_SORD-KWMENG_S.
            GT_DATA-VRKME     = LT_SORD-VRKME.
            GT_DATA-NETPR     = LT_SORD-NETPR.
            GT_DATA-NETWR     = LT_SORD-NETWR.

            GT_DATA-WAERK     = LT_SORD-WAERK.
            GT_DATA-LFGSA     = LT_SORD-LFGSA.
            GT_DATA-LFGSA_T   = LT_SORD-LFGSA_T.
            GT_DATA-ABGRU     = LT_SORD-ABGRU.
            GT_DATA-ABGRU_T   = LT_SORD-ABGRU_T.

            GT_DATA-VBELN_VL  = LT_SORD-VBELN_VL.
            GT_DATA-POSNR_VL  = LT_SORD-POSNR_VL.
            GT_DATA-LFIMG     = LT_SORD-LFIMG.
            GT_DATA-WBSTA     = LT_SORD-WBSTA.
            GT_DATA-WBSTA_T   = LT_SORD-WBSTA_T.
            GT_DATA-WADAT     = LT_SORD-WADAT.
            GT_DATA-MBDAT_D   = LT_SORD-MBDAT_D.
            GT_DATA-LFDAT     = LT_SORD-LFDAT.
            GT_DATA-WADAT_IST = LT_SORD-WADAT_IST.

            GT_DATA-TKNUM     = LT_SORD-TKNUM.
            GT_DATA-EXTI1     = LT_SORD-EXTI1.
            GT_DATA-STTRG     = LT_SORD-STTRG .
            GT_DATA-STTRG_T   = LT_SORD-STTRG_T.
            GT_DATA-DTABF     = LT_SORD-DTABF.

            GT_DATA-VBELN_B   = LT_SORD-VBELN_B.
            GT_DATA-FKDAT     = LT_SORD-FKDAT.
            GT_DATA-POSNR_B   = LT_SORD-POSNR_B.
            APPEND GT_DATA TO GT_MAIN.
            DELETE LT_SORD INDEX LV_INDEX.
          ENDLOOP.
        ENDIF.
      ENDAT.
    ENDLOOP.
  ENDIF.
*  IF r_all IS INITIAL.
*    PERFORM checkbox.
*  ENDIF.
ENDFORM.
**&---------------------------------------------------------------------*
**& Form MODIFY_SCREEN
**&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .
  IF R_ALL = C_X.
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'GR1'.
          SCREEN-ACTIVE = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

*S_2021/03/15 BY E00064
  IF R_ALLB = C_X.
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'GR2'.
          SCREEN-ACTIVE = 0.
        WHEN 'GR3'.
          IF R_ALLB IS INITIAL AND
           ( C_BOX2B IS NOT INITIAL OR C_BOX3B IS NOT INITIAL ).
            SCREEN-ACTIVE = 1.
          ELSE.
            SCREEN-ACTIVE = 0.
          ENDIF.

      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'GR3'.
          IF R_ALLB IS INITIAL AND
           ( C_BOX2B IS NOT INITIAL OR C_BOX3B IS NOT INITIAL ).
            SCREEN-ACTIVE = 1.
          ELSE.
            SCREEN-ACTIVE = 0.
          ENDIF.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
*E_2021/03/15 BY E00064

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_CREATE_DOCKING_CONTAINER
*&---------------------------------------------------------------------*
FORM ALV_CREATE_DOCKING_CONTAINER .
  CREATE OBJECT GR_DOCKING_CONTAINER
    EXPORTING
      EXTENSION                   = 3000
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  CREATE OBJECT GR_SPLITTER
    EXPORTING
      PARENT  = GR_DOCKING_CONTAINER
      ROWS    = 2
      COLUMNS = 1.
  CALL METHOD GR_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GR_PARENT_HTML.

  CALL METHOD GR_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 2
      COLUMN    = 1
    RECEIVING
      CONTAINER = GR_PARENT_GRID.

  CALL METHOD GR_SPLITTER->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 16.

ENDFORM.                    "ALV_CREATE_DOCKING_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  ALV_CREATE_GRID
*&---------------------------------------------------------------------*
FORM ALV_CREATE_GRID .
  CREATE OBJECT GR_ALV_GRID
    EXPORTING
      I_PARENT          = GR_PARENT_GRID
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

ENDFORM.                    "ALV_CREATE_GRID
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM ALV_FIELD_CATALOG .
  PERFORM APPEND_FIELD_CATALOG USING :
* FNAME         KEY      COLTEXT      EDIT     QFIELD   CFIELD  REF_TAB  REF_FIELD  NO_ZERO  NO_OUT
'VBELN'          'X'      TEXT-C01      ' '       ' '    ' '   'VBAK'    'VBELN'     ' '      ' ',
'POSNR'          'X'      TEXT-C02      ' '       ' '    ' '   'VBAP'    'POSNR'     ' '      ' ',
'MATNR'          'X'      TEXT-C03      ' '       ' '    ' '   'VBAP'    'MATNR'     ' '      ' ',
'MAKTX'          'X'      TEXT-C36      ' '       ' '    ' '   'MAKT'    'MAKTX'     ' '      ' ',
'VKAUS_Q'        'X'      TEXT-C51      ' '       ' '    ' '   'VBAP'    'VKAUS'     ' '      ' ',
'KWMENG'         'X'      TEXT-C04      ' '   'VRKME_Q'  ' '   'VBAP'    'KWMENG'    ' '      ' ',
'VRKME_Q'        'X'      TEXT-C37      ' '       ' '    ' '   'VBAP'    'VRKME'     ' '      ' ',
'VDATU'          'X'      TEXT-C50      ' '       ' '    ' '   'VBAK'    'VDATU'     ' '      ' ',
'EDATU_Q'        'X'      TEXT-C49      ' '       ' '    ' '   ''        ''          ' '      ' ',
'BMENG'          'X'      TEXT-C48      ' '   'VRKME_Q'  ' '   'VBEP'    'BMENG'     ' '      ' ',
'BSTNK'          'X'      TEXT-C05      ' '       ' '    ' '   'VBAK'    'BSTNK'     ' '      ' ',
'VTWEG'          'X'      TEXT-C47      ' '       ' '    ' '   'VBAK'    'VTWEG'     ' '      ' ',
'GBSTK'          'X'      TEXT-C43      ' '       ' '    ' '   'VBAK'    'GBSTK'     ' '      ' ',
'GBSTK_T'        ' '      TEXT-C44      ' '       ' '    ' '   'DD07V'   'DDTEXT'    ' '      ' ',

'VBELN_S'        ' '      TEXT-C06      ' '       ' '    ' '   'VBAK'    'VBELN'     ' '      ' ',
'VTWEG_S'        ' '      TEXT-C47      ' '       ' '    ' '   'VBAK'    'VTWEG'     ' '      ' ',
'POSNR_S'        ' '      TEXT-C13      ' '       ' '    ' '   'VBAP'    'POSNR'     ' '      ' ',
'MATNR_S'        ' '      TEXT-C14      ' '       ' '    ' '   'VBAP'    'MATNR'     ' '      ' ',
'MAKTX_S'        ' '      TEXT-C14      ' '       ' '    ' '   'MAKT'    'MAKTX'     ' '      ' ',
'VKAUS'          ' '      TEXT-C51      ' '       ' '    ' '   'VBAP'    'VKAUS'     ' '      ' ',
'KWMENG_S'       ' '      TEXT-C17      ' '     'VRKME'  ' '   'VBAP'    'KWMENG'    ' '      ' ',
'VRKME'          ' '      TEXT-C18      ' '       ' '    ' '   'VBAP'    'VRKME'     ' '      ' ',
'EDATU'          ' '      TEXT-C46      ' '       ' '    ' '   'VBEP'    'EDATU'     ' '      'X',
'MBDAT'          ' '      TEXT-C56      ' '       ' '    ' '   'VBEP'    'MBDAT'     ' '      ' ',
'NETPR'          ' '      TEXT-C40      ' '       ' '  'WAERK' 'VBAP'    'NETPR'     ' '      ' ',
'NETWR'          ' '      TEXT-C19      ' '       ' '  'WAERK' 'VBAP'    'NETWR'     ' '      ' ',
'WAERK'          ' '      TEXT-C20      ' '       ' '    ' '   'VBAP'    'WAERK'     ' '      ' ',
'WERKS'          ' '      TEXT-C15      ' '       ' '    ' '   'VBAP'    'WERKS'     ' '      ' ',
'KUNNR'          ' '      TEXT-C07      ' '       ' '    ' '   'VBAK'    'KUNNR'     ' '      ' ',
'KUNNR_T'        ' '      TEXT-C07      ' '       ' '    ' '   'KNA1'    'NAME1'     ' '      ' ',
'KUNWE'          ' '      TEXT-C08      ' '       ' '    ' '   'VBPA'    'KUNNR'     ' '      ' ',
'KUNWE_T'        ' '      TEXT-C08      ' '       ' '    ' '   'KNA1'    'NAME1'     ' '      ' ',
'ORGSOLDTO'      ' '      TEXT-C09      ' '       ' '    ' '   'VBPA'    'KUNNR'     ' '      ' ',
'ORGS_T'         ' '      TEXT-C09      ' '       ' '    ' '   'KNA1'    'NAME1'     ' '      ' ',
'BSTKD_E'        ' '      TEXT-C10      ' '       ' '    ' '   'VBKD'    'BSTKD_E'   ' '      ' ',
'IHREZ'          ' '      TEXT-C11      ' '       ' '    ' '   'VBKD'    'IHREZ'     ' '      ' ',
'BSTKD'          ' '      TEXT-C12      ' '       ' '    ' '   'VBKD'    'BSTKD'     ' '      ' ',
'LGORT'          ' '      TEXT-C16      ' '       ' '    ' '   'VBAP'    'LGORT'     ' '      ' ',
'LFGSA'          ' '      TEXT-C21      ' '       ' '    ' '   'VBAP'    'LFGSA'     ' '      ' ',
'LFGSA_T'        ' '      TEXT-C41      ' '       ' '    ' '   'DD07V'   'DDTEXT'    ' '      ' ',
'ABGRU'          ' '      TEXT-C22      ' '       ' '    ' '   'VBAP'    'ABGRU'     ' '      ' ',
'ABGRU_T'        ' '      TEXT-C42      ' '       ' '    ' '   'TVAGT'   'BEZEI'     ' '      ' ',
'VBELN_VL'       ' '      TEXT-C23      ' '       ' '    ' '   'LIPS'    'VBELN'     ' '      ' ',
'POSNR_VL'       ' '      TEXT-C24      ' '       ' '    ' '   'LIPS'    'POSNR'     ' '      ' ',
'LFIMG'          ' '      TEXT-C25      ' '     'VRKME'  ' '   'LIPS'    'LFIMG'     'X'      ' ',
'WBSTA'          ' '      TEXT-C37      ' '       ' '    ' '   'LIPS'    'WBSTA'     ' '      ' ',
'WBSTA_T'        ' '      TEXT-C26      ' '       ' '    ' '   'DD07V'   'DDTEXT'    ' '      ' ',
'WADAT'          ' '      TEXT-C57      ' '       ' '    ' '   'LIKP'    'WADAT'     ' '      ' ',
'MBDAT_D'        ' '      TEXT-C58      ' '       ' '    ' '   'LIPS'    'MBDAT'     ' '      ' ',
'LFDAT'          ' '      TEXT-C59      ' '       ' '    ' '   'LIKP'    'LFDAT'     ' '      ' ',
'WADAT_IST'      ' '      TEXT-C27      ' '       ' '    ' '   'LIKP'    'WADAT_IST' ' '      ' ',
'TKNUM'          ' '      TEXT-C28      ' '       ' '    ' '   'VTTK'    'TKNUM'     ' '      ' ',
'EXTI1'          ' '      TEXT-C55      ' '       ' '    ' '   'VTTK'    'EXTI1'     ' '      ' ',
'STTRG'          ' '      TEXT-C38      ' '       ' '    ' '   'VTTK'    'STTRG'     ' '      ' ',
'STTRG_T'        ' '      TEXT-C29      ' '       ' '    ' '   'DD07V'   'DDTEXT'    ' '      ' ',
'DTABF'          ' '      TEXT-C30      ' '       ' '    ' '   'VTTK'    'DTABF'     ' '      ' ',
'MENGE_GR'       ' '      TEXT-C52      ' '     'VRKME'  ' '   'EKBE'    'MENGE'     ' '      ' ',
'MENGE_IV'       ' '      TEXT-C53      ' '     'VRKME'  ' '   'EKBE'    'MENGE'     ' '      ' ',
'VBELN_B'        ' '      TEXT-C31      ' '       ' '    ' '   'VBRK'    'VBELN'     ' '      ' ',
'POSNR_B'        ' '      TEXT-C32      ' '       ' '    ' '   'VBRP'    'POSNR'     ' '      ' ',
'FKDAT'          ' '      TEXT-C33      ' '       ' '    ' '   'VBRK'    'FKDAT'     ' '      ' ',
*S_2021/3/4 add BY E00064
* Billing amount
'RFWRT'          ' '      TEXT-C34      ' '       ' '  'WAERS' 'VBFA'    'RFWRT'     ' ' ' ',
* Accounting Doc.
'VBELN_A'        ' '      TEXT-C35      ' '       ' '    ' '   'VBFA'    'VBELN'     ' ' ' ',

'WRBTR'          ' '      TEXT-C54      ' '       ' '  'WAERS' 'EKBE'    'WRBTR'     ' ' ' ',
* bdc result icon
'RET_ICON'       ' '      TEXT-C38      ' '       ' '    ' '   ''        ''          ' ' ' ',
* bdc result message
'RET_MSG'        ' '      TEXT-C39      ' '       ' '    ' '   ''        ''          ' ' ' '.
*E_2021/3/4 add BY E00064


ENDFORM.                    "ALV_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM APPEND_FIELD_CATALOG  USING P_FNAME
                                 P_KEY
                                 P_COLTEXT
                                 P_EDIT
                                 P_QFIELDNAME
                                 P_CFIELDNAME
                                 P_REF_TABLE
                                 P_REF_FIELD
                                 P_NO_ZERO
                                 P_NO_OUT.

  DATA: LS_FCAT TYPE LVC_S_FCAT.
  LS_FCAT-FIELDNAME  = P_FNAME.
  LS_FCAT-KEY        = P_KEY.
  LS_FCAT-COLTEXT    = P_COLTEXT.
  LS_FCAT-EDIT       = P_EDIT.
  LS_FCAT-QFIELDNAME = P_QFIELDNAME.
  LS_FCAT-CFIELDNAME = P_CFIELDNAME.
  LS_FCAT-REF_TABLE  = P_REF_TABLE.
  LS_FCAT-REF_FIELD  = P_REF_FIELD.
  LS_FCAT-NO_ZERO    = P_NO_ZERO.
  LS_FCAT-NO_OUT     = P_NO_OUT.

  APPEND LS_FCAT TO GT_FCAT.
ENDFORM.                    " APPEND_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
FORM ALV_LAYOUT USING PS_LAYOUT TYPE LVC_S_LAYO
                       P_OPT
                       P_ZEBRA
                       P_BOX
                       P_SEL_MODE
                       P_INFO_FNAME
                       P_TITLE
                       P_CTAB
                       P_STYLE.

  CLEAR  : PS_LAYOUT.
  PS_LAYOUT-CWIDTH_OPT = P_OPT.
  PS_LAYOUT-ZEBRA      = P_ZEBRA.
  PS_LAYOUT-BOX_FNAME  = P_BOX.
  PS_LAYOUT-SEL_MODE   = P_SEL_MODE.
  PS_LAYOUT-INFO_FNAME = P_INFO_FNAME.
  PS_LAYOUT-GRID_TITLE = P_TITLE.
  PS_LAYOUT-CTAB_FNAME = P_CTAB.
  PS_LAYOUT-STYLEFNAME = P_STYLE.

  GS_VARIANT-REPORT   = SY-REPID.
  GS_VARIANT-USERNAME = SY-UNAME.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV_EXCLUDE_TOOLBAR
*&---------------------------------------------------------------------*
FORM ALV_EXCLUDE_TOOLBAR .

  DEFINE _EXCLUDE_TOOLBAR.
    gs_excluding = cl_gui_alv_grid=>&1.
    APPEND gs_excluding TO gt_excluding.
  END-OF-DEFINITION.

  _EXCLUDE_TOOLBAR MC_FC_DETAIL.           " 세부사항
*  _EXCLUDE_TOOLBAR MC_FC_CHECK.             " 엔트리 점검
  _EXCLUDE_TOOLBAR MC_FC_REFRESH.           " 최신표시
*  _EXCLUDE_TOOLBAR MC_FC_LOC_CUT.           " 잘라내기
*  _EXCLUDE_TOOLBAR MC_FC_LOC_COPY.          " 텍스트복사
*  _EXCLUDE_TOOLBAR MC_FC_LOC_PASTE.         " 겹쳐쓰기로 삽입
*  _EXCLUDE_TOOLBAR MC_FC_LOC_PASTE_NEW_ROW. " 신규행에 삽입
*  _EXCLUDE_TOOLBAR MC_FC_LOC_UNDO.          " 실행취소
  _EXCLUDE_TOOLBAR MC_FC_LOC_APPEND_ROW.    " 행 추가
  _EXCLUDE_TOOLBAR MC_FC_LOC_INSERT_ROW.    " 행 삽입
  _EXCLUDE_TOOLBAR MC_FC_LOC_DELETE_ROW.    " 행 삭제
  _EXCLUDE_TOOLBAR MC_FC_LOC_COPY_ROW.      " 행 복제
*  _EXCLUDE_TOOLBAR MC_FC_SORT.              " SORT
*  _exclude_toolbar mc_fc_sort_asc.          " 오름차순 정렬
*  _exclude_toolbar mc_fc_sort_dsc.          " 내림차순 정렬
*  _exclude_toolbar mc_fc_find.              " 찾기
*  _exclude_toolbar mc_mb_filter.            " 필터설정
*  _EXCLUDE_TOOLBAR MC_FC_SUM.               " 총계
*  _EXCLUDE_TOOLBAR MC_MB_SUBTOT.            " SUBTOT
*  _EXCLUDE_TOOLBAR MC_FC_AVERAGE.           " 평균
*  _EXCLUDE_TOOLBAR MC_FC_MINIMUM.           " 최소
*  _EXCLUDE_TOOLBAR MC_FC_MAXIMUM.           " 최대
*  _EXCLUDE_TOOLBAR MC_FC_PRINT.            " 인쇄
*  _EXCLUDE_TOOLBAR MC_FC_VIEWS.            " 뷰
*  _EXCLUDE_TOOLBAR MC_FC_VIEWS_GRID.       " 뷰 - 그리드
*  _EXCLUDE_TOOLBAR MC_FC_VIEWS_EXCEL.      " 뷰 - 엑셀 통합
*  _EXCLUDE_TOOLBAR MC_FC_VIEWS_LOTUS.      " 뷰 - LOTUS
*  _EXCLUDE_TOOLBAR MC_MB_EXPORT.           " EXPORT
*  _EXCLUDE_TOOLBAR MC_PC_FILE.             " EXPORT - 로컬파일
*  _EXCLUDE_TOOLBAR MC_TO_OFFICE.           " EXPORT - OFFICE
*  _EXCLUDE_TOOLBAR MC_FC_GRAPH.             " 그래프 조회
*  _EXCLUDE_TOOLBAR MC_FC_INFO.              " 사용자 문서
*  _EXCLUDE_TOOLBAR MC_MB_VARIANT.          " 레이아웃 변경
*  _EXCLUDE_TOOLBAR MC_FC_EXCL_ALL.         " 툴바 전체 삭제

** 툴바 전부 삭제
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.


ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  ALV_EVENT
*&---------------------------------------------------------------------*
FORM ALV_EVENT .
  CALL METHOD GR_ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT GR_EVENT_HANDLER.
*  SET HANDLER gr_event_handler->handle_data_changed   FOR gr_alv_grid.
  SET HANDLER GR_EVENT_HANDLER->HANDLE_TOP_OF_PAGE    FOR GR_ALV_GRID.
*  SET HANDLER gr_event_handler->handle_toolbar        FOR gr_alv_grid.
*  SET HANDLER gr_event_handler->handle_user_command   FOR gr_alv_grid.
  SET HANDLER GR_EVENT_HANDLER->HANDLE_DOUBLE_CLICK   FOR GR_ALV_GRID.
*  SET HANDLER GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK  FOR GR_ALV_GRID.
*  SET HANDLER GR_EVENT_HANDLER->HANDLE_ON_F4          FOR GR_ALV_GRID.

ENDFORM.                    "ALV_EVENT
*&---------------------------------------------------------------------*
*&      Form  ALV_HTML
*&---------------------------------------------------------------------*
FORM ALV_HTML .
  CREATE OBJECT GR_HTML
    EXPORTING
      PARENT = GR_PARENT_HTML.

  CREATE OBJECT GR_DOCUMENT
    EXPORTING
      STYLE = 'ALV_GRID'.

  CALL METHOD GR_DOCUMENT->INITIALIZE_DOCUMENT.
  CALL METHOD GR_ALV_GRID->LIST_PROCESSING_EVENTS
    EXPORTING
      I_EVENT_NAME = 'TOP_OF_PAGE'
      I_DYNDOC_ID  = GR_DOCUMENT.

  GR_DOCUMENT->HTML_CONTROL = GR_HTML.

  CALL METHOD GR_DOCUMENT->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = C_X
      PARENT             = GR_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.
ENDFORM.                    " ALV_HTML
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM ALV_DISPLAY .

  CALL METHOD GR_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     I_BUFFER_ACTIVE               =
*     I_BYPASSING_BUFFER            =
*     I_CONSISTENCY_CHECK           =
*     I_STRUCTURE_NAME              =
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = C_X
      I_DEFAULT                     = C_X
      IS_LAYOUT                     = GS_LAYOUT
*     IS_PRINT                      =
*     IT_SPECIAL_GROUPS             =
      IT_TOOLBAR_EXCLUDING          = GT_EXCLUDING
*     IT_HYPERLINK                  =
*     IT_ALV_GRAPHICS               =
*     IT_EXCEPT_QINFO               =
*     IR_SALV_ADAPTER               =
    CHANGING
      IT_OUTTAB                     = GT_MAIN[]
      IT_FIELDCATALOG               = GT_FCAT
*     IT_SORT                       = GT_SORT.
*     IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_REFRESH
*&---------------------------------------------------------------------*
FORM ALV_REFRESH .
  DATA: LS_STABLE TYPE LVC_S_STBL.
  LS_STABLE-ROW = C_X.
  LS_STABLE-COL = C_X.
  CALL METHOD GR_ALV_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = C_X.
ENDFORM.                    " ALV_REFRESH
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HANDLE_TOP_OF_PAGE USING CL_DD TYPE REF TO CL_DD_DOCUMENT.
  DATA: L_TEXT(255)  TYPE C.
*        l_text1(255) TYPE c,
*        l_name1      TYPE t001w-name1.

  IF S_BSTNK-LOW IS NOT INITIAL AND S_BSTNK-HIGH IS INITIAL.
    CONCATENATE TEXT-T08 S_BSTNK-LOW INTO L_TEXT SEPARATED BY SPACE. "'Customer PO# : '
  ELSEIF S_BSTNK-LOW IS NOT INITIAL AND S_BSTNK-HIGH IS NOT INITIAL.
    CONCATENATE TEXT-T08  S_VTWEG-LOW '~' S_BSTNK-HIGH INTO L_TEXT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TEXT-T08 'All' INTO L_TEXT SEPARATED BY SPACE.
  ENDIF.

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  CONCATENATE TEXT-T01 P_VKORG INTO L_TEXT SEPARATED BY SPACE. "Sales org :

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  IF S_VTWEG-LOW IS NOT INITIAL AND S_VTWEG-HIGH IS INITIAL.
    CONCATENATE TEXT-T02 S_VTWEG-LOW INTO L_TEXT SEPARATED BY SPACE. "'Dist. Channel : '
  ELSEIF S_VTWEG-LOW IS NOT INITIAL AND S_VTWEG-HIGH IS NOT INITIAL.
    CONCATENATE TEXT-T02  S_VTWEG-LOW '~' S_VTWEG-HIGH INTO L_TEXT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TEXT-T02 'All' INTO L_TEXT SEPARATED BY SPACE.
  ENDIF.

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  CONCATENATE TEXT-T03 P_SPART INTO L_TEXT SEPARATED BY SPACE. "Division :

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  CONCATENATE TEXT-T04 P_AUART INTO L_TEXT SEPARATED BY SPACE. "Sales doc. type :

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  IF S_MATNR-LOW IS NOT INITIAL AND S_MATNR-HIGH IS INITIAL.
    CONCATENATE TEXT-T05 S_MATNR-LOW INTO L_TEXT SEPARATED BY SPACE. "Material : '
  ELSEIF S_MATNR-LOW IS NOT INITIAL AND S_MATNR-HIGH IS NOT INITIAL.
    CONCATENATE TEXT-T05  S_MATNR-LOW '~' S_MATNR-HIGH INTO L_TEXT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TEXT-T05 'All' INTO L_TEXT SEPARATED BY SPACE.
  ENDIF.

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  IF S_KUNNR-LOW IS NOT INITIAL AND S_KUNNR-HIGH IS INITIAL.
    CONCATENATE TEXT-T06 S_KUNNR-LOW INTO L_TEXT SEPARATED BY SPACE. "Ship to party : '
  ELSEIF S_KUNNR-LOW IS NOT INITIAL AND S_KUNNR-HIGH IS NOT INITIAL.
    CONCATENATE TEXT-T06  S_KUNNR-LOW '~' S_KUNNR-HIGH INTO L_TEXT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TEXT-T06 'All' INTO L_TEXT SEPARATED BY SPACE.
  ENDIF.

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  IF S_ERDAT-LOW IS NOT INITIAL AND S_ERDAT-HIGH IS INITIAL.
    CONCATENATE TEXT-T07 S_ERDAT-LOW INTO L_TEXT SEPARATED BY SPACE. "Create on : '
  ELSEIF S_ERDAT-LOW IS NOT INITIAL AND S_ERDAT-HIGH IS NOT INITIAL.
    CONCATENATE TEXT-T07  S_ERDAT-LOW '~' S_ERDAT-HIGH INTO L_TEXT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TEXT-T07 'All' INTO L_TEXT SEPARATED BY SPACE.
  ENDIF.

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

ENDFORM.                    " HANDLE_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& Form CHECKBOX
*&---------------------------------------------------------------------*
FORM CHECKBOX .

  RANGES R_LFGSA FOR VBAP-LFGSA.


  IF C_BOX1 IS NOT INITIAL AND C_BOX2 IS INITIAL
                           AND C_BOX3 IS INITIAL.
*                           AND c_box4 IS INITIAL.
    DELETE GT_MAIN WHERE LFGSA NE 'A'.
  ENDIF.

  IF C_BOX2 IS NOT INITIAL AND C_BOX1 IS INITIAL
                           AND C_BOX3 IS INITIAL.
*                           AND c_box4 IS INITIAL.
    DELETE GT_MAIN WHERE LFGSA NE 'B'.
  ENDIF.

  IF C_BOX3 IS NOT INITIAL AND C_BOX1 IS INITIAL
                           AND C_BOX2 IS INITIAL.
*                           AND c_box4 IS INITIAL.
    DELETE GT_MAIN WHERE LFGSA NE 'C'.
  ENDIF.

*  IF c_box4 IS NOT INITIAL AND c_box1 IS INITIAL
*                           AND c_box2 IS INITIAL
*                           AND c_box3 IS INITIAL.
*    DELETE gt_main WHERE lfgsa NE ' '.
*  ENDIF.

  IF C_BOX1 IS NOT INITIAL AND C_BOX2 IS NOT INITIAL
                           AND C_BOX3 IS INITIAL.
    R_LFGSA-LOW = 'A'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    R_LFGSA-LOW = 'B'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    DELETE GT_MAIN WHERE LFGSA NOT IN R_LFGSA.
    DELETE GT_MAIN WHERE LFGSA IS INITIAL.
  ENDIF.

  IF C_BOX1 IS NOT INITIAL AND C_BOX2 IS INITIAL
                           AND C_BOX3 IS NOT INITIAL.
*                           AND c_box4 IS INITIAL.
    R_LFGSA-LOW = 'A'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    R_LFGSA-LOW = 'C'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    DELETE GT_MAIN WHERE LFGSA NOT IN R_LFGSA.
    DELETE GT_MAIN WHERE LFGSA IS INITIAL.
  ENDIF.

  IF C_BOX1 IS INITIAL AND C_BOX2 IS NOT INITIAL
                           AND C_BOX3 IS NOT INITIAL.
    R_LFGSA-LOW = 'B'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    R_LFGSA-LOW = 'C'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    DELETE GT_MAIN WHERE LFGSA NOT IN R_LFGSA.
    DELETE GT_MAIN WHERE LFGSA IS INITIAL.
  ENDIF.

*  IF c_box1 IS INITIAL AND c_box2 IS NOT INITIAL
*                       AND c_box3 IS NOT INITIAL.
**                       AND c_box4 IS INITIAL.
*    r_lfgsa-low = 'B'.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    r_lfgsa-low = 'C'.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    DELETE gt_main WHERE lfgsa NOT IN r_lfgsa.
*  ENDIF.

*  IF c_box1 IS INITIAL AND c_box2 IS NOT INITIAL
*                       AND c_box3 IS INITIAL
*                       AND c_box4 IS NOT INITIAL.
*    r_lfgsa-low = 'B'.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    r_lfgsa-low = ' '.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    DELETE gt_main WHERE lfgsa NOT IN r_lfgsa.
*  ENDIF.


  IF C_BOX1 IS NOT INITIAL AND C_BOX2 IS NOT INITIAL
                           AND C_BOX3 IS NOT INITIAL.
*                           AND c_box4 IS INITIAL.
    R_LFGSA-LOW = 'A'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    R_LFGSA-LOW = 'B'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    R_LFGSA-LOW = 'C'.
    R_LFGSA-SIGN = 'I'.
    R_LFGSA-OPTION = 'EQ'.
    APPEND R_LFGSA. CLEAR R_LFGSA.
    DELETE GT_MAIN WHERE LFGSA NOT IN R_LFGSA.
*    DELETE gt_main WHERE lfgsa is INITIAL.
  ENDIF.

*  IF c_box1 IS NOT INITIAL AND c_box2 IS NOT INITIAL
*                           AND c_box3 IS INITIAL.
**                           AND c_box4 IS NOT INITIAL.
*    r_lfgsa-low = 'A'.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    r_lfgsa-low = 'B'.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    r_lfgsa-low = ' '.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    DELETE gt_main WHERE lfgsa NOT IN r_lfgsa.
*  ENDIF.
*
*  IF c_box1 IS INITIAL AND c_box2 IS NOT INITIAL
*                       AND c_box3 IS NOT INITIAL.
**                       AND c_box4 IS NOT INITIAL.
*    r_lfgsa-low = 'B'.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    r_lfgsa-low = 'C'.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    r_lfgsa-low = ' '.
*    r_lfgsa-sign = 'I'.
*    r_lfgsa-option = 'EQ'.
*    APPEND r_lfgsa. CLEAR r_lfgsa.
*    DELETE gt_main WHERE lfgsa NOT IN r_lfgsa.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECKBOX
*&---------------------------------------------------------------------*
FORM CHECKBOX2 .

  IF C_BOX1B IS INITIAL AND
     C_BOX2B IS INITIAL AND
     C_BOX3B IS INITIAL .
    EXIT.
  ENDIF.

* Delete GI not completed : WBSTA NE 'C' .
  DELETE GT_MAIN WHERE WBSTA NE 'C' .

** check billing date .
*  DELETE GT_MAIN WHERE FKDAT NOT IN s_bidat.

* Delete GI not completed and no billing doc .
  LOOP AT GT_MAIN.
    IF C_BOX1B IS INITIAL.
      IF GT_MAIN-VBELN_B IS INITIAL.
        DELETE GT_MAIN. CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Delete billed and accounting doc not released .
  LOOP AT GT_MAIN.
*-- Delete billed and accounting doc not released .
    IF C_BOX2B IS INITIAL.
      IF GT_MAIN-VBELN_B IS NOT INITIAL AND
*         GT_MAIN-FKDAT IN s_bidat AND
         GT_MAIN-VBELN_A IS INITIAL.
        DELETE GT_MAIN. CONTINUE.
      ENDIF.
    ELSE.
      IF GT_MAIN-VBELN_B IS NOT INITIAL AND
         GT_MAIN-FKDAT NOT IN S_BIDAT AND
         GT_MAIN-VBELN_A IS INITIAL.
        DELETE GT_MAIN. CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Delete billed and accounting doc released .
  LOOP AT GT_MAIN.
*-- Delete billed and accounting doc released .
    IF C_BOX3B IS INITIAL.
      IF GT_MAIN-VBELN_B IS NOT INITIAL AND
*         GT_MAIN-FKDAT IN s_bidat AND
         GT_MAIN-VBELN_A IS NOT INITIAL.
        DELETE GT_MAIN. CONTINUE.
      ENDIF.
    ELSE.
      IF GT_MAIN-VBELN_B IS NOT INITIAL AND
         GT_MAIN-FKDAT NOT IN S_BIDAT AND
         GT_MAIN-VBELN_A IS NOT INITIAL.
        DELETE GT_MAIN. CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  IF C_BOX1B IS INITIAL.
*    DELETE GT_MAIN WHERE VBELN_B IS INITIAL.
*  ELSE.
*    IF C_BOX2B IS INITIAL.
*      DELETE GT_MAIN WHERE VBELN_B IS NOT INITIAL
*                       AND VBELN_A IS INITIAL.
*    ELSE.
*      DELETE GT_MAIN WHERE VBELN_B IS NOT INITIAL
*                       AND FKDAT NOT IN s_bidat.
*    ENDIF.
*
*    IF C_BOX3B IS INITIAL.
*      DELETE GT_MAIN WHERE VBELN_B IS NOT INITIAL
*                       AND VBELN_A IS NOT INITIAL.
*    ELSE.
*      DELETE GT_MAIN WHERE VBELN_B IS NOT INITIAL
*                       AND FKDAT NOT IN s_bidat.
*    ENDIF.
*
*  ENDIF.
*
*  IF C_BOX2B IS INITIAL.
*    DELETE GT_MAIN WHERE VBELN_B IS NOT INITIAL
*                     AND VBELN_A IS INITIAL.
*  ELSE.
*    DELETE GT_MAIN WHERE VBELN_B IS NOT INITIAL
*                     AND FKDAT NOT IN s_bidat.
*  ENDIF.
*
*  IF C_BOX3B IS INITIAL.
*    DELETE GT_MAIN WHERE VBELN_B IS NOT INITIAL
*                     AND VBELN_A IS NOT INITIAL.
*  ELSE.
*    DELETE GT_MAIN WHERE VBELN_B IS NOT INITIAL
*                     AND FKDAT NOT IN s_bidat.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    P_ROW TYPE LVC_S_ROW
                                   P_COLUMN TYPE LVC_S_COL.
  DATA: LS_MAIN    LIKE GT_MAIN,
        LV_TKNUM   TYPE VTTK-TKNUM,
        LV_VBELN_B TYPE VBRK-VBELN.
  CLEAR :LS_MAIN.
  READ TABLE GT_MAIN INTO LS_MAIN INDEX P_ROW.
  CASE P_COLUMN.
    WHEN 'VBELN'.
      IF LS_MAIN-VBELN IS NOT INITIAL AND LS_MAIN-GBSTK = 'C'.
        SET PARAMETER ID 'AGN' FIELD LS_MAIN-VBELN.
        CALL TRANSACTION 'VA23' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'AGN'.
      ELSEIF LS_MAIN-VBELN IS NOT INITIAL AND LS_MAIN-GBSTK = 'A'.
        SET PARAMETER ID 'AGN' FIELD LS_MAIN-VBELN.
        CALL TRANSACTION 'VA22' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'AGN'.
      ENDIF.
    WHEN 'VBELN_S'.
      IF LS_MAIN-VBELN_S IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD LS_MAIN-VBELN_S.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'AUN'.
      ENDIF.
    WHEN 'VBELN_VL'.
      IF LS_MAIN-VBELN_VL IS NOT INITIAL.

        SET PARAMETER ID 'VL' FIELD LS_MAIN-VBELN_VL.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'VL'.
      ENDIF.
    WHEN 'TKNUM'.
      IF LS_MAIN-TKNUM IS NOT INITIAL.

        LV_TKNUM = LS_MAIN-TKNUM.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LV_TKNUM
          IMPORTING
            OUTPUT = LV_TKNUM.

        SET PARAMETER ID 'TNR' FIELD LV_TKNUM.
        CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'TNR'.
      ENDIF.
    WHEN 'VBELN_B'.
      IF LS_MAIN-VBELN_B IS NOT INITIAL.

        LV_VBELN_B = LS_MAIN-VBELN_B.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LV_VBELN_B
          IMPORTING
            OUTPUT = LV_VBELN_B.

        SET PARAMETER ID 'VF' FIELD LV_VBELN_B.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'VF'.
      ENDIF.
    WHEN 'BSTKD'. "HQ PO
      IF LS_MAIN-BSTKD IS NOT INITIAL.

        SET PARAMETER ID 'BES' FIELD LS_MAIN-BSTKD.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'BES'.
      ENDIF.

*S_2021/3/5 added BY E00064
    WHEN 'VBELN_A'.
      IF LS_MAIN-VBELN_S IS NOT INITIAL.
*        LS_MAIN-BUKRS = ''.
*        LS_MAIN-GJAHR = ''.
        SET PARAMETER ID: 'BLN' FIELD LS_MAIN-VBELN_A.
*                          'BUK' FIELD LS_MAIN-BUKRS,
*                          'GJR' FIELD LS_MAIN-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'BLN'.
      ENDIF.
*E_2021/3/5 added BY E00064

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA2 .
  DATA: LT_SORD    LIKE TABLE OF GT_DATA WITH HEADER LINE,
        WA_SORD    LIKE GT_DATA,
        LT_COMWA   LIKE TABLE OF VBCO6 WITH HEADER LINE,
        LT_VBFA    LIKE TABLE OF VBFAS WITH HEADER LINE,
        LT_VBFAB   LIKE TABLE OF VBFAS WITH HEADER LINE,
*S_2021/3/4 add BY E00064
        LT_VBFAB_A LIKE TABLE OF VBFAS WITH HEADER LINE,
*E_2021/3/4 add BY E00064
        LV_INDEX   TYPE SY-TABIX,
        LV_TABIX   TYPE SY-TABIX,
        LT_MAKT_S  LIKE TABLE OF MAKT WITH HEADER LINE,
        LV_BSTNK   LIKE VBAK-BSTNK,
        LV_CNT     TYPE I,
        LV_UNIT    TYPE VBAP-VRKME.

  DATA: LT_STTRG TYPE TABLE OF DD07V WITH HEADER LINE,
        LT_WBSTA TYPE TABLE OF DD07V WITH HEADER LINE,
        LT_KNA1  TYPE TABLE OF KNA1  WITH HEADER LINE,
        LT_KUNWE TYPE TABLE OF KNA1  WITH HEADER LINE,
        LT_ORGS  TYPE TABLE OF KNA1 WITH HEADER LINE,
        LT_ABGRU TYPE TABLE OF TVAGT WITH HEADER LINE,
        LT_VTTK  TYPE TABLE OF VTTK  WITH HEADER LINE,
        LT_VBRK  TYPE TABLE OF VBRK  WITH HEADER LINE,
        LT_VBEP  TYPE TABLE OF VBEP  WITH HEADER LINE.

  DATA: LV_FIRST        TYPE STRING,
        LV_SECOND       TYPE STRING,
        LV_VBELN_VL(14).

  RANGES:  R_VBTYP FOR VBFAS-VBTYP_N. "flow
  R_VBTYP-LOW = 'N'.
  R_VBTYP-SIGN = 'I'.
  R_VBTYP-OPTION = 'EQ'.
  APPEND R_VBTYP. CLEAR R_VBTYP.

  R_VBTYP-LOW = 'M'.
  R_VBTYP-SIGN = 'I'.
  R_VBTYP-OPTION = 'EQ'.
  APPEND R_VBTYP. CLEAR R_VBTYP.

  "sales order
  SELECT    A~VBELN AS VBELN_S,   "S/O #
            A~KUNNR,              "Sold-to party
            A~VTWEG AS VTWEG_S,   "Distr. Channel
            B~KUNNR AS KUNWE,     "Ship-to party
            C~KUNNR AS ORGSOLDTO, "org-sold to
            K~BSTKD_E,            "AZ PO#
            K~IHREZ,              "HQ PO
            K~BSTKD,              "AZ PO#
            P~POSNR AS POSNR_S,   "Item #
            P~MATNR AS MATNR_S,   "Material
            P~WERKS,              "Plant
            P~LGORT,              "Storage location
            P~NETPR,              "Net Price
            P~KWMENG AS KWMENG_S, "Qty
            P~VRKME,              "Sales Unit
            P~NETWR,              "Net Value
            P~WAERK,              "Currency
            P~LFGSA,              "Status
            P~ABGRU,              "Reason for Rejection
            P~VKAUS,              "Usage Indicator
            L~VBELN AS VBELN_VL,  "Delivery
            L~POSNR AS POSNR_VL,  "Delivery Item
            I~WADAT_IST,          "Qty delivered
            L~LFIMG,              "Movement Status
            L~WBSTA,               "Actual goods movement date
            I~WADAT,              "Plan GI date
            L~MBDAT AS MBDAT_D,   "Mat.avail.date
            I~LFDAT               "Delivery date
     INTO CORRESPONDING FIELDS OF TABLE @LT_SORD
     FROM VBAK AS A INNER JOIN VBPA AS B ON A~VBELN = B~VBELN
                                        AND B~POSNR = '00000'
                                        AND B~PARVW = 'WE'
               LEFT OUTER JOIN VBPA AS C ON A~VBELN = C~VBELN
                                        AND C~POSNR = '00000'
                                        AND C~PARVW = 'SX'
                    INNER JOIN VBKD AS K ON A~VBELN = K~VBELN
                                        AND K~POSNR = '00000'
                    INNER JOIN VBAP AS P ON A~VBELN = P~VBELN
               LEFT OUTER JOIN LIPS AS L ON P~VBELN = L~VGBEL
                                        AND P~POSNR = L~VGPOS
               LEFT OUTER JOIN LIKP AS I ON I~VBELN = L~VBELN
     WHERE A~AUART EQ @P_AUART
       AND P~MATNR IN @S_MATNR
       AND A~VKORG EQ @P_VKORG
       AND A~VTWEG IN @S_VTWEG
       AND A~SPART EQ @P_SPART
       AND B~KUNNR IN @S_KUNNR
       AND ( K~BSTKD_E NE ''
       AND   K~BSTKD_E IN @S_BSTNK )
       AND K~BSTKD_E NOT IN ( SELECT X~BSTKD FROM VBKD AS X INNER JOIN VBAK AS Y ON X~VBELN = Y~VBELN AND X~POSNR = '00000' AND Y~AUART = 'ZQT'
                                                                 WHERE Y~ERDAT IN @S_ERDAT ).


  IF LT_SORD[] IS NOT INITIAL.
    SELECT MATNR MAKTX FROM MAKT INTO CORRESPONDING FIELDS OF TABLE LT_MAKT_S
      FOR ALL ENTRIES IN LT_SORD WHERE MATNR = LT_SORD-MATNR_S
                                   AND SPRAS = SY-LANGU.
    SORT LT_MAKT_S BY MATNR.

    "soldto
    SELECT KUNNR NAME1 FROM KNA1 INTO CORRESPONDING FIELDS OF TABLE LT_KNA1
      FOR ALL ENTRIES IN LT_SORD WHERE KUNNR = LT_SORD-KUNNR
                                   AND SPRAS = SY-LANGU.
    SORT LT_KNA1 BY KUNNR.

    "shipto
    SELECT KUNNR NAME1 FROM KNA1 INTO CORRESPONDING FIELDS OF TABLE LT_KUNWE
      FOR ALL ENTRIES IN LT_SORD WHERE KUNNR = LT_SORD-KUNWE
                                   AND SPRAS = SY-LANGU.
    SORT LT_KUNWE BY KUNNR.

    "orgsoldto
    SELECT KUNNR NAME1 FROM KNA1 INTO CORRESPONDING FIELDS OF TABLE LT_ORGS
      FOR ALL ENTRIES IN LT_SORD WHERE KUNNR = LT_SORD-ORGSOLDTO
                                   AND SPRAS = SY-LANGU.
    SORT LT_ORGS BY KUNNR.

    "rj
    SELECT ABGRU BEZEI FROM TVAGT INTO CORRESPONDING FIELDS OF TABLE LT_ABGRU
      FOR ALL ENTRIES IN LT_SORD WHERE ABGRU = LT_SORD-ABGRU
                                   AND SPRAS = SY-LANGU.
    SORT LT_ABGRU BY ABGRU.

    "Schedule line date
    SELECT VBELN POSNR EDATU MBDAT FROM VBEP INTO CORRESPONDING FIELDS OF TABLE LT_VBEP
      FOR ALL ENTRIES IN LT_SORD WHERE VBELN = LT_SORD-VBELN_S
                                   AND POSNR = LT_SORD-POSNR_S.
    SORT LT_VBEP BY VBELN POSNR.
  ENDIF.

  CALL FUNCTION 'DD_DD07V_GET'
    EXPORTING
      DOMAIN_NAME    = 'STTRG'
      LANGU          = SY-LANGU
      WITHTEXT       = 'X'
    TABLES
      DD07V_TAB      = LT_STTRG
    EXCEPTIONS
      ACCESS_FAILURE = 1
      OTHERS         = 2.
  SORT LT_STTRG BY DOMVALUE_L.

  CALL FUNCTION 'DD_DD07V_GET'
    EXPORTING
      DOMAIN_NAME    = 'STATV'
      LANGU          = SY-LANGU
      WITHTEXT       = 'X'
    TABLES
      DD07V_TAB      = LT_WBSTA
    EXCEPTIONS
      ACCESS_FAILURE = 1
      OTHERS         = 2.
  SORT LT_WBSTA BY DOMVALUE_L.


  SORT LT_SORD BY VBELN_S POSNR_S.

  LOOP AT LT_SORD.
    LV_INDEX = SY-TABIX.

    AT NEW VBELN_S.

      CLEAR: LT_COMWA, LT_COMWA[], LT_VBFA, LT_VBFA[], LT_VBFAB, LT_VBFAB[].

      LT_COMWA-VBELN = LT_SORD-VBELN_S.
      APPEND LT_COMWA.

      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
        EXPORTING
          COMWA         = LT_COMWA
        TABLES
          VBFA_TAB      = LT_VBFA
        EXCEPTIONS
          NO_VBFA       = 1
          NO_VBUK_FOUND = 2
          OTHERS        = 3.

      LT_VBFAB[] = LT_VBFA[].
*S_2021/3/4 add BY E00064
      LT_VBFAB_A[] = LT_VBFA[].
*E_2021/3/4 add BY E00064

      DELETE LT_VBFA WHERE VBTYP_N NE '8'.
      SORT LT_VBFA BY VBELV.

      IF LT_VBFA[] IS NOT INITIAL.
        CLEAR : LT_VTTK, LT_VTTK[].
        SELECT TKNUM STTRG DTABF EXTI1 FROM VTTK INTO CORRESPONDING FIELDS OF TABLE LT_VTTK "Shipment
          FOR ALL ENTRIES IN LT_VBFA
          WHERE TKNUM EQ LT_VBFA-VBELN.
        SORT LT_VTTK BY TKNUM.
      ENDIF.

      DELETE LT_VBFAB WHERE VBTYP_N NOT IN R_VBTYP. " M , N
      SORT LT_VBFAB BY VBELV POSNV ERDAT DESCENDING ERZET DESCENDING.

      IF LT_VBFAB[] IS NOT INITIAL.
        CLEAR : LT_VBRK, LT_VBRK[].
        SELECT VBELN FKDAT FROM VBRK INTO CORRESPONDING FIELDS OF TABLE LT_VBRK "Billing
          FOR ALL ENTRIES IN LT_VBFAB
          WHERE VBELN EQ LT_VBFAB-VBELN.
        SORT LT_VBRK BY VBELN.
      ENDIF.
    ENDAT.

    IF LT_VBFA[] IS NOT INITIAL.

      CLEAR LT_VBFA.
      READ TABLE LT_VBFA WITH KEY VBELV = LT_SORD-VBELN_VL BINARY SEARCH.

      IF SY-SUBRC = 0.
        LT_SORD-TKNUM = LT_VBFA-VBELN.  "Shipment

        CLEAR LT_VTTK.
        READ TABLE LT_VTTK WITH KEY TKNUM = LT_VBFA-VBELN BINARY SEARCH.
        IF SY-SUBRC = 0.
          LT_SORD-STTRG = LT_VTTK-STTRG. "shmt status
          LT_SORD-DTABF = LT_VTTK-DTABF. "compl date
          LT_SORD-EXTI1 = LT_VTTK-EXTI1. "BL No.
        ENDIF.

        CLEAR LT_STTRG.
        READ TABLE LT_STTRG WITH KEY DOMVALUE_L = LT_SORD-STTRG.
        IF SY-SUBRC = 0.
          LT_SORD-STTRG_T = LT_STTRG-DDTEXT. "shmt sts text
        ENDIF.
      ENDIF.
    ENDIF.

    IF LT_VBFAB[] IS NOT INITIAL.
      CLEAR LT_VBFAB.
      READ TABLE LT_VBFAB WITH KEY VBELV = LT_SORD-VBELN_VL
                                   POSNV = LT_SORD-POSNR_VL BINARY SEARCH.
      IF LT_VBFAB-VBTYP_N = 'M'.
        LT_SORD-VBELN_B = LT_VBFAB-VBELN.  "Billing doc
        LT_SORD-POSNR_B = LT_VBFAB-POSNN.  "item
*S_2021/3/4 add BY E00064
*위의 FUNC RV_ORDER_FLOW_INFORMATION로 가져온 vbfa_tab 빌링문서에 금액필드도 가져와 display
        LT_SORD-RFWRT   = LT_VBFAB-RFWRT.  "Billing amount
        LT_SORD-WAERS   = LT_VBFAB-WAERS.  "Billing amt currency
*E_2021/3/4 add BY E00064

        CLEAR LT_VBRK.
        READ TABLE LT_VBRK WITH KEY VBELN = LT_VBFAB-VBELN BINARY SEARCH.
        IF SY-SUBRC = 0.
          LT_SORD-FKDAT = LT_VBRK-FKDAT.  "date
        ENDIF.
      ENDIF.
    ENDIF.

*S_2021/3/4 add BY E00064
*빌링의 accounting Doc을 display
*가져온 빌링문서, 아이템번호를 선행문서필드(vbelv, posnv) 에 넣고 vbtyp_n이 '+'인 vbeln
    IF LT_SORD-VBELN_B IS NOT INITIAL.
      SORT LT_VBFAB_A BY VBELV POSNV VBTYP_N.
      CLEAR LT_VBFAB_A.
      READ TABLE LT_VBFAB_A WITH KEY VBELV = LT_SORD-VBELN_B
                                     POSNV = LT_SORD-POSNR_B
                                     VBTYP_N = '+' BINARY SEARCH.
      IF SY-SUBRC = 0.
        LT_SORD-VBELN_A = LT_VBFAB_A-VBELN.
      ENDIF.
    ENDIF.
*E_2021/3/4 add BY E00064

    CLEAR LT_KNA1.
    READ TABLE LT_KNA1 WITH KEY KUNNR = LT_SORD-KUNNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LT_SORD-KUNNR_T = LT_KNA1-NAME1. "Shipto
    ENDIF.

    CLEAR LT_KUNWE.
    READ TABLE LT_KUNWE WITH KEY KUNNR = LT_SORD-KUNWE BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LT_SORD-KUNWE_T = LT_KUNWE-NAME1. "Soldto
    ENDIF.

    CLEAR LT_ORGS.
    READ TABLE LT_ORGS WITH KEY KUNNR = LT_SORD-ORGSOLDTO BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LT_SORD-ORGS_T = LT_ORGS-NAME1. "orgsoldto
    ENDIF.

    CLEAR LT_WBSTA.
    READ TABLE LT_WBSTA WITH KEY DOMVALUE_L = LT_SORD-LFGSA BINARY SEARCH.
    IF SY-SUBRC = 0.
      LT_SORD-LFGSA_T = LT_WBSTA-DDTEXT.  "delv status
    ENDIF.

    CLEAR LT_ABGRU.
    READ TABLE LT_ABGRU WITH KEY ABGRU = LT_SORD-ABGRU BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LT_SORD-ABGRU_T = LT_ABGRU-BEZEI. "reason for rej
    ENDIF.

    IF LT_SORD-WBSTA IS NOT INITIAL.
      CLEAR LT_WBSTA.
      READ TABLE LT_WBSTA WITH KEY DOMVALUE_L = LT_SORD-WBSTA BINARY SEARCH.
      IF SY-SUBRC = 0.
        LT_SORD-WBSTA_T = LT_WBSTA-DDTEXT.  "movmt status
      ENDIF.
    ENDIF.

    CLEAR LT_MAKT_S.
    READ TABLE LT_MAKT_S WITH KEY MATNR = LT_SORD-MATNR_S BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LT_SORD-MAKTX_S = LT_MAKT_S-MAKTX.
    ENDIF.

    CLEAR LT_VBEP.
    READ TABLE LT_VBEP WITH KEY VBELN = LT_SORD-VBELN_S
                                POSNR = LT_SORD-POSNR_S BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LT_SORD-EDATU = LT_VBEP-EDATU. "Schedule line date
      LT_SORD-MBDAT = LT_VBEP-MBDAT. "Mat.Avail.date
    ENDIF.

    IF LT_SORD-WBSTA EQ 'C'. "출고 완료된 경우
      DATA: LV_EBELN     TYPE EKBE-EBELN,
            LV_EBELP     TYPE EKBE-EBELP,
            LV_XBLNR(16).

*      DATA: LT_EKBE  TYPE TABLE OF EKBE.

      CLEAR : LV_EBELN, LV_EBELP, LV_XBLNR.

      LV_EBELN = LT_SORD-BSTKD(10).
      LV_EBELP = LT_SORD-POSNR_S+1(5).
      CONCATENATE 'ZIVN' LT_SORD-VBELN_VL INTO LV_XBLNR.

*      CLEAR LT_EKBE.
*      CALL FUNCTION 'ME_READ_HISTORY'
*        EXPORTING
*          EBELN              = LV_EBELN
*          EBELP              = LV_EBELP
*          WEBRE              = ''
*          I_BYPASSING_BUFFER = 'X'
*          I_REFRESH_BUFFER   = 'X'
*        TABLES
*          XEKBE              = LT_EKBE.
**           XEKBES                   =
**           XEKBEZ                   =

      "HQ GR
      SELECT A~VGABE, A~CPUDT, A~CPUTM, A~SHKZG, A~MENGE, A~WRBTR,
             B~FRBNR, B~MBLNR, B~MJAHR
        INTO TABLE @DATA(LT_EKBE)
        FROM EKBE AS A INNER JOIN MKPF AS B
                               ON A~BELNR EQ B~MBLNR
                              AND A~GJAHR EQ B~MJAHR
        WHERE A~EBELN EQ @LV_EBELN
          AND A~EBELP EQ @LV_EBELP
          AND A~VGABE EQ '1'.
*          AND B~FRBNR EQ @LT_SORD-EXTI1.

      "Invoice
      SELECT A~VGABE, A~CPUDT, A~CPUTM, A~SHKZG, A~MENGE, A~WRBTR
        INTO TABLE @DATA(LT_EKBE_I)
        FROM EKBE AS A INNER JOIN RBKP AS B
                               ON A~BELNR EQ B~BELNR
                              AND A~GJAHR EQ B~GJAHR
        WHERE A~EBELN EQ @LV_EBELN
          AND A~EBELP EQ @LV_EBELP
          AND A~VGABE EQ '2'
          AND B~XBLNR EQ @LV_XBLNR. " ZIVN+delivery

      IF LT_EKBE[] IS NOT INITIAL.
        DATA: LT_HQGR   LIKE LT_EKBE.
        DATA: LT_RESULT LIKE TABLE OF MSEG.
*{   INSERT         ZUQK901230                                        1
        SORT LT_EKBE BY CPUDT DESCENDING CPUTM DESCENDING. "날짜, 시간 역순 정렬 처리
        LOOP AT LT_EKBE INTO DATA(LS_EKBE) .
          LV_TABIX = SY-TABIX.
          CLEAR : LT_RESULT[].
          CALL FUNCTION 'MB_READ_MATERIAL_POSITION'
            EXPORTING
              MBLNR  = LS_EKBE-MBLNR
              MJAHR  = LS_EKBE-MJAHR
              TRTYP  = 'A'
            TABLES
              SEQTAB = LT_RESULT.

          READ TABLE LT_RESULT INTO DATA(LX_RESULT) INDEX 1.
          IF SY-SUBRC EQ 0 AND LX_RESULT-SMBLN IS NOT INITIAL.
            DELETE LT_EKBE WHERE MBLNR = LX_RESULT-SMBLN AND MJAHR = LX_RESULT-SJAHR .
            DELETE LT_EKBE INDEX LV_TABIX.
          ENDIF.
        ENDLOOP.
*}   INSERT

        "HQ G/R
        LT_HQGR[] = LT_EKBE[].
*        DELETE LT_HQGR WHERE VGABE NE '1'. "1 제외 다 삭제 처리
        SORT LT_HQGR BY CPUDT DESCENDING CPUTM DESCENDING. "날짜, 시간 역순 정렬 처리
*        READ TABLE LT_HQGR INTO DATA(LS_HQGR) WITH KEY FRBNR = LT_SORD-EXTI1.

        "2021/05/14 S E00035
        LOOP AT LT_HQGR INTO DATA(LS_HQGR) WHERE FRBNR = LT_SORD-EXTI1.
          CLEAR LT_RESULT.
          CALL FUNCTION 'MB_READ_MATERIAL_POSITION'
            EXPORTING
              MBLNR  = LS_HQGR-MBLNR
              MJAHR  = LS_HQGR-MJAHR
              TRTYP  = 'A'
            TABLES
              SEQTAB = LT_RESULT.

          READ TABLE LT_RESULT INTO DATA(LS_RESULT) INDEX 1.
          IF SY-SUBRC EQ 0.
            CLEAR: LV_FIRST, LV_SECOND, LV_VBELN_VL.
            SPLIT LS_RESULT-SGTXT AT '/' INTO LV_FIRST LV_SECOND LV_VBELN_VL.

            IF LV_VBELN_VL+4(10) = LT_SORD-VBELN_VL.
              "기존 로직 S
              IF LS_HQGR-SHKZG = 'S'.
                LT_SORD-MENGE_GR = LS_HQGR-MENGE.

                IF LT_SORD-LFIMG NE LS_HQGR-MENGE. "딜리버리 수량과 다르면 ERROR
                  LT_SORD-RET_ICON = ICON_LED_RED.
                  LT_SORD-RET_MSG  = 'Different with HQ G/R'.
                ENDIF.
              ELSE.
                LT_SORD-RET_ICON = ICON_LED_RED.
                LT_SORD-RET_MSG  = 'Different with HQ G/R'.
              ENDIF.
              "기존 로직 E
              EXIT.
            ELSE.
              CONTINUE.
*              LT_SORD-RET_ICON = ICON_LED_RED.
*              LT_SORD-RET_MSG  = 'Different with HQ G/R'.
            ENDIF.
            CLEAR LS_RESULT.
          ENDIF.
        ENDLOOP.

        IF LT_SORD-LFIMG NE LT_SORD-MENGE_GR.
          LT_SORD-RET_ICON = ICON_LED_RED.
          LT_SORD-RET_MSG  = 'Different with HQ G/R'.
        ENDIF.
        "2021/05/14 E E00035
      ELSE.
        LT_SORD-RET_ICON = ICON_LED_RED.
        LT_SORD-RET_MSG  = 'Different with HQ G/R'.
      ENDIF.

      IF LT_EKBE_I[] IS NOT INITIAL.
        DATA: LT_HQIV LIKE LT_EKBE_I.

        " HQ Invoice
        LT_HQIV[] = LT_EKBE_I[].
*        DELETE LT_HQIV WHERE VGABE NE '2'. "2 제외 다 삭제 처리
        SORT LT_HQIV BY CPUDT DESCENDING CPUTM DESCENDING. "날짜, 시간 역순 정렬 처리

        READ TABLE LT_HQIV INTO DATA(LS_HQIV) INDEX 1.
        IF SY-SUBRC EQ 0.
          IF LS_HQIV-SHKZG = 'S'.
            LT_SORD-MENGE_IV = LS_HQIV-MENGE.

            IF LT_SORD-RET_ICON IS INITIAL AND LT_SORD-LFIMG NE LS_HQIV-MENGE. "딜리버리 수량과 다르면 ERROR
              LT_SORD-RET_ICON = ICON_LED_RED.
              LT_SORD-RET_MSG  = 'Different with HQ Invoice'.
            ENDIF.

            " Billing Doc 있는 경우
            IF LT_SORD-VBELN_B IS NOT INITIAL.
              LT_SORD-WRBTR = LS_HQIV-WRBTR.

              IF LT_SORD-RET_ICON IS INITIAL AND LT_SORD-RFWRT NE LS_HQIV-WRBTR. "빌링 금액과 다르면 ERROR
                LT_SORD-RET_ICON = ICON_LED_RED.
                LT_SORD-RET_MSG  = 'Different with HQ Invoice amount'.
              ENDIF.
            ENDIF.
          ELSE. " H 인 경우
            IF LT_SORD-RET_ICON IS INITIAL. "기존 에러메세지가 없는 경우
              LT_SORD-RET_ICON = ICON_LED_RED.
              LT_SORD-RET_MSG  = 'Different with HQ Invoice'.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSEIF LT_EKBE_I[] IS INITIAL AND LT_SORD-RET_ICON IS INITIAL.
        LT_SORD-RET_ICON = ICON_LED_RED.
        LT_SORD-RET_MSG  = 'Different with HQ Invoice'.
      ENDIF.
    ENDIF.

    MODIFY LT_SORD INDEX LV_INDEX.

    APPEND LT_SORD TO GT_MAIN.
    CLEAR  LT_SORD.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRSH_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRSH_ALV .
  PERFORM GET_DATA.
  " select s/o without Qoutation
  IF C_BOX4 EQ ABAP_TRUE.
    PERFORM GET_DATA2.
  ENDIF.

  IF R_ALL IS INITIAL.
    PERFORM CHECKBOX.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_BILLING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_BILLING .
  "----// 변수선언
  DATA: LT_SELECTED TYPE LVC_T_ROW WITH HEADER LINE,
        LV_RETCD    TYPE CHAR01,
        LV_RETXT    TYPE TEXT100.

  DATA : BEGIN OF LT_VBELN OCCURS 0,
           VBELN   TYPE LIPS-VBELN,      "Delivery
           VBELN_S TYPE VBAK-VBELN,      "S/O #
         END OF LT_VBELN.

  DATA: LT_MAIN LIKE TABLE OF GT_MAIN    WITH HEADER LINE.
*  DATA: LT_VBRP     LIKE TABLE OF VBRP  WITH HEADER LINE.

  DATA: LT_VBRK     LIKE TABLE OF VBRK  WITH HEADER LINE.
  DATA: LT_VBFA     LIKE TABLE OF VBFAS WITH HEADER LINE.
  DATA: LT_VBFAB    LIKE TABLE OF VBFAS WITH HEADER LINE.
  DATA: LT_VBFAB_A  LIKE TABLE OF VBFAS WITH HEADER LINE.
  DATA: LT_COMWA    LIKE TABLE OF VBCO6 WITH HEADER LINE.

  DATA: LV_VBELN_B TYPE VBRK-VBELN.




*  RANGES:  R_VBTYP FOR VBFAS-VBTYP_N. "flow
*  R_VBTYP-LOW = 'N'.
*  R_VBTYP-SIGN = 'I'.
*  R_VBTYP-OPTION = 'EQ'.
*  APPEND R_VBTYP. CLEAR R_VBTYP.
*  R_VBTYP-LOW = 'M'.
*  R_VBTYP-SIGN = 'I'.
*  R_VBTYP-OPTION = 'EQ'.
*  APPEND R_VBTYP. CLEAR R_VBTYP.

  "----// BDC 변수
*  DATA: LV_ERROR    TYPE C.
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA    WITH HEADER LINE.
  DATA: LT_MESSTAB TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
  DATA: LS_OPT     TYPE CTU_PARAMS.
*  CLEAR LV_ERROR.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
*  LS_OPT-DISMODE = 'A'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.


  CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELECTED[].
  IF LT_SELECTED[] IS INITIAL.
    MESSAGE S004.
    EXIT.
  ENDIF.
  "-----// ERROR가 있는 데이터를 선택 했을 경우, 생성 X
  CLEAR WA_MAIN.
  LOOP AT LT_SELECTED INTO DATA(LS_SELECTED).
    READ TABLE GT_MAIN INTO WA_MAIN INDEX LS_SELECTED-INDEX.
*    READ TABLE GT_MAIN INDEX LS_SELECTED-INDEX.
    IF SY-SUBRC EQ 0 AND WA_MAIN-WBSTA NE 'C'.
      MESSAGE S000 WITH TEXT-004 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF SY-SUBRC EQ 0 AND WA_MAIN-VBELN_B IS NOT INITIAL .
      MESSAGE S000 WITH TEXT-005 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

*-- Collect Delivery
    LT_VBELN-VBELN = WA_MAIN-VBELN_VL.
*    LT_VBELN-VBELN_S = WA_MAIN-VBELN_S.
    COLLECT LT_VBELN. CLEAR LT_VBELN.

    CLEAR WA_MAIN.
  ENDLOOP.

  CHECK LT_VBELN[] IS NOT INITIAL.

  LT_MAIN[] = GT_MAIN[].
*  SORT LT_MAIN BY VBELN_S VBELN_VL POSNR_VL.
  SORT LT_MAIN BY VBELN_VL POSNR_VL.
  LOOP AT LT_VBELN.
    "----// BDC
    CLEAR: LT_BDCDATA[], LT_BDCDATA,
           LT_MESSTAB[], LT_MESSTAB.

    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV60A'         '0102'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'KOMFK-VBELN(01)'  LT_VBELN-VBELN.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'       '=SICH'.
*    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'       '/00'.
*
*    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV60A'         '0104'.
*    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'       '=SICH'.

    CALL TRANSACTION 'VF01' WITH AUTHORITY-CHECK USING LT_BDCDATA
                                                 MESSAGES INTO LT_MESSTAB
                                                 OPTIONS FROM LS_OPT.

    READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = LT_MESSTAB-MSGID
          MSGNR               = LT_MESSTAB-MSGNR
          MSGV1               = LT_MESSTAB-MSGV1
          MSGV2               = LT_MESSTAB-MSGV2
          MSGV3               = LT_MESSTAB-MSGV3
          MSGV4               = LT_MESSTAB-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = WA_MAIN-RET_MSG.
      WA_MAIN-RET_ICON = ICON_LED_RED.
      MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
        WHERE VBELN_VL = LT_VBELN-VBELN.

    ELSE.
      READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'S'.
      IF SY-SUBRC EQ 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = LT_MESSTAB-MSGID
            MSGNR               = LT_MESSTAB-MSGNR
            MSGV1               = LT_MESSTAB-MSGV1
            MSGV2               = LT_MESSTAB-MSGV2
            MSGV3               = LT_MESSTAB-MSGV3
            MSGV4               = LT_MESSTAB-MSGV4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = WA_MAIN-RET_MSG.
        WA_MAIN-RET_ICON = ICON_LED_GREEN.
        WA_MAIN-VBELN_B = LT_MESSTAB-MSGV1.
        LV_VBELN_B = WA_MAIN-VBELN_B.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LV_VBELN_B
          IMPORTING
            OUTPUT = LV_VBELN_B.

        MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
                                                 VBELN_B
        WHERE VBELN_VL = LT_VBELN-VBELN.

*------ Read document flow
        CLEAR: LT_COMWA, LT_COMWA[], LT_VBFA, LT_VBFA[].
        CLEAR: LT_VBFAB, LT_VBFAB[], LT_VBFAB_A, LT_VBFAB_A[].
        CLEAR: LT_VBRK, LT_VBRK[].
        LT_COMWA-VBELN = LT_VBELN-VBELN.
*        LT_COMWA-VBELN = LT_VBELN-VBELN_S.
        APPEND LT_COMWA.
        CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
          EXPORTING
            COMWA         = LT_COMWA
          TABLES
            VBFA_TAB      = LT_VBFA
          EXCEPTIONS
            NO_VBFA       = 1
            NO_VBUK_FOUND = 2
            OTHERS        = 3.
* Billing
        LT_VBFAB[]   = LT_VBFA[]. " Billing
*        DELETE LT_VBFAB WHERE VBTYP_N NOT IN R_VBTYP. " M , N
        DELETE LT_VBFAB WHERE VBTYP_N NE 'M'. " M , N
        SORT LT_VBFAB BY VBELV POSNV ERDAT DESCENDING ERZET DESCENDING.

* Accounting
        LT_VBFAB_A[] = LT_VBFA[]. " Accounting
        DELETE LT_VBFAB_A WHERE VBTYP_N NE '+'. " +
        SORT LT_VBFAB_A BY VBELV POSNV VBTYP_N.


*------ Read vbrk
        IF LT_VBFAB[] IS NOT INITIAL.
          CLEAR : LT_VBRK, LT_VBRK[].
          SELECT VBELN FKDAT FROM VBRK INTO CORRESPONDING FIELDS OF TABLE LT_VBRK "Billing
            FOR ALL ENTRIES IN LT_VBFAB
            WHERE VBELN EQ LT_VBFAB-VBELN
              AND VBELN EQ LV_VBELN_B.  "WA_MAIN-VBELN_B .
          SORT LT_VBRK BY VBELN.
        ENDIF.

        IF LT_VBFAB[] IS NOT INITIAL.
          READ TABLE LT_MAIN WITH KEY "VBELN_S = LT_VBELN-VBELN_S
                                      VBELN_VL = LT_VBELN-VBELN
                                      BINARY SEARCH.
          LOOP AT LT_MAIN WHERE VBELN_VL = LT_VBELN-VBELN.
*---------- Billing
            CLEAR LT_VBFAB.
            READ TABLE LT_VBFAB WITH KEY VBELV = LT_MAIN-VBELN_VL
                                         POSNV = LT_MAIN-POSNR_VL
                                         BINARY SEARCH.
            IF LT_VBFAB-VBTYP_N = 'M'.
*              WA_MAIN-VBELN_B = LT_VBFAB-VBELN.  "Billing doc
              WA_MAIN-POSNR_B = LT_VBFAB-POSNN.  "item
              WA_MAIN-RFWRT   = LT_VBFAB-RFWRT.  "Billing amount

              CLEAR LT_VBRK.
              READ TABLE LT_VBRK WITH KEY VBELN = LT_VBFAB-VBELN
                                          BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_MAIN-FKDAT = LT_VBRK-FKDAT.  "date
              ENDIF.
            ENDIF.

*---------- Accounting
            CLEAR LT_VBFAB_A.
            READ TABLE LT_VBFAB_A WITH KEY VBELV = LV_VBELN_B "LT_MAIN-VBELN_B
                                           POSNV = WA_MAIN-POSNR_B
                                           VBTYP_N = '+' BINARY SEARCH.
            IF SY-SUBRC = 0.
              WA_MAIN-VBELN_A = LT_VBFAB_A-VBELN.
            ENDIF.

            MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
                                                     VBELN_B FKDAT
                                                     POSNR_B RFWRT
                                                     VBELN_A
              WHERE VBELN_VL = LT_MAIN-VBELN_VL
                AND POSNR_VL = LT_MAIN-POSNR_VL.

          ENDLOOP.

        ENDIF.


*
**        WAIT UP TO '0.5' SECONDS.
*
*        CLEAR VBRK.
*        SELECT SINGLE * FROM VBRK WHERE VBELN = WA_MAIN-VBELN_B.
*        WA_MAIN-FKDAT   = VBRK-FKDAT.
*        WA_MAIN-VBELN_A = VBRK-BELNR.
*
*        MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
*                                                 VBELN_B FKDAT
*                                                 VBELN_A
*        WHERE VBELN_VL = LT_VBRP-VGBEL.
*
*        SELECT VBELN POSNR VGBEL VGPOS
*          FROM VBRP
*          INTO CORRESPONDING FIELDS OF TABLE LT_VBRP
*         WHERE VBELN = WA_MAIN-VBELN_B.
*        LOOP AT LT_VBRP.
*          WA_MAIN-POSNR_B = VBRP-POSNR.
*          WA_MAIN-RFWRT   = VBRP-NETWR.
*          MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
*                                                   VBELN_B FKDAT
*                                                   POSNR_B RFWRT
*                                                   VBELN_A
*            WHERE VBELN_VL = LT_VBRP-VGBEL
*              AND POSNR_VL = LT_VBRP-VGPOS.
*        ENDLOOP.


      ELSE.
        WA_MAIN-RET_ICON = ICON_LED_RED.
        MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
          WHERE VBELN_VL = LT_VBELN-VBELN.
      ENDIF.

    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL_BILLING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CANCEL_BILLING .
  "----// 변수선언
*  DATA: LV_ERROR    TYPE C.

  DATA: LT_SELECTED TYPE LVC_T_ROW WITH HEADER LINE,
        LV_RETCD    TYPE CHAR01,
        LV_RETXT    TYPE TEXT100.

  DATA : BEGIN OF LT_VBELN OCCURS 0,
           VBELN TYPE LIPS-VBELN,      "Delivery
         END OF LT_VBELN.

  "----// BDC 변수
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA    WITH HEADER LINE.
  DATA: LT_MESSTAB TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
  DATA: LS_OPT     TYPE CTU_PARAMS.

*  CLEAR LV_ERROR.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.

  CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELECTED[].

  IF LT_SELECTED[] IS INITIAL.
    MESSAGE S014 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  "-----// ERROR가 있는 데이터를 선택 했을 경우, 생성 X
  CLEAR WA_MAIN.
  LOOP AT LT_SELECTED INTO DATA(LS_SELECTED).
    READ TABLE GT_MAIN INTO WA_MAIN INDEX LS_SELECTED-INDEX.
    IF SY-SUBRC EQ 0 AND WA_MAIN-VBELN_B IS INITIAL .
      MESSAGE S000 WITH TEXT-006 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

*-- Collect Delivery
    LT_VBELN-VBELN = WA_MAIN-VBELN_B.
    COLLECT LT_VBELN. CLEAR LT_VBELN.

    CLEAR WA_MAIN.
  ENDLOOP.

  LOOP AT LT_VBELN.
    "----// BDC
    CLEAR: LT_BDCDATA[], LT_BDCDATA,
           LT_MESSTAB[], LT_MESSTAB.

    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV60A'         '0102'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'KOMFK-VBELN(01)'  LT_VBELN-VBELN.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'       '=SICH'.
*    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'       '/00'.

*    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV60A'         '0103'.
*    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'       '=SICH'.

    CALL TRANSACTION 'VF11' WITH AUTHORITY-CHECK USING LT_BDCDATA
                                                 MESSAGES INTO LT_MESSTAB
                                                 OPTIONS FROM LS_OPT.

    READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = LT_MESSTAB-MSGID
          MSGNR               = LT_MESSTAB-MSGNR
          MSGV1               = LT_MESSTAB-MSGV1
          MSGV2               = LT_MESSTAB-MSGV2
          MSGV3               = LT_MESSTAB-MSGV3
          MSGV4               = LT_MESSTAB-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = WA_MAIN-RET_MSG.
      WA_MAIN-RET_ICON = ICON_LED_RED.
      MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
      WHERE VBELN_B = LT_VBELN-VBELN.

    ELSE.
      READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'S'.
      IF SY-SUBRC EQ 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = LT_MESSTAB-MSGID
            MSGNR               = LT_MESSTAB-MSGNR
            MSGV1               = LT_MESSTAB-MSGV1
            MSGV2               = LT_MESSTAB-MSGV2
            MSGV3               = LT_MESSTAB-MSGV3
            MSGV4               = LT_MESSTAB-MSGV4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = WA_MAIN-RET_MSG.
        WA_MAIN-RET_ICON = ICON_LED_GREEN.
        CLEAR: WA_MAIN-VBELN_B, WA_MAIN-FKDAT,
               WA_MAIN-POSNR_B, WA_MAIN-RFWRT,
               WA_MAIN-VBELN_A.
        MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
                                                 VBELN_B FKDAT
                                                 POSNR_B RFWRT
                                                 VBELN_A
        WHERE VBELN_B = LT_VBELN-VBELN.

      ELSE.
        WA_MAIN-RET_ICON = ICON_LED_RED.
        MODIFY GT_MAIN FROM WA_MAIN TRANSPORTING RET_ICON RET_MSG
        WHERE VBELN_B = LT_VBELN-VBELN.
      ENDIF.

    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_BDCDATA
*&---------------------------------------------------------------------*
*& BDCDATA 라인 추가
*&---------------------------------------------------------------------*
*&      --> P_DYNBEGIN
*&      --> P_FNAM
*&      --> P_FVAL
*&---------------------------------------------------------------------*
FORM APPEND_BDCDATA TABLES PT_BDCDATA  STRUCTURE BDCDATA
                    USING  PV_DYNBEGIN "TYPE      bdcdata-dynbegin
                           PV_FNAM     "TYPE      bdcdata-fnam
                           PV_FVAL.    "TYPE      bdcdata-fval.
  DATA: WA_BDCDATA TYPE BDCDATA.
  CLEAR WA_BDCDATA.
  IF PV_DYNBEGIN EQ 'X'.
    WA_BDCDATA-DYNBEGIN = 'X'.
    WA_BDCDATA-PROGRAM  = PV_FNAM.
    WA_BDCDATA-DYNPRO   = PV_FVAL.
  ELSE.
    WA_BDCDATA-FNAM     = PV_FNAM.
    WA_BDCDATA-FVAL     = PV_FVAL.
  ENDIF.
  APPEND WA_BDCDATA TO PT_BDCDATA.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCLUDE_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EXCLUDE_STATUS .

  DATA: LV_GROUP  TYPE AGR_NAME,
        LV_UNAME  TYPE SY-UNAME,
        LV_DATUM  TYPE SY-DATUM,
        LT_RESULT TYPE TABLE OF AGR_USERS.

  LV_GROUP = 'Z2010_SD_006'.
  CALL FUNCTION 'PRGN_1001_READ_USER_ASSIGNMENT'
    EXPORTING
      ACTIVITY_GROUP    = LV_GROUP
*     TIME_COMPARE      =
*     EXCL_ASSGM        =
    TABLES
      I_AGR_USERS       = LT_RESULT
    EXCEPTIONS
      NO_DATA_AVAILABLE = 1
      OTHERS            = 2.

  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  LV_UNAME = SY-UNAME.  " 현재 접속 계정
  LV_DATUM = SY-DATUM.  " 시스템 날짜

  READ TABLE LT_RESULT INTO DATA(LS_RESULT) WITH KEY AGR_NAME = LV_GROUP
                                                     UNAME    = LV_UNAME.
  IF SY-SUBRC EQ 0.
    IF LV_DATUM NOT BETWEEN LS_RESULT-FROM_DAT AND LS_RESULT-TO_DAT."from-to date 에 포함되지 않으면
      "Billing , Billing Cancle 버튼 제외
      CLEAR : GT_EXCLU, GT_EXCLU[].

      GT_EXCLU = 'BILLING'. APPEND GT_EXCLU.
      GT_EXCLU = 'BILLCAN'. APPEND GT_EXCLU.
      GT_EXCLU = 'PRICE'.   APPEND GT_EXCLU.
      GT_EXCLU = 'PRINT'.   APPEND GT_EXCLU.
      GT_EXCLU = 'PDF'.     APPEND GT_EXCLU.
      GT_EXCLU = 'EMAIL'.   APPEND GT_EXCLU.
    ENDIF.
  ELSE. "접속 계정이 테이블에 존재 하지 않는 경우
    "Billing , Billing Cancle 버튼 제외
    CLEAR : GT_EXCLU, GT_EXCLU[].

    GT_EXCLU = 'BILLING'. APPEND GT_EXCLU.
    GT_EXCLU = 'BILLCAN'. APPEND GT_EXCLU.
    GT_EXCLU = 'PRICE'.   APPEND GT_EXCLU.
    GT_EXCLU = 'PRINT'.   APPEND GT_EXCLU.
    GT_EXCLU = 'PDF'.     APPEND GT_EXCLU.
    GT_EXCLU = 'EMAIL'.   APPEND GT_EXCLU.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_NEW_PRICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BDC_NEW_PRICING .
  DATA: LT_BDC LIKE TABLE OF GT_MAIN WITH HEADER LINE.

  CLEAR : ET_ROW_NO, ES_ROW_NO.
  "선택된 ROW ID
  CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = ET_ROW_NO.

  IF ET_ROW_NO[] IS INITIAL.
    MESSAGE S014 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR : LT_BDC, LT_BDC[].
  LOOP AT ET_ROW_NO INTO ES_ROW_NO.
    READ TABLE GT_MAIN INTO LT_BDC INDEX ES_ROW_NO-ROW_ID.
    APPEND LT_BDC.
    CLEAR : GT_MAIN, LT_BDC.
  ENDLOOP.

  SORT LT_BDC BY VBELN_S.
  DELETE ADJACENT DUPLICATES FROM LT_BDC COMPARING VBELN_S.

  CLEAR : CTU_PARAMS.
  CTU_PARAMS-DISMODE = 'N'.

  LOOP AT LT_BDC.
    CLEAR : GT_BDC, GT_BDC[], GT_MSG, GT_MSG[].

    PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV45A'    '0102',
                                   ' ' 'BDC_OKCODE'  '=ENT2',
                                   ' ' 'VBAK-VBELN'  LT_BDC-VBELN_S.

    PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV45A'    '4001',
                                   ' ' 'BDC_OKCODE'  '=KONB'.

    PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV45A'    '4001',
                                   ' ' 'BDC_OKCODE'  '=SICH'.

    CALL TRANSACTION 'VA02'  USING GT_BDC
                         OPTIONS FROM CTU_PARAMS
                         MESSAGES INTO GT_MSG.

    READ TABLE GT_MSG WITH KEY MSGTYP = 'E'.
*    IF SY-SUBRC EQ 0. "BDC 실패
*    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_APPEND_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM BDC_APPEND_DATA  USING    VALUE(P_DYNBEGIN)
                               VALUE(P_NAME)
                               VALUE(P_VALUE).
  CLEAR :GT_BDC.

  IF P_DYNBEGIN = 'X'.
    GT_BDC-DYNBEGIN = P_DYNBEGIN.
    GT_BDC-PROGRAM  = P_NAME.
    GT_BDC-DYNPRO   = P_VALUE.
  ELSE.
    GT_BDC-FNAM = P_NAME.
    GT_BDC-FVAL = P_VALUE.
  ENDIF.

  APPEND GT_BDC.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRINT_INVOICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PRINT_INVOICE .
  DATA: LT_SEL  LIKE TABLE OF GT_MAIN WITH HEADER LINE, "선택된 행의 shipment와 동일한 데이터
        LT_TEMP LIKE TABLE OF GT_MAIN WITH HEADER LINE. "선택한 행의  데이터

  DATA: LV_KUNNR TYPE KNA1-ADRNR,
        LV_KUNWE TYPE KNA1-ADRNR,
        LV_WERKS TYPE T001W-ADRNR,
        LT_WERKS TYPE TABLE OF ADRC,
        LT_KUNNR TYPE TABLE OF ADRC,
        LT_KUNWE TYPE TABLE OF ADRC.

  CLEAR : ET_ROW_NO, ES_ROW_NO, GV_ERROR.
  "선택된 ROW ID
  CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = ET_INDEX_ROWS.

  "선택한 행이 없는 경우 ERROR
  IF ET_INDEX_ROWS[] IS INITIAL.
    MESSAGE S014 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "selected row data read
  CLEAR : LT_TEMP[], LT_TEMP.
  LOOP AT ET_INDEX_ROWS INTO ES_INDEX_ROWS.
    READ TABLE GT_MAIN INTO LT_TEMP INDEX ES_INDEX_ROWS-INDEX.
    IF SY-SUBRC EQ 0.
      IF LT_TEMP-VBELN_B IS INITIAL.
        MESSAGE S000 WITH 'The Billing document does not exist' DISPLAY LIKE 'E'.
        GV_ERROR = 'X'.
        EXIT.
      ENDIF.

      APPEND LT_TEMP.
      CLEAR LT_TEMP.
    ENDIF.
  ENDLOOP.

  CHECK LT_TEMP[] IS NOT INITIAL.

  "같은 shpiment 중 선택되지 않은 데이터 선택
  CLEAR : LT_SEL[], LT_SEL.
  LOOP AT LT_TEMP INTO DATA(LS_TEMP).
    IF LS_TEMP-TKNUM IS NOT INITIAL.
      LOOP AT GT_MAIN INTO LT_SEL WHERE TKNUM = LS_TEMP-TKNUM.
        APPEND LT_SEL.
        CLEAR: LS_TEMP, LT_SEL.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT LT_SEL BY TKNUM VBELN_S POSNR_S VBELN_VL VBELN_B.
  DELETE ADJACENT DUPLICATES FROM LT_SEL COMPARING TKNUM VBELN_S POSNR_S VBELN_VL VBELN_B.

  "plant address
  SELECT WERKS, ADRNR
    INTO TABLE @DATA(LT_T001W)
    FROM T001W
    FOR ALL ENTRIES IN @LT_SEL
    WHERE WERKS EQ @LT_SEL-WERKS.
  SORT LT_T001W BY WERKS.

  "sold-to, ship-to address
  SELECT KUNNR, ADRNR
     INTO TABLE @DATA(LT_KNA1)
     FROM KNA1
     FOR ALL ENTRIES IN @LT_SEL
     WHERE KUNNR EQ @LT_SEL-KUNNR
        OR KUNNR EQ @LT_SEL-KUNWE.
  SORT LT_KNA1 BY KUNNR.

  "Payment and Method
  SELECT VBELN AS VBELN_B, ZTERM, ZLSCH
    INTO TABLE @DATA(LT_VBRK)
    FROM VBRK
    FOR ALL ENTRIES IN @LT_SEL
    WHERE VBELN EQ @LT_SEL-VBELN_B.
  SORT LT_VBRK BY VBELN_B.

  IF LT_VBRK IS NOT INITIAL.
    "Terms of Payment Texts
    SELECT ZTERM, VTEXT
      INTO TABLE @DATA(LT_TVZBT)
      FROM TVZBT
      FOR ALL ENTRIES IN @LT_VBRK
      WHERE ZTERM EQ @LT_VBRK-ZTERM
        AND SPRAS EQ @SY-LANGU.
    SORT LT_TVZBT BY ZTERM.
  ENDIF.

  CLEAR : GT_PRINT[], GT_PRINT.
  LOOP AT LT_SEL.
    "invocing party
    CLEAR LV_WERKS.
    READ TABLE LT_T001W INTO DATA(LS_T001W) WITH KEY LT_SEL-WERKS.
    IF SY-SUBRC EQ 0.
      LV_WERKS = LS_T001W-ADRNR.
    ENDIF.

    "sold-to
    CLEAR LV_KUNNR.
    READ TABLE LT_KNA1 INTO DATA(LS_KNA1) WITH KEY LT_SEL-KUNNR.
    IF SY-SUBRC EQ 0.
      LV_KUNNR = LS_KNA1-ADRNR.
    ENDIF.

    "ship-to
    CLEAR : LS_KNA1, LV_KUNWE.
    READ TABLE LT_KNA1 INTO LS_KNA1 WITH KEY LT_SEL-KUNWE.
    IF SY-SUBRC EQ 0.
      LV_KUNWE = LS_KNA1-ADRNR.
    ENDIF.

    "invocing party information 구하기
    CALL FUNCTION 'ADDR_SELECT_ADRC_SINGLE'
      EXPORTING
        ADDRNUMBER        = LV_WERKS
      TABLES
        ET_ADRC           = LT_WERKS
      EXCEPTIONS
        ADDRESS_NOT_EXIST = 1
        PARAMETER_ERROR   = 2
        INTERNAL_ERROR    = 3
        ADDRESS_BLOCKED   = 4
        OTHERS            = 5.

    CLEAR : LT_KUNNR, LT_KUNWE.

    "Sold-to / Payer / Bill-to
    CALL FUNCTION 'ADDR_SELECT_ADRC_SINGLE'
      EXPORTING
        ADDRNUMBER        = LV_KUNNR
      TABLES
        ET_ADRC           = LT_KUNNR
      EXCEPTIONS
        ADDRESS_NOT_EXIST = 1
        PARAMETER_ERROR   = 2
        INTERNAL_ERROR    = 3
        ADDRESS_BLOCKED   = 4
        OTHERS            = 5.

    "Ship-to
    CALL FUNCTION 'ADDR_SELECT_ADRC_SINGLE'
      EXPORTING
        ADDRNUMBER        = LV_KUNWE
      TABLES
        ET_ADRC           = LT_KUNWE
      EXCEPTIONS
        ADDRESS_NOT_EXIST = 1
        PARAMETER_ERROR   = 2
        INTERNAL_ERROR    = 3
        ADDRESS_BLOCKED   = 4
        OTHERS            = 5.

    "*---Smartform 출력 테이블에 담기
    "Shipment
    GT_PRINT-TKNUM      = LT_SEL-TKNUM.

    "Invocing Party data
    READ TABLE LT_WERKS INTO DATA(LS_WERKS) INDEX 1.
    IF SY-SUBRC EQ 0.
      GT_PRINT-NAME1      = LS_WERKS-NAME1.
      GT_PRINT-HOUSE_NUM1 = LS_WERKS-HOUSE_NUM1.
      GT_PRINT-STREET     = LS_WERKS-STREET.
      GT_PRINT-POST_CODE1 = LS_WERKS-POST_CODE1.
      GT_PRINT-CITY1      = LS_WERKS-CITY1.
      GT_PRINT-REGION     = LS_WERKS-REGION.
      GT_PRINT-COUNTRY    = LS_WERKS-COUNTRY.
    ENDIF.

    "Billing No. and Date
    GT_PRINT-VBELN_B      = LT_SEL-VBELN_B.
    GT_PRINT-FKDAT        = LT_SEL-FKDAT.

    "Payment terms.
    READ TABLE LT_VBRK INTO DATA(LS_VBRK) WITH KEY VBELN_B = LT_SEL-VBELN_B BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      "Payment terms
      READ TABLE LT_TVZBT INTO DATA(LS_TVZBT) WITH KEY ZTERM = LS_VBRK-ZTERM BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GT_PRINT-VTEXT = LS_TVZBT-VTEXT.
      ENDIF.
    ENDIF.

    "Sold-to / Payer / Bill-to
    READ TABLE LT_KUNNR INTO DATA(LS_KUNNR) INDEX 1.
    IF SY-SUBRC EQ 0 .
      "Sold-to
      GT_PRINT-NAME1_S       = LS_KUNNR-NAME1.
      GT_PRINT-STREET_S      = LS_KUNNR-STREET.
      GT_PRINT-SUPPL1_S      = LS_KUNNR-STR_SUPPL1.
      GT_PRINT-SUPPL2_S      = LS_KUNNR-STR_SUPPL2.
      GT_PRINT-COUNTRY_S     = LS_KUNNR-COUNTRY.
      GT_PRINT-POST_CODE1_S  = LS_KUNNR-POST_CODE1.

      "Payer
      GT_PRINT-NAME1_P       = LS_KUNNR-NAME1.
      GT_PRINT-STREET_P      = LS_KUNNR-STREET.
      GT_PRINT-SUPPL1_P      = LS_KUNNR-STR_SUPPL1.
      GT_PRINT-SUPPL2_P      = LS_KUNNR-STR_SUPPL2.
      GT_PRINT-COUNTRY_P     = LS_KUNNR-COUNTRY.
      GT_PRINT-POST_CODE1_P  = LS_KUNNR-POST_CODE1.

      "Bill-to
      GT_PRINT-NAME1_B       = LS_KUNNR-NAME1.
      GT_PRINT-STREET_B      = LS_KUNNR-STREET.
      GT_PRINT-SUPPL1_B      = LS_KUNNR-STR_SUPPL1.
      GT_PRINT-SUPPL2_B      = LS_KUNNR-STR_SUPPL2.
      GT_PRINT-COUNTRY_B     = LS_KUNNR-COUNTRY.
      GT_PRINT-POST_CODE1_B  = LS_KUNNR-POST_CODE1.
    ENDIF.

    "Ship-to
    READ TABLE LT_KUNWE INTO DATA(LS_KUNWE) INDEX 1.
    IF SY-SUBRC EQ 0 .
      GT_PRINT-NAME1_SI      = LS_KUNWE-NAME1.
      GT_PRINT-NAME4_SI      = LS_KUNWE-NAME4.
      GT_PRINT-STREET_SI     = LS_KUNWE-STREET.
      GT_PRINT-CITY1_SI      = LS_KUNWE-CITY1.
      GT_PRINT-REGION_SI     = LS_KUNWE-REGION.
      GT_PRINT-POST_CODE1_SI = LS_KUNWE-POST_CODE1.
    ENDIF.

    "Table data
    GT_PRINT-POSNR_B         = LT_SEL-POSNR_B. "Billing item

    GT_PRINT-BSTKD_E         = LT_SEL-BSTKD_E. "PO #
    GT_PRINT-MATNR           = LT_SEL-MATNR_S. "SKU
    GT_PRINT-MAKTX           = LT_SEL-MAKTX_S. "Material Desc.
    GT_PRINT-KWMENG          = LT_SEL-LFIMG.   "Quantity
    GT_PRINT-VRKME           = LT_SEL-VRKME.   "Unit
    GT_PRINT-WAERK           = LT_SEL-WAERK.   "Currency
    GT_PRINT-NETPR           = LT_SEL-NETPR.   "Unit Price
    GT_PRINT-NETWR           = LT_SEL-RFWRT.   "Amount

    "read common table 하단 data
    SELECT ZCM_CODE1, ZCMF01_CH, ZCMF02_CH, ZCMF03_CH
     INTO TABLE @DATA(LT_CONTACT)
      FROM ZCOMMT0021
     WHERE SPRAS = @SY-LANGU
       AND ZMODULE = 'SD'
       AND ZCLASS = 'SD013'.

    READ TABLE LT_CONTACT INTO DATA(LS_CONTACT) WITH KEY ZCM_CODE1 = '1'.
    IF SY-SUBRC EQ 0.
      GT_PRINT-POSITION = LS_CONTACT-ZCMF01_CH.
      GT_PRINT-NAME     = LS_CONTACT-ZCMF02_CH.
      GT_PRINT-EMAIL    = LS_CONTACT-ZCMF03_CH.
    ENDIF.

    READ TABLE LT_CONTACT INTO LS_CONTACT WITH KEY ZCM_CODE1 = '2'.
    IF SY-SUBRC EQ 0.
      GT_PRINT-POSITION2 = LS_CONTACT-ZCMF01_CH.
      GT_PRINT-NAME2     = LS_CONTACT-ZCMF02_CH.
      GT_PRINT-EMAIL2    = LS_CONTACT-ZCMF03_CH.
    ENDIF.

    APPEND GT_PRINT.
    CLEAR : GT_PRINT, LT_SEL, LS_WERKS, LS_KUNNR, LS_KUNWE.
  ENDLOOP.

  IF GT_PRINT[] IS NOT INITIAL.
    "Region text
    SELECT LAND1, BLAND, BEZEI
      INTO TABLE @DATA(LT_T005U)
      FROM T005U
      FOR ALL ENTRIES IN @GT_PRINT
      WHERE LAND1 EQ @GT_PRINT-COUNTRY
        AND BLAND EQ @GT_PRINT-REGION
        AND SPRAS EQ @SY-LANGU.

    "Country text
    SELECT LAND1, LANDX
      INTO TABLE @DATA(LT_T005T)
      FROM T005T
      FOR ALL ENTRIES IN @GT_PRINT
      WHERE LAND1 EQ @GT_PRINT-COUNTRY_S
        AND SPRAS EQ @SY-LANGU.

   CLEAR : GT_ITEM[], GT_ITEM.
    LOOP AT GT_PRINT.
      MOVE-CORRESPONDING GT_PRINT TO GT_ITEM.
      APPEND GT_ITEM.

      "invocing pary region
      READ TABLE LT_T005U INTO DATA(LS_T005U) WITH KEY LAND1 = GT_PRINT-COUNTRY
                                                       BLAND = GT_PRINT-REGION BINARY SEARCH.
      IF SY-SUBRC EQ 0 .
        GT_PRINT-BEZEI = LS_T005U-BEZEI.
      ENDIF.

      "sold-to/payer/bill-to country
      READ TABLE LT_T005T INTO DATA(LS_T005T) WITH KEY LAND1 = GT_PRINT-COUNTRY_S BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        GT_PRINT-COUNTRY_NAME = LS_T005T-LANDX.
      ENDIF.

      MODIFY GT_PRINT.
    ENDLOOP.

    SORT GT_PRINT BY TKNUM. "Shipment no로 sort
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_PDF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DOWNLOAD_PDF .
  DATA: LT_PRINT LIKE TABLE OF GT_PRINT WITH HEADER LINE.
  DATA: LT_TEMP  LIKE TABLE OF GT_PRINT,
        LT_ITEM  LIKE TABLE OF GT_ITEM,
        LT_FNAME LIKE TABLE OF GT_PRINT WITH HEADER LINE,
        LV_TKNUM TYPE I.

  DATA: LV_FORMNAME TYPE TDSFNAME.
  DATA: LV_COUNT    TYPE I.

  DATA: LV_FILENAME TYPE STRING,
        LV_PONUM    TYPE STRING,
        LV_BSTKD_E  TYPE VBKD-BSTKD_E.

  DATA: LV_FILE_PATH TYPE STRING,
        LV_FULL_PATH TYPE STRING.

  CLEAR: GS_PRINT_OPTION, GS_OUTPUT_OPTION.

  GS_PRINT_OPTION-NO_DIALOG  = 'X'.       " 팝업창. X = 안뜨는것.
  GS_PRINT_OPTION-GETOTF     = 'X'.       " GET OTF DATA

  GS_PRINT_OPTION-PREVIEW    = ' '.       " 미리보기
  GS_PRINT_OPTION-LANGU      = SY-LANGU.
  GS_OUTPUT_OPTION-TDNOPRINT = 'X'.
  GS_OUTPUT_OPTION-TDCOPIES = 1.


*Get Device Type
  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      I_LANGUAGE             = SY-LANGU
    IMPORTING
      E_DEVTYPE              = GV_DEVTYPE
    EXCEPTIONS
      NO_LANGUAGE            = 1
      LANGUAGE_NOT_INSTALLED = 2
      NO_DEVTYPE_FOUND       = 3
      SYSTEM_ERROR           = 4
      OTHERS                 = 5.

  GS_OUTPUT_OPTION-TDPRINTER = GV_DEVTYPE.
  GS_OUTPUT_OPTION-TDAUTORITY    = SPACE.
  GS_OUTPUT_OPTION-TDFINAL       = 'X'.

  LV_FORMNAME = 'ZSD_BILLING_INVOICE'  .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LV_FORMNAME
    IMPORTING
      FM_NAME            = GS_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  LOOP AT GT_PRINT INTO LT_PRINT. "shipment가 같은 데이터만 담아서 smartform call
    APPEND LT_PRINT.

    AT END OF TKNUM.
      "File name
      LT_FNAME[] = LT_PRINT[].
      SORT LT_PRINT BY BSTKD_E.
      DELETE ADJACENT DUPLICATES FROM LT_FNAME COMPARING BSTKD_E.

      CLEAR LV_FILENAME.
      LOOP AT LT_FNAME.
        DATA(LV_LINE) = LINES( LT_FNAME ).
        LV_BSTKD_E = LT_FNAME-BSTKD_E.
        AT FIRST.
          IF LV_LINE NE 1.
            CONCATENATE LV_BSTKD_E ', ' INTO LV_PONUM.
            CONTINUE.
          ELSE.
            CONCATENATE LV_BSTKD_E '.PDF' INTO LV_FILENAME.
            CONTINUE.
          ENDIF.
        ENDAT.

        AT LAST.
          CONCATENATE LV_PONUM LV_BSTKD_E '.PDF' INTO LV_FILENAME.
          CONTINUE.
        ENDAT.

        CONCATENATE LV_PONUM LV_BSTKD_E ', ' INTO LV_PONUM.
      ENDLOOP.

      LT_ITEM[] = GT_ITEM[]. "shipment가 다른 item data 삭제
      DELETE LT_ITEM WHERE TKNUM NE LT_PRINT-TKNUM.
      SORT LT_ITEM BY POSNR_B.

      SORT LT_PRINT BY VBELN_B. "Billing no 수 = page 수
      DELETE ADJACENT DUPLICATES FROM LT_PRINT COMPARING VBELN_B.

      CALL FUNCTION GS_FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = GS_PRINT_OPTION
          OUTPUT_OPTIONS     = GS_OUTPUT_OPTION
          USER_SETTING       = 'X'
        IMPORTING
          JOB_OUTPUT_INFO    = GS_JOB_OUTPUT_INFO
        TABLES
          T_MAIN             = LT_PRINT
          T_ITEM             = LT_ITEM
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.

*Covert the OTF data returned to PDF format
      CALL FUNCTION 'CONVERT_OTF_2_PDF'
        EXPORTING
          USE_OTF_MC_CMD         = 'X'
        IMPORTING
          BIN_FILESIZE           = GV_BIN_SIZE
        TABLES
          OTF                    = GS_JOB_OUTPUT_INFO-OTFDATA
          DOCTAB_ARCHIVE         = GT_DOC
          LINES                  = GT_LINES
        EXCEPTIONS
          ERR_CONV_NOT_POSSIBLE  = 1
          ERR_OTF_MC_NOENDMARKER = 2
          OTHERS                 = 3.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


      IF GV_FILE_PATH IS INITIAL.
* To display File SAVE dialog window
        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
          EXPORTING
            WINDOW_TITLE         = 'Download PDF'
            DEFAULT_EXTENSION    = '.PDF'
            DEFAULT_FILE_NAME    = LV_FILENAME
            PROMPT_ON_OVERWRITE  = 'X'
          CHANGING
            FILENAME             = LV_FILENAME
            PATH                 = LV_FILE_PATH
            FULLPATH             = LV_FULL_PATH
          EXCEPTIONS
            CNTL_ERROR           = 1
            ERROR_NO_GUI         = 2
            NOT_SUPPORTED_BY_GUI = 3
            OTHERS               = 4.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSEIF SY-SUBRC = 0.
          GV_FILE_PATH = LV_FILE_PATH.
        ENDIF.

* Use the FM GUI_DOWNLOAD to download the generated PDF file onto the
* presentation server
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            BIN_FILESIZE            = GV_BIN_SIZE
            FILENAME                = LV_FULL_PATH
            FILETYPE                = 'BIN'
          TABLES
            DATA_TAB                = GT_LINES
          EXCEPTIONS
            FILE_WRITE_ERROR        = 1
            NO_BATCH                = 2
            GUI_REFUSE_FILETRANSFER = 3
            INVALID_TYPE            = 4
            NO_AUTHORITY            = 5
            UNKNOWN_ERROR           = 6
            HEADER_NOT_ALLOWED      = 7
            SEPARATOR_NOT_ALLOWED   = 8
            FILESIZE_NOT_ALLOWED    = 9
            HEADER_TOO_LONG         = 10
            DP_ERROR_CREATE         = 11
            DP_ERROR_SEND           = 12
            DP_ERROR_WRITE          = 13
            UNKNOWN_DP_ERROR        = 14
            ACCESS_DENIED           = 15
            DP_OUT_OF_MEMORY        = 16
            DISK_FULL               = 17
            DP_TIMEOUT              = 18
            FILE_NOT_FOUND          = 19
            DATAPROVIDER_EXCEPTION  = 20
            CONTROL_FLUSH_ERROR     = 21
            OTHERS                  = 22.

      ELSE.
        CLEAR : LV_FULL_PATH.
        CONCATENATE GV_FILE_PATH LV_FILENAME INTO LV_FULL_PATH.

* Use the FM GUI_DOWNLOAD to download the generated PDF file onto the
* presentation server
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            BIN_FILESIZE            = GV_BIN_SIZE
            FILENAME                = LV_FULL_PATH
            FILETYPE                = 'BIN'
          TABLES
            DATA_TAB                = GT_LINES
          EXCEPTIONS
            FILE_WRITE_ERROR        = 1
            NO_BATCH                = 2
            GUI_REFUSE_FILETRANSFER = 3
            INVALID_TYPE            = 4
            NO_AUTHORITY            = 5
            UNKNOWN_ERROR           = 6
            HEADER_NOT_ALLOWED      = 7
            SEPARATOR_NOT_ALLOWED   = 8
            FILESIZE_NOT_ALLOWED    = 9
            HEADER_TOO_LONG         = 10
            DP_ERROR_CREATE         = 11
            DP_ERROR_SEND           = 12
            DP_ERROR_WRITE          = 13
            UNKNOWN_DP_ERROR        = 14
            ACCESS_DENIED           = 15
            DP_OUT_OF_MEMORY        = 16
            DISK_FULL               = 17
            DP_TIMEOUT              = 18
            FILE_NOT_FOUND          = 19
            DATAPROVIDER_EXCEPTION  = 20
            CONTROL_FLUSH_ERROR     = 21
            OTHERS                  = 22.

      ENDIF.

      CLEAR : LT_PRINT, LT_PRINT[], LT_ITEM[], LV_FILENAME.
    ENDAT.
  ENDLOOP.
  CLEAR : GT_PRINT, GT_PRINT[], LT_TEMP[], GT_ITEM, GT_ITEM[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_SMARTFORMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_SMARTFORMS .
  DATA: LT_PRINT LIKE TABLE OF GT_PRINT WITH HEADER LINE.
  DATA: LT_TEMP  LIKE TABLE OF GT_PRINT,
        LT_ITEM  LIKE TABLE OF GT_ITEM,
        LV_TKNUM TYPE I.

  DATA: LV_FORMNAME TYPE TDSFNAME.
  DATA: LV_COUNT    TYPE I.

  CLEAR: GS_PRINT_OPTION, GS_OUTPUT_OPTION.

  GS_PRINT_OPTION-PREVIEW   = 'X'.       " 미리보기 O
*  GS_OUTPUT_OPTION-TDDEST =  'ZHP'.     " 프린터 지정.
  GS_OUTPUT_OPTION-TDNOPREV  = SPACE.
  GS_OUTPUT_OPTION-TDNOPRINT = SPACE.
  GS_OUTPUT_OPTION-TDIMMED = 'X'.

  LV_FORMNAME = 'ZSD_BILLING_INVOICE'  .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LV_FORMNAME
    IMPORTING
      FM_NAME            = GS_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  CLEAR : LT_TEMP[], LV_TKNUM.
  LT_TEMP[] = GT_PRINT[].
  SORT LT_TEMP BY TKNUM.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING TKNUM.
  LV_TKNUM = LINES( LT_TEMP ). "Shipment 수

  CLEAR LV_COUNT.
  LOOP AT GT_PRINT INTO LT_PRINT. "shipment가 같은 데이터만 담아서 smartform call
    APPEND LT_PRINT.

    AT END OF TKNUM.
      ADD 1 TO LV_COUNT.

      GS_PRINT_OPTION-NO_OPEN  = 'X'.
      GS_PRINT_OPTION-NO_CLOSE = 'X'.

      IF LV_TKNUM NE 1 AND LV_COUNT = 1 . "첫번째 출력
        GS_PRINT_OPTION-NO_OPEN  = ' '.
        GS_PRINT_OPTION-NO_CLOSE = 'X'.
      ENDIF.

      IF LV_TKNUM NE 1 AND LV_COUNT = LV_TKNUM. "마지막 출력
        GS_PRINT_OPTION-NO_OPEN  = 'X'.
        GS_PRINT_OPTION-NO_CLOSE = ' '.
      ENDIF.

      IF LV_TKNUM = 1."출력 데이터가 1건인 경우
        GS_PRINT_OPTION-NO_OPEN  = ' '.
        GS_PRINT_OPTION-NO_CLOSE = ' '.
      ENDIF.

      LT_ITEM[] = GT_ITEM[]. "shipment가 다른 item data 삭제
      DELETE LT_ITEM WHERE TKNUM NE LT_PRINT-TKNUM.
      SORT LT_ITEM BY POSNR_B.

      SORT LT_PRINT BY VBELN_B. "billing no 수 = page 수
      DELETE ADJACENT DUPLICATES FROM LT_PRINT COMPARING VBELN_B.

      CALL FUNCTION GS_FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = GS_PRINT_OPTION
          OUTPUT_OPTIONS     = GS_OUTPUT_OPTION
          USER_SETTING       = 'X'
        TABLES
          T_MAIN             = LT_PRINT
          T_ITEM             = LT_ITEM
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.

      CLEAR : LT_PRINT, LT_PRINT[], LT_ITEM[].
    ENDAT.
  ENDLOOP.
  CLEAR : GT_PRINT, GT_PRINT[], LT_TEMP[], GT_ITEM, GT_ITEM[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_PDF_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DOWNLOAD_PDF_FILE .
* Internal table to hold the OTF data
  DATA: LT_OTF     TYPE ITCOO OCCURS 0 WITH HEADER LINE,
        LT_OTF_TMP TYPE ITCOO OCCURS 0 WITH HEADER LINE,
        LT_DOC     LIKE DOCS OCCURS 0 WITH HEADER LINE,
        LT_PDF_TAB LIKE TLINE OCCURS 0 WITH HEADER LINE.

  DATA : LV_BIN_FILESIZE TYPE I, " Binary File Size
         LV_FILE_NAME    TYPE STRING,
         LV_FILE_PATH    TYPE STRING,
         LV_FULL_PATH    TYPE STRING.

  LT_OTF[] = GS_JOB_OUTPUT_INFO-OTFDATA.

*Covert the OTF data returned to PDF format
  CALL FUNCTION 'CONVERT_OTF_2_PDF'
    EXPORTING
      USE_OTF_MC_CMD         = 'X'
    IMPORTING
      BIN_FILESIZE           = LV_BIN_FILESIZE
    TABLES
      OTF                    = LT_OTF
      DOCTAB_ARCHIVE         = LT_DOC
      LINES                  = LT_PDF_TAB
    EXCEPTIONS
      ERR_CONV_NOT_POSSIBLE  = 1
      ERR_OTF_MC_NOENDMARKER = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DATA : LV_TIMESTAMP TYPE TIMESTAMP,
         LV_STAMP_C   TYPE CHAR15.


  CONCATENATE 'temporary_file_name' '.pdf' INTO LV_FILE_NAME.

* To display File SAVE dialog window
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE         = 'Purchase Order PDF'
      DEFAULT_EXTENSION    = '.PDF'
      DEFAULT_FILE_NAME    = LV_FILE_NAME
*     FILE_FILTER          = 'PDF File (*.PDF)'
      PROMPT_ON_OVERWRITE  = 'X'
    CHANGING
      FILENAME             = LV_FILE_NAME
      PATH                 = LV_FILE_PATH
      FULLPATH             = LV_FULL_PATH
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Use the FM GUI_DOWNLOAD to download the generated PDF file onto the
* presentation server
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      BIN_FILESIZE            = LV_BIN_FILESIZE
      FILENAME                = LV_FULL_PATH
      FILETYPE                = 'BIN'
    TABLES
      DATA_TAB                = LT_PDF_TAB
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEND_EMAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SEND_EMAIL.
  DATA: LT_PRINT LIKE TABLE OF GT_PRINT WITH HEADER LINE.
  DATA: LT_TEMP  LIKE TABLE OF GT_PRINT,
        LT_ITEM  LIKE TABLE OF GT_ITEM,
        LT_FNAME LIKE TABLE OF GT_PRINT WITH HEADER LINE,
        LV_TKNUM TYPE I.

  DATA: LV_FORMNAME TYPE TDSFNAME.
  DATA: LV_COUNT    TYPE I.

  DATA: LV_FILENAME(50),
        LV_PONUM        TYPE STRING,
        LV_BSTKD_E      TYPE VBKD-BSTKD_E.

  "Object References
  DATA: LO_BCS     TYPE REF TO CL_BCS,
        LO_DOC_BCS TYPE REF TO CL_DOCUMENT_BCS,
        LO_RECEP   TYPE REF TO IF_RECIPIENT_BCS,
        LO_SENDER  TYPE REF TO IF_SENDER_BCS,
        LO_CX_BCX  TYPE REF TO CX_BCS.

  "Internal Tables.
  DATA: LT_OTFDATA        TYPE SSFCRESCL,
        LT_BINARY_CONTENT TYPE SOLIX_TAB,
        LT_TEXT           TYPE BCSY_TEXT,
        LT_PDF_TAB        TYPE STANDARD TABLE OF TLINE,
        LT_OTF            TYPE STANDARD TABLE OF ITCOO.

  "Variables
  DATA: LV_BIN_FILESIZE TYPE SO_OBJ_LEN,
        LV_SENT_TO_ALL  TYPE OS_BOOLEAN,
        LV_BIN_XSTR     TYPE XSTRING,
        LV_FNAME        TYPE RS38L_FNAM,
        LV_STRING_TEXT  TYPE STRING.

  DATA: LV_DESC     TYPE SO_OBJ_DES.
  DATA: LV_INDEX    LIKE SY-INDEX.
  DATA: LV_SUBJECT(50).
  DATA: LV_SENDER   TYPE AD_SMTPADR.

  CLEAR: GS_PRINT_OPTION, GS_OUTPUT_OPTION.

  GS_PRINT_OPTION-NO_DIALOG  = 'X'.       " 팝업창. X = 안뜨는것.
  GS_PRINT_OPTION-GETOTF     = 'X'.       " GET OTF DATA
  GS_PRINT_OPTION-PREVIEW    = ' '.       " 미리보기

  GS_OUTPUT_OPTION-TDNOPRINT  = 'X'.
  GS_OUTPUT_OPTION-TDDEST = 'LOCL'.
  GS_OUTPUT_OPTION-TDNOPRINT = 'X'.

  LV_FORMNAME = 'ZSD_BILLING_INVOICE'  .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LV_FORMNAME
    IMPORTING
      FM_NAME            = GS_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  "Read Email Table
  CLEAR GT_SMTP_RECV[].
  SELECT ZCMF01_CH AS SMTP_ADDR , ZCM_CODE5 AS CODE5
   INTO CORRESPONDING FIELDS OF TABLE @GT_SMTP_RECV
    FROM ZCOMMT0021
   WHERE SPRAS = @SY-LANGU
     AND ZMODULE   = 'CM'
     AND ZCLASS    = 'CM001'
     AND ( ZCM_CODE2 = 'ZSDR0100' OR ZCM_CODE2 = 'zsdr0100' )
     AND ( ZCM_CODE5 = 'R' OR ZCM_CODE5 = 'C' ) "수신인, 참조인
     AND ZCMF03_CH = ' '.

  CLEAR LV_SENDER.
  SELECT SINGLE ZCMF01_CH AS SMTP_ADDR
   INTO @LV_SENDER
    FROM ZCOMMT0021
   WHERE SPRAS = @SY-LANGU
     AND ZMODULE   = 'CM'
     AND ZCLASS    = 'CM001'
     AND ( ZCM_CODE2 = 'ZSDR0100' OR ZCM_CODE2 = 'zsdr0100' )
     AND ZCM_CODE5 = 'S' "발신인
     AND ZCMF03_CH = ' '.

  CLEAR LV_SUBJECT.
  SELECT SINGLE ZCM_CODE4
   INTO @LV_SUBJECT
    FROM ZCOMMT0021
   WHERE SPRAS = @SY-LANGU
     AND ZMODULE   = 'CM'
     AND ZCLASS    = 'CM001'
     AND ( ZCM_CODE2 = 'ZSDR0100' OR ZCM_CODE2 = 'zsdr0100' )
     AND ZCM_CODE5 = 'S' "발신인
     AND ZCMF03_CH = ' '.

  TRY.
*     -------- create persistent send request ------------------------
      LO_BCS = CL_BCS=>CREATE_PERSISTENT( ).

*     -------- create mail body ------------------------
      CLEAR LT_TEXT[].
      "First line
      CONCATENATE 'Dear All' CL_ABAP_CHAR_UTILITIES=>NEWLINE INTO LV_STRING_TEXT.
      APPEND LV_STRING_TEXT TO LT_TEXT.
      CLEAR LV_STRING_TEXT.

      "Second line
      CONCATENATE 'Please check the attached invoice.'
       CL_ABAP_CHAR_UTILITIES=>NEWLINE INTO LV_STRING_TEXT.
      APPEND LV_STRING_TEXT TO LT_TEXT.
      CLEAR LV_STRING_TEXT.

*     -------- create document ------------------------

      LO_DOC_BCS = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                      I_TYPE    = 'RAW'
                      I_TEXT    = LT_TEXT[]
*                          I_LENGTH  = '12'
                      I_SUBJECT = LV_SUBJECT ).   "Subject of the Email

      LOOP AT GT_PRINT INTO LT_PRINT. "shipment가 같은 데이터만 담아서 smartform call
        APPEND LT_PRINT.

        AT END OF TKNUM.
          "File name
          LT_FNAME[] = LT_PRINT[].
          SORT LT_PRINT BY BSTKD_E.
          DELETE ADJACENT DUPLICATES FROM LT_FNAME COMPARING BSTKD_E.

          CLEAR: LV_PONUM, LV_FILENAME.
          LOOP AT LT_FNAME.
            DATA(LV_LINE) = LINES( LT_FNAME ).
            CLEAR LV_BSTKD_E.
            LV_BSTKD_E = LT_FNAME-BSTKD_E.
            AT FIRST.
              IF LV_LINE NE 1.
                CONCATENATE LV_BSTKD_E ', ' INTO LV_PONUM.
                CONTINUE.
              ELSE.
                CONCATENATE LV_BSTKD_E '.PDF' INTO LV_FILENAME.
                CONTINUE.
              ENDIF.
            ENDAT.

            AT LAST.
              CONCATENATE LV_PONUM LV_BSTKD_E '.PDF' INTO LV_FILENAME.
              CONTINUE.
            ENDAT.

            CONCATENATE LV_PONUM LV_BSTKD_E ', ' INTO LV_PONUM.
          ENDLOOP.


          LT_ITEM[] = GT_ITEM[]. "shipment가 다른 item data 삭제
          DELETE LT_ITEM WHERE TKNUM NE LT_PRINT-TKNUM.
          SORT LT_ITEM BY POSNR_B.

          SORT LT_PRINT BY VBELN_B. "Billing no 수 = page 수
          DELETE ADJACENT DUPLICATES FROM LT_PRINT COMPARING VBELN_B.

          "Smartforms 발행
          CLEAR GS_JOB_OUTPUT_INFO.
          CALL FUNCTION GS_FM_NAME
            EXPORTING
              CONTROL_PARAMETERS = GS_PRINT_OPTION
              OUTPUT_OPTIONS     = GS_OUTPUT_OPTION
              USER_SETTING       = 'X'
            IMPORTING
              JOB_OUTPUT_INFO    = GS_JOB_OUTPUT_INFO
            TABLES
              T_MAIN             = LT_PRINT
              T_ITEM             = LT_ITEM
            EXCEPTIONS
              FORMATTING_ERROR   = 1
              INTERNAL_ERROR     = 2
              SEND_ERROR         = 3
              USER_CANCELED      = 4
              OTHERS             = 5.

          IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            EXIT.
          ENDIF.

          CLEAR : GV_BIN_FILESIZE, LV_BIN_XSTR, LT_OTF[].
          LT_OTF[] = GS_JOB_OUTPUT_INFO-OTFDATA[].
          CALL FUNCTION 'CONVERT_OTF'
            EXPORTING
              FORMAT                = 'PDF'
            IMPORTING
*             BIN_FILESIZE          = GV_BIN_FILESIZE
              BIN_FILESIZE          = LV_BIN_FILESIZE
              BIN_FILE              = LV_BIN_XSTR
            TABLES
              OTF                   = LT_OTF[]
              LINES                 = LT_PDF_TAB[]
            EXCEPTIONS
              ERR_MAX_LINEWIDTH     = 1
              ERR_FORMAT            = 2
              ERR_CONV_NOT_POSSIBLE = 3
              OTHERS                = 4.

          IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            EXIT.
          ENDIF.

          "Xstring to binary
          CLEAR : GT_BINARY_CONTENT.
          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              BUFFER     = LV_BIN_XSTR
            TABLES
*             BINARY_TAB = GT_BINARY_CONTENT.
              BINARY_TAB = LT_BINARY_CONTENT.


*     -------- CREATE DOCUMENT ------------------------
          CALL METHOD LO_DOC_BCS->ADD_ATTACHMENT
            EXPORTING
              I_ATTACHMENT_TYPE    = 'PDF'
              I_ATTACHMENT_SIZE    = LV_BIN_FILESIZE
              I_ATTACHMENT_SUBJECT = LV_FILENAME
              I_ATT_CONTENT_HEX    = LT_BINARY_CONTENT.

*     add document to send request
          CALL METHOD LO_BCS->SET_DOCUMENT( LO_DOC_BCS ).

          CLEAR : LT_PRINT, LT_PRINT[], LT_ITEM[], LV_FILENAME.
        ENDAT.
      ENDLOOP.

*     -------- SET SENDER ------------------------
      IF NOT LV_SENDER IS INITIAL.
        LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
                  I_ADDRESS_STRING = LV_SENDER ).
      ENDIF.

      CALL METHOD LO_BCS->SET_SENDER
        EXPORTING
          I_SENDER = LO_SENDER.

*     -------- ADD RECIPIENT ------------------------
      CLEAR : MAILTO, LO_RECEP.
      LOOP AT GT_SMTP_RECV.
        MAILTO = GT_SMTP_RECV-SMTP_ADDR."이메일주소
        LO_RECEP = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( MAILTO ).

        "Add recipient with its respective attributes to send request
        IF GT_SMTP_RECV-CODE5 = 'R'. "수신인
          CALL METHOD LO_BCS->ADD_RECIPIENT
            EXPORTING
              I_RECIPIENT = LO_RECEP.
        ELSEIF GT_SMTP_RECV-CODE5 = 'C'. "참조
          CALL METHOD LO_BCS->ADD_RECIPIENT
            EXPORTING
              I_RECIPIENT = LO_RECEP
              I_COPY      = 'X'.
        ENDIF.

        CALL METHOD LO_BCS->SET_SEND_IMMEDIATELY
          EXPORTING
            I_SEND_IMMEDIATELY = 'X'.
      ENDLOOP.

*     -------- send email ------------------------
      CLEAR LV_SENT_TO_ALL.
      CALL METHOD LO_BCS->SEND(
        EXPORTING
          I_WITH_ERROR_SCREEN = 'X'
        RECEIVING
          RESULT              = LV_SENT_TO_ALL ).

      IF LV_SENT_TO_ALL IS NOT INITIAL.
        MESSAGE S022(SO).
        COMMIT WORK.
      ELSE.
        MESSAGE I500(SBCOMS) WITH MAILTO.
      ENDIF.

*    ---------------exception handling ------------------------
    CATCH CX_BCS INTO LO_CX_BCX.
      MESSAGE I865(SO) WITH BCS_EXCEPTION->ERROR_TYPE.
  ENDTRY.

  CLEAR : GT_PRINT, GT_PRINT[], LT_TEMP[], GT_ITEM, GT_ITEM[], LT_BINARY_CONTENT, LV_BIN_FILESIZE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONFIRM_POPUP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_T09
*&      --> TEXT_T10
*&      <-- GV_ANSWER
*&---------------------------------------------------------------------*
FORM CONFIRM_POPUP  USING    PV_TITLE
                             PV_QUEST
                    CHANGING PV_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = PV_TITLE
      TEXT_QUESTION         = PV_QUEST
      DISPLAY_CANCEL_BUTTON = ABAP_FALSE
    IMPORTING
      ANSWER                = PV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.

ENDFORM.
