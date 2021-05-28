*----------------------------------------------------------------------*
***INCLUDE ZSDR0070_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .
  IF SY-TCODE = 'ZSDR0071'.  " Display invoice completed data only
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-ACTIVE = 0.
      ELSE.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-ACTIVE = 1.
      ELSE.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FORWARD_AGENT_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- S_TDLNR_LOW
*&---------------------------------------------------------------------*
FORM FORWARD_AGENT_F4 CHANGING P_TDLNR.
  DATA: BEGIN OF LT_F4HELP OCCURS 0,
          LIFNR TYPE LFA1-LIFNR,
          NAME1 TYPE LFA1-NAME1,
        END OF LT_F4HELP.
  DATA: LV_KTOKK TYPE KTOKK,
        LV_BUKRS TYPE BUKRS.
  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL.
  DATA: LS_RETURN TYPE DDSHRETVAL.
  "----//
  IF P_TPLST EQ '2011'.
    LV_KTOKK = '8000'.
    LV_BUKRS = '2010'.
  ELSE.

  ENDIF.
  SELECT LIFNR,
         NAME1
    INTO CORRESPONDING FIELDS OF TABLE @LT_F4HELP
     FROM ZSDV0043
   WHERE KTOKK EQ @LV_KTOKK
     AND BUKRS EQ @LV_BUKRS.
  "----// Fowared Agent F4
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'LIFNR'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'S_TDLNR-LOW'
      WINDOW_TITLE    = 'Forward Agent'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = LT_F4HELP
      RETURN_TAB      = LT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT
*&---------------------------------------------------------------------*
*& 프로그램 첫 구동시 1회 수행하는 로직
*&---------------------------------------------------------------------*
*&      <-- SSCRFIELDS Screen 1000 Upload Format Download Button
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INIT .
  P_TPLST = '2011'.
  P_SHTYP = 'ZO01'.
  P_POSTD = SY-DATUM.

  IF SY-TCODE = 'ZSDR0071'.  " Display invoice completed data only
    P_COMP = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SHIPMENT_COST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN
*&---------------------------------------------------------------------*
FORM GET_SHIPMENT_COST TABLES PT_MAIN LIKE GT_MAIN.
  "----// 변수선언
  DATA: BEGIN OF LT_LOCAL_MAIN OCCURS 0,
          FKNUM  TYPE VFKK-FKNUM, "Shipment Cost Number
          STBER  TYPE VFKK-STBER, "Status of calculation (shipment costs header data)
          STFRE  TYPE VFKK-STFRE, "Status of account determination (shipment costs header data)
          STABR  TYPE VFKK-STABR, "Status of transfer (shipment costs header data)
          TKNUM  TYPE VTTK-TKNUM, "Shipment Number
          ADD02  TYPE VTTK-ADD02, "trailer overall length
          STTRG  TYPE VTTK-STTRG, "Overall transportation status
*S_2021/2/21 VENDOR I/V BY E00064
          EXTI2  TYPE VFKK-EXTI2, "Vendor I/V
*E_2021/2/21 VENDOR I/V BY E00064
          DDTEXT TYPE VAL_TEXT,   "Overall transportation status TEXT
          TDLNR  TYPE VTTK-TDLNR, "Number of forwarding agent
*         vbeln  TYPE vttp-vbeln, "Delivery No.
          KUNWE  TYPE LIKP-KUNNR, "Ship-to party
          ZFR4P  TYPE KONP-KBETR, "Base Freight 기본운임
          ZFR5P  TYPE KONP-KBETR, "Surchage     추가운임
          ZFR8P  TYPE KONP-KBETR, "Via          경유운임
          ZFR9P  TYPE KONP-KBETR, "Etc Fee      기타운임
          SUMMP  TYPE KONP-KBETR, "Summation    합계
          KONWA  TYPE KONP-KONWA, "Condition unit (currency or percentage) 통화키
          KPEIN  TYPE KONP-KPEIN, "Condition Pricing Unit 가격단위
          KMEIN  TYPE KONP-KMEIN, "Condition Unit 수량단위
          EBELN  TYPE VFKP-EBELN, "PO
          EBELP  TYPE VFKP-EBELP, "PO Item
          BELNR  TYPE EKBE-BELNR, "IR-L No.
          GJAHR  TYPE EKBE-GJAHR, "Fiscal year
          BUDAT  TYPE EKBE-BUDAT, "Inv. post date
          LBLNI  TYPE VFKP-LBLNI, "Entry Sheet Number
        END OF LT_LOCAL_MAIN,
        BEGIN OF LT_SHIPMENT OCCURS 0,
          TKNUM TYPE VTTP-TKNUM,  "shipment no.
          TPNUM TYPE VTTP-TPNUM,  "shipment Item no.
          ADD02 TYPE VTTK-ADD02,  "trailer overall length
          TDLNR TYPE VTTK-TDLNR,  "Number of forwarding agent
          STTRG TYPE VTTK-STTRG,  "Overall transportation status
          VBELN TYPE VTTP-VBELN,  "outbound delivery
          KUNWE TYPE LIKP-KUNNR,  "ship-to party
          VOLUM TYPE LIPS-VOLUM,  "Volumn
          VOLEH TYPE LIPS-VOLEH,  "Volumn unit of measure (M3)
        END OF LT_SHIPMENT,

        BEGIN OF LT_SHIPMENT_COST OCCURS 0,
          TKNUM TYPE VTTP-TKNUM,  "shipment no.
          FKNUM TYPE VFKK-FKNUM,
          STBER TYPE VFKK-STBER,
          STFRE TYPE VFKK-STFRE,
          STABR TYPE VFKK-STABR,
*S_2021/2/21 VENDOR I/V BY E00064
          EXTI2 TYPE VFKK-EXTI2, "Vendor I/V
*E_2021/2/21 VENDOR I/V BY E00064
          EBELN TYPE VFKP-EBELN, "PO
          EBELP TYPE VFKP-EBELP, "PO Item
          BELNR TYPE RBKP-BELNR, "IV
          LBLNI TYPE VFKP-LBLNI, "Entry Sheet Number
          KNUMV TYPE VFKP-KNUMV, "Number of the Document Condition
          KALSM TYPE VFKP-KALSM, "Sales and Distribution: Pricing Procedure in Pricing
        END OF LT_SHIPMENT_COST,
        BEGIN OF LT_PRICE_INF OCCURS 0,
          KUNWE TYPE KUNWE,      "ship-to
          ADD02 TYPE A908-ADD02, "Overall length
          KSCHL TYPE KSCHL,      "Condition type
          DATAB TYPE A305-DATAB, "begin data
          TDLNR TYPE TDLNRS,     "ServcAgent
          KBETR TYPE KBETR_KOND, "Condition amount (83.80)
          KONWA TYPE KONWA,      "Currency (USD)
          KPEIN TYPE KPEIN,      "Condition Pricing Unit (1)
          KMEIN TYPE KMEIN,      "Condition Unit (EA)
        END OF LT_PRICE_INF.
*S_2021/3/26 by e00064
  DATA: LV_TABIX TYPE SY-TABIX.
  DATA: LT_SHIPMENT_ITEM  LIKE TABLE OF LT_SHIPMENT WITH HEADER LINE.
*E_2021/3/26 by e00064


  DATA: LV_BELNR TYPE EKBE-BELNR,
        LV_GJAHR TYPE EKBE-GJAHR,
        LV_BUDAT TYPE EKBE-BUDAT,
        LV_SHKZG TYPE EKBE-SHKZG.

*S_2021/2/21 VENDOR I/V BY E00064
  DATA: LV_EXTI2 TYPE VTTK-EXTI2.
  DATA : LV_KUNWE LIKE LT_LOCAL_MAIN-KUNWE.
  DATA : LV_ZFR4P LIKE LT_LOCAL_MAIN-ZFR4P.
  DATA : LV_ZFR5P LIKE LT_LOCAL_MAIN-ZFR5P.
*E_2021/2/21 VENDOR I/V BY E00064

  DATA: LS_COMM_HEAD_I TYPE KOMK,
        LS_COMM_HEAD_E TYPE KOMK,
        LT_TKOMV       TYPE TABLE OF KOMV WITH HEADER LINE.
  TYPES: BEGIN OF TY_TKNUM,
           TKNUM TYPE VTTP-TKNUM, "shipment no.
         END OF TY_TKNUM.
  DATA:  LT_TKNUM TYPE SORTED TABLE OF TY_TKNUM WITH UNIQUE KEY TKNUM WITH HEADER LINE.
  DATA: LV_INPUT  TYPE DEC16_3,
        LV_OUTPUT TYPE DEC16_3.
  "----// Overall transportation status
  DATA: LT_DD07V TYPE /LTB/BAS_T_DD07V,
        LS_DD07V TYPE DD07V.
  CLEAR LT_DD07V[].
  CALL FUNCTION 'CNV_OR_PE_GET_DOMAIN_VALUES'
    EXPORTING
      IV_DOMNAME         = 'STTRG'
    IMPORTING
      ET_DD07V           = LT_DD07V
    EXCEPTIONS
      PARAMETERS_MISSING = 1
      UNEXPECTED_ERROR   = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    CHECK 1 = 1.
  ENDIF.
  "----// 선적정보 취합
  CLEAR: LT_SHIPMENT, LT_SHIPMENT[].
  SELECT VTTP~TKNUM,
         VTTP~TPNUM,
         VTTK~ADD02,
         VTTK~TDLNR,
         VTTK~STTRG,
         VTTP~VBELN,
         LIKP~KUNNR AS KUNWE,
         SUM( LIPS~VOLUM ) AS VOLUM,
         LIPS~VOLEH
    INTO CORRESPONDING FIELDS OF TABLE @LT_SHIPMENT
    FROM VTTK INNER JOIN VTTP ON VTTP~TKNUM EQ VTTK~TKNUM
              INNER JOIN LIKP ON LIKP~VBELN EQ VTTP~VBELN
              INNER JOIN LIPS ON LIPS~VBELN EQ VTTP~VBELN
   WHERE VTTK~TPLST EQ @P_TPLST
     AND VTTK~SHTYP EQ @P_SHTYP
     AND VTTK~TKNUM IN @S_TKNUM
     AND VTTK~TDLNR IN @S_TDLNR
     AND VTTK~DTABF IN @S_DTABF
     AND VTTK~FRKRL EQ 'X'
     AND VTTK~TDLNR NE @SPACE
*   GROUP BY VTTP~TKNUM, VTTP~TPNUM, VTTK~TDLNR, VTTK~STTRG, VTTP~VBELN, LIKP~KUNNR, LIPS~VOLEH.
   GROUP BY VTTP~TKNUM, VTTP~TPNUM, VTTK~ADD02, VTTK~TDLNR, VTTK~STTRG, VTTP~VBELN, LIKP~KUNNR, LIPS~VOLEH.

*S_2021/3/26 by e00064
  LT_SHIPMENT_ITEM[] = LT_SHIPMENT[].
  SORT LT_SHIPMENT_ITEM BY TKNUM KUNWE.
  DELETE ADJACENT DUPLICATES FROM LT_SHIPMENT_ITEM COMPARING TKNUM KUNWE.
*E_2021/3/26 by e00064

  SORT LT_SHIPMENT BY TKNUM  .
*  DELETE ADJACENT DUPLICATES FROM lt_shipment COMPARING tknum tpnum sttrg vbeln voleh.
  DELETE ADJACENT DUPLICATES FROM LT_SHIPMENT COMPARING TKNUM.

  IF LT_SHIPMENT[] IS NOT INITIAL.
    "----// 선적비용정보
    CLEAR: LT_SHIPMENT_COST, LT_SHIPMENT_COST[].
    SELECT VFKP~REBEL AS TKNUM,
           VFKK~FKNUM,
           VFKK~STBER,
           VFKK~STFRE,
           VFKK~STABR,
*S_2021/2/21 VENDOR I/V BY E00064
           VFKK~EXTI2,
*E_2021/2/21 VENDOR I/V BY E00064
           VFKP~EBELN,
           VFKP~EBELP,
           VFKP~LBLNI,
           VFKP~KNUMV,
           VFKP~KALSM
      INTO CORRESPONDING FIELDS OF TABLE @LT_SHIPMENT_COST
      FROM VFKK INNER JOIN VFKP ON VFKP~FKNUM EQ VFKK~FKNUM
       FOR ALL ENTRIES IN @LT_SHIPMENT
     WHERE VFKP~REBEL EQ @LT_SHIPMENT-TKNUM.
    SORT LT_SHIPMENT_COST BY TKNUM.
    "----// 선적비용 가격 마스터
    CLEAR LT_PRICE_INF[].
    SELECT A~TDLNR,
           A~ADD02,
           A~KUNWE,
           A~KSCHL,
           A~DATAB,
           K~KBETR,
           K~KONWA,
           K~KPEIN,
           K~KMEIN
      INTO CORRESPONDING FIELDS OF TABLE @LT_PRICE_INF
      FROM A908 AS A INNER JOIN KONP AS K ON K~KNUMH = A~KNUMH "A908로 변경 04/08/21
                                         AND K~KOPOS = '01'
     WHERE A~KAPPL    EQ 'F'
       AND A~KSCHL    IN ('ZFR4', 'ZFR5')
       AND A~DATAB    LE @SY-DATLO
       AND A~DATBI    GE @SY-DATLO
       AND A~TPLST    EQ @P_TPLST
       AND K~LOEVM_KO EQ @SPACE.

*S_2021/3/26 by e00064
*SORT TDLNR KUNWE KSCHL KBETR DESCENDING.으로 수정
* TDLNR KUNWE KSCHL키로 중복제거 로직은 제거
* comment out old logic start
*    SORT LT_PRICE_INF BY TDLNR KUNWE KSCHL DATAB DESCENDING.
*    DELETE ADJACENT DUPLICATES FROM LT_PRICE_INF COMPARING TDLNR KUNWE KSCHL.
* comment out old logic end
    SORT LT_PRICE_INF BY TDLNR ADD02 KUNWE KSCHL KBETR DESCENDING.
* new logic
*E_2021/3/26 by e00064
  ENDIF.

  "----// MAIN 데이터 만들기
  CLEAR: LT_LOCAL_MAIN, LT_LOCAL_MAIN[],
         LT_TKNUM, LT_TKNUM[],
         PT_MAIN, PT_MAIN[].

  LOOP AT LT_SHIPMENT.
    LT_LOCAL_MAIN-TDLNR = LT_SHIPMENT-TDLNR.
    LT_LOCAL_MAIN-TKNUM = LT_SHIPMENT-TKNUM.
    LT_LOCAL_MAIN-ADD02 = LT_SHIPMENT-ADD02.
    LT_LOCAL_MAIN-STTRG = LT_SHIPMENT-STTRG.

    CLEAR LS_DD07V.
    READ TABLE LT_DD07V INTO LS_DD07V WITH KEY DOMNAME    = 'STTRG'
                                               DOMVALUE_L = LT_SHIPMENT-STTRG
                                      BINARY SEARCH.
    LT_LOCAL_MAIN-DDTEXT = LS_DD07V-DDTEXT.
*    lt_local_main-vbeln = lt_shipment-vbeln.
    LT_LOCAL_MAIN-KUNWE = LT_SHIPMENT-KUNWE.
    "----// 생성된 shipment cost 정보
    CLEAR LT_SHIPMENT_COST.
    READ TABLE LT_SHIPMENT_COST WITH KEY TKNUM = LT_SHIPMENT-TKNUM BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LT_LOCAL_MAIN-FKNUM = LT_SHIPMENT_COST-FKNUM.
      LT_LOCAL_MAIN-STBER = LT_SHIPMENT_COST-STBER.
      LT_LOCAL_MAIN-STFRE = LT_SHIPMENT_COST-STFRE.
      LT_LOCAL_MAIN-STABR = LT_SHIPMENT_COST-STABR.
*S_2021/2/21 VENDOR I/V BY E00064
      LT_LOCAL_MAIN-EXTI2 = LT_SHIPMENT_COST-EXTI2.
*E_2021/2/21 VENDOR I/V BY E00064
      LT_LOCAL_MAIN-EBELN = LT_SHIPMENT_COST-EBELN.
      LT_LOCAL_MAIN-EBELP = LT_SHIPMENT_COST-EBELP.
      LT_LOCAL_MAIN-LBLNI = LT_SHIPMENT_COST-LBLNI.
      IF LT_LOCAL_MAIN-LBLNI IS NOT INITIAL.
        "----// 송장 정보
        CLEAR: LV_BELNR, LV_GJAHR, LV_SHKZG.
        SELECT BELNR,
               GJAHR,
               SHKZG,
               BUDAT
          INTO (@LV_BELNR, @LV_GJAHR, @LV_SHKZG, @LV_BUDAT)
          FROM EKBE
         WHERE VGABE EQ '2'
           AND LFBNR EQ @LT_LOCAL_MAIN-LBLNI ORDER BY BELNR DESCENDING,
                                                      GJAHR DESCENDING.
          EXIT.
        ENDSELECT.
        IF SY-SUBRC EQ 0.
          IF LV_SHKZG EQ 'S'.
            LT_LOCAL_MAIN-BELNR = LV_BELNR.
            LT_LOCAL_MAIN-GJAHR = LV_GJAHR.
            LT_LOCAL_MAIN-BUDAT = LV_BUDAT.
          ELSE.
            CHECK 1 = 1.
          ENDIF.
        ELSE.
          CHECK 1 = 1.
        ENDIF.

      ENDIF.
      "----// 운임정보
      READ TABLE LT_TKNUM WITH TABLE KEY TKNUM = LT_SHIPMENT-TKNUM.
      IF SY-SUBRC EQ 0.
        CLEAR LT_LOCAL_MAIN.
        CONTINUE.
      ELSE.
        CLEAR: LS_COMM_HEAD_I, LT_TKOMV[], LT_TKOMV.
        LS_COMM_HEAD_I-KNUMV = LT_SHIPMENT_COST-KNUMV.
        LS_COMM_HEAD_I-KAPPL = 'F'.
        LS_COMM_HEAD_I-KALSM = LT_SHIPMENT_COST-KALSM.
        CALL FUNCTION 'RV_KONV_SELECT'
          EXPORTING
            COMM_HEAD_I           = LS_COMM_HEAD_I
            GENERAL_READ          = 'X'
            READ_CONDITION_RECORD = 'X'
          IMPORTING
            COMM_HEAD_E           = LS_COMM_HEAD_E
          TABLES
            TKOMV                 = LT_TKOMV.

        LOOP AT LT_TKOMV WHERE KWERT > 0
                           AND KPOSN IS NOT INITIAL.
          IF     LT_TKOMV-KSCHL EQ 'ZFR4'.
            LT_LOCAL_MAIN-ZFR4P = LT_LOCAL_MAIN-ZFR4P + LT_TKOMV-KWERT.
            LT_LOCAL_MAIN-KONWA = LT_TKOMV-WAERS.
            LT_LOCAL_MAIN-KPEIN = 0.  "lt_tkomv-kpein.
            LT_LOCAL_MAIN-KMEIN = LT_TKOMV-KMEIN.
          ELSEIF LT_TKOMV-KSCHL EQ 'ZFR5'.
            LT_LOCAL_MAIN-ZFR5P = LT_LOCAL_MAIN-ZFR5P + LT_TKOMV-KWERT.
            LT_LOCAL_MAIN-KONWA = LT_TKOMV-WAERS.
            LT_LOCAL_MAIN-KPEIN = 0.  "lt_tkomv-kpein.
            "lt_local_main-kmein = lt_tkomv-kmein.
*        ELSEIF lt_tkomv-kschl EQ 'ZFR8'.
*          lt_local_main-zfr8p = lt_local_main-zfr8p + lt_tkomv-kwert.
*          lt_local_main-konwa = lt_tkomv-waers.
*          lt_local_main-kpein = lt_tkomv-kpein.
*          lt_local_main-kmein = lt_tkomv-kmein.
          ELSEIF LT_TKOMV-KSCHL EQ 'ZFR9'.
            LT_LOCAL_MAIN-ZFR9P = LT_LOCAL_MAIN-ZFR9P + LT_TKOMV-KWERT.
            LT_LOCAL_MAIN-KONWA = LT_TKOMV-WAERS.
            LT_LOCAL_MAIN-KPEIN = 0.  "lt_tkomv-kpein.
            "lt_local_main-kmein = lt_tkomv-kmein.
          ELSE.
            CHECK 1 = 1.
          ENDIF.
        ENDLOOP.
        LT_TKNUM-TKNUM = LT_SHIPMENT-TKNUM.
        INSERT LT_TKNUM INTO TABLE LT_TKNUM.
      ENDIF.

    ELSE.

      "----// 운임단가 마스터
      READ TABLE LT_SHIPMENT_ITEM WITH KEY TKNUM = LT_SHIPMENT-TKNUM BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LV_TABIX = SY-TABIX.
      ENDIF.
      CLEAR : LV_ZFR4P, LV_ZFR5P.

      LOOP AT LT_SHIPMENT_ITEM FROM LV_TABIX.
        IF LT_SHIPMENT_ITEM-TKNUM NE LT_SHIPMENT-TKNUM.
          EXIT.
        ENDIF.

        CLEAR LT_PRICE_INF.
        READ TABLE LT_PRICE_INF WITH KEY TDLNR = LT_SHIPMENT-TDLNR
                                         ADD02 = LT_SHIPMENT-ADD02
                                         KUNWE = LT_SHIPMENT_ITEM-KUNWE
                                         KSCHL = 'ZFR4'
                                BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*          IF lt_shipment-voleh EQ lt_price_inf-kmein.
*            lv_output = lt_shipment-volum.
*          ELSE.
*            IF lt_shipment-voleh IS NOT INITIAL.
*              lv_input = lt_shipment-volum.
*              CALL FUNCTION 'ZMD_UNIT_CONVERSION'
*                EXPORTING
*                  input    = lv_input
*                  unit_in  = lt_shipment-voleh
*                  unit_out = lt_price_inf-kmein
*                IMPORTING
*                  output   = lv_output.
*  *            lt_shipment-volum = lv_output.
*            ELSE.
*              lv_output = 0.
*            ENDIF.
*          ENDIF.
*          lt_local_main-zfr4p = lv_output * lt_price_inf-kbetr.  " lt_shipment-volum * lt_price_inf-kbetr.
*          LT_LOCAL_MAIN-ZFR4P = LT_PRICE_INF-KBETR.
          IF  LT_PRICE_INF-KBETR >  LV_ZFR4P .
            LV_ZFR4P  = LT_PRICE_INF-KBETR.
            LV_KUNWE  = LT_PRICE_INF-KUNWE.
          ENDIF.
          LT_LOCAL_MAIN-KONWA = LT_PRICE_INF-KONWA.
          LT_LOCAL_MAIN-KPEIN = 0.  "lt_price_inf-kpein.
          LT_LOCAL_MAIN-KMEIN = LT_PRICE_INF-KMEIN.
        ELSE.

        ENDIF.

        CLEAR LT_PRICE_INF.
        READ TABLE LT_PRICE_INF WITH KEY TDLNR = LT_SHIPMENT-TDLNR
                                         ADD02 = LT_SHIPMENT-ADD02
                                         KUNWE = LT_SHIPMENT_ITEM-KUNWE
                                         KSCHL = 'ZFR5'
                                BINARY SEARCH.
        IF SY-SUBRC EQ 0.
*          IF lt_shipment-voleh EQ lt_price_inf-kmein.
*            lv_output = lt_shipment-volum.
*          ELSE.
*            IF lt_shipment-voleh IS NOT INITIAL.
*              lv_input = lt_shipment-volum.
*              CALL FUNCTION 'ZMD_UNIT_CONVERSION'
*                EXPORTING
*                  input    = lv_input
*                  unit_in  = lt_shipment-voleh
*                  unit_out = lt_price_inf-kmein
*                IMPORTING
*                  output   = lv_output.
*  *            lt_shipment-volum = lv_output.
*            ELSE.
*              lv_output = 0.
*            ENDIF.
*          ENDIF.
*          lt_local_main-zfr5p = lv_output * lt_price_inf-kbetr.  " lt_shipment-volum * lt_price_inf-kbetr.
          IF  LT_PRICE_INF-KBETR >  LV_ZFR5P .
            LV_ZFR5P  = LT_PRICE_INF-KBETR.
          ENDIF.
          LT_LOCAL_MAIN-ZFR5P = LT_PRICE_INF-KBETR.
          LT_LOCAL_MAIN-KONWA = LT_PRICE_INF-KONWA.
          LT_LOCAL_MAIN-KPEIN = 0.  "lt_price_inf-kpein.
          "lt_local_main-kmein = lt_price_inf-kmein.
        ENDIF.

      ENDLOOP.

      IF LV_KUNWE IS NOT INITIAL.
        LT_LOCAL_MAIN-KUNWE = LV_KUNWE .
      ENDIF.
      LT_LOCAL_MAIN-ZFR4P = LV_ZFR4P .
      LT_LOCAL_MAIN-ZFR5P = LV_ZFR5P .

    ENDIF.

    LT_LOCAL_MAIN-SUMMP = LT_LOCAL_MAIN-ZFR4P + LT_LOCAL_MAIN-ZFR5P + LT_LOCAL_MAIN-ZFR8P + LT_LOCAL_MAIN-ZFR9P.

    COLLECT LT_LOCAL_MAIN.
    CLEAR LT_LOCAL_MAIN.
  ENDLOOP.
  "----// 신호등 & 편집셀 설정
  DATA: LT_CELLSTYL TYPE LVC_T_STYL,
        LS_CELLSTYL TYPE LVC_S_STYL,
        LV_IDX      TYPE SY-TABIX.

  LOOP AT LT_LOCAL_MAIN.
    MOVE-CORRESPONDING LT_LOCAL_MAIN TO WA_MAIN.

*S_2021/3/2 add logic By E00064
* 1. open :   shipment cost 미생성
* 2. in progress : 	shiment cost생성되었으나 invoice 아직 생성안된거
* 3. completed : 	invoice  생성완료

* 1. open :   shipment cost 미생성
    IF P_OPEN = 'X'.
      IF WA_MAIN-FKNUM IS INITIAL.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.
* 2. in progress : 	shiment cost생성되었으나 invoice 아직 생성안된거
    IF P_PROG = 'X'.
      IF WA_MAIN-FKNUM IS NOT INITIAL AND
         WA_MAIN-BELNR IS INITIAL.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.
* 3. completed : 	invoice  생성완료
    IF P_COMP = 'X'.
      IF WA_MAIN-BELNR IS INITIAL.
        CONTINUE.
      ELSE.
      ENDIF.
    ENDIF.

*E_2021/3/2 add logic By E00064

    WA_MAIN-KPEIN = 1.
    IF     WA_MAIN-FKNUM IS INITIAL.
      WA_MAIN-STATUS = 0.  "Gray
    ELSEIF WA_MAIN-BELNR IS NOT INITIAL.
      WA_MAIN-STATUS = 3.  "Green
    ELSEIF WA_MAIN-STFRE EQ 'C'.
      WA_MAIN-STATUS = 2.  "Yellow
    ELSE.
      WA_MAIN-STATUS = 1.  "Red
    ENDIF.

    IF WA_MAIN-ZFR4P IS INITIAL AND WA_MAIN-FKNUM IS INITIAL.
      WA_MAIN-STATUS = 1.  "Red
      WA_MAIN-RETXT  = 'Base Freight is empty.'.
    ENDIF.

    "----// cell style
    CLEAR: LS_CELLSTYL, LT_CELLSTYL, WA_MAIN-CELLSTYL.
    IF WA_MAIN-STABR NE 'C'.
      LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.
    LS_CELLSTYL-FIELDNAME = 'ZFR9P'.
    INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
    INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.

*S_2021/2/21 VENDOR I/V BY E00064
    CLEAR: LS_CELLSTYL, LT_CELLSTYL.
*    IF WA_MAIN-STABR NE 'C'.
    IF WA_MAIN-FKNUM IS INITIAL.
      LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ELSE.
      LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.
    LS_CELLSTYL-FIELDNAME = 'EXTI2'.
    INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
    INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
*E_2021/2/21 VENDOR I/V BY E00064

    APPEND WA_MAIN TO PT_MAIN.
    CLEAR WA_MAIN.
  ENDLOOP.

  CHECK 1 = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HEADER
*&---------------------------------------------------------------------*
*& 컨테이너 TOP 텍스트 헤더
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM HEADER CHANGING PV_HEIGHT TYPE I.
  DATA: LV_STR1     TYPE CHAR255,
        LV_STR2     TYPE CHAR255,
        LV_STR3     TYPE CHAR255,
        LV_STR4     TYPE CHAR255,
        LV_STR5     TYPE CHAR255,
        LO_DOCUMENT TYPE REF TO CL_DD_DOCUMENT.

  PV_HEIGHT = 10.

  "----// 텍스트
  CONCATENATE TEXT-T41 P_TPLST INTO LV_STR1 SEPARATED BY SPACE.
  CONCATENATE TEXT-T42 P_SHTYP INTO LV_STR2 SEPARATED BY SPACE.
  "----// 문서객체
  CREATE OBJECT LO_DOCUMENT
    EXPORTING
      STYLE = 'ALV_GRID'.
  CALL METHOD LO_DOCUMENT->SET_DOCUMENT_BACKGROUND
    EXPORTING
      PICTURE_ID = 'ALV_BACKGROUND'.
  CALL METHOD LO_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_STR1
      SAP_STYLE    = CL_DD_DOCUMENT=>SMALL
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL
      SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
      STYLE_CLASS  = SPACE.
  CALL METHOD LO_DOCUMENT->NEW_LINE.
  CALL METHOD LO_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_STR2
      SAP_STYLE    = CL_DD_DOCUMENT=>SMALL
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL
      SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
      STYLE_CLASS  = SPACE.
  IF S_TKNUM[] IS NOT INITIAL.
    PV_HEIGHT = 14.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = S_TKNUM-LOW
      IMPORTING
        OUTPUT = S_TKNUM-LOW.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = S_TKNUM-HIGH
      IMPORTING
        OUTPUT = S_TKNUM-HIGH.
    CONCATENATE TEXT-T43 S_TKNUM-LOW '-' S_TKNUM-HIGH INTO LV_STR3 SEPARATED BY SPACE.
    CALL METHOD LO_DOCUMENT->NEW_LINE.
    CALL METHOD LO_DOCUMENT->ADD_TEXT
      EXPORTING
        TEXT         = LV_STR3
        SAP_STYLE    = CL_DD_DOCUMENT=>SMALL
        SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
        SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL
        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
        STYLE_CLASS  = SPACE.
  ENDIF.
  IF S_TDLNR[] IS NOT INITIAL.
    PV_HEIGHT = 18.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = S_TDLNR-LOW
      IMPORTING
        OUTPUT = S_TDLNR-LOW.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = S_TDLNR-HIGH
      IMPORTING
        OUTPUT = S_TDLNR-HIGH.
    CONCATENATE TEXT-T44 S_TDLNR-LOW '-' S_TDLNR-HIGH INTO LV_STR4 SEPARATED BY SPACE.
    CALL METHOD LO_DOCUMENT->NEW_LINE.
    CALL METHOD LO_DOCUMENT->ADD_TEXT
      EXPORTING
        TEXT         = LV_STR4
        SAP_STYLE    = CL_DD_DOCUMENT=>SMALL
        SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
        SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL
        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
        STYLE_CLASS  = SPACE.
  ENDIF.
  IF S_DTABF[] IS NOT INITIAL.
    PV_HEIGHT = 22.
    CONCATENATE TEXT-T45 S_DTABF-LOW '-' S_DTABF-HIGH INTO LV_STR3 SEPARATED BY SPACE.
    CALL METHOD LO_DOCUMENT->NEW_LINE.
    CALL METHOD LO_DOCUMENT->ADD_TEXT
      EXPORTING
        TEXT         = LV_STR5
        SAP_STYLE    = CL_DD_DOCUMENT=>SMALL
        SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
        SAP_FONTSIZE = CL_DD_DOCUMENT=>SMALL
        SAP_EMPHASIS = CL_DD_DOCUMENT=>STRONG
        STYLE_CLASS  = SPACE.
  ENDIF.
  CALL METHOD LO_DOCUMENT->DISPLAY_DOCUMENT
    EXPORTING
      PARENT = GO_CONTAINER_0100_1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_VARIANT
*&---------------------------------------------------------------------*
*& ALV Variant
*&---------------------------------------------------------------------*
*&      <-- GS_VARIANT_0100 ALV 그리드 Variant
*&---------------------------------------------------------------------*
FORM ALV_VARIANT CHANGING PS_VARIANT LIKE GS_VARIANT_0100.
  CLEAR PS_VARIANT.
  PS_VARIANT-REPORT   = SY-REPID.
  PS_VARIANT-USERNAME = SY-UNAME.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_LAYOUT
*&---------------------------------------------------------------------*
*& ALV Layout
*&---------------------------------------------------------------------*
*&      <-- GS_LAYOUT_0100 ALV 그리드 옵션
*&---------------------------------------------------------------------*
FORM ALV_LAYOUT CHANGING PS_LAYOUT LIKE GS_LAYOUT_0100.
  CLEAR PS_LAYOUT.
  PS_LAYOUT-ZEBRA      = 'X'.        "LINE COLOR
  PS_LAYOUT-CWIDTH_OPT = ' '.        "ALV 제어: 열너비최적화
  PS_LAYOUT-NO_ROWMARK = ' '.        "행선택 가능
  PS_LAYOUT-INFO_FNAME = 'INFO'.     "ROW COLOR.
  PS_LAYOUT-CTAB_FNAME = 'CELLSCOL'. "CELL COLOR.
  PS_LAYOUT-STYLEFNAME = 'CELLSTYL'. "CELL STYLE
  PS_LAYOUT-SEL_MODE   = 'D'.
  PS_LAYOUT-EDIT       = ' '.        "편집가능
  PS_LAYOUT-EXCP_FNAME = 'STATUS'.   "신호등 컬럼 필드'
  PS_LAYOUT-EXCP_LED   = ' '.        "'X' = display LED, else traffic lights
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_TOOLBAR_EXCLUDE
*&---------------------------------------------------------------------*
*& ALV 툴바 제외 버튼
*&---------------------------------------------------------------------*
*&      <-- GT_EXCLUDE_0100 제외 버튼 목록
*&---------------------------------------------------------------------*
FORM ALV_TOOLBAR_EXCLUDE TABLES PT_EXCLUDE LIKE GT_EXCLUDE_0100.
  DATA LS_EXCLUDE TYPE UI_FUNC.
  CLEAR: PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.           "Undo
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.           "Local: Copy
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.       "Local: Copy Row
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.            "Local: Cut
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.     "Local: Delete Row
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.     "Local: Append Row
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.     "Local: Insert Row
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.          "Local: Paste
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.  "Locally: Paste new Row
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_REFRESH.            "Refresh
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.     "Local: Delete Row
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.     "Local: Append Row
  APPEND LS_EXCLUDE TO PT_EXCLUDE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_FIELDCATALOG
*&---------------------------------------------------------------------*
*& ALV 필드카탈로그
*&---------------------------------------------------------------------*
*&      <-- GT_FCAT_0100 필드카탈로그
*&---------------------------------------------------------------------*
FORM ALV_FIELDCATALOG TABLES PT_FCAT LIKE GT_FCAT_0100.
  DATA: LT_FIELDCATALOG TYPE LVC_T_FCAT,
        LS_FIELDCATALOG LIKE LINE OF LT_FIELDCATALOG.
  CLEAR PT_FCAT.
  PERFORM APPEND_CATALOG TABLES PT_FCAT USING:
    ' '  ' '  'RETXT'       TEXT-C01  'BAPIRET2'   'MESSAGE'      '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'FKNUM'       TEXT-C02  'VFKK'       'FKNUM'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'STBER'       TEXT-C03  'VFKK'       'STBER'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'STFRE'       TEXT-C04  'VFKK'       'STFRE'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'STABR'       TEXT-C05  'VFKK'       'STABR'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'TKNUM'       TEXT-C06  'VTTK'       'TKNUM'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'ADD02'       TEXT-C25  'VTTK'       'ADD02'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'STTRG'       TEXT-C22  'VTTK'       'STTRG'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'DDTEXT'      TEXT-C23  'DD07V'      'DDTEXT'       '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'TDLNR'       TEXT-C07  'VTTK'       'TDLNR'        '     '  '     '  '10'  'L' ' ',
*   ' '  ' '  'VBELN'       TEXT-c08  'VTTP'       'VBELN'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'KUNWE'       TEXT-C09  'LIKP'       'KUNNR'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'ZFR4P'       TEXT-C10  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R' ' ',
    ' '  ' '  'ZFR5P'       TEXT-C11  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R' ' ',
*   ' '  ' '  'ZFR8P'       TEXT-c12  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R' ' ',
    ' '  ' '  'ZFR9P'       TEXT-C13  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R' ' ',
    ' '  ' '  'SUMMP'       TEXT-C14  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R' ' ',
    ' '  ' '  'KONWA'       TEXT-C15  'KONP'       'KONWA'        '     '  '     '  '06'  'L' ' ',
    ' '  ' '  'KPEIN'       TEXT-C16  'KONP'       'KPEIN'        '     '  '     '  '06'  'R' 'X',
    ' '  ' '  'KMEIN'       TEXT-C17  'KONP'       'KMEIN'        'KMEIN'  '     '  '06'  'L' 'X',
*S_2021/2/25 ADD VENDOR I/V BY E00064
*    ' '  ' '  'SGTXT'       TEXT-C31  'BSEG'       'SGTXT'        '     '  '     '  '06'  'L' ' ',
    ' '  ' '  'EXTI2'       TEXT-C31  'VTTK'       'EXTI2'        '     '  '     '  '06'  'L' ' ',
*E_2021/2/25 ADD VENDOR I/V BY E00064
    ' '  ' '  'EBELN'       TEXT-C18  'VFKP'       'EBELN'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'BELNR'       TEXT-C19  'RBKP'       'BELNR'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'GJAHR'       TEXT-C20  'RBKP'       'GJAHR'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'BUDAT'       TEXT-C24  'EKBE'       'BUDAT'        '     '  '     '  '10'  'L' ' ',
    ' '  ' '  'LBLNI'       TEXT-C21  'VFKP'       'LBLNI'        '     '  '     '  '10'  'L' ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_CATALOG
*&---------------------------------------------------------------------*
*& 필드카탈로그 칼럼 속성 정의
*&---------------------------------------------------------------------*
*&      <-> PT_FCAT_0100 필드카탈로그 목록
*&      --> P_KEY        ALV 키 - 색상이 헤더스타일
*&      --> P_FIX_COLUMN 고정칼럼 - 가로스크롤바 나오더라도 고정
*&      --> P_FIELDNAME  컬럼명
*&      --> P_COLTEXT    컬럼텍스트
*&      --> P_REF_TABLE  참조테이블
*&      --> P_REF_FIELD  참조필드
*&      --> P_QFIELDNAME 수량단위
*&      --> P_CFIELDNAME 화폐단위
*&      --> P_OUTPUTLEN  필드길이
*&      --> P_JUST       정렬(좌,중,우)
*&---------------------------------------------------------------------*
FORM APPEND_CATALOG TABLES PT_FCAT LIKE GT_FCAT_0100
                    USING  "VALUE(P_COL_POS)
                           VALUE(P_KEY)
                           VALUE(P_FIX_COLUMN)
                           VALUE(P_FIELDNAME)
                           VALUE(P_COLTEXT)
                           VALUE(P_REF_TABLE)
                           VALUE(P_REF_FIELD)
                           VALUE(P_QFIELDNAME)
                           VALUE(P_CFIELDNAME)
                           VALUE(P_OUTPUTLEN)
                           VALUE(P_JUST)
                           VALUE(P_NO_OUT).
  DATA: LS_FIELDCAT TYPE LVC_S_FCAT.
  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-COL_POS    = LINES( PT_FCAT[] ) + 1.
  LS_FIELDCAT-KEY        = P_KEY.
  LS_FIELDCAT-FIX_COLUMN = P_FIX_COLUMN.
  LS_FIELDCAT-FIELDNAME  = P_FIELDNAME.
  LS_FIELDCAT-REPTEXT    = P_COLTEXT.
  LS_FIELDCAT-COLTEXT    = P_COLTEXT.
  LS_FIELDCAT-SCRTEXT_L  = P_COLTEXT.
  LS_FIELDCAT-SCRTEXT_M  = P_COLTEXT.
  LS_FIELDCAT-SCRTEXT_S  = P_COLTEXT.
  LS_FIELDCAT-REF_TABLE  = P_REF_TABLE.
  LS_FIELDCAT-REF_FIELD  = P_REF_FIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELDNAME.
  LS_FIELDCAT-CFIELDNAME = P_CFIELDNAME.
  LS_FIELDCAT-OUTPUTLEN  = P_OUTPUTLEN.
  LS_FIELDCAT-JUST       = P_JUST.
  LS_FIELDCAT-NO_OUT     = P_NO_OUT.

  IF P_FIELDNAME(5) EQ 'ZFR8P' OR
     P_FIELDNAME(5) EQ 'ZFR9P'.
    LS_FIELDCAT-EDIT = 'X'.
  ENDIF.
*S_2021/2/25 ADD VENDOR I/V BY E00064
*  IF P_FIELDNAME EQ 'SGTXT'.
  IF P_FIELDNAME EQ 'EXTI2'.
    LS_FIELDCAT-EDIT = 'X'.
  ENDIF.
*E_2021/2/25 ADD VENDOR I/V BY E00064
  APPEND LS_FIELDCAT TO PT_FCAT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_EVENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GO_GRID_0100
*&---------------------------------------------------------------------*
FORM ALV_EVENT  USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID.
  "----// 이벤트 객체 생성
  CREATE OBJECT GO_EVENT_RECEIVER_0100.
  "----// 이벤트 객체에 핸들러 등록
  SET HANDLER GO_EVENT_RECEIVER_0100->HANDLE_TOOLBAR               FOR PO_GRID.
  SET HANDLER GO_EVENT_RECEIVER_0100->HANDLE_DOUBLE_CLICK          FOR PO_GRID.
  SET HANDLER GO_EVENT_RECEIVER_0100->HANDLE_DATA_CHANGED          FOR PO_GRID.
  SET HANDLER GO_EVENT_RECEIVER_0100->HANDLE_DATA_CHANGED_FINISHED FOR PO_GRID.
  SET HANDLER GO_EVENT_RECEIVER_0100->HANDLE_USER_COMMAND          FOR PO_GRID.
  "----// 편집 이벤트 등록
  CALL METHOD PO_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
  CALL METHOD PO_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_LAYOUT_0100
*&      --> GS_VARIANT_0100
*&      --> GT_EXCLUDE_0100
*&      --> GT_FCAT_0100
*&      --> GT_SORT_0100
*&      --> GT_MAIN_W[]
*&---------------------------------------------------------------------*
FORM ALV_DISPLAY TABLES PT_FCAT    LIKE GT_FCAT_0100
                        PT_SORT    LIKE GT_SORT_0100
                        PT_TAB     TYPE TABLE
                 USING  PS_LAYOUT  TYPE LVC_S_LAYO
                        PS_VARIANT TYPE DISVARIANT
                        PT_EXCLUDE TYPE UI_FUNCTIONS
                        PO_GRID    TYPE REF TO CL_GUI_ALV_GRID.
  "----// 편집모드 결정
  CALL METHOD PO_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.
  "----// 그리드 출력
  CALL METHOD PO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT            = 'X'
      IS_LAYOUT            = PS_LAYOUT
      IS_VARIANT           = PS_VARIANT
      IT_TOOLBAR_EXCLUDING = PT_EXCLUDE
      I_SAVE               = 'A'
    CHANGING
      IT_OUTTAB            = PT_TAB[]
      IT_FIELDCATALOG      = PT_FCAT[]
      IT_SORT              = PT_SORT[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       BACK, EXIT, CANC
*----------------------------------------------------------------------*
MODULE EXIT_COMMAND_0100 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      SET SCREEN 0.  LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.  LEAVE SCREEN.
    WHEN 'CANC'.
      SET SCREEN 0.  LEAVE SCREEN.
    WHEN OTHERS.
      CHECK 1 = 1.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&      --> E_UCOMM
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED  USING  PO_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                 PV_UCOMM        TYPE SY-UCOMM.
  "----// 변수선언
  DATA: WA_MOD_CELL TYPE LVC_S_MODI.
  DATA: LV_ZFR9P TYPE KONP-KBETR, "Etc Fee      기타운임
        LV_SUMMP TYPE KONP-KBETR. "Summation    합계
  LOOP AT PO_DATA_CHANGED->MT_GOOD_CELLS INTO WA_MOD_CELL.
    READ TABLE GT_MAIN INTO WA_MAIN INDEX WA_MOD_CELL-ROW_ID.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    CASE WA_MOD_CELL-FIELDNAME(5).
      WHEN 'ZFR9P'.
        LV_ZFR9P = WA_MOD_CELL-VALUE.
        LV_SUMMP = WA_MAIN-ZFR4P + WA_MAIN-ZFR5P + WA_MAIN-ZFR8P + LV_ZFR9P.
        CALL METHOD PO_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = WA_MOD_CELL-ROW_ID
            I_FIELDNAME = 'SUMMP'
            I_VALUE     = LV_SUMMP.
      WHEN OTHERS.
        CHECK 1 = 1.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_OBJECT
*&      --> E_INTERACTIVE
*&---------------------------------------------------------------------*
FORM HANDLE_TOOLBAR USING PO_OBJECT      TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                          PV_INTERACTIVE TYPE CHAR01.
  DATA : LS_TOOLBAR TYPE STB_BUTTON,
         LV_TABIX   TYPE SY-TABIX.
  DELETE PO_OBJECT->MT_TOOLBAR WHERE FUNCTION EQ '&DETAIL'.

  IF SY-TCODE NE 'ZSDR0071'.  " Display invoice completed data only

    "----// create/update shipment cost 버튼 만들기
    LV_TABIX = LV_TABIX + 1.
    CLEAR: LS_TOOLBAR.
    LS_TOOLBAR-FUNCTION  = TEXT-T01.
    LS_TOOLBAR-ICON      = ICON_CREATE.
    LS_TOOLBAR-QUICKINFO = TEXT-T02.
    LS_TOOLBAR-TEXT      = TEXT-T02.
    LS_TOOLBAR-DISABLED  = ' '.
    INSERT LS_TOOLBAR INTO PO_OBJECT->MT_TOOLBAR INDEX LV_TABIX.
    "----// transfer 버튼 만들기
    LV_TABIX = LV_TABIX + 1.
    CLEAR: LS_TOOLBAR.
    LS_TOOLBAR-FUNCTION  = TEXT-T03.
    LS_TOOLBAR-ICON      = ICON_WS_TRANSFER.
    LS_TOOLBAR-QUICKINFO = TEXT-T04.
    LS_TOOLBAR-TEXT      = TEXT-T04.
    LS_TOOLBAR-DISABLED  = ' '.
    INSERT LS_TOOLBAR INTO PO_OBJECT->MT_TOOLBAR INDEX LV_TABIX.
    "----// invoice 버튼 만들기
    LV_TABIX = LV_TABIX + 1.
    CLEAR: LS_TOOLBAR.
    LS_TOOLBAR-FUNCTION  = TEXT-T05.
    LS_TOOLBAR-ICON      = ICON_FINANCING.
    LS_TOOLBAR-QUICKINFO = TEXT-T06.
    LS_TOOLBAR-TEXT      = TEXT-T06.
    LS_TOOLBAR-DISABLED  = ' '.
    INSERT LS_TOOLBAR INTO PO_OBJECT->MT_TOOLBAR INDEX LV_TABIX.
    "----// delete shipment 버튼 만들기
    LV_TABIX = LV_TABIX + 1.
    CLEAR: LS_TOOLBAR.
    LS_TOOLBAR-FUNCTION  = TEXT-T07.
    LS_TOOLBAR-ICON      = ICON_DELETE.
    LS_TOOLBAR-QUICKINFO = TEXT-T08.
    LS_TOOLBAR-TEXT      = TEXT-T08.
    LS_TOOLBAR-DISABLED  = ' '.
    INSERT LS_TOOLBAR INTO PO_OBJECT->MT_TOOLBAR INDEX LV_TABIX.
    "----// cancel transfer 버튼 만들기
    LV_TABIX = LV_TABIX + 1.
    CLEAR: LS_TOOLBAR.
    LS_TOOLBAR-FUNCTION  = TEXT-T09.
    LS_TOOLBAR-ICON      = ICON_DELETE.
    LS_TOOLBAR-QUICKINFO = TEXT-T10.
    LS_TOOLBAR-TEXT      = TEXT-T10.
    LS_TOOLBAR-DISABLED  = ' '.
    INSERT LS_TOOLBAR INTO PO_OBJECT->MT_TOOLBAR INDEX LV_TABIX.
    "----// cancel invoice 버튼 만들기
    LV_TABIX = LV_TABIX + 1.
    CLEAR: LS_TOOLBAR.
    LS_TOOLBAR-FUNCTION  = TEXT-T11.
    LS_TOOLBAR-ICON      = ICON_DELETE.
    LS_TOOLBAR-QUICKINFO = TEXT-T12.
    LS_TOOLBAR-TEXT      = TEXT-T12.
    LS_TOOLBAR-DISABLED  = ' '.
    INSERT LS_TOOLBAR INTO PO_OBJECT->MT_TOOLBAR INDEX LV_TABIX.
  ENDIF.

  "----// refresh 버튼 만들기
  LV_TABIX = LV_TABIX + 1.
  CLEAR: LS_TOOLBAR.
  LS_TOOLBAR-FUNCTION  = TEXT-T13.
  LS_TOOLBAR-ICON      = ICON_REFRESH.
  LS_TOOLBAR-QUICKINFO = TEXT-T14.
  LS_TOOLBAR-TEXT      = TEXT-T14.
  LS_TOOLBAR-DISABLED  = ' '.
  INSERT LS_TOOLBAR INTO PO_OBJECT->MT_TOOLBAR INDEX LV_TABIX.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& 툴바 버튼 기능 구현
*&---------------------------------------------------------------------*
*&      --> E_UCOMM 클릭한 툴바 버튼
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING PV_UCOMM TYPE SY-UCOMM.
  "----// 변수선언
  DATA: LT_MAIN TYPE TABLE OF TY_MAIN.
  DATA: LT_SELECTED TYPE LVC_T_ROW WITH HEADER LINE,
        LV_RETCD    TYPE CHAR01,
        LV_RETXT    TYPE TEXT100.
  "----// 버튼 기능 구현
  CASE PV_UCOMM.
    WHEN 'CREA'.
      CLEAR: LT_SELECTED, LV_RETCD, LV_RETXT.
      CALL METHOD GO_GRID_0100->GET_SELECTED_ROWS
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
        IF SY-SUBRC EQ 0 AND WA_MAIN-STATUS = 1.
          MESSAGE S001 WITH TEXT-009 DISPLAY LIKE 'E'.

          RETURN.
        ENDIF.
        CLEAR WA_MAIN.
      ENDLOOP.

      PERFORM POP_UP USING    TEXT-002  TEXT-003
                     CHANGING GV_ANSWER.
      IF GV_ANSWER = '1'.
        PERFORM CREATE_SHIPMENT_COST TABLES   GT_MAIN   LT_SELECTED
                                     CHANGING LV_RETCD  LV_RETXT     GV_CHGED.
        IF LV_RETCD EQ 'E'.
          CHECK 1 = 1.
        ELSE.
          MESSAGE S007(VY) INTO LV_RETXT WITH 'All'.
        ENDIF.
        MESSAGE LV_RETXT TYPE 'S' DISPLAY LIKE LV_RETCD.
      ELSE.
        CHECK 1 = 1.
      ENDIF.
    WHEN 'TRAN'.
      CLEAR: LT_SELECTED, LV_RETCD, LV_RETXT.
      CALL METHOD GO_GRID_0100->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = LT_SELECTED[].
      IF LT_SELECTED[] IS INITIAL.
        MESSAGE S004.
        EXIT.
      ENDIF.
      PERFORM POP_UP USING    TEXT-002  TEXT-004
                     CHANGING GV_ANSWER.
      IF GV_ANSWER = '1'.
        PERFORM TRANSFER TABLES   GT_MAIN   LT_SELECTED
                         CHANGING LV_RETCD  LV_RETXT     GV_CHGED.
        IF LV_RETCD EQ 'E'.
          CHECK 1 = 1.
        ELSE.
          MESSAGE S007(VY) INTO LV_RETXT WITH 'All'.
        ENDIF.
        MESSAGE LV_RETXT TYPE 'S' DISPLAY LIKE LV_RETCD.
      ELSE.
        CHECK 1 = 1.
      ENDIF.
    WHEN 'INVO'.
      CLEAR: LT_SELECTED, LV_RETCD, LV_RETXT.
      CALL METHOD GO_GRID_0100->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = LT_SELECTED[].
      IF LT_SELECTED[] IS INITIAL.
        MESSAGE S004.
        EXIT.
      ENDIF.
      PERFORM POP_UP USING    TEXT-002  TEXT-006
                     CHANGING GV_ANSWER.
      IF GV_ANSWER = '1'.
        PERFORM CREATE_INVOICE TABLES   GT_MAIN   LT_SELECTED
                               CHANGING LV_RETCD  LV_RETXT     GV_CHGED.
        IF LV_RETCD EQ 'E'.
          CHECK 1 = 1.
        ELSE.
          MESSAGE S060(M8) INTO LV_RETXT WITH 'All'.
        ENDIF.
        MESSAGE LV_RETXT TYPE 'S' DISPLAY LIKE LV_RETCD.
      ELSE.
        CHECK 1 = 1.
      ENDIF.
    WHEN 'DEL_SHIP'.
      CLEAR: LT_SELECTED, LV_RETCD, LV_RETXT.
      CALL METHOD GO_GRID_0100->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = LT_SELECTED[].
      IF LT_SELECTED[] IS INITIAL.
        MESSAGE S004.
        EXIT.
      ENDIF.
      PERFORM POP_UP USING    TEXT-002  TEXT-008
                     CHANGING GV_ANSWER.
      IF GV_ANSWER = '1'.
        PERFORM DELETE_SHIPMENT_COST TABLES   GT_MAIN   LT_SELECTED
                                     CHANGING LV_RETCD  LV_RETXT     GV_CHGED.
        IF LV_RETCD EQ 'E'.
          CHECK 1 = 1.
        ELSE.
          MESSAGE S013(VY) INTO LV_RETXT WITH 'All'.
        ENDIF.
        MESSAGE LV_RETXT TYPE 'S' DISPLAY LIKE LV_RETCD.
      ELSE.
        CHECK 1 = 1.
      ENDIF.
    WHEN 'CAN_TRAN'.
      CLEAR: LT_SELECTED, LV_RETCD, LV_RETXT.
      CALL METHOD GO_GRID_0100->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = LT_SELECTED[].
      IF LT_SELECTED[] IS INITIAL.
        MESSAGE S004.
        EXIT.
      ENDIF.
      PERFORM POP_UP USING    TEXT-002  TEXT-005
                     CHANGING GV_ANSWER.
      IF GV_ANSWER = '1'.
        PERFORM CANCEL_TRANSFER TABLES   GT_MAIN   LT_SELECTED
                                CHANGING LV_RETCD  LV_RETXT     GV_CHGED.
        IF LV_RETCD EQ 'E'.
          CHECK 1 = 1.
        ELSE.
          MESSAGE S007(VY) INTO LV_RETXT WITH 'All'.
        ENDIF.
        MESSAGE LV_RETXT TYPE 'S' DISPLAY LIKE LV_RETCD.
      ELSE.
        CHECK 1 = 1.
      ENDIF.
    WHEN 'CAN_INVO'.
      CLEAR: LT_SELECTED, LV_RETCD, LV_RETXT.
      CALL METHOD GO_GRID_0100->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = LT_SELECTED[].
      IF LT_SELECTED[] IS INITIAL.
        MESSAGE S004.
        EXIT.
      ENDIF.
      PERFORM POP_UP USING    TEXT-002  TEXT-007
                     CHANGING GV_ANSWER.
      IF GV_ANSWER = '1'.
        PERFORM CANCEL_INVOICE TABLES   GT_MAIN   LT_SELECTED
                               CHANGING LV_RETCD  LV_RETXT     GV_CHGED.
        IF LV_RETCD EQ 'E'.
          CHECK 1 = 1.
        ELSE.
          MESSAGE S007(VY) INTO LV_RETXT WITH 'All'.
        ENDIF.
        MESSAGE LV_RETXT TYPE 'S' DISPLAY LIKE LV_RETCD.
      ELSE.
        CHECK 1 = 1.
      ENDIF.
    WHEN 'REFRESH'.
      PERFORM GET_SHIPMENT_COST TABLES GT_MAIN.
    WHEN OTHERS.
      CHECK 1 = 1.
  ENDCASE.
*  "----// ALV 갱신
*  CALL METHOD go_grid_0100->refresh_table_display
*    EXCEPTIONS
*      finished = 1
*      OTHERS   = 2.
  "----// 그리드 새로고침
  DATA: LS_STBL TYPE LVC_S_STBL.
  CLEAR LS_STBL.
*    ls_stbl-row = 'X'.
*    ls_stbl-col = 'X'.
  CALL METHOD GO_GRID_0100->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STBL.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form POP_UP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GV_ANSWER
*&---------------------------------------------------------------------*
FORM POP_UP USING    PV_TITLEBAR TYPE STRING
                     PV_QUESTION TYPE STRING
            CHANGING PV_ANSWER   LIKE GV_ANSWER.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = PV_TITLEBAR
      TEXT_QUESTION         = PV_QUESTION
      TEXT_BUTTON_1         = 'Yes'(027)
      ICON_BUTTON_1         = ' '
      TEXT_BUTTON_2         = 'Nein'(028)
      ICON_BUTTON_2         = ' '
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = 'X'
    IMPORTING
      ANSWER                = PV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_SHIPMENT_COST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN
*&      --> LT_SELECTED
*&      <-- LV_RETCD
*&      <-- LV_RETXT
*&      <-- GV_CHGED
*&---------------------------------------------------------------------*
FORM CREATE_SHIPMENT_COST TABLES   PT_MAIN     LIKE GT_MAIN
                                   PT_SELECTED TYPE LVC_T_ROW
                          CHANGING PV_RETCD    TYPE CHAR01
                                   PV_RETXT    TYPE TEXT100
                                   PV_CHGED    TYPE C.
  "----// 변수선언
  DATA: LV_ZFR9P    TYPE KBETR.  "가격 ZFR4P - etc fee
  DATA: LV_ERROR    TYPE C.

*S_2021/2/25 ADD VENDOR I/V BY E00064
*  DATA: LV_SGTXT    TYPE SGTXT.  "
  DATA: LV_EXTI2    TYPE VTTK-EXTI2.  "
  "----// 신호등 & 편집셀 설정
  DATA: LT_CELLSTYL TYPE LVC_T_STYL,
        LS_CELLSTYL TYPE LVC_S_STYL,
        LV_IDX      TYPE SY-TABIX.
*E_2021/2/25 ADD VENDOR I/V BY E00064

*S_2021/3/26 BY E00064
*ZFR4, ZFR5 금액을 각각 export memory함 memoryid는 ZFR4P, ZFR5P
  DATA: LV_ZFR4P    TYPE KBETR.  "가격 ZFR4P -
  DATA: LV_ZFR5P    TYPE KBETR.  "가격 ZFR4P -
*E_2021/3/26 BY E00064

  "----// BDC 변수
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA    WITH HEADER LINE.
  DATA: LT_MESSTAB TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
  DATA: LS_OPT     TYPE CTU_PARAMS.

  CLEAR LV_ERROR.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Shipment Cost 생성
  "////
  "///////////////////////////////////////////////////////////////////
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    IF WA_MAIN-FKNUM IS NOT INITIAL AND WA_MAIN-STABR NE 'A'.
      PV_RETCD = 'E'.
      PV_RETXT = TEXT-T61.
      PV_CHGED = ' '.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF PV_RETCD EQ 'E'.
    EXIT.
  ENDIF.
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    "----// ZFR4P 세팅
    CLEAR LV_ZFR9P.
    LV_ZFR9P = WA_MAIN-ZFR9P.
    EXPORT LV_ZFR9P = LV_ZFR9P TO MEMORY ID 'ZFR9P'.

*S_2021/2/25 ADD VENDOR I/V BY E00064
    LV_EXTI2 = WA_MAIN-EXTI2.
*    LV_SGTXT = WA_MAIN-SGTXT.
    EXPORT LV_EXTI2 = LV_EXTI2 TO MEMORY ID 'ZEXTI2'.
*E_2021/2/25 ADD VENDOR I/V BY E00064

*S_2021/3/26 BY E00064
    LV_ZFR4P = WA_MAIN-ZFR4P.
    LV_ZFR5P = WA_MAIN-ZFR5P.
    EXPORT LV_ZFR4P = LV_ZFR4P TO MEMORY ID 'ZFR4P'.
    EXPORT LV_ZFR5P = LV_ZFR5P TO MEMORY ID 'ZFR5P'.
*E_2021/3/26 BY E00064


    "----// BDC
    CLEAR: LT_BDCDATA[], LT_BDCDATA,
           LT_MESSTAB[], LT_MESSTAB.
    IF WA_MAIN-FKNUM IS INITIAL.  "생성
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0010'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VTTK-TKNUM'            WA_MAIN-TKNUM.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=UEBP'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0030'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_CURSOR'            'VFKP-FKPOS(1)'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=PDET'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0040'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=KSAC'.
*      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLV54K'              '0100'.
*      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VFKN-SAKTO'            '51116101'.
*      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '/00'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLV54K'              '0200'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=SICH'.
      CALL TRANSACTION 'VI01' WITH AUTHORITY-CHECK USING LT_BDCDATA MESSAGES INTO LT_MESSTAB OPTIONS FROM LS_OPT.
    ELSE.  "변경
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0020'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VFKK-FKNUM'            WA_MAIN-FKNUM.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=UEBP'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0030'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_CURSOR'            'VFKP-FKPOS(1)'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=PDET'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0040'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=KONB'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLSPO1'              '0100'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=YES'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0040'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=KSAC'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLV54K'              '0100'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VFKN-SAKTO'            '51116101'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '/00'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLV54K'              '0200'.
      PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=SICH'.
      CALL TRANSACTION 'VI02' WITH AUTHORITY-CHECK USING LT_BDCDATA MESSAGES INTO LT_MESSTAB OPTIONS FROM LS_OPT.
    ENDIF.
    READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'S'
                                   MSGID  = 'VY'
                                   MSGNR  = '007'.
    IF SY-SUBRC EQ 0.
      WA_MAIN-RETCD  = 'S'.
      WA_MAIN-RETXT  = ' '.
      WA_MAIN-STATUS = '2'.
      WA_MAIN-FKNUM  =  LT_MESSTAB-MSGV1.
      WA_MAIN-STBER  = 'C'.
      WA_MAIN-STFRE  = 'C'.
      WA_MAIN-STABR  = 'A'.
*S_2021/2/21 VENDOR I/V BY E00064
      "----// cell style
      CLEAR: LS_CELLSTYL, LT_CELLSTYL, WA_MAIN-CELLSTYL.
      IF WA_MAIN-STABR NE 'C'.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_CELLSTYL-FIELDNAME = 'ZFR9P'.
      INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
      INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
      " Vendor I/V
      CLEAR: LS_CELLSTYL, LT_CELLSTYL.
      IF WA_MAIN-FKNUM IS INITIAL.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_CELLSTYL-FIELDNAME = 'EXTI2'.
      INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
      INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
*E_2021/2/21 VENDOR I/V BY E00064
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING FKNUM
                                               STBER
                                               STFRE
                                               STABR
                                               STATUS
                                               RETCD
                                               RETXT
*S_2021/2/21 VENDOR I/V BY E00064
                                               CELLSTYL.
*E_2021/2/21 VENDOR I/V BY E00064
    ELSE.
      READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'E'.
      IF SY-SUBRC NE 0.
        READ TABLE LT_MESSTAB INDEX 1.
      ENDIF.
      WA_MAIN-RETCD  = 'E'.
      MESSAGE ID     LT_MESSTAB-MSGID
              TYPE   LT_MESSTAB-MSGTYP
              NUMBER LT_MESSTAB-MSGNR
              INTO   WA_MAIN-RETXT
              WITH   LT_MESSTAB-MSGV1  LT_MESSTAB-MSGV2  LT_MESSTAB-MSGV3  LT_MESSTAB-MSGV4.
      WA_MAIN-STATUS = '1'.
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STATUS
                                               RETCD
                                               RETXT.

      LV_ERROR = 'X'.
    ENDIF.
  ENDLOOP.
*  IF LINE_EXISTS( PT_MAIN[ STATUS = '1' ] ).
  IF LV_ERROR IS NOT INITIAL.
    PV_RETCD = 'E'.
    PV_RETXT = TEXT-T62.
    EXIT.
  ENDIF.
  PV_RETCD = 'S'.
  PV_RETXT = ' '.
  PV_CHGED = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form TRANSFER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN
*&      --> LT_SELECTED
*&      <-- LV_RETCD
*&      <-- LV_RETXT
*&      <-- GV_CHGED
*&---------------------------------------------------------------------*
FORM TRANSFER TABLES   PT_MAIN     LIKE GT_MAIN
                       PT_SELECTED TYPE LVC_T_ROW
              CHANGING PV_RETCD    TYPE CHAR01
                       PV_RETXT    TYPE TEXT100
                       PV_CHGED    TYPE C.

  DATA: LV_ERROR    TYPE C.
  "----// 신호등 & 편집셀 설정
  DATA: LT_CELLSTYL TYPE LVC_T_STYL,
        LS_CELLSTYL TYPE LVC_S_STYL,
        LV_IDX      TYPE SY-TABIX.
  "----// BDC 변수
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA    WITH HEADER LINE.
  DATA: LT_MESSTAB TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
  DATA: LS_OPT     TYPE CTU_PARAMS.

  CLEAR LV_ERROR.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Transfer
  "////
  "///////////////////////////////////////////////////////////////////
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    IF WA_MAIN-STABR NE 'A'.
      PV_RETCD = 'E'.
      PV_RETXT = TEXT-T63.
      PV_CHGED = ' '.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF PV_RETCD EQ 'E'.
    EXIT.
  ENDIF.
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    "----// BDC
    CLEAR: LT_BDCDATA[], LT_BDCDATA,
           LT_MESSTAB[], LT_MESSTAB.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0020'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VFKK-FKNUM'            WA_MAIN-FKNUM.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=UEBP'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0030'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_CURSOR'            'VFKP-FKPOS(1)'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=PDET'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0040'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=PABR'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0040'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VFKPD-SLFREI'          'X'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=SICH'.
    CALL TRANSACTION 'VI02' WITH AUTHORITY-CHECK USING LT_BDCDATA MESSAGES INTO LT_MESSTAB OPTIONS FROM LS_OPT.
    READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC NE 0.
      READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'S'
                                     MSGID  = 'VY'
                                     MSGNR  = '007'.
      "----// Transfer 후 생성된 구매오더 및 엔트리번호 -----------------
      DO 5 TIMES.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_MAIN-FKNUM
          IMPORTING
            OUTPUT = WA_MAIN-FKNUM.

        SELECT SINGLE
               EBELN,
               LBLNI
          INTO (@WA_MAIN-EBELN, @WA_MAIN-LBLNI)
          FROM VFKP
         WHERE FKNUM EQ @WA_MAIN-FKNUM.
        IF WA_MAIN-EBELN IS NOT INITIAL.
          EXIT.
        ELSE.
          WAIT UP TO '0.5' SECONDS.
        ENDIF.
      ENDDO.
      "------------------------------------------------------------------
      IF WA_MAIN-EBELN IS NOT INITIAL.
        WA_MAIN-RETCD  = 'S'.
        WA_MAIN-RETXT  = ' '.
        WA_MAIN-STABR  = 'C'.
        "----// cell style
        CLEAR: LS_CELLSTYL, LT_CELLSTYL, WA_MAIN-CELLSTYL.
        IF WA_MAIN-STABR NE 'C'.
          LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ENDIF.
        LS_CELLSTYL-FIELDNAME = 'ZFR9P'.
        INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
        INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
*S_2021/2/21 VENDOR I/V BY E00064
        CLEAR: LS_CELLSTYL, LT_CELLSTYL.
        IF WA_MAIN-FKNUM IS INITIAL.
          LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ENDIF.
        LS_CELLSTYL-FIELDNAME = 'EXTI2'.
        INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
        INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
*E_2021/2/21 VENDOR I/V BY E00064

        MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                    TRANSPORTING STABR
                                                 EBELN
                                                 LBLNI
                                                 RETCD
                                                 RETXT
                                                 CELLSTYL.
      ELSE. "Purchasing doc 가 생성이 안된 경우
        WA_MAIN-STATUS = '1'.
        WA_MAIN-RETCD  = 'E'.
        MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                    TRANSPORTING STATUS
                                                 RETCD.
      ENDIF.



    ELSE.
      WA_MAIN-RETCD  = 'E'.
      MESSAGE ID     LT_MESSTAB-MSGID
              TYPE   LT_MESSTAB-MSGTYP
              NUMBER LT_MESSTAB-MSGNR
              INTO   WA_MAIN-RETXT
              WITH   LT_MESSTAB-MSGV1  LT_MESSTAB-MSGV2  LT_MESSTAB-MSGV3  LT_MESSTAB-MSGV4.
      WA_MAIN-STATUS = '1'.
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STATUS
                                               RETCD
                                               RETXT.
      LV_ERROR = 'X'.
    ENDIF.
  ENDLOOP.
*  IF LINE_EXISTS( PT_MAIN[ STATUS = '1' ] ).
  IF LV_ERROR IS NOT INITIAL.
    PV_RETCD = 'E'.
    PV_RETXT = TEXT-T62.
    EXIT.
  ENDIF.
  PV_RETCD = 'S'.
  PV_RETXT = ' '.
  PV_CHGED = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL_TRANSFER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN
*&      --> LT_SELECTED
*&      <-- LV_RETCD
*&      <-- LV_RETXT
*&      <-- GV_CHGED
*&---------------------------------------------------------------------*
FORM CANCEL_TRANSFER TABLES   PT_MAIN     LIKE GT_MAIN
                              PT_SELECTED TYPE LVC_T_ROW
                     CHANGING PV_RETCD    TYPE CHAR01
                              PV_RETXT    TYPE TEXT100
                              PV_CHGED    TYPE C.
  DATA: LV_ERROR TYPE C.

  "----// 신호등 & 편집셀 설정
  DATA: LT_CELLSTYL TYPE LVC_T_STYL,
        LS_CELLSTYL TYPE LVC_S_STYL,
        LV_IDX      TYPE SY-TABIX.
  "----// BDC 변수
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA    WITH HEADER LINE.
  DATA: LT_MESSTAB TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
  DATA: LS_OPT     TYPE CTU_PARAMS.

  CLEAR LV_ERROR.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Transfer 취소
  "////
  "///////////////////////////////////////////////////////////////////
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    IF WA_MAIN-STABR EQ 'C' AND WA_MAIN-BELNR IS INITIAL.
      CHECK 1 = 1.
    ELSE.
      PV_RETCD = 'E'.
      PV_RETXT = TEXT-T64.
      PV_CHGED = ' '.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF PV_RETCD EQ 'E'.
    EXIT.
  ENDIF.
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    "----// BDC
    CLEAR: LT_BDCDATA[], LT_BDCDATA,
           LT_MESSTAB[], LT_MESSTAB.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0020'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VFKK-FKNUM'            WA_MAIN-FKNUM.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=UEBP'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0030'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_CURSOR'            'VFKP-FKPOS(1)'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=PDET'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0040'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=PABR'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0040'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VFKPD-SLSTOR'          'X'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=SICH'.
    CALL TRANSACTION 'VI02' WITH AUTHORITY-CHECK USING LT_BDCDATA MESSAGES INTO LT_MESSTAB OPTIONS FROM LS_OPT.
    READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC NE 0.
      READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'S'
                                     MSGID  = 'VY'
                                     MSGNR  = '007'.
      WA_MAIN-RETCD  = 'S'.
      WA_MAIN-RETXT  = ' '.
      WA_MAIN-STABR  = 'A'.
      WA_MAIN-EBELN  = ' '.
      WA_MAIN-LBLNI  = ' '.
      "----// cell style
      CLEAR: LS_CELLSTYL, LT_CELLSTYL, WA_MAIN-CELLSTYL.
      IF WA_MAIN-STABR NE 'C'.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_CELLSTYL-FIELDNAME = 'ZFR9P'.
      INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
      INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
*S_2021/2/21 VENDOR I/V BY E00064
      CLEAR: LS_CELLSTYL, LT_CELLSTYL.
      IF WA_MAIN-FKNUM IS INITIAL.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_CELLSTYL-FIELDNAME = 'EXTI2'.
      INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
      INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
*E_2021/2/21 VENDOR I/V BY E00064

      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STABR
                                               EBELN
                                               LBLNI
                                               RETCD
                                               RETXT
                                               CELLSTYL.
    ELSE.
      WA_MAIN-RETCD  = 'E'.
      MESSAGE ID     LT_MESSTAB-MSGID
              TYPE   LT_MESSTAB-MSGTYP
              NUMBER LT_MESSTAB-MSGNR
              INTO   WA_MAIN-RETXT
              WITH   LT_MESSTAB-MSGV1  LT_MESSTAB-MSGV2  LT_MESSTAB-MSGV3  LT_MESSTAB-MSGV4.
      WA_MAIN-STATUS = '1'.
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STATUS
                                               RETCD
                                               RETXT.

      LV_ERROR = 'X'.
    ENDIF.
  ENDLOOP.
*  IF LINE_EXISTS( PT_MAIN[ STATUS = '1' ] ).
  IF LV_ERROR IS NOT INITIAL.
    PV_RETCD = 'E'.
    PV_RETXT = TEXT-T62.
    EXIT.
  ENDIF.
  PV_RETCD = 'S'.
  PV_RETXT = ' '.
  PV_CHGED = ' '.
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
*& Form CREATE_INVOICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN
*&      --> LT_SELECTED
*&      <-- LV_RETCD
*&      <-- LV_RETXT
*&      <-- GV_CHGED
*&---------------------------------------------------------------------*
FORM CREATE_INVOICE TABLES   PT_MAIN     LIKE GT_MAIN
                             PT_SELECTED TYPE LVC_T_ROW
                    CHANGING PV_RETCD    TYPE CHAR01
                             PV_RETXT    TYPE TEXT100
                             PV_CHGED    TYPE C.
  "----// 변수선언
*  DATA: ls_headerdata       TYPE bapi_incinv_create_header,
*        lv_invoicedocnumber TYPE bapi_incinv_fld-inv_doc_no,
*        lv_fiscalyear       TYPE bapi_incinv_fld-fisc_year,
*        lt_itemdata         TYPE TABLE OF bapi_incinv_create_item WITH HEADER LINE,
*        lt_accountingdata   TYPE TABLE OF bapi_incinv_create_account WITH HEADER LINE,
*        lt_return           TYPE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: LT_EKBE TYPE TABLE OF EKBE WITH HEADER LINE.

  DATA: LV_BUKRS TYPE BUKRS,
        LV_DATLO TYPE TEXT20,
        LV_SUMMP TYPE TEXT20,
        LV_BELNR TYPE BELNR_D,
        LV_ERROR TYPE C.
  "----// BDC 변수
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA    WITH HEADER LINE.
  DATA: LT_MESSTAB TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
  DATA: LS_OPT     TYPE CTU_PARAMS.

  CLEAR LV_ERROR.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Invoice 생성
  "////
  "///////////////////////////////////////////////////////////////////
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    IF WA_MAIN-EBELN IS NOT INITIAL AND WA_MAIN-BELNR IS INITIAL.
      CHECK 1 = 1.
    ELSE.
      PV_RETCD = 'E'.
      PV_RETXT = TEXT-T65.
      PV_CHGED = ' '.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF PV_RETCD EQ 'E'.
    EXIT.
  ENDIF.
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
*    "////////////////////////////////////////////////////////////////
*    "////
*    "//// INVOICE BAPI 호출 로직
*    "////
*    "////////////////////////////////////////////////////////////////
*    "----// 변수 초기하
*    CLEAR: ls_headerdata, lv_invoicedocnumber, lv_fiscalyear, lt_itemdata, lt_itemdata[], lt_return, lt_return[].
*    "----// 변수 값 설정
*    CLEAR: lv_bukrs.
*    SELECT SINGLE
*           k~bukrs
*      INTO @lv_bukrs
*      FROM ekko AS k INNER JOIN ekpo AS p ON p~ebeln = k~ebeln
*     WHERE p~ebeln EQ @wa_main-ebeln
*       AND p~ebelp EQ @wa_main-ebelp.
*    ls_headerdata-invoice_ind  = 'X'.
*    ls_headerdata-doc_type     = 'RE'.
*    ls_headerdata-doc_date     = sy-datlo.
*    ls_headerdata-ref_doc_no   = wa_main-lblni.
*    ls_headerdata-pstng_date   = p_postd.
*    ls_headerdata-comp_code    = lv_bukrs.
*    ls_headerdata-currency     = wa_main-konwa.
*    ls_headerdata-gross_amount = wa_main-summp.
*    ls_headerdata-exch_rate    = '1.00000'.
*    ls_headerdata-bline_date   = sy-datlo.
*    lt_itemdata-invoice_doc_item = '000001'.
*    lt_itemdata-po_number = wa_main-ebeln.
*    lt_itemdata-po_item = wa_main-ebelp.
*    lt_itemdata-tax_code = ' '.
*    lt_itemdata-item_amount = wa_main-summp.
*    lt_itemdata-quantity = 0.
*    lt_itemdata-po_unit = ' '.
*    lt_itemdata-ref_doc = wa_main-lblni.
*    lt_itemdata-ref_doc_it = ' '.
*    lt_itemdata-ref_doc_year = p_postd(4).
*    lt_itemdata-sheet_no = wa_main-lblni.
*    "lt_itemdata-sheet_item = '0000000010'.
*    APPEND lt_itemdata.
*    "----// BAPI Call
*    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
*      EXPORTING
*        headerdata       = ls_headerdata
*      IMPORTING
*        invoicedocnumber = lv_invoicedocnumber
*        fiscalyear       = lv_fiscalyear
*      TABLES
*        itemdata         = lt_itemdata
*        accountingdata   = lt_accountingdata
*        return           = lt_return.
*    IF lv_invoicedocnumber IS NOT INITIAL.  "Success
*      wa_main-status = '3'.
*      wa_main-retcd  = 'S'.
*      wa_main-retxt  = ' '.
*      wa_main-belnr  = lv_invoicedocnumber.
*      wa_main-gjahr  = lv_fiscalyear.
*      MODIFY pt_main FROM wa_main INDEX pt_selected-index
*                                  TRANSPORTING status
*                                               belnr
*                                               gjahr
*                                               retcd
*                                               retxt.
*    ELSE.
*      READ TABLE lt_return WITH KEY type = 'E'.
*      wa_main-retcd  = lt_return-type.
*      MESSAGE ID     lt_return-id
*              TYPE   lt_return-type
*              NUMBER lt_return-number
*              INTO   wa_main-retxt
*              WITH   lt_return-message_v1  lt_return-message_v2  lt_return-message_v3  lt_return-message_v4.
*      wa_main-status = '1'.
*      MODIFY pt_main FROM wa_main INDEX pt_selected-index
*                                  TRANSPORTING status
*                                               retcd
*                                               retxt.
*
*    ENDIF.
    "----// 변수 값 설정
    CLEAR: LV_BUKRS, LV_SUMMP, LV_DATLO.
    SELECT SINGLE
           K~BUKRS
      INTO @LV_BUKRS
      FROM EKKO AS K "INNER JOIN ekpo AS p ON p~ebeln = k~ebeln
     WHERE K~EBELN EQ @WA_MAIN-EBELN.
*       AND p~ebelp EQ @wa_main-ebelp.
    SET PARAMETER ID 'BUK' FIELD LV_BUKRS.
    WRITE WA_MAIN-SUMMP TO LV_SUMMP CURRENCY WA_MAIN-KONWA.
*    WRITE SY-DATLO      TO LV_DATLO.
    WRITE P_POSTD      TO LV_DATLO.
    CONDENSE: LV_SUMMP, LV_DATLO.
    "----// BDC
    CLEAR: LT_BDCDATA[], LT_BDCDATA,
           LT_MESSTAB[], LT_MESSTAB.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLMR1M'               '6000'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'INVFO-BLDAT'            LV_DATLO.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'INVFO-BUDAT'            LV_DATLO.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'INVFO-XBLNR'            WA_MAIN-LBLNI.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'RM08M-REFERENZBELEGTYP' '4'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=HEADER_PAY'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLMR1M'              '6000'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'INVFO-ZFBDT'            LV_DATLO.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=HEADER_TOTAL'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLMR1M'               '6000'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'RM08M-LBLNI'            WA_MAIN-LBLNI.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '/00'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPLMR1M'               '6000'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'INVFO-WRBTR'            LV_SUMMP.
*S_2021/2/25 ADD VENDOR I/V BY E00064
*    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'INVFO-SGTXT'            WA_MAIN-SGTXT.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'INVFO-SGTXT'            WA_MAIN-EXTI2.
*E_2021/2/25 ADD VENDOR I/V BY E00064
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=BU'.
    CALL TRANSACTION 'MIRO' WITH AUTHORITY-CHECK USING LT_BDCDATA MESSAGES INTO LT_MESSTAB OPTIONS FROM LS_OPT.
    READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC NE 0.
*      WAIT UP TO '0.5' SECONDS.
*      GET PARAMETER ID 'RBN' FIELD lv_belnr.

      DO 5 TIMES.
        CLEAR: LT_EKBE, LT_EKBE[].
        SELECT BELNR, GJAHR, SHKZG, BUDAT
          INTO CORRESPONDING FIELDS OF TABLE @LT_EKBE
          FROM EKBE
          WHERE EBELN = @WA_MAIN-EBELN
            AND VGABE = '2'
            AND LFBNR = @WA_MAIN-LBLNI
          ORDER BY CPUDT, CPUTM DESCENDING.

        READ TABLE LT_EKBE INDEX 1.
        IF LT_EKBE-SHKZG = 'S'.
          WA_MAIN-BELNR = LT_EKBE-BELNR.
          WA_MAIN-GJAHR = LT_EKBE-GJAHR.
          WA_MAIN-BUDAT = LT_EKBE-BUDAT.
          EXIT.
        ELSE.
          WAIT UP TO '0.5' SECONDS.
        ENDIF.
      ENDDO.

      IF WA_MAIN-BELNR IS NOT INITIAL.
        WA_MAIN-STATUS = '3'.
        WA_MAIN-RETCD  = 'S'.
        WA_MAIN-RETXT  = ' '.
*      wa_main-belnr  = lv_belnr.
*      wa_main-gjahr  = p_postd(4).
        MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                    TRANSPORTING STATUS
                                                 BELNR
                                                 GJAHR
                                                 BUDAT
                                                 RETCD
                                                 RETXT.
      ELSE.
        WA_MAIN-STATUS = '1'.
        WA_MAIN-RETCD  = 'E'.
        WA_MAIN-RETXT  = ' '.
        MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                            TRANSPORTING STATUS
                                         RETCD
                                         RETXT.

        LV_ERROR = 'X'.
      ENDIF.

    ELSE.
      WA_MAIN-RETCD  = 'E'.
      MESSAGE ID     LT_MESSTAB-MSGID
              TYPE   LT_MESSTAB-MSGTYP
              NUMBER LT_MESSTAB-MSGNR
              INTO   WA_MAIN-RETXT
              WITH   LT_MESSTAB-MSGV1  LT_MESSTAB-MSGV2  LT_MESSTAB-MSGV3  LT_MESSTAB-MSGV4.
      WA_MAIN-STATUS = '1'.
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STATUS
                                               RETCD
                                               RETXT.
      LV_ERROR = 'X'.
    ENDIF.
  ENDLOOP.
*  IF LINE_EXISTS( PT_MAIN[ STATUS = '1' ] ).
  IF LV_ERROR IS NOT INITIAL.
    PV_RETCD = 'E'.
    PV_RETXT = TEXT-T62.
    EXIT.
  ENDIF.
  PV_RETCD = 'S'.
  PV_RETXT = ' '.
  PV_CHGED = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL_INVOICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN
*&      --> LT_SELECTED
*&      <-- LV_RETCD
*&      <-- LV_RETXT
*&      <-- GV_CHGED
*&---------------------------------------------------------------------*
FORM CANCEL_INVOICE TABLES   PT_MAIN     LIKE GT_MAIN
                             PT_SELECTED TYPE LVC_T_ROW
                    CHANGING PV_RETCD    TYPE CHAR01
                             PV_RETXT    TYPE TEXT100
                             PV_CHGED    TYPE C.
  "----// 변수선언
  DATA: LV_BUKRS TYPE BUKRS,
        LV_DATLO TYPE TEXT20,
        LV_SUMMP TYPE TEXT20,
        LV_ERROR TYPE C.
  "----// BDC 변수
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA    WITH HEADER LINE.
  DATA: LT_MESSTAB TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
  DATA: LS_OPT     TYPE CTU_PARAMS.

  CLEAR LV_ERROR.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.

  "---BAPI Variables
  DATA: LT_RETURN  LIKE TABLE OF BAPIRET2 WITH HEADER LINE,
        LV_INVOICE LIKE BAPI_INCINV_FLD-INV_DOC_NO.

  "///////////////////////////////////////////////////////////////////
  "////
  "//// INVOICE 취소
  "////
  "///////////////////////////////////////////////////////////////////
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    IF WA_MAIN-BELNR IS NOT INITIAL.
      CHECK 1 = 1.
    ELSE.
      PV_RETCD = 'E'.
      PV_RETXT = TEXT-T66.
      PV_CHGED = ' '.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF PV_RETCD EQ 'E'.
    EXIT.
  ENDIF.
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    "----// 변수 값 설정
    CLEAR: LV_BUKRS, LV_SUMMP, LV_DATLO.
    SELECT SINGLE
           K~BUKRS
      INTO @LV_BUKRS
      FROM EKKO AS K INNER JOIN EKPO AS P ON P~EBELN = K~EBELN
     WHERE P~EBELN EQ @WA_MAIN-EBELN
       AND P~EBELP EQ @WA_MAIN-EBELP.
    SET PARAMETER ID 'BUK' FIELD LV_BUKRS.
    WRITE WA_MAIN-SUMMP TO LV_SUMMP CURRENCY WA_MAIN-KONWA.
*    WRITE SY-DATLO      TO LV_DATLO.
    "----// BDC
*    CLEAR: lt_bdcdata[], lt_bdcdata,
*           lt_messtab[], lt_messtab.
*    PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPLMR1M'              '0300'.
*    PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RBKPV-BELNR'           wa_main-belnr.
*    PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RBKPV-GJAHR'           wa_main-gjahr.
*    PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'UF05A-STGRD'           '01'.
*    PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '=CANC'.
*    CALL TRANSACTION 'MR8M' WITH AUTHORITY-CHECK USING lt_bdcdata MESSAGES INTO lt_messtab OPTIONS FROM ls_opt.
*    READ TABLE lt_messtab WITH KEY msgtyp = 'E'.
*    IF sy-subrc NE 0.
*      READ TABLE lt_messtab WITH KEY msgtyp = 'S'.

    CLEAR: LV_INVOICE, LT_RETURN, LT_RETURN[].

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
      EXPORTING
        INVOICEDOCNUMBER          = WA_MAIN-BELNR
        FISCALYEAR                = WA_MAIN-GJAHR
        REASONREVERSAL            = '01'
*       POSTINGDATE               = P_POSTD
        POSTINGDATE               = WA_MAIN-BUDAT
      IMPORTING
        INVOICEDOCNUMBER_REVERSAL = LV_INVOICE
*       FISCALYEAR_REVERSAL       =
      TABLES
        RETURN                    = LT_RETURN.

    IF LV_INVOICE IS NOT INITIAL. "success
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      WA_MAIN-STATUS = '2'.
      WA_MAIN-RETCD  = 'S'.
      WA_MAIN-RETXT  = ' '.
      WA_MAIN-BELNR  = ' '.
      WA_MAIN-GJAHR  = ' '.
      WA_MAIN-BUDAT  = ' '.
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STATUS
                                               BELNR
                                               GJAHR
                                               BUDAT
                                               RETCD
                                               RETXT.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

      READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
      IF SY-SUBRC = 0.
        WA_MAIN-RETXT = LT_RETURN-MESSAGE.
      ENDIF.

      WA_MAIN-RETCD  = 'E'.
*      MESSAGE ID     lt_messtab-msgid
*              TYPE   lt_messtab-msgtyp
*              NUMBER lt_messtab-msgnr
*              INTO   wa_main-retxt
*              WITH   lt_messtab-msgv1  lt_messtab-msgv2  lt_messtab-msgv3  lt_messtab-msgv4.
      WA_MAIN-STATUS = '1'.
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STATUS
                                               RETCD
                                               RETXT.
      LV_ERROR = 'X'.
    ENDIF.
  ENDLOOP.

*  IF LINE_EXISTS( PT_MAIN[ STATUS = '1' ] ).
  IF LV_ERROR IS NOT INITIAL.
    PV_RETCD = 'E'.
    PV_RETXT = TEXT-T62.
    EXIT.
  ENDIF.
  PV_RETCD = 'S'.
  PV_RETXT = ' '.
  PV_CHGED = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_SHIPMENT_COST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN
*&      --> LT_SELECTED
*&      <-- LV_RETCD
*&      <-- LV_RETXT
*&      <-- GV_CHGED
*&---------------------------------------------------------------------*
FORM DELETE_SHIPMENT_COST TABLES   PT_MAIN     LIKE GT_MAIN
                                   PT_SELECTED TYPE LVC_T_ROW
                          CHANGING PV_RETCD    TYPE CHAR01
                                   PV_RETXT    TYPE TEXT100
                                   PV_CHGED    TYPE C.

  DATA: LV_ERROR TYPE C.
  "----// BDC 변수
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA    WITH HEADER LINE.
  DATA: LT_MESSTAB TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
  DATA: LS_OPT     TYPE CTU_PARAMS.

  "----// 신호등 & 편집셀 설정
  DATA: LT_CELLSTYL TYPE LVC_T_STYL,
        LS_CELLSTYL TYPE LVC_S_STYL,
        LV_IDX      TYPE SY-TABIX.

  CLEAR LV_ERROR.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'N'.
  LS_OPT-UPDMODE = 'S'.
  LS_OPT-DEFSIZE = 'X'.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Delete Shipment Cost
  "////
  "///////////////////////////////////////////////////////////////////
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    IF WA_MAIN-STABR EQ 'A'.
      CHECK 1 = 1.
    ELSE.
      PV_RETCD = 'E'.
      PV_RETXT = TEXT-T67.
      PV_CHGED = ' '.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF PV_RETCD EQ 'E'.
    EXIT.
  ENDIF.
  LOOP AT PT_SELECTED.
    READ TABLE PT_MAIN INTO WA_MAIN INDEX PT_SELECTED-INDEX.
    "----// BDC
    CLEAR: LT_BDCDATA[], LT_BDCDATA,
           LT_MESSTAB[], LT_MESSTAB.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0020'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'VFKK-FKNUM'            WA_MAIN-FKNUM.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '=UEBP'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING 'X'  'SAPMV54A'              '0030'.
    PERFORM APPEND_BDCDATA TABLES LT_BDCDATA USING ' '  'BDC_OKCODE'            '/ELOES'.
    CALL TRANSACTION 'VI02' WITH AUTHORITY-CHECK USING LT_BDCDATA MESSAGES INTO LT_MESSTAB OPTIONS FROM LS_OPT.
    READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC NE 0.
      READ TABLE LT_MESSTAB WITH KEY MSGTYP = 'S'
                                     MSGID  = 'VY'
                                     MSGNR  = '007'.
      WA_MAIN-RETCD  = 'S'.
      WA_MAIN-RETXT  = ' '.
      WA_MAIN-STATUS = 0.
      WA_MAIN-FKNUM  = ' '.
      WA_MAIN-STBER  = ' '.
      WA_MAIN-STFRE  = ' '.
      WA_MAIN-STABR  = ' '.
      WA_MAIN-ZFR9P  = 0.
*S_2021/2/26 Add Vendor I/V logic
      CLEAR: WA_MAIN-EXTI2.
*E_2021/2/26 Add Vendor I/V logic
*S_2021/2/21 VENDOR I/V BY E00064
      "----// cell style
      CLEAR: LS_CELLSTYL, LT_CELLSTYL, WA_MAIN-CELLSTYL.
      IF WA_MAIN-STABR NE 'C'.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_CELLSTYL-FIELDNAME = 'ZFR9P'.
      INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
      INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
      CLEAR: LS_CELLSTYL, LT_CELLSTYL.
      IF WA_MAIN-FKNUM IS INITIAL.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
      LS_CELLSTYL-FIELDNAME = 'EXTI2'.
      INSERT LS_CELLSTYL INTO TABLE LT_CELLSTYL.
      INSERT LINES OF LT_CELLSTYL INTO TABLE WA_MAIN-CELLSTYL.
*E_2021/2/21 VENDOR I/V BY E00064

      WA_MAIN-SUMMP  = WA_MAIN-ZFR4P + WA_MAIN-ZFR5P + WA_MAIN-ZFR9P.
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STATUS
                                               FKNUM
                                               STBER
                                               STFRE
                                               STABR
                                               ZFR9P
                                               SUMMP
*S_2021/2/26 Add Vendor I/V logic
                                               EXTI2
*E_2021/2/26 Add Vendor I/V logic
                                               RETCD
                                               RETXT
*S_2021/2/26 Add Vendor I/V logic
                                               CELLSTYL.
*E_2021/2/26 Add Vendor I/V logic
    ELSE.
      WA_MAIN-RETCD  = 'E'.
      MESSAGE ID     LT_MESSTAB-MSGID
              TYPE   LT_MESSTAB-MSGTYP
              NUMBER LT_MESSTAB-MSGNR
              INTO   WA_MAIN-RETXT
              WITH   LT_MESSTAB-MSGV1  LT_MESSTAB-MSGV2  LT_MESSTAB-MSGV3  LT_MESSTAB-MSGV4.
      WA_MAIN-STATUS = '1'.
      MODIFY PT_MAIN FROM WA_MAIN INDEX PT_SELECTED-INDEX
                                  TRANSPORTING STATUS
                                               RETCD
                                               RETXT.

      LV_ERROR = 'X'.
    ENDIF.
  ENDLOOP.
*  IF LINE_EXISTS( PT_MAIN[ STATUS = '1' ] ).
  IF LV_ERROR IS NOT INITIAL.
    PV_RETCD = 'E'.
    PV_RETXT = TEXT-T62.
    EXIT.
  ENDIF.
  PV_RETCD = 'S'.
  PV_RETXT = ' '.
  PV_CHGED = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK USING P_ROW    TYPE LVC_S_ROW
                               P_COLUMN TYPE LVC_S_COL.
  "----// 변수선언
  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA.
  DATA: LS_OPT     TYPE CTU_PARAMS.
  CLEAR LS_OPT.
  LS_OPT-DISMODE = 'E'.
  LS_OPT-UPDMODE = 'S'.
  CLEAR: WA_MAIN.
  READ TABLE GT_MAIN INTO WA_MAIN INDEX P_ROW-INDEX.
  IF     P_COLUMN-FIELDNAME EQ 'FKNUM' AND WA_MAIN-FKNUM IS NOT INITIAL.
    SET PARAMETER ID 'FKK' FIELD WA_MAIN-FKNUM.
    CALL TRANSACTION 'VI03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
  ELSEIF P_COLUMN-FIELDNAME EQ 'TKNUM' AND WA_MAIN-FKNUM IS NOT INITIAL.
    SET PARAMETER ID 'TNR' FIELD WA_MAIN-TKNUM.
    CALL TRANSACTION 'VT03N' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
  ELSEIF P_COLUMN-FIELDNAME EQ 'EBELN' AND WA_MAIN-EBELN IS NOT INITIAL.
    SET PARAMETER ID 'BES' FIELD WA_MAIN-EBELN.
    CALL TRANSACTION 'ME23N' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
  ELSEIF P_COLUMN-FIELDNAME EQ 'BELNR' AND WA_MAIN-BELNR IS NOT INITIAL.
    SET PARAMETER ID 'RBN' FIELD WA_MAIN-BELNR.
    SET PARAMETER ID 'GJR' FIELD WA_MAIN-GJAHR.
    CALL TRANSACTION 'MIR4' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
  ELSE.
    CHECK 1 = 1.
  ENDIF.
ENDFORM.
