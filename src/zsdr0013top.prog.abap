*&---------------------------------------------------------------------*
*& Include          ZSDR0011TOP
*&---------------------------------------------------------------------*
TABLES : MARA, ZSDT0020.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        KUNNR         LIKE VBAK-KUNNR,           "End Customer
        KUNNR_TXT     LIKE KNA1-NAME1,           "Desc
        ZBASE_PRICE   LIKE KONP-KBETR,           "기준금액
        KONWA_1       LIKE KONP-KONWA,           "Currency
        KPEIN_1       LIKE KONP-KPEIN,           "기준
        KMEIN_1       LIKE KONP-KMEIN,           "기준
        FLAG          TYPE C LENGTH 6,           "구분  FOB OR MDDP
        ZPRODH_GROUP  LIKE ZSDT0020-ZPRODH_GROUP, "HIERARCHY GROUP
        CHK_MATNR     TYPE C LENGTH 4,           "SKU로 등록한 마진
        MATNR         LIKE MARA-MATNR,           "SKU
        MATNR_TXT     LIKE MAKT-MAKTX,           "Desc
        ZMARGIN_MM    LIKE ZSDT0020-ZMARGIN,     "INFO MARGIN
        LIFNR         LIKE LFA1-LIFNR,           "Vendor
        LIFNR_TXT     LIKE LFA1-NAME1,           "Desc
        EKORG         LIKE EKKO-EKORG,           "Purchasing Org
        EKORG_TXT     LIKE T024E-EKOTX,          "Desc
        WERKS         LIKE T001W-WERKS,          "Plant
        WERKS_TXT     LIKE T001W-NAME1,          "Desc
        ZKMEIN_MM     LIKE KONP-KPEIN,           "Info Unit
        DATAB         LIKE A017-DATAB,           "Validity start date
        ZKBETR_MM_OLD LIKE KONP-KBETR,           "Info Price(OLD)
        ZKBETR_MM     LIKE KONP-KBETR,           "Info price(new)
        ZKONWA_MM     LIKE KONP-KONWA,           "Currency
        ZMARGIN_SD    LIKE ZSDT0020-ZMARGIN,     "SALES MARGIN
        VKORG         LIKE VBAK-VKORG,           "Sales org
        VKORG_TXT     LIKE TVKOT-VTEXT,          "Sales org Desc
        VTWEG         LIKE VBAK-VTWEG,           "Distribution ch
        VTWEG_TXT     LIKE TVTWT-VTEXT,          "Distr. Chl Desc
        ZKUNNR_IC     LIKE ZSDT0042-ZKUNNR_IC,   "INTERCOMPANY
        ZKUNNR_SUB    LIKE ZSDT0042-ZKUNNR_SUB,  "Sold to
        ZKUNNR_IC_TXT LIKE KNA1-NAME1,           "NAME
        KUNWE         LIKE VBAK-KUNNR,           "Ship-to
        KUNWE_TXT     LIKE KNA1-NAME1,           "Desc
        ZKMEIN_SD     LIKE KONP-KPEIN,           "SALES Unit
        KMEIN_3       LIKE KONP-KMEIN,           "Condition Unit
        ZKBETR_SD_OLD LIKE KONP-KBETR,           "Sales Price(Old)
        ZKBETR_SD     LIKE KONP-KBETR,           "Sales price(new)
        ZKONWA_SD     LIKE KONP-KONWA,           "Currency
        LVORM         LIKE MARA-LVORM,           "discontinue
        ZTYPE(10),
        ICON(4),
        MESSAGE(100),
        DEL,
        CELLSTYLE     TYPE LVC_T_STYL,
        CELLCOLOR     TYPE LVC_T_SCOL,
      END OF GT_LIST.

*ZSDT0020 기준 데이터
DATA : BEGIN OF GT_DATA OCCURS 0,
         ZKUNNR_IC    LIKE ZSDT0020-ZKUNNR_IC,
         KUNNR        LIKE ZSDT0020-KUNNR,
         LIFNR        LIKE ZSDT0020-LIFNR,
         ZPRODH_GROUP LIKE ZSDT0020-ZPRODH_GROUP,
         MATNR        LIKE ZSDT0020-MATNR,
         ZSTART       LIKE ZSDT0020-ZSTART,
         ZTYPE        LIKE ZSDT0020-ZTYPE,
         ZMARGIN      LIKE ZSDT0020-ZMARGIN,
         ZEXCEPT      LIKE ZSDT0020-ZEXCEPT,
         ZCONFIRM     LIKE ZSDT0020-ZCONFIRM,
         VKORG        LIKE ZSDT0042-VKORG,
         VTWEG        LIKE ZSDT0042-VTWEG,
         ZKUNNR_S     LIKE ZSDT0042-ZKUNNR_S,
         ZKUNNR_SUB   LIKE ZSDT0042-ZKUNNR_SUB,
         ZKUNNR       LIKE ZSDT0041-ZKUNNR,
         ZKUNNR_DESC  LIKE ZSDT0041-ZKUNNR_DESC,
         EKORG        LIKE ZSDT0060-EKORG,
         PRODH        LIKE ZSDT0090-PRODH,
       END OF GT_DATA.

DATA : BEGIN OF GT_MDDP OCCURS 0,
         ZKUNNR_IC     LIKE ZSDT0020-ZKUNNR_IC, "Intercompany
         ZKUNNR_S      LIKE ZSDT0042-ZKUNNR_S,
         ZKUNNR_SUB    LIKE ZSDT0042-ZKUNNR_SUB,
         ZMARGIN       LIKE ZSDT0020-ZMARGIN,
         ZHQ           LIKE ZSDT0040-ZHQ,
         ZPRODH_GROUP  LIKE ZSDT0020-ZPRODH_GROUP,
         LIFNR         LIKE LFA1-LIFNR,        "Vendor
         EKORG         LIKE EKKO-EKORG,        "Purchasing Org
         VKORG         LIKE A305-VKORG,
         VTWEG         LIKE A305-VTWEG,
         CHK_MATNR     TYPE C LENGTH 4,        "SKU로 등록한 마진
         MATNR         LIKE ZSDT0020-MATNR,
         MSTAE         LIKE MARA-MSTAE,
         KUNNR         LIKE ZSDT0020-KUNNR,
         ZKUNNR_DESC   LIKE ZSDT0041-ZKUNNR_DESC,
         ZBASE_PRICE   LIKE KONP-KBETR, "기준
         KONWA_1       LIKE KONP-KONWA, "기준
         KPEIN_1       LIKE KONP-KPEIN, "기준
         KMEIN_1       LIKE KONP-KMEIN, "기준
         DATAB         LIKE A017-DATAB,
         ZMARGIN_MM    LIKE ZSDT0020-ZMARGIN,  "SALES MARGIN
         ZKBETR_MM_OLD LIKE KONP-KBETR, "매입
         ZKONWA_MM     LIKE KONP-KONWA, "매입
         ZKMEIN_MM     LIKE KONP-KPEIN, "매입
         KMEIN_2       LIKE KONP-KMEIN, "매입
         ZMARGIN_SD    LIKE ZSDT0020-ZMARGIN,  "SALES MARGIN
         ZKBETR_SD_OLD LIKE KONP-KBETR, "판가
         ZKONWA_SD     LIKE KONP-KONWA, "판가
         ZKMEIN_SD     LIKE KONP-KPEIN, "판가
         KMEIN_3       LIKE KONP-KMEIN, "판가
       END OF GT_MDDP,
       GT_MDDP_KEY    LIKE TABLE OF GT_MDDP WITH HEADER LINE,
       GT_FOB_305     LIKE TABLE OF GT_MDDP WITH HEADER LINE,
       GT_FOB_305_KEY LIKE TABLE OF GT_MDDP WITH HEADER LINE,
       GT_FOB_903     LIKE TABLE OF GT_MDDP WITH HEADER LINE,
       GT_FOB_903_KEY LIKE TABLE OF GT_MDDP WITH HEADER LINE,
       GT_0070        LIKE TABLE OF GT_MDDP WITH HEADER LINE,
       GT_0070_KEY    LIKE TABLE OF GT_MDDP WITH HEADER LINE.

DATA : GT_0020 LIKE TABLE OF ZSDT0020 WITH HEADER LINE.
DATA : GT_0040 LIKE TABLE OF ZSDT0040 WITH HEADER LINE.
DATA : GT_0050 LIKE TABLE OF ZSDT0050 WITH HEADER LINE.
DATA : GT_0090 LIKE TABLE OF ZSDT0090 WITH HEADER LINE.
DATA : GT_MARA LIKE TABLE OF MARA WITH HEADER LINE.
DATA : GT_INFO LIKE TABLE OF ZMMS0070 WITH HEADER LINE.

DATA : BEGIN OF GT_CHK_PRICE OCCURS 0,
         VKORG LIKE A903-VKORG,
         MATNR LIKE A903-MATNR,
         VTWEG LIKE A903-VTWEG,
         KUNNR LIKE A903-KUNNR,
         KUNWE LIKE A903-KUNWE,
         KSCHL LIKE A903-KSCHL,
         DATAB LIKE A903-DATAB,
         DATBI LIKE A903-DATBI,
         KBETR LIKE KONP-KBETR,
         KONWA LIKE KONP-KONWA,
         KPEIN LIKE KONP-KPEIN,
         KMEIN LIKE KONP-KMEIN,
       END OF GT_CHK_PRICE.

DATA : GT_RETURNTAB TYPE STANDARD TABLE OF DDSHRETVAL,
       GS_RETURNTAB LIKE LINE OF GT_RETURNTAB.
DATA : GV_DATE  TYPE SY-DATUM,
       GV_DATE2 TYPE SY-DATUM.
DATA : GV_NOT_CONFIRM.
DATA : GV_ERROR.
*-----------------------------------------------------------------------
* BDC
*-----------------------------------------------------------------------
DATA : GT_BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
       GT_BDCMSG  TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
DATA : GS_CTU_PARAMS TYPE CTU_PARAMS.

*&----------------------------------------- ----------------------------*
*&  GLOBAL VARIABLE DECLARATION
*&---------------------------------------------------------------------*
RANGES : GR_PRODH FOR MARA-PRDHA.

DATA : OK_CODE TYPE SY-UCOMM.
DATA : GV_SUCCESS(6) TYPE N,
       GV_FAILURE(6) TYPE N.
DATA GV_ZTYPE.
*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-T01.

SELECT-OPTIONS : S_DATE FOR ZSDT0020-ZSTART MODIF ID Z02 NO-EXTENSION.

PARAMETERS : P_DATE LIKE SY-DATUM OBLIGATORY MODIF ID Z01,
             P_END  LIKE SY-DATUM NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B0.

*********************************************************************
*** DEFINE
*********************************************************************
DEFINE _CLEAR.
  CLEAR : &1. CLEAR : &1[].
END-OF-DEFINITION.
DEFINE _RANGE.
  &1-SIGN   = &2.
  &1-OPTION = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  COLLECT &1. CLEAR &1.
END-OF-DEFINITION.
