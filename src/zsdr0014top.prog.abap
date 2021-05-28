*&---------------------------------------------------------------------*
*& Include          ZSDR0014TOP
*&---------------------------------------------------------------------*
TABLES : MARA.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        START_DATE   TYPE SYDATUM,
        VKORG        LIKE VBAK-VKORG,           "Sales org
        VKORG_TXT    LIKE TVKOT-VTEXT,          "Sales org Desc
        VTWEG        LIKE VBAK-VTWEG,           "Distribut. chan
        VTWEG_TXT    LIKE TVTWT-VTEXT,          "Dsch Desc
        KUNNR        LIKE VBAK-KUNNR,           "Sold to
        KUNNR_TXT    LIKE KNA1-NAME1,           "Sold to Desc
        KUNWE        LIKE VBAK-KUNNR,           "Ship-to
        KUNWE_TXT    LIKE KNA1-NAME1,           "Ship to Desc
        KMEIN_1      LIKE KONP-KMEIN,           "Sales unit
        ZBASE_PRICE  LIKE KONP-KBETR,           "Sales Price
        KONWA_1      LIKE KONP-KONWA,           "Currency
        MATNR        LIKE MARA-MATNR,           "SKU
        MATNR_TXT    LIKE MAKT-MAKTX,           "SKU Desc
        LIFNR        LIKE LFA1-LIFNR,           "Vendor
        LIFNR_TXT    LIKE LFA1-NAME1,           "Vendor Desc
        BUKRS        LIKE T001-BUKRS,           "Company code
        BUKRS_TXT    LIKE T001-BUTXT,           "Company code desc.
        EKORG        LIKE EKKO-EKORG,           "Purchasing Org
        EKORG_TXT    LIKE T024E-EKOTX,          "Purchasing Org Desc
        WERKS        LIKE T001W-WERKS,          "Plant
        WERKS_TXT    LIKE T001W-NAME1,          "Plant Desc
        KUNNR_WL     LIKE VBAK-KUNNR,           "goods supplier
        KUNNR_WL_TXT LIKE KNA1-NAME1,           "goods supplier Desc
        ZKMEIN_MM    LIKE KONP-KMEIN,           "Info Unit
        ZKBETR_MM    LIKE KONP-KBETR,           "Info price
        ZKPEIN_MM    LIKE KONP-KPEIN,           "Info price
        ZKONWA_MM    LIKE KONP-KONWA,           "Currency
        DATAB        LIKE A017-DATAB,           "Validity start date
        ZKMEIN_SO    LIKE KONP-KMEIN,           "Sales Unit
        ZKBETR_SO    LIKE KONP-KBETR,           "Sales price
        ZKPEIN_SO    LIKE KONP-KPEIN,           "Sales price
        ZKONWA_SO    LIKE KONP-KONWA,           "Sales Currency
        ZCONFIRM     TYPE ZSDT0021-ZCONFIRM,    "Confirm status
        LVORM        TYPE C,
        ICON(4),
        MESSAGE(100),
        DEL,
        CELLSTYLE    TYPE LVC_T_STYL,
        CELLCOLOR    TYPE LVC_T_SCOL,
      END OF GT_LIST.

DATA : BEGIN OF GT_MAT_C OCCURS 0,
         EKORG TYPE EKKO-EKORG,
         MATNR TYPE MARA-MATNR,
       END OF GT_MAT_C.

DATA : GT_INFO LIKE TABLE OF ZMMS0070 WITH HEADER LINE.

DATA : GS_VPRICE LIKE ZSDS0050,
       GT_VPRICE LIKE TABLE OF GS_VPRICE.

RANGES : GR_MATNR FOR MARA-MATNR.

DATA : GV_NOT_CONFIRM.
DATA : GV_ERROR.
DATA : GV_DATE TYPE SY-DATUM.

DATA : GT_RETURNTAB TYPE STANDARD TABLE OF DDSHRETVAL,
       GS_RETURNTAB LIKE LINE OF GT_RETURNTAB.
*-----------------------------------------------------------------------
* BDC
*-----------------------------------------------------------------------
DATA: GT_BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
      GT_BDCMSGE LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
      GS_BDC_OPT LIKE CTU_PARAMS.

*&----------------------------------------- ----------------------------*
*&  GLOBAL VARIABLE DECLARATION
*&---------------------------------------------------------------------*

DATA : OK_CODE TYPE SY-UCOMM.
DATA : GV_SUCCESS(5),
       GV_FAILURE(5),
       GV_ERR,
       GV_MSG(200).

DATA : GV_ZTYPE.

*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-T01.
PARAMETERS : P_DATE LIKE SY-DATUM OBLIGATORY,
             P_END  LIKE SY-DATUM NO-DISPLAY.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: P_INFO RADIOBUTTON GROUP R01 DEFAULT 'X'  USER-COMMAND UC01.
SELECTION-SCREEN COMMENT 2(15) TEXT-S01 FOR FIELD P_INFO.
SELECTION-SCREEN POSITION 31.
PARAMETERS: P_SALES RADIOBUTTON GROUP R01.
SELECTION-SCREEN COMMENT 32(20) TEXT-S02 FOR FIELD P_SALES.
SELECTION-SCREEN END OF LINE.

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
