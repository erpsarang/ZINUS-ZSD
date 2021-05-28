*&---------------------------------------------------------------------*
*& Include          ZSDR0180TOP
*&---------------------------------------------------------------------*
TABLES : ZSDT0020, T001W.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*

* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        ICON(4),                                    "Icon
        MESSAGE(100),                               "Message
        ZKUNNR_IC     LIKE ZSDT0020-ZKUNNR_IC,      "Intercompany
        ZKUNNR_IC_TXT LIKE BUT000-NAME_ORG1,
        KUNNR         LIKE ZSDT0020-KUNNR,          "Customer
        KUNNR_TXT     LIKE BUT000-NAME_ORG1,
        LIFNR         LIKE ZSDT0020-LIFNR,          "Vendor
        LIFNR_TXT     LIKE BUT000-NAME_ORG1,
        MATNR         LIKE ZSDT0020-MATNR,          "SKU
        MATNR_TXT     LIKE MAKT-MAKTX,              "
        WERKS         LIKE T001W-WERKS,             "Plant
        WERKS_TXT     LIKE T001W-NAME1,             "Plant TXT
        ZPRODH_GROUP  LIKE ZSDT0020-ZPRODH_GROUP,   "Margin Group
        LVORM         LIKE MARA-LVORM,              "Discountinue
        CHK_PLANT,                                  "Expand
        ZMARGIN       LIKE ZSDT0020-ZMARGIN,        "Margin Rate
        FLAG          TYPE C LENGTH 6,              "Flag
        KBETR_BASE    LIKE KONP-KBETR,              "Base Price
        KBETR_REQ     LIKE KONP-KBETR,              "Request Info
        KUNWE         LIKE ZSDT0020-KUNNR,          "HQ Ship-to
        KBETR_COND    LIKE KONP-KBETR,              "HQ Sales Price
        EKORG         LIKE EKKO-EKORG,              "HQ Pur. Org.
        KBETR_INFO    LIKE KONP-KBETR,              "HQ Info
        KONWA         LIKE KONP-KONWA,
        CELLSTYLE     TYPE LVC_T_STYL,
        CELLCOLOR     TYPE LVC_T_SCOL,
      END OF GT_LIST.

*&---------------------------------------------------------------------*
*&  GLOBAL VARIABLE DECLARATION
*&---------------------------------------------------------------------*
DATA: OK_CODE TYPE SY-UCOMM.
DATA : GV_SUCCESS(5),
       GV_FAILURE(5).

*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS : S_INC    FOR ZSDT0020-ZKUNNR_IC NO-EXTENSION NO INTERVALS
                                                 OBLIGATORY MODIF ID Z01,
                 S_KUNNR  FOR ZSDT0020-KUNNR NO-EXTENSION NO INTERVALS OBLIGATORY,
                 S_LIFNR  FOR ZSDT0020-LIFNR NO INTERVALS OBLIGATORY,
                 S_MATNR  FOR ZSDT0020-MATNR NO INTERVALS OBLIGATORY,
                 S_WERKS  FOR T001W-WERKS.
SELECTION-SCREEN END OF BLOCK B1.

*********************************************************************
*** DEFINE
*********************************************************************
DEFINE _CLEAR.
  CLEAR : &1. CLEAR : &1[].
END-OF-DEFINITION.
DEFINE _APPEND.
  APPEND : &1. CLEAR : &1.
END-OF-DEFINITION.
DEFINE _COLLECT.
  COLLECT : &1. CLEAR : &1.
END-OF-DEFINITION.
DEFINE _RANGE.
  &1-SIGN   = &2.
  &1-OPTION = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  COLLECT &1. CLEAR &1.
END-OF-DEFINITION.
