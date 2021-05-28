*&---------------------------------------------------------------------*
*& Include          ZSDR0015TOP
*&---------------------------------------------------------------------*
TABLES : A903 , A017, A305, A914.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display

DATA: BEGIN OF GT_LIST OCCURS 0,
        MATNR      LIKE MARA-MATNR,       "SKU
        MATNR_TXT  LIKE MAKT-MAKTX,       "DESC
        PRODH      LIKE T179T-PRODH,      "제품계층구조
        PRODH_TXT  LIKE T179T-VTEXT,      "DESC
        "1.HQ 판매단가
        VKORG1     LIKE A305-VKORG,       "SALES ORG
        VTWEG1     LIKE A305-VTWEG,       "Distr. Chl
        KUNNR1     LIKE A305-KUNNR,       "SOLD-TO
        KUNNR1_TXT LIKE BUT000-NAME_ORG1, "DESC
        KUNWE1     LIKE A903-KUNWE,       "SHIP-TO
        KUNWE1_TXT LIKE BUT000-NAME_ORG1, "DESC
        KPEIN1     LIKE KONP-KPEIN,       "PER UNIT
        KMEIN1     LIKE KONP-KMEIN,       "SALES UNIT
        KBETR1     LIKE KONP-KBETR,       "HQ SALES PRICE
        KONWA1     LIKE KONP-KONWA,       "CURRENCY
        "2.HQ 매입단가
        LIFNR      LIKE A017-LIFNR,       "VENDOR
        LIFNR_TXT  LIKE BUT000-NAME_ORG1, "DESC
        EKORG2     LIKE A017-EKORG,       "P.ORG
        WERKS      LIKE A017-WERKS,       "PLANT
        KPEIN2     LIKE KONP-KPEIN,       "PER
        KMEIN2     LIKE KONP-KMEIN,       "UNIT
        KBETR2     LIKE KONP-KBETR,       "HQ INFO PRICE
        KONWA2     LIKE KONP-KONWA,       "CURRENCY
        "3.판매법인 INFO
        BUKRS3     LIKE A914-BUKRS,       "COMPANY
        EKORG3     LIKE A914-EKORG,       "P.ORG
        WERKS3     LIKE A914-WERKS,       "PLANT
        LLIEF      LIKE A914-LLIEF,       "COO(생산지)
        LLIEF_TXT  LIKE BUT000-NAME_ORG1, "DESC
        KPEIN3     LIKE KONP-KPEIN,       "PER
        KMEIN3     LIKE KONP-KMEIN,       "UNIT
        KBETR3     LIKE KONP-KBETR,       "Sales INC. INFO PRICE
        KONWA3     LIKE KONP-KONWA,       "CURRENCY
        "4.제조법인 SALES PRICE (SAP)
        VKORG4     LIKE A305-VKORG,       "SALES ORG
        VTWEG4     LIKE A305-VTWEG,       "Distr. Chl
        KUNNR4     LIKE A305-KUNNR,       "SOLD-TO
        KUNNR4_TXT LIKE BUT000-NAME_ORG1, "DESC
        KPEIN4     LIKE KONP-KPEIN,       "PER
        KMEIN4     LIKE KONP-KMEIN,       "UNIT
        KBETR4     LIKE KONP-KBETR,       "SALES PRICE (SAP)
        KONWA4     LIKE KONP-KONWA,       "CURRENCY
        CELLSTYLE  TYPE LVC_T_STYL,
        CELLCOLOR  TYPE LVC_T_SCOL,
      END OF GT_LIST.

DATA : GT_RETURNTAB TYPE STANDARD TABLE OF DDSHRETVAL,
       GS_RETURNTAB LIKE LINE OF GT_RETURNTAB.

*&----------------------------------------- ----------------------------*
*&  GLOBAL VARIABLE DECLARATION
*&---------------------------------------------------------------------*
DATA : GV_DATE TYPE SY-DATUM.
DATA : OK_CODE TYPE SY-UCOMM,
       GV_ZTYPE.
*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-T01.
PARAMETERS : P_DATE LIKE SY-DATUM OBLIGATORY,
             P_END  LIKE SY-DATUM NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B0.

*HQ Sales Price
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T02.
SELECT-OPTIONS : S_KUNNR1 FOR A903-KUNNR,
                 S_KUNWE1 FOR A903-KUNWE,
                 S_VTWEG1 FOR A305-VTWEG,
                 S_MATNR1 FOR A305-MATNR.
SELECTION-SCREEN END OF BLOCK B1.

*HQ Info
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T03.
SELECT-OPTIONS : S_LIFNR2 FOR A017-LIFNR,
                 S_EKORG2 FOR A017-EKORG,
                 S_WERKS2 FOR A017-WERKS,
                 S_MATNR2 FOR A305-MATNR.
SELECTION-SCREEN END OF BLOCK B2.

*Intercompany Sales Price
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-T04.
SELECT-OPTIONS : S_KUNNR3 FOR A903-KUNNR,
                 S_VTWEG3 FOR A305-VTWEG,
                 S_MATNR3 FOR A305-MATNR.
SELECTION-SCREEN END OF BLOCK B3.

*Intercompany Info
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-T05.
SELECT-OPTIONS : S_BUKRS4 FOR A914-BUKRS,
                 S_EKORG4 FOR A017-EKORG,
                 S_WERKS4 FOR A017-WERKS,
                 S_LLIEF4 FOR A914-LLIEF,
                 S_MATNR4 FOR A305-MATNR.
SELECTION-SCREEN END OF BLOCK B4.

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
