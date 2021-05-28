*&---------------------------------------------------------------------*
*& Include          ZSDI0020TOP
*&---------------------------------------------------------------------*
TABLES : MARA.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display

DATA: BEGIN OF GT_LIST OCCURS 0,
        LIFNR          LIKE LFA1-LIFNR,           "Company(SAP)
        ZBUKRS_JDE     LIKE ZSDT0040-ZBUKRS_JDE,  "Company(JDE)
        KUNNR          LIKE VBAK-KUNNR,           "Customer(SAP)
        KUNNR_JDE      LIKE BUT000-TITLE_LET,     "Customer(JDE)
        MATNR          LIKE MARA-MATNR,           "SKU
        MATNR_TXT      LIKE MAKT-MAKTX,           "SKU Desc
        START_DATE     TYPE SYDATUM,              "Start Date
        END_DATE       TYPE SYDATUM,              "End Date
        KBETR          LIKE KONP-KBETR,           "Sales Price
        KONWA          LIKE KONP-KONWA,           "Currency
        ICON(4),
        MESSAGE(100),
        CELLSTYLE      TYPE LVC_T_STYL,
        CELLCOLOR      TYPE LVC_T_SCOL,
      END OF GT_LIST,

      GT_INTERFACE LIKE TABLE OF GT_LIST WITH HEADER LINE.

DATA : GV_NOT_CONFIRM.
DATA : GV_DATE TYPE SY-DATUM.
DATA : GV_ERROR.
DATA : GT_RETURNTAB TYPE STANDARD TABLE OF DDSHRETVAL,
       GS_RETURNTAB LIKE LINE OF GT_RETURNTAB.

RANGES : GR_MATNR FOR MARA-MATNR.
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
       GV_MSG(200),
       GV_MESSAGE  TYPE STRING,
       GV_ZTYPE.
*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-T01.
PARAMETERS : P_DATE LIKE SY-DATUM OBLIGATORY,
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
