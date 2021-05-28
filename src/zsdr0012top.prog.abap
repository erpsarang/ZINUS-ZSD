*&---------------------------------------------------------------------*
*& Include          ZSDR0012TOP
*&---------------------------------------------------------------------*
TABLES : SSCRFIELDS, ZSDT0070.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        VTWEG        LIKE TVTW-VTWEG,             "Distribution Chan
        ZKUNNR       LIKE ZSDT0041-ZKUNNR,        "Customer
        ZKUNNR_TXT   LIKE ZSDT0041-ZKUNNR_DESC,    "Customer Desc
        MATNR        LIKE MARA-MATNR,             "SKU
        MATNR_TXT    LIKE MAKT-MAKTX,             "SKU Desc
        KBETR        LIKE ZSDT0070-KBETR,         "Sales price
        KONWA        LIKE ZSDT0070-KONWA,         "Currency
        KPEIN        LIKE ZSDT0070-KPEIN,         "Condition price unit
        KMEIN        LIKE ZSDT0070-KMEIN,         "Condition Unit
        DATAB        LIKE ZSDT0070-DATAB,         "Start Date
        DATBI        LIKE ZSDT0070-DATBI,         "End Date
        LOEVM_KO     LIKE ZSDT0070-LOEVM_KO,      "Deletion
        UP,
        FLAG,
        DEL,
        ICON(4),
        MESSAGE(100),
        CELLSTYLE    TYPE LVC_T_STYL,
        CELLCOLOR    TYPE LVC_T_SCOL,
      END OF GT_LIST.


DATA : BEGIN OF GT_UPLOAD OCCURS 0,
         VTWEG  LIKE TVTW-VTWEG,             "Distribution Chan
         ZKUNNR LIKE ZSDT0041-ZKUNNR,        "Customer
         MATNR  LIKE MARA-MATNR,             "SKU
         KBETR  LIKE ZSDT0070-KBETR,         "Amount
         KONWA  LIKE ZSDT0070-KONWA,         "Currency
         KPEIN  LIKE ZSDT0070-KPEIN,         "Condition price unit
         KMEIN  LIKE ZSDT0070-KMEIN,         "Condition Unit
         DATAB  LIKE ZSDT0070-DATAB,         "Validity Start Date
         DATBI  LIKE ZSDT0070-DATBI,         "Validity End Date
       END OF GT_UPLOAD.


DATA : BEGIN OF GT_0040 OCCURS 0,
         ZKUNNR_IC     LIKE ZSDT0040-ZKUNNR_IC,
         ZKUNNR_IC_TXT LIKE BUT000-PARTNER,
       END OF GT_0040.

DATA : BEGIN OF GT_0041 OCCURS 0,
         ZKUNNR     LIKE ZSDT0041-ZKUNNR,
         ZKUNNR_TXT LIKE ZSDT0041-ZKUNNR_DESC,
       END OF GT_0041.

DATA : GT_RETURNTAB TYPE STANDARD TABLE OF DDSHRETVAL,
       GS_RETURNTAB LIKE LINE OF GT_RETURNTAB.

DATA : GT_TVTWT LIKE TABLE OF TVTWT WITH HEADER LINE .

*&---------------------------------------------------------------------*
*&  GLOBAL VARIABLE DECLARATION
*&---------------------------------------------------------------------*
DATA: OK_CODE TYPE SY-UCOMM.
DATA : GV_SUCCESS(5),
       GV_FAILURE(5),
       GV_DUP,
       GV_PATH  LIKE RLGRAP-FILENAME.
*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: P_CRE RADIOBUTTON GROUP R01 DEFAULT 'X' USER-COMMAND U01.
SELECTION-SCREEN COMMENT 2(15) TEXT-S01 FOR FIELD P_CRE.
SELECTION-SCREEN POSITION 31.
PARAMETERS: P_DIS RADIOBUTTON GROUP R01.
SELECTION-SCREEN COMMENT 32(18) TEXT-S02 FOR FIELD P_DIS.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  TEXT-S03 FOR FIELD P_INCOMP .
SELECTION-SCREEN POSITION  25.
PARAMETERS    : P_INCOMP   LIKE ZSDT0020-ZKUNNR_IC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  TEXT-S04 FOR FIELD S_VTWEG MODIF ID Z01.
SELECTION-SCREEN POSITION  22.
SELECT-OPTIONS : S_VTWEG  FOR ZSDT0070-VTWEG MODIF ID Z01.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  TEXT-S05 FOR FIELD S_ZKUNNR MODIF ID Z01.
SELECTION-SCREEN POSITION  22.
SELECT-OPTIONS : S_ZKUNNR FOR ZSDT0070-ZKUNNR MODIF ID Z01.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN FUNCTION KEY 1.

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
  APPEND &1. CLEAR &1.
END-OF-DEFINITION.
