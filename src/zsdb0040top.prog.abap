*&---------------------------------------------------------------------*
*& Include          ZSDB0040TOP
*&---------------------------------------------------------------------*
TABLES : SSCRFIELDS, KONP, A305.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        KSCHL        LIKE A305-KSCHL,
        VKORG        LIKE KNMT-VKORG,
        VTWEG        LIKE A305-VTWEG,
        KUNNR1       LIKE A305-KUNNR,
        KUNNR1_TXT   LIKE KNA1-NAME1,
        KUNNR2       LIKE A305-KUNNR,
        KUNNR2_TXT   LIKE KNA1-NAME1,
        MATNR        LIKE A305-MATNR,
        MATNR_TXT    LIKE MAKT-MAKTX,
        KBETR        LIKE KONP-KBETR,
        KONWA        LIKE KONP-KONWA,
        KPEIN        LIKE KONP-KPEIN,
        KMEIN        LIKE KONP-KMEIN,
        DATAB        LIKE RV13A-DATAB,
        DATBI        LIKE RV13A-DATBI,
        ICON(4),
        MESSAGE(100),
        CELLSTYLE    TYPE LVC_T_STYL,
        CELLCOLOR    TYPE LVC_T_SCOL,
      END OF GT_LIST.

DATA: BEGIN OF GT_UPLOAD1 OCCURS 0, "SALES Org. NE '1001'
        KSCHL  TYPE CHAR40,
        VKORG  TYPE CHAR40,
        VTWEG  TYPE CHAR40,
        KUNNR1 TYPE CHAR40,
        MATNR  TYPE CHAR40,
        KBETR  TYPE CHAR40,
        KONWA  TYPE CHAR40,
        KPEIN  TYPE CHAR40,
        KMEIN  TYPE CHAR40,
        DATAB  TYPE CHAR40,
        DATBI  TYPE CHAR40,
      END OF GT_UPLOAD1.

DATA: BEGIN OF GT_UPLOAD2 OCCURS 0, "SALES Org. EQ '1001'
        KSCHL  TYPE CHAR40,
        VKORG  TYPE CHAR40,
        VTWEG  TYPE CHAR40,
        KUNNR1 TYPE CHAR40,
        KUNNR2 TYPE CHAR40,
        MATNR  TYPE CHAR40,
        KBETR  TYPE CHAR40,
        KONWA  TYPE CHAR40,
        KPEIN  TYPE CHAR40,
        KMEIN  TYPE CHAR40,
        DATAB  TYPE CHAR40,
        DATBI  TYPE CHAR40,
      END OF GT_UPLOAD2.

DATA : GT_0040 LIKE TABLE OF ZSDT0040 WITH HEADER LINE.

DATA : GV_SUCCESS(5),
       GV_FAILURE(5).

*&---------------------------------------------------------------------*
*&  GLOBAL VARIABLE DECLARATION
*&---------------------------------------------------------------------*
DATA: OK_CODE TYPE SY-UCOMM.

*-----------------------------------------------------------------------
* BDC
*-----------------------------------------------------------------------
DATA : GT_BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
       GT_BDCMSG  TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
DATA : GS_CTU_PARAMS TYPE CTU_PARAMS.
*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T01.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  TEXT-001 FOR FIELD P_VKORG.
SELECTION-SCREEN POSITION  25.
PARAMETERS    : P_VKORG   LIKE KNMT-VKORG OBLIGATORY.
SELECTION-SCREEN POSITION  40.
PARAMETERS    : T_VKORG(30) MODIF ID A.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  TEXT-002 FOR FIELD P_FILE.
SELECTION-SCREEN POSITION  25.
PARAMETERS    : P_FILE LIKE RLGRAP-FILENAME DEFAULT 'C:\' .
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  TEXT-009 FOR FIELD P_MODE.
SELECTION-SCREEN POSITION  25.
PARAMETERS    : P_MODE LIKE CTU_PARAMS-DISMODE OBLIGATORY DEFAULT 'N' .
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END OF BLOCK B2.
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
  COLLECT &1. CLEAR &1.
END-OF-DEFINITION.
