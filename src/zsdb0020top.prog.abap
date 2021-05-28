*&---------------------------------------------------------------------*
*& Include          ZSDB0020TOP
*&---------------------------------------------------------------------*
TABLES : SSCRFIELDS.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        MATNR        LIKE MAST-MATNR,
        WERKS        LIKE T001W-WERKS,
        IDNRK        LIKE STPO-IDNRK,
        BMENG        LIKE STKO-BMENG,
        MENGE        LIKE STPO-MENGE,
        MEINS        LIKE MARA-MEINS,
        ICON(4),
        MESSAGE(100),
        CELLSTYLE    TYPE LVC_T_STYL,
        CELLCOLOR    TYPE LVC_T_SCOL,
      END OF GT_LIST.

DATA: BEGIN OF GT_UPLOAD OCCURS 0,
        MATNR TYPE CHAR40,
        WERKS TYPE CHAR40,
        IDNRK TYPE CHAR40,
        BMENG TYPE CHAR40,
        MENGE TYPE CHAR40,
      END OF GT_UPLOAD.

DATA : BEGIN OF GT_COMP OCCURS 0,
         MATNR LIKE MAST-MATNR,
         IDNRK LIKE STPO-IDNRK,
         BMENG LIKE STKO-BMENG,
         MENGE LIKE STPO-MENGE,
         MEINS LIKE MARA-MEINS,
       END OF GT_COMP.

DATA : GV_SUCCESS(5),
       GV_FAILURE(5).

*&---------------------------------------------------------------------*
*&  GLOBAL VARIABLE DECLARATION
*&---------------------------------------------------------------------*
DATA: OK_CODE TYPE SY-UCOMM.

*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  TEXT-001 FOR FIELD P_WERKS.
SELECTION-SCREEN POSITION  25.
PARAMETERS    : P_WERKS   LIKE T001W-WERKS OBLIGATORY.
SELECTION-SCREEN POSITION  40.
PARAMETERS    : T_WERKS(30) MODIF ID A.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20)  TEXT-002 FOR FIELD P_FILE.
SELECTION-SCREEN POSITION  25.
PARAMETERS    : P_FILE LIKE RLGRAP-FILENAME DEFAULT 'C:\' .
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
  APPEND &1. CLEAR &1.
END-OF-DEFINITION.
