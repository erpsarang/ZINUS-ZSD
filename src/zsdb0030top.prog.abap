*&---------------------------------------------------------------------*
*& Include          ZSDB0030TOP
*&---------------------------------------------------------------------*
TABLES : SSCRFIELDS.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        KUNNR        TYPE KNMT-KUNNR,
        VKORG        TYPE KNMT-VKORG,
        VTWEG        TYPE KNMT-VTWEG,
        MATNR        TYPE KNMT-MATNR,
        KDMAT        TYPE KNMT-MATNR,
*       CLASS        TYPE RMCLF-CLASS,  " Not use HQ 2020.10.26
*       MWERT        TYPE RCTMS-MWERT,  " Not use HQ 2020.10.26
        ICON(4),
        MESSAGE(100),
        CELLSTYLE    TYPE LVC_T_STYL,
        CELLCOLOR    TYPE LVC_T_SCOL,
      END OF GT_LIST.

DATA: BEGIN OF GT_UPLOAD OCCURS 0,
        KUNNR TYPE KNMT-KUNNR,
        VKORG TYPE KNMT-VKORG,
        VTWEG TYPE KNMT-VTWEG,
        MATNR TYPE KNMT-MATNR,
        KDMAT TYPE KNMT-MATNR,
*       CLASS TYPE RMCLF-CLASS,         " Not use HQ 2020.10.26
*       MWERT TYPE RCTMS-MWERT,         " Not use HQ 2020.10.26
      END OF GT_UPLOAD.

DATA : GV_SUCCESS(5),
       GV_FAILURE(5).

DATA : GT_KNMT LIKE TABLE OF KNMT WITH HEADER LINE.
DATA : GT_KNMT_KEY LIKE TABLE OF KNMT WITH HEADER LINE.

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
  APPEND &1. CLEAR &1.
END-OF-DEFINITION.
