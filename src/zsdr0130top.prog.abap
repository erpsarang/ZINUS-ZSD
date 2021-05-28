*&---------------------------------------------------------------------*
*& Include          ZSDR0130TOP
*&---------------------------------------------------------------------*
TABLES : EDIDC.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*

* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        DOCNUM LIKE EDIDC-DOCNUM,
        ICON(4),
        CREDAT LIKE EDIDC-CREDAT,
        MESTYP LIKE EDIDC-MESTYP,
        STATUS LIKE EDIDC-STATUS,
        MESSAGE(100),
        EBELN  LIKE EKKO-EBELN,
        BEDAT  LIKE EKKO-BEDAT,
        VBELN  LIKE VBAK-VBELN,
        OPEN_PO(4),
        LIFNR  LIKE VBPA-LIFNR,
        LIFNR_TXT  LIKE BUT000-NAME_ORG1,
        CELLSTYLE    TYPE LVC_T_STYL,
        CELLCOLOR    TYPE LVC_T_SCOL,
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

SELECT-OPTIONS : S_DOCNUM FOR EDIDC-DOCNUM,
                 S_CREDAT FOR EDIDC-CREDAT OBLIGATORY,
                 S_MESTYP FOR EDIDC-MESTYP.

SELECTION-SCREEN END OF BLOCK B1.

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
