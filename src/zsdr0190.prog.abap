*&---------------------------------------------------------------------*
*& Program ID   : ZSDR0190
*& Program Name : Delivery Rescheduling
*& Created Date : 2021.04.20
*& Programmer   : E00035
*& Description  : [SD] Delivery Rescheduling
*&---------------------------------------------------------------------*
*& 001. 04/20/2021
*&---------------------------------------------------------------------*
REPORT ZSDR0190 MESSAGE-ID ZSD.

INCLUDE ZSD0190_TOP.
INCLUDE ZSD0190_CLS.
INCLUDE ZSD0190_SCR.
INCLUDE ZSD0190_O01.
INCLUDE ZSD0190_I01.
INCLUDE ZSD0190_F01.

*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN F4
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FILENAME_F4 CHANGING P_FILE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM DOWNLOAD_FORMAT.
    WHEN 'FC02'.
      CALL TRANSACTION 'ZSDR0100'.
  ENDCASE.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM DATA_FROM_EXCEL.

*&---------------------------------------------------------------------*
*& END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_DISP[] IS NOT INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE S001 DISPLAY LIKE 'E'.
  ENDIF.
