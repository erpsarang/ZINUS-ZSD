*---------------------------------------------------------------------*
*Module        : SD
*---------------------------------------------------------------------*
*Tcode         :  ZSDR0140
*Creator       :  E00064
*Create Date   :  2021.01.29
*Description   :  [SD] Create mass Quotation
*---------------------------------------------------------------------*
*                     Change History.
*---------------------------------------------------------------------*
*Chg No.       Chg Date         Modifier         Description
*---------------------------------------------------------------------*
*  000         2021/01/29       E00064      Initial Version
*---------------------------------------------------------------------*
REPORT ZSDR0140 MESSAGE-ID ZSD NO STANDARD PAGE HEADING.

*&---------------------------------------------------------------------*
*&     INCLUDE
*&---------------------------------------------------------------------*
INCLUDE ZSDR0140_TOP.
INCLUDE ZSDR0140_CLS.
INCLUDE ZSDR0140_PBO.
INCLUDE ZSDR0140_PAI.
INCLUDE ZSDR0140_F01.

*&--------------------------------------------------------------------*
*& INITIALIZATION
*&--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*&--------------------------------------------------------------------*
*& AT SELECTION-SCREEN OUTPUT
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM SELECT_FILE CHANGING P_FILE.

*&--------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&--------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
*      PERFORM EXCEL_LAYOUT_OUTPUT.
      PERFORM EXCEL_TEMPLATE_DOWN.
    WHEN 'FC02'.
      PERFORM CALL_QUOTATION_LIST.
  ENDCASE.

*&--------------------------------------------------------------------*
*& START-OF-SELECTION
*&--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM MAIN_PROCESS.

*&--------------------------------------------------------------------*
*& END-OF-SELECTION
*&--------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM DISPLAY.
