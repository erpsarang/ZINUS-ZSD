*----------------------------------------------------------------------*
* Program ID  : zsdr0060                                               *
* Title       : Shipment Cost Price Master                             *
* Author      : E00059                                                 *
* Create Date : 2020.11.09                                             *
* T_CODE      : ZSDR0060                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.11.09   E00059    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0060 MESSAGE-ID ZMCSD1.

INCLUDE ZSDR0060_TOP.
INCLUDE ZSDR0060_SCR.
INCLUDE ZSDR0060_CLS.
INCLUDE ZSDR0060_O01.
INCLUDE ZSDR0060_I01.
INCLUDE ZSDR0060_F01.


INITIALIZATION.
  PERFORM INIT.

  SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN C_FC01.
      PERFORM EXCEL_TEMPLATE_DOWN.
  ENDCASE .

AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM GET_EXCEL_NAME USING P_FILE.

START-OF-SELECTION.
  IF P_SEAR = C_X.
    PERFORM GET_DATA.
  ELSE.
    PERFORM CHECK_INPUT_VALUE.
    PERFORM EXCEL_UPLOAD.
  ENDIF.

END-OF-SELECTION.
  IF GT_DATA[] IS NOT INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE S002 DISPLAY LIKE C_E.
  ENDIF.
