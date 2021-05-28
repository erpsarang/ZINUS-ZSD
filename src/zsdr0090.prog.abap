*----------------------------------------------------------------------*
* Program ID  : zsdr0090                                               *
* Title       : Creation for FOC Sample                                *
* Author      : E00059                                                 *
* Create Date : 2020.12.14                                             *
* T_CODE      : ZSDR0090                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.12.14   E00059    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0090 MESSAGE-ID ZMCSD1.

INCLUDE ZSDR0090_TOP.
INCLUDE ZSDR0090_SCR.
INCLUDE ZSDR0090_CLS.
INCLUDE ZSDR0090_O01.
INCLUDE ZSDR0090_I01.
INCLUDE ZSDR0090_F01.

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

  PERFORM CHECK_DATA.

  IF P_SEAR = C_X.
    PERFORM GET_DATA.
  ELSE.
    PERFORM EXCEL_UPLOAD.
  ENDIF.

END-OF-SELECTION.
  IF GT_DATA[] IS NOT INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE S002 DISPLAY LIKE C_E.
  ENDIF.
