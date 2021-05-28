*----------------------------------------------------------------------*
* Program ID  : ZSDB0050                                               *
* Title       : Create Sales Order to Billing                          *
* Author      : E00033                                                 *
* Create Date : 2020.07.21                                             *
* T_CODE      : ZSDB0050                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.07.21   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDB0050 MESSAGE-ID ZSD.

INCLUDE ZSDB0050TOP.
INCLUDE ZSDB0050C01.
INCLUDE ZSDB0050O01.
INCLUDE ZSDB0050I01.
INCLUDE ZSDB0050F01.
INCLUDE ZSDB0050F02.

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
  PERFORM SET_INIT.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'FC01'.
    PERFORM TEMPLATE_DOWNLOAD.
  ENDIF.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR .
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FILENAME_INPUT_HELP_EXCEL CHANGING P_FILE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM TEMPLATE_BUTTON.
  PERFORM SET_SELECTION.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM AUTHORIZATION_CHECK.
  CASE 'X'.
    WHEN P_UPLOAD.
      PERFORM UPLOAD_DATA.
      PERFORM MAKE_DATA.
    WHEN P_DISP.
      PERFORM GET_DATA.
  ENDCASE.
*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  IF GT_LIST[] IS INITIAL.
    MESSAGE S001 DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.
