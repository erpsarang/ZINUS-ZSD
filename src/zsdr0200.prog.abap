*----------------------------------------------------------------------*
* Program ID  : ZSDR0200                                               *
* Title       : Batch change of sales price & info record              *
* Author      : E00033                                                 *
* Create Date : 2021.05.10                                             *
* T_CODE      : ZSDR0200                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2021.05.10   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0200 MESSAGE-ID ZSD.

INCLUDE ZSDR0200TOP.
INCLUDE ZSDR0200C01.
INCLUDE ZSDR0200O01.
INCLUDE ZSDR0200I01.
INCLUDE ZSDR0200F01.
INCLUDE ZSDR0200F02.

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
  PERFORM CHECK_INPUT.
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
