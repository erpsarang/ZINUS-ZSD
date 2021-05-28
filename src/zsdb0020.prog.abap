*----------------------------------------------------------------------*
* Program ID  : ZSDB0020                                               *
* Title       : Create Sales BOM                                       *
* Author      : E00033                                                 *
* Create Date : 2020.07.13                                             *
* T_CODE      : ZSDB0020                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.07.13   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDB0020 MESSAGE-ID ZSD.

INCLUDE ZSDB0020TOP.
INCLUDE ZSDB0020C01.
INCLUDE ZSDB0020O01.
INCLUDE ZSDB0020I01.
INCLUDE ZSDB0020F01.
INCLUDE ZSDB0020F02.

*----------------------------------------------------------------------*
*INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM SET_INIT.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'FC01'.
    PERFORM TEMPLATE_DOWNLOAD.
  ENDIF.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM TEMPLATE_BUTTON.
  PERFORM MODIFY_SELECTION_SCREEN.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR .
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FILENAME_INPUT_HELP_EXCEL CHANGING P_FILE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM AUTHORIZATION_CHECK.
  PERFORM UPLOAD_DATA.
  PERFORM MAKE_DATA.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  IF GT_LIST[] IS INITIAL.
    MESSAGE S001 DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.
