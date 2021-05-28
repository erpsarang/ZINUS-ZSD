*----------------------------------------------------------------------*
* Program ID  : ZSDB0040                                               *
* Title       : Create Condition                                       *
* Author      : E00033                                                 *
* Create Date : 2020.07.14                                             *
* T_CODE      : ZSDB0040                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.07.14   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDB0040 MESSAGE-ID ZSD.

INCLUDE ZSDB0040TOP.
INCLUDE ZSDB0040C01.
INCLUDE ZSDB0040O01.
INCLUDE ZSDB0040I01.
INCLUDE ZSDB0040F01.
INCLUDE ZSDB0040F02.

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
