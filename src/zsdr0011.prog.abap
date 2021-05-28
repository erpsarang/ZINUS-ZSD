*----------------------------------------------------------------------*
* Program ID  : ZSDR0011                                               *
* Title       : Margin Management                                      *
* Author      : E00033                                                 *
* Create Date : 2020.08.06                                             *
* T_CODE      : ZSDR0011                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.08.06   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0011 MESSAGE-ID ZSD.

INCLUDE ZSDR0011TOP.
INCLUDE ZSDR0011C01.
INCLUDE ZSDR0011O01.
INCLUDE ZSDR0011I01.
INCLUDE ZSDR0011F01.
INCLUDE ZSDR0011F02.

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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_DATE.
  PERFORM SEARCH_HELP_SDATE USING 'P_DATE'.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
*  PERFORM AUTHORIZATION_CHECK.
  PERFORM CHECK_DATE.
  PERFORM GET_DATA.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  CALL SCREEN 9000.
