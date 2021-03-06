*----------------------------------------------------------------------*
* Program ID  : ZSDR0015                                               *
* Title       : Price report                                           *
* Author      : E00033                                                 *
* Create Date : 2020.11.25                                             *
* T_CODE      : ZSDR0015                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.11.25   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0015 MESSAGE-ID ZSD.

INCLUDE ZSDR0015TOP.
INCLUDE ZSDR0015C01.
INCLUDE ZSDR0015PBO.
INCLUDE ZSDR0015PAI.
INCLUDE ZSDR0015F01.
INCLUDE ZSDR0015F02.

*----------------------------------------------------------------------*
*INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM SET_INIT.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_SELECTION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_DATE.
  PERFORM SEARCH_HELP_DATE USING 'P_DATE'.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM GET_DATA.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  CALL SCREEN 9000.
