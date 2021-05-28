*----------------------------------------------------------------------*
* Program ID  : ZSDR0180                                               *
* Title       : Price Status Information by Intercompany               *
* Author      : E00033                                                 *
* Create Date : 2021.02.25                                             *
* T_CODE      : ZSDR0180                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2021.02.25   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0180 MESSAGE-ID ZSD.

INCLUDE ZSDR0180TOP.
INCLUDE ZSDR0180C01.
INCLUDE ZSDR0180O01.
INCLUDE ZSDR0180I01.
INCLUDE ZSDR0180F01.
INCLUDE ZSDR0180F02.


*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
  PERFORM SET_INIT.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
PERFORM SET_SELECTION.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM GET_DATA.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  IF GT_LIST[] IS INITIAL.
    MESSAGE S001 DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.
