*----------------------------------------------------------------------*
* Program ID  : ZSDI0020                                               *
* Title       : Sales Price Interface (SAP -> JDE)                     *
* Author      : E00033                                                 *
* Create Date : 2020.11.03                                             *
* T_CODE      : ZSDI0020                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.11.03   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDI0020 MESSAGE-ID ZSD.

INCLUDE ZSDI0020TOP.
INCLUDE ZSDI0020C01.
INCLUDE ZSDI0020PBO.
INCLUDE ZSDI0020PAI.
INCLUDE ZSDI0020F01.
INCLUDE ZSDI0020F02.

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
   IF GV_ERROR IS INITIAL.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE S000 WITH TEXT-E01 TEXT-E02 DISPLAY LIKE 'E'.
  ENDIF.
