*----------------------------------------------------------------------*
* Program ID  : ZSDR0014                                               *
* Title       : Intercompany Price Management                          *
* Author      : E00033                                                 *
* Create Date : 2020.10.21                                             *
* T_CODE      : ZSDR0014                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.10.21   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0014 MESSAGE-ID ZSD.

INCLUDE ZSDR0014TOP.
INCLUDE ZSDR0014C01.
INCLUDE ZSDR0014PBO.
INCLUDE ZSDR0014PAI.
INCLUDE ZSDR0014F01.
INCLUDE ZSDR0014F02.

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
  CASE ABAP_ON.
    WHEN P_INFO.
      PERFORM GET_DATA.
    WHEN P_SALES.
      PERFORM GET_DATA_SALES_PRICE.
  ENDCASE.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  IF GV_ERROR IS INITIAL.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
  ENDIF.
