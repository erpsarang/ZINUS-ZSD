*----------------------------------------------------------------------*
* Program ID  : ZSDR0013                                               *
* Title       : Calculation & HQ Applied                               *
* Author      : E00033                                                 *
* Create Date : 2020.09.09                                             *
* T_CODE      : ZSDR0013                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.09.09   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0013 MESSAGE-ID ZSD.

INCLUDE ZSDR0013TOP.
INCLUDE ZSDR0013C01.
INCLUDE ZSDR0013PBO.
INCLUDE ZSDR0013PA1.
INCLUDE ZSDR0013F01.
INCLUDE ZSDR0013F02.

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
    CASE GV_ZTYPE.
      WHEN 'P'.
        CALL SCREEN 9100.
      WHEN 'S'.
        CALL SCREEN 9200.
      WHEN OTHERS.
        CALL SCREEN 9000.
    ENDCASE.
  ELSE.
    MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
  ENDIF.
