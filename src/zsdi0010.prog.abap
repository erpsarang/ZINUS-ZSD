*----------------------------------------------------------------------*
* Program ID  : ZSDI0010                                               *
* Title       : Sales Order Interface (SAP -> Import and Export )      *
* Author      : E00033                                                 *
* Create Date : 2020.09.02                                             *
* T_CODE      : ZSDI0010                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.09.02   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDI0010 MESSAGE-ID ZSD.

INCLUDE ZSDI0010TOP.
INCLUDE ZSDI0010C01.
INCLUDE ZSDI0010PBO.
INCLUDE ZSDI0010PAI.
INCLUDE ZSDI0010F01.
INCLUDE ZSDI0010F02.


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
    PERFORM CALL_VIEW USING 'ZSDT0120'.
  ENDIF.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_SELECTION.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM AUTHORIZATION_CHECK.
  IF P_LOG = 'X'.
    PERFORM GET_LOG_DATA.
  ELSE.
    PERFORM GET_DATA.
  ENDIF.
*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  IF P_LOG = 'X'.
    CALL SCREEN 9100.
  ELSE.
    DESCRIBE TABLE GT_LIST LINES DATA(LV_LINE).
    IF LV_LINE IS INITIAL.
      MESSAGE S001 DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE s014 WITH LV_LINE.
      CALL SCREEN 9000.
    ENDIF.
  ENDIF.
