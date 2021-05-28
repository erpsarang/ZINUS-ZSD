*----------------------------------------------------------------------*
* Program ID  : ZSDR0100                                               *
* Title       : Sales Progress Mornitoring                             *
* Author      : E00059                                                 *
* Create Date : 2020.12.23                                             *
* T_CODE      : ZSDR0100                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.12.23   E00059    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0100 MESSAGE-ID ZMCSD1.

INCLUDE ZSDR0100_TOP.
INCLUDE ZSDR0100_SCR.
INCLUDE ZSDR0100_CLS.
INCLUDE ZSDR0100_O01.
INCLUDE ZSDR0100_I01.
INCLUDE ZSDR0100_F01.

INITIALIZATION.
  PERFORM INIT.

AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

START-OF-SELECTION.
  PERFORM GET_DATA.
  " select s/o without Qoutation
  IF C_BOX4 EQ ABAP_TRUE.
    PERFORM GET_DATA2.
  ENDIF.

  IF R_ALL IS INITIAL.
    PERFORM CHECKBOX.
  ENDIF.

*S_2021/03/15 BY E00064
  IF R_ALLB IS INITIAL.
    PERFORM CHECKBOX2.
  ENDIF.
*E_2021/03/15 BY E00064

END-OF-SELECTION.
  IF GT_MAIN[] IS NOT INITIAL.
    DESCRIBE TABLE GT_MAIN LINES GV_COUNT.
    MESSAGE S006 WITH GV_COUNT DISPLAY LIKE C_S.

    CALL SCREEN 0100.
  ELSE.
    MESSAGE S002 DISPLAY LIKE C_E.
  ENDIF.
