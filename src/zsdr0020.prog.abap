*----------------------------------------------------------------------*
* Program ID  : ZSDR0020                                               *
* Title       : Display Sales Price                                    *
* Author      : E00033                                                 *
* Create Date : 2020.10.13                                             *
* T_CODE      : ZSDR0020                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.10.13   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0020 MESSAGE-ID ZMCSD1.

INCLUDE ZSDR0020_TOP.
INCLUDE ZSDR0020_SCR.
INCLUDE ZSDR0020_CLS.
INCLUDE ZSDR0020_O01.
INCLUDE ZSDR0020_I01.
INCLUDE ZSDR0020_F01.
INCLUDE ZSDR0020_ALV.

*----------------------------------------------------------------------*
**INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
**AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION_SCREEN.

*----------------------------------------------------------------------*
**AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
**START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM AUTHORIZATION_CHECK.
  PERFORM CHECK_ORG.
  IF GT_ZSDT0040[] IS INITIAL.
    PERFORM SELECT_DATA.
  ELSE.
    PERFORM SELECT_DATA_INTERCOM. "관계사일경우
  ENDIF.

*----------------------------------------------------------------------*
**END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF GT_DISP[] IS NOT INITIAL.
    CLEAR GT_DISP.
    SORT GT_DISP BY MATNR.
    DESCRIBE TABLE GT_DISP LINES GV_LINES.

    MESSAGE S006 WITH GV_LINES.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE S002(ZMCSD1) DISPLAY LIKE 'E'.
  ENDIF.
