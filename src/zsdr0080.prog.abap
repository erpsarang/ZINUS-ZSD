*----------------------------------------------------------------------*
* Program ID  : ZSDR0080                                               *
* Title       : Sales report                                           *
* Author      : E00033                                                 *
* Create Date : 2020.12.09                                             *
* T_CODE      : ZSDR0080                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.12.09   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0080 MESSAGE-ID ZMCSD1.

INCLUDE ZSDR0080_TOP.
INCLUDE ZSDR0080_CLS.
INCLUDE ZSDR0080_SCR.
INCLUDE ZSDR0080_O01.
INCLUDE ZSDR0080_I01.
INCLUDE ZSDR0080_F01.
INCLUDE ZSDR0080_ALV.

*----------------------------------------------------------------------*
**INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
**AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_KUNNR-LOW.
  PERFORM F4_KUNNR CHANGING S_KUNNR-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_KUNNR-HIGH.
  PERFORM F4_KUNNR CHANGING S_KUNNR-HIGH.
*----------------------------------------------------------------------*
**AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
**START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM AUTHORIZATION_CHECK.
  PERFORM CHECK_OBLIGATORY.

  PERFORM GET_BASE_DATA.
  PERFORM GET_STATUS_VBELN.

  IF GT_DISP[] IS NOT INITIAL.
    CLEAR GT_DISP.
    SORT GT_DISP BY KUNNR VBELN POSNR.
    DESCRIBE TABLE GT_DISP LINES GV_LINES.
    MESSAGE S006 WITH GV_LINES.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE S002(ZMCSD1) DISPLAY LIKE 'E'.

  ENDIF.
*----------------------------------------------------------------------*
**END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
