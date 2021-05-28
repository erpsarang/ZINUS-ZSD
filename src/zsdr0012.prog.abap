*----------------------------------------------------------------------*
* Program ID  : ZSDR0012                                               *
* Title       : Input Sales price (Non SAP)                            *
* Author      : E00033                                                 *
* Create Date : 2020.08.12                                             *
* T_CODE      : ZSDR0012                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.08.12   E00033    Initial Release                               *
*----------------------------------------------------------------------*
REPORT ZSDR0012 MESSAGE-ID ZSD.

INCLUDE ZSDR0012TOP.
INCLUDE ZSDR0012C01.
INCLUDE ZSDR0012O01.
INCLUDE ZSDR0012I01.
INCLUDE ZSDR0012F01.
INCLUDE ZSDR0012F02.

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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_INCOMP.
  PERFORM SEARCH_HELP_INTERCOMPANY USING 'P_INCOMP'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZKUNNR-LOW.
  PERFORM SEARCH_HELP_SHIP-TOPARTY USING 'S_ZKUNNR-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZKUNNR-HIGH.
  PERFORM SEARCH_HELP_SHIP-TOPARTY USING 'S_ZKUNNR-HIGH'.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
*  PERFORM AUTHORIZATION_CHECK.
  PERFORM CHECK_INPUT.
  PERFORM GET_DATA.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
  CALL SCREEN 9000.
