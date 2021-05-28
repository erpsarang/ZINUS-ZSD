*----------------------------------------------------------------------*
* Program ID  : zsdr0030                                               *
* Title       : BP Sales Area Expansion                                *
* Author      : E00059                                                 *
* Create Date : 2020.10.21                                             *
* T_CODE      : ZSDR0030                                               *
*----------------------------------------------------------------------*
* Date         Authors   Description                                   *
*----------------------------------------------------------------------*
* 2020.10.21   E00059    Initial Release                               *
*----------------------------------------------------------------------*
REPORT zsdr0030 MESSAGE-ID zmcsd1.

INCLUDE zsdr0030_top.
INCLUDE zsdr0030_scr.
INCLUDE zsdr0030_cls.
INCLUDE zsdr0030_o01.
INCLUDE zsdr0030_i01.
INCLUDE zsdr0030_f01.

INITIALIZATION.
  PERFORM init.

SELECTION-SCREEN FUNCTION KEY 1.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN c_fc01.
      PERFORM excel_template_down.
  ENDCASE .

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_excel_name USING p_file.

START-OF-SELECTION.
IF p_sear = c_x.
  PERFORM get_data.
ELSE.
  PERFORM excel_upload.
ENDIF.

END-OF-SELECTION.
IF gt_data[] IS NOT INITIAL.
  CALL SCREEN 0100.
ELSE.
MESSAGE S002 DISPLAY LIKE c_e.
ENDIF.
