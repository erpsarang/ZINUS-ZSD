*&---------------------------------------------------------------------*
*& Include          ZSDR0090_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: lt_code(20) OCCURS 0 WITH HEADER LINE.
  IF p_sear IS NOT INITIAL.
    CLEAR: lt_code, lt_code[].
    lt_code = 'EXEC'.
    APPEND lt_code.
    SET PF-STATUS 'STATUS 100' EXCLUDING lt_code.
  ELSE.
    CLEAR: lt_code, lt_code[].
    lt_code = 'DELT'. APPEND lt_code.
    lt_code = 'CHAN'. APPEND lt_code.
    lt_code = 'TRAN'. APPEND lt_code.
    lt_code = 'GIOT'. APPEND lt_code.
    SET PF-STATUS 'STATUS 100' EXCLUDING lt_code.
  ENDIF.
  SET TITLEBAR 'TITLEBAR 100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_alv OUTPUT.
  IF gr_alv_grid IS INITIAL.
    PERFORM alv_create_docking_container.
    PERFORM alv_create_grid.
    PERFORM alv_exclude_toolbar.
    PERFORM alv_field_catalog.
    PERFORM alv_layout USING gs_layout 'X' 'X' '' 'D' '' '' 'CELL_COLOR' 'CELL_STYLE'.
    PERFORM alv_cell_style.
    PERFORM alv_cell_color.
    PERFORM alv_event.
    PERFORM alv_html.
    PERFORM alv_display.
  ELSE.
    PERFORM alv_refresh.
  ENDIF.
ENDMODULE.                 " SET_AL
