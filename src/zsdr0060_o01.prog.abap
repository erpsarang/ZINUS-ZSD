*&---------------------------------------------------------------------*
*& Include          ZSDR0060_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA : fcode TYPE TABLE OF sy-ucomm WITH HEADER LINE.
  IF p_sear IS NOT INITIAL.
    fcode = TEXT-007. "SAVE
    APPEND fcode.
  ENDIF.
  SET PF-STATUS 'STATUS 100' EXCLUDING fcode.
  SET TITLEBAR 'TITLEBAR 100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_alv OUTPUT.
  IF gr_alv_grid IS INITIAL.
    PERFORM alv_create_docking_container.
    PERFORM alv_create_grid.
    PERFORM alv_field_catalog.
    PERFORM alv_layout USING gs_layout 'X' 'X' '' 'D' '' '' '' ''.
*    PERFORM ALV_CELL_STYLE.
*    PERFORM ALV_CELL_COLOR.
    PERFORM alv_event.
    PERFORM alv_html.
    PERFORM alv_display.
  ELSE.
    PERFORM alv_refresh.
  ENDIF.
ENDMODULE.                 " SET_AL
