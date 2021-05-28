*&---------------------------------------------------------------------*
*& Include          ZSD0190_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  GV_TITLE = 'Delivery Rescheduling '.

  SET PF-STATUS 'PF_0100'.
  SET TITLEBAR  'TITLE_0100' WITH GV_TITLE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_0100 OUTPUT.
  IF GO_DOCKING IS INITIAL.
    PERFORM ALV_CREAT.
    PERFORM ALV_FIELD_CATALOG.
    PERFORM ALV_LAYOUT.
*    PERFORM CELL_STYLE.
    PERFORM ALV_TOOLBAR.
    PERFORM ALV_EVENT.
*    PERFORM ALV_HTML.
    PERFORM ALV_DISPLAY.
  ELSE.
    PERFORM ALV_REFRESH.
  ENDIF.
ENDMODULE.
