*&---------------------------------------------------------------------*
*& Include          ZSDR0020_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: LT_CODE(20) OCCURS 0 WITH HEADER LINE.

  SET TITLEBAR  '0100' WITH TEXT-T01.
  SET PF-STATUS '0100' EXCLUDING LT_CODE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_0100 OUTPUT.
  IF GC_DOCKING IS INITIAL.
    PERFORM SETTING_CONTAINER.
    PERFORM SET_GRID CHANGING GC_CONTAINER GC_GRID.
    PERFORM DISPLAY_ALV USING GC_GRID.

  ELSE.
* ALV Grid Refresh
    CALL METHOD GC_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STBL.
  ENDIF.

ENDMODULE.
