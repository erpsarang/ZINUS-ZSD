*&---------------------------------------------------------------------*
*& Include          ZSDR0080_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA : BEGIN OF LT_EXC OCCURS 0,
           FCODE LIKE RSMPE-FUNC,
         END OF LT_EXC.
  CLEAR : LT_EXC, LT_EXC[].

  LT_EXC-FCODE = 'PRINT'.
  APPEND LT_EXC. CLEAR : LT_EXC.
  IF P_VKORG NE '1002'.
    LT_EXC-FCODE = 'PREVIEW'.
    APPEND LT_EXC. CLEAR : LT_EXC.
    LT_EXC-FCODE = 'PDF'.
    APPEND LT_EXC. CLEAR : LT_EXC.
  ENDIF.

  SET TITLEBAR  '0100' WITH TEXT-T01.
  SET PF-STATUS '0100' EXCLUDING LT_EXC.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
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
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  DATA: LT_EXCL_100  TYPE TABLE OF SY-UCOMM,
        LS_UCOMM_100 TYPE SY-UCOMM.

  LS_UCOMM_100 = 'INFO'. APPEND LS_UCOMM_100 TO LT_EXCL_100.
  LS_UCOMM_100 = 'PRINT_IV'. APPEND LS_UCOMM_100 TO LT_EXCL_100.

  SET TITLEBAR  '0100' WITH TEXT-T02.
  SET PF-STATUS '0100' EXCLUDING LT_EXCL_100.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_0200 OUTPUT.
  IF GC_DOCKING_200 IS INITIAL.
    PERFORM SETTING_CONTAINER_200.
    PERFORM SET_GRID CHANGING GC_CONTAINER_200 GC_GRID_200.
    PERFORM DISPLAY_ALV_200.

  ELSE.
* ALV Grid Refresh
    CALL METHOD GC_GRID_200->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STBL_200.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  LS_UCOMM_100 = 'INFO'. APPEND LS_UCOMM_100 TO LT_EXCL_100.
  LS_UCOMM_100 = 'PRINT_IV'. APPEND LS_UCOMM_100 TO LT_EXCL_100.

  SET TITLEBAR  '0100' WITH TEXT-T03.
  SET PF-STATUS '0100' EXCLUDING LT_EXCL_100.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_0300 OUTPUT.
  IF GC_DOCKING_300 IS INITIAL.
    PERFORM SETTING_CONTAINER_300.
    PERFORM SET_GRID CHANGING GC_CONTAINER_300 GC_GRID_300.
    PERFORM DISPLAY_ALV_300.

  ELSE.
* ALV Grid Refresh
    CALL METHOD GC_GRID_300->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STBL_300.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0400 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0400 OUTPUT.
  LS_UCOMM_100 = 'INFO'. APPEND LS_UCOMM_100 TO LT_EXCL_100.
  LS_UCOMM_100 = 'PRINT_IV'. APPEND LS_UCOMM_100 TO LT_EXCL_100.

  SET TITLEBAR  '0100' WITH TEXT-T04.
  SET PF-STATUS '0100' EXCLUDING LT_EXCL_100.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0400 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_0400 OUTPUT.
  IF GC_DOCKING_400 IS INITIAL.
    PERFORM SETTING_CONTAINER_400.
    PERFORM SET_GRID CHANGING GC_CONTAINER_400 GC_GRID_400.
    PERFORM DISPLAY_ALV_400.

  ELSE.
* ALV Grid Refresh
    CALL METHOD GC_GRID_400->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STBL_400.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0500 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0500 OUTPUT.
  LS_UCOMM_100 = 'INFO'. APPEND LS_UCOMM_100 TO LT_EXCL_100.
  LS_UCOMM_100 = 'PRINT_IV'. APPEND LS_UCOMM_100 TO LT_EXCL_100.

  SET TITLEBAR  '0100' WITH TEXT-T05.
  SET PF-STATUS '0100' EXCLUDING LT_EXCL_100.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0500 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_0500 OUTPUT.
  IF GC_DOCKING_500 IS INITIAL.
    PERFORM SETTING_CONTAINER_500.
    PERFORM SET_GRID CHANGING GC_CONTAINER_500 GC_GRID_500.
    PERFORM DISPLAY_ALV_500.

  ELSE.
* ALV Grid Refresh
    CALL METHOD GC_GRID_500->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STBL_500.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0600 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0600 OUTPUT.
  LS_UCOMM_100 = 'INFO'. APPEND LS_UCOMM_100 TO LT_EXCL_100.
  LS_UCOMM_100 = 'PRINT_IV'. APPEND LS_UCOMM_100 TO LT_EXCL_100.

  SET TITLEBAR  '0100' WITH TEXT-T06.
  SET PF-STATUS '0100' EXCLUDING LT_EXCL_100.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0600 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_0600 OUTPUT.
  IF GC_DOCKING_600 IS INITIAL.
    PERFORM SETTING_CONTAINER_600.
    PERFORM SET_GRID CHANGING GC_CONTAINER_600 GC_GRID_600.
    PERFORM DISPLAY_ALV_600.

  ELSE.
* ALV Grid Refresh
    CALL METHOD GC_GRID_600->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STBL_600.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0700 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0700 OUTPUT.
  LS_UCOMM_100 = 'INFO'. APPEND LS_UCOMM_100 TO LT_EXCL_100.
  LS_UCOMM_100 = 'PRINT_IV'. APPEND LS_UCOMM_100 TO LT_EXCL_100.

  SET TITLEBAR  '0100' WITH TEXT-T07.
  SET PF-STATUS '0100' EXCLUDING LT_EXCL_100.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0700 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_0700 OUTPUT.
  IF GC_DOCKING_700 IS INITIAL.
    PERFORM SETTING_CONTAINER_700.
    PERFORM SET_GRID CHANGING GC_CONTAINER_700 GC_GRID_700.
    PERFORM DISPLAY_ALV_700.

  ELSE.
* ALV Grid Refresh
    CALL METHOD GC_GRID_700->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STBL_700.

  ENDIF.
ENDMODULE.
