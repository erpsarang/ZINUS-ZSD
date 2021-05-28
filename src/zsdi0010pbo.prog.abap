*&---------------------------------------------------------------------*
*& Include          ZSDI0010PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  DATA: BEGIN OF LT_CODE OCCURS 0,
          FCODE(12),
        END OF LT_CODE.

  SET PF-STATUS 'S9000' EXCLUDING LT_CODE.
  SET TITLEBAR 'T9000'.

ENDMODULE. " STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.

  IF GO_CUSTOM IS INITIAL.
    PERFORM SET_FRAME.
    PERFORM SET_VARIANT.
    PERFORM SET_SORT.
    PERFORM SET_TOOLBAR_EXCLUDE.
    PERFORM SET_LAYOUT.
    PERFORM SET_EVENT  USING GO_GRID.            " Event
    PERFORM SET_FIELDCAT.
    PERFORM SET_DISPLAY.
  ELSE.
    PERFORM REFRESH_LIST USING GO_GRID.
  ENDIF.

ENDMODULE. " CREATE_OBJECT OUTPUT
*&---------------------------------------------------------------------*
*& Module STATUS_9100 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.


  SET PF-STATUS 'S9100' EXCLUDING LT_CODE.
  SET TITLEBAR 'T9100'.

ENDMODULE. " STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT_9100 OUTPUT.

  IF GO_CUSTOM1 IS INITIAL.
    PERFORM SET_FRAME_9100.
    PERFORM SET_VARIANT_9100.
    PERFORM SET_SORT_9100.
    PERFORM SET_TOOLBAR_EXCLUDE.
    PERFORM SET_LAYOUT_9100.
    PERFORM SET_EVENT  USING GO_GRID1.            " Event
    PERFORM SET_FIELDCAT_9100.
    PERFORM SET_DISPLAY_9100.
  ELSE.
    PERFORM REFRESH_LIST USING GO_GRID1.
  ENDIF.

ENDMODULE. " CREATE_OBJECT OUTPUT
