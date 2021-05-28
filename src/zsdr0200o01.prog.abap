*&---------------------------------------------------------------------*
*& Include          ZSDR0200O01
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
  SET TITLEBAR 'T9000'  WITH TEXT-T02.


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
    PERFORM CELL_STYLE.
    PERFORM SET_EVENT  USING GO_GRID.            " Event
    PERFORM SET_FIELDCAT.
    PERFORM SET_DISPLAY.
  ELSE.
    PERFORM REFRESH_LIST.
  ENDIF.

ENDMODULE. " CREATE_OBJECT OUTPUT
*&---------------------------------------------------------------------*
*& Module STATUS_9100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9500 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_9500 OUTPUT.
  SET PF-STATUS '9500'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
