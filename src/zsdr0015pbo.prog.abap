*&---------------------------------------------------------------------*
*& Include          ZSDR0015PBO
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

  DATA : LV_TITLE TYPE CHAR100.

  LV_TITLE = 'Price report'.

  SET PF-STATUS 'S9000' EXCLUDING LT_CODE.
  SET TITLEBAR 'T9000' WITH LV_TITLE.

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
    PERFORM REFRESH_LIST.
  ENDIF.

ENDMODULE. " CREATE_OBJECT OUTPUT
