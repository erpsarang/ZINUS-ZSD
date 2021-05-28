*&---------------------------------------------------------------------*
*& Include          ZSDI0020PBO
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

  IF GV_NOT_CONFIRM IS INITIAL.
    LT_CODE-FCODE = 'SEND'.
    APPEND LT_CODE. CLEAR : LT_CODE.
  ENDIF.

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
    PERFORM REFRESH_LIST.
  ENDIF.

ENDMODULE. " CREATE_OBJECT OUTPUT
