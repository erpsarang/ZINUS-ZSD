*&---------------------------------------------------------------------*
*& Include          ZSDR0011O01
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

  _CLEAR LT_CODE.
  IF GV_NOT_CONFIRM IS INITIAL.
    LT_CODE-FCODE = 'APPLY'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'REFRESH'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDC0010'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDR0011'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDR0012'.
    APPEND LT_CODE. CLEAR : LT_CODE.
  ENDIF.

  SET PF-STATUS 'S9000' EXCLUDING LT_CODE.
  SET TITLEBAR 'T9000'  WITH TEXT-TIT.

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
*&---------------------------------------------------------------------*
*& Module STATUS_9100 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.

  _CLEAR LT_CODE.
  IF GV_NOT_CONFIRM IS INITIAL.
    LT_CODE-FCODE = 'APPLY'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'REFRESH'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDC0010'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDR0011'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDR0012'.
    APPEND LT_CODE. CLEAR : LT_CODE.
  ENDIF.

  SET PF-STATUS 'S9100' EXCLUDING LT_CODE.
  SET TITLEBAR 'T9100'  WITH TEXT-TIT.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_OBJECT_9100 OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_OBJECT_9100 OUTPUT.

  IF GO_CUSTOM IS INITIAL.
    PERFORM SET_FRAME.
    PERFORM SET_VARIANT.
    PERFORM SET_SORT.
    PERFORM SET_TOOLBAR_EXCLUDE.
    PERFORM SET_LAYOUT.
    PERFORM SET_EVENT  USING GO_GRID.
    PERFORM SET_FIELDCAT_9100.
    PERFORM SET_DISPLAY.
  ELSE.
    PERFORM REFRESH_LIST.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9200 OUTPUT.

  _CLEAR LT_CODE.
  IF GV_NOT_CONFIRM IS INITIAL.
    LT_CODE-FCODE = 'APPLY'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'REFRESH'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDC0010'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDR0011'.
    APPEND LT_CODE. CLEAR : LT_CODE.

    LT_CODE-FCODE = 'ZSDR0012'.
    APPEND LT_CODE. CLEAR : LT_CODE.
  ENDIF.

  SET PF-STATUS 'S9000' EXCLUDING LT_CODE.
  SET TITLEBAR 'T9000'  WITH TEXT-TIT.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_OBJECT_9200 OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_OBJECT_9200 OUTPUT.

  IF GO_CUSTOM IS INITIAL.
    PERFORM SET_FRAME.
    PERFORM SET_VARIANT.
    PERFORM SET_SORT.
    PERFORM SET_TOOLBAR_EXCLUDE.
    PERFORM SET_LAYOUT.
    PERFORM SET_EVENT  USING GO_GRID.
    PERFORM SET_FIELDCAT_9200.
    PERFORM SET_DISPLAY.
  ELSE.
    PERFORM REFRESH_LIST.
  ENDIF.
ENDMODULE.
