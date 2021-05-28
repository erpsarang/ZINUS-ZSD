*&---------------------------------------------------------------------*
*& Include          ZSDB0010O01
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

  CLEAR : LT_CODE, LT_CODE[].
  IF P_OPENPO EQ 'X'.
    LT_CODE-FCODE = 'APPROVE'.
    APPEND LT_CODE. CLEAR LT_CODE.
  ENDIF.

  IF P_DISP EQ 'X'.
    LT_CODE-FCODE = 'APPROVE'.
    APPEND LT_CODE. CLEAR LT_CODE.
    LT_CODE-FCODE = 'CREATE'.
    APPEND LT_CODE. CLEAR LT_CODE.
  ENDIF.

  SET PF-STATUS 'S9000' EXCLUDING LT_CODE.
  IF P_UPLOAD = 'X'.
    SET TITLEBAR 'T9000' WITH TEXT-T01.
  ELSE.
    SET TITLEBAR 'T9000' WITH TEXT-T02.
  ENDIF.

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
    IF P_UPLOAD = 'X'.
      PERFORM SET_FIELDCAT.
    ELSE.
      PERFORM SET_FIELDCAT_DISP.
    ENDIF.
    PERFORM CELLCOLOR.
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
