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

  CLEAR : LT_CODE, LT_CODE[].
***  IT'S BLOCKED FOR TEST.
*  IF P_DIS = 'X' AND P_DATE2 < SY-DATUM.
*    LT_CODE-FCODE = 'COPY'.
*    APPEND LT_CODE. CLEAR LT_CODE.
*    LT_CODE-FCODE = 'SAVE'.
*    APPEND LT_CODE. CLEAR LT_CODE.
*    LT_CODE-FCODE = 'EXCEL'.
*    APPEND LT_CODE. CLEAR LT_CODE.
*  ENDIF.
*
*  IF LT_CODE[] IS INITIAL.
**NOT ALLOW MODIFY CONFIRMED DATA
*    READ TABLE GT_LIST WITH KEY ZCONFIRM = 'X'.
*    IF SY-SUBRC EQ 0.
*      LT_CODE-FCODE = 'COPY'.
*      APPEND LT_CODE. CLEAR LT_CODE.
*      LT_CODE-FCODE = 'SAVE'.
*      APPEND LT_CODE. CLEAR LT_CODE.
*      LT_CODE-FCODE = 'EXCEL'.
*      APPEND LT_CODE. CLEAR LT_CODE.
*    ENDIF.
*  ENDIF.

*  IF P_DIS IS NOT INITIAL AND
*     GV_NOT_CONFIRM IS INITIAL.
  IF GV_NOT_CONFIRM IS INITIAL.
    LT_CODE-FCODE = 'SAVE'.
    APPEND LT_CODE. CLEAR LT_CODE.

    LT_CODE-FCODE = 'COPY'.
    APPEND LT_CODE. CLEAR LT_CODE.

    LT_CODE-FCODE = 'DATE'.
    APPEND LT_CODE. CLEAR LT_CODE.

    LT_CODE-FCODE = 'EXCEL'.
    APPEND LT_CODE. CLEAR LT_CODE.
  ENDIF.

  LT_CODE-FCODE = 'DATE'.
  APPEND LT_CODE. CLEAR LT_CODE.

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
    PERFORM CELL_STYLE.
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
*& Module CREATE_OBJECT_9100 OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_OBJECT_9100 OUTPUT.

  IF GO_CUSTOM1 IS INITIAL.
    PERFORM SET_FRAME_9100.
    PERFORM SET_TOOLBAR_EXCLUDE.
    PERFORM SET_LAYOUT_9100.
    PERFORM SET_EVENT  USING GO_GRID1.            " Event
    PERFORM SET_FIELDCAT_9100.
    PERFORM SET_DISPLAY_9100.
  ELSE.
    PERFORM REFRESH_LIST_9100.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9200 OUTPUT.
  SET PF-STATUS '9200'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
