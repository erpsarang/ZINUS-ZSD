*&---------------------------------------------------------------------*
*& Report ZSDB0011
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDB0011.

TABLES : VBAK.

DATA :LS_HEADX  LIKE BAPISDH1X,
      LV_VBELN  LIKE BAPIVBELN-VBELN,
      LT_RETURN LIKE BAPIRET2   OCCURS 0 WITH HEADER LINE.

SELECT-OPTIONS : S_VBELN FOR VBAK-VBELN NO INTERVALS.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  CHECK S_VBELN[] IS NOT INITIAL.

  LOOP AT S_VBELN.
    CLEAR :  LS_HEADX, LV_VBELN,
             LT_RETURN, LT_RETURN[].

    LS_HEADX-UPDATEFLAG = 'D'.
    LV_VBELN = S_VBELN-LOW.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        SALESDOCUMENT    = LV_VBELN
        ORDER_HEADER_INX = LS_HEADX
      TABLES
        RETURN           = LT_RETURN.

    READ TABLE LT_RETURN WITH KEY TYPE   = 'S'
                                  ID     = 'V1'.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ENDIF.
  ENDLOOP.
*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
