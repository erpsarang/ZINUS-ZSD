*----------------------------------------------------------------------*
***INCLUDE ZSDR0010I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_SDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_HELP_SDATE INPUT.

  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL,
        LS_RETURN TYPE DDSHRETVAL.
  DATA: LS_DYNPFIELDS TYPE DYNPREAD,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  DATA: LV_DATE    LIKE WORKFLDS-GKDAY,
        LV_DISPLAY LIKE WORKFLDS-DISPL.
  CLEAR :  GV_ZTYPE.

  CASE 'X'.
    WHEN RA_NORM.
      GV_ZTYPE = 'N'.
    WHEN RA_MDDP.
      GV_ZTYPE = 'M'.
    WHEN RA_INIT.
      GV_ZTYPE = 'I'.
    WHEN RA_MANU.
      GV_ZTYPE = 'P'.
    WHEN RA_SALES.
      GV_ZTYPE = 'S'.
  ENDCASE.

  IF RA_MANU EQ 'X' OR RA_SALES EQ 'X'.
    SELECT DISTINCT A~ZSTART,B~END_DATE, A~ZTYPE, A~ZCONFIRM
    INTO TABLE @DATA(LT_LINE)
    FROM ZSDT0021 AS A INNER JOIN ZSDT0020 AS B ON A~ZSTART = B~ZSTART
                                               AND A~ZTYPE  = B~ZTYPE
    WHERE A~ZTYPE = @GV_ZTYPE.

    IF SY-SUBRC = 0.
      SORT LT_LINE BY ZSTART DESCENDING.
      DELETE ADJACENT DUPLICATES FROM LT_LINE COMPARING ZSTART ZTYPE.
*-- Pop-up F4.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'ZSTART'
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = LT_LINE
          RETURN_TAB      = LT_RETURN
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.

      READ TABLE LT_RETURN INTO LS_RETURN INDEX 1.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
          EXPORTING
            INPUT  = LS_RETURN-FIELDVAL
          IMPORTING
            OUTPUT = GV_DATE.
      ENDIF.

    ELSE.
      CLEAR: LV_DATE.
      LV_DATE = SY-DATUM.

      CALL FUNCTION 'F4_DATE'
        EXPORTING
          DATE_FOR_FIRST_MONTH = LV_DATE
          DISPLAY              = LV_DISPLAY
        IMPORTING
          SELECT_DATE          = GV_DATE
        EXCEPTIONS
          OTHERS               = 4.
    ENDIF.


  ELSE.

    SELECT DISTINCT ZSTART, ZTYPE, ZCONFIRM
    INTO TABLE @DATA(LT_LINE2)
     FROM ZSDT0021 WHERE ZTYPE = @GV_ZTYPE.


    IF SY-SUBRC = 0.
      SORT LT_LINE BY ZSTART DESCENDING.
      DELETE ADJACENT DUPLICATES FROM LT_LINE COMPARING ZSTART ZTYPE.
*-- Pop-up F4.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'ZSTART'
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = LT_LINE2
          RETURN_TAB      = LT_RETURN
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.

      READ TABLE LT_RETURN INTO LS_RETURN INDEX 1.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
          EXPORTING
            INPUT  = LS_RETURN-FIELDVAL
          IMPORTING
            OUTPUT = GV_DATE.
      ENDIF.
    ELSE.
      CLEAR: LV_DATE.
      LV_DATE = SY-DATUM.

      CALL FUNCTION 'F4_DATE'
        EXPORTING
          DATE_FOR_FIRST_MONTH = LV_DATE
          DISPLAY              = LV_DISPLAY
        IMPORTING
          SELECT_DATE          = GV_DATE
        EXCEPTIONS
          OTHERS               = 4.
    ENDIF.
  ENDIF.




ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_9100 INPUT.

  CASE OK_CODE.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.

  CASE OK_CODE.
    WHEN 'CONF'.
      PERFORM SEND_EAMIL.
      CLEAR OK_CODE.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  DATA : LS_0021 LIKE ZSDT0021.
  CLEAR : OK_CODE.
  OK_CODE = SY-UCOMM.

  CLEAR GV_ZTYPE.
  CASE 'X'.
    WHEN RA_NORM.
      GV_ZTYPE = 'N'.
    WHEN RA_MDDP.
      GV_ZTYPE = 'M'.
    WHEN RA_INIT.
      GV_ZTYPE = 'I'.
    WHEN RA_MANU.
      GV_ZTYPE = 'P'.
    WHEN RA_SALES.
      GV_ZTYPE = 'S'.
  ENDCASE.

  IF RA_MANU = 'X' OR RA_SALES = 'X'.
    IF GV_DATE IS INITIAL.
      MESSAGE S000 WITH 'Input Start date' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    SELECT SINGLE END_DATE
      INTO GV_DATE2
    FROM ZSDT0020
    WHERE ZSTART = GV_DATE
      AND ZTYPE = GV_ZTYPE.
    IF GV_DATE2 IS INITIAL.
      IF SY-SUBRC NE 0.
        MESSAGE S000 WITH 'Input End date' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ELSE.
      IF GV_DATE2 <= GV_DATE.
        MESSAGE S000 WITH 'Check End date' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR GV_DATE2.
  ENDIF.

  CASE OK_CODE.
    WHEN 'BTN1'.
      CALL TRANSACTION 'ZSDC0010'.
    WHEN 'BTN2'.
      IF GV_DATE IS NOT INITIAL.
        SET PARAMETER ID 'Z01' FIELD GV_DATE.
      ENDIF.
      IF GV_ZTYPE IS NOT INITIAL.
        SET PARAMETER ID 'Z02' FIELD GV_ZTYPE.
      ENDIF.
      IF GV_DATE2 IS NOT INITIAL.
        SET PARAMETER ID 'Z03' FIELD GV_DATE2.
      ENDIF.
      CALL TRANSACTION 'ZSDR0011'.
    WHEN 'BTN3'.
      CALL TRANSACTION 'ZSDR0012'.
    WHEN 'BTN4'.
      CALL TRANSACTION 'ZSDB0040'.
    WHEN 'BTN5'.
      CALL TRANSACTION 'ZMMR0020'.
    WHEN 'BTN6'.
      IF GV_DATE IS NOT INITIAL.
        SET PARAMETER ID 'Z01' FIELD GV_DATE.
      ENDIF.
      IF GV_ZTYPE IS NOT INITIAL.
        SET PARAMETER ID 'Z02' FIELD GV_ZTYPE.
      ENDIF.
      IF GV_DATE2 IS NOT INITIAL.
        SET PARAMETER ID 'Z03' FIELD GV_DATE2.
      ENDIF.
      CALL TRANSACTION 'ZSDR0013'.
    WHEN 'BTN7'.
      IF GV_DATE IS NOT INITIAL.
        SET PARAMETER ID 'Z01' FIELD GV_DATE.
      ENDIF.
      IF GV_ZTYPE IS NOT INITIAL.
        SET PARAMETER ID 'Z02' FIELD GV_ZTYPE.
      ENDIF.
      CALL TRANSACTION 'ZSDR0014'.
    WHEN 'BTN8'.
      IF GV_DATE IS NOT INITIAL.
        SET PARAMETER ID 'Z01' FIELD GV_DATE.
      ENDIF.
      IF GV_ZTYPE IS NOT INITIAL.
        SET PARAMETER ID 'Z02' FIELD GV_ZTYPE.
      ENDIF.
      CALL TRANSACTION 'ZSDR0015'.
    WHEN 'BTN9'.
      IF GV_DATE IS NOT INITIAL.
        SET PARAMETER ID 'Z01' FIELD GV_DATE.
      ENDIF.
      IF GV_ZTYPE IS NOT INITIAL.
        SET PARAMETER ID 'Z02' FIELD GV_ZTYPE.
      ENDIF.
      CALL TRANSACTION 'ZSDI0020'.
    WHEN 'BTN10'.                   " Confirmation
      PERFORM SET_CONFIRMATION.
    WHEN 'BTN11'.                   " Send E-mail
      IF GV_DATE IS NOT INITIAL.
        CLEAR : LS_0021.
        SELECT SINGLE *
                      INTO CORRESPONDING FIELDS OF LS_0021
                      FROM ZSDT0021
                      WHERE ZSTART EQ GV_DATE
                        AND ZTYPE  EQ GV_ZTYPE.
        IF SY-SUBRC = 0.
          IF LS_0021-ZCONFIRM IS INITIAL.
            MESSAGE S000 WITH 'You can send E-mail after confirmaion' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
      CALL SCREEN 9100 STARTING AT 10 10
                        ENDING AT 130 22.
*    WHEN 'CHECK'.
*      PERFORM CHECK_STATUS_BY_DATE.
    WHEN OTHERS.
      IF GV_DATE IS INITIAL.
        CLEAR : GS_STATUS.
      ENDIF.
  ENDCASE.

  PERFORM CHECK_STATUS_BY_DATE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_SDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_HELP_SDATE2 INPUT.

  CLEAR: LV_DATE.
  LV_DATE = SY-DATUM.

  CALL FUNCTION 'F4_DATE'
    EXPORTING
      DATE_FOR_FIRST_MONTH = LV_DATE
      DISPLAY              = LV_DISPLAY
    IMPORTING
      SELECT_DATE          = GV_DATE2
    EXCEPTIONS
      OTHERS               = 4.


ENDMODULE.
