*&---------------------------------------------------------------------*
*& Include          ZSDR0010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_STATUS_BY_DATE
*&---------------------------------------------------------------------*
FORM CHECK_STATUS_BY_DATE .

  DATA : LV_DATE TYPE CHAR10.
  DATA : LV_DATE_S TYPE CHAR10.
  DATA : LV_DATE_I TYPE CHAR10.

  CLEAR : GS_STATUS, GV_INTERCOM_FG.
  IF GV_DATE IS INITIAL.
    MESSAGE S000 WITH 'Input start date' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.

    IF RA_MANU = 'X' OR RA_SALES = 'X'.
      IF GV_DATE2 IS INITIAL.
        MESSAGE S000 WITH 'Input End date' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        IF GV_DATE2 <= GV_DATE.
          MESSAGE S000 WITH 'Check End date' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF GS_0021
                  FROM ZSDT0021
                  WHERE ZSTART EQ GV_DATE
                    AND ZTYPE  EQ GV_ZTYPE.
    IF SY-SUBRC = 0.

* Margin status
      IF GS_0021-ZMARGIN IS NOT INITIAL.
        GS_STATUS-STAT_01  = ICON_LED_GREEN.
        CLEAR : LV_DATE.
        WRITE GS_0021-ZMARGIN_DATE TO LV_DATE.
        GS_STATUS-STAT_01D = LV_DATE.
      ELSE.
        GS_STATUS-STAT_01 = ICON_LED_RED.
      ENDIF.

* HQ calculation status
      IF GS_0021-ZHQCAL IS NOT INITIAL.
        GS_STATUS-STAT_02  = ICON_LED_GREEN.
        CLEAR : LV_DATE.
        WRITE GS_0021-ZHQCAL_DATE TO LV_DATE.
        GS_STATUS-STAT_02D = LV_DATE.
      ELSE.
        GS_STATUS-STAT_02 = ICON_LED_RED.
      ENDIF.

* Intercompany calculation Info status / Sales status
      IF GS_0021-ZINTERCAL_I IS NOT INITIAL AND
         GS_0021-ZINTERCAL_S IS NOT INITIAL.

        GS_STATUS-STAT_03  = ICON_LED_GREEN.
        GV_INTERCOM_FG     = 'A'.

        IF GS_0021-ZINTERCAL_IDATE >= GS_0021-ZINTERCAL_SDATE.
          WRITE GS_0021-ZINTERCAL_IDATE TO LV_DATE.
          GS_STATUS-STAT_03D = LV_DATE.
        ELSE.
          WRITE GS_0021-ZINTERCAL_SDATE TO LV_DATE.
          GS_STATUS-STAT_03D = LV_DATE.
        ENDIF.

      ELSE.
        IF GS_0021-ZINTERCAL_I IS NOT INITIAL.
          GV_INTERCOM_FG     = 'I'.

          CLEAR : LV_DATE.
          WRITE GS_0021-ZINTERCAL_IDATE TO LV_DATE.
          GS_STATUS-STAT_03D = LV_DATE.
        ENDIF.

        IF GS_0021-ZINTERCAL_S IS NOT INITIAL.
          GV_INTERCOM_FG     = 'S'.

          CLEAR : LV_DATE.
          WRITE GS_0021-ZINTERCAL_SDATE TO LV_DATE.
          GS_STATUS-STAT_03D = LV_DATE.
        ENDIF.

        IF GS_0021-ZINTERCAL_I IS INITIAL AND
           GS_0021-ZINTERCAL_S IS INITIAL.
          GS_STATUS-STAT_03 = ICON_LED_RED.
        ELSE.
          GS_STATUS-STAT_03 = ICON_LED_YELLOW.
        ENDIF.
      ENDIF.

* I/F status
      IF GS_0021-ZINTERFACE IS NOT INITIAL.
        GS_STATUS-STAT_04  = ICON_LED_GREEN.
        CLEAR : LV_DATE.
        WRITE GS_0021-ZINTERFACE_DATE TO LV_DATE.
        GS_STATUS-STAT_04D = LV_DATE.
      ELSE.
        GS_STATUS-STAT_04 = ICON_LED_RED.
      ENDIF.

* Confirm status
      IF GS_0021-ZCONFIRM IS NOT INITIAL.
        GS_STATUS-STAT_05  = ICON_LED_GREEN.
        CLEAR : LV_DATE.
        WRITE GS_0021-ZCONFIRM_DATE TO LV_DATE.
        GS_STATUS-STAT_05D = LV_DATE.
      ELSE.
        GS_STATUS-STAT_05 = ICON_LED_YELLOW.
      ENDIF.

    ELSE.
      MESSAGE S000 WITH 'Not exist data for input date.' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CONFIRMATION
*&---------------------------------------------------------------------*
FORM SET_CONFIRMATION .

  DATA : LV_DATE TYPE CHAR10.
  DATA : LV_CHECK TYPE CHAR1.

  DATA : LS_0021 LIKE ZSDT0021.

  CLEAR : LS_0021.
  IF GV_DATE IS NOT INITIAL.

    SELECT SINGLE *
                  INTO CORRESPONDING FIELDS OF LS_0021
                  FROM ZSDT0021
                  WHERE ZSTART EQ GV_DATE
                   AND  ZTYPE  EQ GV_ZTYPE.
    IF SY-SUBRC = 0.
      IF LS_0021-ZCONFIRM IS NOT INITIAL.
        MESSAGE S000 WITH 'This data is already confirmed.'.
        EXIT.
      ENDIF.

      IF LS_0021-ZMARGIN     = '' OR
         LS_0021-ZHQCAL      = '' OR
         LS_0021-ZINTERCAL_I = '' OR
         LS_0021-ZINTERCAL_S = '' OR
         LS_0021-ZINTERFACE  = ''.
        MESSAGE S000 WITH 'There are incomplete steps.'.
        EXIT.
      ENDIF.

      PERFORM POPUP_MSG USING TEXT-P01
                              TEXT-P02
                              LV_CHECK.
      IF LV_CHECK EQ '1'.

      ENDIF.

      IF LV_CHECK = '1'.
        UPDATE ZSDT0021
               SET ZCONFIRM = 'X'
                   ZCONFIRM_DATE = SY-DATLO
               WHERE ZSTART = GV_DATE
                 AND ZTYPE  = GV_ZTYPE.
        IF SY-SUBRC = 0.
          GS_0021-ZCONFIRM = 'X'.
          GS_STATUS-STAT_05  = ICON_LED_GREEN.

          CLEAR : LV_DATE.
          WRITE SY-DATUM TO LV_DATE.
          GS_STATUS-STAT_05D = LV_DATE.
        ELSE.
          GS_STATUS-STAT_05 = ICON_LED_YELLOW.
          MESSAGE S000 WITH 'Failed to save confirmation.'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_MSG
*&---------------------------------------------------------------------*
FORM POPUP_MSG USING P_MSG1 P_MSG2 PV_CHECK.

  CLEAR PV_CHECK.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = P_MSG1
      TEXT_QUESTION  = P_MSG2
      TEXT_BUTTON_1  = 'YES'
      TEXT_BUTTON_2  = 'NO'
    IMPORTING
      ANSWER         = PV_CHECK
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEND_EAMIL
*&---------------------------------------------------------------------*
FORM SEND_EAMIL .

  DATA : UP_OBJCNT           LIKE TABLE OF  SOLI WITH HEADER LINE,
         UP_RECIVERS         LIKE TABLE OF SOOS1 WITH HEADER LINE,
         UP_OBJECT_HD_CHANGE LIKE   SOOD1.

  UP_OBJECT_HD_CHANGE-OBJLA =  SY-LANGU.
  UP_OBJECT_HD_CHANGE-OBJNAM = 'EMAIL'.
  UP_OBJECT_HD_CHANGE-OBJDES = GV_TITLE.
  UP_OBJECT_HD_CHANGE-OBJSNS = 'F'.

*메일 본문 작성
  IF GV_TEXT1 IS NOT INITIAL.
    UP_OBJCNT-LINE = GV_TEXT1.
    APPEND UP_OBJCNT.
  ENDIF.
  IF GV_TEXT2 IS NOT INITIAL.
    UP_OBJCNT-LINE = GV_TEXT2.
    APPEND UP_OBJCNT.
  ENDIF.
  IF GV_TEXT3 IS NOT INITIAL.
    UP_OBJCNT-LINE = GV_TEXT3.
    APPEND UP_OBJCNT.
  ENDIF.
  IF GV_TEXT4 IS NOT INITIAL.
    UP_OBJCNT-LINE = GV_TEXT4.
    APPEND UP_OBJCNT.
  ENDIF.
  IF GV_TEXT5 IS NOT INITIAL.
    UP_OBJCNT-LINE = GV_TEXT5.
    APPEND UP_OBJCNT.
  ENDIF.
  IF GV_TEXT6 IS NOT INITIAL.
    UP_OBJCNT-LINE = GV_TEXT6.
    APPEND UP_OBJCNT.
  ENDIF.
  IF GV_TEXT7 IS NOT INITIAL.
    UP_OBJCNT-LINE = GV_TEXT7.
    APPEND UP_OBJCNT.
  ENDIF.

  SELECT ZEMAIL
  INTO TABLE @DATA(LT_0030)
  FROM ZSDT0030.

  LOOP AT LT_0030 INTO DATA(LS_0030).
    UP_RECIVERS-RECEXTNAM = LS_0030-ZEMAIL.
    UP_RECIVERS-RECESC = 'U'.
    UP_RECIVERS-RECNAM = 'U-'.
    APPEND UP_RECIVERS.
  ENDLOOP.

  CALL FUNCTION 'SO_OBJECT_SEND'
    EXPORTING
      OBJECT_HD_CHANGE           = UP_OBJECT_HD_CHANGE
      OBJECT_TYPE                = 'RAW'
      OUTBOX_FLAG                = 'X'
    TABLES
      OBJCONT                    = UP_OBJCNT
      RECEIVERS                  = UP_RECIVERS
    EXCEPTIONS
      ACTIVE_USER_NOT_EXIST      = 1
      COMMUNICATION_FAILURE      = 2
      COMPONENT_NOT_AVAILABLE    = 3
      FOLDER_NOT_EXIST           = 4
      FOLDER_NO_AUTHORIZATION    = 5
      FORWARDER_NOT_EXIST        = 6
      NOTE_NOT_EXIST             = 7
      OBJECT_NOT_EXIST           = 8
      OBJECT_NOT_SENT            = 9
      OBJECT_NO_AUTHORIZATION    = 10
      OBJECT_TYPE_NOT_EXIST      = 11
      OPERATION_NO_AUTHORIZATION = 12
      OWNER_NOT_EXIST            = 13
      PARAMETER_ERROR            = 14
      SUBSTITUTE_NOT_ACTIVE      = 15
      SUBSTITUTE_NOT_DEFINED     = 16
      SYSTEM_FAILURE             = 17
      TOO_MUCH_RECEIVERS         = 18
      USER_NOT_EXIST             = 19
      ORIGINATOR_NOT_EXIST       = 20
      X_ERROR                    = 21
      OTHERS                     = 22.

  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF SY-SUBRC = 0.
    MESSAGE I000 WITH 'E-mail has been sent.'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_9100 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'S9000'.
  SET TITLEBAR '9000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CONTROL_BTN OUTPUT
*&---------------------------------------------------------------------*
MODULE CONTROL_BTNPBO OUTPUT.

  IF RA_MANU NE 'X' AND RA_SALES NE 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'Z03'.
        SCREEN-ACTIVE  = '0'.
        SCREEN-INPUT  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'Z03'.
        SCREEN-ACTIVE  = '1'.
        SCREEN-INPUT  = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF RA_MANU NE 'X' AND RA_SALES NE 'X'.
  ELSE.

    SELECT SINGLE END_DATE
    INTO GV_DATE2
    FROM ZSDT0020
    WHERE ZSTART = GV_DATE
    AND ZTYPE = GV_ZTYPE.

    IF SY-SUBRC = 0.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 EQ 'Z03'.
          SCREEN-ACTIVE  = '1'.
          SCREEN-INPUT  = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 EQ 'Z03'.
          SCREEN-ACTIVE  = '1'.
          SCREEN-INPUT  = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMODULE.
