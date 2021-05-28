*&---------------------------------------------------------------------*
*& Report ZSDR0052
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDR0052 MESSAGE-ID ZSD.

INCLUDE ZSDR0052_TOP.

DATA: LS_HEADER   LIKE BAPISDH1,
      LS_HEADER_X LIKE BAPISDH1X,
      LT_ITEM     LIKE TABLE OF BAPISDITM WITH HEADER LINE,
      LT_ITEM_X   LIKE TABLE OF BAPISDITMX WITH HEADER LINE,
      LT_RETURN   LIKE TABLE OF BAPIRET2 WITH HEADER LINE.

DATA: LV_TOTAL TYPE I,
      LV_BAPI  TYPE I,
      LV_PEGG  TYPE I,
      LV_ABSTK TYPE ABSTK.

DATA: LT_RESULT   TYPE TABLE OF ZSDS0100,
      LT_PPRESULT TYPE TABLE OF ZPPT1000.

DATA: LS_RETURN TYPE BAPIRET2.

DATA: LV_MESSAGE TYPE STRING,
      LV_MSG     TYPE C LENGTH 300.

" VBEP  : ORDER QYT, CONFIRMED QTY
SELECT VBELN, POSNR,
  SUM( WMENG ) AS WMENG,
  SUM( BMENG ) AS BMENG
  INTO TABLE @DATA(LT_VBEP)
  FROM VBEP
 WHERE VBELN EQ @P_VBELN
 GROUP BY VBELN, POSNR.

" MATNR
SELECT VBELN, POSNR, MATNR
  INTO TABLE @DATA(LT_MATNR)
  FROM VBAP
  FOR ALL ENTRIES IN @LT_VBEP
  WHERE VBELN EQ @LT_VBEP-VBELN
    AND POSNR EQ @LT_VBEP-POSNR.
SORT LT_MATNR BY VBELN POSNR.

LOOP AT LT_VBEP INTO DATA(LS_VBEP).
  IF LS_VBEP-WMENG EQ LS_VBEP-BMENG. "ORDER QTY 와 CONFIRMED QTY가 같은 경우

    "HEADER
    LS_HEADER_X-UPDATEFLAG = 'U'. "U = change

    "ITEM
    READ TABLE LT_MATNR INTO DATA(LS_MATNR) WITH KEY VBELN = LS_VBEP-VBELN
                                                     POSNR = LS_VBEP-POSNR BINARY SEARCH.

    LT_ITEM-MATERIAL     = LS_MATNR-MATNR.
    LT_ITEM-ITM_NUMBER   = LS_VBEP-POSNR.
    LT_ITEM_X-ITM_NUMBER = LS_VBEP-POSNR.
    LT_ITEM-REASON_REJ   = 'CR'.
    LT_ITEM_X-REASON_REJ = 'X'.
    LT_ITEM_X-UPDATEFLAG = 'U'.

    APPEND LT_ITEM.
    APPEND LT_ITEM_X.
    CLEAR: LT_ITEM, LT_ITEM_X.
  ENDIF.
ENDLOOP.

"Quotation 문서 내 아이템 수
DESCRIBE TABLE LT_VBEP LINES LV_TOTAL.
"오더 수량과 컨펌 수량이 같은 아이템 수
DESCRIBE TABLE LT_ITEM LINES LV_BAPI.

IF LV_TOTAL EQ LV_BAPI."fully confirm 일때만 실행
  CLEAR LT_RESULT[].
  CALL FUNCTION 'ZSD_QT_CHANGE_CHECK' " pegging, confirm check fm
    EXPORTING
      I_VBELN  = P_VBELN
    TABLES
      T_RESULT = LT_RESULT.

  IF LT_RESULT[] IS NOT INITIAL. "Pegging o, Confirm o
    "Plan order end date와 QT 자재가용일 비교
    LOOP AT LT_RESULT INTO DATA(LS_RESULT).
      IF LS_RESULT-DAT00 <> LS_RESULT-DAT00_QT. " 날짜가 다르면 error
        MESSAGE S020 WITH LS_RESULT-DAT00 LS_RESULT-DAT00_QT.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDLOOP.

    "LT_RESULT의 아이템 수와 LT_ITEM의 아이템 수 비교
    DESCRIBE TABLE LT_RESULT LINES LV_PEGG.
    IF LV_BAPI = LV_PEGG. "아이템 수가 같은 경우에만 function 실행
      CALL FUNCTION 'BAPI_CUSTOMERQUOTATION_CHANGE' DESTINATION 'NONE'
        EXPORTING
          SALESDOCUMENT        = P_VBELN
          QUOTATION_HEADER_INX = LS_HEADER_X
        TABLES
          RETURN               = LT_RETURN
          QUOTATION_ITEM_IN    = LT_ITEM
          QUOTATION_ITEM_INX   = LT_ITEM_X.

      READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
      IF SY-SUBRC EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' DESTINATION 'NONE'.
        "message read
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = LT_RETURN-ID
            MSGNR               = LT_RETURN-NUMBER
            MSGV1               = LT_RETURN-MESSAGE_V1
            MSGV2               = LT_RETURN-MESSAGE_V2
            MSGV3               = LT_RETURN-MESSAGE_V3
            MSGV4               = LT_RETURN-MESSAGE_V4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = LV_MESSAGE.

        "error message
        MESSAGE S000 WITH LV_MESSAGE.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' DESTINATION 'NONE'
          EXPORTING
            WAIT = ABAP_TRUE.

        CLEAR LS_RETURN.
        CALL FUNCTION 'ZPP_PLNORD_DELETE_WITH_QT'
          EXPORTING
            I_VBELN  = P_VBELN
          IMPORTING
            E_RETURN = LS_RETURN
          TABLES
            T_DATA   = LT_PPRESULT.
        "성공 message
        MESSAGE S000 WITH 'Success!'.
      ENDIF.

      CALL FUNCTION 'RFC_CONNECTION_CLOSE'
        EXPORTING
          DESTINATION          = 'NONE'
        EXCEPTIONS
          DESTINATION_NOT_OPEN = 1
          OTHERS               = 2.
    ELSE. "아이템 수가 다른 경우
      "error message
      MESSAGE S000 WITH 'The number of items is different' .
    ENDIF.
  ELSE. "pegging, confirm 되지 않은 경우
    "error message
    MESSAGE S000 WITH 'There is no confirmed data' .
  ENDIF.
ENDIF.
CLEAR: LT_ITEM[], LT_ITEM_X[], LT_RETURN, LT_RETURN[], LV_TOTAL, LV_BAPI.
