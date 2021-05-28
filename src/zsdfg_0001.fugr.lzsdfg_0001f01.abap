*----------------------------------------------------------------------*
***INCLUDE LZSDFG_0001F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form DELIVERY
*&---------------------------------------------------------------------*
FORM DELIVERY .

  CLEAR : GT_ITEMS,        GS_ITEMS,
          GT_ET_DLV_ITEMS, GS_ET_DLV_ITEMS,
          GT_ET_MESSAGES,  GS_ET_MESSAGES.

  DATA : LV_VSTEL    TYPE VSTEL,
         LV_MSG(255) TYPE C.

  CLEAR LV_VSTEL.
  LOOP AT GT_DATA WHERE VBELN = GT_HEADER-VBELN
*                    AND CONNO = GT_HEADER-CONNO
                    AND BLDOC = GT_HEADER-BLDOC
                    AND INVNO_EX = GT_HEADER-INVNO_EX.
    CLEAR GT_VBAP.
    READ TABLE GT_VBAP WITH KEY VBELN = GT_DATA-VBELN
                                POSNR = GT_DATA-POSNR BINARY SEARCH.
    GS_ITEMS-RFBEL = GT_DATA-VBELN.
    GS_ITEMS-RFPOS = GT_DATA-POSNR.
    GS_ITEMS-LFIMG = GT_DATA-LFIMG.
    GS_ITEMS-VRKME = GT_VBAP-MEINS.
    APPEND GS_ITEMS TO GT_ITEMS. CLEAR GS_ITEMS.

*SET일경우 상위품번 필요
    IF GT_DATA-POSNR+5(1) NE 0.
      GS_ITEMS-RFBEL = GT_DATA-VBELN.
      GS_ITEMS-RFPOS = GT_VBAP-UEPOS.
      GS_ITEMS-LFIMG = GT_DATA-LFIMG.
      GS_ITEMS-VRKME = GT_VBAP-MEINS.
      APPEND GS_ITEMS TO GT_ITEMS. CLEAR GS_ITEMS.
    ENDIF.

    CLEAR LV_VSTEL.
    LV_VSTEL = GT_VBAP-VSTEL.
  ENDLOOP.

  SORT GT_ITEMS BY RFBEL RFPOS.
  DELETE ADJACENT DUPLICATES FROM GT_ITEMS COMPARING RFBEL RFPOS.

  CALL FUNCTION 'SHP_DELIVERY_CREATE_FROM_SLS'
    EXPORTING
      IF_VSTEL     = LV_VSTEL
      IF_LEDAT     = '29991231'                "Delivery Creation Date
      IT_SLS_ITEMS = GT_ITEMS                  "Delivery Items with Reference to Sales Order
    IMPORTING
      ET_DLV_ITEMS = GT_ET_DLV_ITEMS           "Delivery Items
      ET_MESSAGES  = GT_ET_MESSAGES.           "Error Log for Collective Processing

  LOOP AT GT_ET_MESSAGES INTO GS_ET_MESSAGES WHERE MSGTY = 'E' OR MSGTY = 'A'.
    EXIT.
  ENDLOOP.

  IF SY-SUBRC = 0.
    CLEAR LV_MSG.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = GS_ET_MESSAGES-MSGID
        MSGNR               = GS_ET_MESSAGES-MSGNO
        MSGV1               = GS_ET_MESSAGES-MSGV1
        MSGV2               = GS_ET_MESSAGES-MSGV2
        MSGV3               = GS_ET_MESSAGES-MSGV3
        MSGV4               = GS_ET_MESSAGES-MSGV4
      IMPORTING
        MESSAGE_TEXT_OUTPUT = LV_MSG.

    GT_HEADER-MESSAGE = LV_MSG.
    MODIFY GT_HEADER TRANSPORTING MESSAGE.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    READ TABLE GT_ET_DLV_ITEMS INTO GS_ET_DLV_ITEMS INDEX 1.
    GT_HEADER-VBELN_VL = GS_ET_DLV_ITEMS-VBELN.
    MODIFY GT_HEADER TRANSPORTING VBELN_VL.
    WAIT UP TO 1 SECONDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GOODS_ISSUE
*&---------------------------------------------------------------------*
FORM GOODS_ISSUE .

  _CLEAR : GT_BDCDATA, GT_BDCMSG.

  DATA: LV_DATE(10),
        LV_MSG(200),
        LV_ERZET LIKE SY-UZEIT.

  CLEAR LV_ERZET.
  LV_ERZET = SY-UZEIT.

  CLEAR GT_DATA.
  READ TABLE GT_DATA WITH KEY VBELN = GT_HEADER-VBELN
*                              CONNO = GT_HEADER-CONNO
                              BLDOC = GT_HEADER-BLDOC
                              INVNO_EX = GT_HEADER-INVNO_EX
                              BINARY SEARCH.
* 사용자 포맷에 맞게 날짜 변경
  CLEAR LV_DATE.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      DATE_INTERNAL = GT_DATA-WADAT_IST
    IMPORTING
      DATE_EXTERNAL = LV_DATE.

  PERFORM BDCDATA_SET USING :
        'X'  'SAPMV50A'        '0101',
        ' '  'BDC_OKCODE'      '/00',
        ' '  'LIKP-VBELN'      GT_HEADER-VBELN_VL, "DELIVERY.
        'X'  'SAPMV50A'        '0200',
        ' '  'BDC_OKCODE'      '=WABU',
        ' '  'LIKP-WADAT_IST'  LV_DATE,            "DELIVERYDATE,
        ' '  'LIKP-BLDAT'      LV_DATE.            "DELIVERYDATE.

** CALL TRANSACTION
  CLEAR : GT_BDCMSG, GT_BDCMSG[].
  CALL TRANSACTION 'VL02'
             USING GT_BDCDATA
             OPTIONS FROM GS_CTU_PARAMS
             MESSAGES INTO GT_BDCMSG.

  CLEAR GT_BDCMSG.
  READ TABLE GT_BDCMSG WITH KEY MSGTYP = 'S' MSGNR = '311'.
  IF SY-SUBRC EQ 0.
    DO GV_TIMES TIMES.
      WAIT UP TO GV_SEC SECONDS.
      SELECT VBELN,
             ERDAT,
             ERZET
      INTO TABLE @DATA(LT_VBFA)
        FROM VBFA
       WHERE VBELV   EQ @GT_HEADER-VBELN_VL    "Delivery.
         AND POSNV   EQ @GT_HEADER-POSNR
         AND VBTYP_N EQ 'R'
         AND ERDAT   EQ @SY-DATUM
         AND ERZET   GE @LV_ERZET.

      SORT LT_VBFA BY ERDAT DESCENDING ERZET DESCENDING.
      READ TABLE LT_VBFA INTO DATA(LS_VBFA) INDEX 1.
      IF SY-SUBRC = 0.
        GT_HEADER-MAT_DOC = LS_VBFA-VBELN.
        MODIFY GT_HEADER TRANSPORTING MAT_DOC.
        EXIT.
      ENDIF.
    ENDDO.

    IF GT_HEADER-MAT_DOC IS INITIAL.
      GT_HEADER-MESSAGE = 'GOODS ISSUE FAILED!!'.
    ENDIF.

  ELSE.
***  오류 발생시.
    LOOP AT GT_BDCMSG WHERE MSGTYP = 'E' OR MSGTYP = 'A'.
      EXIT.
    ENDLOOP.
    CLEAR LV_MSG.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = GT_BDCMSG-MSGID
        MSGNR               = GT_BDCMSG-MSGNR
        MSGV1               = GT_BDCMSG-MSGV1
        MSGV2               = GT_BDCMSG-MSGV2
        MSGV3               = GT_BDCMSG-MSGV3
        MSGV4               = GT_BDCMSG-MSGV4
      IMPORTING
        MESSAGE_TEXT_OUTPUT = LV_MSG.
    GT_HEADER-MESSAGE = LV_MSG.
    MODIFY GT_HEADER TRANSPORTING MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BILLING
*&---------------------------------------------------------------------*
FORM BILLING .

  DATA : LV_MSG(200),
         LV_BELNR LIKE VBRK-BELNR.

  _CLEAR : GT_BDCDATA, GT_BDCMSG.

  PERFORM BDCDATA_SET USING :
        'X'  'SAPMV60A'        '0102',
        ' '  'BDC_OKCODE'      '=SICH',
        ' '  'KOMFK-VBELN(01)' GT_HEADER-VBELN_VL. "DELIVERY.

** CALL TRANSACTION
  CLEAR : GT_BDCMSG, GT_BDCMSG[].
  CALL TRANSACTION 'VF01'
             USING GT_BDCDATA
             OPTIONS FROM GS_CTU_PARAMS
             MESSAGES INTO GT_BDCMSG.

  CLEAR GT_BDCMSG.
  READ TABLE GT_BDCMSG WITH KEY MSGTYP = 'S' MSGID = 'VF' MSGNR = '311'.
  IF SY-SUBRC = 0.
    GT_HEADER-VBELN_VF = GT_BDCMSG-MSGV1.
    PERFORM ALPHA_INPUT USING GT_HEADER-VBELN_VF.
    MODIFY GT_HEADER TRANSPORTING VBELN_VF.

    IF GT_HEADER-INVNO_EX IS NOT INITIAL.
      DO GV_TIMES TIMES.
        WAIT UP TO GV_SEC SECONDS.
        SELECT SINGLE BELNR INTO LV_BELNR
          FROM VBRK
         WHERE VBELN   EQ  GT_HEADER-VBELN_VF.
        IF SY-SUBRC = 0.
          UPDATE VBRK SET ZZKOINV = GT_HEADER-INVNO_EX
          WHERE VBELN = GT_HEADER-VBELN_VF.

          UPDATE YKTXT0100 SET EXDOCUM = GT_HEADER-INVNO_EX
          WHERE BELNR = LV_BELNR
            AND GJAHR = SY-DATUM(4).
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ELSE.
***  오류 발생시.
    CLEAR LV_MSG.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = GT_BDCMSG-MSGID
        MSGNR               = GT_BDCMSG-MSGNR
        MSGV1               = GT_BDCMSG-MSGV1
        MSGV2               = GT_BDCMSG-MSGV2
        MSGV3               = GT_BDCMSG-MSGV3
        MSGV4               = GT_BDCMSG-MSGV4
      IMPORTING
        MESSAGE_TEXT_OUTPUT = LV_MSG.
    GT_HEADER-MESSAGE = LV_MSG.
    MODIFY GT_HEADER TRANSPORTING MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PREPARE_DATA
*&---------------------------------------------------------------------*
FORM PREPARE_DATA .
** BDC MODE
  CLEAR GS_CTU_PARAMS.
  GS_CTU_PARAMS-DISMODE  = GV_MODE.
  GS_CTU_PARAMS-UPDMODE  = 'S'.
  GS_CTU_PARAMS-RACOMMIT = 'X'.

  _CLEAR GT_HEADER.
  LOOP AT GT_DATA.
    MOVE-CORRESPONDING GT_DATA TO GT_HEADER.
    APPEND GT_HEADER. CLEAR GT_HEADER.
  ENDLOOP.

*  SORT GT_DATA BY VBELN CONNO BLDOC.
*  SORT GT_HEADER BY VBELN CONNO BLDOC.
*  DELETE ADJACENT DUPLICATES FROM GT_HEADER COMPARING VBELN CONNO BLDOC.

  SORT GT_DATA BY VBELN BLDOC INVNO_EX.
  SORT GT_HEADER BY VBELN BLDOC INVNO_EX.
  DELETE ADJACENT DUPLICATES FROM GT_HEADER COMPARING VBELN BLDOC INVNO_EX.

  IF GT_DATA[] IS NOT INITIAL.
    "UNIT, SHIPPING POINT
    _CLEAR : GT_VBAP.
    SELECT VBELN POSNR MEINS VSTEL UEPOS
    INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
    FROM VBAP
      FOR ALL ENTRIES IN GT_DATA
    WHERE VBELN = GT_DATA-VBELN
      AND POSNR = GT_DATA-POSNR.

    SORT GT_VBAP BY VBELN POSNR.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDCDATA_SET
*&---------------------------------------------------------------------*
FORM BDCDATA_SET  USING P_START P_OBJECT P_VALUE.

  CLEAR GT_BDCDATA.

  IF P_START = 'X'.
    GT_BDCDATA-DYNBEGIN = P_START.
    GT_BDCDATA-PROGRAM = P_OBJECT.
    GT_BDCDATA-DYNPRO = P_VALUE.
  ELSE.
    GT_BDCDATA-FNAM = P_OBJECT.
    GT_BDCDATA-FVAL = P_VALUE.
  ENDIF.

  APPEND GT_BDCDATA.
  CLEAR GT_BDCDATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL_DELIVERY
*&---------------------------------------------------------------------*
FORM CANCEL_DELIVERY .

  DATA:LV_MSG(200).

  _CLEAR : GT_BDCDATA, GT_BDCMSG.

  PERFORM BDCDATA_SET USING :
              'X'  'SAPMV50A'           '4004',
              ' '  'BDC_OKCODE'         '/00',
              ' '  'LIKP-VBELN'        GT_DATA_CAN-VBELN_VL,

              'X'  'SAPMV50A'           '1000',
              ' '  'BDC_OKCODE'         '/ELOES_T'.  "삭제(Delete)아이콘.

** CALL TRANSACTION
  CLEAR : GT_BDCMSG, GT_BDCMSG[].
  CALL TRANSACTION 'VL02N'
             USING GT_BDCDATA
             OPTIONS FROM GS_CTU_PARAMS
             MESSAGES INTO GT_BDCMSG.

  CLEAR GT_BDCMSG.
  READ TABLE GT_BDCMSG WITH KEY MSGTYP = 'S' MSGID = 'VL' MSGNR = '310'.
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    GT_DATA_CAN-TYPE    = 'S'.
    MODIFY GT_DATA_CAN TRANSPORTING TYPE.
  ELSE.
***  오류 발생시.
    CLEAR LV_MSG.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = GT_BDCMSG-MSGID
        MSGNR               = GT_BDCMSG-MSGNR
        MSGV1               = GT_BDCMSG-MSGV1
        MSGV2               = GT_BDCMSG-MSGV2
        MSGV3               = GT_BDCMSG-MSGV3
        MSGV4               = GT_BDCMSG-MSGV4
      IMPORTING
        MESSAGE_TEXT_OUTPUT = LV_MSG.
    GT_DATA_CAN-MESSAGE = LV_MSG.
    GT_DATA_CAN-TYPE    = 'E'.
    MODIFY GT_DATA_CAN TRANSPORTING MESSAGE TYPE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form PREPARE_CANC_DATA
*&---------------------------------------------------------------------*
FORM PREPARE_CANC_DATA .

  _CLEAR GT_VBFA.
**Define S/O Status
  SELECT RUUID
         VBELV
         VBELN
         VBTYP_N
         ERDAT
         ERZET
  INTO CORRESPONDING FIELDS OF TABLE GT_VBFA
   FROM VBFA
  FOR ALL ENTRIES IN GT_DATA_CAN
  WHERE VBELV = GT_DATA_CAN-VBELN_VL
    AND VBTYP_V = 'J'.





*  _CLEAR GT_MSEG_KEY.
*  LOOP AT GT_VBFA WHERE VBTYP_N = 'R'.
*    GT_MSEG_KEY-VBELN_VL = GT_VBFA-VBELV.
*    GT_MSEG_KEY-MBLNR = GT_VBFA-VBELN.
*    GT_MSEG_KEY-ERDAT = GT_VBFA-ERDAT.
*    GT_MSEG_KEY-ERZET = GT_VBFA-ERZET.
*    COLLECT GT_MSEG_KEY. CLEAR GT_MSEG_KEY.
*  ENDLOOP.
*
*  SORT GT_MSEG_KEY BY MBLNR DESCENDING ERDAT DESCENDING ERZET DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM GT_MSEG_KEY COMPARING VBELN_VL.
*
*  IF GT_MSEG_KEY[] IS NOT INITIAL.
**Posting Date in the Material Document
*    _CLEAR GT_MSEG.
*    SELECT MBLNR BUDAT_MKPF
*    INTO CORRESPONDING FIELDS OF TABLE GT_MSEG
*    FROM MSEG
*    FOR ALL ENTRIES IN GT_MSEG_KEY
*    WHERE MBLNR = GT_MSEG_KEY-MBLNR.
*
*    SORT GT_MSEG BY MBLNR.
*
*    LOOP AT GT_MSEG_KEY.
*      CLEAR GT_MSEG.
*      READ TABLE GT_MSEG WITH KEY MBLNR = GT_MSEG_KEY-MBLNR BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        GT_MSEG_KEY-BUDAT_MKPF = GT_MSEG-BUDAT_MKPF.
*        MODIFY GT_MSEG_KEY TRANSPORTING BUDAT_MKPF.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  SORT GT_VBFA BY VBELV ASCENDING ERDAT DESCENDING ERZET DESCENDING.
  DELETE ADJACENT DUPLICATES FROM GT_VBFA COMPARING VBELV.
  SORT GT_VBFA BY VBELV.

** BDC MODE
  CLEAR GS_CTU_PARAMS.
  GS_CTU_PARAMS-DISMODE  = GV_MODE.
  GS_CTU_PARAMS-UPDMODE  = 'S'.
  GS_CTU_PARAMS-RACOMMIT = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL_BILLING
*&---------------------------------------------------------------------*
FORM CANCEL_BILLING .

  DATA:LV_MSG(220).
  _CLEAR : GT_BDCDATA, GT_BDCMSG.

  PERFORM BDCDATA_SET  USING:
              'X'  'SAPMV60A'           '0102',
              ' '  'BDC_OKCODE'         '=SICH',
              ' '  'KOMFK-VBELN(01)'    GT_VBFA-VBELN.

** CALL TRANSACTION
  CLEAR : GT_BDCMSG, GT_BDCMSG[].
  CALL TRANSACTION 'VF11'
             USING GT_BDCDATA
             OPTIONS FROM GS_CTU_PARAMS
             MESSAGES INTO GT_BDCMSG.

  CLEAR GT_BDCMSG.
  READ TABLE GT_BDCMSG WITH KEY MSGTYP = 'S' MSGID = 'VF' MSGNR = '311'.
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    READ TABLE GT_BDCMSG INDEX 2.
    GT_DATA_CAN-VBELN_VF = GT_VBFA-VBELN.
    GT_DATA_CAN-VBELN_VF_CAN = GT_BDCMSG-MSGV1.
    PERFORM ALPHA_INPUT USING GT_DATA_CAN-VBELN_VF_CAN.
    MODIFY GT_DATA_CAN TRANSPORTING VBELN_VF VBELN_VF_CAN.

    PERFORM REVERSE_GI.

  ELSE.
***  오류 발생시.
    CLEAR LV_MSG.
    LOOP AT GT_BDCMSG.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = GT_BDCMSG-MSGID
          MSGNR               = GT_BDCMSG-MSGNR
          MSGV1               = GT_BDCMSG-MSGV1
          MSGV2               = GT_BDCMSG-MSGV2
          MSGV3               = GT_BDCMSG-MSGV3
          MSGV4               = GT_BDCMSG-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = LV_MSG.
      IF LV_MSG IS INITIAL.
      ELSE.
        LV_MSG = LV_MSG && '/' && LV_MSG.
      ENDIF.
    ENDLOOP.
    GT_DATA_CAN-MESSAGE = 'Cancel billing failed!' && '/' && LV_MSG.
    GT_DATA_CAN-TYPE    = 'E'.
    MODIFY GT_DATA_CAN TRANSPORTING MESSAGE TYPE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REVERSE_GI
*&---------------------------------------------------------------------*
FORM REVERSE_GI .

  DATA : LT_MESG  LIKE TABLE OF MESG WITH HEADER LINE,
         LV_VBELN LIKE LIKP-VBELN,
         LV_BUDAT LIKE SY-DATLO,
         LS_MKPF  LIKE EMKPF,
         LV_MBLNR TYPE MBLNR.

*  CLEAR GT_MSEG_KEY.
*  READ TABLE GT_MSEG_KEY WITH KEY VBELN_VL = GT_DATA_CAN-VBELN_VL BINARY SEARCH.
*
*  CLEAR : LV_VBELN, LV_BUDAT, LS_MKPF.
*  LV_VBELN = GT_DATA_CAN-VBELN_VL.
*  LV_BUDAT = GT_MSEG_KEY-BUDAT_MKPF.


  CLEAR LV_BUDAT.
  SELECT SINGLE WADAT_IST INTO LV_BUDAT
  FROM LIKP
  WHERE VBELN = GT_DATA_CAN-VBELN_VL.

  LV_VBELN = GT_DATA_CAN-VBELN_VL.
  IF LV_BUDAT IS NOT INITIAL.

    "출고 취소(Cancel G/I) 프로그램(ZSDB0051) 호출하여 처리함.
    SUBMIT ZSDB0051      WITH P_VBELN EQ LV_VBELN
                         WITH P_BUDAT EQ LV_BUDAT
                         AND RETURN.

    CLEAR : LV_MBLNR.
    GET PARAMETER ID 'ZSDGICAC' FIELD LV_MBLNR.
  ENDIF.

  IF LV_MBLNR IS NOT INITIAL.
    GT_DATA_CAN-MAT_DOC_CAN = LV_MBLNR.
    MODIFY GT_DATA_CAN TRANSPORTING MAT_DOC_CAN.
*          PERFORM CANCEL_DELIVERY.
    PERFORM CANCEL_DELIVERY_BAPI.
  ELSE.
    GT_DATA_CAN-TYPE = 'E'.
    GT_DATA_CAN-MESSAGE = 'Error_reverse_goods_issue'.
    MODIFY GT_DATA_CAN TRANSPORTING TYPE MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL_BILLING_BAPI
*&---------------------------------------------------------------------*
FORM CANCEL_BILLING_BAPI .

  DATA: LT_RETURN    LIKE BAPIRETURN1 OCCURS 0 WITH HEADER LINE,
        LT_SUCCESS   LIKE BAPIVBRKSUCCESS OCCURS 0 WITH HEADER LINE,
        LV_VBELN_CAN LIKE LT_SUCCESS-BILL_DOC.

  _CLEAR : LT_RETURN, LT_SUCCESS.

  SELECT SINGLE VBELN,
                FKDAT
    INTO @DATA(LS_DATA)
    FROM VBRK
   WHERE VBELN EQ @GT_VBFA-VBELN.

  IF SY-SUBRC = 0.

**   프로그램(ZSDB0052) 호출하여 처리함.
*    SUBMIT ZSDB0052      WITH P_VBELN EQ LS_DATA-VBELN
*                         WITH P_BUDAT EQ LS_DATA-FKDAT
*                         AND RETURN.
*
*    CLEAR : LV_VBELN_CAN.
*    GET PARAMETER ID 'ZSDBILLCAC' FIELD LV_VBELN_CAN.
*
*    IF LV_VBELN_CAN IS NOT INITIAL.
*      GT_DATA_CAN-VBELN_VF_CAN = LT_SUCCESS-BILL_DOC.
*      PERFORM REVERSE_GI.
*    ENDIF.

    CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
      EXPORTING
        BILLINGDOCUMENT = LS_DATA-VBELN
        BILLINGDATE     = LS_DATA-FKDAT
      TABLES
        RETURN          = LT_RETURN
        SUCCESS         = LT_SUCCESS.

    READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      GT_DATA_CAN-MESSAGE = LT_RETURN-MESSAGE.
      GT_DATA_CAN-TYPE    = 'E'.
      MODIFY GT_DATA_CAN TRANSPORTING MESSAGE TYPE.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      READ TABLE LT_SUCCESS INDEX 1.
      IF SY-SUBRC = 0.
        GT_DATA_CAN-VBELN_VF = GT_VBFA-VBELN.
        GT_DATA_CAN-VBELN_VF_CAN = LT_SUCCESS-BILL_DOC.
        PERFORM REVERSE_GI.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL_DELIVERY_BAPI
*&---------------------------------------------------------------------*
FORM CANCEL_DELIVERY_BAPI .

  DATA: LS_HEADER_DATA    TYPE BAPIOBDLVHDRCHG,
        LS_HEADER_CONTROL TYPE BAPIOBDLVHDRCTRLCHG.

  DATA: LT_RETURN TYPE STANDARD TABLE OF BAPIRET2   WITH HEADER LINE.

  CLEAR: LS_HEADER_DATA, LS_HEADER_CONTROL.
  _CLEAR LT_RETURN.

  LS_HEADER_DATA-DELIV_NUMB    = GT_DATA_CAN-VBELN_VL.
  LS_HEADER_CONTROL-DELIV_NUMB = GT_DATA_CAN-VBELN_VL.
  LS_HEADER_CONTROL-DLV_DEL    = 'D'.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      HEADER_DATA    = LS_HEADER_DATA
      HEADER_CONTROL = LS_HEADER_CONTROL
      DELIVERY       = GT_DATA_CAN-VBELN_VL
    TABLES
      RETURN         = LT_RETURN.

  READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    GT_DATA_CAN-MESSAGE = LT_RETURN-MESSAGE.
    GT_DATA_CAN-TYPE    = 'E'.
    MODIFY GT_DATA_CAN TRANSPORTING MESSAGE TYPE.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    GT_DATA_CAN-TYPE    = 'S'.
    MODIFY GT_DATA_CAN TRANSPORTING TYPE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_INPUT
*&---------------------------------------------------------------------*
FORM ALPHA_INPUT  USING    PV_VALUE.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = PV_VALUE
    IMPORTING
      OUTPUT = PV_VALUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_DELIVERY
*&---------------------------------------------------------------------*
FORM UPDATE_DELIVERY .

  DATA: LV_CHAR  TYPE CHAR15,
        LV_MENGE TYPE MENGE_D.

  " BAPI - Deliver Order Change
  DATA: LS_HEADER_DATA    LIKE BAPIOBDLVHDRCHG,                                " Delivery header
        LS_HEADER_CONTROL LIKE BAPIOBDLVHDRCTRLCHG,                            " Delivery header control
        LS_TECHN_CONTROL  LIKE BAPIDLVCONTROL,                                 " TECHN_CONTROL
        LT_ITEM_DATA      LIKE BAPIOBDLVITEMCHG OCCURS 0 WITH HEADER LINE,     " ITEM_DATA delivery item
        LT_ITEM_CONTROL   LIKE BAPIOBDLVITEMCTRLCHG OCCURS 0 WITH HEADER LINE, " ITEM_CONTROL
        LT_RETURN         TYPE STANDARD TABLE OF BAPIRET2   WITH HEADER LINE.

  CLEAR : LS_HEADER_DATA,
          LS_HEADER_CONTROL,
          LS_TECHN_CONTROL ,
          LT_ITEM_DATA, LT_ITEM_DATA[],
          LT_ITEM_CONTROL, LT_ITEM_CONTROL[],
          LT_RETURN, LT_RETURN[].

*Set Delivery Header data
  LS_HEADER_DATA-DELIV_NUMB    = GT_HEADER-VBELN_VL.
  LS_HEADER_CONTROL-DELIV_NUMB = GT_HEADER-VBELN_VL.

*  LS_HEADER_DATA-INCOTERMS3L   = GT_HEADER-INVNO_EX.
*  LS_HEADER_CONTROL-INCO3_L_FLG = 'X'.
  LS_TECHN_CONTROL-UPD_IND     = 'U'.

  LOOP AT GT_DATA WHERE VBELN = GT_HEADER-VBELN
*                    AND CONNO = GT_HEADER-CONNO
                    AND BLDOC = GT_HEADER-BLDOC
                    AND INVNO_EX = GT_HEADER-INVNO_EX.

    LT_ITEM_DATA-DELIV_NUMB      = GT_HEADER-VBELN_VL.
    LT_ITEM_DATA-DELIV_ITEM      = GT_DATA-POSNR.

    CLEAR: LV_CHAR, LV_MENGE.
    LV_CHAR = GT_DATA-LFIMG.
    CALL FUNCTION 'CHECK_AND_CONVERT_NUMERICS'
      EXPORTING
        DMZEI = '.'
        DTYPE = 'QUAN'
        EFELD = LV_CHAR
      IMPORTING
        IFELD = LV_MENGE.

    LT_ITEM_DATA-DLV_QTY         = LV_MENGE.
    LT_ITEM_DATA-FACT_UNIT_NOM   = '1'.
    LT_ITEM_DATA-FACT_UNIT_DENOM = '1'.
    LT_ITEM_DATA-VAL_TYPE        = GT_DATA-BWTAR.
    APPEND LT_ITEM_DATA.

    LT_ITEM_CONTROL-DELIV_NUMB   = GT_HEADER-VBELN_VL.
    LT_ITEM_CONTROL-DELIV_ITEM   = GT_DATA-POSNR.
    LT_ITEM_CONTROL-CHG_DELQTY   = ABAP_ON.
    APPEND LT_ITEM_CONTROL.
  ENDLOOP.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      HEADER_DATA    = LS_HEADER_DATA
      HEADER_CONTROL = LS_HEADER_CONTROL
      DELIVERY       = GT_HEADER-VBELN_VL
      TECHN_CONTROL  = LS_TECHN_CONTROL
    TABLES
      ITEM_DATA      = LT_ITEM_DATA
      ITEM_CONTROL   = LT_ITEM_CONTROL
      RETURN         = LT_RETURN.

  READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    UPDATE LIKP SET INCO3_L = GT_HEADER-INVNO_EX
    WHERE VBELN = GT_HEADER-VBELN_VL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form OPEN_JOB USING oj_jobname
*&---------------------------------------------------------------------*
FORM OPEN_JOB USING OJ_JOBNAME P_TEST.
  CLEAR : JOB_ACTIV.

* rest job counter
  CLEAR JOBCOUNT.
  IF P_TEST = SPACE.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = OJ_JOBNAME
        JOBGROUP         = JOBGROUP
      IMPORTING
        JOBCOUNT         = JOBCOUNT
      EXCEPTIONS
        CANT_CREATE_JOB  = 01
        INVALID_JOB_DATA = 02
        JOBNAME_MISSING  = 03
        OTHERS           = 04.
    IF NOT SY-SUBRC IS INITIAL.

      EXIT.
    ENDIF.

    JOB_ACTIV = 'X'.
  ENDIF.
ENDFORM.                    "OPEN_JOB
*&---------------------------------------------------------------------*
*& Form submit_step
*&---------------------------------------------------------------------*
FORM SUBMIT_STEP USING P_DATE TYPE D
                       P_VBELN TYPE VBELN_VL
                       P_VKORG TYPE VKORG.
  DATA: LT_SELTAB TYPE TABLE OF RSPARAMS,
        LS_SELTAB TYPE RSPARAMS.

  LT_SELTAB = VALUE #( ( SELNAME = 'S_VBELN'
                         KIND = 'S'
                         SIGN = 'I'
                         OPTION = 'EQ'
                         LOW = P_VBELN )

                       ( SELNAME = 'P_KUNNR'
                         KIND = 'S'
                         SIGN = 'I'
                         OPTION = 'EQ'
                         LOW = '1000' ) ).


  CHECK JOB_ACTIV = 'X'.
  SUBMIT SDBILLDL VIA JOB JOBNAME
                     NUMBER JOBCOUNT
                    WITH P_VKORG  = P_VKORG
                    WITH P_FKDAT = '00000000'
                    WITH P_FKDAB = P_DATE
                    WITH P_SAMML = 'X'
                    WITH SELECTION-TABLE LT_SELTAB
                    AND RETURN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form job_clise
*&---------------------------------------------------------------------*
FORM JOB_CLOSE USING CJ_JOBNAME
                     P_DATE TYPE D
                     P_TIME TYPE T.
  IF JOB_ACTIV = 'X'.
    IF P_DATE IS NOT INITIAL AND
       P_DATE NE '00000000'.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          JOBNAME              = CJ_JOBNAME
          JOBCOUNT             = JOBCOUNT
          SDLSTRTDT            = P_DATE
          SDLSTRTTM            = P_TIME
        EXCEPTIONS
          CANT_START_IMMEDIATE = 01
          INVALID_STARTDATE    = 02
          JOBNAME_MISSING      = 03
          JOB_CLOSE_FAILED     = 04
          JOB_NOSTEPS          = 05
          JOB_NOTEX            = 06
          LOCK_FAILED          = 07
          OTHERS               = 08.
    ELSE.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          JOBNAME              = CJ_JOBNAME
          JOBCOUNT             = JOBCOUNT
          STRTIMMED            = 'X'
        EXCEPTIONS
          CANT_START_IMMEDIATE = 01
          INVALID_STARTDATE    = 02
          JOBNAME_MISSING      = 03
          JOB_CLOSE_FAILED     = 04
          JOB_NOSTEPS          = 05
          JOB_NOTEX            = 06
          LOCK_FAILED          = 07
          OTHERS               = 08.
    ENDIF.
    JOB_CLOSE = SPACE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUBMIT_POST_DELIVERY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_VBELN
*&      --> I_BUDAT
*&      --> I_CANCEL
*&---------------------------------------------------------------------*
FORM SUBMIT_POST_DELIVERY  USING    P_VBELN TYPE VBELN_VL
                                    P_BUDAT TYPE BUDAT
                                    P_CANCEL.
  CHECK JOB_ACTIV = 'X'.

  P_VBELN = |{ P_VBELN ALPHA = IN }|.
  SUBMIT ZSDR0051 VIA JOB JOBNAME_GR
                   NUMBER JOBCOUNT
                   WITH P_VBELN = P_VBELN
                   WITH P_BUDAT = P_BUDAT
                   WITH P_CAN = P_CANCEL AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form JOB_OPEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_JOBNAME
*&---------------------------------------------------------------------*
FORM JOB_OPEN  USING    P_JOBNAME P_JOBC.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      JOBNAME          = P_JOBNAME
    IMPORTING
      JOBCOUNT         = P_JOBC
    EXCEPTIONS
      CANT_CREATE_JOB  = 01
      INVALID_JOB_DATA = 02
      JOBNAME_MISSING  = 03
      OTHERS           = 04.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUBMIT_PROG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_JOBNAME
*&      --> LV_JOBC
*&      --> I_VBELN
*&---------------------------------------------------------------------*
FORM SUBMIT_PROG  USING    P_JOBNAME
                           P_JOBC
                           PV_VBELN.

  SUBMIT ZSDR0052  VIA JOB P_JOBNAME
                   NUMBER  P_JOBC
                   WITH    P_VBELN  = PV_VBELN
                   AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLOSE_JOB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_JOBNAME
*&      --> LV_JOBC
*&      --> TARGET_DATE
*&      --> TARGET_TIME
*&---------------------------------------------------------------------*
FORM CLOSE_JOB  USING    P_JOBNAME
                         P_JOBC
                         P_DATE
                         P_TIME.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      JOBNAME              = P_JOBNAME
      JOBCOUNT             = P_JOBC
      SDLSTRTDT            = P_DATE
      SDLSTRTTM            = P_TIME
    EXCEPTIONS
      CANT_START_IMMEDIATE = 01
      INVALID_STARTDATE    = 02
      JOBNAME_MISSING      = 03
      JOB_CLOSE_FAILED     = 04
      JOB_NOSTEPS          = 05
      JOB_NOTEX            = 06
      LOCK_FAILED          = 07
      OTHERS               = 08.

ENDFORM.
