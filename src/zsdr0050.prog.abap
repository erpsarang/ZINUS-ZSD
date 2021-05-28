*&---------------------------------------------------------------------*
*& Report ZBA0
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSDR0050 MESSAGE-ID VL
                LINE-SIZE 132.

TABLES: NAST.

CONSTANTS LC_VBELN_LEN TYPE I VALUE 10.
CONSTANTS LC_POS_OFFSET TYPE I VALUE 10.


FORM ENTRY_NEW   USING
    E_RC
    E_US_SCREEN.

  PERFORM ENTRY_CHANGE.

  CLEAR : E_RC.

ENDFORM.

FORM ENTRY_CHANGE   .
* 이거만 쓸거임
  DATA: L_MSG_TAB     LIKE BALMI              OCCURS 0 WITH HEADER LINE.
  DATA: L_NAST        LIKE NAST.
  DATA: L_VBAK        LIKE VBAK.
  DATA: L_VBAP        LIKE VBAP.

  DATA: LV_PONUM LIKE VBKD-BSTKD_E,
        LV_QTNUM LIKE VBAK-VBELN,
        LT_VBAP  LIKE TABLE OF VBAP WITH HEADER LINE.

  DATA: LS_HEADER   LIKE BAPISDH1,
        LS_HEADER_X LIKE BAPISDH1X,
        LT_ITEM     LIKE TABLE OF BAPISDITM WITH HEADER LINE,
        LT_ITEM_X   LIKE TABLE OF BAPISDITMX WITH HEADER LINE,
        LT_RETURN   LIKE TABLE OF BAPIRET2 WITH HEADER LINE.

  DATA: LT_RESULT   TYPE TABLE OF ZSDS0100,
        LT_PPRESULT TYPE TABLE OF ZPPT1000,
        LV_PEGG     TYPE I,
        LV_BAPI     TYPE I,
        LS_RETURN   TYPE BAPIRET2.

  DATA: LV_MESSAGE TYPE STRING,
        LV_MSG     TYPE STRING.

  PERFORM L_EXTRACT_DATA_FROM_NAST
              TABLES
                L_MSG_TAB
              CHANGING
                L_NAST
                L_VBAK
                L_VBAP.

*  DATA : x_vbeln TYPE vbak-vbeln.
*  IMPORT x_vbeln FROM MEMORY ID 'x_vbeln'.
*  FREE MEMORY ID 'x_vbeln'.
*
*  IF l_vbak-vbeln IS INITIAL.
*    l_vbak-vbeln = x_vbeln .
*  ENDIF.

  "2021/03/11 S
  DATA : X_KOMKBV1 TYPE KOMKBV1.
  IMPORT X_KOMKBV1 FROM MEMORY ID 'X_KOMKBV1'.
  FREE MEMORY ID 'X_KOMKBV1'.

  IF L_VBAK-VBELN IS INITIAL.
    L_VBAK-VBELN = X_KOMKBV1-VBELN .
  ENDIF.
  "2021/03/11 E

  CLEAR: LV_PONUM, LV_QTNUM, LT_VBAP, LT_VBAP[].

  SELECT SINGLE BSTKD_E
    INTO @LV_PONUM
    FROM VBKD
    WHERE VBELN EQ @L_VBAK-VBELN
    AND POSNR EQ '000000'.
  IF SY-SUBRC = 0.

    SELECT SINGLE VBELN
    INTO @LV_QTNUM
    FROM VBAK
    WHERE AUART EQ 'ZQT'
    AND   BSTNK EQ @LV_PONUM.

    IF SY-SUBRC EQ 0.
      SELECT POSNR, MATNR
        INTO CORRESPONDING FIELDS OF TABLE @LT_VBAP
        FROM VBAP
        WHERE VBELN EQ @LV_QTNUM.

*Possible UPDATEFLAGS:
*U = change
*D = delete
*I = add
      LS_HEADER_X-UPDATEFLAG = 'U'.

      LOOP AT LT_VBAP.
        LT_ITEM-MATERIAL     = LT_VBAP-MATNR.

        LT_ITEM-ITM_NUMBER   = LT_VBAP-POSNR.
        LT_ITEM_X-ITM_NUMBER = LT_VBAP-POSNR.

        LT_ITEM-REASON_REJ   = 'CR'.
        LT_ITEM_X-REASON_REJ = 'X'.

        LT_ITEM_X-UPDATEFLAG = 'U'.

        APPEND LT_ITEM.
        APPEND LT_ITEM_X.
        CLEAR: LT_ITEM, LT_ITEM_X.
      ENDLOOP.

      CLEAR LT_RESULT[].
      CALL FUNCTION 'ZSD_QT_CHANGE_CHECK' " pegging, confirm check fm
        EXPORTING
          I_VBELN  = LV_QTNUM
        TABLES
          T_RESULT = LT_RESULT.

      IF LT_RESULT[] IS NOT INITIAL. "Pegging o, Confirm o
        "Plan order end date와 QT 자재가용일 비교
        LOOP AT LT_RESULT INTO DATA(LS_RESULT).
          IF LS_RESULT-DAT00 <> LS_RESULT-DAT00_QT. " 날짜가 다르면 error
*            CONCATENATE 'Plnd Order End Date''( 'LS_RESULT-DAT00' )' 'and Material avail.date''( 'LS_RESULT-DAT00_QT ' )' 'are different'
*             INTO LV_MSG.
*            MESSAGE S001 WITH LV_MSG DISPLAY LIKE 'E'.
            LEAVE LIST-PROCESSING.
          ENDIF.
        ENDLOOP.

        "오더 수량과 컨펌 수량이 같은 아이템 수
        DESCRIBE TABLE LT_ITEM LINES LV_BAPI.
        "LT_RESULT의 아이템 수와 LT_ITEM의 아이템 수 비교
        DESCRIBE TABLE LT_RESULT LINES LV_PEGG.
        IF LV_BAPI = LV_PEGG. "아이템 수가 같은 경우에만 function 실행
          CALL FUNCTION 'BAPI_CUSTOMERQUOTATION_CHANGE' DESTINATION 'NONE'
            EXPORTING
              SALESDOCUMENT        = LV_QTNUM
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
*            MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' DESTINATION 'NONE'
              EXPORTING
                WAIT = ABAP_TRUE.

            CLEAR LS_RETURN.
            CALL FUNCTION 'ZPP_PLNORD_DELETE_WITH_QT'
              EXPORTING
                I_VBELN  = LV_QTNUM
              IMPORTING
                E_RETURN = LS_RETURN
              TABLES
                T_DATA   = LT_PPRESULT.

            "성공 message
*            MESSAGE S001 WITH 'Success!'.
          ENDIF.
          CALL FUNCTION 'RFC_CONNECTION_CLOSE'
            EXPORTING
              DESTINATION          = 'NONE'
*             taskname             =
            EXCEPTIONS
              DESTINATION_NOT_OPEN = 1
              OTHERS               = 2.
        ELSE."아이템 수가 다른 경우 error
*          MESSAGE S001 WITH 'The number of items is different' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE. " pegging , confirm 되지 않은 경우
*        MESSAGE S001 WITH 'There is no confirmed data' DISPLAY LIKE 'E'.
      ENDIF.
      CLEAR: LT_VBAP, LT_ITEM[], LT_ITEM_X[], LT_RETURN, LT_RETURN[].
    ENDIF.
  ENDIF.

  SY-SUBRC = 4.

ENDFORM.

FORM L_EXTRACT_DATA_FROM_NAST
* Purpose: read data from structure nast (from interface)
*          and extract some values
  TABLES
    X_MSG_TAB STRUCTURE BALMI
  CHANGING
    X_NAST  LIKE NAST
    X_VBAK  LIKE VBAK
    X_VBAP  LIKE VBAP.

* Local data -----------------------------------------------------------

* Function body---------------------------------------------------------

  X_NAST        = NAST.
  X_VBAK-MANDT  = NAST-MANDT.
  X_VBAK-VBELN  = NAST-OBJKY(LC_VBELN_LEN).
  X_VBAP-MANDT  = NAST-MANDT.
  X_VBAP-POSNR  = NAST-OBJKY+LC_POS_OFFSET(LC_VBELN_LEN).
ENDFORM.
*{   INSERT         ZUQK901230                                        1
form entry_billing USING E_RC
                          E_US_SCREEN.
  DATA: L_MSG_TAB     LIKE BALMI OCCURS 0 WITH HEADER LINE.
  DATA: L_NAST        LIKE NAST.
  DATA: L_VBAK        LIKE VBAK.
  DATA: L_VBAP        LIKE VBAP.

  PERFORM L_EXTRACT_DATA_FROM_NAST
           TABLES
             L_MSG_TAB
           CHANGING
             L_NAST
             L_VBAK
             L_VBAP.

DATA : X_KOMKBV3 TYPE KOMKBV3.
  IMPORT X_KOMKBV3 FROM MEMORY ID 'X_KOMKBV3'.
  FREE MEMORY ID 'X_KOMKBV3'.

* 생성시에는 루틴의 메모리값을 못가져옴

  CLEAR : E_RC.
ENDFORM.

form entry_billing_change .
  DATA: L_MSG_TAB     LIKE BALMI OCCURS 0 WITH HEADER LINE.
  DATA: L_NAST        LIKE NAST.
  DATA: L_VBAK        LIKE VBAK.
  DATA: L_VBAP        LIKE VBAP.

  PERFORM L_EXTRACT_DATA_FROM_NAST
           TABLES
             L_MSG_TAB
           CHANGING
             L_NAST
             L_VBAK
             L_VBAP.

 DATA : X_KOMKBV3 TYPE KOMKBV3.
  IMPORT X_KOMKBV3 FROM MEMORY ID 'X_KOMKBV3'.
  FREE MEMORY ID 'X_KOMKBV3'.

* 수정시에는 루틴의 메모리값을 가져옴

  SY-SUBRC = 4.
ENDFORM.

*}   INSERT

FORM ENTRY_DELIVERY USING E_RC
                          E_US_SCREEN.
  "-- entry_delivery에 반드시 e_rc, e_us_screen을 받을 것.  필히 지켜주세요 덤프나요
  DATA: L_MSG_TAB     LIKE BALMI OCCURS 0 WITH HEADER LINE.
  DATA: L_NAST        LIKE NAST.
  DATA: L_VBAK        LIKE VBAK.
  DATA: L_VBAP        LIKE VBAP.

  PERFORM L_EXTRACT_DATA_FROM_NAST
           TABLES
             L_MSG_TAB
           CHANGING
             L_NAST
             L_VBAK
             L_VBAP.


  CLEAR : E_RC.
ENDFORM.

FORM ENTRY_DELIVERY_CHANGE.
* Purpose: Shipment complete trigger to HQ GR, Invoice
  DATA: L_MSG_TAB     LIKE BALMI OCCURS 0 WITH HEADER LINE.
  DATA: L_NAST        LIKE NAST.
  DATA: L_VBAK        LIKE VBAK.
  DATA: L_VBAP        LIKE VBAP.
  DATA: LS_VBUK       LIKE VBUK.
  DATA: LV_DATE       TYPE SY-DATUM.
  DATA: LV_EDATE      TYPE SY-DATUM.
  DATA: LV_ETIME      TYPE SY-UZEIT.
*  DATA: lv_fkdab      TYPE sy-datum.
  DATA: LV_DATUM(10).
  DATA: LV_FKDAB(10).
  DATA: LV_VKORG      TYPE VKORG.
  DATA: LV_JOBGROUP	LIKE TBTCJOB-JOBGROUP,
        LV_JOBNAME  LIKE TBTCJOB-JOBNAME,
        LV_JOBCOUNT	LIKE TBTCJOB-JOBCOUNT.
  DATA: LV_WADAT_IST LIKE LIKP-WADAT_IST.

  DATA: CTU_PARAMS LIKE CTU_PARAMS,
        GT_BDC     LIKE TABLE OF BDCDATA    WITH HEADER LINE,
        GT_MSG     LIKE TABLE OF BDCMSGCOLL WITH HEADER LINE.

  PERFORM L_EXTRACT_DATA_FROM_NAST
            TABLES
              L_MSG_TAB
            CHANGING
              L_NAST
              L_VBAK
              L_VBAP.

  DATA : X_KOMKBV2 TYPE KOMKBV2.
  IMPORT X_KOMKBV2 FROM MEMORY ID 'X_KOMKBV2'.
  FREE MEMORY ID 'X_KOMKBV2'.

  IF L_VBAK-VBELN IS INITIAL.
    L_VBAK-VBELN = X_KOMKBV2-VBELN .
  ENDIF.

  DATA : X_LIKP TYPE LIKP.
  IMPORT X_LIKP FROM MEMORY ID 'X_LIKP'.
  FREE MEMORY ID 'X_LIKP'.

*  CALL FUNCTION 'SD_VBUK_SINGLE_READ'
*    EXPORTING
*      I_VBELN          = L_VBAK-VBELN
*      I_VBTYP          = 'J'
*    IMPORTING
*      E_VBUK           = LS_VBUK
*    EXCEPTIONS
*      RECORD_NOT_FOUND = 1
*      OTHERS           = 2.

  IF X_KOMKBV2-WBSTK = 'C' AND X_KOMKBV2-FKSTK NE 'C'.
*    call function 'BP_EVENT_RAISE'
*      exporting
*        eventid = 'ZSD_GI'.

    CALL FUNCTION 'ZSD_BP_EVENT_RAISE'
      EXPORTING
        I_VBELN = L_VBAK-VBELN
*       I_TEST  = ' '
      .


* E00062(03/01/2020)
    IF   X_KOMKBV2-LFART EQ 'ZICW'.
      CALL FUNCTION 'ZSD_POST_DELIVERY'
        EXPORTING
          I_VBELN = L_VBAK-VBELN
          I_BUDAT = X_LIKP-WADAT_IST.
*         I_CANCEL       =
*         I_TEST         =
      .

*    submit zsdr0051 with p_vbeln = l_vbak-vbeln
*                                 with p_budat = x_likp-wadat_ist
*                                  with p_can = '' and return.
    ENDIF.
* E00062(03/01/2020)

  ENDIF.

* E00062(03/01/2020)
  IF X_KOMKBV2-WBSTK NE 'C' AND  SY-TCODE = 'VL09' AND X_KOMKBV2-LFART EQ 'ZICW'.
    SELECT SINGLE WADAT_IST
      INTO @LV_WADAT_IST
      FROM LIKP
      WHERE VBELN = @L_VBAK-VBELN.

    CALL FUNCTION 'ZSD_POST_DELIVERY'
      EXPORTING
        I_VBELN  = L_VBAK-VBELN
        I_BUDAT  = LV_WADAT_IST
        I_CANCEL = 'X'.
*         I_TEST         =
*
*    submit zsdr0051 with p_vbeln = l_vbak-vbeln
*                               with p_budat = lv_wadat_ist
*                               with p_can = 'X' and return.
  ENDIF.
* E00062(03/01/2020)

*  "/////////////////////////////////////////////////////////////////////
*  "////
*  "//// HQ Invoice 생성
*  "////
*  "/////////////////////////////////////////////////////////////////////
*  "----// 변수선언
*  DATA: lv_api_doc_id TYPE psi_doc_id,
*        lv_cancel     TYPE cancel,
*        lv_xtrade     TYPE char1,
*        lt_head       TYPE TABLE OF zmms0200 WITH HEADER LINE,
*        lt_item       TYPE TABLE OF zmms0202 WITH HEADER LINE,
*        lt_return     TYPE TABLE OF zmms0201 WITH HEADER LINE,
*        lt_vbfa       TYPE TABLE OF vbfas    WITH HEADER LINE.
*  "----// 변수 초기화 및 입력
*  CLEAR: lv_api_doc_id,
*         lv_cancel,
*         lv_xtrade,
*         lt_head[],   lt_head,
*         lt_item[],   lt_item,
*         lt_return[], lt_return,
*         lt_vbfa[],   lt_vbfa.
*  lv_api_doc_id    = sy-datlo(8) && sy-timlo(6).
*  lv_cancel        = ' '.
*  lv_xtrade        = 'X'.
*  lt_head-invno_im = 'ZVIN' && lv_api_doc_id.
*  lt_head-bukrs    = '1000'.
*  lt_head-waers    = 'USD'.
*  APPEND lt_head.
*  lt_item-invno_im = lt_head-invno_im.
*  CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
*    EXPORTING
*      comwa         = l_vbak-vbeln
*    TABLES
*      vbfa_tab      = lt_vbfa
*    EXCEPTIONS
*      no_vbfa       = 1
*      no_vbuk_found = 2
*      OTHERS        = 3.
*  IF sy-subrc <> 0.
*    EXIT.
*  ENDIF.
*  LOOP AT lt_vbfa.
*
*  ENDLOOP.
*  "----// IMPORT INVOICE
*  CALL FUNCTION 'ZMM_IF_IMPORT_INVOICE'
*    EXPORTING
*      api_doc_id = lv_api_doc_id
*      cancel     = lv_cancel
*      xtrade     = lv_xtrade
*    TABLES
*      gt_head    = lt_head
*      gt_item    = lt_item
*      gt_return  = lt_return.
*  "/////////////////////////////////////////////////////////////////////
*  "////
*  "//// HQ GR
*  "////
*  "/////////////////////////////////////////////////////////////////////
*  "----//
*  CALL FUNCTION 'ZMM_IF_BL_CONTAINER'
*    EXPORTING
*      api_doc_id =
**     CANCEL     =
**     XTRADE     =
**     ZIM_EX     =
*    tables
*      gt_head    =
*      gt_item    =
*      gt_return  =.


  SY-SUBRC = 4.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_APPEND_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_BDC
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM BDC_APPEND_DATA  TABLES   PT_BDC STRUCTURE BDCDATA
                      USING    VALUE(P_DYNBEGIN)
                               VALUE(P_NAME)
                               VALUE(P_VALUE).

  CLEAR :PT_BDC.

  IF P_DYNBEGIN = 'X'.
    PT_BDC-DYNBEGIN = P_DYNBEGIN.
    PT_BDC-PROGRAM  = P_NAME.
    PT_BDC-DYNPRO   = P_VALUE.
  ELSE.
    PT_BDC-FNAM = P_NAME.
    PT_BDC-FVAL = P_VALUE.
  ENDIF.

  APPEND PT_BDC.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ENTRY_NEW_QT
*&---------------------------------------------------------------------*
FORM ENTRY_NEW_QT   USING
    E_RC
    E_US_SCREEN.
  DATA : LT_MSG TYPE BAPIRET2_T,
         LS_MSG TYPE BAPIRET2.

  DATA: L_MSG_TAB     LIKE BALMI              OCCURS 0 WITH HEADER LINE.
  DATA: L_NAST        LIKE NAST.
  DATA: L_VBAK        LIKE VBAK.
  DATA: L_VBAP        LIKE VBAP.

*S_2021/3/11 BY E00064
  DATA: LV_CUSTPO  LIKE VBKD-BSTKD,
        LV_PREQ_NO LIKE BAPIMEREQHEADER-PREQ_NO,
        LT_VBAP    LIKE TABLE OF VBAP WITH HEADER LINE,
        LT_RET     LIKE TABLE OF BAPIRET2 WITH HEADER LINE.
*  DATA: lv_error.
*E_2021/3/11 BY E00064

  PERFORM L_EXTRACT_DATA_FROM_NAST
              TABLES
                L_MSG_TAB
              CHANGING
                L_NAST
                L_VBAK
                L_VBAP.

*  DATA : x_vbeln TYPE vbak-vbeln.
*  IMPORT x_vbeln FROM MEMORY ID 'x_vbeln'.
*  FREE MEMORY ID 'x_vbeln'.
*
*  IF l_vbak-vbeln IS INITIAL.
*    l_vbak-vbeln = x_vbeln .
*  ENDIF.

  "2021/03/11 S
  DATA : X_KOMKBV1 TYPE KOMKBV1.
  IMPORT X_KOMKBV1 FROM MEMORY ID 'X_KOMKBV1'.
  FREE MEMORY ID 'X_KOMKBV1'.

  IF L_VBAK-VBELN IS INITIAL.
    L_VBAK-VBELN = X_KOMKBV1-VBELN .
  ENDIF.
  "2021/03/11 E

*  // 로직은 여기에...
*S_2021/3/11 BY E00064
  CLEAR: LV_CUSTPO, LT_VBAP, LT_VBAP[].

* 고객PO번호 (있는 경우는 넣어주시고, 없는 경우는 안 넣음)  VBKD-BSTKD  (vbkd-posnr : 000000)
  SELECT SINGLE BSTKD
    INTO @LV_CUSTPO
    FROM VBKD
    WHERE VBELN EQ @L_VBAK-VBELN
    AND POSNR EQ '000000'.

* read item data
  SELECT VBELN, POSNR, MATNR, WERKS, VKAUS, KWMENG, VRKME
    INTO CORRESPONDING FIELDS OF TABLE @LT_VBAP
    FROM VBAP
    WHERE VBELN EQ @L_VBAK-VBELN.

  LOOP AT LT_VBAP.
    CALL FUNCTION 'ZMM_PR_FOR_PACKINGMAT'
      IN BACKGROUND TASK
      DESTINATION 'NONE'
      EXPORTING
        I_BSTKD  = LV_CUSTPO
        I_VBELN  = LT_VBAP-VBELN
        I_POSNR  = LT_VBAP-POSNR
        I_MATNR  = LT_VBAP-MATNR
        I_WERKS  = LT_VBAP-WERKS
        I_VKAUS  = LT_VBAP-VKAUS
        I_KWMENG = LT_VBAP-KWMENG
        I_VRKME  = LT_VBAP-VRKME
        I_FLAG   = 'C'
      IMPORTING
        E_NUMBER = LV_PREQ_NO
      TABLES
        T_RETURN = LT_RET.

*    READ TABLE lt_ret WITH KEY type = 'E'.
*    IF sy-subrc EQ 0.
*      lv_error = 'X'. EXIT.
*    ENDIF.
  ENDLOOP.

* IF lv_error = 'X'.
*   CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.  "DESTINATION 'NONE'.
* ELSE.
*   CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'     "DESTINATION 'NONE'
*     EXPORTING
*       wait = abap_true.
* ENDIF.

*E_2021/3/11 BY E00064

*  CALL FUNCTION 'ZSD_QT_EVENT_RAISE'
*    IN BACKGROUND TASK
*    DESTINATION 'NONE'
*    EXPORTING
*      I_VBELN = L_VBAK-VBELN.

  CLEAR : E_RC.
ENDFORM .

*&---------------------------------------------------------------------*
*& Form ENTRY_CHANGE_QT
*&---------------------------------------------------------------------*
FORM ENTRY_CHANGE_QT.
  DATA : LT_MSG TYPE BAPIRET2_T,
         LS_MSG TYPE BAPIRET2.

  DATA: L_VBAK        LIKE VBAK.

*S_2021/3/11 BY E00064
  DATA: LV_BSTKD_E LIKE VBKD-BSTKD_E,
        LV_CUSTPO  LIKE VBKD-BSTKD,
*        lv_qtnum   LIKE vbak-vbeln,
*        lv_auart   LIKE vbak-auart,
        LV_PREQ_NO LIKE BAPIMEREQHEADER-PREQ_NO,
        LT_VBAP    LIKE TABLE OF VBAP WITH HEADER LINE,
        LT_RET     LIKE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA: LV_ERROR.
  DATA: LS_VBUK    LIKE VBUK.
*E_2021/3/11 BY E00064

  DATA: LS_RETURN TYPE BAPIRET2.

*  DATA : x_vbeln TYPE vbak-vbeln.
*  IMPORT x_vbeln FROM MEMORY ID 'x_vbeln'.
*  FREE MEMORY ID 'x_vbeln'.
*
*  l_vbak-vbeln = x_vbeln .

  "2021/03/11 S
  DATA : X_KOMKBV1 TYPE KOMKBV1.
  IMPORT X_KOMKBV1 FROM MEMORY ID 'X_KOMKBV1'.
  FREE MEMORY ID 'X_KOMKBV1'.

  IF L_VBAK-VBELN IS INITIAL.
    L_VBAK-VBELN = X_KOMKBV1-VBELN .
  ENDIF.
  "2021/03/11 E

*  // 로직은 여기에...
*S_2021/3/11 BY E00064

*s/o생성 후 자동소멸 : vbak-auart : ZICW 이고 vbkd-bstkd_e가 해당 고객PO번호로 존재 시에는 해당로직 적용안함
*quotation의 overall status가 'C', Overall Rejection Status가 'C'인 경우만 해당 로직 적용
*SD_VBUK_SINGLE_READ에 해당 quotation번호 넣고
*E_VBUK-GBSTK 가 'C' , ABSTK 가 C인 경우만 해당 로직 적용


*  CLEAR: lv_custpo, lv_qtnum, lt_vbap, lt_vbap[].
  CLEAR: LV_CUSTPO, LT_VBAP, LT_VBAP[].

* 고객PO번호 (있는 경우는 넣어주시고, 없는 경우는 안 넣음)  VBKD-BSTKD  (vbkd-posnr : 000000)
  SELECT SINGLE BSTKD
    INTO (@LV_CUSTPO)
    FROM VBKD
    WHERE VBELN EQ @L_VBAK-VBELN
    AND POSNR EQ '000000'.

* Sales Document Type
*  lv_auart = l_vbak-auart.  " Sales Document Type
*  SELECT SINGLE auart INTO @lv_auart FROM vbak
*  WHERE vbeln EQ @l_vbak-vbeln.


*s/o생성 후 자동소멸 : vbak-auart : ZICW 이고 vbkd-bstkd_e가 해당 고객PO번호로 존재 시에는 해당로직 적용안함
*  테스트 해본 결과 s/o생성 후 해당 테이블에 아직 업데이트 되지 않아 해당 아래 로직은 성립이 안됨
*  select single a~bstkd_e
*    into (@lv_bstkd_e)
*    from vbkd as a join vbak as b on a~vbeln = b~vbeln
*    where a~bstkd_e eq @lv_custpo
*    and a~posnr eq '000000'
*    and b~auart = 'ZICW'.   "Sales Document Type
*  if lv_bstkd_e is not initial.
*    sy-subrc = 4. exit.
*  endif.
*  그냥 tcode 가 va22로 접근하여 소멸한 경우만 아래 로직을 타도록 적용함

**2021/03/19 주석처리
*  if sy-tcode NE 'VA22'.
*    sy-subrc = 4. exit.
*  endif.

*##############
* Check Status
*##############
*quotation의 overall status가 'C', Overall Rejection Status가 'C'인 경우만 해당 로직 적용
*SD_VBUK_SINGLE_READ에 해당 quotation번호 넣고
*E_VBUK-GBSTK 가 'C' , ABSTK 가 C인 경우만 해당 로직 적용
*  CALL FUNCTION 'SD_VBUK_SINGLE_READ'
*    EXPORTING
*      I_VBELN          = L_VBAK-VBELN "lv_qtnum
*      I_VBTYP          = 'B'    " QT
**     I_BYPASSING_BUFFER       = ' '
**     I_REFRESH_BUFFER =
*    IMPORTING
**     E_VBUKVB         =
*      E_VBUK           = LS_VBUK
*    EXCEPTIONS
*      RECORD_NOT_FOUND = 1
*      OTHERS           = 2.
*  IF SY-SUBRC <> 0.
**   Implement suitable error handling here
*  ENDIF.

  "2021/03/24 KMS S
  IF X_KOMKBV1-ABSTK NE 'C'.
    CALL FUNCTION 'ZSD_QT_EVENT_RAISE'
      EXPORTING
        I_VBELN  = L_VBAK-VBELN
      IMPORTING
        E_RETURN = LS_RETURN.
  ENDIF.
  "2021/03/24 KMS E

  IF X_KOMKBV1-ABSTK = 'C'.
  ELSE.
    SY-SUBRC = 4. EXIT.
  ENDIF.

  SELECT VBELN, POSNR, MATNR, WERKS, VKAUS, KWMENG, VRKME
    INTO CORRESPONDING FIELDS OF TABLE @LT_VBAP
    FROM VBAP
    WHERE VBELN EQ @L_VBAK-VBELN.

  LOOP AT LT_VBAP.
    CALL FUNCTION 'ZMM_PR_FOR_PACKINGMAT'
      IN BACKGROUND TASK
      DESTINATION 'NONE'
      EXPORTING
        I_BSTKD  = LV_CUSTPO
        I_VBELN  = LT_VBAP-VBELN
        I_POSNR  = LT_VBAP-POSNR
        I_MATNR  = LT_VBAP-MATNR
        I_WERKS  = LT_VBAP-WERKS
        I_VKAUS  = LT_VBAP-VKAUS
        I_KWMENG = LT_VBAP-KWMENG
        I_VRKME  = LT_VBAP-VRKME
        I_FLAG   = 'D'
      IMPORTING
        E_NUMBER = LV_PREQ_NO    " PR No.
      TABLES
        T_RETURN = LT_RET.

*    READ TABLE lt_ret WITH KEY type = 'E'.
*    IF sy-subrc EQ 0.
*      lv_error = 'X'. EXIT.
*    ENDIF.
  ENDLOOP.

* IF lv_error = 'X'.
*   CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.  "DESTINATION 'NONE'.
* ELSE.
*   CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'     "DESTINATION 'NONE'
*     EXPORTING
*       wait = abap_true.
* ENDIF.

*E_2021/3/11 BY E00064

  SY-SUBRC = 4.
ENDFORM.
