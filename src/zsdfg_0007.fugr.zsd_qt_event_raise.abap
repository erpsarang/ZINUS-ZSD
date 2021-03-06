FUNCTION ZSD_QT_EVENT_RAISE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VBELN) TYPE  VBELN
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------
  DATA: TARGET_DATE TYPE D,
        TARGET_TIME TYPE T.

  DATA: LV_JOBNAME LIKE TBTCJOB-JOBNAME VALUE 'QT&X_&D_&T_&H',
        LV_JOBC    LIKE TBTCJOB-JOBCOUNT.
  DATA: LV_VBELN(8).

  DATA: LV_TOTAL TYPE I,
        LV_BAPI  TYPE I.

  DATA: LV_VKORG TYPE VKORG.
  DATA: LV_ZCMF01_CH(30).
  DATA: LV_ZCMF02_CH(30).
  DATA: LV_DURATION TYPE I.


  SELECT SINGLE VKORG
    INTO @LV_VKORG
    FROM VBAK
    WHERE VBELN = @I_VBELN.

  IF SY-SUBRC NE 0.
    E_RETURN-TYPE = 'E'.
    E_RETURN-MESSAGE = 'Not found Sales Organization'.
    EXIT.
  ELSE.
    "사용여부 CHECK
    SELECT SINGLE ZCMF02_CH
    INTO @LV_ZCMF02_CH
    FROM ZCOMMT0021
    WHERE SPRAS = @SY-LANGU
      AND ZMODULE = 'SD'
      AND ZCLASS = 'SD010'
      AND ZCM_CODE1 = @LV_VKORG.

    IF LV_ZCMF02_CH IS INITIAL.
      E_RETURN-TYPE = 'E'.
      E_RETURN-MESSAGE = 'Do not use'.
      EXIT.
    ENDIF.
  ENDIF.

  "duration 시간 구하기
  SELECT SINGLE ZCMF01_CH
    INTO @LV_ZCMF01_CH
    FROM ZCOMMT0021
    WHERE SPRAS = @SY-LANGU
      AND ZMODULE = 'SD'
      AND ZCLASS = 'SD010'
      AND ZCM_CODE1 = @LV_VKORG.

  LV_DURATION  = LV_ZCMF01_CH.

  " VBEP  : ORDER QYT, CONFIRMED QTY
  SELECT VBELN, POSNR,
    SUM( WMENG ) AS WMENG,
    SUM( BMENG ) AS BMENG
    INTO TABLE @DATA(LT_VBEP)
    FROM VBEP
   WHERE VBELN EQ @I_VBELN
   GROUP BY VBELN, POSNR.

  " MATNR
  SELECT VBELN, POSNR, MATNR
    INTO TABLE @DATA(LT_MATNR)
    FROM VBAP
    FOR ALL ENTRIES IN @LT_VBEP
    WHERE VBELN EQ @LT_VBEP-VBELN
      AND POSNR EQ @LT_VBEP-POSNR.
  SORT LT_MATNR BY VBELN POSNR.

  "오더 수량과 컨펌 수량이 같은 아이템 수
  CLEAR LV_BAPI.
  LOOP AT LT_VBEP INTO DATA(LS_VBEP).
    IF LS_VBEP-WMENG EQ LS_VBEP-BMENG. "ORDER QTY 와 CONFIRMED QTY가 같은 경우
      ADD 1 TO LV_BAPI.
    ENDIF.
  ENDLOOP.

  "Quotation 문서 내 아이템 수
  DESCRIBE TABLE LT_VBEP LINES LV_TOTAL.

  IF LV_TOTAL EQ LV_BAPI."fully confirm 일때만 실행
    CALL FUNCTION 'TSTR_CALC_TIME'
      EXPORTING
        IV_BEGIN_DATELOCAL_REQ = SY-DATUM
        IV_BEGIN_TIMELOCAL_REQ = SY-UZEIT
        IV_DURATION_INTEGER    = LV_DURATION
      IMPORTING
        EV_END_DATELOCAL       = TARGET_DATE
        EV_END_TIMELOCAL       = TARGET_TIME.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = I_VBELN
      IMPORTING
        OUTPUT = LV_VBELN.

    REPLACE '&D' WITH TARGET_DATE  INTO LV_JOBNAME.
    REPLACE '&T' WITH TARGET_TIME  INTO LV_JOBNAME.
    REPLACE '&H' WITH SYST-HOST    INTO LV_JOBNAME.
    REPLACE '&X' WITH LV_VBELN      INTO LV_JOBNAME.
    TRANSLATE LV_JOBNAME TO UPPER CASE.

    PERFORM JOB_OPEN    USING  LV_JOBNAME LV_JOBC E_RETURN.
    PERFORM SUBMIT_PROG USING  LV_JOBNAME LV_JOBC I_VBELN E_RETURN.
    PERFORM CLOSE_JOB   USING  LV_JOBNAME LV_JOBC TARGET_DATE TARGET_TIME E_RETURN.
  ELSE.
    E_RETURN-TYPE = 'E'.
    E_RETURN-TYPE = 'Not fully confirm'.
  ENDIF.

ENDFUNCTION.
