FUNCTION Z_CHANGE_GET_TEXT_AMOUNT.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_AMOUNT) TYPE  BSIS-DMBTR OPTIONAL
*"     VALUE(I_WAERS) TYPE  T001-WAERS OPTIONAL
*"  EXPORTING
*"     VALUE(E_AMT_TEXT) TYPE  BSIS-SGTXT
*"     VALUE(E_AMT_DIGIT) TYPE  BSIS-SGTXT
*"--------------------------------------------------------------------
* - 숫자로된 금액을 한글로 변환한다.
  DATA: L_AMOUNT(60),
        L_AMOUNT_TEXT(60),
        L_DIGIT,
        L_DIGIT_TEXT(2),
        L_UNIT          TYPE  I,
        L_UNIT_TEXT(4),
        LV_LEN          TYPE  I.


* - - - 입력받은 금액을 문자필드로 옮긴다.
  WRITE I_AMOUNT  CURRENCY I_WAERS LEFT-JUSTIFIED TO L_AMOUNT.
  MOVE  L_AMOUNT   TO  E_AMT_DIGIT.


  CHECK  I_WAERS  = 'KRW'.


  DO.
    REPLACE ','  WITH  ''  INTO  L_AMOUNT.
    IF SY-SUBRC <> 0.
       EXIT.
    ENDIF.
  ENDDO.


  CONDENSE  L_AMOUNT  NO-GAPS.


* -- 하나씩 한글로 변환 한다.


    DO.
    L_UNIT = STRLEN( L_AMOUNT ).
    L_DIGIT = L_AMOUNT(1).


    IF L_UNIT EQ 0. EXIT. ENDIF.


    CASE L_DIGIT.
      WHEN '0'.
        PERFORM ZERO_AMOUNT USING L_UNIT L_UNIT_TEXT.
        IF L_UNIT_TEXT NE ' '.
          CLEAR LV_LEN.
          LV_LEN = STRLEN( L_AMOUNT_TEXT ).
          LV_LEN = LV_LEN - 2.
          IF L_AMOUNT_TEXT+LV_LEN(2) = '억'.
          ELSE.
            CONCATENATE L_AMOUNT_TEXT L_UNIT_TEXT INTO L_AMOUNT_TEXT.
          ENDIF.
        ENDIF.
      WHEN '1'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '일' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
      WHEN '2'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '이' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
      WHEN '3'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '삼' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
      WHEN '4'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '사' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
      WHEN '5'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '오' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
      WHEN '6'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '육' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
      WHEN '7'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '칠' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
      WHEN '8'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '팔' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
      WHEN '9'.
        PERFORM GET_UNIT_TEXT USING L_UNIT L_UNIT_TEXT.
        CONCATENATE L_AMOUNT_TEXT '구' L_UNIT_TEXT INTO L_AMOUNT_TEXT.
    ENDCASE.


    SHIFT L_AMOUNT.
  ENDDO.


  CONCATENATE L_AMOUNT_TEXT '원' INTO E_AMT_TEXT.


ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  zero_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_UNIT  text
*      -->P_L_UNIT_TEXT  text
*----------------------------------------------------------------------*
FORM ZERO_AMOUNT  USING P_UNIT P_UNIT_TEXT.


  DATA: L_UNIT    TYPE   I,
        L_PORTION TYPE   I.


  CLEAR: P_UNIT_TEXT.
  L_UNIT = P_UNIT MOD 4.
  L_PORTION = TRUNC( P_UNIT / 4 ).


  CASE L_UNIT.
    WHEN 1.
      CASE L_PORTION.
        WHEN 0.
        WHEN 1.
          CONCATENATE P_UNIT_TEXT '만' INTO P_UNIT_TEXT.
        WHEN 2.
          CONCATENATE P_UNIT_TEXT '억' INTO P_UNIT_TEXT.
        WHEN 3.
          CONCATENATE P_UNIT_TEXT '조' INTO P_UNIT_TEXT.
        WHEN 4.
          CONCATENATE P_UNIT_TEXT '경' INTO P_UNIT_TEXT.
      ENDCASE.
  ENDCASE.



ENDFORM.                    " zero_amount
*&---------------------------------------------------------------------*
*&      Form  get_unit_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_UNIT  text
*      -->P_L_UNIT_TEXT  text
*----------------------------------------------------------------------*
FORM GET_UNIT_TEXT  USING P_UNIT P_UNIT_TEXT.


  DATA: L_UNIT    TYPE  I,
        L_PORTION TYPE   I.


  CLEAR: P_UNIT_TEXT.
*  L_UNIT = CEIL( P_UNIT / 4 ).
*  L_PORTION = P_UNIT MOD 4.
  L_UNIT = P_UNIT MOD 4.
  L_PORTION = TRUNC( P_UNIT / 4 ).


  CASE L_UNIT.
    WHEN 1.
      CASE L_PORTION.
        WHEN 0.
        WHEN 1.
          CONCATENATE P_UNIT_TEXT '만' INTO P_UNIT_TEXT.
        WHEN 2.
          CONCATENATE P_UNIT_TEXT '억' INTO P_UNIT_TEXT.
        WHEN 3.
          CONCATENATE P_UNIT_TEXT '조' INTO P_UNIT_TEXT.
        WHEN 4.
          CONCATENATE P_UNIT_TEXT '경' INTO P_UNIT_TEXT.
      ENDCASE.
    WHEN 2.
      MOVE: '십' TO P_UNIT_TEXT.
    WHEN 3.
      MOVE: '백' TO P_UNIT_TEXT.
    WHEN 0.
      MOVE: '천' TO P_UNIT_TEXT.
  ENDCASE.



ENDFORM.                    " get_unit_text
