*&---------------------------------------------------------------------*
*& Include          ZSDR0020_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .
  CLEAR: GS_STBL.
  GS_STBL-ROW = 'X'. GS_STBL-COL = 'X'.

  IF P_VALID IS INITIAL.
    P_VALID = SY-DATUM.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION_SCREEN .
  CASE  SSCRFIELDS-UCOMM.
    WHEN 'FC01'.

  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CUSTOMER_DESC
*&---------------------------------------------------------------------*
FORM CUSTOMER_DESC  USING    PV_KUNNR
                    CHANGING PV_NAME1.

  DATA : BEGIN OF LS_KUNNR,
           KUNNR LIKE KNA1-KUNNR,
           NAME1 LIKE KNA1-NAME1,
         END OF LS_KUNNR.
  STATICS : LT_KUNNR LIKE HASHED TABLE OF LS_KUNNR WITH UNIQUE KEY KUNNR.


  CLEAR : LS_KUNNR.
  READ TABLE LT_KUNNR INTO LS_KUNNR WITH KEY KUNNR = PV_KUNNR.
  IF SY-SUBRC = 0.
    PV_NAME1 = LS_KUNNR-NAME1.
  ELSE.
    SELECT SINGLE KUNNR NAME1
           INTO CORRESPONDING FIELDS OF LS_KUNNR
           FROM KNA1
           WHERE KUNNR EQ PV_KUNNR.
    IF SY-SUBRC = 0.
      INSERT LS_KUNNR INTO TABLE LT_KUNNR.

      PV_NAME1 = LS_KUNNR-NAME1.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_DATA
*&---------------------------------------------------------------------*
FORM SELECT_DATA .

  SELECT A~MATNR, A~VTWEG, A~KUNNR, C~NAME1, T~MAKTX, A~KSCHL, K~KBETR, K~KONWA, K~KPEIN,
         K~KMEIN, A~DATAB, A~DATBI, K~LOEVM_KO AS LOEVM
         INTO TABLE @DATA(LT_A305)
         FROM A305 AS A INNER JOIN KONP AS K
         ON   A~KNUMH EQ K~KNUMH
         LEFT JOIN MAKT AS T
         ON   A~MATNR EQ T~MATNR
         LEFT JOIN KNA1 AS C
         ON   A~KUNNR EQ C~KUNNR
         WHERE A~VKORG IN @S_VKORG
         AND   A~KUNNR IN @S_KUNNR
         AND   A~VTWEG IN @S_VTWEG
         AND   A~MATNR IN @S_MATNR
         AND   A~DATBI GE @P_VALID
         AND   A~DATAB LE @P_VALID
         AND   A~KSCHL IN @S_KSCHL
         AND   T~SPRAS EQ @SY-LANGU.

  IF SY-SUBRC = 0.
    GT_DISP[] = CORRESPONDING #( LT_A305[] ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ORG
*&---------------------------------------------------------------------*
FORM CHECK_ORG .

  _CLEAR GT_ZSDT0040.

  READ TABLE S_VKORG WITH KEY LOW = '1001'.
  IF SY-SUBRC = 0.
    SELECT ZKUNNR_IC
    INTO CORRESPONDING FIELDS OF TABLE GT_ZSDT0040
    FROM ZSDT0040
    WHERE ZKUNNR_IC IN S_KUNNR.

    SORT GT_ZSDT0040 BY ZKUNNR_IC.
    DELETE ADJACENT DUPLICATES FROM GT_ZSDT0040 COMPARING ZKUNNR_IC.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_DATA_INTERCOM
*&---------------------------------------------------------------------*
FORM SELECT_DATA_INTERCOM .

  RANGES : LR_KUNNR_IC FOR A305-KUNNR.
  CLEAR GV_CHK_IC.
  _CLEAR : LR_KUNNR_IC.
  LOOP AT GT_ZSDT0040.
    _RANGES LR_KUNNR_IC 'I' 'EQ' GT_ZSDT0040-ZKUNNR_IC ''.
  ENDLOOP.

  SELECT A~MATNR, A~VTWEG, A~KUNNR, A~KUNWE, C~NAME1, D~NAME1 AS KUNWE_TXT, T~MAKTX, A~KSCHL, K~KBETR, K~KONWA, K~KPEIN,
         K~KMEIN, A~DATAB, A~DATBI, K~LOEVM_KO AS LOEVM
         INTO TABLE @DATA(LT_A903)
         FROM A903 AS A INNER JOIN KONP AS K
         ON   A~KNUMH EQ K~KNUMH
         LEFT JOIN MAKT AS T
         ON   A~MATNR EQ T~MATNR
         LEFT JOIN KNA1 AS C
         ON   A~KUNNR EQ C~KUNNR
         LEFT JOIN KNA1 AS D
         ON   A~KUNWE EQ D~KUNNR
         WHERE A~VKORG IN @S_VKORG
         AND   A~KUNNR IN @LR_KUNNR_IC
         AND   A~KUNWE IN @S_KUNWE
         AND   A~VTWEG IN @S_VTWEG
         AND   A~MATNR IN @S_MATNR
         AND   A~DATBI GE @P_VALID
         AND   A~DATAB LE @P_VALID
         AND   A~KSCHL IN @S_KSCHL
         AND   T~SPRAS EQ @SY-LANGU.

  IF SY-SUBRC = 0.
    GV_CHK_IC = 'X'.
    GT_DISP[] = CORRESPONDING #( LT_A903[] ).
  ENDIF.


  SELECT A~MATNR, A~VTWEG, A~KUNNR, C~NAME1, T~MAKTX, A~KSCHL, K~KBETR, K~KONWA, K~KPEIN,
         K~KMEIN, A~DATAB, A~DATBI, K~LOEVM_KO AS LOEVM
         INTO TABLE @DATA(LT_A305)
         FROM A305 AS A INNER JOIN KONP AS K
         ON   A~KNUMH EQ K~KNUMH
         LEFT JOIN MAKT AS T
         ON   A~MATNR EQ T~MATNR
         LEFT JOIN KNA1 AS C
         ON   A~KUNNR EQ C~KUNNR
         WHERE A~VKORG IN @S_VKORG
         AND   A~KUNNR IN @S_KUNNR
         AND   A~VTWEG IN @S_VTWEG
         AND   A~MATNR IN @S_MATNR
         AND   A~DATBI GE @P_VALID
         AND   A~DATAB LE @P_VALID
         AND   A~KSCHL IN @S_KSCHL
         AND   T~SPRAS EQ @SY-LANGU.

*- Intercompany에 대한 sold to party 제거
  DELETE LT_A305 WHERE KUNNR IN LR_KUNNR_IC.

  LOOP AT LT_A305 INTO DATA(LS_A305).
    CLEAR GT_DISP.
    MOVE-CORRESPONDING LS_A305 TO GT_DISP.
    APPEND GT_DISP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORIZATION_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORIZATION_CHECK .

 DATA : LS_RETURN LIKE BAPIRETURN1 .

  CALL FUNCTION 'ZBC_AUTHORIZATION_CHECK'
    EXPORTING
      I_VKORG   = S_VKORG-LOW
    IMPORTING
      ES_RETURN = LS_RETURN.

  IF LS_RETURN-TYPE = 'E'.
    MESSAGE E000 WITH LS_RETURN-MESSAGE.
  ENDIF.

ENDFORM.
