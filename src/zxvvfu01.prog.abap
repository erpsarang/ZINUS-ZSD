*&---------------------------------------------------------------------*
*& Include          ZXVVFU01
*&---------------------------------------------------------------------*
READ TABLE CVBRP INDEX 1.

SELECT SINGLE INCO3_L
  INTO @DATA(LV_TEXT)
  FROM LIKP
  WHERE VBELN = @CVBRP-VGBEL.

IF SY-SUBRC = 0 AND LV_TEXT IS NOT INITIAL.
  XACCIT-XBLNR = LV_TEXT.
ELSE.
  SELECT SINGLE B~ZZKOINV
  INTO @DATA(LV_INVOICE)
  FROM VBFA AS A INNER JOIN VBRK AS B ON A~VBELV = B~VBELN
  WHERE A~VBELN = @CVBRP-VGBEL
    AND A~POSNN = '000000'
    AND A~VBTYP_V = 'M'.
  IF SY-SUBRC = 0.
    XACCIT-XBLNR = LV_INVOICE.
  ENDIF.
ENDIF.
