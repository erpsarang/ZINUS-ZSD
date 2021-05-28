FUNCTION ZSD_SET_DATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_DATE) TYPE  CHAR8 OPTIONAL
*"  EXPORTING
*"     VALUE(E_DATE) TYPE  DATUM
*"----------------------------------------------------------------------
  DATA : LV_DATFM LIKE USR01-DATFM.

  SELECT SINGLE DATFM INTO LV_DATFM
   FROM USR01
   WHERE BNAME = SY-UNAME.

*20200818
  CLEAR E_DATE.
  CASE LV_DATFM.
    WHEN '1'. "DD.MM.YYYY
      E_DATE = I_DATE+6(2)  && I_DATE+4(2) && I_DATE+0(4).
    WHEN '2' OR '3'. "MM/DD/YYYY
      E_DATE = I_DATE+4(2) && I_DATE+6(2) && I_DATE+0(4).
    WHEN OTHERS.
      E_DATE = I_DATE.
  ENDCASE.


ENDFUNCTION.
