*----------------------------------------------------------------------*
***INCLUDE LZSD1130F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- E_MSG
*&---------------------------------------------------------------------*
form check_data  changing pt_data like zmm1t5020
                                           p_msg type string.

  data: lv_msg type string.

  if pt_data-kcoo is initial.
    p_msg = text-t01.
    exit.
  endif.

  if pt_data-lifnr is initial.
    p_msg = text-t10.
    exit.
  endif.

  if pt_data-conno is initial.
    p_msg = text-t02.
    exit.
  endif.

  if pt_data-blnum is initial.
    p_msg = text-t03.
    exit.
  endif.

  if pt_data-poline is initial.
    p_msg = text-t04.
    exit.
  endif.


  if pt_data-invno is initial.
    p_msg = text-t05.
    exit.
  endif.

  if pt_data-matno is initial.
    p_msg = text-t06.
    exit.
  else.
    pt_data-fcode = pt_data-matno.
  endif.

  if pt_data-ordqty is initial.
    p_msg = text-t07.
    exit.
  endif.

  if pt_data-quant is initial.
    p_msg = text-t08.
    exit.
  endif.

  if pt_data-ponum is initial.
    p_msg = text-t09.
    exit.
  endif.

  if pt_data-consz is initial.
    p_msg = text-t11.
    exit.
  endif.

  if pt_data-cnprice_usd is initial.
    p_msg = text-t12.
    exit.
  endif.

  if pt_data-cnamount_usd is initial.
    p_msg = text-t13.
    exit.
  endif.




endform.
