class ZCL_IM_LE_SHP_GOODSMOVEMEN definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_GOODSMOVEMENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_LE_SHP_GOODSMOVEMEN IMPLEMENTATION.


  method if_ex_le_shp_goodsmovement~change_input_header_and_items.
    data: x_likp type likp.
    if is_likp-lfart eq 'ZICW' or is_likp-lfart eq 'ZFTD'.
      export x_likp from is_likp to memory id 'X_LIKP'.
    endif.
  endmethod.
ENDCLASS.
