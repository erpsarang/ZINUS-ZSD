class ZCL_IM_SDBADI_SCD_CREATE definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_SCD_CREATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDBADI_SCD_CREATE IMPLEMENTATION.


  method IF_EX_BADI_SCD_CREATE~SET_HEADER_DATA.
    IF I_REFOBJ-VTTKF-TPLST EQ '2011'.
*      BREAK-POINT.
      IMPORT LV_EXTI2 TO C_VFKK_EXTI2 FROM MEMORY  ID 'ZEXTI2'.
*    EXPORT LV_EXTI2 = LV_EXTI2 TO MEMORY ID 'ZEXTI2'.
      FREE MEMORY ID 'EXTI2'.
    ENDIF.
  endmethod.


  method IF_EX_BADI_SCD_CREATE~SET_SHIPMENT_COST_TYPE.
  endmethod.
ENDCLASS.
