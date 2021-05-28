class ZCL_IM_SDBADI_SCD_ACCTG definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_SCD_ACCTG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDBADI_SCD_ACCTG IMPLEMENTATION.


  method IF_EX_BADI_SCD_ACCTG~BEFORE_CHECK.
    IF I_REFOBJ-VTTKF-TPLST EQ '2011'.
      C_VFKN-KOSTL = '30010'.
      C_VFKN-SAKTO = '51116101'.
    ENDIF.
  endmethod.
ENDCLASS.
