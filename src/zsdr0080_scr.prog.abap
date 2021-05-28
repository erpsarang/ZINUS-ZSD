*&---------------------------------------------------------------------*
*& Include          ZSDR0080_SCR
*&---------------------------------------------------------------------*
DATA : LV_ZDOSTST TYPE ZEOSTST,
       LV_MATNR   TYPE MATNR.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS    : P_VKORG LIKE VBAK-VKORG OBLIGATORY.

SELECT-OPTIONS: S_AUDAT FOR VBAK-AUDAT NO-DISPLAY,  " Customer Purchase Date
                S_VTWEG FOR VBAK-VTWEG,             " Dist.Chl
                S_VBELN FOR VBAK-VBELN,
                S_AUART FOR VBAK-AUART,             " Order Type
                S_KUNNR FOR VBAK-KUNNR,             " Customer
                S_ERDAT FOR VBAK-ERDAT,             " Order Creation Date
                S_MATNR FOR LV_MATNR,
                S_BSTKD FOR VBKD-BSTKD,             " Customer PO
                S_WADAT FOR LIKP-WADAT_IST,
                S_LDDAT FOR VBEP-LDDAT NO-DISPLAY,  " Loading Planned
                S_STAT  FOR LV_ZDOSTST NO-DISPLAY,  " Order Status
                S_FKDAT FOR VBRK-FKDAT .            " Billing Date
SELECTION-SCREEN END OF BLOCK B1.
