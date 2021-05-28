*&---------------------------------------------------------------------*
*& Include          ZSDR0030_SCR
*&---------------------------------------------------------------------*
TABLES : vbak, bus_joel_main, sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK b00 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_uplo  TYPE c RADIOBUTTON GROUP rd1 DEFAULT 'X' USER-COMMAND rad1.
SELECTION-SCREEN COMMENT 10(10) TEXT-002 FOR FIELD p_uplo.
PARAMETERS: p_sear  TYPE c RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT 40(12) TEXT-003 FOR FIELD p_sear.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN END OF BLOCK b00.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.

PARAMETERS: p_vkorg TYPE vbak-vkorg MODIF ID gr2,  "Sales Org
            p_vtweg TYPE vbak-vtweg MODIF ID gr2,  "Distr. Channel
            p_spart TYPE vbak-spart MODIF ID gr2.  "Division
PARAMETERS: p_file LIKE rlgrap-filename  DEFAULT c_xls MODIF ID gr1.

SELECTION-SCREEN END OF BLOCK b01.
