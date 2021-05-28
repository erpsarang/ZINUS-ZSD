*&---------------------------------------------------------------------*
*& Include          ZSDR0100_SCR
*&---------------------------------------------------------------------*
TABLES : vbak, vbap, sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS s_bstnk FOR vbak-bstnk. "Customer PO#

PARAMETERS: p_vkorg TYPE vbak-vkorg. " Sales org.

SELECT-OPTIONS: s_vtweg FOR vbak-vtweg. "Dist. Channel.

PARAMETERS: p_spart TYPE vbak-spart, " Division
            p_auart TYPE vbak-auart. " Type.

SELECT-OPTIONS: s_matnr FOR vbap-matnr, "Material
                s_kunnr FOR vbak-kunnr, "Ship to party
                s_erdat FOR vbak-erdat. "Creat On.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.

PARAMETERS : r_all  TYPE c RADIOBUTTON GROUP rd1 DEFAULT 'X' USER-COMMAND rad1,
             r_each TYPE c RADIOBUTTON GROUP rd1,

             c_box1 TYPE c AS CHECKBOX MODIF ID gr1 DEFAULT 'X', "납품 X
             c_box2 TYPE c AS CHECKBOX MODIF ID gr1 DEFAULT 'X', "일부 납품
             c_box3 TYPE c AS CHECKBOX MODIF ID gr1. "납품 완료 (거절 사유 포함)
*             c_box4 TYPE c AS CHECKBOX MODIF ID gr1. "납품 관련X
SELECTION-SCREEN END OF BLOCK b02.

*S_2021/03/15 BY E00064
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-007.
PARAMETERS : r_allb  TYPE c RADIOBUTTON GROUP rd2 DEFAULT 'X' USER-COMMAND rad2,
             r_eachb TYPE c RADIOBUTTON GROUP rd2,
             c_box1b TYPE c AS CHECKBOX MODIF ID gr2, "출고완료 O, 빌링문서 X
             c_box2b TYPE c AS CHECKBOX MODIF ID gr2 USER-COMMAND chk1, "빌링문서 O, 회계문서 X
             c_box3b TYPE c AS CHECKBOX MODIF ID gr2 USER-COMMAND chk2 DEFAULT 'X'. "빌링문서 O, 회계문서 O
SELECT-OPTIONS: s_bidat FOR vbak-erdat MODIF ID gr3. "Billing Date.
SELECTION-SCREEN END OF BLOCK b03.
*E_2021/03/15 BY E00064

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : C_BOX4 TYPE C AS CHECKBOX. " S/O Check without Qoutation
SELECTION-SCREEN COMMENT 3(80) text-003.
SELECTION-SCREEN END OF LINE.
