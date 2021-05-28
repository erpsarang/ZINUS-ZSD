*&---------------------------------------------------------------------*
*& Include          ZSDR0020_SCR
*&---------------------------------------------------------------------*
TABLES : A305, KONP, KNA1.
DATA : LV_KSCHL TYPE A305-KSCHL,
       LV_VKORG TYPE A305-VKORG,
       LV_VTWEG TYPE A305-VTWEG,
       LV_KUNNR TYPE A305-KUNNR,
       LV_KUNWE TYPE A903-KUNWE,
       LV_MATNR TYPE A305-MATNR,
       LV_KFRST TYPE A305-KFRST,
       LV_DATBI TYPE A305-DATBI,
       LV_DATAB TYPE A305-DATAB.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : S_VKORG FOR LV_VKORG OBLIGATORY NO-EXTENSION no INTERVALS,
                 S_KUNNR FOR LV_KUNNR,
                 S_KUNWE FOR LV_KUNWE,
                 S_VTWEG FOR LV_VTWEG,
                 S_MATNR FOR LV_MATNR. " SKU
PARAMETERS : P_VALID LIKE LV_DATBI OBLIGATORY.
SELECT-OPTIONS : S_KSCHL FOR LV_KSCHL NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B1.
