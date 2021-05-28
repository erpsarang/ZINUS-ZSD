*&---------------------------------------------------------------------*
*& Include          ZSDI0010TOP
*&---------------------------------------------------------------------*
TABLES : EKKO, T001W, VBKD, SSCRFIELDS.
*&---------------------------------------------------------------------*
*& INTERNAL TABLES
*&---------------------------------------------------------------------*
* Display
DATA: BEGIN OF GT_LIST OCCURS 0,
        VBELN             LIKE VBAK-VBELN,            "Sales order#
        KUNNR             LIKE VBAK-KUNNR,            "Sold-To party
        KUNNRT            LIKE KNA1-NAME1,            "Sold-To party name
        KUNNR_ADDRESS(255),                           "Sold-To address
        STRAS             LIKE KNA1-STRAS,            "Sold-To address
        PSTLZ             LIKE KNA1-PSTLZ,            "Sold-To address
        ORT01             LIKE KNA1-ORT01,            "Sold-To address
        KUNNR_LAND        LIKE KNA1-LAND1,            "Sold-To COUNTRY
        KUNWE             LIKE LIKP-KUNNR,            "Ship-To party
        KUNWET            LIKE KNA1-NAME1,            "Ship-To party name
        KUNWE_ADDRESS(255),                           "Ship-To address
        STRAS_2           LIKE KNA1-STRAS,            "Ship-To address
        PSTLZ_2           LIKE KNA1-PSTLZ,            "Ship-To address
        ORT01_2           LIKE KNA1-ORT01,            "Ship-To address
        KUNWE_LAND        LIKE KNA1-LAND1,            "Ship-To COUNTRY
        AUART             LIKE VBAK-AUART,            "Sales Document Type
        AUART_TXT         LIKE TVAKT-BEZEI,           "Sales Document Type NAME
        AUDAT             LIKE VBAK-AUDAT,            "Document Date
        VDATU             LIKE VBAK-VDATU,            "Requested Delivery Date
        WAERK             LIKE VBAK-WAERK,            "SD document currency
        VKORG             LIKE VBAK-VKORG,            "Sales Organization
        VKORG_TXT         LIKE TVKOT-VTEXT,           "Sales Organization NAME
        VTWEG             LIKE VBAK-VTWEG,            "Distribution Channel
        VTWEG_TXT         LIKE TVTWT-VTEXT,           "Distribution Channel NAME
        SPART             LIKE VBAK-SPART,            "Division
        SPART_TXT         LIKE TSPAT-VTEXT,           "Division NAME
        BSTKD             LIKE VBKD-BSTKD,            "Customer Purchase Order#
        BUKRS             LIKE T001-BUKRS,            "Corp code
        BUKRS_TXT         LIKE T001-BUTXT,            "Corp code NAME
        LAND1             LIKE T001-LAND1,
        ZTERM             LIKE VBKD-ZTERM,            "payment term
        ZTERM_TXT         LIKE TVZBT-VTEXT,           "payment term NAME
        INCO1             LIKE VBKD-INCO1,            "incoterms
        INCO2             LIKE VBKD-INCO2,            "Incoterms Location
        IHREZ             LIKE VBKD-IHREZ,            "CHECK OPEN PO
        ZSHIP_LAND(25),                               "Ship-to + Country
        VBELN_I           LIKE VBAP-VBELN,            "Sales order#
        POSNR             LIKE VBAP-POSNR,            "Sales order item#
        ZDELE,                                        "Sales order DELETION
        ZCOMPLETE,                                    "zcomplete
        MATNR             LIKE VBAP-MATNR,            "Sales material
        MATNR_TXT         LIKE VBAP-ARKTX,            "Sales material name
        CHARG             LIKE VBAP-CHARG,            "Batch Number
        ABGRU             LIKE VBAP-ABGRU,            "Reason for Rejection
        KDMAT             LIKE VBAP-KDMAT,            "Material Number Used by Customer
        WERKS             LIKE VBAP-WERKS,            "Sales Plant
        WERKS_TXT         LIKE T001W-NAME1,           "Sales Plant NAME
        LGORT             LIKE VBAP-LGORT,            "Sales Storage location
        LGORT_TXT         LIKE T001L-LGOBE,           "Sales Storage location NAME
        KWMENG            LIKE VBAP-KWMENG,           "Sales Qty
        VRKME             LIKE VBAP-VRKME,            "Sales unit
        NETPR             LIKE VBAP-NETPR,            "Sales NET price
        WAERK_2           LIKE VBAP-WAERK,            "Sales curency
        KPEIN             LIKE VBAP-KPEIN,            "Condition Pricing Unit
        KMEIN             LIKE VBAP-KMEIN,            "Condition Unit
        NETWR             LIKE VBAP-NETWR,            "Net Value of the Order Item
        ZZBIG             LIKE MARA-ZZBIG,            "Big Category
        ZBIGTX            LIKE ZMDT0180-ZBIGTX,       "Big Category Text
        ZZMID             LIKE MARA-ZZMID,            "Middle Category
        ZMIDTX            LIKE ZMDT0181-ZMIDTX,       "Middle Category Text
        ZZSIN             LIKE MARA-ZZSIN,            "Single Category
        ZSINGTX           LIKE ZMDT0182-ZSINGTX,      "Single Category Text
        BSTKD_2           LIKE VBKD-BSTKD,            "Customer Purchase Order#
        POSEX             LIKE VBAP-POSEX,            "Customer Purchase Order item#
        NTGEW             LIKE VBAP-NTGEW,            "Net Weight
        GEWEI             LIKE VBAP-GEWEI,            "Unit of Weight
        BRGEW             LIKE VBAP-BRGEW,            "Gross Weight
        VOLUM             LIKE VBAP-VOLUM,            "Volume
        VOLEH             LIKE VBAP-VOLEH,            "Volume unit
        NTGEW_G           LIKE VBAP-NTGEW,            "Net Weight of the Item
        BRGEW_G           LIKE VBAP-BRGEW,             "Gross Weight of the Item
        VOLUM_G           LIKE VBAP-VOLUM,             "Volume of the item
        CWERK             LIKE EKPO-WERKS,            "Customer PO plant
        CWERK_TXT         LIKE T001W-NAME1,           "Customer PO plant NAME
        CEIND             LIKE EKET-EINDT,            "Customer PO DELIVERY DATE
        GROES             LIKE MARA-GROES,            "SIZE
        ZWFSKU            LIKE ZSDS0020-ZWFSKU,       "Wayfair SKU --> Customer SKU
        ZASIN             LIKE ZSDS0020-ZASIN,        "ASIN
        ZWFPOH            LIKE ZSDS0020-ZWFPOH,       "Customer PO (Ship To)
        ZWFPOI            LIKE ZSDS0020-ZWFPOI,       "Customer PO Line No
        BSTKD_AZ          LIKE EKKO-VERKF,            "Amzone PO
        POSEX_AZ          LIKE VBAP-POSEX ,           "Amazone PO Line NO
        EAN11             LIKE MARA-EAN11,            "UPC NP
        CCNGN             LIKE MARC-STAWN,            "HS CODE
        CCNGNX            LIKE MARC-STAWN,            "HS CODE text
        ZRSLT_SAP         TYPE CHAR1,                 "처리상태 (S, E)_
        ZMSG_SAP          TYPE CHAR255,               "SAP 처리 메시지(SAP기준)
        ZDATE_SAP         TYPE CHAR8,                 "SAP 처리일자(SAP기준)
        ZTIME_SAP         TYPE CHAR6,                 "SAP 처리시간(SAP기준)
        PSTYV             LIKE VBAP-PSTYV,
        ICON(4),
        MESSAGE(100),
        CELLSTYLE         TYPE LVC_T_STYL,
        CELLCOLOR         TYPE LVC_T_SCOL,
      END OF GT_LIST,
      GT_SEND LIKE TABLE OF GT_LIST WITH HEADER LINE.


DATA : BEGIN OF GT_9100 OCCURS 0,
         ZCONT     LIKE  ZMMT0010-ZCONT,
         WERKS     LIKE  ZMMT0010-WERKS,
         WERKS_TXT LIKE  T001W-NAME1,
         IFDAT     LIKE  SY-DATUM,
         IFZET     LIKE  SY-UZEIT,
         IFTSP     LIKE  ZMMT0010-IFTSP,
         IFNAM     LIKE  ZMMT0010-IFNAM,
         IFCOUNT   LIKE  ZMMT0010-IFCOUNT,
       END OF GT_9100.

DATA: BEGIN OF GS_HEAD.
    INCLUDE STRUCTURE ZSDS0010.
DATA: END OF GS_HEAD.

DATA: BEGIN OF GS_ITEM.
    INCLUDE STRUCTURE ZSDS0020.
DATA: END OF GS_ITEM.

DATA: GT_HEAD LIKE TABLE OF GS_HEAD,
      GT_ITEM LIKE TABLE OF GS_ITEM.

DATA: BEGIN OF GS_RETURN.
    INCLUDE STRUCTURE ZMMS0140.
DATA: END OF GS_RETURN.

DATA GT_ZSDT0081 LIKE TABLE OF ZSDT0081 WITH HEADER LINE.

DATA: GS_LOG_H TYPE ZMMT0010,
      GT_LOG_H LIKE TABLE OF GS_LOG_H.

*----------------------------------------------------------------------*
*  FIELD-SYMBOLS
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <GT_TBL>   TYPE ANY TABLE,
               <GT_LOG_D> TYPE TABLE.

*&---------------------------------------------------------------------*
*&  GLOBAL VARIABLE DECLARATION
*&---------------------------------------------------------------------*
DATA:    OK_CODE TYPE SY-UCOMM.
DATA: GV_IF_TSP    TYPE TIMESTAMPL,
      GV_SEL_TSP   TYPE TIMESTAMPL,
      GV_LAST_DATE TYPE ZMMT0010-SELTSP,
      GV_IFZET     LIKE SY-UZEIT.

DATA: GV_IF_STATUS TYPE ZMMT0010-STATUS,
      GV_IF_MSG    TYPE ZMMT0010-MSG.
RANGES: GR_DATE FOR EKKO-LASTCHANGEDATETIME.

* API
DATA: GV_API_D TYPE STRING VALUE 'http://10.65.1.20:9500/api/v2/e_so_receive', "개발
      GV_API_Q TYPE STRING VALUE 'http://10.65.1.20:9000/api/v2/e_so_receive', "테스트
      GV_API_P TYPE STRING VALUE 'http://10.65.1.20:8000/api/v2/e_so_receive', "운영
      GV_API   TYPE STRING.

CONSTANTS: GC_CON_TYP TYPE STRING VALUE 'application/json; charset=UTF-8',
           GC_API_KEY TYPE STRING VALUE 'x-zinus-api-key',
           GC_API_VAL TYPE STRING VALUE '6b33645a-1064-495b-8a33-3ab45134a562'.
*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN.
*&---------------------------------------------------------------------*

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) FOR FIELD P_DATE MODIF ID Z01.
PARAMETERS: P_DATE TYPE DATS MODIF ID Z01 OBLIGATORY,
            P_TIME TYPE TIMS MODIF ID Z01 OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS: S_CDATE FOR EKKO-BEDAT MODIF ID Z02 OBLIGATORY,
                S_PLANT FOR T001W-WERKS.

SELECTION-SCREEN SKIP 1.
PARAMETERS : P_RESEND AS CHECKBOX USER-COMMAND UC1.
SELECT-OPTIONS : S_BSTKD FOR VBKD-BSTKD,
                 S_VBELN FOR VBKD-VBELN.

SELECTION-SCREEN SKIP 1.
PARAMETERS : P_LOG AS CHECKBOX USER-COMMAND UC1.
PARAMETERS : P_NOSENT AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK B0.

*********************************************************************
*** DEFINE
*********************************************************************
DEFINE _CLEAR.
  CLEAR : &1. CLEAR : &1[].
END-OF-DEFINITION.
DEFINE _RANGE.
  &1-SIGN   = &2.
  &1-OPTION = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  COLLECT &1. CLEAR &1.
END-OF-DEFINITION.
DEFINE _CONV_ISOLA_OUTPUT.
  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
    EXPORTING
      INPUT        = &1
   IMPORTING
     OUTPUT        = &2.
END-OF-DEFINITION.
DEFINE _STAMPL.
  CLEAR: &1.
  &1      = 'IBT'.
  &1-LOW  = &2.
  &1-HIGH = &3.
  APPEND &1.
END-OF-DEFINITION.
