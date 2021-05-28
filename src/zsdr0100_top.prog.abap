*&---------------------------------------------------------------------*
*& Include          ZSDR0100_TOP
*&---------------------------------------------------------------------*

*---tables
TABLES : VBRK.
TABLES : VBRP.

*---ALV
DATA : OK_CODE TYPE SY-UCOMM.
DATA : SAVE_OK LIKE SY-UCOMM.

DATA : GR_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER,
       GR_ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
       GT_FCAT              TYPE LVC_T_FCAT,
       GS_FCAT              TYPE LVC_S_FCAT.

DATA : GS_LAYOUT TYPE LVC_S_LAYO.
DATA : GS_VARIANT TYPE DISVARIANT.
DATA : GT_SORT TYPE LVC_T_SORT.
DATA : GT_EXCLUDE TYPE UI_FUNCTIONS.

DATA : GT_EXCLUDING TYPE UI_FUNCTIONS.
DATA : GS_EXCLUDING TYPE UI_FUNC.

DATA : GR_SPLITTER      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       GR_EASY_SPLITTER TYPE REF TO CL_GUI_EASY_SPLITTER_CONTAINER,
       GR_PARENT_HTML   TYPE REF TO CL_GUI_CONTAINER,
       GR_PARENT_GRID   TYPE REF TO CL_GUI_CONTAINER,
       GR_HTML          TYPE REF TO CL_GUI_HTML_VIEWER,
       GR_DOCUMENT      TYPE REF TO CL_DD_DOCUMENT.

DATA: ET_INDEX_ROWS TYPE LVC_T_ROW,
      ES_INDEX_ROWS TYPE LVC_S_ROW,
      ET_ROW_NO     TYPE LVC_T_ROID,
      ES_ROW_NO     TYPE LVC_S_ROID.

DATA : GS_ROW  TYPE LVC_S_ROW,
       GT_ROWS TYPE LVC_T_ROW.

DATA : GV_ANSWER TYPE C.
DATA : GV_UCOMM TYPE SY-UCOMM.
DATA : GS_FUNCTEXT TYPE SMP_DYNTXT.
DATA : GV_ERROR  TYPE C.

DATA: GT_EXCLU TYPE TABLE OF SY-UCOMM WITH HEADER LINE.

*---BDC
DATA: CTU_PARAMS LIKE CTU_PARAMS,
      GT_BDC     LIKE TABLE OF BDCDATA    WITH HEADER LINE,
      GT_MSG     LIKE TABLE OF BDCMSGCOLL WITH HEADER LINE.

*---IT

DATA : BEGIN OF GT_DATA OCCURS 0,
*Quot
         VBELN       TYPE VBAK-VBELN,      "Quotation
         POSNR       TYPE VBAP-POSNR,      "Item line no.
         MATNR       TYPE VBAP-MATNR,      "Material
         MAKTX       TYPE MAKT-MAKTX,      "material text
         VKAUS_Q     TYPE VBAP-VKAUS,      "Usage Indicator
         KWMENG      TYPE VBAP-KWMENG,     "Qty
         BSTNK       TYPE VBKD-BSTKD_E,    "PO#        vbak-bstnk,
         VRKME_Q     TYPE VBAP-VRKME,      "unit
         GBSTK       TYPE VBAK-GBSTK,      "status
         GBSTK_T     TYPE DD07V-DDTEXT,
         VDATU       TYPE VBAK-VDATU,      "Requested Delivery Date
         EDATU_Q(32),                      "Confirmed Schedule Line Date
         BMENG       TYPE VBEP-BMENG,      "Confirmed Quantity
         VTWEG       TYPE VBAK-VTWEG,      "Distribution Channel

*S/O
         VBELN_S     TYPE VBAK-VBELN,      "S/O #
         KUNNR       TYPE VBAK-KUNNR,      "Sold-to party
         KUNNR_T     TYPE KNA1-NAME1,
         KUNWE       TYPE VBAK-KUNNR,      "Ship-to party
         KUNWE_T     TYPE KNA1-NAME1,
         ORGSOLDTO   TYPE VBPA-KUNNR,      "org-sold to
         ORGS_T      TYPE KNA1-NAME1,
         BSTKD_E     TYPE VBKD-BSTKD_E,    "AZ PO#
         VTWEG_S     TYPE VBAK-VTWEG,      "Distribution Channel
         VKAUS       TYPE VBAP-VKAUS,      "Usage Indicator

         IHREZ       TYPE VBKD-IHREZ,      "HQ PO
         BSTKD       TYPE VBKD-BSTKD,      "AZ PO#

         POSNR_S     TYPE VBAP-POSNR,      "Item #
         MATNR_S     TYPE VBAP-MATNR,      "Material
         MAKTX_S     TYPE MAKT-MAKTX,      "material text
         WERKS       TYPE VBAP-WERKS,      "Plant
         LGORT       TYPE VBAP-LGORT,      "Storage location
         NETPR       TYPE VBAP-NETPR,      "Net Price
         KWMENG_S    TYPE VBAP-KWMENG,     "Qty
         VRKME       TYPE VBAP-VRKME,      "Sales Unit
         NETWR       TYPE VBAP-NETWR,      "Net Value

         WAERK       TYPE VBAP-WAERK,      "Currency
         LFGSA       TYPE VBAP-LFGSA,      "dl Status
         LFGSA_T     TYPE DD07V-DDTEXT,
         ABGRU       TYPE VBAP-ABGRU,      "Reason for Rejection
         ABGRU_T     TYPE TVAGT-BEZEI,

         VBELN_VL    TYPE LIPS-VBELN,      "Delivery
         POSNR_VL    TYPE LIPS-POSNR,      "Delivery Item
         LFIMG       TYPE LIPS-LFIMG,      "Qty delivered
         WBSTA       TYPE LIPS-WBSTA,      "Movement Status (GS)
         WBSTA_T     TYPE DD07V-DDTEXT,
         WADAT       TYPE LIKP-WADAT,      "Plan GI date
         MBDAT_D     TYPE LIPS-MBDAT,      "Mat.avail.date
         LFDAT       TYPE LIKP-LFDAT,      "Delivery date
         WADAT_IST   TYPE LIKP-WADAT_IST,  "Actual goods movement date
         EDATU       TYPE VBEP-EDATU,      "Schedule line date
         MBDAT       TYPE VBEP-MBDAT,      "Mat.avail.date
*--Shipment
         TKNUM       TYPE VTTK-TKNUM,      "Shipment #
         EXTI1       TYPE VTTK-EXTI1,      "BL No.
         STTRG       TYPE VTTK-STTRG,      "Overall transp. status
         STTRG_T     TYPE DD07V-DDTEXT,
         DTABF       TYPE VTTK-DTABF,      "Current date of shipment completion
*--Billing
         MENGE_GR    TYPE EKBE-MENGE,      "HQ G/R
         MENGE_IV    TYPE EKBE-MENGE,      "HQ Invoice
         VBELN_B     TYPE VBRK-VBELN,      "Billing doc
         FKDAT       TYPE VBRK-FKDAT,      "Biliing date
         POSNR_B     TYPE VBRP-POSNR,      "Billing item
*S_2021/3/4 add BY E00064
*--Document flow
         RFWRT       TYPE VBFA-RFWRT,      "Reference Value : Billing amount
         WAERS       TYPE VBFA-WAERS,      "Currency
*--Accounting Doc.
         VBELN_A     TYPE VBFA-VBELN,      "Subsequent Sales and Distribution Document
         WRBTR       TYPE EKBE-WRBTR,      "HQ Amount
         RET_ICON    TYPE ICON-ID,         "ICON
         RET_MSG     TYPE BAPI_MSG,        "Message
*E_2021/3/4 add BY E00064

       END OF GT_DATA.

DATA: GT_MAIN LIKE TABLE OF GT_DATA WITH HEADER LINE.
DATA: WA_MAIN LIKE          GT_DATA.

DATA : BEGIN OF GT_VBEP OCCURS 0,
         VBELN   TYPE VBAK-VBELN,      "Quotation
         POSNR   TYPE VBAP-POSNR,      "Item line no.
         EDATU_Q TYPE VBAK-VDATU,      "Schedule Line Date
         BMENG   TYPE VBEP-BMENG,      "Confirmed Quantity
       END OF GT_VBEP.

DATA: GV_COUNT TYPE I.

*---Smartform table
DATA: BEGIN OF GT_PRINT OCCURS 0,
        "Shipment
        TKNUM         TYPE VTTK-TKNUM,      "Shipment No.

        "Invocing Party
        NAME1         TYPE ADRC-NAME1,      "Name 1
        HOUSE_NUM1    TYPE ADRC-HOUSE_NUM1, "House Number1
        STREET        TYPE ADRC-STREET,     "Street Name
        POST_CODE1    TYPE ADRC-POST_CODE1, "Post code
        CITY1         TYPE ADRC-CITY1,      "City
        REGION        TYPE ADRC-REGION,     "Region
        BEZEI         TYPE T005U-BEZEI,     "Region text
        COUNTRY       TYPE ADRC-COUNTRY,    "Country

        "Billing No. and Date
        VBELN_B       TYPE VBRK-VBELN,      "Billing doc
        FKDAT         TYPE VBRK-FKDAT,      "Biliing date

        "Payment and Method
        ZTERM         TYPE VBRK-ZTERM,       "Payment Terms
        VTEXT         TYPE TVZBT-VTEXT,      "Description of terms of payment
        ZLSCH         TYPE VBRK-ZLSCH,       "Method
        TEXT1         TYPE T042Z-TEXT1,      "Method text

        "sold-to
        NAME1_S       TYPE ADRC-NAME1,      "Name 1
        STREET_S      TYPE ADRC-STREET,     "Street Name
        SUPPL1_S      TYPE ADRC-STR_SUPPL1, "Street 2
        SUPPL2_S      TYPE ADRC-STR_SUPPL2, "Street 3
        COUNTRY_S     TYPE ADRC-COUNTRY,    "Country key
        COUNTRY_NAME  TYPE T005T-LANDX,     "Country Name
        POST_CODE1_S  TYPE ADRC-POST_CODE1, "Postal Code

        "payer
        NAME1_P       TYPE ADRC-NAME1,      "Name 1
        STREET_P      TYPE ADRC-STREET,     "Street Name
        SUPPL1_P      TYPE ADRC-STR_SUPPL1, "Street 2
        SUPPL2_P      TYPE ADRC-STR_SUPPL2, "Street 3
        COUNTRY_P     TYPE ADRC-COUNTRY,    "Country key
        POST_CODE1_P  TYPE ADRC-POST_CODE1, "Postal Code

        "bill-to
        NAME1_B       TYPE ADRC-NAME1,      "Name 1
        STREET_B      TYPE ADRC-STREET,     "Street Name
        SUPPL1_B      TYPE ADRC-STR_SUPPL1, "Street 2
        SUPPL2_B      TYPE ADRC-STR_SUPPL2, "Street 3
        COUNTRY_B     TYPE ADRC-COUNTRY,    "Country key
        POST_CODE1_B  TYPE ADRC-POST_CODE1, "Postal Code

        "ship-to
        NAME1_SI      TYPE ADRC-NAME1,      "Name 1
        NAME4_SI      TYPE ADRC-NAME4,      "Name 4
        STREET_SI     TYPE ADRC-STREET,     "Street Name
        CITY1_SI      TYPE ADRC-CITY1,      "City
        REGION_SI     TYPE ADRC-REGION,     "Region
        POST_CODE1_SI TYPE ADRC-POST_CODE1, "Post_Code

        "Item
        POSNR_B       TYPE VBRP-POSNR,       "Billing item

        BSTKD_E       TYPE VBKD-BSTKD_E,     "PO#
        MATNR         TYPE MARA-MATNR,       "Material
        MAKTX         TYPE MAKT-MAKTX,       "Material desc.
        KWMENG        TYPE VBAP-KWMENG,      "Quantity
        VRKME         TYPE VBAP-VRKME,       "Unit
        WAERK         TYPE VBAP-WAERK,       "Currency
        NETPR         TYPE VBAP-NETPR,       "Unit Price
        NETWR         TYPE VBAP-NETWR,       "Amount

        VBELN_S       TYPE VBAK-VBELN,      "S/O #
        VBELN_VL      TYPE LIPS-VBELN,      "Delivery

        "Footer
        POSITION(30),                       "Position
        NAME(30),                           "Name
        EMAIL(30),                          "Email

        POSITION2(30),                      "Position
        NAME2(30),                          "Name
        EMAIL2(30),                         "Email
      END OF GT_PRINT.

DATA: BEGIN OF GT_ITEM OCCURS 0,
        TKNUM    TYPE VTTK-TKNUM,       "Shipment No.
        VBELN_S  TYPE VBAK-VBELN,       "S/O #
        VBELN_VL TYPE LIPS-VBELN,       "Delivery
        VBELN_B  TYPE VBRK-VBELN,       "Billing doc
        POSNR_B  TYPE VBRP-POSNR,       "Billing item

        BSTKD_E  TYPE VBKD-BSTKD_E,     "PO#
        MATNR    TYPE MARA-MATNR,       "Material
        MAKTX    TYPE MAKT-MAKTX,       "Material desc.
        KWMENG   TYPE VBAP-KWMENG,      "Quantity
        VRKME    TYPE VBAP-VRKME,       "Unit
        WAERK    TYPE VBAP-WAERK,       "Currency
        NETPR    TYPE VBAP-NETPR,       "Unit Price
        NETWR    TYPE VBAP-NETWR,       "Amount
      END OF GT_ITEM.

DATA: GS_OUTPUT_OPTION   TYPE SSFCOMPOP,
      GS_PRINT_OPTION    TYPE SSFCTRLOP,
      GS_JOB_OUTPUT_INFO TYPE SSFCRESCL,
      GS_FM_NAME         TYPE RS38L_FNAM,
      GV_DEVTYPE         TYPE RSPOPTYPE,
      GT_LINES           LIKE TABLE OF TLINE,
      GT_DOC             LIKE TABLE OF DOCS,
      GV_BIN_SIZE        TYPE I,             "Binary File Size
      GV_FILE_PATH       TYPE STRING.

"Send Mail
DATA: SENT_TO_ALL   TYPE OS_BOOLEAN,
      MAIN_TEXT     TYPE BCSY_TEXT,
      SEND_REQUEST  TYPE REF TO CL_BCS,
      DOCUMENT      TYPE REF TO CL_DOCUMENT_BCS,
      RECIPIENT     TYPE REF TO IF_RECIPIENT_BCS,
      BCS_EXCEPTION TYPE REF TO CX_BCS,
      MAILTO        TYPE AD_SMTPADR.

DATA : BEGIN OF GT_SMTP_RECV OCCURS 0,
         SMTP_ADDR LIKE ADR6-SMTP_ADDR, "email address
         CODE5     TYPE C,              "수신인, 참조인 구별자
       END OF GT_SMTP_RECV.

DATA: GT_BINARY_CONTENT TYPE SOLIX_TAB.
DATA: GV_BIN_FILESIZE TYPE SO_OBJ_LEN.
************************************************************************
* Constants
************************************************************************
CONSTANTS:
  C_SPACE             TYPE C        VALUE ' ',
  C_NON               TYPE C        VALUE '',
  C_LANGU_KR          TYPE C        VALUE '3',
  C_YES(1)            TYPE C        VALUE '1',
  C_NO(1)             TYPE C        VALUE '2',
  C_CANC(1)           TYPE C        VALUE 'A',
  C_BTN_TXT_YES(3)    TYPE C        VALUE 'YES',
  C_BTN_TXT_NO(3)     TYPE C        VALUE 'NO',
  C_FC01(4)           TYPE C        VALUE 'FC01',
  C_XLS(3)            TYPE C        VALUE 'C:/',
  C_BT(2)             TYPE C        VALUE 'BT',
  C_ICON_LED_INACTIVE TYPE C VALUE ICON_LED_INACTIVE,
  C_ICON_LED_RED      TYPE ICON_D   VALUE ICON_LED_RED,
  C_ICON_LED_YELLOW   TYPE ICON_D   VALUE ICON_LED_YELLOW,
  C_ICON_LED_GREEN    TYPE ICON_D   VALUE ICON_LED_GREEN,
  C_ICON_FAILURE      TYPE ICON_D   VALUE ICON_FAILURE,
  C_ICON_BEGIN        TYPE ICON_D   VALUE ICON_WF_WORKITEM_STARTED,
  C_0(1)              TYPE C        VALUE '0',
  C_1(1)              TYPE C        VALUE '1',
  C_2(1)              TYPE C        VALUE '2',
  C_3(1)              TYPE C        VALUE '3',
  C_4(1)              TYPE C        VALUE '4',
  C_5(1)              TYPE C        VALUE '5',
  C_6(1)              TYPE C        VALUE '6',
  C_7(1)              TYPE C        VALUE '7',
  C_8(1)              TYPE C        VALUE '8',
  C_9(1)              TYPE C        VALUE '9',
  C_00(2)             TYPE C        VALUE '00',
  C_01(2)             TYPE C        VALUE '01',
  C_02(2)             TYPE C        VALUE '02',
  C_03(2)             TYPE C        VALUE '03',
  C_04(2)             TYPE C        VALUE '04',
  C_05(2)             TYPE C        VALUE '05',
  C_06(2)             TYPE C        VALUE '06',
  C_07(2)             TYPE C        VALUE '07',
  C_08(2)             TYPE C        VALUE '08',
  C_09(2)             TYPE C        VALUE '09',
  C_10(2)             TYPE C        VALUE '10',
  C_11(2)             TYPE C        VALUE '11',
  C_12(2)             TYPE C        VALUE '12',
  C_13(2)             TYPE C        VALUE '13',
  C_14(2)             TYPE C        VALUE '14',
  C_15(2)             TYPE C        VALUE '15',
  C_16(2)             TYPE C        VALUE '16',
  C_17(2)             TYPE C        VALUE '17',
  C_18(2)             TYPE C        VALUE '18',
  C_19(2)             TYPE C        VALUE '19',
  C_20(2)             TYPE C        VALUE '20',
  C_21(2)             TYPE C        VALUE '21',
  C_23(2)             TYPE C        VALUE '23',
  C_24(2)             TYPE C        VALUE '24',
  C_25(2)             TYPE C        VALUE '25',
  C_30(2)             TYPE C        VALUE '30',
  C_31(2)             TYPE C        VALUE '31',
  C_33(2)             TYPE C        VALUE '33',
  C_35(2)             TYPE C        VALUE '35',
  C_37(2)             TYPE C        VALUE '37',
  C_40(2)             TYPE C        VALUE '40',
  C_47(2)             TYPE C        VALUE '47',
  C_49(2)             TYPE C        VALUE '49',
  C_50(2)             TYPE C        VALUE '50',
  C_60(2)             TYPE C        VALUE '60',
  C_62(2)             TYPE C        VALUE '62',
  C_63(2)             TYPE C        VALUE '63',
  C_68(2)             TYPE C        VALUE '68',
  C_69(2)             TYPE C        VALUE '69',
  C_70(2)             TYPE C        VALUE '70',
  C_71(2)             TYPE C        VALUE '71',
  C_72(2)             TYPE C        VALUE '72',
  C_73(2)             TYPE C        VALUE '73',
  C_74(2)             TYPE C        VALUE '74',
  C_75(2)             TYPE C        VALUE '75',
  C_76(2)             TYPE C        VALUE '76',
  C_80(2)             TYPE C        VALUE '80',
  C_90(2)             TYPE C        VALUE '90',
  C_95(2)             TYPE C        VALUE '95',
  C_99(2)             TYPE C        VALUE '99',
  C_000(3)            TYPE C        VALUE '000',
  C_001(3)            TYPE C        VALUE '001',
  C_002(3)            TYPE C        VALUE '002',
  C_003(3)            TYPE C        VALUE '003',
  C_004(3)            TYPE C        VALUE '004',
  C_100(3)            TYPE C        VALUE '100',
  C_110(3)            TYPE C        VALUE '110',
  C_120(3)            TYPE C        VALUE '120',
  C_2011(4)           TYPE C        VALUE '2011',
  C_A(1)              TYPE C        VALUE 'A',
  C_B(1)              TYPE C        VALUE 'B',
  C_C(1)              TYPE C        VALUE 'C',
  C_D(1)              TYPE C        VALUE 'D',
  C_E(1)              TYPE C        VALUE 'E',
  C_F(1)              TYPE C        VALUE 'F',
  C_G(1)              TYPE C        VALUE 'G',
  C_H(1)              TYPE C        VALUE 'H',
  C_I(1)              TYPE C        VALUE 'I',
  C_J(1)              TYPE C        VALUE 'J',
  C_K(1)              TYPE C        VALUE 'K',
  C_L(1)              TYPE C        VALUE 'L',
  C_M(1)              TYPE C        VALUE 'M',
  C_N(1)              TYPE C        VALUE 'N',
  C_O(1)              TYPE C        VALUE 'O',
  C_P(1)              TYPE C        VALUE 'P',
  C_Q(1)              TYPE C        VALUE 'Q',
  C_R(1)              TYPE C        VALUE 'R',
  C_S(1)              TYPE C        VALUE 'S',
  C_T(1)              TYPE C        VALUE 'T',
  C_U(1)              TYPE C        VALUE 'U',
  C_V(1)              TYPE C        VALUE 'V',
  C_W(1)              TYPE C        VALUE 'W',
  C_X(1)              TYPE C        VALUE 'X',
  C_Y(1)              TYPE C        VALUE 'Y',
  C_Z(1)              TYPE C        VALUE 'Z'.
