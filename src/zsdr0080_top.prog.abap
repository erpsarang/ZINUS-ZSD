*&---------------------------------------------------------------------*
*& Include          ZSDR0080_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON, SLIS.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
*INCLUDE: <cl_alv_control>.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: VBKD, VBAK, VBAP, VBUV, LIKP, LIPS,
        VBRK, VBRP, VBEP, ZSD1T0021, ZSD1T0072.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
* Alv
DATA: BEGIN OF GT_DISP OCCURS 0,
        KUNNR	        LIKE VBAK-KUNNR,      " Ship to-party
        NAME1	        LIKE KNA1-NAME1,      " Ship to-party name
        PRSDT         LIKE VBKD-PRSDT,
        KONDA         LIKE VBKD-KONDA,
        KONDA_TXT(40),
        EDATU         LIKE VBEP-EDATU,      " Delivery Date
        LDDAT         LIKE VBEP-LDDAT,      " Loading Planned
        AUDAT         LIKE VBAK-AUDAT,      " Customer Purchase Date
        ERDAT         LIKE VBAK-ERDAT,      " Order Creation Date
        ERZET         LIKE VBAK-ERZET,
        VDATU	        LIKE VBAK-VDATU,      " Customer Request Date
        DOCNUM        LIKE ZSD1T0021-DOCNUM,  " EDI Number
        ZIFTYPE       LIKE ZSD1T0021-ZIFTYPE, " EDI Type
        BSTKD	        LIKE VBKD-BSTKD,      " Customer PO
        POSEX	        LIKE VBAP-POSEX,      " Customer PO Item
        KDMAT	        LIKE VBAP-KDMAT,      " Customer SKU
        VBELN	        LIKE VBAP-VBELN,      " SO
        AUART         LIKE VBAK-AUART,      " Order Type
        AEZEI         LIKE TVAKT-BEZEI,     " Order Type Doc.
        VSBED         LIKE VBAK-VSBED,      " Shipping Conditions
        VTWEG         LIKE VBAK-VTWEG,
        KUNWE         LIKE VBPA-KUNNR,      " Ship to-party
        SAME1         LIKE ADRC-NAME1,      " Ship to-party  Name
        STREET        LIKE ADRC-STREET,     " Shipping Address
        POST_CODE1    LIKE ADRC-POST_CODE1, "
        CITY1         LIKE ADRC-CITY1,
        TEL_NUMBER    LIKE ADRC-TEL_NUMBER,
        REGION        LIKE ADRC-REGION,
        LIFNR         LIKE VBPA-LIFNR,      " Carrier
        LAME1         LIKE LFA1-NAME1,      " Carrier Name
        POSNR	        LIKE VBAP-POSNR,      " SO Item
        BWTAR	        LIKE VBAP-BWTAR,      " VAL TYPE
        ABGRU         LIKE VBAP-ABGRU,      " Reject
        BEZEI         LIKE TVAGT-BEZEI,     " Reject Dec.
        MATNR	        LIKE VBAP-MATNR,      " ZINUS SKU
        MAKTX         LIKE MAKT-MAKTX,
        PRODH_TXT     LIKE T179T-VTEXT,     "Product hierarchy text
        REMAIN_QTY    LIKE VBAP-KWMENG,     " REMAIN_QTY
        PR00          LIKE KONP-KBETR,      " PR00
        PR01          LIKE KONP-KBETR,      " PR01
        EDI1          LIKE KONP-KBETR,      " EDI1
        MWSI          LIKE KONP-KBETR,      " 부가세
        ZDRV          LIKE KONP-KBETR,      " 운송료
        ZDRX          LIKE KONP-KBETR,      " 설치비
        RC00          LIKE KONP-KBETR,      " RC00
        EDID          LIKE KONP-KBETR,      " EDID
        DIFF          LIKE KONP-KBETR,      " DIFF = PR00 - EDI1.
        CTAX          LIKE KONP-KBETR,      " CTAX
        NETWR         LIKE VBAP-NETWR,      " Net VAlue
        NETPR         LIKE VBAP-NETPR,      " Net price
        BETWR         LIKE VBRP-NETWR,      " Billing Net VAlue
        WAERK         LIKE VBAP-WAERK,      " Currency
        KWMENG        LIKE VBAP-KWMENG,     " SO ITEM QTY.
        VRKME	        LIKE VBAP-VRKME,      " Unit
        BRGEW         LIKE VBAP-BRGEW,      " Gross Weight of the Item
        NTGEW         LIKE VBAP-NTGEW,      " Net Weight of the Item
        GEWEI         LIKE VBAP-GEWEI,      " Weight Unit
        VOLUM         LIKE VBAP-VOLUM,      " Volume of the item
        VOLEH         LIKE VBAP-VOLEH,      " Volume unit
        PSTYV	        LIKE VBAP-PSTYV,      " Item Type
        VTEXT	        LIKE TVAPT-VTEXT,     " Item Type Dec.
        ICON_I        LIKE ICON-ID,         " Incomp.
        ICON_S        LIKE ICON-ID,         " SO Ack
        STATUS        TYPE ZEOSTST,         " SO Status
        DDTEXT        LIKE DD07V-DDTEXT,    " SO Status Dec.
        WERKS	        LIKE VBAP-WERKS,      " Waer House
        VBELV	        LIKE LIPS-VBELN,      " DO
        POSNV	        LIKE LIPS-POSNR,      " DO Item
        LFIMG         LIKE LIPS-LFIMG,      " DO Qty
        DO_VALUE      LIKE KONP-KBETR,      " DO Value  -> 2019.02.06
        DO_VOLUME     LIKE VBAP-VOLUM,      " DO Volume -> 03.06.2019
        DO_WEIGHT     LIKE VBAP-BRGEW,      " DO Weight -> 03.06.2019
        TKNUM	        LIKE VTTK-TKNUM,      " Shipment
        EXTI2         LIKE VTTK-EXTI2,      " "ARN No   -> Add 04/03/2019 request Mark by ZEN20
        MASTER_TR     LIKE VEKP-EXIDV2,     " Master Trac. No. Add 07/18/2019 request Sang by ZEN20
        ICON_T        LIKE ICON-ID,         " Tracking
        QDATU         LIKE LTAK-QDATU,      " Picked
        RFMNG         LIKE VBFA-RFMNG,      " Pick QTY
        DALEN         LIKE VTTK-DALEN,      " Loaded
        WADAT_IST     LIKE LIKP-WADAT_IST,  " GI
        ICON_P        LIKE ICON-ID,         " Transp
        PODAT         LIKE LIKP-PODAT,      " POD
        VBELF	        LIKE ICON-ID,         " Billing
        VBELB	        LIKE VBRP-VBELN,      " Billing
        POSNB         LIKE VBRP-POSNR,      " Billing Item
        MWSBP         LIKE VBRP-MWSBP,
        ZZKOINV       LIKE VBRK-ZZKOINV,    " INVOICE
        ZTERM         LIKE VBKD-ZTERM,
        ICON_D        LIKE ICON-ID,         " IV Send
        N_BLDAT       LIKE LIKP-BLDAT,      " Released.
        N_DTDIS       LIKE VTTK-DTDIS,      " Carrier Planned
        N_QDATU       LIKE LTAK-QDATU,      " Picked
        N_DALEN       LIKE VTTK-DALEN,      " Loaded
        N_WADAT       LIKE LIKP-WADAT_IST,  " Goods Issued
        N_PTIME       LIKE LIKP-WADAT_IST,  " Carrier First Scanned
        N_PODAT       LIKE LIKP-PODAT,      " POD
        N_FKDAT       LIKE VBRK-FKDAT,      " Invoiced
        LIFEX         LIKE LIKP-LIFEX,
        855           TYPE EDI_DOCNUM,
        855_S         LIKE ICON-ID,
        ASN           TYPE EDI_DOCNUM,
        ASN_S         LIKE ICON-ID,
        810           TYPE EDI_DOCNUM,
        810_S         LIKE ICON-ID,
        SALES_AMT     LIKE VBRP-NETWR,
        SALES_TOT     LIKE VBRP-NETWR,      " Sales Total
        SALES_DIFF    LIKE VBRP-NETWR,      " Sales Difference
        GD            TYPE SY-INDEX,
        GS            TYPE SY-INDEX,
        GA            TYPE SY-INDEX,
        FKDAT         LIKE VBRK-FKDAT,
      END OF GT_DISP.
* Sales Order
DATA: BEGIN OF GT_SALES OCCURS 0,
        BSTKD         LIKE VBKD-BSTKD,
        KONDA         LIKE VBKD-KONDA,
        PRSDT         LIKE VBKD-PRSDT,
        VTWEG         LIKE VBAK-VTWEG,
        KUNNR         LIKE VBAK-KUNNR,
        AUART         LIKE VBAK-AUART,      " Order Type
        AEZEI         LIKE TVAKT-BEZEI,     " Order Type Doc.
        VSBED         LIKE VBAK-VSBED,      " Shipping Conditions
        AUDAT         LIKE VBAK-AUDAT,      " Customer Purchase Date
        ERDAT         LIKE VBAK-ERDAT,      " Order Creation Date
        ERZET         LIKE VBAK-ERZET,
        VDATU         LIKE VBAK-VDATU,      " Req Delivery Date
        KNUMV         LIKE VBAK-KNUMV,
        POSEX         LIKE VBAP-POSEX,
        KDMAT         LIKE VBAP-KDMAT,
        VBELN         LIKE VBAP-VBELN,
        POSNR         LIKE VBAP-POSNR,
        BWTAR         LIKE VBAP-BWTAR,
        ABGRU         LIKE VBAP-ABGRU,
        BEZEI         LIKE TVAGT-BEZEI,
        MATNR         LIKE VBAP-MATNR,
        MAKTX         LIKE MAKT-MAKTX,
        WGBEZ60       LIKE T023T-WGBEZ60,
        PRODH_TXT     LIKE T179T-VTEXT,     "Product hierarchy text
        NETWR         LIKE VBAP-NETWR,      " Net VAlue
        NETPR         LIKE VBAP-NETPR,      " Net price
        WAERK         LIKE VBAP-WAERK,      " Currency
        KWMENG        LIKE VBAP-KWMENG,
        VRKME         LIKE VBAP-VRKME,
        BRGEW         LIKE VBAP-BRGEW,      " Gross Weight of the Item
        NTGEW         LIKE VBAP-NTGEW,      " Net Weight of the Item
        GEWEI         LIKE VBAP-GEWEI,      " Weight Unit
        VOLUM         LIKE VBAP-VOLUM,      " Volume of the item
        VOLEH         LIKE VBAP-VOLEH,      " Volume unit
        PSTYV         LIKE VBAP-PSTYV,
        WERKS         LIKE VBAP-WERKS,
        NAME1         LIKE KNA1-NAME1,
        ZTERM         LIKE VBKD-ZTERM,
        ZZSET_INFO    LIKE VBAP-ZZSET_INFO,  "Set item Info
        ZSET_MAT      LIKE VBAP-MATNR,       "Set sytem SKU
        FKDAT         LIKE VBRK-FKDAT,
        WADAT_IST     LIKE LIKP-WADAT_IST,
        KONDA_TXT(40),
      END OF GT_SALES.

DATA: BEGIN OF GS_DEL,
        VBELN LIKE VBUV-VBELN,
      END OF GS_DEL.
DATA : GT_DEL LIKE HASHED TABLE OF GS_DEL WITH UNIQUE KEY VBELN.

DATA: BEGIN OF GS_SH,
        VBELN      LIKE VBUV-VBELN,
        KUNWE      LIKE VBPA-KUNNR,  "
        SAME1      LIKE ADRC-NAME1,  " Shipping Name
        STREET     LIKE ADRC-STREET, " Shipping Address
        POST_CODE1 LIKE ADRC-POST_CODE1,
        REGION     LIKE ADRC-REGION,
        CITY1      LIKE ADRC-CITY1,
        TEL_NUMBER LIKE ADRC-TEL_NUMBER,
      END OF GS_SH.
DATA : GT_SH LIKE HASHED TABLE OF GS_SH WITH UNIQUE KEY VBELN.

DATA: BEGIN OF GS_SP,
        VBELN LIKE VBUV-VBELN,
        LIFNR LIKE VBPA-LIFNR,      " Carrier
        LAME1 LIKE LFA1-NAME1,      " Carrier Name
      END OF GS_SP.
DATA : GT_SP LIKE HASHED TABLE OF GS_SP WITH UNIQUE KEY VBELN.

DATA : BEGIN OF GT_REMAIN OCCURS 0,
         VGBEL LIKE LIPS-VGBEL,
         VGPOS LIKE LIPS-VGPOS,
         LFIMG LIKE LIPS-LFIMG,
       END OF GT_REMAIN.

* Sales Order Incomplete Header
DATA: BEGIN OF GS_VBUV_H,
        VBELN LIKE VBUV-VBELN,
        FDNAM LIKE VBUV-FDNAM,
      END OF GS_VBUV_H.
DATA : GT_VBUV_H LIKE HASHED TABLE OF GS_VBUV_H WITH UNIQUE KEY VBELN.
* Sales Order Incomplete
DATA: BEGIN OF GS_VBUV OCCURS 0,
        VBELN LIKE VBUV-VBELN,
        POSNR LIKE VBUV-POSNR,
        FDNAM LIKE VBUV-FDNAM,
      END OF GS_VBUV.
DATA : GT_VBUV LIKE HASHED TABLE OF GS_VBUV WITH UNIQUE KEY VBELN POSNR.
* Sales Order 생성 정보 - EDI
DATA: BEGIN OF GS_0021,
        VBELN   LIKE ZSD1T0021-VBELN,
        DOCNUM  LIKE ZSD1T0021-DOCNUM,
        ZIFTYPE LIKE ZSD1T0021-ZIFTYPE,
      END OF GS_0021.
DATA : GT_0021 LIKE HASHED TABLE OF GS_0021 WITH UNIQUE KEY VBELN.

* Sales Order Acknowledge - 855
DATA: BEGIN OF GS_0023,
        VBELN  LIKE ZSD1T0023-VBELN,
        DOCNUM LIKE ZSD1T0023-DOCNUM,
      END OF GS_0023.
DATA : GT_0023 LIKE HASHED TABLE OF GS_0023 WITH UNIQUE KEY VBELN.
* First Delivery Date
DATA: BEGIN OF GS_VBEP,
        VBELN LIKE VBEP-VBELN,
        POSNR LIKE VBEP-POSNR,
        ETENR LIKE VBEP-ETENR,
        EDATU LIKE VBEP-EDATU,
        LDDAT LIKE VBEP-LDDAT,
      END OF GS_VBEP.
DATA : GT_VBEP LIKE HASHED TABLE OF GS_VBEP WITH UNIQUE KEY VBELN POSNR.
* Change Sales Order Acknowledge - 865
DATA: BEGIN OF GS_0025,
        VBELN  LIKE ZSD1T0025-VBELN,
        POSNR  LIKE ZSD1T0025-POSNR,
        DOCNUM LIKE ZSD1T0025-DOCNUM,
        BSTKD  LIKE ZSD1T0025-BSTKD,
        POSEX  LIKE ZSD1T0025-POSEX,
      END OF GS_0025.
DATA : GT_0025 LIKE HASHED TABLE OF GS_0025 WITH UNIQUE KEY VBELN POSNR.
* Delivery Order
DATA: BEGIN OF GT_DELIV OCCURS 0,
        VGBEL     LIKE LIPS-VGBEL,
        VGPOS     LIKE LIPS-VGPOS,
        VBELN     LIKE LIPS-VBELN,
        POSNR     LIKE LIPS-POSNR,
        LFIMG     LIKE LIPS-LFIMG,
        WADAT_IST LIKE LIKP-WADAT_IST,
        PDSTK     LIKE LIKP-PDSTK,
        PODAT     LIKE LIKP-PODAT,
        BLDAT     LIKE LIKP-BLDAT,
        LIFEX     LIKE LIKP-LIFEX,
      END OF GT_DELIV.
* Shipment
DATA: BEGIN OF GS_TKNUM,
        VBELN LIKE LIPS-VBELN,
        TKNUM LIKE VTTK-TKNUM,
        DALEN LIKE VTTK-DALEN,
        ERDAT LIKE VTTK-ERDAT,
        EXTI2 LIKE VTTK-EXTI2,  "ARN No
      END OF GS_TKNUM.
DATA : GT_TKNUM LIKE HASHED TABLE OF GS_TKNUM WITH UNIQUE KEY VBELN.
* Tracking
DATA: BEGIN OF GS_VEKP,
        VBELN    LIKE LIPS-VBELN,
        VPOBJKEY LIKE VEKP-VPOBJKEY,
        EXIDV2   LIKE VEKP-EXIDV2,
        INHALT   LIKE VEKP-INHALT,
      END OF GS_VEKP.
DATA : GT_VEKP LIKE HASHED TABLE OF GS_VEKP WITH UNIQUE KEY VBELN.
* Tracking Status
DATA: BEGIN OF GT_0072 OCCURS 0,
        VBELN  LIKE ZSD1T0072-VBELN,
        INHALT LIKE ZSD1T0072-INHALT,
        ZNUM   LIKE ZSD1T0072-ZNUM,
        ZCODE  LIKE ZSD1T0072-ZCODE,
        ZDESC  LIKE ZSD1T0072-ZDESC,
        ZTIME  LIKE ZSD1T0072-ZTIME,
      END OF GT_0072.

DATA : BEGIN OF GS_SET_MAT,
         EAN11 LIKE MARA-EAN11,
         MATNR LIKE MARA-MATNR,
       END OF GS_SET_MAT.
DATA : GT_SET_MAT LIKE HASHED TABLE OF GS_SET_MAT WITH UNIQUE KEY EAN11.

DATA : BEGIN OF GS_VBSS,
         VBELN LIKE LIKP-VBELN,
         REFNR LIKE LTAK-REFNR,   "WAVE GROUP
         QDATU LIKE LTAK-QDATU,
       END OF GS_VBSS.
DATA : GT_VBSS LIKE HASHED TABLE OF GS_VBSS WITH UNIQUE KEY VBELN.
* Billing
DATA: BEGIN OF GT_BILLI OCCURS 0,
        VGBEL     LIKE VBRP-VGBEL,
        VGPOS     LIKE VBRP-VGPOS,
        AUBEL     LIKE VBRP-AUBEL,
        AUPOS     LIKE VBRP-AUPOS,
        VBELN     LIKE VBRP-VBELN,
        POSNR     LIKE VBRP-POSNR,
        NETWR     LIKE VBRP-NETWR,
        MATNR     LIKE VBRP-MATNR,
        ARKTX     LIKE VBRP-ARKTX,
        FKIMG     LIKE VBRP-FKIMG,
        VRKME     LIKE VBRP-VRKME,
        MWSBP     LIKE VBRP-MWSBP,
        KZWI1     LIKE VBRP-KZWI1,
        SFAKN     LIKE VBRK-SFAKN,
        FKSTO     LIKE VBRK-FKSTO,
        FKDAT     LIKE VBRK-FKDAT,
        WAERK     LIKE VBRK-WAERK,
        MEINS     LIKE VBRP-MEINS,
        ZZKOINV   LIKE VBRK-ZZKOINV,
        KUNRG     LIKE VBRK-KUNRG,
        NAME_ORG1 LIKE BUT000-NAME_ORG1,
      END OF GT_BILLI.

* Invoice Send
DATA: BEGIN OF GS_0024,
        VBELN  LIKE ZSD1T0024-VBELN,
        VBEVF  LIKE ZSD1T0024-VBEVF,
        DOCNUM LIKE ZSD1T0024-DOCNUM,
      END OF GS_0024.
DATA : GT_0024 LIKE HASHED TABLE OF GS_0024 WITH UNIQUE KEY VBELN.

DATA : GS_0024_CK LIKE GS_0024,
       GT_0024_CK LIKE HASHED TABLE OF GS_0024 WITH UNIQUE KEY VBELN.
* ACK Display
DATA: BEGIN OF GT_ACK OCCURS 0,
        DOCNUM  LIKE ZSD1T0023-DOCNUM,
        VBELN   LIKE ZSD1T0023-VBELN,
        TYPE(3) TYPE C,
      END OF GT_ACK.
* Invoice IDOC
DATA: BEGIN OF GT_IDOC OCCURS 0,
        DOCNUM LIKE ZSD1T0024-DOCNUM,
        VBELN  LIKE ZSD1T0024-VBELN,
        VBEVF  LIKE ZSD1T0024-VBEVF,
      END OF GT_IDOC.
* Invoice List
DATA: BEGIN OF GT_VBELF OCCURS 0,
        AUBEL LIKE VBRP-AUBEL,
        AUPOS LIKE VBRP-AUPOS,
        VGBEL LIKE VBRP-VGBEL,
        VGPOS LIKE VBRP-VGPOS,
        VBELN LIKE VBRP-VBELN,
        POSNR LIKE VBRP-POSNR,
        SFAKN LIKE VBRK-SFAKN,
        FKSTO LIKE VBRK-FKSTO,
      END OF GT_VBELF.
* Transportation List
DATA: BEGIN OF GT_STATUS OCCURS 0,
        VBELN  LIKE ZSD1T0072-VBELN,
        INHALT LIKE ZSD1T0072-INHALT,
        ZNUM   LIKE ZSD1T0072-ZNUM,
        ZCODE  LIKE ZSD1T0072-ZCODE,
        ZDESC  LIKE ZSD1T0072-ZDESC,
        ZTIME  LIKE ZSD1T0072-ZTIME,
      END OF GT_STATUS.
* Tracking List
DATA: BEGIN OF GT_TRACK OCCURS 0,
        VBELN  LIKE LIPS-VBELN,
        EXIDV2 LIKE VEKP-EXIDV2,
        INHALT LIKE VEKP-INHALT,
      END OF GT_TRACK.
* Tracking Info List
DATA: BEGIN OF GT_INFO OCCURS 0,
        BSTKD	 LIKE VBKD-BSTKD,          " Customer PO
        VBELN	 LIKE VBAP-VBELN,          " SO
        VBELV	 LIKE LIPS-VBELN,          " DO
        LIFNR  LIKE VBPA-LIFNR,          " Carrier
        LAME1  LIKE LFA1-NAME1,          " Carrier Name
        INHALT LIKE ZSD1T0072-INHALT,    " Tracking No
        FTIME  LIKE ZSD1T0072-ZTIME,     " Carrier First Scanned
        TTIME  LIKE ZSD1T0072-ZTIME,     " Carrier Delivered
      END OF GT_INFO.
* 가격
DATA: BEGIN OF GS_COND,
        KNUMV LIKE PRCD_ELEMENTS-KNUMV,
        KPOSN LIKE PRCD_ELEMENTS-KPOSN,
        KSCHL LIKE PRCD_ELEMENTS-KSCHL,
        KBETR LIKE PRCD_ELEMENTS-KBETR,
        WAERS LIKE PRCD_ELEMENTS-WAERS,
      END OF GS_COND.
DATA : GT_COND LIKE HASHED TABLE OF GS_COND WITH UNIQUE KEY KNUMV KPOSN KSCHL.

DATA : BEGIN OF GS_ADDR_V,
         ADRNR      TYPE ADRC-ADDRNUMBER,
         NAME1      TYPE ADRC-NAME1,
         STREET     TYPE ADRC-STREET,
         STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
         COUNTRY    TYPE ADRC-COUNTRY,
         STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
         CITY1      TYPE ADRC-CITY1,
         POST_CODE1 TYPE ADRC-POST_CODE1,
         REGION     TYPE ADRC-REGION,
         TEL_NUMBER TYPE ADRC-TEL_NUMBER,
         LANDX      TYPE T005T-LANDX,
       END OF GS_ADDR_V.
DATA : GT_ADDR_V LIKE HASHED TABLE OF GS_ADDR_V WITH UNIQUE KEY ADRNR.

DATA : BEGIN OF GS_ADDR_S,
         ADRNR      TYPE ADRC-ADDRNUMBER,
         NAME1      TYPE ADRC-NAME1,
         STREET     TYPE ADRC-STREET,
         STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
         COUNTRY    TYPE ADRC-COUNTRY,
         STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
         CITY1      TYPE ADRC-CITY1,
         POST_CODE1 TYPE ADRC-POST_CODE1,
         REGION     TYPE ADRC-REGION,
         TEL_NUMBER TYPE ADRC-TEL_NUMBER,
         LANDX      TYPE T005T-LANDX,
       END OF GS_ADDR_S.
DATA : GT_ADDR_S LIKE HASHED TABLE OF GS_ADDR_S WITH UNIQUE KEY ADRNR.

DATA : BEGIN OF GS_ADDR_SE,
         ADRN2      TYPE EKPO-ADRN2,
         NAME1      TYPE ADRC-NAME1,
         NAME2      TYPE ADRC-NAME2,
         STREET     TYPE ADRC-STREET,
         CITY1      TYPE ADRC-MC_CITY1,
         COUNTRY    TYPE ADRC-COUNTRY,
         REGION     TYPE ADRC-REGION,
         BEZEI      TYPE T005U-BEZEI,
         POST_CODE1 TYPE ADRC-POST_CODE1,
         TEL_NUMBER TYPE ADRC-TEL_NUMBER,
         LOCATION   TYPE ADRC-LOCATION,
         STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
         STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
         LANDX      TYPE T005T-LANDX,
       END OF GS_ADDR_SE.
DATA : GT_ADDR_SE LIKE HASHED TABLE OF GS_ADDR_SE WITH UNIQUE KEY ADRN2.

DATA : GV_LINES TYPE I.
DATA : GV_CHECK.

*>> ALPHA_INPUT
DEFINE _ALPHA_INPUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = &1
    IMPORTING
      OUTPUT = &2
    EXCEPTIONS
      OTHERS = 1.

END-OF-DEFINITION.

*>> ALPHA_OUTPUT
DEFINE _ALPHA_OUTPUT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = &1
    IMPORTING
      OUTPUT = &2
    EXCEPTIONS
      OTHERS = 1.

END-OF-DEFINITION.

* Smartforms
DATA : GS_HEAD LIKE ZSDS0070,
       GT_HEAD LIKE TABLE OF GS_HEAD,
       GT_ITEM LIKE TABLE OF ZSDS0071 WITH HEADER LINE.

DATA: GS_OUTPUT_OPTION   TYPE SSFCOMPOP,
      GS_PRINT_OPTION    TYPE SSFCTRLOP,
      GS_JOB_OUTPUT_INFO TYPE SSFCRESCL,
      GS_FM_NAME         TYPE RS38L_FNAM.
DATA: GV_FILE_PATH TYPE STRING.
*----------------------------------------------------------------------*
*  Global Variables
*----------------------------------------------------------------------*
DATA: GV_OK_CODE   TYPE SY-UCOMM,
      GV_UCOMM     TYPE SY-UCOMM,
      GV_POS       TYPE I,
      GV_ERROR(1)  TYPE C,
      GV_ANSWER(1) TYPE C,          " POP-UP Return Data
      GV_MSG       TYPE CHAR50.     " POP-UP Message
*----------------------------------------------------------------------*
* Class alv
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Class alv
*----------------------------------------------------------------------*
DATA: GC_DOCKING       TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_200   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_300   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_400   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_500   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_600   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_700   TYPE REF TO CL_GUI_DOCKING_CONTAINER,

      GC_SPLITTER      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_200  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_300  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_400  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_500  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_600  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_700  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

      GC_CONTAINER     TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_200 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_300 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_400 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_500 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_600 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_700 TYPE REF TO CL_GUI_CONTAINER,

      GC_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_200      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_300      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_400      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_500      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_600      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_700      TYPE REF TO CL_GUI_ALV_GRID.

*-- ALV
DATA:
  "VARIANT
  GS_VARIANT             TYPE DISVARIANT,
  GS_VARIANT_200         TYPE DISVARIANT,
  GS_VARIANT_300         TYPE DISVARIANT,
  GS_VARIANT_400         TYPE DISVARIANT,
  GS_VARIANT_500         TYPE DISVARIANT,
  GS_VARIANT_600         TYPE DISVARIANT,
  GS_VARIANT_700         TYPE DISVARIANT,
  "LAYOUT
  GS_LAYOUT              TYPE LVC_S_LAYO,
  GS_LAYOUT_200          TYPE LVC_S_LAYO,
  GS_LAYOUT_300          TYPE LVC_S_LAYO,
  GS_LAYOUT_400          TYPE LVC_S_LAYO,
  GS_LAYOUT_500          TYPE LVC_S_LAYO,
  GS_LAYOUT_600          TYPE LVC_S_LAYO,
  GS_LAYOUT_700          TYPE LVC_S_LAYO,
  "FIELDCATALOG
  GS_SLIS_FIELDCAT       TYPE SLIS_FIELDCAT_ALV,
  GT_SLIS_FIELDCAT       TYPE SLIS_T_FIELDCAT_ALV,
  GS_FIELDCAT            TYPE LVC_S_FCAT,

  GT_FIELDCAT            TYPE LVC_T_FCAT,
  GT_FIELDCAT_200        TYPE LVC_T_FCAT,
  GT_FIELDCAT_300        TYPE LVC_T_FCAT,
  GT_FIELDCAT_400        TYPE LVC_T_FCAT,
  GT_FIELDCAT_500        TYPE LVC_T_FCAT,
  GT_FIELDCAT_600        TYPE LVC_T_FCAT,
  GT_FIELDCAT_700        TYPE LVC_T_FCAT,
  "ROWS
  GS_ROWS                TYPE LVC_S_ROW,
  GT_ROWS                TYPE LVC_T_ROW,
  GT_ROWS_200            TYPE LVC_T_ROW,
  GT_ROWS_300            TYPE LVC_T_ROW,
  GT_ROWS_400            TYPE LVC_T_ROW,
  GT_ROWS_500            TYPE LVC_T_ROW,
  GT_ROWS_600            TYPE LVC_T_ROW,
  GT_ROWS_700            TYPE LVC_T_ROW,
  "SORT
  GS_SORT                TYPE LVC_S_SORT,
  GT_SORT                TYPE LVC_T_SORT,
  GT_SORT_200            TYPE LVC_T_SORT,
  GT_SORT_300            TYPE LVC_T_SORT,
  GT_SORT_400            TYPE LVC_T_SORT,
  GT_SORT_500            TYPE LVC_T_SORT,
  GT_SORT_600            TYPE LVC_T_SORT,
  GT_SORT_700            TYPE LVC_T_SORT,
  "F4
  GS_F4                  TYPE LVC_S_F4,
  GT_F4                  TYPE LVC_T_F4,
  GT_F4_200              TYPE LVC_T_F4,
  GT_F4_300              TYPE LVC_T_F4,
  GT_F4_400              TYPE LVC_T_F4,
  GT_F4_500              TYPE LVC_T_F4,
  GT_F4_600              TYPE LVC_T_F4,
  GT_F4_700              TYPE LVC_T_F4,
  "Filter
  GS_FILTER              TYPE LVC_S_FILT,
  GT_FILTER              TYPE LVC_T_FILT,
  GT_FILTER_200          TYPE LVC_T_FILT,
  GT_FILTER_300          TYPE LVC_T_FILT,
  GT_FILTER_400          TYPE LVC_T_FILT,
  GT_FILTER_500          TYPE LVC_T_FILT,
  GT_FILTER_600          TYPE LVC_T_FILT,
  GT_FILTER_700          TYPE LVC_T_FILT,
  "TOOLBAR
  GT_TOOLBAR_EXCLUDE     TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_200 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_300 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_400 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_500 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_600 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_700 TYPE UI_FUNCTIONS,
  "CELL COLOR
  GS_COLOR               TYPE LVC_S_SCOL,
  GT_COLOR               TYPE LVC_T_SCOL,
  "CELL STYLE
  GS_STYLE               TYPE LVC_S_STYL,
  GT_STYLE               TYPE LVC_T_STYL,
  "Fix lines and columns at refresh
  GS_STBL                TYPE LVC_S_STBL,
  GS_STBL_200            TYPE LVC_S_STBL,
  GS_STBL_300            TYPE LVC_S_STBL,
  GS_STBL_400            TYPE LVC_S_STBL,
  GS_STBL_500            TYPE LVC_S_STBL,
  GS_STBL_600            TYPE LVC_S_STBL,
  GS_STBL_700            TYPE LVC_S_STBL.

DATA: GV_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GV_SPLITTER          TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GV_CONT_ALV          TYPE REF TO CL_GUI_CONTAINER,
      GV_CONT_HTML         TYPE REF TO CL_GUI_CONTAINER.

DATA: GV_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: GV_DOCUMENT TYPE REF TO CL_DD_DOCUMENT,
      GV_HTML     TYPE REF TO CL_GUI_HTML_VIEWER.
*----------------------------------------------------------------------*
*  CONSTANT
*----------------------------------------------------------------------*
CONSTANTS: GC_I_NEW(4)  VALUE '@BZ@',  "ICON_LED_INACTIVE
           GC_I_YEL(4)  VALUE '@5D@',  "ICON_LED_YELLOW
           GC_I_GRE(4)  VALUE '@5B@',  "ICON_LED_GREEN
           GC_I_RED(4)  VALUE '@5C@',  "ICON_LED_RED
           GC_I_LIST(4) VALUE '@3W@'. "ICON_LIST
*----------------------------------------------------------------------*
*  MACOR
*----------------------------------------------------------------------*
*-- CLEAR 구문 MACOR
DEFINE _CLEAR.
  CLEAR: &1, &1[].
END-OF-DEFINITION.
*-- APPEND 구문 MACOR
DEFINE _APPEND.
  APPEND &1. CLEAR &1.
END-OF-DEFINITION.
*-- RANGES 구문 MACOR
DEFINE _RANGES.
  &1-sign     = &2.
  &1-option   = &3.
  &1-low      = &4.
  &1-high     = &5.
  APPEND &1.
END-OF-DEFINITION.
*-- Tollbar에서 제거할 버튼지정 MACOR
DEFINE SET_TOOLBAR_0100.
  APPEND &1 TO gt_toolbar_exclude.
END-OF-DEFINITION.

DEFINE SET_TOOLBAR_0200.
  APPEND &1 TO gt_toolbar_exclude_200.
END-OF-DEFINITION.

DEFINE SET_TOOLBAR_0300.
  APPEND &1 TO gt_toolbar_exclude_300.
END-OF-DEFINITION.

DEFINE SET_TOOLBAR_0400.
  APPEND &1 TO gt_toolbar_exclude_400.
END-OF-DEFINITION.

DEFINE SET_TOOLBAR_0500.
  APPEND &1 TO gt_toolbar_exclude_500.
END-OF-DEFINITION.

DEFINE SET_TOOLBAR_0600.
  APPEND &1 TO gt_toolbar_exclude_600.
END-OF-DEFINITION.

DEFINE SET_TOOLBAR_0700.
  APPEND &1 TO gt_toolbar_exclude_700.
END-OF-DEFINITION.
