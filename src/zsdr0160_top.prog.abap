*&---------------------------------------------------------------------*
*& Include          ZSDR0160_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* TABLES
*-----------------------------------------------------------------------
TABLES : SSCRFIELDS.
TABLES: KNMT.

*-----------------------------------------------------------------------
* RANGES
*-----------------------------------------------------------------------
RANGES : R_BSTNK FOR VBAK-BSTNK,
         R_XBLNR FOR VBRK-XBLNR.

*-----------------------------------------------------------------------
* CONSTANTS
*-----------------------------------------------------------------------
CONSTANTS : C_A TYPE C VALUE 'A',
            C_B TYPE C VALUE 'B',
            C_C TYPE C VALUE 'C',
            C_D TYPE C VALUE 'D',
            C_E TYPE C VALUE 'E',
            C_F TYPE C VALUE 'F',
            C_G TYPE C VALUE 'G',
            C_H TYPE C VALUE 'H',
            C_I TYPE C VALUE 'I',
            C_J TYPE C VALUE 'J',
            C_K TYPE C VALUE 'K',
            C_L TYPE C VALUE 'L',
            C_M TYPE C VALUE 'M',
            C_N TYPE C VALUE 'N',
            C_O TYPE C VALUE 'O',
            C_P TYPE C VALUE 'P',
            C_Q TYPE C VALUE 'Q',
            C_R TYPE C VALUE 'R',
            C_S TYPE C VALUE 'S',
            C_T TYPE C VALUE 'T',
            C_U TYPE C VALUE 'U',
            C_V TYPE C VALUE 'V',
            C_W TYPE C VALUE 'W',
            C_X TYPE C VALUE 'X',
            C_Y TYPE C VALUE 'Y',
            C_Z TYPE C VALUE 'Z'.

*---EXCEL UPLOAD
CONSTANTS : GV_SOI_DOCTYPE_WORD97_DOCUMENT(15) TYPE C
                                               VALUE  'Word.Document.8'.

*-----------------------------------------------------------------------
* ALV Declaration
*-----------------------------------------------------------------------
DATA : GCL_CONTROL_NAME1  TYPE SCRFNAME VALUE 'G_CON1',
       GCL_CONTAINER1     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GCL_DOCKING1       TYPE REF TO CL_GUI_DOCKING_CONTAINER,
       GCL_DOCKING2       TYPE REF TO CL_GUI_DOCKING_CONTAINER,
       GCL_SPLITTER       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       GCL_TOP_CONTAINER  TYPE REF TO CL_GUI_CONTAINER,
       GCL_TOP_CONTAINER2 TYPE REF TO CL_GUI_CONTAINER,
       GCL_ALV_CONTAINER1 TYPE REF TO CL_GUI_CONTAINER,
       GCL_ALV_CONTAINER2 TYPE REF TO CL_GUI_CONTAINER,
       GCL_DYNDOC         TYPE REF TO CL_DD_DOCUMENT.

DATA : LR_DYNDOC  TYPE REF TO CL_DD_DOCUMENT,
       LR_DYNDOC2 TYPE REF TO CL_DD_DOCUMENT.

CLASS : LCL_ALV_GRID DEFINITION DEFERRED,
  LCL_RECEIVER DEFINITION DEFERRED.

* Define ALV Data
DATA : GCL_ALV1     TYPE REF TO LCL_ALV_GRID,
       GCL_ALV2     TYPE REF TO LCL_ALV_GRID,
       GCL_RECEIVER TYPE REF TO LCL_RECEIVER,
       GT_FIELDCAT  TYPE SLIS_T_FIELDCAT_ALV,
       GT_FCAT1     TYPE LVC_T_FCAT, " WITH HEADER LINE,
       GT_FCAT2     TYPE LVC_T_FCAT, " WITH HEADER LINE,
       GT_SORT1     TYPE LVC_T_SORT, " WITH HEADER LINE,
       GT_SORT2     TYPE LVC_T_SORT, " WITH HEADER LINE,
       GT_SORT3     TYPE LVC_T_SORT, " WITH HEADER LINE,
       GT_TOOLBAR1  TYPE UI_FUNCTIONS WITH HEADER LINE,
       GT_TOOLBAR2  TYPE UI_FUNCTIONS WITH HEADER LINE,
       GS_EXCLUDE   TYPE UI_FUNC,
       GS_FCAT1     TYPE LVC_S_FCAT,
       GS_FCAT2     TYPE LVC_S_FCAT,
       GS_LAYO1     TYPE LVC_S_LAYO,
       GS_LAYO2     TYPE LVC_S_LAYO,
       GS_STABLE    TYPE LVC_S_STBL VALUE 'XX',
       GS_VARIANT   TYPE DISVARIANT.

DATA : GT_ROW_NO    TYPE LVC_T_ROID WITH HEADER LINE,
       GT_ROW_TABLE TYPE LVC_T_ROW  WITH HEADER LINE. "selected rows

DATA : IT_STYLE_CELLS TYPE LVC_T_ROID WITH HEADER LINE,
       GT_CELLS       TYPE LVC_T_ROID WITH HEADER LINE,
       GT_BAD_CELLS   TYPE LVC_T_ROID WITH HEADER LINE,
       GT_CONVERSION  TYPE LVC_T_ROID WITH HEADER LINE.

DATA : GT_FIELDCAT1    TYPE SLIS_T_FIELDCAT_ALV,
       GT_LVC_FIELDCAT TYPE LVC_T_FCAT.

** 필드 카달로그 *******************************************************
FIELD-SYMBOLS: <FS_S_FCAT> TYPE LVC_S_FCAT.

FIELD-SYMBOLS : <FS>, <FS2>.

*-----------------------------------------------------------------------
* TYPE
*-----------------------------------------------------------------------
TYPES : BEGIN OF TY_EXCEL,
          AUART     TYPE VBAK-AUART,     " Order Type
          VKORG     TYPE VBAK-VKORG,     " Sales Org
          VTWEG     TYPE VBAK-VTWEG,     " Distribution Channel
          SPART     TYPE VBAK-SPART,     " Division
          KUNNR     TYPE VBAK-KUNNR,     " SoldTO
          KUNNR1    TYPE VBAK-KUNNR,     " ShipTo
          KUNNRO    TYPE VBAK-KUNNR,     " Origin SoldTo
          BSTKD_E   TYPE VBKD-BSTKD_E,   " Origin PO#
          IHREZ     TYPE VBKD-IHREZ,     " ZINUS USA PO#
          BSTKD     TYPE VBKD-BSTKD,     " HQ PO#

          VDATU     TYPE VBAK-VDATU,     " RDD : Requested Delivery Date
          WERKS     TYPE VBAP-WERKS,     " plant
          LGORT     TYPE VBAP-LGORT,     " stor.loc
          MATNR     TYPE VBAP-MATNR,     " SKU
          KWMENG(15) TYPE C,             " QTY
          MEINS     TYPE VBAP-MEINS,     " UOM VBAP-VRKME

          POSEX_E   TYPE VBKD-POSEX_E,   " Origin PO# Item No

          ZZISAID   TYPE VBAK-ZZISAID,   "ISA-ID
          ZZINTVN   TYPE VBAK-ZZINTVN,   "Internal Vendor Number
          ZZSCAC    TYPE VBAK-ZZSCAC,    "SCAC
          ZZINTCN   TYPE VBAK-ZZINTCN,   "Internal Control Number
          ZZSHRO    TYPE VBAK-ZZSHRO,    "SHP ROUTING
          ZZSHLE    TYPE VBAK-ZZSHLE,    "SHP SVC LEVEL
          ZZSSNO    TYPE VBAK-ZZSSNO,    "SHP SEAL NUM
          ZZVENDID  TYPE VBAK-ZZVENDID,  "CUST VENDOR ID
          ZZWHCODE  TYPE VBAK-ZZWHCODE,  "WH CODE
          ZZSRDFR   TYPE VBAK-ZZSRDFR,   "Expected date
          ZZSRDTO   TYPE VBAK-ZZSRDTO,   "Delivery by Date
          ZZSVCAT   TYPE VBAK-ZZSVCAT,   "Service catalog
          ZZSCACC   TYPE VBAK-ZZSCACC,   "SCAC Code

        END OF TY_EXCEL.


TYPES : BEGIN OF TY_DISP.
    INCLUDE TYPE TY_EXCEL.
TYPES : "ICON         TYPE ICON-ID,
        ICON1        TYPE ICON-ID,
*        ICON2        TYPE ICON-ID,
        KVGR1        LIKE KNVV-KVGR1,  " customer group
        VKAUS        TYPE VBAP-VKAUS,  " Usage
        BEZEI        TYPE TVLVT-BEZEI, " Usage Description
        KDMAT        TYPE KNMT-KDMAT,  " customer SKU
        VBELN_S      TYPE VBAK-VBELN,
        POSNR        TYPE VBAP-POSNR,
*        VBELN_B      TYPE VBRK-VBELN,
        MESSAGE(255) TYPE C,
        CHK          TYPE C.
TYPES : END OF TY_DISP.

TYPES : BEGIN OF TY_VBRP,
          VBELN TYPE VBRK-VBELN,
          POSNR TYPE VBRP-POSNR,
          ZUONR TYPE VBRK-ZUONR,
          VKORG TYPE VBRK-VKORG,
          VTWEG TYPE VBRK-VTWEG,
          KUNAG TYPE VBRK-KUNAG,
          KNUMV TYPE VBRK-KNUMV,
          MATNR TYPE VBRP-MATNR,
          WERKS TYPE VBRP-WERKS,
        END OF TY_VBRP.

TYPES : BEGIN OF TY_VBAK,
          VBELN TYPE VBAK-VBELN,
          BSTNK TYPE VBAK-BSTNK,
        END OF TY_VBAK.

TYPES : BEGIN OF TY_VBRK,
          VBELN TYPE VBRK-VBELN,
          XBLNR TYPE VBRK-XBLNR,
        END OF TY_VBRK.

TYPES : BEGIN OF TY_VBFA,
          RUUID TYPE VBFA-RUUID,
          VBELN TYPE VBFA-VBELN,
          VBELV TYPE VBFA-VBELV,
        END OF TY_VBFA.

TYPES : BEGIN OF TY_VBPA,
          VBELN TYPE VBPA-VBELN,
          POSNR TYPE VBPA-POSNR,
          PARVW TYPE VBPA-PARVW,
          KUNNR TYPE VBPA-KUNNR,
        END OF TY_VBPA.

*-----------------------------------------------------------------------
* Structure & Internal table
*-----------------------------------------------------------------------
DATA : GT_EXCEL TYPE TABLE OF TY_EXCEL WITH HEADER LINE,
       GT_DISP  TYPE TABLE OF TY_DISP WITH HEADER LINE.
DATA : GS_DISP LIKE GT_DISP.

DATA : GT_VBRP TYPE TABLE OF TY_VBRP WITH HEADER LINE,
       GT_VBAK TYPE TABLE OF TY_VBAK WITH HEADER LINE,
       GT_VBRK TYPE TABLE OF TY_VBRK WITH HEADER LINE,
       GT_VBFA TYPE TABLE OF TY_VBFA WITH HEADER LINE,
       GT_VBPA TYPE TABLE OF TY_VBPA WITH HEADER LINE,
       GT_HEAD TYPE TABLE OF TY_DISP WITH HEADER LINE,
       GT_SKU_C TYPE TABLE OF TY_DISP WITH HEADER LINE, " CUSTOMER SKU
       GT_USAGE TYPE TABLE OF TY_DISP WITH HEADER LINE. " Usage

DATA : GT_TVLVT LIKE TABLE OF TVLVT WITH HEADER LINE.

*-----------------------------------------------------------------------
* Global Variable
*-----------------------------------------------------------------------
DATA : GV_CNT      TYPE I,
       GV_SCNT     TYPE I,
       GV_ECNT     TYPE I,
       GV_TABIX    TYPE SY-TABIX,
       GV_MSG(255) TYPE C,
       GV_ANSWER   TYPE C,
       GV_ERR      TYPE C,
       GV_POPUP    TYPE C,
       GV_ERDAT    TYPE SY-DATUM,
       GV_ERZET    TYPE SY-UZEIT,
       GV_ERNAM    TYPE SY-UNAME,
       GV_VAL      TYPE I.

** SCREEN 제어용
DATA: OK_CODE   LIKE RSMPE-FUNC,
      G_OK_CODE LIKE RSMPE-FUNC.

*-----------------------------------------------------------------------
* BDC
*-----------------------------------------------------------------------
DATA : GT_BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
       GT_BDCMSG  TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE.
DATA : GS_CTU_PARAMS TYPE CTU_PARAMS.
DATA : G_BDCMODE(1),
       G_UMODE VALUE 'S'.

*-----------------------------------------------------------------------
* Exclude Button
*-----------------------------------------------------------------------
DATA : BEGIN OF GT_EXCL OCCURS 50,
         FUNC LIKE RSMPE-FUNC,
       END   OF GT_EXCL.

*-----------------------------------------------------------------------
* SELECTION-SCREEN
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_FILE TYPE RLGRAP-FILENAME.
SELECTION-SCREEN END   OF BLOCK B1.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
