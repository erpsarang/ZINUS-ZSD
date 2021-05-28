*&---------------------------------------------------------------------*
*& Include          ZSD0190_TOP
*&---------------------------------------------------------------------*
TABLES : LIKP, LIPS, VBAK, VBAP, VBKD, VBEP, SSCRFIELDS.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
"EXCEL UPLOAD
TYPES: BEGIN OF TY_XLS_VAL,
         ROW(6) TYPE N,
         COL(4) TYPE N,
         VALUE  TYPE TEXT100,
       END OF TY_XLS_VAL.

DATA : GT_EXCEL_TMP TYPE TY_XLS_VAL OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF GT_UPLOAD OCCURS 0,
         BSTKD_E  TYPE  VBKD-BSTKD_E,   "PO #
         LFDAT_EX TYPE  LIKP-LFDAT,     "Delivery Date
         ROUTE_EX TYPE  LIKP-ROUTE,     "Route
       END OF GT_UPLOAD.

"ALV DISPLAY
DATA: BEGIN OF GT_DISP OCCURS 0,
        ICON      TYPE ICON-ID,      "ICON
        MSG       TYPE CHAR200,      "MESSAGE
        BSTKD_E   TYPE VBKD-BSTKD_E, "PO#
        VBELN     TYPE VBAK-VBELN,   "S/O
        POSNR     TYPE VBAP-POSNR,   "Item no
        MATNR     TYPE VBAP-MATNR,   "SKU
        MAKTX     TYPE MAKT-MAKTX,   "Material Desc.
        MBDAT_S   TYPE VBEP-MBDAT,   "S/O Mat.avail.date
        VBELN_D   TYPE LIKP-VBELN,   "Delivery
        LFDAT     TYPE LIKP-LFDAT,   "Delivery date
        WADAT     TYPE LIKP-WADAT,   "Plan GI date
        MBDAT     TYPE LIPS-MBDAT,   "Mat.avail.date
        ROUTE     TYPE LIKP-ROUTE,   "Route
        LFDAT_EX  TYPE LIKP-LFDAT,   "Delivery date Excel Upload
        ROUTE_EX  TYPE LIKP-ROUTE,   "Route Excel Upload

        CELL_MODE TYPE C,            "CELL MODE
        COLOR     TYPE LVC_T_SCOL,   "COLOR
        STYLE     TYPE LVC_T_STYL,   "Row, Cell별 수정가능 여부
      END   OF GT_DISP.

DATA: LT_BDC LIKE TABLE OF GT_DISP WITH HEADER LINE.
*----------------------------------------------------------------------*
*  Global Variables
*----------------------------------------------------------------------*
DATA: OK_CODE   TYPE SY-UCOMM,
      SAVE_OK   TYPE SY-UCOMM,
      GV_TITLE  TYPE SY-TITLE,
      GV_ANSWER TYPE C,
      GV_MSG    TYPE STRING,
      GV_MODE   TYPE C,
      GV_CHANGE TYPE C,
      GV_COUNT  TYPE I,
      GV_S_CNT  TYPE I,
      GV_E_CNT  TYPE I,
      GV_POS    TYPE I.

DATA: GV_ERROR_S TYPE C,
      GV_ERROR_E TYPE C,
      GV_ERROR_D TYPE C.
*&---------------------------------------------------------------------*
*&  LAYOUT 화면 제어
*&---------------------------------------------------------------------*
* ALV 제어
DATA : GO_DOCKING   TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA : GO_SPLITTER  TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA : GO_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA : GO_ALV_GRID  TYPE REF TO CL_GUI_ALV_GRID.
DATA : GO_ALV_GRID1 TYPE REF TO CL_GUI_ALV_GRID.
DATA : GO_ALV_GRID2 TYPE REF TO CL_GUI_ALV_GRID.
DATA : GO_ALV_GRID3 TYPE REF TO CL_GUI_ALV_GRID.
DATA : GO_ALV_CON   TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_ALV_CON1  TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_ALV_CON2  TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_ALV_CON3  TYPE REF TO CL_GUI_CONTAINER.

* ALV 제어 옵션
DATA : GS_LAYOUT     TYPE LVC_S_LAYO.
DATA : GS_VARIANT    TYPE DISVARIANT.
DATA : GT_EXCLUDE    TYPE UI_FUNCTIONS.
DATA : GS_EXCLUDE    TYPE UI_FUNC.
DATA : GT_FIELDCAT   TYPE LVC_T_FCAT.
DATA : GT_FIELDCAT2  TYPE LVC_T_FCAT.
DATA : GS_FIELDCAT   TYPE LVC_S_FCAT.
DATA : GS_FIELDCAT2  TYPE LVC_S_FCAT.
DATA : GS_MODI       TYPE LVC_S_MODI.
DATA : GS_STBL       TYPE LVC_S_STBL.
DATA : GT_SORT       TYPE LVC_T_SORT.

DATA : GT_LVC_T_ROID TYPE LVC_T_ROID,
       GS_LVC_S_ROID TYPE LVC_S_ROID.

* TOP-OF-PAGE.
DATA: GO_DYNDOC_ID  TYPE REF TO CL_DD_DOCUMENT,
      GO_PARENT_TOP TYPE REF TO CL_GUI_CONTAINER,
      GO_HTML_CNTRL TYPE REF TO CL_GUI_HTML_VIEWER.

* SMARTFORM
DATA: GS_CONTROL_PARAMETERS LIKE  SSFCTRLOP,
      GS_OUTPUT_OPTIONS     TYPE  SSFCOMPOP,
      GS_JOB_OUTPUT_INFO    TYPE  SSFCRESCL,
      GV_NO_DIALOG.

* BDC
DATA: CTU_PARAMS LIKE CTU_PARAMS,
      GT_BDC     LIKE TABLE OF BDCDATA    WITH HEADER LINE,
      GT_MSG     LIKE TABLE OF BDCMSGCOLL WITH HEADER LINE.
