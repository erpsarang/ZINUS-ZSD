*&---------------------------------------------------------------------*
*& Include          ZSDR0020_TOP
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
TABLES: SSCRFIELDS.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
* Display
DATA: BEGIN OF GT_DISP OCCURS 0,
        MATNR     TYPE A305-MATNR,
        MAKTX     TYPE MAKT-MAKTX,
        KUNNR     TYPE A305-KUNNR,
        VTWEG     TYPE A305-VTWEG,
        NAME1     TYPE KNA1-NAME1,
        KUNWE     TYPE A903-KUNWE,
        KUNWE_TXT TYPE KNA1-NAME1,
        KSCHL     TYPE A305-KSCHL,
        KBETR     TYPE KONP-KBETR,
        KONWA     TYPE KONP-KONWA,
        KPEIN     TYPE KONP-KPEIN,
        KMEIN     TYPE KONP-KMEIN,
        DATAB     TYPE A305-DATAB,
        DATBI     TYPE A305-DATBI,
        LOEVM     TYPE KONP-LOEVM_KO,
      END OF GT_DISP.

DATA : GT_ZSDT0040 LIKE TABLE OF ZSDT0040 WITH HEADER LINE.
DATA : GV_LINES TYPE I.

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
DATA: GC_DOCKING       TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_200   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_300   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_400   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_500   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GC_DOCKING_600   TYPE REF TO CL_GUI_DOCKING_CONTAINER,

      GC_SPLITTER      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_200  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_300  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_400  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_500  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GC_SPLITTER_600  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

      GC_CONTAINER     TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_200 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_300 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_400 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_500 TYPE REF TO CL_GUI_CONTAINER,
      GC_CONTAINER_600 TYPE REF TO CL_GUI_CONTAINER,

      GC_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_200      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_300      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_400      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_500      TYPE REF TO CL_GUI_ALV_GRID,
      GC_GRID_600      TYPE REF TO CL_GUI_ALV_GRID.

*-- ALV
DATA:
  "VARIANT
  GS_VARIANT             TYPE DISVARIANT,
  GS_VARIANT_200         TYPE DISVARIANT,
  GS_VARIANT_300         TYPE DISVARIANT,
  GS_VARIANT_400         TYPE DISVARIANT,
  GS_VARIANT_500         TYPE DISVARIANT,
  GS_VARIANT_600         TYPE DISVARIANT,
  "LAYOUT
  GS_LAYOUT              TYPE LVC_S_LAYO,
  GS_LAYOUT_200          TYPE LVC_S_LAYO,
  GS_LAYOUT_300          TYPE LVC_S_LAYO,
  GS_LAYOUT_400          TYPE LVC_S_LAYO,
  GS_LAYOUT_500          TYPE LVC_S_LAYO,
  GS_LAYOUT_600          TYPE LVC_S_LAYO,
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
  "ROWS
  GS_ROWS                TYPE LVC_S_ROW,
  GT_ROWS                TYPE LVC_T_ROW,
  GT_ROWS_200            TYPE LVC_T_ROW,
  GT_ROWS_300            TYPE LVC_T_ROW,
  GT_ROWS_400            TYPE LVC_T_ROW,
  GT_ROWS_500            TYPE LVC_T_ROW,
  GT_ROWS_600            TYPE LVC_T_ROW,
  "SORT
  GS_SORT                TYPE LVC_S_SORT,
  GT_SORT                TYPE LVC_T_SORT,
  GT_SORT_200            TYPE LVC_T_SORT,
  GT_SORT_300            TYPE LVC_T_SORT,
  GT_SORT_400            TYPE LVC_T_SORT,
  GT_SORT_500            TYPE LVC_T_SORT,
  GT_SORT_600            TYPE LVC_T_SORT,
  "F4
  GS_F4                  TYPE LVC_S_F4,
  GT_F4                  TYPE LVC_T_F4,
  GT_F4_200              TYPE LVC_T_F4,
  GT_F4_300              TYPE LVC_T_F4,
  GT_F4_400              TYPE LVC_T_F4,
  GT_F4_500              TYPE LVC_T_F4,
  GT_F4_600              TYPE LVC_T_F4,
  "Filter
  GS_FILTER              TYPE LVC_S_FILT,
  GT_FILTER              TYPE LVC_T_FILT,
  GT_FILTER_200          TYPE LVC_T_FILT,
  GT_FILTER_300          TYPE LVC_T_FILT,
  GT_FILTER_400          TYPE LVC_T_FILT,
  GT_FILTER_500          TYPE LVC_T_FILT,
  GT_FILTER_600          TYPE LVC_T_FILT,
  "TOOLBAR
  GT_TOOLBAR_EXCLUDE     TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_200 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_300 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_400 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_500 TYPE UI_FUNCTIONS,
  GT_TOOLBAR_EXCLUDE_600 TYPE UI_FUNCTIONS,
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
  GS_STBL_600            TYPE LVC_S_STBL.

DATA: GV_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GV_SPLITTER          TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GV_CONT_ALV          TYPE REF TO CL_GUI_CONTAINER,
      GV_CONT_HTML         TYPE REF TO CL_GUI_CONTAINER.

DATA: GV_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: GV_DOCUMENT TYPE REF TO CL_DD_DOCUMENT,
      GV_HTML     TYPE REF TO CL_GUI_HTML_VIEWER.

DATA : GV_CHK_IC. "INTERCOMPANY 인지 확인
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
  APPEND &1. CLEAR &1.
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
