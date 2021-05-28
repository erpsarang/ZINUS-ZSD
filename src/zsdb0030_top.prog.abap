*&---------------------------------------------------------------------*
*& Include          ZSD1R0040_TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* TABLES
*-----------------------------------------------------------------------
TABLES : SSCRFIELDS.

*-----------------------------------------------------------------------
* RANGES
*-----------------------------------------------------------------------

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
          KUNNR TYPE KNMT-KUNNR,
          VKORG TYPE KNMT-VKORG,
          VTWEG TYPE KNMT-VTWEG,
          MATNR TYPE KNMT-MATNR,
          KDMAT TYPE KNMT-MATNR,
          CLASS TYPE RMCLF-CLASS,
          MWERT TYPE RCTMS-MWERT,
        END OF TY_EXCEL.

TYPES : BEGIN OF TY_DISP.
    INCLUDE TYPE TY_EXCEL.
TYPES : ICON         TYPE ICON-ID,
        MESSAGE(255) TYPE C,
        CHK          TYPE C.
TYPES : END OF TY_DISP.

TYPES : BEGIN OF TY_KNVV,
          KUNNR TYPE KNVV-KUNNR,
          VKORG TYPE KNVV-VKORG,
          VTWEG TYPE KNVV-VTWEG,
          SPART TYPE KNVV-SPART,
        END OF TY_KNVV.

TYPES : BEGIN OF TY_MVKE,
          MATNR TYPE MVKE-MATNR,
          VKORG TYPE MVKE-VKORG,
          VTWEG TYPE MVKE-VTWEG,
        END OF TY_MVKE.

*-----------------------------------------------------------------------
* Structure & Internal table
*-----------------------------------------------------------------------
DATA : GT_EXCEL TYPE TABLE OF TY_EXCEL WITH HEADER LINE,
       GT_DISP  TYPE TABLE OF TY_DISP WITH HEADER LINE.

DATA : GT_KNVV TYPE TABLE OF TY_KNVV WITH HEADER LINE,
       GT_MVKE TYPE TABLE OF TY_MVKE WITH HEADER LINE.

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
