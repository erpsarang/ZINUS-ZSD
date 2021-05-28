*&---------------------------------------------------------------------*
*& Include          ZSDB0010C01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS LCL_ALV_GRID DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID DEFINITION INHERITING FROM CL_GUI_ALV_GRID.
  PUBLIC SECTION.

    METHODS: HANDLE_DATA_CHANGED
                  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
      IMPORTING ER_DATA_CHANGED.

    METHODS: HANDLE_DATA_CHANGED_FINISHED
                  FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
      IMPORTING E_MODIFIED ET_GOOD_CELLS.

    METHODS: HANDLE_HOTSPOT_CLICK
                  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID E_COLUMN_ID.

    METHODS: HANDLE_DOUBLE_CLICK
                  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW E_COLUMN.

    METHODS: HANDLE_TOP_OF_PAGE
                  FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
      IMPORTING E_DYNDOC_ID.

    METHODS: HANDLE_USER_COMMAND
                  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM.

    METHODS: HANDLE_TOOLBAR
                  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
      IMPORTING E_OBJECT E_INTERACTIVE.

    METHODS: ON_F4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
      IMPORTING SENDER
                  E_FIELDNAME
                  E_FIELDVALUE
                  ES_ROW_NO
                  ER_EVENT_DATA
                  ET_BAD_CELLS
                  E_DISPLAY.

    TYPES: DDSHRETVAL_TABLE TYPE TABLE OF DDSHRETVAL.

    METHODS: MY_F4
      IMPORTING SENDER        TYPE REF TO CL_GUI_ALV_GRID
                ET_BAD_CELLS  TYPE LVC_T_MODI
                ES_ROW_NO     TYPE LVC_S_ROID
                ER_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA
                E_DISPLAY     TYPE C
                E_FIELDNAME   TYPE LVC_FNAME
                E_LINE        TYPE C
      EXPORTING LT_F4         TYPE DDSHRETVAL_TABLE.


ENDCLASS. "LCL_EVENT_RECEIVER

*---------------------------------------------------------------------*
* CLASS LCL_AlV_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_ALV_GRID IMPLEMENTATION.
  METHOD HANDLE_DATA_CHANGED.
    PERFORM HANDLE_DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD. "HANDLE_DATA_CHANGED

  METHOD HANDLE_DATA_CHANGED_FINISHED.
*    PERFORM HANDLE_DATA_CHANGED_FINISHED USING E_MODIFIED ET_GOOD_CELLS.
  ENDMETHOD. "HANDLE_DATA_CHANGED

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID.
  ENDMETHOD. "HANDLE_HOTSPOT_CLICK

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK USING E_ROW E_COLUMN.
  ENDMETHOD. "HANDLE_DOUBLE_CLICK

  METHOD HANDLE_TOP_OF_PAGE.
    PERFORM HANDLE_TOP_OF_PAGE USING E_DYNDOC_ID.
  ENDMETHOD. "HANDLE_TOP_OF_PAGE

  METHOD HANDLE_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND USING E_UCOMM.
  ENDMETHOD. "HANDLE_USER_COMMAND

  METHOD HANDLE_TOOLBAR.
    PERFORM HANDLE_TOOLBAR USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD. "HANDLE_TOOLBAR

  METHOD ON_F4.
*    PERFORM ON_F4 USING SENDER
*                        E_FIELDNAME
*                        E_FIELDVALUE
*                        ES_ROW_NO
*                        ER_EVENT_DATA
*                        ET_BAD_CELLS
*                        E_DISPLAY.
  ENDMETHOD.                                                "ON_F4

  METHOD MY_F4.
*    PERFORM MY_F4 TABLES LT_F4
*                   USING SENDER
*                         ET_BAD_CELLS
*                         ES_ROW_NO
*                         ER_EVENT_DATA
*                         E_DISPLAY
*                         E_FIELDNAME.
  ENDMETHOD.                                                "MY_F4

ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*  ALV TREE
*----------------------------------------------------------------------*
DATA: GO_TREE  TYPE REF TO CL_GUI_ALV_TREE,
      GO_STREE TYPE REF TO CL_GUI_SIMPLE_TREE,
      GO_CTREE TYPE REF TO CL_GUI_COLUMN_TREE,
      GT_LAYI  TYPE LVC_T_LAYI,
      GS_LAYN  LIKE LVC_S_LAYN.

*----------------------------------------------------------------------*
* ALV HEADER
*----------------------------------------------------------------------*
DATA: GO_HEADER    TYPE REF TO CL_GUI_HTML_VIEWER,
      GO_DOCUMENT  TYPE REF TO CL_DD_DOCUMENT,
      GO_DOCUMENT1 TYPE REF TO CL_DD_DOCUMENT,
      GO_DOCUMENT2 TYPE REF TO CL_DD_DOCUMENT.
DATA: GO_DOC_AREA  TYPE REF TO CL_DD_AREA,
      GO_DOC_AREA1 TYPE REF TO CL_DD_AREA,
      GO_DOC_AREA2 TYPE REF TO CL_DD_AREA.

*----------------------------------------------------------------------*
* ALV GRID
*----------------------------------------------------------------------*
DATA: GO_GRID  TYPE REF TO LCL_ALV_GRID,
      GO_GRID1 TYPE REF TO CL_GUI_ALV_GRID,
      GO_GRID2 TYPE REF TO CL_GUI_ALV_GRID,
      GO_GRID3 TYPE REF TO CL_GUI_ALV_GRID.

*----------------------------------------------------------------------*
* ALV CONTAINER
*----------------------------------------------------------------------*
DATA: GO_CON  TYPE REF TO CL_GUI_CONTAINER,
      GO_CON1 TYPE REF TO CL_GUI_CONTAINER,
      GO_CON2 TYPE REF TO CL_GUI_CONTAINER,
      GO_CON3 TYPE REF TO CL_GUI_CONTAINER.
DATA: GO_CUSTOM  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GO_CUSTOM1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GO_CUSTOM2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GO_CUSTOM3 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA: GO_DOCKING  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GO_DOCKING1 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GO_DOCKING2 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GO_DOCKING3 TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA: GO_SPLITTER  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GO_SPLITTER1 TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GO_SPLITTER2 TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GO_SPLITTER3 TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA: G_PARENT_HTML TYPE REF TO CL_GUI_CONTAINER, "SPLIT HTML ##
      G_PARENT_GRID TYPE REF TO CL_GUI_CONTAINER. "SPLIT GRID ##

*----------------------------------------------------------------------*
* ALV VARIANCE
*----------------------------------------------------------------------*
DATA: GT_FCAT  TYPE LVC_T_FCAT,
      GT_FCAT1 TYPE LVC_T_FCAT,
      GT_FCAT2 TYPE LVC_T_FCAT,
      GT_FCAT3 TYPE LVC_T_FCAT,
      GS_FCAT  TYPE LVC_S_FCAT.
DATA: GT_SORT    TYPE LVC_T_SORT,
      GT_SORT1   TYPE LVC_T_SORT,
      GT_SORT2   TYPE LVC_T_SORT,
      GS_SORT    TYPE LVC_S_SORT,
      GS_LAYOUT  TYPE LVC_S_LAYO,
      GS_LAYOUT1 TYPE LVC_S_LAYO,
      GS_LAYOUT2 TYPE LVC_S_LAYO.
DATA: GS_F4 TYPE LVC_S_F4,
      GT_F4 TYPE LVC_T_F4.
DATA: GS_PRINT   TYPE LVC_S_PRNT.
DATA: GS_STBL    TYPE LVC_S_STBL.
DATA: GT_STYL    TYPE LVC_T_STYL.
DATA: GS_STYL    TYPE LVC_S_STYL.
DATA: GS_COLOR TYPE LVC_S_SCOL,
      GT_COLOR TYPE LVC_T_SCOL.

*----------------------------------------------------------------------*
* ALV TOOLBAR
*----------------------------------------------------------------------*
DATA: GO_TOOLBAR TYPE REF TO CL_GUI_TOOLBAR.
DATA: GT_EXCLUDE TYPE UI_FUNCTIONS.

*----------------------------------------------------------------------*
* ALV VARIANT
*----------------------------------------------------------------------*
DATA: GS_VARIANT TYPE DISVARIANT.

*----------------------------------------------------------------------*
* ALV CONTROL
*----------------------------------------------------------------------*
DATA: GT_ROWS TYPE LVC_T_ROW,
      GS_ROWS TYPE LVC_S_ROW,
      GT_COLS TYPE LVC_T_COL,
      GS_COLS TYPE LVC_S_COL.
