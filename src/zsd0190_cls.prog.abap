*&---------------------------------------------------------------------*
*& Include          ZSD0190_CLS
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
CLASS LCL_ALV_GRID DEFINITION DEFERRED.

DATA: GO_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    METHODS : TOP_OF_PAGE FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
      IMPORTING E_DYNDOC_ID.

    METHODS : HANDLE_DATA_CHANGED
                  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
      IMPORTING ER_DATA_CHANGED.

    METHODS : HANDLE_TOOLBAR
                  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
      IMPORTING E_OBJECT E_INTERACTIVE.

    METHODS : HANDLE_DATA_CHANGED_FINISHED
                  FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
      IMPORTING E_MODIFIED ET_GOOD_CELLS.

    METHODS : HANDLE_DOUBLE_CLICK
                  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW E_COLUMN.

    METHODS : HANDLE_HOTSPOT_CLICK
                  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID E_COLUMN_ID.

    METHODS : HANDLE_USER_COMMAND
                  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM.

    METHODS : HANDLE_ONF4_1
                   FOR EVENT ONF4 OF CL_GUI_ALV_GRID
      IMPORTING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.


ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID DEFINITION INHERITING FROM CL_GUI_ALV_GRID.

  PUBLIC SECTION.
    METHODS : SET_OPTIMIZER    IMPORTING HEADER TYPE I.

ENDCLASS. "LCL_ALV_GRID DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD TOP_OF_PAGE.
*    PERFORM TOP_OF_PAGE USING GO_DYNDOC_ID.
  ENDMETHOD.                    "TOP_OF_PAGE

  METHOD HANDLE_DATA_CHANGED.
*    PERFORM DATA_CHANGED  USING ER_DATA_CHANGED.
  ENDMETHOD.                    "handle_data_changed

  METHOD HANDLE_DATA_CHANGED_FINISHED.
*     PERFORM DATA_CHANGED_FINISHED TABLES ET_GOOD_CELLS
*                                     USING E_MODIFIED .
  ENDMETHOD.                    "handle_data_changed

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DOUBLE_CLICK  USING E_ROW E_COLUMN.
  ENDMETHOD.                    "handle_double_click

  METHOD HANDLE_HOTSPOT_CLICK.
*    PERFORM HOTSPOT_CLICK USING E_ROW_ID
*                                E_COLUMN_ID.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD HANDLE_TOOLBAR.
*    PERFORM TOOLBAR_PART  USING E_OBJECT
*                                E_INTERACTIVE.

  ENDMETHOD.                    "HANDLE_TOOLBAR

  METHOD HANDLE_USER_COMMAND.
*    PERFORM USER_COMMAND_PART USING E_UCOMM.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD HANDLE_ONF4_1.
*    PERFORM HANDLE_ONF4_1 USING E_FIELDNAME
*                                ES_ROW_NO
*                                ER_EVENT_DATA.
  ENDMETHOD.                    "HANDLE_ONF4_1

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID IMPLEMENTATION.

  METHOD SET_OPTIMIZER.
    CALL METHOD ME->OPTIMIZE_ALL_COLS
      EXPORTING
        INCLUDE_HEADER = HEADER.
  ENDMETHOD.                    "SET_OPTIMIZER

ENDCLASS.                    "LCL_ALV_GRID IMPLEMENTATION
