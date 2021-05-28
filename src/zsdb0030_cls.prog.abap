*&---------------------------------------------------------------------*
*& Include          ZSD1R0040_CLS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       ALV Grid ### ##
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID DEFINITION INHERITING FROM CL_GUI_ALV_GRID.
  PUBLIC SECTION.
    METHODS U_OPTIMIZE_ALL_COLS.
ENDCLASS. "lcl_grid DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_grid IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_ALV_GRID IMPLEMENTATION.
  METHOD U_OPTIMIZE_ALL_COLS.
    ME->OPTIMIZE_ALL_COLS( 1 ).
  ENDMETHOD.                    "u_optimize_all_cols
ENDCLASS. "lcl_grid IMPLEMENTATION
*----------------------------------------------------------------------*
*       Receiver Class ##
*----------------------------------------------------------------------*
CLASS LCL_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS HANDLE_TOP_OF_PAGE
                  FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
      IMPORTING E_DYNDOC_ID.

    METHODS HANDLE_DATA_CHANGED
          FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
      IMPORTING
          ER_DATA_CHANGED
          E_UCOMM.

    METHODS HANDLE_DOUBLE_CLICK
          FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING
          SENDER
          E_ROW
          E_COLUMN
          ES_ROW_NO.

ENDCLASS. "lcl_receiver DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_TOP_OF_PAGE.
*    PERFORM HANDLE_TOP_OF_PAGE USING E_DYNDOC_ID.
  ENDMETHOD.                    "top_of_page

  METHOD HANDLE_DATA_CHANGED.
*    PERFORM HANDLE_DATA_CHANGED USING ER_DATA_CHANGED
*                                      E_UCOMM.
  ENDMETHOD.                    "handle_data_changed
  METHOD HANDLE_DOUBLE_CLICK.
*    PERFORM HANDLE_DOUBLE_CLICK USING SENDER
*                                      E_ROW
*                                      E_COLUMN
*                                      ES_ROW_NO.
  ENDMETHOD.                    "handle_double_click
ENDCLASS.
