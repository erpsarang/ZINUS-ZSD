*&---------------------------------------------------------------------*
*& Include          ZSDR0100_CLS
*&---------------------------------------------------------------------*
CLASS: lcl_event_handler DEFINITION DEFERRED.

DATA : gr_event_handler TYPE REF TO lcl_event_handler.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_data_changed
                    FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      handle_top_of_page
                    FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id,

      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object   e_interactive,

      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_double_click
                    FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      handle_hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id    e_column_id,

      handle_on_f4
                    FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender    e_fieldname     e_fieldvalue   es_row_no
                    er_event_data   et_bad_cells       e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_data_changed.
*    PERFORM handle_data_changed USING er_data_changed.
  ENDMETHOD.                    "handle_data_changed

  METHOD handle_top_of_page.
    PERFORM handle_top_of_page USING e_dyndoc_id.
  ENDMETHOD.                    "handle_data_changed

  METHOD handle_toolbar.
*    IF p_sear IS NOT INITIAL.
*      PERFORM handle_toolbar USING e_object.
*    ENDIF.
  ENDMETHOD.                    "HANDLE_Toolbar

  METHOD handle_user_command.
*    PERFORM handle_user_command USING e_ucomm.
  ENDMETHOD.                    "HANDLE_user_command

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row  e_column.
* Event 후 강제로 화면 돌리는 (PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "HANDLE_double_click

  METHOD handle_hotspot_click.
*    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID  E_COLUMN_ID.
  ENDMETHOD.                    "HANDLE_hotspot_click

  METHOD handle_on_f4.
*    CASE  E_FIELDNAME.
*      WHEN 'MEINS'.
*        PERFORM HANDLE_ON_F4_HEINS USING ES_ROW_NO-ROW_ID.
*        ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.
*    ENDCASE.
  ENDMETHOD.                    "HANDLE_ON_F4

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
