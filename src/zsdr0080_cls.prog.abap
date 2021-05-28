*&---------------------------------------------------------------------*
*& Include          ZSDR0080_CLS
*&---------------------------------------------------------------------*
CLASS: lcl_event_handler DEFINITION DEFERRED.

DATA: g_event_handler TYPE REF TO lcl_event_handler.
*----------------------------------------------------------------------*
*       class lcl_event_handler definition
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                    e_column_id
                    es_row_no,

      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING sender
                    e_object
                    e_interactive,

      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING sender
                    e_ucomm,

      handle_hotspot_click_200
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                    e_column_id
                    es_row_no,

      handle_hotspot_click_300
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                    e_column_id
                    es_row_no,

      handle_hotspot_click_400
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                    e_column_id
                    es_row_no,

      handle_hotspot_click_500
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                    e_column_id
                    es_row_no,

      handle_hotspot_click_600
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                    e_column_id
                    es_row_no,

      handle_hotspot_click_700
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                    e_column_id
                    es_row_no.
ENDCLASS. "LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*       class lcl_event_handler implementation
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION .
  METHOD: handle_hotspot_click.
    PERFORM alv_hotspot_click USING e_row_id e_column_id es_row_no.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_toolbar.
    PERFORM alv_toolbar USING e_object e_interactive.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
    PERFORM alv_user_command USING e_ucomm.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "handle_user_command

  METHOD: handle_hotspot_click_200.
    PERFORM alv_hotspot_click_200 USING e_row_id e_column_id es_row_no.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD: handle_hotspot_click_300.
    PERFORM alv_hotspot_click_300 USING e_row_id e_column_id es_row_no.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD: handle_hotspot_click_400.
    PERFORM alv_hotspot_click_400 USING e_row_id e_column_id es_row_no.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD: handle_hotspot_click_500.
    PERFORM alv_hotspot_click_500 USING e_row_id e_column_id es_row_no.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD: handle_hotspot_click_600.
    PERFORM alv_hotspot_click_600 USING e_row_id e_column_id es_row_no.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD: handle_hotspot_click_700.
    PERFORM alv_hotspot_click_700 USING e_row_id e_column_id es_row_no.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    gv_ucomm = TEXT-d01.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gv_ucomm.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS. "LCL_EVENT_HANDLER IMPLEMENTATION
