*&---------------------------------------------------------------------*
*& Include          ZSDR0070_CLS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_toolbar                "<----// ALV 툴바 생성 이벤트 - ALV 출력 전
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      handle_double_click           "<----// ALV 필드 더블클릭 이벤트
                    FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,
      handle_data_changed           "<----// ALV 셀 데이터 변경 이벤트
                    FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed  e_onf4  e_onf4_before  e_onf4_after  e_ucomm,
      handle_data_changed_finished  "<----// 변경 완료
                    FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified  et_good_cells,
      handle_user_command           "<----// ALV 툴바 버튼 클릭 이벤트
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.
*&---------------------------------------------------------------------*
*&       CLASS lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_toolbar.
    PERFORM handle_toolbar USING e_object e_interactive.
  ENDMETHOD.
  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row  e_column.
  ENDMETHOD.
  METHOD handle_data_changed.
    PERFORM handle_data_changed USING er_data_changed  e_ucomm.
  ENDMETHOD.
  METHOD handle_data_changed_finished.

  ENDMETHOD.
  METHOD handle_user_command.
    PERFORM handle_user_command USING e_ucomm.
  ENDMETHOD.
ENDCLASS.
