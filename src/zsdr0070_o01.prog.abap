*----------------------------------------------------------------------*
***INCLUDE ZSDR0070_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*& 0100 화면 메뉴
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TIT0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  DATA: lv_edit   TYPE int4,    "편집모드 판별 변수
        lv_fnm    TYPE char40,  "
        lv_height TYPE i.     "헤더높이
  FIELD-SYMBOLS: <fs_tab> TYPE table.
  IF go_grid_0100 IS INITIAL.
    "----// 1:1 ALV 도킹 생성
    CREATE OBJECT go_docking_0100
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
*       ratio     = 100.
        side      = go_docking_0100->dock_at_left
        extension = 2000.
    "----// 도킹 상하로 나누기
    CREATE OBJECT go_splitter_0100
      EXPORTING
        parent  = go_docking_0100
        rows    = 2
        columns = 1.
    "----// 상단 컨테이너 (Header)
    CALL METHOD go_splitter_0100->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_container_0100_1.
    "----// 하단 컨테이너 (ALV)
    CALL METHOD go_splitter_0100->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = go_container_0100_2.
    "----// 헤더 텍스트
    PERFORM header CHANGING lv_height.
    "----// 헤더(=>상단) 높이
    CALL METHOD go_splitter_0100->set_row_height
      EXPORTING
        id     = 1
        height = lv_height.
    "----// 하단 컨테이너 위에 ALV 그리드 배치
    CREATE OBJECT go_grid_0100
      EXPORTING
        i_parent = go_container_0100_2.
    "----// Variant
    PERFORM alv_variant CHANGING gs_variant_0100.
    "----// 레이아웃
    PERFORM alv_layout  CHANGING gs_layout_0100.
    "----// 툴바 제외 버튼
    PERFORM alv_toolbar_exclude TABLES gt_exclude_0100.
    "----// 필드카탈로그
    PERFORM alv_fieldcatalog TABLES gt_fcat_0100.
    "----// 이벤트 등록
    PERFORM alv_event USING go_grid_0100.
    "----// 그리드
    lv_fnm = 'GT_MAIN'.
    ASSIGN (lv_fnm) TO <fs_tab>.
    PERFORM alv_display TABLES gt_fcat_0100
                               gt_sort_0100
                               <fs_tab>[]
                        USING  gs_layout_0100
                               gs_variant_0100
                               gt_exclude_0100
                               go_grid_0100.
  ELSE.
    "----// 그리드 새로고침
    DATA: ls_stbl TYPE lvc_s_stbl.
    CLEAR ls_stbl.
*    ls_stbl-row = 'X'.
*    ls_stbl-col = 'X'.
    CALL METHOD go_grid_0100->refresh_table_display
      EXPORTING
        is_stable = ls_stbl.
  ENDIF.
ENDMODULE.
