*----------------------------------------------------------------------*
***INCLUDE ZSDR0040_PBO.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*& 0100 화면 메뉴
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TIT0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*& ALV 그리드 만들기
*&---------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.
  DATA: LV_EDIT TYPE INT4,    "편집모드 판별 변수
        LV_FNM  TYPE CHAR40.  "
  FIELD-SYMBOLS: <FS_TAB> TYPE TABLE.
  IF GO_GRID_0100 IS INITIAL.
    "----// 1:1 ALV 도킹 생성
    CREATE OBJECT GO_DOCKING_0100
      EXPORTING
        REPID     = SY-REPID
        DYNNR     = SY-DYNNR
*       ratio     = 100.
        SIDE      = GO_DOCKING_0100->DOCK_AT_LEFT
        EXTENSION = 2000.
    "----// 도킹 상하로 나누기
    CREATE OBJECT GO_SPLITTER_0100
      EXPORTING
        PARENT  = GO_DOCKING_0100
        ROWS    = 2
        COLUMNS = 1.
    "----// 상단 컨테이너 (Header)
    CALL METHOD GO_SPLITTER_0100->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = GO_CONTAINER_0100_1.
    "----// 하단 컨테이너 (ALV)
    CALL METHOD GO_SPLITTER_0100->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = GO_CONTAINER_0100_2.
    "----// 상단 높이
    CALL METHOD GO_SPLITTER_0100->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 10.
    "----// 헤더 텍스트
    PERFORM HEADER.
    "----// 하단 컨테이너 위에 ALV 그리드 배치
    CREATE OBJECT GO_GRID_0100
      EXPORTING
        I_PARENT = GO_CONTAINER_0100_2.
    "----// Variant
    PERFORM ALV_VARIANT CHANGING GS_VARIANT_0100.
    "----// 레이아웃
    PERFORM ALV_LAYOUT  CHANGING GS_LAYOUT_0100.
    "----// 툴바 제외 버튼
    PERFORM ALV_TOOLBAR_EXCLUDE TABLES GT_EXCLUDE_0100.
    "----// 필드카탈로그
    PERFORM ALV_FIELDCATALOG TABLES GT_FCAT_0100.
    "----// 이벤트 등록
    PERFORM ALV_EVENT USING GO_GRID_0100.
    "----// 그리드
    IF P_MONT EQ 'X'.
      LV_FNM = 'GT_MAIN_W'.
    ELSE.
      LV_FNM = 'GT_MAIN_M'.
    ENDIF.
    ASSIGN (LV_FNM) TO <FS_TAB>.
    PERFORM ALV_DISPLAY TABLES GT_FCAT_0100
                               GT_SORT_0100
                               <FS_TAB>[]
                        USING  GS_LAYOUT_0100
                               GS_VARIANT_0100
                               GT_EXCLUDE_0100
                               GO_GRID_0100.
  ELSE.
    "----// 그리드 새로고침
    DATA: LS_STBL TYPE LVC_S_STBL.
    CLEAR LS_STBL.
*    ls_stbl-row = 'X'.
*    ls_stbl-col = 'X'.
    CALL METHOD GO_GRID_0100->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = LS_STBL.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TIT0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE PBO_0200 OUTPUT.
*  DATA: lv_edit TYPE int4,    "편집모드 판별 변수
*        lv_fnm  TYPE char40.  "
*   FIELD-SYMBOLS: <fs_tab> TYPE table.
  IF GO_GRID_0200 IS INITIAL.
*----// 1:1 ALV 도킹 생성
    CREATE OBJECT GO_DOCKING_0200
      EXPORTING
        REPID     = SY-REPID
        DYNNR     = SY-DYNNR
*       ratio     = 90
        SIDE      = GO_DOCKING_0200->DOCK_AT_LEFT
        EXTENSION = 2000.

    "----// 도킹 상하로 나누기
    CREATE OBJECT GO_SPLITTER_0200
      EXPORTING
       PARENT  = GO_DOCKING_0200
        ROWS    = 2
        COLUMNS = 1.
    "----// 상단 컨테이너 (Header)
    CALL METHOD GO_SPLITTER_0200->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = GO_CONTAINER_0200_1.
    "----// 하단 컨테이너 (ALV)
    CALL METHOD GO_SPLITTER_0200->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = GO_CONTAINER_0200_2.
    "----// 상단 높이
    CALL METHOD GO_SPLITTER_0200->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 10.
    "----// 헤더 텍스트
    PERFORM HEADER_SUB.
    "----// 하단 컨테이너 위에 ALV 그리드 배치
    CREATE OBJECT GO_GRID_0200
      EXPORTING
        I_PARENT = GO_CONTAINER_0200_2.
    "----// Variant
    PERFORM ALV_VARIANT CHANGING GS_VARIANT_0200.
    "----// 레이아웃
    PERFORM ALV_LAYOUT  CHANGING GS_LAYOUT_0200.
    "----// 툴바 제외 버튼
    PERFORM ALV_TOOLBAR_EXCLUDE TABLES GT_EXCLUDE_0200.
    "----// 필드카탈로그
    PERFORM ALV_FIELDCATALOG TABLES GT_FCAT_0200.
    "----// 이벤트 등록
    PERFORM ALV_EVENT USING GO_GRID_0200.

    "----// 그리드
    IF P_MONT EQ 'X'.
      LV_FNM = 'GT_IREQ_W'.
    ELSE.
      LV_FNM = 'GT_IREQ_M'.
    ENDIF.
    ASSIGN (LV_FNM) TO <FS_TAB>.
    PERFORM ALV_DISPLAY TABLES GT_FCAT_0200
                               GT_SORT_0200
                               <FS_TAB>[]
                        USING  GS_LAYOUT_0200
                               GS_VARIANT_0200
                               GT_EXCLUDE_0200
                               GO_GRID_0200.
  ELSE.
*----// 그리드 새로고침
*    DATA: ls_stbl TYPE lvc_s_stbl.
    CLEAR LS_STBL.
*    ls_stbl-row = 'X'.
*    ls_stbl-col = 'X'.
    CALL METHOD GO_GRID_0200->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = LS_STBL.
  ENDIF.
ENDMODULE.
