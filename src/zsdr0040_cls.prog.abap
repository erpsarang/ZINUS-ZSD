*&---------------------------------------------------------------------*
*& Include          ZSDR0040_CLS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       CLASS LCL_EVENT_RECEIVER DEFINITION
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      HANDLE_TOOLBAR                "<----// ALV 툴바 생성 이벤트 - ALV 출력 전
                    FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,
      HANDLE_DOUBLE_CLICK           "<----// ALV 필드 더블클릭 이벤트
                    FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN,
      HANDLE_DATA_CHANGED           "<----// ALV 셀 데이터 변경 이벤트
                    FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED  E_ONF4  E_ONF4_BEFORE  E_ONF4_AFTER  E_UCOMM,
      HANDLE_DATA_CHANGED_FINISHED  "<----// 변경 완료
                    FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED  ET_GOOD_CELLS,
      HANDLE_USER_COMMAND           "<----// ALV 툴바 버튼 클릭 이벤트
                    FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.
*&---------------------------------------------------------------------*
*&       CLASS lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_TOOLBAR.
    PERFORM HANDLE_TOOLBAR USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.
  METHOD HANDLE_DOUBLE_CLICK.

  ENDMETHOD.
  METHOD HANDLE_DATA_CHANGED.
    PERFORM HANDLE_DATA_CHANGED USING ER_DATA_CHANGED  E_UCOMM.
  ENDMETHOD.
  METHOD HANDLE_DATA_CHANGED_FINISHED.
    PERFORM HANDLE_DATA_CHANGED_FINISHED USING E_MODIFIED  ET_GOOD_CELLS.
  ENDMETHOD.
  METHOD HANDLE_USER_COMMAND.
    PERFORM HANDLE_USER_COMMAND USING E_UCOMM.
  ENDMETHOD.
ENDCLASS.

"2021/03/19 추가
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      ON_CLOSE
                    FOR EVENT CLOSE OF CL_GUI_DIALOGBOX_CONTAINER
        IMPORTING SENDER.
ENDCLASS.
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_CLOSE.
    IF NOT SENDER IS INITIAL.
      CALL METHOD SENDER->FREE
        EXCEPTIONS
          OTHERS = 1.
      IF SY-SUBRC <> 0.
*      ERROR HANDLING
      ENDIF.
      FREE GO_DIALOG_0200.
      CLEAR GO_DIALOG_0200.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
