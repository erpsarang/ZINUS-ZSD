*&---------------------------------------------------------------------*
*& Include          ZSDR0020_CLS
*&---------------------------------------------------------------------*
CLASS: LCL_EVENT_HANDLER DEFINITION DEFERRED.

DATA: G_EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER.
*----------------------------------------------------------------------*
*       class lcl_event_handler definition
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      HANDLE_HOTSPOT_CLICK
                    FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID
                    E_COLUMN_ID
                    ES_ROW_NO,

      HANDLE_TOOLBAR
                    FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING SENDER
                    E_OBJECT
                    E_INTERACTIVE,

      HANDLE_USER_COMMAND
                    FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING SENDER
                    E_UCOMM.

ENDCLASS. "LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*       class lcl_event_handler implementation
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION .
  METHOD: HANDLE_HOTSPOT_CLICK.
    PERFORM ALV_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    GV_UCOMM = TEXT-D01.
    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = GV_UCOMM.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD HANDLE_TOOLBAR.
    PERFORM ALV_TOOLBAR USING E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

  METHOD HANDLE_USER_COMMAND.
    PERFORM ALV_USER_COMMAND USING E_UCOMM.
* Event 후 강제로 화면 돌리느(PBO-PAI) 메소드
    GV_UCOMM = TEXT-D01.
    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = GV_UCOMM.
  ENDMETHOD.                    "handle_user_command

ENDCLASS. "LCL_EVENT_HANDLER IMPLEMENTATION
