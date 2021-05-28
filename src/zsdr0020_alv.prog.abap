*&---------------------------------------------------------------------*
*& Include          ZSDR0020_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SETTING_CONTAINER
*&---------------------------------------------------------------------*
FORM SETTING_CONTAINER .
* ALV Docking Container
  CREATE OBJECT GC_DOCKING
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

  CREATE OBJECT GC_SPLITTER
    EXPORTING
      PARENT  = GC_DOCKING
      ROWS    = 1
      COLUMNS = 1.

  CALL METHOD GC_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GC_CONTAINER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
FORM SET_GRID CHANGING PC_CONTAINER TYPE REF TO CL_GUI_CONTAINER
                       PC_GRID      TYPE REF TO CL_GUI_ALV_GRID.

  CREATE OBJECT PC_GRID
    EXPORTING
      I_PARENT = PC_CONTAINER.
*      i_appl_events = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV USING P_GRID.
  " Variant
  CLEAR GS_VARIANT.
  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = 'A'.
  GS_VARIANT-LOG_GROUP   = 'AAA'.

  " Layout
  PERFORM SET_LAYOUT.
  " Toolbar
  PERFORM SET_TOOLBAR.
  " Fieldcatlog
  PERFORM SET_FIELDCATALOG.
  " Event
  PERFORM SET_EVENT.
  " Sort
  PERFORM SET_SORT.
  " ALV Display
  PERFORM CALL_METHOD_DISPLAY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LAYOUT .
  CLEAR: GS_LAYOUT.
  GS_LAYOUT-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT-ZEBRA      = ABAP_ON.
  GS_LAYOUT-SEL_MODE   = 'A'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TOOLBAR
*&---------------------------------------------------------------------*
FORM SET_TOOLBAR .
  _CLEAR: GT_TOOLBAR_EXCLUDE.

  SET_TOOLBAR_0100 :
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM SET_FIELDCATALOG.
*-Rebuild Merged Catalog
*  #1  필드명
*  #2  키지정
*  #3  정렬
*  #4  안보이기
*  #5  핫스팟
*  #6  체크박스
*  #7  수정모드
*  #8  합계
*  #9  색강조
*  #10 필드타이틀
*  #11 금액참조필드
*  #12 수량참조필드
*  #13 FIELDNAME
*  #14 TABNAME
*  #15 NO_ZERO
  CLEAR: GS_FIELDCAT, GV_POS.
  _CLEAR GT_FIELDCAT.
  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT USING:
*  #1          #2  #3  #4  #5  #6  #7  #8  #9     #10      #11 #12  #13  #14  #15

 'MATNR'      'X' 'L' '' 'X'  ''  ''  ''  ''     TEXT-C01 ''  ''   ''   ''   '',
 'MAKTX'      'X' 'L' '' 'X'  ''  ''  ''  ''     TEXT-C02 ''  ''   ''   ''   '',
 'VTWEG'      '' 'L' '' 'X'  ''  ''  ''  ''      TEXT-C15 ''  ''   ''   ''   '',
 'KSCHL'      ''  'L' '' ''   ''  ''  ''  'C500' TEXT-C03 ''  ''   ''   ''   '',

 'KUNNR'      ''  'L' '' ''   ''  ''  ''  'C500'  TEXT-C11 '' ''   ''   ''  'X',
 'NAME1'      ''  'L' '' ''   ''  ''  ''  'C500'  TEXT-C12 '' ''   ''   ''  ''.
  IF GV_CHK_IC = 'X'.
    PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT USING:
     'KUNWE'      '' 'L'  ''  ''  ''  ''  '' 'C500'  TEXT-C13 '' '' '' ''  'X',
     'KUNWE_TXT'  '' 'L'  ''  ''  ''  ''  '' 'C500'  TEXT-C14 '' '' '' ''  ' '.
  ENDIF.

  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT USING:
'KBETR'      ''  'L'  ''  ''  ''  ''  ''  'C500'  TEXT-C04 'KONWA' '' '' ''  '',
'KONWA'      ''  'L'  ''  ''  ''  ''  ''  'C400'  TEXT-C05 ''      '' '' ''  '',
'KPEIN'      ''  'L'  ''  ''  ''  ''  ''  '   '   TEXT-C06 ''      '' '' ''  '',
'KMEIN'      ''  'L'  ''  ''  ''  ''  ''  'C400'  TEXT-C07 ''      '' '' ''  '',
'DATAB'      ''  'L'  ''  ''  ''  ''  ''  '   '   TEXT-C08 ''      '' '' ''  '',
'DATBI'      ''  'L'  ''  ''  ''  ''  ''  'C400'  TEXT-C09 ''      '' '' ''  '',
'LOEVM'      ''  'L'  ''  ''  ''  ''  ''  '   '   TEXT-C10 ''      '' '' ''  ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REBUILD_LVC_CATALOG
*&---------------------------------------------------------------------*
FORM REBUILD_LVC_CATALOG TABLES PT_FIELDCAT
                          USING P_FIELDNAME  "필드명칭
                                P_KEY        "키지정
                                P_JUST       "정렬
                                P_NO_OUT     "안보이기
                                P_HOTSPOT    "핫스팟
                                P_CHECKBOX   "체크박스
                                P_EDIT       "수정모드
                                P_DO_SUM     "합계
                                P_EMPHASIZE  "색강조
                                P_TITLE      "필드타이틀
                                P_CFIELDNAME "금액참조필드
                                P_QFIELDNAME "수량참조필드
                                P_FNAME      "Reference Field
                                P_TABNAME   "Reference Table
                                P_NO_ZERO.
  "필드 순번 증가
  GV_POS = GV_POS + 1.
  "입력된 값들 적용
  MOVE: P_FIELDNAME  TO GS_FIELDCAT-FIELDNAME,  "필드명
        P_KEY        TO GS_FIELDCAT-KEY,        "키지정
        P_JUST       TO GS_FIELDCAT-JUST,       "정렬
        P_NO_OUT     TO GS_FIELDCAT-NO_OUT,     "안보이기
        P_HOTSPOT    TO GS_FIELDCAT-HOTSPOT,    "핫스팟
        P_CHECKBOX   TO GS_FIELDCAT-CHECKBOX,   "체크박스
        P_EDIT       TO GS_FIELDCAT-EDIT,       "수정모드
        P_DO_SUM     TO GS_FIELDCAT-DO_SUM,     "합계
        P_EMPHASIZE  TO GS_FIELDCAT-EMPHASIZE,  "색강조
        P_CFIELDNAME TO GS_FIELDCAT-CFIELDNAME, "금액참조필드
        P_QFIELDNAME TO GS_FIELDCAT-QFIELDNAME, "수량참조필드
        P_FNAME      TO GS_FIELDCAT-REF_FIELD,  "Reference Field
        P_TABNAME    TO GS_FIELDCAT-REF_TABLE,  "Reference Table
        P_NO_ZERO    TO GS_FIELDCAT-NO_ZERO,    "NO ZERO

        GV_POS       TO GS_FIELDCAT-COL_POS.    "순번
  "타이틀은 따로 입력된 경우만 적용
  MOVE: P_TITLE TO GS_FIELDCAT-COLTEXT,    "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_L,  "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_M,  "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_S.  "필드타이틀

  APPEND GS_FIELDCAT TO PT_FIELDCAT.
  CLEAR GS_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EVENT
*&---------------------------------------------------------------------*
FORM SET_EVENT .
  CALL METHOD GC_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GC_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CREATE OBJECT G_EVENT_HANDLER.
  SET HANDLER: G_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR GC_GRID,
               G_EVENT_HANDLER->HANDLE_TOOLBAR       FOR GC_GRID,
               G_EVENT_HANDLER->HANDLE_USER_COMMAND  FOR GC_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SORT
*&---------------------------------------------------------------------*
FORM SET_SORT .
  _CLEAR: GT_SORT.
  CLEAR GS_SORT.
  GS_SORT-SPOS ='1'.
  GS_SORT-FIELDNAME = 'MATNR'.
  GS_SORT-UP = ABAP_ON.
  APPEND GS_SORT TO GT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_METHOD_DISPLAY
*&---------------------------------------------------------------------*
FORM CALL_METHOD_DISPLAY .
  CALL METHOD GC_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_buffer_active               = abap_on
*     i_bypassing_buffer            = abap_on
      I_SAVE                        = 'A'
      I_DEFAULT                     = ABAP_ON
      IS_LAYOUT                     = GS_LAYOUT
      IS_VARIANT                    = GS_VARIANT
      IT_TOOLBAR_EXCLUDING          = GT_TOOLBAR_EXCLUDE
    CHANGING
      IT_OUTTAB                     = GT_DISP[]
      IT_FIELDCATALOG               = GT_FIELDCAT
      IT_SORT                       = GT_SORT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FREE
*&---------------------------------------------------------------------*
FORM ALV_GRID_FREE USING PC_GRID TYPE REF TO CL_GUI_ALV_GRID.
  IF PC_GRID IS NOT INITIAL.
    CALL METHOD PC_GRID->FREE.
    CLEAR PC_GRID.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DOCKING_FREE
*&---------------------------------------------------------------------*
FORM ALV_DOCKING_FREE USING PC_DOCKING TYPE REF TO CL_GUI_DOCKING_CONTAINER.
  IF PC_DOCKING IS NOT INITIAL.
    CALL METHOD PC_DOCKING->FREE.
    CLEAR PC_DOCKING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM ALV_HOTSPOT_CLICK USING P_ROW_ID    TYPE LVC_S_ROW
                             P_COLUMN_ID TYPE LVC_S_COL
                             P_ROW_NO    TYPE LVC_S_ROID.

  DATA : LV_FIELD TYPE CHAR40,
         LV_SEQ   TYPE NUMC2.
  FIELD-SYMBOLS : <FS_MATNR>.
  DATA: LS_DISP LIKE GT_DISP.
  CLEAR LS_DISP.

  READ TABLE GT_DISP INTO LS_DISP INDEX P_ROW_ID.
  CHECK SY-SUBRC IS INITIAL.
  CASE P_COLUMN_ID.
    WHEN 'MATNR' OR 'MAKTX'.
      IF LS_DISP-MATNR IS NOT INITIAL.
        SET PARAMETER ID 'MXX' FIELD 'K'.
        SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_TOOLBAR
*&---------------------------------------------------------------------*
FORM ALV_TOOLBAR USING P_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                       P_INTERACTIVE TYPE C.

  DATA: LS_TOOLBAR TYPE STB_BUTTON.

  IF SY-DYNNR = '0100'.
    LS_TOOLBAR-FUNCTION  = TEXT-D01.
    LS_TOOLBAR-BUTN_TYPE = '3'.
    APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    LS_TOOLBAR-FUNCTION  = 'REF'.
    LS_TOOLBAR-ICON      = ICON_REFRESH.
    LS_TOOLBAR-QUICKINFO = 'Refresh'.
    LS_TOOLBAR-TEXT      = 'Refresh'.
    APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_USER_COMMAND
*&---------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING PF_UCOMM TYPE SYUCOMM.
  CLEAR: GV_ERROR.

  CASE PF_UCOMM.
    WHEN 'REF'.
      IF GT_ZSDT0040[] IS INITIAL.
        PERFORM SELECT_DATA.
      ELSE.
        PERFORM SELECT_DATA_INTERCOM. "관계사일경우
      ENDIF.
      CLEAR GT_DISP.
      SORT GT_DISP BY MATNR.
      DESCRIBE TABLE GT_DISP LINES GV_LINES.

      MESSAGE S006 WITH GV_LINES.
  ENDCASE.

ENDFORM.
