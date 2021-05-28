*&---------------------------------------------------------------------*
*& Include          ZSD0190_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_FORMAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DOWNLOAD_FORMAT .
  DATA : WWWDATATAB LIKE WWWDATATAB.

  DATA : LV_FILENAME TYPE STRING,
         LV_PATH     TYPE STRING,
         LV_FULLPATH TYPE STRING.

  DATA : FILENAME TYPE RLGRAP-FILENAME.

  CLEAR : WWWDATATAB.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF WWWDATATAB
    FROM WWWDATA
   WHERE OBJID EQ 'ZSDR0190'.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG " 파일 저장 팝업 화면
    EXPORTING
      WINDOW_TITLE      = 'Excel Format'
      DEFAULT_EXTENSION = 'xlsx'
      DEFAULT_FILE_NAME = 'Delivery rescheduling teamplate'
      FILE_FILTER       = 'Only Excel Files (*.xls;*.xlsx)'
      INITIAL_DIRECTORY = 'C:\'
    CHANGING
      FILENAME          = LV_FILENAME
      PATH              = LV_PATH
      FULLPATH          = LV_FULLPATH.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  FILENAME = LV_FULLPATH.

  CHECK FILENAME IS NOT INITIAL .

  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = WWWDATATAB
      DESTINATION = FILENAME.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>EXECUTE
    EXPORTING
      DOCUMENT               = LV_FULLPATH
    EXCEPTIONS
      CNTL_ERROR             = 1
      ERROR_NO_GUI           = 2
      BAD_PARAMETER          = 3
      FILE_NOT_FOUND         = 4
      PATH_NOT_FOUND         = 5
      FILE_EXTENSION_UNKNOWN = 6
      ERROR_EXECUTE_FAILED   = 7
      SYNCHRONOUS_FAILED     = 8
      NOT_SUPPORTED_BY_GUI   = 9
      OTHERS                 = 10.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .
  DATA : LS_DYNTXT TYPE SMP_DYNTXT.

  CLEAR : LS_DYNTXT.
  LS_DYNTXT-ICON_ID   = ICON_XLS.
  LS_DYNTXT-TEXT      = TEXT-T02. " Excel Template
  LS_DYNTXT-ICON_TEXT = TEXT-T02. " Excel Template
  LS_DYNTXT-QUICKINFO = TEXT-T02. " Excel Template
  SSCRFIELDS-FUNCTXT_01  = LS_DYNTXT.

  CLEAR : LS_DYNTXT.
  LS_DYNTXT-ICON_ID   = ICON_LIST..
  LS_DYNTXT-TEXT      = TEXT-T03. " Sales Progress Report
  LS_DYNTXT-ICON_TEXT = TEXT-T03. " Sales Progress Report
  LS_DYNTXT-QUICKINFO = TEXT-T03. " Sales Progress Report
  SSCRFIELDS-FUNCTXT_02  = LS_DYNTXT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_FROM_EXCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DATA_FROM_EXCEL .
  DATA : LV_INDEX LIKE SY-TABIX,
         LV_BROW  TYPE I VALUE '2',
         LV_BCOL  TYPE I VALUE '1',
         LV_EROW  TYPE I VALUE '65536',
         LV_ECOL  TYPE I VALUE '3'.

  DATA : L_TBNAME TYPE CHAR20.
  DATA : L_WANAME TYPE CHAR20.

  FIELD-SYMBOLS : <FS_TB> TYPE STANDARD TABLE. "table
  FIELD-SYMBOLS : <FS_WA> TYPE ANY.            "structure
  FIELD-SYMBOLS : <FS_VA> TYPE ANY.            "value

  L_TBNAME = 'GT_UPLOAD[]'.
  L_WANAME = 'GT_UPLOAD'.

  ASSIGN (L_TBNAME) TO <FS_TB>.
  ASSIGN (L_WANAME) TO <FS_WA>.

  " excel file -> gt_excel_tmp
  CLEAR : GT_EXCEL_TMP, GT_EXCEL_TMP[].
  CALL FUNCTION 'ZMM1_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = LV_BCOL
      I_BEGIN_ROW             = LV_BROW
      I_END_COL               = LV_ECOL
      I_END_ROW               = LV_EROW
    TABLES
      INTERN                  = GT_EXCEL_TMP
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  SORT GT_EXCEL_TMP BY ROW COL.

*- Excel Data
  CLEAR : GT_UPLOAD, GT_UPLOAD[].

  LOOP AT GT_EXCEL_TMP.
    MOVE : GT_EXCEL_TMP-COL TO LV_INDEX.
    ASSIGN COMPONENT LV_INDEX OF STRUCTURE <FS_WA> TO <FS_VA>.

    CONDENSE GT_EXCEL_TMP-VALUE.
    MOVE : GT_EXCEL_TMP-VALUE TO <FS_VA>.

    AT END OF ROW.
      APPEND <FS_WA> TO <FS_TB>.
      CLEAR <FS_WA>.
    ENDAT.
  ENDLOOP.

  CHECK GT_UPLOAD[] IS NOT INITIAL.
  SELECT D~BSTKD_E, "PO#
         A~VBELN,   "S/O
         A~POSNR,   "ITEM NO
         A~MATNR,   "SKU
         B~MBDAT,   "Mat.avail.date
         E~MBDAT AS MBDAT_S, "S/O Mat.avail.date
         C~VBELN AS VBELN_D, "Delivery
         C~LFDAT,   "Delivery date
         C~WADAT,   "Plan GI date
         C~ROUTE    "Route
    INTO TABLE @DATA(LT_VBAP)
    FROM VBAP AS A INNER JOIN VBKD AS D
                           ON A~VBELN EQ D~VBELN
                          AND A~POSNR EQ D~POSNR
                   INNER JOIN LIPS AS B
                           ON A~VBELN EQ B~VGBEL
                          AND A~POSNR EQ B~VGPOS
                   INNER JOIN LIKP AS C
                           ON C~VBELN EQ B~VBELN
                  INNER JOIN VBEP AS E
                          ON A~VBELN EQ E~VBELN
                         AND A~POSNR EQ E~POSNR
    FOR ALL ENTRIES IN @GT_UPLOAD
    WHERE D~BSTKD_E EQ @GT_UPLOAD-BSTKD_E.

  "SKU Desc.
  SELECT MATNR,
         MAKTX
    INTO TABLE @DATA(LT_MAKT)
    FROM MAKT
    FOR ALL ENTRIES IN @LT_VBAP
    WHERE MATNR EQ @LT_VBAP-MATNR
      AND SPRAS EQ @SY-LANGU.

  CLEAR : GT_DISP, GT_DISP[].
  LOOP AT GT_UPLOAD.
    IF GT_UPLOAD-BSTKD_E IS INITIAL.
      GT_DISP-ICON = ICON_LED_RED.
      GT_DISP-MSG  = 'Fill the all required fields'.
    ELSEIF GT_UPLOAD-LFDAT_EX IS INITIAL.
      GT_DISP-ICON = ICON_LED_RED.
      GT_DISP-MSG  = 'Fill the all required fields'.
    ENDIF.

    IF GT_DISP-ICON IS INITIAL.

      LOOP AT LT_VBAP INTO DATA(LS_VBAP) WHERE BSTKD_E = GT_UPLOAD-BSTKD_E.
        IF SY-SUBRC EQ 0.
          MOVE-CORRESPONDING GT_UPLOAD TO GT_DISP.
          MOVE-CORRESPONDING LS_VBAP TO GT_DISP.

          IF GT_DISP-LFDAT = GT_DISP-LFDAT_EX. "delivery date 가 기존의 date와 동일한 경우
            IF LS_VBAP-ROUTE = GT_UPLOAD-ROUTE_EX. "route가 기존과 동일한지 check
              GT_DISP-ICON = ICON_LED_GREEN.
            ELSEIF GT_UPLOAD-ROUTE_EX IS INITIAL. "업로드 route가 없는 경우
              GT_DISP-ICON = ICON_LED_GREEN.
            ENDIF.
          ENDIF.
        ENDIF.

        "SKU Desc.
        READ TABLE LT_MAKT INTO DATA(LS_MAKT) WITH KEY MATNR = GT_DISP-MATNR.
        IF SY-SUBRC EQ 0.
          GT_DISP-MAKTX = LS_MAKT-MAKTX.
        ENDIF.

        APPEND GT_DISP.
        CLEAR  GT_DISP.
      ENDLOOP.
    ELSE. "업로드 데이터에 error가 있는 경우
      MOVE-CORRESPONDING GT_UPLOAD TO GT_DISP.

      APPEND GT_DISP.
      CLEAR  GT_DISP.
    ENDIF.

    CLEAR GT_UPLOAD.
  ENDLOOP.
  SORT GT_DISP BY BSTKD_E VBELN POSNR.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILENAME_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM FILENAME_F4  CHANGING P_P_FILE.
  CALL FUNCTION 'WS_FILENAME_GET' "파일 선택 팝업창
    EXPORTING
      DEF_PATH         = 'Delivery rescheduling'
      MASK             = '*.XLSX.'
      MODE             = 'O'
      TITLE            = 'Select File'
    IMPORTING
      FILENAME         = P_P_FILE
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_BDC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EXECUTE_BDC .
  CLEAR : GT_LVC_T_ROID, GS_LVC_S_ROID.
  "선택된 ROW ID
  CALL METHOD GO_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = GT_LVC_T_ROID.

  IF GT_LVC_T_ROID[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR : LT_BDC, LT_BDC[].
  LOOP AT GT_LVC_T_ROID INTO GS_LVC_S_ROID.
    READ TABLE GT_DISP INTO LT_BDC INDEX GS_LVC_S_ROID-ROW_ID.
    APPEND LT_BDC.
    CLEAR  LT_BDC.
  ENDLOOP.

  SORT LT_BDC BY BSTKD_E VBELN POSNR.
  DELETE ADJACENT DUPLICATES FROM LT_BDC COMPARING VBELN.

  LOOP AT LT_BDC.
    IF LT_BDC-ROUTE_EX IS INITIAL. "엑셀 ROUTE 공백
      IF LT_BDC-LFDAT NE LT_BDC-LFDAT_EX. "엑셀 Delivery date와 기존 date가 다른 경우
        "scheduling 버튼 수행
        PERFORM EXECUTE_BDC1.
      ENDIF.
    ELSEIF LT_BDC-ROUTE_EX EQ LT_BDC-ROUTE. "기존 ROUTE 동일
      IF LT_BDC-LFDAT NE LT_BDC-LFDAT_EX.
        "scheduling 버튼 수행
        PERFORM EXECUTE_BDC1.
      ENDIF.
    ELSEIF LT_BDC-ROUTE_EX NE LT_BDC-ROUTE. "기존 ROUTE 다른 경우
      PERFORM EXECUTE_BDC2.
    ENDIF.

    "ICON MSG 변경
    IF LT_BDC-ICON = ICON_LED_RED.
      MODIFY  GT_DISP FROM LT_BDC TRANSPORTING ICON MSG WHERE BSTKD_E = LT_BDC-BSTKD_E
                                                          AND VBELN   = LT_BDC-VBELN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_CREAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_CREAT .
* ALV Container
  CREATE OBJECT GO_DOCKING
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = '6000'.

*화면 분할
  CREATE OBJECT GO_SPLITTER
    EXPORTING
      PARENT  = GO_DOCKING
      ROWS    = 1
      COLUMNS = 1.

**1행 1열
*  CALL METHOD GO_SPLITTER->GET_CONTAINER
*    EXPORTING
*      ROW       = 1
*      COLUMN    = 1
*    RECEIVING
*      CONTAINER = GO_PARENT_TOP.
*
**1행 높이
*  CALL METHOD GO_SPLITTER->SET_ROW_HEIGHT
*    EXPORTING
*      ID     = 1
*      HEIGHT = 10.

* 2행 1열.
  CALL METHOD GO_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GO_ALV_CON.

  CREATE OBJECT GO_ALV_GRID
    EXPORTING
      I_PARENT          = GO_ALV_CON
      I_APPL_EVENTS     = 'X'
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_FIELD_CATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_FIELD_CATALOG .
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
*  #10 사이즈
*  #11 필드타이틀
*  #12 금액참조필드
*  #13 수량참조필드
*  #14 FIELDNAME
*  #15 TABNAME
*  #16 F4
*  #17 NO_ZERO
  CLEAR: GS_FIELDCAT, GV_POS.

  PERFORM REBUILD_LVC_CATALOG TABLES GT_FIELDCAT USING:
*#1          #2  #3   #4   #5   #6   #7    #8   #9    #10    #11      #12      #13     #14         #15     #16  #17
'ICON'     'X' 'C'  ' '   ' '  ' '  ' '  ' '   ''    '10'   TEXT-F01  ' '      ' '     ''          ''       ''  '',
'MSG'      'X' 'L'  ' '   ' '  ' '  ' '  ' '   ''    '45'   TEXT-F02  ' '      ' '     ''          ''       ''  '',
'BSTKD_E'  'X' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F03  ' '      ' '     'BSTKD_E'   'VBKD'   ''  '',
'VBELN'    'X' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F04  ' '      ' '     'VBELN'     'VBAP'   ''  '',
'POSNR'    'X' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F05  ' '      ' '     'POSNR'     'VBAP'   ''  '',
'MATNR'    'X' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F06  ' '      ' '     'MATNR'     'VBAP'   ''  '',
'MAKTX'    'X' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F14  ' '      ' '     'MAKTX'     'MAKT'   ''  '',
'MBDAT_S'  ' ' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F15  ' '      ' '     'MBDAT'     'VBEP'   ''  '',
'VBELN_D'  ' ' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F07  ' '      ' '     'VBELN'     'LIKP'   ''  '',
'LFDAT'    ' ' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F08  ' '      ' '     'LFDAT'     'LIKP'   ''  '',
'WADAT'    ' ' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F09  ' '      ' '     'WADAT'     'LIKP'   ''  '',
'MBDAT'    ' ' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F10  ' '      ' '     'MBDAT'     'LIPS'   ''  '',
'ROUTE'    ' ' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F11  ' '      ' '     'ROUTE'     'LIKP'   ''  '',
'LFDAT_EX' ' ' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F12  ' '      ' '     'LFDAT'     'LIKP'   ''  '',
'ROUTE_EX' ' ' 'L'  ' '   ' '  ' '  ' '  ' '   ''    ' '    TEXT-F13  ' '      ' '     'ROUTE'     'LIKP'   ''  ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_LAYOUT .
  CLEAR: GS_LAYOUT.
  GS_LAYOUT-CWIDTH_OPT = ABAP_ON.
  GS_LAYOUT-ZEBRA      = ABAP_ON.
  GS_LAYOUT-SEL_MODE   = 'A'.
  GS_LAYOUT-STYLEFNAME = 'STYLE'.
  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_TOOLBAR .
  CLEAR: GT_EXCLUDE.

  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO GT_EXCLUDE. "Undo
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO GT_EXCLUDE. "Local: Copy
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO GT_EXCLUDE. "Local: Copy Row
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO GT_EXCLUDE. "Local: Paste
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO GT_EXCLUDE. "Local: Cut
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO GT_EXCLUDE. "Local: DeleteRow
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO GT_EXCLUDE. "Local: InsertRow
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW      TO GT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO GT_EXCLUDE. "Local: AppendRow
  APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO GT_EXCLUDE. "Refresh
*  APPEND CL_GUI_ALV_GRID=>MC_FC_INFO              TO GT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_HTML              TO GT_EXCLUDE.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL          TO GT_EXCLUDE. "Exclude
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_EVENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_EVENT .
  " 데이터 변경시 DATA CHANGED 이벤트 호출
  CALL METHOD GO_ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT GO_EVENT_RECEIVER .
  SET HANDLER:
      GO_EVENT_RECEIVER->TOP_OF_PAGE                  FOR GO_ALV_GRID,
      GO_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK          FOR GO_ALV_GRID,
      GO_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK         FOR GO_ALV_GRID,
      GO_EVENT_RECEIVER->HANDLE_DATA_CHANGED          FOR GO_ALV_GRID,
      GO_EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR GO_ALV_GRID,
      GO_EVENT_RECEIVER->HANDLE_USER_COMMAND          FOR GO_ALV_GRID,
      GO_EVENT_RECEIVER->HANDLE_TOOLBAR               FOR GO_ALV_GRID,
      GO_EVENT_RECEIVER->HANDLE_ONF4_1                FOR GO_ALV_GRID.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HTML
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_HTML .
  CREATE OBJECT GO_HTML_CNTRL
    EXPORTING
      PARENT = GO_PARENT_TOP.

* Create TOP-Document
  CREATE OBJECT GO_DYNDOC_ID
    EXPORTING
      STYLE = 'AVL_GRID'.

* TOP_OF_PAGE
  CALL METHOD GO_DYNDOC_ID->INITIALIZE_DOCUMENT.

* PROCESSING EVENT
  CALL METHOD GO_ALV_GRID->LIST_PROCESSING_EVENTS
    EXPORTING
      I_EVENT_NAME = 'TOP_OF_PAGE'
      I_DYNDOC_ID  = GO_DYNDOC_ID.

  GO_DYNDOC_ID->HTML_CONTROL = GO_HTML_CNTRL.

* Display document
  CALL METHOD GO_DYNDOC_ID->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = GO_PARENT_TOP
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_DISPLAY .
  "TOP ALV
  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-USERNAME = SY-UNAME.
  GS_VARIANT-HANDLE = '0001'.

  CALL METHOD GO_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = GS_LAYOUT
      I_SAVE                        = 'A'
      IS_VARIANT                    = GS_VARIANT
      IT_TOOLBAR_EXCLUDING          = GT_EXCLUDE
    CHANGING
      IT_OUTTAB                     = GT_DISP[]
      IT_FIELDCATALOG               = GT_FIELDCAT
      IT_SORT                       = GT_SORT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_REFRESH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_REFRESH .
  DATA: LS_STABLE TYPE LVC_S_STBL.

  LS_STABLE-ROW = 'X'.
  LS_STABLE-COL = 'X'.

  CALL METHOD GO_ALV_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REBUILD_LVC_CATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_FIELDCAT
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> TEXT_F01
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
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
                                 P_OUTPUTLEN  "사이즈
                                 P_TITLE      "필드타이틀
                                 P_CFIELDNAME "금액참조필드
                                 P_QFIELDNAME "수량참조필드
                                 P_FNAME      "Reference Field
                                 P_TABNAME    "Reference Table
                                 P_F4AVAILABL "F4
                                 P_NO_ZERO.   "NO ZERO
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
        P_OUTPUTLEN  TO GS_FIELDCAT-OUTPUTLEN,
        P_CFIELDNAME TO GS_FIELDCAT-CFIELDNAME, "금액참조필드
        P_QFIELDNAME TO GS_FIELDCAT-QFIELDNAME, "수량참조필드
        P_FNAME      TO GS_FIELDCAT-REF_FIELD,  "Reference Field
        P_TABNAME    TO GS_FIELDCAT-REF_TABLE,  "Reference Table
        P_F4AVAILABL TO GS_FIELDCAT-F4AVAILABL, "F4
        P_NO_ZERO    TO GS_FIELDCAT-NO_ZERO,    "NO ZERO

        GV_POS       TO GS_FIELDCAT-COL_POS.    "순번

  "타이틀은 따로 입력된 경우만 적용
  MOVE: P_TITLE TO GS_FIELDCAT-COLTEXT,    "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_L,  "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_M,  "필드타이틀
        P_TITLE TO GS_FIELDCAT-SCRTEXT_S.  "필드타이틀

  APPEND GS_FIELDCAT TO PT_FIELDCAT.
  CLEAR  GS_FIELDCAT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_BDC1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EXECUTE_BDC1.
  DATA : LV_UNLOCK TYPE CHAR1.
  DATA : LV_LFDAT(10).
  DATA : LT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

  CLEAR : GT_BDC, GT_BDC[], GT_MSG, GT_MSG[].
  CLEAR : CTU_PARAMS.
  CTU_PARAMS-DISMODE = 'N'.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      DATE_INTERNAL = LT_BDC-LFDAT_EX
    IMPORTING
      DATE_EXTERNAL = LV_LFDAT.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '4004',
                                 ' ' 'BDC_OKCODE'  '=HDET_T',
                                 ' ' 'LIKP-VBELN'  LT_BDC-VBELN_D.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '2000',
                                 ' ' 'BDC_OKCODE'  '=T\01',
                                 ' ' 'LIKP-LFDAT'  LV_LFDAT.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '2000',
                                 ' ' 'BDC_OKCODE'  '=NEUT_T'.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '2000',
                                 ' ' 'BDC_OKCODE'  '=SICH_T'.

  CALL TRANSACTION 'VL02N' USING GT_BDC
                           OPTIONS FROM CTU_PARAMS
                           MESSAGES INTO GT_MSG.

  READ TABLE GT_MSG WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC EQ 0. "BDC 실패
    LT_BDC-ICON = ICON_LED_RED.

    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        ID   = GT_MSG-MSGID
        LANG = GT_MSG-MSGSPRA
        NO   = GT_MSG-MSGNR
        V1   = GT_MSG-MSGV1
      IMPORTING
        MSG  = LT_BDC-MSG.
  ELSE.
    "S/O SCHEDULINE DATE 변경
    DO 10 TIMES .
      CLEAR : LV_UNLOCK.
      WAIT UP TO '0.2' SECONDS.

      CALL FUNCTION 'ENQUEUE_EVVBAKE'
        EXPORTING
          VBELN          = LT_BDC-VBELN
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.
      IF SY-SUBRC EQ 0.
        LV_UNLOCK = 'X'.
        EXIT.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'DEQUEUE_EVVBAKE'
      EXPORTING
*       MODE_VBAK       = 'E'
*       MANDT = SY-MANDT
        VBELN = LT_BDC-VBELN
*       X_VBELN         = ' '
*       _SCOPE          = '3'
*       _SYNCHRON       = ' '
*       _COLLECT        = ' '
      .

    IF LV_UNLOCK = 'X'.
      CLEAR : LT_RETURN, LT_RETURN[].
      CALL FUNCTION 'ZSD_CHANGE_SO_SCHEDULINE_DATE'
        EXPORTING
          I_VBELN    = LT_BDC-VBELN_D
          I_VBELN_SO = LT_BDC-VBELN
          I_LFDAT_EX = LT_BDC-LFDAT_EX
        TABLES
          T_RETURN   = LT_RETURN.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_BDC2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EXECUTE_BDC2 .
  DATA : LV_UNLOCK TYPE CHAR1.
  DATA : LV_LFDAT(10).
  DATA : LT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

  CLEAR : GT_BDC, GT_BDC[], GT_MSG, GT_MSG[].
  CLEAR: CTU_PARAMS.
  CTU_PARAMS-DISMODE = 'N'.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      DATE_INTERNAL = LT_BDC-LFDAT_EX
    IMPORTING
      DATE_EXTERNAL = LV_LFDAT.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '4004',
                                 ' ' 'BDC_OKCODE'  '=HDET_T',
                                 ' ' 'LIKP-VBELN'  LT_BDC-VBELN_D.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '2000',
                                 ' ' 'BDC_OKCODE'  '=T\04'.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '2000',
                                 ' ' 'LIKP-ROUTE'  LT_BDC-ROUTE_EX.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '2000',
                                 ' ' 'BDC_OKCODE'  '=T\01',
                                 ' ' 'LIKP-LFDAT'  LV_LFDAT.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '2000',
                                 ' ' 'BDC_OKCODE'  '=NEUT_T'.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV50A'    '2000',
                                 ' ' 'BDC_OKCODE'  '=SICH_T'.

  CALL TRANSACTION 'VL02N' USING GT_BDC
                           OPTIONS FROM CTU_PARAMS
                           MESSAGES INTO GT_MSG.

  READ TABLE GT_MSG WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC EQ 0. "BDC 실패
    LT_BDC-ICON = ICON_LED_RED.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        ID   = GT_MSG-MSGID
        LANG = GT_MSG-MSGSPRA
        NO   = GT_MSG-MSGNR
        V1   = GT_MSG-MSGV1
      IMPORTING
        MSG  = LT_BDC-MSG.
  ELSE.
    "S/O SCHEDULINE DATE 변경
    DO 10 TIMES .
      CLEAR : LV_UNLOCK.
      WAIT UP TO '0.2' SECONDS.
      CALL FUNCTION 'ENQUEUE_EVVBAKE'
        EXPORTING
          VBELN          = LT_BDC-VBELN
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.
      IF SY-SUBRC EQ 0.
        LV_UNLOCK = 'X'.
        EXIT.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'DEQUEUE_EVVBAKE'
      EXPORTING
*       MODE_VBAK       = 'E'
*       MANDT = SY-MANDT
        VBELN = LT_BDC-VBELN
*       X_VBELN         = ' '
*       _SCOPE          = '3'
*       _SYNCHRON       = ' '
*       _COLLECT        = ' '
      .

    IF LV_UNLOCK = 'X'.
      CLEAR : LT_RETURN, LT_RETURN[].
      CALL FUNCTION 'ZSD_CHANGE_SO_SCHEDULINE_DATE'
        EXPORTING
          I_VBELN    = LT_BDC-VBELN_D
          I_VBELN_SO = LT_BDC-VBELN
          I_LFDAT_EX = LT_BDC-LFDAT_EX
        TABLES
          T_RETURN   = LT_RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_BDCDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BDCDATA
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM APPEND_BDCDATA TABLES PT_BDCDATA  STRUCTURE BDCDATA
                    USING  PV_DYNBEGIN
                           PV_FNAM
                           PV_FVAL.

  DATA: WA_BDCDATA TYPE BDCDATA.
  CLEAR WA_BDCDATA.
  IF PV_DYNBEGIN EQ 'X'.
    WA_BDCDATA-DYNBEGIN = 'X'.
    WA_BDCDATA-PROGRAM  = PV_FNAM.
    WA_BDCDATA-DYNPRO   = PV_FVAL.
  ELSE.
    WA_BDCDATA-FNAM     = PV_FNAM.
    WA_BDCDATA-FVAL     = PV_FVAL.
  ENDIF.
  APPEND WA_BDCDATA TO PT_BDCDATA.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_APPEND_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM BDC_APPEND_DATA  USING    VALUE(P_DYNBEGIN)
                               VALUE(P_NAME)
                               VALUE(P_VALUE).
  CLEAR :GT_BDC.

  IF P_DYNBEGIN = 'X'.
    GT_BDC-DYNBEGIN = P_DYNBEGIN.
    GT_BDC-PROGRAM  = P_NAME.
    GT_BDC-DYNPRO   = P_VALUE.
  ELSE.
    GT_BDC-FNAM = P_NAME.
    GT_BDC-FVAL = P_VALUE.
  ENDIF.

  APPEND GT_BDC.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_ALV .
  CLEAR : GT_DISP, GT_DISP[].
  PERFORM DATA_FROM_EXCEL.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM DOUBLE_CLICK  USING    P_ROW
                            P_COLUMN.

  READ TABLE GT_DISP INDEX P_ROW.

  CASE P_COLUMN.
    WHEN 'VBELN'. "S/O
      CHECK GT_DISP-VBELN IS NOT INITIAL.
      SET PARAMETER ID 'AUN' FIELD GT_DISP-VBELN.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    WHEN 'VBELN_D'. "Delivery
      CHECK GT_DISP-VBELN IS NOT INITIAL.
      SET PARAMETER ID 'VL' FIELD GT_DISP-VBELN_D.
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
  ENDCASE.
ENDFORM.
