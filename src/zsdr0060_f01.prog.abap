*&---------------------------------------------------------------------*
*& Include          ZSDR0060_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INIT
*&---------------------------------------------------------------------*
FORM INIT .

  GS_FUNCTEXT-ICON_ID   = ICON_XLS.
  GS_FUNCTEXT-QUICKINFO = TEXT-004.
  GS_FUNCTEXT-ICON_TEXT = TEXT-004.
  SSCRFIELDS-FUNCTXT_01  = GS_FUNCTEXT.

*  P_TYPE = 'ZFR4'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .

  DATA: LT_SELC LIKE TABLE OF GT_DATA WITH HEADER LINE.

  SELECT A~TDLNR, A~ADD02, A~KUNWE, A~DATAB, A~DATBI, "A~KSCHL,
         K~KBETR, K~KONWA
    FROM A908 AS A JOIN KONP AS K           "A908로 변경
                     ON A~KNUMH EQ K~KNUMH
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA
    WHERE A~DATAB    LE @P_DATAB
      AND A~DATBI    GE @P_DATAB
      AND A~KSCHL    EQ 'ZFR4'
      AND A~TDLNR    IN @S_TDLNR
      AND A~ADD02    IN @S_ADD02
      AND A~KUNWE    IN @S_KUNWE
      AND K~LOEVM_KO EQ @C_NON
      .



* For Surcharge Amount
  SELECT A~TDLNR, A~ADD02, A~KUNWE, A~DATAB, A~DATBI, "A~KSCHL,
         K~KBETR, K~KONWA
    FROM A908 AS A JOIN KONP AS K            "A908로 변경
                     ON A~KNUMH EQ K~KNUMH
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA1
    WHERE A~DATAB    LE @P_DATAB
      AND A~DATBI    GE @P_DATAB
*      AND A~TDLNR    EQ @P_DATAB
      AND A~KSCHL    EQ 'ZFR5'
      AND A~TDLNR    IN @S_TDLNR
      AND A~ADD02    IN @S_ADD02
      AND A~KUNWE    IN @S_KUNWE
      AND K~LOEVM_KO EQ @C_NON
      .
  SORT GT_DATA1 BY TDLNR ADD02 KUNWE.


  PERFORM GET_TEXT.

*  SELECT A~KUNWE, A~KSCHL, A~DATAB, A~DATBI,
*         K~KBETR, K~KONWA, K~KPEIN, K~KMEIN
*    FROM A922 AS A JOIN KONP AS K ON A~KNUMH EQ K~KNUMH
*    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA
*    WHERE A~TPLST    EQ @P_TPLST
*      AND A~DATAB    LE @P_DATAB
*      AND A~DATBI    GE @P_DATAB
*      AND K~LOEVM_KO EQ @C_NON.
*
*  PERFORM GET_TEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .
  DATA: LT_LIST  TYPE VRM_VALUES WITH HEADER LINE.

  IF P_SEAR = C_X.
    LOOP AT SCREEN.
*      CASE SCREEN-GROUP1.
*        WHEN 'GR1'.
*          SCREEN-ACTIVE = 0.
*      ENDCASE.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-ACTIVE = 0.
      ELSEIF SCREEN-NAME = 'P_TYPE'.
        SCREEN-REQUIRED = 0. "필수 입력 X
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    " LISTBOX 구성
*S_2021/2/23 comment out by E00064
*    CLEAR: LT_LIST, LT_LIST[].
*    LT_LIST-KEY = 'ZFR4'.
*    LT_LIST-TEXT = 'Base Freight'.
*    APPEND LT_LIST.
*
*    LT_LIST-KEY = 'ZFR5'.
*    LT_LIST-TEXT = 'Surcharge'.
*    APPEND LT_LIST.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID     = 'P_TYPE'
*        VALUES = LT_LIST[].
*E_2021/2/23 comment out by E00064

    LOOP AT SCREEN.
*      CASE SCREEN-GROUP1.
*        WHEN 'GR3'.
*          SCREEN-ACTIVE = 0.
*      ENDCASE.
      IF SCREEN-GROUP1 = 'GR3'.
        SCREEN-ACTIVE = 0.
      ELSEIF SCREEN-NAME = 'P_TYPE'.
        SCREEN-REQUIRED = 2. "필수 입력
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_CREATE_DOCKING_CONTAINER
*&---------------------------------------------------------------------*
FORM ALV_CREATE_DOCKING_CONTAINER .
  CREATE OBJECT GR_DOCKING_CONTAINER
    EXPORTING
      EXTENSION                   = 3000
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  CREATE OBJECT GR_SPLITTER
    EXPORTING
      PARENT  = GR_DOCKING_CONTAINER
      ROWS    = 2
      COLUMNS = 1.
  CALL METHOD GR_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 1
      COLUMN    = 1
    RECEIVING
      CONTAINER = GR_PARENT_HTML.

  CALL METHOD GR_SPLITTER->GET_CONTAINER
    EXPORTING
      ROW       = 2
      COLUMN    = 1
    RECEIVING
      CONTAINER = GR_PARENT_GRID.

  CALL METHOD GR_SPLITTER->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 12.

ENDFORM.                    "ALV_CREATE_DOCKING_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  ALV_CREATE_GRID
*&---------------------------------------------------------------------*
FORM ALV_CREATE_GRID .
  CREATE OBJECT GR_ALV_GRID
    EXPORTING
      I_PARENT          = GR_PARENT_GRID
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.


ENDFORM.                    "ALV_CREATE_GRID
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM ALV_FIELD_CATALOG .

  PERFORM APPEND_FIELD_CATALOG USING :
*  FNAME         KEY    OUTPUTLEN  COLTEXT      EDIT     CFIELD  REF_TAB  REF_FIELD
'ICON'           C_X       '40'     TEXT-C01      ' '       ' '      ' '       ' '          ' ',
'MSG'            C_X       '50'     TEXT-C02      ' '       ' '      ' '       ' '          ' ',
'TDLNR'          C_X       '10'     TEXT-C08      ' '       ' '      'A908'    'TDLNR'      ' ',
'TDLNR_T'        C_X       '10'     TEXT-C08      ' '       ' '      'NAME1'   'LFA1'       ' ',
'ADD02'          C_X       '10'     TEXT-C21      ' '       ' '      'A908'    'ADD02'       ' ',
'KUNWE'          C_X       '10'     TEXT-C03      ' '       ' '      'A908'    'KUNWE'      ' ',
'KUNWE_T'        ' '       '50'     TEXT-C03      ' '       ' '      ' '       ' '          ' ',
*'KSCHL'          ' '       '4'      TEXT-C04      ' '       ' '      'A908'    'KSCHL'      ' ',
*'KSCHL_T'        ' '       '10'     TEXT-C04      ' '       ' '      'VTEXT'   'T686T'      ' ',
'KBETR'          ' '       '11'     TEXT-C05      ' '       'KONWA'  'KONP'    'KBETR'      ' ',
'KBETR1'         ' '       '11'     TEXT-C15      ' '       'KONWA'  'KONP'    'KBETR'      ' ',
'KONWA'          ' '       '5'      TEXT-C06      ' '       ' '      'KONP'    'KONWA'      ' ',
'DATAB'          ' '       '8'      TEXT-C09      ' '       ' '      'A908'    'DATAB'      ' ',
'DATBI'          ' '       '8'      TEXT-C10      ' '       ' '      'A908'    'DATBI'      ' ',
'DATAB1'         ' '       '8'      TEXT-C19      ' '       ' '      'A908'    'DATAB'      ' ',
'DATBI1'         ' '       '8'      TEXT-C20      ' '       ' '      'A908'    'DATBI'      ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM APPEND_FIELD_CATALOG  USING P_FNAME
                                 P_KEY
                                 P_OUTPUTLEN
                                 COLTEXT
                                 P_EDIT
                                 P_CFIELDNAME
                                 P_REF_TABLE
                                 P_REF_FIELD
                                 P_CURRENCY.
*                                 P_DECIMALS_O.

  IF ( P_FNAME EQ 'KSCHL' OR P_FNAME EQ 'KSCHL_T' ) AND P_UPLO EQ 'X'.
    EXIT.
  ENDIF.

  DATA: LS_FCAT TYPE LVC_S_FCAT.
  LS_FCAT-FIELDNAME  = P_FNAME.
  LS_FCAT-KEY        = P_KEY.
  LS_FCAT-OUTPUTLEN  = P_OUTPUTLEN.
  LS_FCAT-COLTEXT    = COLTEXT.
  LS_FCAT-EDIT       = P_EDIT.
  LS_FCAT-QFIELDNAME = P_CFIELDNAME.
  LS_FCAT-REF_TABLE  = P_REF_TABLE.
  LS_FCAT-REF_FIELD  = P_REF_FIELD.
  LS_FCAT-CURRENCY   = P_CURRENCY.
*  LS_FCAT-DECIMALS_O = P_DECIMALS_O.
  APPEND LS_FCAT TO GT_FCAT.

ENDFORM.                    " APPEND_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
FORM ALV_LAYOUT USING PS_LAYOUT TYPE LVC_S_LAYO
                       P_OPT
                       P_ZEBRA
                       P_BOX
                       P_SEL_MODE
                       P_INFO_FNAME
                       P_TITLE
                       P_CTAB
                       P_STYLE.

  CLEAR  : PS_LAYOUT.
  PS_LAYOUT-CWIDTH_OPT = P_OPT.
  PS_LAYOUT-ZEBRA = P_ZEBRA.
  PS_LAYOUT-BOX_FNAME = P_BOX.
  PS_LAYOUT-SEL_MODE = P_SEL_MODE.
  PS_LAYOUT-INFO_FNAME = P_INFO_FNAME.
  PS_LAYOUT-GRID_TITLE = P_TITLE.
  PS_LAYOUT-CTAB_FNAME = P_CTAB.
  PS_LAYOUT-NO_ROWINS = C_X.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV_EXCLUDE_TOOLBAR
*&---------------------------------------------------------------------*
FORM ALV_EXCLUDE_TOOLBAR .

  DEFINE _EXCLUDE_TOOLBAR.
    gs_excluding = cl_gui_alv_grid=>&1.
    APPEND gs_excluding TO gt_excluding.
  END-OF-DEFINITION.

  _EXCLUDE_TOOLBAR MC_FC_DETAIL.           " 세부사항
*  _EXCLUDE_TOOLBAR MC_FC_CHECK.             " 엔트리 점검
*  _EXCLUDE_TOOLBAR MC_FC_REFRESH.           " 최신표시
*  _EXCLUDE_TOOLBAR MC_FC_LOC_CUT.           " 잘라내기
*  _EXCLUDE_TOOLBAR MC_FC_LOC_COPY.          " 텍스트복사
*  _EXCLUDE_TOOLBAR MC_FC_LOC_PASTE.         " 겹쳐쓰기로 삽입
*  _EXCLUDE_TOOLBAR MC_FC_LOC_PASTE_NEW_ROW. " 신규행에 삽입
*  _EXCLUDE_TOOLBAR MC_FC_LOC_UNDO.          " 실행취소
*  _EXCLUDE_TOOLBAR MC_FC_LOC_APPEND_ROW.    " 행 추가
*  _EXCLUDE_TOOLBAR MC_FC_LOC_INSERT_ROW.    " 행 삽입
*  _EXCLUDE_TOOLBAR MC_FC_LOC_DELETE_ROW.    " 행 삭제
*  _EXCLUDE_TOOLBAR MC_FC_LOC_COPY_ROW.      " 행 복제
*  _EXCLUDE_TOOLBAR MC_FC_SORT.              " SORT
  _EXCLUDE_TOOLBAR MC_FC_SORT_ASC.          " 오름차순 정렬
  _EXCLUDE_TOOLBAR MC_FC_SORT_DSC.          " 내림차순 정렬
  _EXCLUDE_TOOLBAR MC_FC_FIND.             " 찾기
  _EXCLUDE_TOOLBAR MC_MB_FILTER.            " 필터설정
  _EXCLUDE_TOOLBAR MC_FC_SUM.               " 총계
  _EXCLUDE_TOOLBAR MC_MB_SUBTOT.            " SUBTOT
  _EXCLUDE_TOOLBAR MC_FC_AVERAGE.           " 평균
  _EXCLUDE_TOOLBAR MC_FC_MINIMUM.           " 최소
  _EXCLUDE_TOOLBAR MC_FC_MAXIMUM.           " 최대
*  _EXCLUDE_TOOLBAR MC_FC_PRINT.            " 인쇄
*  _EXCLUDE_TOOLBAR MC_FC_VIEWS.            " 뷰
*  _EXCLUDE_TOOLBAR MC_FC_VIEWS_GRID.       " 뷰 - 그리드
*  _EXCLUDE_TOOLBAR MC_FC_VIEWS_EXCEL.      " 뷰 - 엑셀 통합
*  _EXCLUDE_TOOLBAR MC_FC_VIEWS_LOTUS.      " 뷰 - LOTUS
*  _EXCLUDE_TOOLBAR MC_MB_EXPORT.           " EXPORT
*  _EXCLUDE_TOOLBAR MC_PC_FILE.             " EXPORT - 로컬파일
*  _EXCLUDE_TOOLBAR MC_TO_OFFICE.           " EXPORT - OFFICE
*  _EXCLUDE_TOOLBAR MC_FC_GRAPH.             " 그래프 조회
*  _EXCLUDE_TOOLBAR MC_FC_INFO.              " 사용자 문서
*  _EXCLUDE_TOOLBAR MC_MB_VARIANT.          " 레이아웃 변경
*  _EXCLUDE_TOOLBAR MC_FC_EXCL_ALL.         " 툴바 전체 삭제

** 툴바 전부 삭제
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.


ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM ALV_DISPLAY .

  CALL METHOD GR_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     I_BUFFER_ACTIVE               =
*     I_BYPASSING_BUFFER            =
*     I_CONSISTENCY_CHECK           =
*     I_STRUCTURE_NAME              =
*     IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = C_X
      I_DEFAULT                     = C_X
      IS_LAYOUT                     = GS_LAYOUT
*     IS_PRINT                      =
*     IT_SPECIAL_GROUPS             =
      IT_TOOLBAR_EXCLUDING          = GT_EXCLUDING
*     IT_HYPERLINK                  =
*     IT_ALV_GRAPHICS               =
*     IT_EXCEPT_QINFO               =
*     IR_SALV_ADAPTER               =
    CHANGING
      IT_OUTTAB                     = GT_DATA[]
      IT_FIELDCATALOG               = GT_FCAT
*     IT_SORT                       = GT_SORT.
*     IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_REFRESH
*&---------------------------------------------------------------------*
FORM ALV_REFRESH .
  DATA: LS_STABLE TYPE LVC_S_STBL.
  LS_STABLE-ROW = C_X.
  LS_STABLE-COL = C_X.
  CALL METHOD GR_ALV_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = LS_STABLE
      I_SOFT_REFRESH = C_X.
ENDFORM.                    " ALV_REFRESH
*&---------------------------------------------------------------------*
*&      Form  EXCEL_TEMPLATE_DOWN
*&---------------------------------------------------------------------*
FORM EXCEL_TEMPLATE_DOWN .

  DATA: LV_ANSWER   TYPE CHAR1,
        LV_FILE     TYPE STRING,
        LV_PATH     TYPE STRING,
        LV_FULLPATH TYPE STRING,
        LV_ACTION   TYPE I.

  DATA: BEGIN OF LS_DOCINFO,
          TABLE      LIKE W3MIME OCCURS 0,
          SIZE       TYPE I,
          TYPE(80)   VALUE GV_SOI_DOCTYPE_WORD97_DOCUMENT,
          FORMAT(80) TYPE C,
        END OF LS_DOCINFO.

  DATA : LV_OBJECT(15).

  CLEAR : LV_OBJECT, LV_FILE.

  LV_OBJECT = TEXT-008."'ZSDR0060'.
  LV_FILE = LV_OBJECT.
*- MIME DOWNLOAD  " SMWO에 엑셀 다운로드할 파일을 지정 후 작성.
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING
      OBJECT_ID        = LV_OBJECT
    IMPORTING
      DATA_SIZE        = LS_DOCINFO-SIZE
      DOCUMENT_FORMAT  = LS_DOCINFO-FORMAT
      DOCUMENT_TYPE    = LS_DOCINFO-TYPE
    TABLES
      DATA_TABLE       = LS_DOCINFO-TABLE
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      INTERNAL_ERROR   = 2
      OTHERS           = 3.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE 'E'
        NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

*- FILENAME GET
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE         = 'SAVE AS'
      DEFAULT_EXTENSION    = 'XLS'
      DEFAULT_FILE_NAME    = LV_FILE
      FILE_FILTER          = '*.XLS'
    CHANGING
      FILENAME             = LV_FILE
      PATH                 = LV_PATH
      FULLPATH             = LV_FULLPATH
      USER_ACTION          = LV_ACTION
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
    EXIT.
  ENDIF.

*- 취소 버튼누른 경우 (SAVE AS)
  IF LV_ACTION NE 0.
    EXIT.
  ENDIF.

*- FILE DOWNLOAD
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
    EXPORTING
      BIN_FILESIZE            = LS_DOCINFO-SIZE
      FILENAME                = LV_FULLPATH
      FILETYPE                = 'BIN'
    CHANGING
      DATA_TAB                = LS_DOCINFO-TABLE
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      NOT_SUPPORTED_BY_GUI    = 22
      ERROR_NO_GUI            = 23
      OTHERS                  = 24.
  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.                    " EXCEL_TEMPLATE_DOWN
*&---------------------------------------------------------------------*
*&      Form  GET_EXCEL_NAME
*&---------------------------------------------------------------------*
FORM GET_EXCEL_NAME  USING    P_P_FILE.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_PATH         = 'C:\'
      MASK             = '*.XLS.'
      MODE             = 'O'
      TITLE            = 'File'
    IMPORTING
      FILENAME         = P_P_FILE
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4.
ENDFORM.                    " GET_EXCEL_NAME
*&---------------------------------------------------------------------*
*&      Form  EXCEL_UPLOAD
*&---------------------------------------------------------------------*
FORM EXCEL_UPLOAD.

  DATA : LV_CURR(3) TYPE C.

  I_S_COL = 1.
  I_S_ROW = 2.
  I_E_COL = 250.
  I_E_ROW = 65000.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = I_S_COL
      I_BEGIN_ROW             = I_S_ROW
      I_END_COL               = I_E_COL
      I_END_ROW               = I_E_ROW
    TABLES
      INTERN                  = I_XLS_DATA
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  SORT I_XLS_DATA BY ROW COL.

  LOOP AT I_XLS_DATA.

    CASE I_XLS_DATA-ROW.
      WHEN '0001'.
*        IF I_XLS_DATA-COL = '0004'. "default currency
        IF I_XLS_DATA-COL = '0006'. "default currency
          LV_CURR = I_XLS_DATA-VALUE.
        ENDIF.

        CONTINUE.
    ENDCASE.

    CASE I_XLS_DATA-COL.
*      WHEN '0003'." Amount
      WHEN '0004' OR '0005'." Amount
        REPLACE ALL OCCURRENCES OF ',' IN I_XLS_DATA-VALUE WITH C_NON.
*      WHEN '0005' OR '0006'."Valid Date
      WHEN '0007' OR '0008' OR '0009' OR '0010'."Valid Date
        REPLACE ALL OCCURRENCES OF ',' IN I_XLS_DATA-VALUE WITH C_NON.
        REPLACE ALL OCCURRENCES OF '/' IN I_XLS_DATA-VALUE WITH C_NON.
        REPLACE ALL OCCURRENCES OF '-' IN I_XLS_DATA-VALUE WITH C_NON.
        REPLACE ALL OCCURRENCES OF '.' IN I_XLS_DATA-VALUE WITH C_NON.
    ENDCASE.

    ASSIGN COMPONENT I_XLS_DATA-COL OF STRUCTURE GT_UPLOAD TO <I_FS0>.
    <I_FS0> = I_XLS_DATA-VALUE.

    AT END OF ROW.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GT_UPLOAD-KUNWE
        IMPORTING
          OUTPUT = GT_UPLOAD-KUNWE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GT_UPLOAD-TDLNR
        IMPORTING
          OUTPUT = GT_UPLOAD-TDLNR.

      IF GT_UPLOAD-ADD02(1) NE '0'.
        CONCATENATE '0' GT_UPLOAD-ADD02 INTO GT_UPLOAD-ADD02.
      ENDIF.

      APPEND  GT_UPLOAD.
      MOVE-CORRESPONDING GT_UPLOAD TO GT_DATA.

      IF GT_DATA-KONWA IS INITIAL.
        GT_DATA-KONWA = LV_CURR.     "Currency
      ENDIF.

      CASE ' '.
        WHEN GT_DATA-TDLNR. "Service Agent
          GT_DATA-ICON = ICON_LED_RED.
          GT_DATA-MSG = 'Fill out all required fields.'.
        WHEN GT_DATA-ADD02. " Trailer overall length
          GT_DATA-ICON = ICON_LED_RED.
          GT_DATA-MSG = 'Fill out all required fields.'.
        WHEN GT_DATA-KUNWE. "ShipTo
          GT_DATA-ICON = ICON_LED_RED.
          GT_DATA-MSG = 'Fill out all required fields.'.
        WHEN GT_DATA-KBETR. "Amount
*          IF P_TYPE = 'ZFR4'.
          GT_DATA-ICON = ICON_LED_RED.
          GT_DATA-MSG = 'Fill out all required fields.'.
*          ENDIF.
        WHEN GT_DATA-DATAB. "Validity start date
          GT_DATA-ICON = ICON_LED_RED.
          GT_DATA-MSG = 'Fill out all required fields.'.
        WHEN GT_DATA-DATBI. "Validity end date
          GT_DATA-ICON = ICON_LED_RED.
          GT_DATA-MSG = 'Fill out all required fields.'.
      ENDCASE.

      APPEND GT_DATA.
      CLEAR: GT_UPLOAD, GT_DATA.
    ENDAT.
  ENDLOOP.

  PERFORM GET_TEXT.

  CLEAR: GT_UPLOAD[].

*  i_s_col = 1.
*  i_s_row = 3.
*  i_e_col = 250.
*  i_e_row = 65000.
*
*  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*    EXPORTING
*      filename                = p_file
*      i_begin_col             = i_s_col
*      i_begin_row             = i_s_row
*      i_end_col               = i_e_col
*      i_end_row               = i_e_row
*    TABLES
*      intern                  = i_xls_data
*    EXCEPTIONS
*      inconsistent_parameters = 1
*      upload_ole              = 2
*      OTHERS                  = 3.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*  SORT i_xls_data BY row col.
*
*  LOOP AT i_xls_data.
*
*    CASE i_xls_data-col.
*      WHEN '0003'." Amount
*        REPLACE ALL OCCURRENCES OF ',' IN i_xls_data-value WITH c_non.
*      WHEN '0007' OR '0008'."Valid Date
*        REPLACE ALL OCCURRENCES OF ',' IN i_xls_data-value WITH c_non.
*        REPLACE ALL OCCURRENCES OF '/' IN i_xls_data-value WITH c_non.
*        REPLACE ALL OCCURRENCES OF '-' IN i_xls_data-value WITH c_non.
*        REPLACE ALL OCCURRENCES OF '.' IN i_xls_data-value WITH c_non.
*    ENDCASE.
*
*    ASSIGN COMPONENT i_xls_data-col OF STRUCTURE gt_upload TO <i_fs0>.
*    <i_fs0> = i_xls_data-value.
*
*    AT END OF row.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = gt_upload-kunwe
*        IMPORTING
*          output = gt_upload-kunwe.
*
*      APPEND  gt_upload.
*      MOVE-CORRESPONDING gt_upload TO gt_data.
*      IF gt_data-konwa IS INITIAL.
*        gt_data-konwa = c_usd.  "Currency
*      ENDIF.
*      IF gt_data-kpein IS INITIAL.
*        gt_data-kpein = c_1.    "Condition Pricing Unit
*      ENDIF.
*      IF gt_data-kmein IS INITIAL.
*        gt_data-kmein = c_m3."  "UOM
*      ENDIF.
*      APPEND gt_data.
*      CLEAR: gt_upload, gt_data.
*    ENDAT.
*  ENDLOOP.
*
*  PERFORM get_text.
*
*  CLEAR: gt_upload[].

ENDFORM.                    " EXCEL_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  ALV_EVENT
*&---------------------------------------------------------------------*
FORM ALV_EVENT .
  CALL METHOD GR_ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT GR_EVENT_HANDLER.
*  SET HANDLER gr_event_handler->handle_data_changed   FOR gr_alv_grid.
  SET HANDLER GR_EVENT_HANDLER->HANDLE_TOP_OF_PAGE    FOR GR_ALV_GRID.
*  SET HANDLER gr_event_handler->handle_toolbar        FOR gr_alv_grid.
*  SET HANDLER gr_event_handler->handle_user_command   FOR gr_alv_grid.
  SET HANDLER GR_EVENT_HANDLER->HANDLE_DOUBLE_CLICK   FOR GR_ALV_GRID.
*  SET HANDLER GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK  FOR GR_ALV_GRID.
*  SET HANDLER GR_EVENT_HANDLER->HANDLE_ON_F4          FOR GR_ALV_GRID.

ENDFORM.                    "ALV_EVENT
*&---------------------------------------------------------------------*
*&      Form  ALV_HTML
*&---------------------------------------------------------------------*
FORM ALV_HTML .
  CREATE OBJECT GR_HTML
    EXPORTING
      PARENT = GR_PARENT_HTML.

  CREATE OBJECT GR_DOCUMENT
    EXPORTING
      STYLE = 'ALV_GRID'.

  CALL METHOD GR_DOCUMENT->INITIALIZE_DOCUMENT.
  CALL METHOD GR_ALV_GRID->LIST_PROCESSING_EVENTS
    EXPORTING
      I_EVENT_NAME = 'TOP_OF_PAGE'
      I_DYNDOC_ID  = GR_DOCUMENT.

  GR_DOCUMENT->HTML_CONTROL = GR_HTML.

  CALL METHOD GR_DOCUMENT->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = C_X
      PARENT             = GR_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.
ENDFORM.                    " ALV_HTML
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HANDLE_TOP_OF_PAGE USING CL_DD TYPE REF TO CL_DD_DOCUMENT.
  DATA: L_TEXT(255)  TYPE C,
        L_TEXT1(255) TYPE C,
        L_NAME1      TYPE T001W-NAME1,
        LV_DATE(10)  TYPE C,
        LV_LOW(10)   TYPE C,
        LV_HIGH(10)  TYPE C.

  CONCATENATE TEXT-005  P_TPLST INTO L_TEXT SEPARATED BY SPACE. "Transportation Planning Point
  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  IF P_UPLO IS NOT INITIAL. "업로드 모드
    SELECT KSCHL, VTEXT
      FROM T685T
      INTO CORRESPONDING FIELDS OF TABLE @GT_T685T
     WHERE KAPPL EQ 'F'
       AND SPRAS EQ 'EN'.

*    CLEAR GT_T685T.
*    READ TABLE GT_T685T WITH KEY KSCHL = P_TYPE.
*
*    CLEAR L_TEXT.
*    CONCATENATE 'Condition type :' P_TYPE '('GT_T685T-VTEXT')' INTO L_TEXT SEPARATED BY SPACE.
*    CALL METHOD CL_DD->ADD_TEXT
*      EXPORTING
*        TEXT         = L_TEXT
**       sap_style    = cl_dd_document=>heading
**       sap_color    = cl_dd_document=>list_heading_int
*        SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
**       sap_emphasis = cl_dd_document=>strong
*        STYLE_CLASS  = SPACE.
*
*    CALL METHOD CL_DD->NEW_LINE
*      EXPORTING
*        REPEAT = 0.

    CLEAR L_TEXT.
    CONCATENATE TEXT-006 P_FILE INTO L_TEXT SEPARATED BY SPACE.  "File Path
    CALL METHOD CL_DD->ADD_TEXT
      EXPORTING
        TEXT         = L_TEXT
*       sap_style    = cl_dd_document=>heading
*       sap_color    = cl_dd_document=>list_heading_int
        SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*       sap_emphasis = cl_dd_document=>strong
        STYLE_CLASS  = SPACE.

  ELSE. "조회모드
    CLEAR L_TEXT.

    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        INPUT  = P_DATAB
      IMPORTING
        OUTPUT = LV_DATE.

    CONCATENATE 'Valid on :'  LV_DATE INTO L_TEXT SEPARATED BY SPACE. "Valid on
    CALL METHOD CL_DD->ADD_TEXT
      EXPORTING
        TEXT         = L_TEXT
*       sap_style    = cl_dd_document=>heading
*       sap_color    = cl_dd_document=>list_heading_int
        SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*       sap_emphasis = cl_dd_document=>strong
        STYLE_CLASS  = SPACE.


*-  Service Agent
    IF S_TDLNR[] IS NOT INITIAL.
      CALL METHOD CL_DD->NEW_LINE
        EXPORTING
          REPEAT = 0.

      READ TABLE S_TDLNR INDEX 1.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = S_TDLNR-LOW
        IMPORTING
          OUTPUT = LV_LOW.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = S_TDLNR-HIGH
        IMPORTING
          OUTPUT = LV_HIGH.

      IF S_TDLNR-HIGH IS INITIAL.
        CONCATENATE 'Service Agent :'  LV_LOW INTO L_TEXT
          SEPARATED BY SPACE.
      ELSE.
        CONCATENATE 'Service Agent :'  LV_LOW '~' LV_HIGH INTO L_TEXT
          SEPARATED BY SPACE.
      ENDIF.
      CALL METHOD CL_DD->ADD_TEXT
        EXPORTING
          TEXT         = L_TEXT
*         sap_style    = cl_dd_document=>heading
*         sap_color    = cl_dd_document=>list_heading_int
          SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*         sap_emphasis = cl_dd_document=>strong
          STYLE_CLASS  = SPACE.
    ENDIF.

*-  Shipto
    IF S_KUNWE[] IS NOT INITIAL.
      CALL METHOD CL_DD->NEW_LINE
        EXPORTING
          REPEAT = 0.

      READ TABLE S_KUNWE INDEX 1.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = S_KUNWE-LOW
        IMPORTING
          OUTPUT = LV_LOW.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = S_KUNWE-HIGH
        IMPORTING
          OUTPUT = LV_HIGH.

      IF S_KUNWE-HIGH IS INITIAL.
        CONCATENATE 'Shipto :'  LV_LOW INTO L_TEXT
          SEPARATED BY SPACE.
      ELSE.
        CONCATENATE 'Shipto :'  LV_LOW '~' LV_HIGH INTO L_TEXT
          SEPARATED BY SPACE.
      ENDIF.
      CALL METHOD CL_DD->ADD_TEXT
        EXPORTING
          TEXT         = L_TEXT
*         sap_style    = cl_dd_document=>heading
*         sap_color    = cl_dd_document=>list_heading_int
          SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*         sap_emphasis = cl_dd_document=>strong
          STYLE_CLASS  = SPACE.
    ENDIF.


  ENDIF.
ENDFORM.                    " HANDLE_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& Form PUSH_SAVE
*&---------------------------------------------------------------------*
FORM PUSH_SAVE .
  DATA: LT_ROWS TYPE LVC_T_ROW,
        LS_ROW  TYPE LVC_S_ROW.

  CLEAR: LT_ROWS, LS_ROW.

  CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS.

  IF LT_ROWS[] IS INITIAL.
    MESSAGE S014 DISPLAY LIKE C_E.
    EXIT.
  ENDIF.

  LOOP AT LT_ROWS INTO LS_ROW.
    READ TABLE GT_DATA INDEX LS_ROW-INDEX.
    IF SY-SUBRC EQ 0 AND GT_DATA-ICON NE ICON_LED_RED.
      PERFORM BDC_CREATE.
    ENDIF.

*    MODIFY GT_DATA INDEX LS_ROW-INDEX TRANSPORTING ICON MSG.
    MODIFY GT_DATA INDEX LS_ROW-INDEX TRANSPORTING ICON MSG DATAB1 DATBI1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_CREATE
*&---------------------------------------------------------------------*
FORM BDC_CREATE.
  DATA: LV_KBETR TYPE CHAR14,
        LV_DATAB TYPE A922-DATAB,
        LV_DATBI TYPE A922-DATBI.

  CLEAR : GT_BDC, GT_BDC[], GT_MSG, GT_MSG[], LV_DATAB, LV_DATBI.

* Base Freight
  PERFORM BDC_APPEND_DATA USING: 'X'  'SAPMV13A'   '0100',
                                 ' '  'BDC_CURSOR' 'RV13A-KSCHL',
                                 ' '  'BDC_OKCODE' '/00',
                                 ' '  'RV13A-KSCHL' 'ZFR4'.  "P_TYPE. " Base Freight

*  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV13A'   '1907',
*                                 ' ' 'BDC_CURSOR' 'KOMG-TPLST',
*                                 ' ' 'BDC_OKCODE' '/00',
*                                 ' ' 'KOMG-TPLST' P_TPLST,
*                                 ' ' 'KOMG-TDLNR' GT_DATA-TDLNR.

  CLEAR LV_KBETR.
  WRITE GT_DATA-KBETR TO LV_KBETR CURRENCY GT_DATA-KONWA.

  PERFORM DATE_SETTING USING GT_DATA-DATAB
                    CHANGING LV_DATAB.

  PERFORM DATE_SETTING USING GT_DATA-DATBI
                    CHANGING LV_DATBI.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV13A'       '1908',
*                                 ' ' 'BDC_CURSOR'     'KONP-KONWA(01)',
*                                 ' ' 'BDC_OKCODE'      '/00',
                                 ' ' 'KOMG-TPLST'      P_TPLST,
                                 ' ' 'KOMG-TDLNR'      GT_DATA-TDLNR,
                                 ' ' 'KOMG-ADD02'      GT_DATA-ADD02,
                                 ' ' 'KOMG-KUNWE(01)'  GT_DATA-KUNWE,
                                 ' ' 'KONP-KBETR(01)'  LV_KBETR,
                                 ' ' 'KONP-KONWA(01)'  GT_DATA-KONWA,
                                 ' ' 'RV13A-DATAB(01)' LV_DATAB,
                                 ' ' 'RV13A-DATBI(01)' LV_DATBI,
                                 ' ' 'BDC_OKCODE'      '=SICH'.

  CALL TRANSACTION 'TK11' USING GT_BDC
                           MODE C_N
*                           MODE C_A
                           MESSAGES INTO GT_MSG.

  READ TABLE GT_MSG WITH KEY MSGTYP = C_E.
  IF SY-SUBRC = 0.
    PERFORM GET_BDC_MESSAGE USING GT_MSG.
    GT_DATA-ICON = ICON_LED_RED.
    EXIT.
  ELSE.
    GT_DATA-ICON = ICON_LED_GREEN.
  ENDIF.


* Surcharge
  CLEAR : GT_BDC, GT_BDC[], GT_MSG, GT_MSG[], LV_DATAB, LV_DATBI.
  PERFORM BDC_APPEND_DATA USING: 'X'  'SAPMV13A'   '0100',
                                 ' '  'BDC_CURSOR' 'RV13A-KSCHL',
                                 ' '  'BDC_OKCODE' '/00',
                                 ' '  'RV13A-KSCHL' 'ZFR5'.  "P_TYPE. " Surcharge

*  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV13A'   '1907',
*                                 ' ' 'BDC_CURSOR' 'KOMG-TPLST',
*                                 ' ' 'BDC_OKCODE' '/00',
*                                 ' ' 'KOMG-TPLST' P_TPLST,
*                                 ' ' 'KOMG-TDLNR' GT_DATA-TDLNR.

  CLEAR LV_KBETR.
  WRITE GT_DATA-KBETR1 TO LV_KBETR CURRENCY GT_DATA-KONWA.

  IF GT_DATA-DATAB1 IS INITIAL. GT_DATA-DATAB1 =  GT_DATA-DATAB. ENDIF.
  PERFORM DATE_SETTING USING GT_DATA-DATAB1
                    CHANGING LV_DATAB.

  IF GT_DATA-DATBI1 IS INITIAL. GT_DATA-DATBI1 =  GT_DATA-DATBI. ENDIF.
  PERFORM DATE_SETTING USING GT_DATA-DATBI1
                    CHANGING LV_DATBI.

  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV13A'       '1908',
*                                 ' ' 'BDC_CURSOR'     'KONP-KONWA(01)',
*                                 ' ' 'BDC_OKCODE'      '/00',
                                 ' ' 'KOMG-TPLST'      P_TPLST,
                                 ' ' 'KOMG-TDLNR'      GT_DATA-TDLNR,
                                 ' ' 'KOMG-ADD02'      GT_DATA-ADD02,
                                 ' ' 'KOMG-KUNWE(01)'  GT_DATA-KUNWE,
                                 ' ' 'KONP-KBETR(01)'  LV_KBETR,
                                 ' ' 'KONP-KONWA(01)'  GT_DATA-KONWA,
                                 ' ' 'RV13A-DATAB(01)' LV_DATAB,
                                 ' ' 'RV13A-DATBI(01)' LV_DATBI,
                                 ' ' 'BDC_OKCODE'      '=SICH'.


  CALL TRANSACTION 'TK11' USING GT_BDC
                           MODE C_N
*                           MODE C_A
                           MESSAGES INTO GT_MSG.

  READ TABLE GT_MSG WITH KEY MSGTYP = C_E.
  IF SY-SUBRC = 0.
    PERFORM GET_BDC_MESSAGE USING GT_MSG.
    GT_DATA-ICON = ICON_LED_RED.
  ELSE.
    GT_DATA-ICON = ICON_LED_GREEN.
  ENDIF.

*  DATA: LV_KBETR TYPE CHAR14,
*        LV_KPEIN TYPE CHAR5,
*        LV_DATAB TYPE A922-DATAB,
*        LV_DATBI TYPE A922-DATBI.
*
*
*  CLEAR : GT_BDC, GT_BDC[], GT_MSG, GT_MSG[], LV_DATAB, LV_DATBI.
*
*  PERFORM BDC_APPEND_DATA USING: 'X'  'SAPMV13A' '0100',
*                                 ' '  'BDC_CURSOR' 'RV13A-KSCHL',
*                                 ' '  'BDC_OKCODE' '/00',
*                                 ' '  'RV13A-KSCHL' GT_DATA-KSCHL.
*
*  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV13A'   '1922',
*                                 ' ' 'BDC_CURSOR' 'KOMG-TPLST',
*                                 ' ' 'BDC_OKCODE' '/00',
*                                 ' ' 'KOMG-TPLST' P_TPLST.
*  CLEAR LV_KBETR.
*  WRITE GT_DATA-KBETR TO LV_KBETR CURRENCY GT_DATA-KONWA.
*  CLEAR LV_KPEIN.
*  WRITE GT_DATA-KPEIN TO LV_KPEIN UNIT GT_DATA-KMEIN.
*
*  PERFORM DATE_SETTING USING GT_DATA-DATAB
*                    CHANGING LV_DATAB.
*
*  PERFORM DATE_SETTING USING GT_DATA-DATBI
*                    CHANGING LV_DATBI.
*
*  PERFORM BDC_APPEND_DATA USING: 'X' 'SAPMV13A'   '1922',
*                                 ' ' 'BDC_CURSOR' 'RV13A-DATBI(01)',
*                                 ' ' 'BDC_OKCODE' 'SICH',
*                                 ' ' 'KOMG-TPLST'      P_TPLST,
*                                 ' ' 'KOMG-KUNWE(01)'  GT_DATA-KUNWE,
*                                 ' ' 'KONP-KBETR(01)'  LV_KBETR,
*                                 ' ' 'KONP-KONWA(01)'  GT_DATA-KONWA,
*                                 ' ' 'KONP-KPEIN(01)'  LV_KPEIN,
*                                 ' ' 'KONP-KMEIN(01)'  GT_DATA-KMEIN,
*                                 ' ' 'RV13A-DATAB(01)' LV_DATAB,
*                                 ' ' 'RV13A-DATBI(01)' LV_DATBI.
*  CALL TRANSACTION 'TK11' USING GT_BDC
*                           MODE C_N
*                           MESSAGES INTO GT_MSG.
*
*  READ TABLE GT_MSG WITH KEY MSGTYP = C_E.
*  IF SY-SUBRC = 0.
*    PERFORM GET_BDC_MESSAGE USING GT_MSG.
*    GT_DATA-ICON = ICON_LED_RED.
*  ELSE.
*    GT_DATA-MSG = TEXT-009.
*    GT_DATA-ICON = ICON_LED_GREEN.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_APPEND_DATA
*&---------------------------------------------------------------------*
FORM BDC_APPEND_DATA  USING P_DYNBEGIN
                            P_NAME
                            P_VALUE.
  CLEAR :GT_BDC.
  IF P_DYNBEGIN = C_X.
    GT_BDC-PROGRAM  = P_NAME.
    GT_BDC-DYNPRO   = P_VALUE.
    GT_BDC-DYNBEGIN = P_DYNBEGIN.
  ELSE.
    GT_BDC-FNAM = P_NAME.
    GT_BDC-FVAL = P_VALUE.
  ENDIF.
  APPEND GT_BDC.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BDC_MESSAGE
*&---------------------------------------------------------------------*
FORM GET_BDC_MESSAGE USING PT_BDCMSG.

  DATA : LV_CNT LIKE SY-TABIX,
         LS_MSG LIKE BDCMSGCOLL,
         LV_MSG TYPE CHAR100.

  DESCRIBE TABLE GT_MSG LINES LV_CNT.
  READ TABLE GT_MSG INDEX LV_CNT INTO LS_MSG.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = LS_MSG-MSGID
      MSGNR               = LS_MSG-MSGNR
      MSGV1               = LS_MSG-MSGV1
      MSGV2               = LS_MSG-MSGV2
      MSGV3               = LS_MSG-MSGV3
      MSGV4               = LS_MSG-MSGV4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = LV_MSG.

  GT_DATA-MSG = LV_MSG.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TEXT
*&---------------------------------------------------------------------*
FORM GET_TEXT.
  DATA: LT_TEMP  LIKE TABLE OF GT_DATA,
        LV_INDEX TYPE SY-TABIX.


  " SHIP TO
  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].
  SORT LT_TEMP BY KUNWE.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING KUNWE.

  SELECT KUNNR, NAME1
    FROM KNA1
    INTO CORRESPONDING FIELDS OF TABLE @GT_KNA1
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE KUNNR EQ @LT_TEMP-KUNWE.

  " Service agent
  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].
  SORT LT_TEMP BY TDLNR.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING TDLNR.

  SELECT LIFNR, NAME1
    FROM LFA1
    INTO CORRESPONDING FIELDS OF TABLE @GT_LFA1
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE LIFNR EQ @LT_TEMP-TDLNR.

  "Trailer overall length
  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].
  SORT LT_TEMP BY ADD02.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING ADD02.

  SELECT ADD_INFO
    FROM VTADD02
    INTO TABLE @DATA(LT_VTADD02)
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE ADD_INFO EQ @LT_TEMP-ADD02.

  " Condition type
*S_2021/2/23 comment out by E00064
*  CLEAR LT_TEMP[].
*  LT_TEMP[] = GT_DATA[].
*  SORT LT_TEMP BY KSCHL.
*  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING KSCHL.
*
*  SELECT KSCHL, VTEXT
*    FROM T685T
*    INTO CORRESPONDING FIELDS OF TABLE @GT_T685T
*    FOR ALL ENTRIES IN @LT_TEMP
*   WHERE KAPPL EQ 'F'
*     AND SPRAS EQ 'EN'
*     AND KSCHL EQ @LT_TEMP-KSCHL.
*E_2021/2/23 comment out by E00064

*  CLEAR LT_TEMP[].
*  LT_TEMP[] = GT_DATA[].
*  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING KSCHL.
*  SORT LT_TEMP BY KSCHL.
*
*  SELECT KSCHL, VTEXT
*    FROM T685T
*    INTO CORRESPONDING FIELDS OF TABLE @GT_T685T
*    FOR ALL ENTRIES IN @LT_TEMP
*    WHERE KSCHL EQ @LT_TEMP-KSCHL
*      AND SPRAS EQ @SY-LANGU.
*
*  CLEAR LT_TEMP[].
*  LT_TEMP[] = GT_DATA[].
*  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING KMEIN.
*  SORT LT_TEMP BY KMEIN.
*
*  IF P_UPLO IS INITIAL.
*    SELECT MSEHI, MSEHL
*      FROM T006A
*      INTO CORRESPONDING FIELDS OF TABLE @GT_T006A
*      FOR ALL ENTRIES IN @LT_TEMP
*      WHERE MSEHI EQ @LT_TEMP-KMEIN
*        AND SPRAS EQ @SY-LANGU.
*  ENDIF.

  LOOP AT GT_DATA.
    LV_INDEX = SY-TABIX.

    "ship to
    READ TABLE GT_KNA1 WITH KEY KUNNR = GT_DATA-KUNWE.
    IF SY-SUBRC = 0.
      GT_DATA-KUNWE_T = GT_KNA1-NAME1.
    ENDIF.

    "service agent
    READ TABLE GT_LFA1 WITH KEY LIFNR = GT_DATA-TDLNR.
    IF SY-SUBRC = 0.
      GT_DATA-TDLNR_T = GT_LFA1-NAME1.
    ENDIF.

    "Surcharge
    CLEAR GT_DATA1.
    READ TABLE GT_DATA1 WITH KEY TDLNR = GT_DATA-TDLNR
                                 ADD02 = GT_DATA-ADD02
                                 KUNWE = GT_DATA-KUNWE.
    IF SY-SUBRC = 0.
      GT_DATA-KBETR1 = GT_DATA1-KBETR.
      GT_DATA-DATAB1 = GT_DATA1-DATAB.
      GT_DATA-DATBI1 = GT_DATA1-DATBI.
    ENDIF.


    "Condition type
*S_2021/2/23 COMMENT OUT BY E00064
*    CLEAR GT_T685T.
*    READ TABLE GT_T685T WITH KEY KSCHL = GT_DATA-KSCHL.
*    IF SY-SUBRC = 0.
*      GT_DATA-KSCHL_T = GT_T685T-VTEXT.
*    ENDIF.
*E_2021/2/23 COMMENT OUT BY E00064

*    READ TABLE GT_T685T WITH KEY KSCHL = GT_DATA-KSCHL.
*    IF SY-SUBRC = 0.
*      GT_DATA-KSCHL_T = GT_T685T-VTEXT.
*    ENDIF.
*
*    IF P_UPLO IS INITIAL.
*      READ TABLE GT_T006A WITH KEY MSEHI = GT_DATA-KMEIN.
*      IF SY-SUBRC = 0.
*        GT_DATA-KMEIN_T = GT_T006A-MSEHL.
*      ENDIF.
*    ELSE.
*      PERFORM F4_TEXT USING 'KONP' 'KMEIN' GT_DATA-KMEIN GT_DATA-KMEIN_T.
*    ENDIF.

    IF P_UPLO IS NOT INITIAL.
      "Trailer overall length 입력값 check
      READ TABLE LT_VTADD02 TRANSPORTING NO FIELDS WITH KEY ADD_INFO = GT_DATA-ADD02.
      IF SY-SUBRC NE 0. "입력값이 테이블에 존재 하지 않으면
        IF GT_DATA-MSG IS INITIAL.
          GT_DATA-MSG = 'Check Trailer overall length'.
          GT_DATA-ICON = ICON_LED_RED.
        ENDIF.
      ENDIF.

      IF GT_DATA-ICON IS INITIAL.
        GT_DATA-ICON = ICON_LED_INACTIVE.
      ENDIF.
    ENDIF.


    MODIFY GT_DATA INDEX LV_INDEX.
  ENDLOOP.

  SORT GT_DATA BY TDLNR ADD02 KUNWE ASCENDING.

  CLEAR: LV_INDEX, GT_KNA1, GT_KNA1[].
*  CLEAR: LV_INDEX, GT_KNA1, GT_KNA1[], GT_T685T, GT_T685T[], GT_T006A, GT_T006A[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATE_SETTING
*&---------------------------------------------------------------------*
FORM DATE_SETTING USING PV_S_DATUM
               CHANGING PV_T_DATUM.
  DATA: LV_STRING TYPE CHAR10,
        LV_FORMAT TYPE CHAR10.

  CALL FUNCTION 'ZCM_DATE_FORMAT_CONVERSION'
    EXPORTING
      I_DATE        = PV_S_DATUM
      I_USER        = SY-UNAME
    IMPORTING
      E_DATE_STRING = LV_STRING
      E_DATA_FORMAT = LV_FORMAT.

  REPLACE ALL OCCURRENCES OF '.' IN LV_STRING WITH SPACE.
  REPLACE ALL OCCURRENCES OF '/' IN LV_STRING WITH SPACE.
  REPLACE ALL OCCURRENCES OF '-' IN LV_STRING WITH SPACE.

  PV_T_DATUM = LV_STRING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TEXT
*&---------------------------------------------------------------------*
FORM F4_TEXT  USING    VALUE(P_TABNAME)
                        VALUE(P_FIELDNAME)
                        P_FIELD
                        P_FIELD_T.

  DATA LT_RETURN LIKE TABLE OF DDSHRETVAL WITH HEADER LINE.

  CLEAR: LT_RETURN.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      TABNAME             = P_TABNAME
      FIELDNAME           = P_FIELDNAME
      SUPPRESS_RECORDLIST = C_X
    TABLES
      RETURN_TAB          = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND     = 1
      NO_HELP_FOR_FIELD   = 2
      INCONSISTENT_HELP   = 3
      NO_VALUES_FOUND     = 4
      OTHERS              = 5.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


  READ TABLE LT_RETURN WITH KEY FIELDVAL = P_FIELD.
  IF SY-SUBRC = 0.

    DELETE LT_RETURN[] WHERE RECORDPOS NE LT_RETURN-RECORDPOS.
    READ TABLE LT_RETURN INDEX 3.

    P_FIELD_T = LT_RETURN-FIELDVAL.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE .
*S_2021/2/23 COMMENT OUT BY E00064
*  IF P_TYPE IS INITIAL.
*    MESSAGE S001 DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*E_2021/2/23 COMMENT OUT BY E00064
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    "SENDER
                                   E_ROW     TYPE	LVC_S_ROW
                                   E_COLUMN  TYPE	LVC_S_COL.
*                                   ES_ROW_NO TYPE  LVC_S_ROID.
*
*  DATA : LV_KSCHL    LIKE A907-KSCHL.
*  DATA : LV_DATAB    LIKE A907-DATAB.
*
*  RANGES : LR_KUNWE FOR A907-KUNWE.
*
*  READ TABLE GT_DATA INDEX E_ROW-INDEX.
*  CASE E_COLUMN-FIELDNAME.
*    WHEN 'KBETR'.
*      CHECK GT_DATA-DATAB IS NOT INITIAL.
*      LV_KSCHL = 'ZFR4'.
*      LV_DATAB = GT_DATA-DATAB.
*      PERFORM CALL_TK13_BDC USING LV_KSCHL LV_DATAB.
**      SET PARAMETER ID 'VKS' FIELD LV_KSCHL.           "Condition type
**      SET PARAMETER ID 'TDP' FIELD P_TPLST.            "Transportaion Planning Point
**      SET PARAMETER ID 'KDA' FIELD GT_DATA-DATAB1.     "Valid On
**      CALL TRANSACTION 'TK13' AND SKIP FIRST SCREEN.
**      CLEAR LR_KUNWE.
**      LR_KUNWE = 'IEQ'. LR_KUNWE-LOW = GT_DATA-KUNWE. APPEND LR_KUNWE.
**      SUBMIT RV13A907 WITH   KSCHL EQ LV_KSCHL          "Condition type
**                      WITH   F001  EQ P_TPLST           "Transportaion Planning Point
**                      WITH   F002  EQ GT_DATA-TDLNR     "Service Agent
**                      WITH   F003  IN LR_KUNWE          "Shipto
**                      WITH   SEL_DATE EQ GT_DATA-DATAB  "Valid On
**                      VIA SELECTION-SCREEN
***                      AND RETURN
**                      .
*    WHEN 'KBETR1'.
*      CHECK GT_DATA-DATAB1 IS NOT INITIAL.
*
*      LV_KSCHL = 'ZFR5'.
*      LV_DATAB = GT_DATA-DATAB1.
*      PERFORM CALL_TK13_BDC USING LV_KSCHL LV_DATAB.
*
**      SET PARAMETER ID 'VKS' FIELD LV_KSCHL.           "Condition type
**      SET PARAMETER ID 'TDP' FIELD P_TPLST.            "Transportaion Planning Point
**      SET PARAMETER ID 'KDA' FIELD GT_DATA-DATAB1.     "Valid On
**      CALL TRANSACTION 'TK13' AND SKIP FIRST SCREEN.
**      CLEAR LR_KUNWE.
**      LR_KUNWE = 'IEQ'. LR_KUNWE-LOW = GT_DATA-KUNWE. APPEND LR_KUNWE.
**      SUBMIT RV13A907 WITH   KSCHL EQ LV_KSCHL          "Condition type
**                      WITH   F001  EQ P_TPLST           "Transportaion Planning Point
**                      WITH   F002  EQ GT_DATA-TDLNR     "Service Agent
**                      WITH   F003  IN LR_KUNWE          "Shipto
**                      WITH   SEL_DATE EQ GT_DATA-DATAB1 "Valid On
**                      VIA SELECTION-SCREEN
***                      AND RETURN
**                      .
*
*
*  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_TK13_BDC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_KSCHL
*&---------------------------------------------------------------------*
FORM CALL_TK13_BDC  USING    P_KSCHL P_DATAB.

*  DATA: OPT TYPE CTU_PARAMS.

  DATA: LV_DATUM LIKE SY-DATUM.
  WRITE P_DATAB TO LV_DATUM.

  CLEAR : GT_BDC, GT_BDC[].

* Condition type
  PERFORM BDC_APPEND_DATA USING: 'X'  'SAPMV13A'   '0100',
                                 ' '  'BDC_CURSOR' 'RV13A-KSCHL',
                                 ' '  'BDC_OKCODE' '/00',
                                 ' '  'RV13A-KSCHL' P_KSCHL.  "P_TYPE. " Base Freight

* Selection
  PERFORM BDC_APPEND_DATA USING: 'X'  'RV13A907'   '1000',
*                                 ' '  'BDC_CURSOR' 'RV13A-KSCHL',
                                 ' '  'BDC_OKCODE' '=ONLI',
                                 ' '  'F001' P_TPLST,  " Transportaion Planning Point
                                 ' '  'F002' GT_DATA-TDLNR,       " Service Agent
                                 ' '  'F003-LOW' GT_DATA-KUNWE,   " Shipto
                                 ' '  'SEL_DATE' LV_DATUM.  " Valid On

** Display
*  PERFORM BDC_APPEND_DATA USING: 'X'  'SAPMV13A'   '1907',
**                                 ' '  'BDC_CURSOR' 'RV13A-KSCHL',
*                                 ' '  'BDC_OKCODE' '/EBAC'.

** Selection
*  PERFORM BDC_APPEND_DATA USING: 'X'  'RV13A907'   '1000',
*                                 ' '  'BDC_OKCODE' '/EE'.

  CALL TRANSACTION 'TK13' USING GT_BDC
                           MODE C_E
*                           MODE C_N
*                           MODE C_A
                           MESSAGES INTO GT_MSG
                           .
*                           AND SKIP FIRST SCREEN


ENDFORM.
