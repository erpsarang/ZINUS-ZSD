*&---------------------------------------------------------------------*
*& Include          ZSDR0030_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INIT
*&---------------------------------------------------------------------*
FORM INIT .
  P_VKORG = C_2011.
  P_VTWEG = C_10.
  P_SPART = C_00.

  GS_FUNCTEXT-ICON_ID   = ICON_XLS.
  GS_FUNCTEXT-QUICKINFO = TEXT-004.
  GS_FUNCTEXT-ICON_TEXT = TEXT-004.
  SSCRFIELDS-FUNCTXT_01  = GS_FUNCTEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .
  DATA: LV_INDEX TYPE SY-TABIX,
        LV_BUKRS TYPE KNB1-BUKRS,
        LT_TEMP  LIKE TABLE OF GT_DATA,
        LV_CHK.

  SELECT SINGLE BUKRS INTO LV_BUKRS FROM TVKO WHERE VKORG EQ P_VKORG.

  SELECT BU~PARTNER AS BPCOD,
         BU~MC_NAME1 AS BPCOD_T,
         VV~KDGRP,
         VV~WAERS AS WAERS_C,
         VV~KALKS,
         VV~VERSG,
         VV~VSBED,
         VV~VWERK,
         VV~INCO1 AS INCO1_C,
         VV~INCO2 AS INCO2_C,
         VV~KTGRD,
         VV~KVGR1,
         VV~KVGR2,
         VV~KVGR3,
         VV~KVGR4,
         VV~KVGR5,
         B1~AKONT,
         B1~ZUAWA,
         B1~ZTERM,
         B1~GUZTE,
         B1~ZWELS
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA[]
    FROM KNVV AS VV JOIN KNB1 AS B1
                      ON VV~KUNNR EQ B1~KUNNR
                    JOIN BUT000 AS BU
                      ON BU~PARTNER EQ VV~KUNNR
    WHERE VV~VKORG EQ @P_VKORG
      AND VV~VTWEG EQ @P_VTWEG
      AND VV~SPART EQ @P_SPART
      AND B1~BUKRS EQ @LV_BUKRS.

  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING AKONT.
  SORT LT_TEMP BY AKONT.

  SELECT SAKNR, TXT50
    INTO CORRESPONDING FIELDS OF TABLE @GT_AKONT
    FROM SKAT
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE SAKNR EQ @LT_TEMP-AKONT
      AND SPRAS EQ @C_E.

  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING INCO1_C.
  SORT LT_TEMP BY INCO1_C.

  SELECT INCO1, BEZEI
    INTO CORRESPONDING FIELDS OF TABLE @GT_INCO1
    FROM TINCT
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE INCO1 EQ @LT_TEMP-INCO1_C
      AND SPRAS EQ @SY-LANGU.

  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING VWERK.
  SORT LT_TEMP BY VWERK.

  SELECT WERKS, NAME1
    INTO CORRESPONDING FIELDS OF TABLE @GT_VWERK
    FROM T001W
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE WERKS EQ @LT_TEMP-VWERK.

  LOOP AT GT_DATA.
    LV_INDEX = SY-TABIX.

    PERFORM TEXT USING LV_CHK.
    GT_DATA-ICON = C_ICON_LED_GREEN.
    MODIFY GT_DATA INDEX LV_INDEX.
  ENDLOOP.



  CLEAR : LV_INDEX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .
  IF P_SEAR = C_X.
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'GR1'.
          SCREEN-ACTIVE = 0.
      ENDCASE.
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
'ICON'           C_X       '40'     TEXT-C01      ' '       ' '   ' '    ' '             ' ',
'MSG'            C_X       '50'     TEXT-C02      ' '       ' '   ' '    ' '             ' ',
'BPCOD'          C_X       '10'     TEXT-C03      ' '       ' '   'BUT000'   'PARTNER'             ' ',
'BPCOD_T'        ' '       '50'     TEXT-C03      ' '       ' '   ' '    ' '             ' ',
'KDGRP'          ' '       '2'      TEXT-C04      ' '       ' '   'KNVV'    'KDGRP'             ' ',
'KDGRP_T'        ' '       '50'     TEXT-C04      ' '       ' '   ' '    ' '             ' ',
'WAERS_C'        ' '       '5'      TEXT-C05      ' '       ' '   'KNVV'    'WAERS'             ' ',
'KALKS'          ' '       '2'      TEXT-C06      ' '       ' '   'KNVV'    'KALKS'             ' ',
'KALKS_T'        ' '       '50'     TEXT-C06      ' '       ' '   ' '    ' '             ' ',
'VERSG'          ' '       '1'      TEXT-C07      ' '       ' '   'KNVV'    'VERSG'             ' ',
'VERSG_T'        ' '       '50'     TEXT-C07      ' '       ' '   ' '    ' '             ' ',
'VSBED'          ' '       '40'     TEXT-C08      ' '       ' '   'KNVV'    'VSBED'             ' ',
'VSBED_T'        ' '       '50'     TEXT-C08      ' '       ' '   ' '    ' '             ' ',
'VWERK'          ' '       '40'     TEXT-C22      ' '       ' '   'KNVV'    'VWERK'             ' ',
'VWERK_T'        ' '       '40'     TEXT-C22      ' '       ' '   ' '    ' '             ' ',
'INCO1_C'        ' '       '3'      TEXT-C09      ' '       ' '   'KNVV'    'INCO1'             ' ',
'INCO1_T'        ' '       '50'     TEXT-C09      ' '       ' '   ' '    ' '             ' ',
'INCO2_C'        ' '       '30'     TEXT-C21      ' '       ' '   'KNVV'    'INCO2'             ' ',
'KTGRD'          ' '       '2'      TEXT-C10      ' '       ' '   'KNVV'    'KTGRD'             ' ',
'KTGRD_T'        ' '       '50'     TEXT-C10      ' '       ' '   ' '    ' '             ' ',
'KVGR1'          ' '       '3'      TEXT-C11      ' '       ' '   'KNVV'    'KVGR1'             ' ',
'KVGR1_T'        ' '       '50'     TEXT-C11      ' '       ' '   ' '    ' '             ' ',
'KVGR2'          ' '       '3'      TEXT-C12      ' '       ' '   'KNVV'    'KVGR2'             ' ',
'KVGR2_T'        ' '       '50'     TEXT-C12      ' '       ' '   ' '    ' '             ' ',
'KVGR3'          ' '       '3'      TEXT-C13      ' '       ' '   'KNVV'    'KVGR3'             ' ',
'KVGR3_T'        ' '       '50'     TEXT-C13      ' '       ' '   ' '    ' '             ' ',
'KVGR4'          ' '       '3'      TEXT-C14      ' '       ' '   'KNVV'    'KVGR4'             ' ',
*'KVGR4_T'        ' '       '50'     TEXT-c14      ' '       ' '   ' '    ' '             ' ',
'KVGR5'          ' '       '3'      TEXT-C15      ' '       ' '   'KNVV'    'KVGR5'             ' ',
*'KVGR5_T'        ' '       '50'     TEXT-c15      ' '       ' '   ' '    ' '             ' ',
'AKONT'          ' '       '10'     TEXT-C16      ' '       ' '   'KNB1'    'AKONT'             ' ',
'AKONT_T'        ' '       '50'     TEXT-C16      ' '       ' '   ' '    ' '             ' ',
'ZUAWA'          ' '       '3'      TEXT-C17      ' '       ' '   'KNB1'    'ZUAWA'             ' ',
'ZUAWA_T'        ' '       '50'     TEXT-C17      ' '       ' '   ' '    ' '             ' ',
'ZTERM'          ' '       '4'      TEXT-C18      ' '       ' '   'KNB1'    'ZTERM'             ' ',
*'ZTERM_T'        ' '       '50'     TEXT-c18      ' '       ' '   ' '    ' '             ' ',
'GUZTE'          ' '       '4'      TEXT-C19      ' '       ' '   'KNB1'    'GUZTE'             ' ',
*'GUZTE_T'        ' '       '50'     TEXT-c19      ' '       ' '   ' '    ' '             ' ',
'ZWELS'          ' '       '10'     TEXT-C20      ' '       ' '   'KNB1'    'ZWELS'             ' '.
*'ZWELS_T'        ' '       '50'     TEXT-c20      ' '       ' '   ' '    ' '             ' '.
ENDFORM.                    "ALV_FIELD_CATALOG
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
  DATA: LS_FCAT TYPE LVC_S_FCAT.
  LS_FCAT-FIELDNAME  = P_FNAME.
  LS_FCAT-KEY        = P_KEY.
  LS_FCAT-OUTPUTLEN  = P_OUTPUTLEN.
  LS_FCAT-COLTEXT  = COLTEXT.
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
*       text
*----------------------------------------------------------------------*
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

  LV_OBJECT = TEXT-005."'ZSDR0030'.
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
FORM EXCEL_UPLOAD .
  DATA : LV_INDEX TYPE SY-TABIX,
         LT_TEMP  LIKE TABLE OF GT_DATA,
         LV_CHK.

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

  SORT I_XLS_DATA BY ROW COL.

  LOOP AT I_XLS_DATA.

    ASSIGN COMPONENT I_XLS_DATA-COL OF STRUCTURE GT_UPLOAD TO <I_FS0>.
    <I_FS0> = I_XLS_DATA-VALUE.

    AT END OF ROW.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GT_UPLOAD-BPCOD
        IMPORTING
          OUTPUT = GT_UPLOAD-BPCOD.
      APPEND  GT_UPLOAD.
      MOVE-CORRESPONDING GT_UPLOAD TO GT_DATA.
      APPEND GT_DATA.
      CLEAR: GT_UPLOAD, GT_DATA.
    ENDAT.
  ENDLOOP.

  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].

  DELETE LT_TEMP INDEX 1.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING BPCOD.
  SORT LT_TEMP BY BPCOD.

  SELECT PARTNER, NAME_ORG1
    INTO CORRESPONDING FIELDS OF TABLE @GT_BPCOD
    FROM BUT000
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE PARTNER EQ @LT_TEMP-BPCOD.

  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING INCO1_C.
  SORT LT_TEMP BY INCO1_C.

  SELECT INCO1, BEZEI
    INTO CORRESPONDING FIELDS OF TABLE @GT_INCO1
    FROM TINCT
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE INCO1 EQ @LT_TEMP-INCO1_C
      AND SPRAS EQ @SY-LANGU.

  CLEAR LT_TEMP[].
  LT_TEMP[] = GT_DATA[].
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING VWERK.
  SORT LT_TEMP BY VWERK.

  SELECT WERKS, NAME1
    INTO CORRESPONDING FIELDS OF TABLE @GT_VWERK
    FROM T001W
    FOR ALL ENTRIES IN @LT_TEMP
    WHERE WERKS EQ @LT_TEMP-VWERK.

  "Read Defalut Data
  READ TABLE GT_DATA INTO GS_DATA INDEX 1.

  LOOP AT GT_DATA FROM 2.
    LV_INDEX = SY-TABIX.

    IF GT_DATA-KDGRP IS INITIAL.
      GT_DATA-KDGRP = GS_DATA-KDGRP.
    ENDIF.
    IF GT_DATA-WAERS_C IS INITIAL.
      GT_DATA-WAERS_C = GS_DATA-WAERS_C.
    ENDIF.
    IF GT_DATA-KALKS IS INITIAL.
      GT_DATA-KALKS = GS_DATA-KALKS.
    ENDIF.
    IF GT_DATA-VERSG IS INITIAL.
      GT_DATA-VERSG = GS_DATA-VERSG.
    ENDIF.
    IF GT_DATA-VWERK IS INITIAL.
      GT_DATA-VWERK = GS_DATA-VWERK.
    ENDIF.
    IF GT_DATA-INCO1_C IS INITIAL.
      GT_DATA-INCO1_C = GS_DATA-INCO1_C.
    ENDIF.
    IF GT_DATA-INCO2_C IS INITIAL.
      GT_DATA-INCO2_C = GS_DATA-INCO2_C.
    ENDIF.
    IF GT_DATA-KTGRD IS INITIAL.
      GT_DATA-KTGRD = GS_DATA-KTGRD.
    ENDIF.
    IF GT_DATA-AKONT IS INITIAL.
      GT_DATA-AKONT = GS_DATA-AKONT.
    ENDIF.
    IF GT_DATA-ZUAWA IS INITIAL.
      GT_DATA-ZUAWA = GS_DATA-ZUAWA.
    ENDIF.
    IF GT_DATA-ZTERM IS INITIAL.
      GT_DATA-ZTERM = GS_DATA-ZTERM.
    ENDIF.
    IF GT_DATA-GUZTE IS INITIAL.
      GT_DATA-GUZTE = GS_DATA-GUZTE.
    ENDIF.
    IF GT_DATA-ZWELS IS INITIAL.
      GT_DATA-ZWELS = GS_DATA-ZWELS.
    ENDIF.
    IF GT_DATA-KVGR1 IS INITIAL OR GT_DATA-KVGR2 IS INITIAL OR GT_DATA-VSBED IS INITIAL OR GT_DATA-KVGR5 IS INITIAL.
      LV_CHK = C_X.
    ENDIF.
    PERFORM TEXT USING LV_CHK.
    IF LV_CHK = C_X.
      GT_DATA-ICON = ICON_LED_RED.
      GT_DATA-MSG = TEXT-008.
    ELSEIF LV_CHK = C_Y.
      GT_DATA-ICON = ICON_LED_RED.
      GT_DATA-MSG = TEXT-009.
    ELSE.
      GT_DATA-ICON = ICON_LED_INACTIVE.
    ENDIF.
*IF lv_chk IS INITIAL.
*   gt_data-icon = icon_led_inactive.
*ELSEIF lv_chk = c_x.
*  gt_data-icon = icon_led_red.
*  gt_data-msg = TEXT-008.
*ELSEIF lv_chk = c_y.
*  gt_data-icon = icon_led_red.
*  gt_data-msg = TEXT-009.

*ENDIF.

    MODIFY GT_DATA INDEX LV_INDEX.
    CLEAR LV_CHK.
  ENDLOOP.

  "delete Defalut row
  DELETE GT_DATA INDEX 1.

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
  SET HANDLER GR_EVENT_HANDLER->HANDLE_TOOLBAR        FOR GR_ALV_GRID.
  SET HANDLER GR_EVENT_HANDLER->HANDLE_USER_COMMAND   FOR GR_ALV_GRID.
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
        L_NAME1      TYPE T001W-NAME1.

  CONCATENATE 'Sales Org : '  P_VKORG INTO L_TEXT SEPARATED BY SPACE.

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

  CONCATENATE 'Distr. Channel : '  P_VTWEG INTO L_TEXT SEPARATED BY SPACE.

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

  CONCATENATE 'Division : '  P_SPART INTO L_TEXT SEPARATED BY SPACE.


  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
*     sap_style    = cl_dd_document=>heading
*     sap_color    = cl_dd_document=>list_heading_int
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
*     sap_emphasis = cl_dd_document=>strong
      STYLE_CLASS  = SPACE.
ENDFORM.                    " HANDLE_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOBAR
*&---------------------------------------------------------------------*
FORM HANDLE_TOOBAR  USING    P_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.
  DATA: LS_TOOLBAR TYPE STB_BUTTON.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-ICON      =  ICON_SYSTEM_SAVE .
  LS_TOOLBAR-FUNCTION  = 'SAVE'.
  LS_TOOLBAR-TEXT      = 'SAVE'.
  LS_TOOLBAR-QUICKINFO = 'SAVE'.
  LS_TOOLBAR-DISABLED  = ' '.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-ICON      =  ICON_SELECT_ALL .
  LS_TOOLBAR-FUNCTION  = 'SELECT'.
  LS_TOOLBAR-TEXT      = 'Select All'.
  LS_TOOLBAR-QUICKINFO = 'Select All'.
  LS_TOOLBAR-DISABLED  = ' '.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-ICON      =  ICON_DESELECT_ALL.
  LS_TOOLBAR-FUNCTION  = 'CANCEL'.
  LS_TOOLBAR-TEXT      = 'Deselect All'.
  LS_TOOLBAR-QUICKINFO = 'Deselect All'.
  LS_TOOLBAR-DISABLED  = ' '.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.
ENDFORM.                    " HANDLE_TOOBAR
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING    P_UCOMM.
  CASE P_UCOMM.
    WHEN 'SAVE'.
      PERFORM BP_CUST.
    WHEN 'SELECT'.
      PERFORM TOOLBAR_SELECT.
    WHEN 'CANCEL'.
      PERFORM TOOLBAR_CANCEL.
  ENDCASE.

ENDFORM.                    " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_SELECT
*&---------------------------------------------------------------------*
FORM TOOLBAR_SELECT .
  DATA: LV_INDEX TYPE SY-TABIX.
  DESCRIBE TABLE GT_DATA[].
  DO SY-TFILL TIMES.
    GS_ROW-INDEX = SYST-INDEX.
    APPEND GS_ROW TO GT_ROWS.
  ENDDO.

  CALL METHOD GR_ALV_GRID->SET_SELECTED_ROWS
    EXPORTING
      IT_INDEX_ROWS = GT_ROWS.
  CLEAR: GS_ROW, GT_ROWS.

ENDFORM.                    " TOOLBAR_SELECT
*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_CANCEL
*&---------------------------------------------------------------------*
FORM TOOLBAR_CANCEL .
  DO SY-TFILL TIMES.
    GS_ROW-INDEX = 0.
    APPEND GS_ROW TO GT_ROWS.
  ENDDO.

  CALL METHOD GR_ALV_GRID->SET_SELECTED_ROWS
    EXPORTING
      IT_INDEX_ROWS = GT_ROWS.
  CLEAR: GS_ROW, GT_ROWS.
ENDFORM.                    " TOOLBAR_CANCEL
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    P_ROW
                                   P_COLUMN.
  DATA: LV_BPARTNER TYPE BU_PARTNER.
  READ TABLE GT_DATA INDEX P_ROW.
  IF SY-SUBRC = 0.
    LV_BPARTNER = GT_DATA-BPCOD.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LV_BPARTNER
    IMPORTING
      OUTPUT = LV_BPARTNER.

  CASE P_COLUMN.
    WHEN 'BPCOD'.
      SET PARAMETER ID 'BPA' FIELD LV_BPARTNER.
      CALL TRANSACTION 'BP' AND SKIP FIRST SCREEN.
      FREE MEMORY ID 'BPA'.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BP_CUST
*&---------------------------------------------------------------------*
FORM BP_CUST .
  DATA: BEGIN OF LT_RETURN OCCURS 0,
          BPARTNER TYPE BU_PARTNER,
          TYPE     TYPE BAPI_MTYPE,
          MESSAGE  TYPE BAPI_MSG,
        END OF LT_RETURN.

  DATA: LS_DATA    LIKE ZFIS2010,
        LV_PARTNER LIKE BUT000-PARTNER,
        LV_TABIX   LIKE SY-TABIX,
        LV_BUKRS   LIKE TVKO-BUKRS,
        LV_AKONT   LIKE LFB1-AKONT.

  DATA : BEGIN OF LT_TEMP OCCURS 0,
           PARTNER   LIKE BUT000-PARTNER,
           BU_GROUP  LIKE BUT000-BU_GROUP,
           BPKIND    LIKE BUT000-BPKIND,
           NAME_ORG1 LIKE BUT000-NAME_ORG1,
           RLTYP     LIKE BUT100-RLTYP,
           COUNTRY   LIKE ADRC-COUNTRY,
         END OF LT_TEMP.

  DATA LT_TEMP2 LIKE TABLE OF GT_DATA WITH HEADER LINE.

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

  CLEAR: LS_DATA, LT_RETURN, LT_RETURN[], LV_BUKRS, LT_TEMP2, LT_TEMP2[].

  LOOP AT GT_DATA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = GT_DATA-BPCOD
      IMPORTING
        OUTPUT = LT_TEMP2-BPCOD.

    APPEND LT_TEMP2. CLEAR LT_TEMP2.
  ENDLOOP.


*  lt_temp2[] = gt_data[].
  DELETE ADJACENT DUPLICATES FROM LT_TEMP2 COMPARING BPCOD.
  SORT LT_TEMP2 BY BPCOD.

  SELECT B0~PARTNER, B0~BU_GROUP, B0~BPKIND, B0~NAME_ORG1,
         B1~RLTYP, AD~COUNTRY
    INTO CORRESPONDING FIELDS OF TABLE @LT_TEMP
    FROM BUT000 AS B0 JOIN BUT100 AS B1
                        ON B0~PARTNER EQ B1~PARTNER
                      JOIN BUT020 AS B2
                        ON B0~PARTNER EQ B2~PARTNER
                      JOIN ADRC AS AD
                        ON B2~ADDRNUMBER EQ AD~ADDRNUMBER
    FOR ALL ENTRIES IN @LT_TEMP2
                  WHERE B0~PARTNER EQ @LT_TEMP2-BPCOD
                    AND B1~RLTYP EQ 'ZZCUST'.
  SORT LT_TEMP BY PARTNER.

  SELECT SINGLE BUKRS
    INTO @LV_BUKRS "Compay Code
    FROM TVKO
    WHERE VKORG EQ @P_VKORG.

  LOOP AT LT_ROWS INTO LS_ROW.

    READ TABLE GT_DATA INDEX LS_ROW-INDEX.
    IF SY-SUBRC = 0.
      IF GT_DATA-ICON EQ ICON_LED_RED.
*        EXIT.
        CONTINUE.
      ENDIF.
      MOVE-CORRESPONDING GT_DATA TO LS_DATA.
      LV_PARTNER = GT_DATA-BPCOD.
      LS_DATA-VKORG = P_VKORG.
      LS_DATA-SPART = P_SPART.
      LS_DATA-VTWEG = P_VTWEG.
      LS_DATA-BUKRS = LV_BUKRS.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LV_PARTNER
        IMPORTING
          OUTPUT = LV_PARTNER.

      LS_DATA-BPARTNER = LV_PARTNER.

      READ TABLE LT_TEMP WITH KEY PARTNER = LV_PARTNER BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DATA-GROUPING = LT_TEMP-BU_GROUP.
        LS_DATA-BPKIND   = LT_TEMP-BPKIND.
        LS_DATA-NAME1    = LT_TEMP-NAME_ORG1.
        LS_DATA-ROLE     = LT_TEMP-RLTYP.
        LS_DATA-COUNTRY  = LT_TEMP-COUNTRY.
      ENDIF.

      CALL FUNCTION 'ZSD_MODIFY_BUSINESS_PARTNER'
        EXPORTING
          IS_DATA     = LS_DATA
        IMPORTING
          EV_BPARTNER = LT_RETURN-BPARTNER
          EV_TYPE     = LT_RETURN-TYPE
          EV_MESSAGE  = LT_RETURN-MESSAGE.
*       TABLES
*               IT_BANK     =
*               IT_PART     =
*               IT_INTER    =
*      .

      IF LT_RETURN-TYPE = C_S.
        GT_DATA-ICON = ICON_LED_GREEN.
      ELSE.
        GT_DATA-ICON = ICON_LED_RED.
      ENDIF.

      GT_DATA-MSG = LT_RETURN-MESSAGE.

      MODIFY GT_DATA INDEX LS_ROW-INDEX TRANSPORTING ICON MSG.

      CLEAR: LT_RETURN, LS_DATA, LS_ROW, GT_DATA, LV_PARTNER.
    ENDIF.
  ENDLOOP.

  PERFORM ALV_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TEXT
*&---------------------------------------------------------------------*
FORM GET_TEXT  USING    VALUE(P_TABNAME)
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
    READ TABLE LT_RETURN INDEX 2.

    P_FIELD_T = LT_RETURN-FIELDVAL.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TEXT
*&---------------------------------------------------------------------*
FORM TEXT USING LV_CHK.
  DATA LT_TEMP  LIKE TABLE OF GT_DATA.
  DATA LV_PARTNER LIKE BUT000-PARTNER.
  IF GT_DATA-BPCOD IS NOT INITIAL.
    CLEAR: LV_PARTNER, GT_BPCOD.
    LV_PARTNER = GT_DATA-BPCOD.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = LV_PARTNER
      IMPORTING
        OUTPUT = LV_PARTNER.

    READ TABLE GT_BPCOD WITH KEY PARTNER = LV_PARTNER.
    IF SY-SUBRC = 0.
      GT_DATA-BPCOD_T = GT_BPCOD-NAME_ORG1.
    ELSE.
      LV_CHK = C_Y.
    ENDIF.
  ENDIF.
  IF GT_DATA-KDGRP IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'KDGRP' GT_DATA-KDGRP GT_DATA-KDGRP_T.
  ENDIF.
  IF GT_DATA-KALKS IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'KALKS' GT_DATA-KALKS GT_DATA-KALKS_T.
  ENDIF.
  IF GT_DATA-VERSG IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'VERSG' GT_DATA-VERSG GT_DATA-VERSG_T.
  ENDIF.
  IF GT_DATA-VSBED IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'VSBED' GT_DATA-VSBED GT_DATA-VSBED_T.
  ENDIF.
  IF GT_DATA-VWERK IS NOT INITIAL.
    READ TABLE GT_VWERK WITH KEY WERKS = GT_DATA-VWERK.
    IF SY-SUBRC = 0.
      GT_DATA-VWERK_T = GT_VWERK-NAME1.
    ENDIF.
*PERFORM get_text USING 'KNVV' 'VWERK' gt_data-vwerk gt_data-vwerk_t.
  ENDIF.
  IF GT_DATA-INCO1_C IS NOT INITIAL.
*PERFORM get_text USING 'KNVV' 'INCO1' gt_data-inco1_c gt_data-inco1_t.
    READ TABLE GT_INCO1 WITH KEY INCO1 = GT_DATA-INCO1_C.
    IF SY-SUBRC = 0.
      GT_DATA-INCO1_T = GT_INCO1-BEZEI.
    ENDIF.
  ENDIF.
*IF gt_data-inco2_c IS NOT INITIAL.
**PERFORM get_text USING 'KNVV' 'INCO2' gt_data-inco2_c gt_data-inco2_t.
*ENDIF.
  IF GT_DATA-KTGRD IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'KTGRD' GT_DATA-KTGRD GT_DATA-KTGRD_T.
  ENDIF.
  IF GT_DATA-KVGR1 IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'KVGR1' GT_DATA-KVGR1 GT_DATA-KVGR1_T.
  ENDIF.
  IF GT_DATA-KVGR2 IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'KVGR2' GT_DATA-KVGR2 GT_DATA-KVGR2_T.
  ENDIF.
  IF GT_DATA-KVGR3 IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'KVGR3' GT_DATA-KVGR3 GT_DATA-KVGR3_T.
  ENDIF.
  IF GT_DATA-KVGR4 IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'KVGR4' GT_DATA-KVGR4 GT_DATA-KVGR4_T.
  ENDIF.
  IF GT_DATA-KVGR5 IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNVV' 'KVGR5' GT_DATA-KVGR5 GT_DATA-KVGR5_T.
  ENDIF.
  IF GT_DATA-AKONT IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = GT_DATA-AKONT
      IMPORTING
        OUTPUT = GT_DATA-AKONT.

    SELECT SAKNR, TXT50
      INTO CORRESPONDING FIELDS OF TABLE @GT_AKONT
      FROM SKAT
      WHERE SAKNR EQ @GT_DATA-AKONT
        AND SPRAS EQ 'EN'.

    READ TABLE GT_AKONT WITH KEY SAKNR = GT_DATA-AKONT.
    GT_DATA-AKONT_T = GT_AKONT-TXT50.
  ENDIF.
  IF GT_DATA-ZUAWA IS NOT INITIAL.
    PERFORM GET_TEXT USING 'KNB1' 'ZUAWA' GT_DATA-ZUAWA GT_DATA-ZUAWA_T.
  ENDIF.
  IF GT_DATA-ZTERM IS NOT INITIAL.
*PERFORM get_text USING 'T052U' 'TEXT1' gt_data-zterm gt_data-zterm_t.
  ENDIF.
*IF gt_data-guzte IS NOT INITIAL.
*PERFORM get_text USING 'KNB1' 'GUZTE' gt_data-guzte gt_data-guzte_t.
*ENDIF.
*IF gt_data-zwels IS NOT INITIAL.
*PERFORM get_text USING 'KNB1' 'ZWELS' gt_data-zwels gt_data-zwels_t.
*ENDIF.

ENDFORM.
