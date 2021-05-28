*&---------------------------------------------------------------------*
*& Include          ZSDR0090_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INIT
*&---------------------------------------------------------------------*
FORM INIT .
  P_WERKS = '2011'."Plant
  P_LGORT = '9000'."From Storage Location
  P_FILE  = C_XLS.

  GS_FUNCTEXT-ICON_ID   = ICON_XLS.
  GS_FUNCTEXT-QUICKINFO = TEXT-004. "Excel Template Down.
  GS_FUNCTEXT-ICON_TEXT = TEXT-004.
  SSCRFIELDS-FUNCTXT_01  = GS_FUNCTEXT.

*  CLEAR: GV_KONTS.
*  SELECT SINGLE KONTS   "G/L Accounts
*    INTO GV_KONTS
*    FROM T030
*    WHERE KTOPL EQ '1000'
*      AND KTOSL EQ 'GBB'
*      AND BWMOD EQ '2010'
*      AND KOMOK EQ P_BWART
*      AND BKLAS EQ '7920'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .
  DATA: LV_INDEX  TYPE SY-TABIX,
        LT_TEMP   LIKE TABLE OF GT_DATA WITH HEADER LINE,
        LT_VBAP   LIKE TABLE OF VBAP WITH HEADER LINE,
        LT_MCHB   LIKE TABLE OF MCHB WITH HEADER LINE,
        LT_COUNT  LIKE TABLE OF VBAP WITH HEADER LINE,
        LV_KWMENG LIKE VBAP-KWMENG,
        LV_LINES  TYPE I.

  DATA: LT_VBAP_SORT LIKE TABLE OF VBAP WITH HEADER LINE,
        LV_TABIX     TYPE SY-TABIX.

  DATA: LV_KZEAR   TYPE STRING,
        LV_LGOBE_F TYPE T001L-LGOBE.

  CLEAR: LT_TEMP, LT_TEMP[], LT_VBAP, LT_VBAP[], LT_MCHB, LT_MCHB[],
         LT_VBAP_SORT, LT_VBAP_SORT[].

  "조회모드에 따른 select 조건
  CLEAR LV_KZEAR.
  IF P_NOT IS NOT INITIAL. " not yet trans post 선택
    LV_KZEAR = 'SB~KZEAR EQ ` `'.
  ELSEIF P_COM IS NOT INITIAL. " trans post completed 선택
    LV_KZEAR = 'SB~KZEAR EQ `X`'.
  ELSE. "all 일때
    CLEAR LV_KZEAR.
  ENDIF.

  SELECT PF~RSNUM, "Reservation No.
         SB~RSPOS, "Item number
         SB~MATNR, "Material
         SB~BDMNG, "Quantity
         SB~MEINS, "UOM
         SB~CHARG, "Batch
         SB~BDTER, "Requirements Date
         SB~KZEAR, "Completed
         SB~SGTXT, "Item Text
         SB~LGORT, "storage location
         SB~UMLGO, "Receiving/issuing storage location
         KT~MAKTX  "Material Text
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA
    FROM RKPF AS PF
    INNER JOIN RESB AS SB ON PF~RSNUM EQ SB~RSNUM
    LEFT JOIN MAKT AS KT ON SB~MATNR EQ KT~MATNR
    WHERE PF~BWART EQ @P_BWART  "Movement type
      AND SB~WERKS EQ @P_WERKS  "Plant
      AND SB~LGORT EQ @P_LGORT  "from Storage Location
      AND SB~BDTER IN @S_BDTER  "Requirements Date
      AND KT~SPRAS EQ @SY-LANGU
      AND (LV_KZEAR)            "Complete
      AND SB~UMLGO IN @S_UMLGO  "to storage loaction
      AND SB~XLOEK NE @C_X.

  SORT GT_DATA BY RSNUM .

  LT_TEMP[] = GT_DATA[].
  SORT LT_TEMP[] BY MATNR CHARG.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING MATNR CHARG.

  IF LT_TEMP[] IS NOT INITIAL.
    SELECT A~VBELN, A~POSNR, A~CHARG, A~KWMENG, A~MATNR
      INTO CORRESPONDING FIELDS OF TABLE @LT_VBAP
      FROM VBAP AS A INNER JOIN VBAK AS B
                             ON A~VBELN = B~VBELN
      FOR ALL ENTRIES IN @LT_TEMP
      WHERE A~MATNR EQ @LT_TEMP-MATNR
        AND A~CHARG EQ @LT_TEMP-CHARG
        AND B~AUART EQ 'ZICW'.   " S/O TYPE
    SORT LT_VBAP BY MATNR CHARG.

    SELECT MATNR WERKS LGORT CHARG CLABS "Batch Stock qty
      FROM MCHB
      INTO CORRESPONDING FIELDS OF TABLE LT_MCHB
      FOR ALL ENTRIES IN LT_TEMP
      WHERE MATNR EQ LT_TEMP-MATNR
        AND WERKS EQ P_WERKS
        AND LGORT EQ P_LGORT "from storage
        AND CHARG EQ LT_TEMP-CHARG.
    SORT LT_MCHB BY MATNR CHARG.
  ENDIF.

  IF GT_DATA[] IS NOT INITIAL.
    SELECT A~BKTXT, B~MBLNR, B~BWART, B~MATNR, B~WERKS, B~LGORT, B~CHARG, B~CPUDT_MKPF, B~CPUTM_MKPF
  INTO TABLE @DATA(LT_MSEG)
  FROM MKPF AS A INNER JOIN MSEG AS B
                         ON A~MBLNR = B~MBLNR
                        AND A~MJAHR = B~MJAHR
  FOR ALL ENTRIES IN @GT_DATA
  WHERE B~MATNR EQ @GT_DATA-MATNR
    AND ( B~BWART EQ 'Z01' OR B~BWART EQ 'Z02' )
    AND B~CHARG EQ @GT_DATA-CHARG
    AND B~LGORT EQ @GT_DATA-UMLGO
    AND B~WERKS EQ @P_WERKS.

*    SORT LT_MSEG BY MATNR LGORT CHARG ASCENDING CPUDT_MKPF CPUTM_MKPF DESCENDING.
    SORT LT_MSEG BY BKTXT MATNR ASCENDING MBLNR DESCENDING.

    " STORAGE LOCATION DESC.
    SELECT LGORT, LGOBE
      INTO TABLE @DATA(LT_T001L)
      FROM T001L
      FOR ALL ENTRIES IN @GT_DATA
      WHERE WERKS EQ @P_WERKS
        AND ( LGORT EQ @GT_DATA-UMLGO OR LGORT EQ @P_LGORT ).

  ENDIF.

  LT_VBAP_SORT[] = LT_VBAP[].
  SORT LT_VBAP_SORT BY VBELN POSNR MATNR.

  "FROM STORAGE LOC
  READ TABLE LT_T001L INTO DATA(LS_T001L) WITH KEY LGORT = P_LGORT.
  IF SY-SUBRC EQ 0.
    LV_LGOBE_F = LS_T001L-LGOBE.
  ENDIF.

  LOOP AT GT_DATA.
    CLEAR : LV_INDEX, LT_TEMP, LV_KWMENG, LT_COUNT, LT_COUNT[], LT_VBAP_SORT.
    LV_INDEX =  SY-TABIX.
    READ TABLE LT_MCHB WITH KEY MATNR = GT_DATA-MATNR
                                CHARG = GT_DATA-CHARG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DATA-CLABS = LT_MCHB-CLABS.   "Batch stock qty
    ENDIF.
    READ TABLE LT_VBAP WITH KEY MATNR = GT_DATA-MATNR
                                CHARG = GT_DATA-CHARG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DATA-VBELN = LT_VBAP-VBELN.    "S/O
      GT_DATA-POSNR = LT_VBAP-POSNR.    "S/O ITEM

      READ TABLE LT_VBAP_SORT WITH KEY VBELN = LT_VBAP-VBELN
                                       POSNR = LT_VBAP-POSNR
                                       MATNR = LT_VBAP-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_TABIX = SY-TABIX.

        LOOP AT LT_VBAP_SORT FROM LV_TABIX.
          IF LT_VBAP_SORT-VBELN EQ LT_VBAP-VBELN AND
             LT_VBAP_SORT-MATNR EQ LT_VBAP-MATNR.

            LV_KWMENG = LV_KWMENG + LT_VBAP_SORT-KWMENG.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        GT_DATA-KWMENG = LV_KWMENG.   " S/O QTY
      ENDIF.

    ENDIF.

    "FROM STORAGE LOC
    GT_DATA-LGOBE_F = LV_LGOBE_F.

    "TO STORAGE LOC
    CLEAR LS_T001L.
    READ TABLE LT_T001L INTO LS_T001L WITH KEY LGORT = GT_DATA-UMLGO.
    IF SY-SUBRC EQ 0.
      GT_DATA-LGOBE_T = LS_T001L-LGOBE.
    ENDIF.

    IF GT_DATA-KZEAR IS NOT INITIAL.  "completed
      GT_DATA-CELL_MODE = C_I.
      READ TABLE LT_MSEG INTO DATA(LS_MSEG) WITH KEY BKTXT = GT_DATA-RSNUM
                                                     MATNR = GT_DATA-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0 AND LS_MSEG-BWART = 'Z01'.
        GT_DATA-MBLNR = LS_MSEG-MBLNR.

      ENDIF.
    ELSE.
      GT_DATA-CELL_MODE = C_C.
    ENDIF.
    GT_DATA-ICON = ICON_LED_INACTIVE.
    MODIFY GT_DATA INDEX LV_INDEX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN .
  IF P_SEAR = C_X. "Search
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'GR1'.
          SCREEN-ACTIVE = 0.
      ENDCASE.

      CASE SCREEN-NAME.
        WHEN 'P_BWART'.
          SCREEN-INPUT = 0.
        WHEN 'P_LGORT'.
          SCREEN-INPUT = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE. " Upload
    LOOP AT SCREEN.
      CASE SCREEN-GROUP1.
        WHEN 'GR2'.
          SCREEN-ACTIVE = 0.
      ENDCASE.
      CASE SCREEN-NAME.
        WHEN 'P_BWART'.
          SCREEN-INPUT = 0.
        WHEN 'P_LGORT'.
          SCREEN-INPUT = 0.
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
      HEIGHT = 16.

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
  IF P_SEAR IS NOT INITIAL.
    PERFORM APPEND_FIELD_CATALOG USING :
*  FNAME         KEY    OUTPUTLEN  COLTEXT      EDIT     QFIELD  REF_TAB  REF_FIELD
  'ICON'           C_X       '40'     TEXT-C01      ' '       ' '     ' '       ' ',
  'MSG'            C_X       '50'     TEXT-C02      ' '       ' '     ' '       ' ',
  'RSNUM'          C_X       '10'     TEXT-C03      ' '       ' '     'RESB'    'RSNUM',
  'MATNR'          ' '       '40'     TEXT-C04      ' '       ' '     'RESB'    'MATNR',
  'MAKTX'          ' '       '50'     TEXT-C14      ' '       ' '     'MAKT'    'MAKTX',
  'BDMNG'          ' '       '20'     TEXT-C05      'X'       'MEINS' 'RESB'    'BDMNG',
  'MEINS'          ' '       '3'      TEXT-C06      ' '       ' '     'RESB'    'MEINS',
  'CHARG'          ' '       '10'     TEXT-C07      ' '       ' '     'RESB'    'CHARG',
  'CLABS'          ' '       '20'     TEXT-C08      ' '       'MEINS' 'MCHB'    'CLABS',
  'VBELN'          ' '       '10'     TEXT-C12      ' '       ' '     'VBKD'    'VBELN',
  'POSNR'          ' '       '5'      TEXT-C15      ' '       ' '    'VBKD'    'POSNR',
  'KWMENG'         ' '       '2'      TEXT-C13      ' '       'MEINS' 'VBAP'    'KWMENG',
  'BDTER'          ' '       '10'     TEXT-C09      ' '       ' '     'RESB'    'BDTER',
  'KZEAR'          ' '       '1'      TEXT-C10      ' '       ' '     'RESB'    'KZEAR',
  'SGTXT'          ' '       '50'     TEXT-C11      'X'       ' '     'MAKT'    'MAKTX',
  'LGORT'          ' '       '50'     TEXT-C16      ' '       ' '     'RESB'    'LGORT',
  'LGOBE_F'        ' '       '50'     TEXT-C19      ' '       ' '     'T001L'   'LGOBE',
  'UMLGO'          ' '       '50'     TEXT-C17      ' '       ' '     'RESB'    'UMLGO',
  'LGOBE_T'        ' '       '50'     TEXT-C20      ' '       ' '     'T001L'   'LGOBE',
  'MBLNR'          ' '       '50'     TEXT-C18      ' '       ' '     'MKPF'    'MBLNR'.

  ELSE.
    PERFORM APPEND_FIELD_CATALOG USING :
*  FNAME         KEY    OUTPUTLEN  COLTEXT      EDIT     QFIELD  REF_TAB  REF_FIELD
  'ICON'           C_X       '40'     TEXT-C01      ' '       ' '     ' '       ' ',
  'MSG'            C_X       '50'     TEXT-C02      ' '       ' '     ' '       ' ',
  'RSNUM'          C_X       '10'     TEXT-C03      ' '       ' '     'RESB'    'RSNUM',
  'MATNR'          ' '       '40'     TEXT-C04      ' '       ' '     'RESB'    'MATNR',
  'MAKTX'          ' '       '50'     TEXT-C14      ' '       ' '     'MAKT'    'MAKTX',
  'BDMNG'          ' '       '20'     TEXT-C05      'X'       'MEINS' 'RESB'    'BDMNG',
  'MEINS'          ' '       '3'      TEXT-C06      ' '       ' '     'RESB'    'MEINS',
  'CHARG'          ' '       '10'     TEXT-C07      ' '       ' '     'RESB'    'CHARG',
  'CLABS'          ' '       '20'     TEXT-C08      ' '       'MEINS' 'MCHB'    'CLABS',
  'VBELN'          ' '       '10'     TEXT-C12      ' '       ' '     'VBKD'    'VBELN',
  'POSNR'          ' '       '5'      TEXT-C15      ' '       ' '    'VBKD'    'POSNR',
  'KWMENG'         ' '       '2'      TEXT-C13      ' '       'MEINS' 'VBAP'    'KWMENG',
  'BDTER'          ' '       '10'     TEXT-C09      ' '       ' '     'RESB'    'BDTER',
  'KZEAR'          ' '       '1'      TEXT-C10      ' '       ' '     'RESB'    'KZEAR',
  'SGTXT'          ' '       '50'     TEXT-C11      'X'       ' '     'MAKT'    'MAKTX',
  'LGORT'          ' '       '50'     TEXT-C16      ' '       ' '     'RESB'    'LGORT',
  'LGOBE_F'        ' '       '50'     TEXT-C19      ' '       ' '     'T001L'   'LGOBE',
  'UMLGO'          ' '       '50'     TEXT-C17      ' '       ' '     'RESB'    'UMLGO',
  'LGOBE_T'        ' '       '50'     TEXT-C20      ' '       ' '     'T001L'   'LGOBE'.
*  'MBLNR'          ' '       '50'     TEXT-C18      ' '       ' '     'MKPF'    'MBLNR'.
  ENDIF.
ENDFORM.                    "ALV_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM APPEND_FIELD_CATALOG  USING P_FNAME
                                 P_KEY
                                 P_OUTPUTLEN
                                 COLTEXT
                                 P_EDIT
                                 P_QFIELDNAME
                                 P_REF_TABLE
                                 P_REF_FIELD.
*                                 p_currency.
*                                 P_DECIMALS_O.
  DATA: LS_FCAT TYPE LVC_S_FCAT.
  LS_FCAT-FIELDNAME  = P_FNAME.
  LS_FCAT-KEY        = P_KEY.
  LS_FCAT-OUTPUTLEN  = P_OUTPUTLEN.
  LS_FCAT-COLTEXT  = COLTEXT.
  LS_FCAT-EDIT       = P_EDIT.
  LS_FCAT-QFIELDNAME = P_QFIELDNAME.
  LS_FCAT-REF_TABLE  = P_REF_TABLE.
  LS_FCAT-REF_FIELD  = P_REF_FIELD.
*  ls_fcat-currency   = p_currency.
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
  PS_LAYOUT-STYLEFNAME = P_STYLE.

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
  _EXCLUDE_TOOLBAR MC_FC_REFRESH.           " 최신표시
*  _EXCLUDE_TOOLBAR MC_FC_LOC_CUT.           " 잘라내기
*  _EXCLUDE_TOOLBAR MC_FC_LOC_COPY.          " 텍스트복사
*  _EXCLUDE_TOOLBAR MC_FC_LOC_PASTE.         " 겹쳐쓰기로 삽입
*  _EXCLUDE_TOOLBAR MC_FC_LOC_PASTE_NEW_ROW. " 신규행에 삽입
*  _EXCLUDE_TOOLBAR MC_FC_LOC_UNDO.          " 실행취소
  _EXCLUDE_TOOLBAR MC_FC_LOC_APPEND_ROW.    " 행 추가
  _EXCLUDE_TOOLBAR MC_FC_LOC_INSERT_ROW.    " 행 삽입
  _EXCLUDE_TOOLBAR MC_FC_LOC_DELETE_ROW.    " 행 삭제
  _EXCLUDE_TOOLBAR MC_FC_LOC_COPY_ROW.      " 행 복제
*  _EXCLUDE_TOOLBAR MC_FC_SORT.              " SORT
*  _EXCLUDE_TOOLBAR MC_FC_SORT_ASC.          " 오름차순 정렬
*  _EXCLUDE_TOOLBAR MC_FC_SORT_DSC.          " 내림차순 정렬
*  _EXCLUDE_TOOLBAR MC_FC_FIND.             " 찾기
*  _EXCLUDE_TOOLBAR MC_MB_FILTER.            " 필터설정
*  _EXCLUDE_TOOLBAR MC_FC_SUM.               " 총계
*  _EXCLUDE_TOOLBAR MC_MB_SUBTOT.            " SUBTOT
*  _EXCLUDE_TOOLBAR MC_FC_AVERAGE.           " 평균
*  _EXCLUDE_TOOLBAR MC_FC_MINIMUM.           " 최소
*  _EXCLUDE_TOOLBAR MC_FC_MAXIMUM.           " 최대
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

  LV_OBJECT = 'ZSDR0090'.
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
  DATA : LT_XLS     TYPE ZFI_ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE,
         LT_MAKT    LIKE TABLE OF MAKT WITH HEADER LINE,
         LT_TEMP    LIKE TABLE OF GT_DATA WITH HEADER LINE,
         LT_VBAP    LIKE TABLE OF VBAP WITH HEADER LINE,
         LT_MCHB    LIKE TABLE OF MCHB WITH HEADER LINE,
         LT_COUNT   LIKE TABLE OF VBAP WITH HEADER LINE,
         LV_KWMENG  LIKE VBAP-KWMENG,
         LV_INDEX   TYPE SY-TABIX,
         LV_NUM     TYPE I,
         LV_LINES   TYPE I,
         LV_LGOBE_F TYPE T001L-LGOBE.

  DATA: LT_VBAP_SORT LIKE TABLE OF VBAP WITH HEADER LINE,
        LV_TABIX     TYPE SY-TABIX.

  DATA: LS_DATA LIKE GT_DATA.

  FIELD-SYMBOLS <FS>.

  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 250
      I_END_ROW               = 65000
    TABLES
      INTERN                  = LT_XLS
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  SORT LT_XLS BY ROW COL.

  LOOP AT LT_XLS .
    MOVE LT_XLS-COL TO LV_NUM.
    ASSIGN COMPONENT LV_NUM OF STRUCTURE GT_UPLOAD TO <FS>.
    MOVE LT_XLS-VALUE TO <FS>.

    AT END OF ROW.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = GT_UPLOAD-MATNR
        IMPORTING
          OUTPUT = GT_UPLOAD-MATNR.
      APPEND  GT_UPLOAD.

      IF LT_XLS-ROW = 1.
        CLEAR LS_DATA.
        MOVE-CORRESPONDING GT_UPLOAD TO LS_DATA.
      ELSE.
        MOVE-CORRESPONDING GT_UPLOAD TO GT_DATA.
        APPEND GT_DATA.
      ENDIF.
      CLEAR: GT_UPLOAD, GT_DATA.
    ENDAT.
  ENDLOOP.

  CLEAR: LT_TEMP, LT_TEMP[], LT_MAKT, LT_MAKT[], LV_INDEX,
         LT_VBAP_SORT, LT_VBAP_SORT[].

  LT_TEMP[] = GT_DATA[].
  SORT LT_TEMP BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP.

  IF LT_TEMP[] IS NOT INITIAL.
    SELECT MATNR, MAKTX
    INTO CORRESPONDING FIELDS OF TABLE @LT_MAKT
      FROM MAKT
      FOR ALL ENTRIES IN @LT_TEMP
      WHERE MATNR EQ @LT_TEMP-MATNR.
    SORT LT_MAKT BY MATNR.
  ENDIF.

  LT_TEMP[] = GT_DATA[].
  SORT LT_TEMP[] BY MATNR CHARG.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING MATNR CHARG.

  IF LT_TEMP[] IS NOT INITIAL.
    SELECT A~VBELN, A~POSNR, A~CHARG, A~KWMENG, A~MATNR
      INTO CORRESPONDING FIELDS OF TABLE @LT_VBAP
      FROM VBAP AS A INNER JOIN VBAK AS B
                             ON A~VBELN = B~VBELN
      FOR ALL ENTRIES IN @LT_TEMP
      WHERE A~MATNR EQ @LT_TEMP-MATNR
        AND A~CHARG EQ @LT_TEMP-CHARG
        AND B~AUART EQ 'ZICW'.
    SORT LT_VBAP BY MATNR CHARG VBELN.

    SELECT MATNR WERKS LGORT CHARG CLABS
      FROM MCHB
      INTO CORRESPONDING FIELDS OF TABLE LT_MCHB
      FOR ALL ENTRIES IN LT_TEMP
      WHERE MATNR EQ LT_TEMP-MATNR
        AND WERKS EQ P_WERKS
        AND LGORT EQ P_LGORT "from storage
        AND CHARG EQ LT_TEMP-CHARG.
    SORT LT_MCHB BY MATNR CHARG.
  ENDIF.

  IF GT_DATA[] IS NOT INITIAL.
    " STORAGE LOCATION DESC.
    SELECT LGORT, LGOBE
      INTO TABLE @DATA(LT_T001L)
      FROM T001L
      FOR ALL ENTRIES IN @GT_DATA
      WHERE WERKS EQ @P_WERKS
        AND ( LGORT EQ @GT_DATA-UMLGO OR LGORT EQ @P_LGORT ).
  ENDIF.

  LT_VBAP_SORT[] = LT_VBAP[].
  SORT LT_VBAP_SORT BY VBELN POSNR MATNR.

  "FROM STORAGE LOC
  READ TABLE LT_T001L INTO DATA(LS_T001L) WITH KEY LGORT = P_LGORT.
  IF SY-SUBRC EQ 0.
    LV_LGOBE_F = LS_T001L-LGOBE.
  ENDIF.

  LOOP AT GT_DATA.
    CLEAR : LV_INDEX, LT_TEMP, LV_KWMENG, LT_COUNT, LT_COUNT[],
            LT_VBAP_SORT.

    LV_INDEX = SY-TABIX.

    "DAFAULT 값 입력
    IF GT_DATA-MATNR IS INITIAL. "Material
      GT_DATA-MATNR = LS_DATA-MATNR.
    ENDIF.
    IF GT_DATA-BDMNG IS INITIAL. "Qty
      GT_DATA-BDMNG = LS_DATA-BDMNG.
    ENDIF.
    IF GT_DATA-MEINS IS INITIAL. "UOM
      GT_DATA-MEINS = LS_DATA-MEINS.
    ENDIF.
    IF GT_DATA-CHARG IS  INITIAL. "Batch
      GT_DATA-CHARG = LS_DATA-CHARG.
    ENDIF.
    IF GT_DATA-UMLGO IS INITIAL. "To storage location
      GT_DATA-UMLGO = LS_DATA-UMLGO.
    ENDIF.
    IF GT_DATA-BDTER IS INITIAL. "Requiremnt date
      GT_DATA-BDTER = LS_DATA-BDTER.
    ENDIF.

    READ TABLE LT_MAKT WITH KEY MATNR = GT_DATA-MATNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DATA-MAKTX = LT_MAKT-MAKTX.
    ENDIF.

    READ TABLE LT_MCHB WITH KEY MATNR = GT_DATA-MATNR
                                CHARG = GT_DATA-CHARG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DATA-CLABS = LT_MCHB-CLABS.   "Batch stock qty
    ENDIF.

    READ TABLE LT_VBAP WITH KEY MATNR = GT_DATA-MATNR
                                CHARG = GT_DATA-CHARG BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DATA-VBELN = LT_VBAP-VBELN.    "S/O
      GT_DATA-POSNR = LT_VBAP-POSNR.    "S/O ITEM

      READ TABLE LT_VBAP_SORT WITH KEY VBELN = LT_VBAP-VBELN
                                       POSNR = LT_VBAP-POSNR
                                       MATNR = LT_VBAP-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        LV_TABIX = SY-TABIX.

        LOOP AT LT_VBAP_SORT FROM LV_TABIX.
          IF LT_VBAP_SORT-VBELN EQ LT_VBAP-VBELN AND
             LT_VBAP_SORT-MATNR EQ LT_VBAP-MATNR.

            LV_KWMENG = LV_KWMENG + LT_VBAP_SORT-KWMENG.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        GT_DATA-KWMENG = LV_KWMENG.    " S/O QTY
      ENDIF.

      "FROM STORAGE LOC
      GT_DATA-LGOBE_F = LV_LGOBE_F.

      "TO STORAGE LOC
      CLEAR LS_T001L.
      READ TABLE LT_T001L INTO LS_T001L WITH KEY LGORT = GT_DATA-UMLGO.
      IF SY-SUBRC EQ 0.
        GT_DATA-LGOBE_T = LS_T001L-LGOBE.
      ENDIF.
    ENDIF.

    GT_DATA-LGORT = P_LGORT.

    "필수값 CHECK
    IF GT_DATA-MATNR IS INITIAL.
      GT_DATA-ICON = ICON_LED_RED.
      GT_DATA-MSG  = 'Fill the all required fields'.
    ENDIF.
    IF GT_DATA-BDMNG IS INITIAL.
      GT_DATA-ICON = ICON_LED_RED.
      GT_DATA-MSG  = 'Fill the all required fields'.
    ENDIF.
    IF GT_DATA-MEINS IS INITIAL.
      GT_DATA-ICON = ICON_LED_RED.
      GT_DATA-MSG  = 'Fill the all required fields'.
    ENDIF.
    IF GT_DATA-UMLGO IS INITIAL.
      GT_DATA-ICON = ICON_LED_RED.
      GT_DATA-MSG  = 'Fill the all required fields'.
    ENDIF.
    IF GT_DATA-BDTER IS INITIAL.
      GT_DATA-ICON = ICON_LED_RED.
      GT_DATA-MSG  = 'Fill the all required fields'.
    ENDIF.

    IF GT_DATA-UMLGO = '9001' OR GT_DATA-UMLGO = '9002' OR GT_DATA-UMLGO = '9003'
       OR GT_DATA-UMLGO = '9004'  OR GT_DATA-UMLGO = '9005'  .
    ELSE.
      GT_DATA-ICON = ICON_LED_RED.
      GT_DATA-MSG  = 'Check To Storage location'.
    ENDIF.

    IF GT_DATA-ICON IS INITIAL.
      GT_DATA-ICON = ICON_LED_INACTIVE.
    ENDIF.

    MODIFY GT_DATA INDEX LV_INDEX.
  ENDLOOP.

  SORT GT_DATA BY CHARG UMLGO.
ENDFORM.                    " EXCEL_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  ALV_EVENT
*&---------------------------------------------------------------------*
FORM ALV_EVENT .
  CALL METHOD GR_ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT GR_EVENT_HANDLER.
  SET HANDLER GR_EVENT_HANDLER->HANDLE_DATA_CHANGED   FOR GR_ALV_GRID.
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
  DATA: L_TEXT(255)  TYPE C.

  CONCATENATE TEXT-T01 P_BWART INTO L_TEXT SEPARATED BY SPACE. "Movement Type :

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  CONCATENATE TEXT-T02 P_WERKS INTO L_TEXT SEPARATED BY SPACE. "Plant :

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  CONCATENATE TEXT-T03 P_LGORT INTO L_TEXT SEPARATED BY SPACE. "From Storage Location :

  CALL METHOD CL_DD->ADD_TEXT
    EXPORTING
      TEXT         = L_TEXT
      SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
      STYLE_CLASS  = SPACE.

  CALL METHOD CL_DD->NEW_LINE
    EXPORTING
      REPEAT = 0.

  IF P_SEAR EQ 'X'. "조회모드
    IF S_UMLGO-LOW IS NOT INITIAL AND S_UMLGO-HIGH IS INITIAL.
      CONCATENATE TEXT-T06 S_UMLGO-LOW INTO L_TEXT SEPARATED BY SPACE. "To Storage Location : '
    ELSEIF S_UMLGO-LOW IS NOT INITIAL AND S_UMLGO-HIGH IS NOT INITIAL.
      CONCATENATE TEXT-T06  S_UMLGO-LOW '~' S_UMLGO-HIGH INTO L_TEXT SEPARATED BY SPACE.
    ELSE.
      CONCATENATE TEXT-T06 'All' INTO L_TEXT SEPARATED BY SPACE.
    ENDIF.
    CALL METHOD CL_DD->ADD_TEXT
      EXPORTING
        TEXT         = L_TEXT
        SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
        STYLE_CLASS  = SPACE.

    CALL METHOD CL_DD->NEW_LINE
      EXPORTING
        REPEAT = 0.

    IF S_BDTER-LOW IS NOT INITIAL AND S_BDTER-HIGH IS INITIAL.
      CONCATENATE TEXT-T05 S_BDTER-LOW INTO L_TEXT SEPARATED BY SPACE. "'Requirements Date : '
    ELSEIF S_BDTER-LOW IS NOT INITIAL AND S_BDTER-HIGH IS NOT INITIAL.
      CONCATENATE TEXT-T05  S_BDTER-LOW '~' S_BDTER-HIGH INTO L_TEXT SEPARATED BY SPACE.
    ELSE.
      CONCATENATE TEXT-T05 'All' INTO L_TEXT SEPARATED BY SPACE.
    ENDIF.
    CALL METHOD CL_DD->ADD_TEXT
      EXPORTING
        TEXT         = L_TEXT
        SAP_FONTSIZE = CL_DD_DOCUMENT=>MEDIUM
        STYLE_CLASS  = SPACE.
  ENDIF.

ENDFORM.                    " HANDLE_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& Form RESERV_CREATE
*&---------------------------------------------------------------------*
FORM RESERV_CREATE .
  DATA : LS_HEADER LIKE BAPIRKPFC,
         LV_RES_NO LIKE BAPIRKPFC-RES_NO,
         LT_RESERV LIKE TABLE OF BAPIRESBC WITH HEADER LINE,
         LT_RETURN LIKE TABLE OF BAPIRETURN WITH HEADER LINE,
         LT_ROWS   LIKE TABLE OF LVC_S_ROW WITH HEADER LINE,
         LT_MCHA   LIKE TABLE OF MCHA WITH HEADER LINE,
         LT_TEMP   LIKE TABLE OF GT_DATA WITH HEADER LINE,
         LT_DATA   LIKE TABLE OF GT_DATA WITH HEADER LINE,
         LV_INDEX  TYPE SY-TABIX VALUE 1,
         LV_CHK.

  CLEAR: LT_ROWS, LT_ROWS[].

  CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS[].

  IF LT_ROWS[] IS INITIAL.
    MESSAGE S014 DISPLAY LIKE C_E.
  ELSE.
    CLEAR: LS_HEADER, LT_RESERV, LT_RESERV[], LT_RETURN, LT_RETURN[].
*--Header
    LS_HEADER-MOVE_TYPE  = P_BWART.
    LS_HEADER-MOVE_PLANT = P_WERKS.
    LS_HEADER-PLANT      = P_WERKS.
    LS_HEADER-RES_DATE   = SY-DATUM.
    LS_HEADER-CREATED_BY = SY-UNAME.

*--Batch data
    LT_TEMP[] = GT_DATA[].
    SORT LT_TEMP BY MATNR CHARG.
    DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING MATNR CHARG.

    SELECT MATNR, WERKS, CHARG
      INTO CORRESPONDING FIELDS OF TABLE @LT_MCHA
      FROM MCHA
      FOR ALL ENTRIES IN @LT_TEMP
      WHERE MATNR = @LT_TEMP-MATNR
        AND WERKS = @P_WERKS
        AND CHARG = @LT_TEMP-CHARG.

    LOOP AT LT_ROWS.
      CLEAR GT_DATA.
      READ TABLE GT_DATA INDEX LT_ROWS-INDEX.
      IF SY-SUBRC = 0.
        IF GT_DATA-RSNUM IS NOT INITIAL. "Reservation no가 있으면
          GT_DATA-ICON = ICON_LED_RED.
          GT_DATA-MSG  = 'Reservation is already exists.'.
          MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.
          CONTINUE.
        ELSE.
          READ TABLE LT_MCHA WITH KEY MATNR = GT_DATA-MATNR
                                      WERKS = P_WERKS
                                      CHARG = GT_DATA-CHARG. "Batch #
          IF SY-SUBRC NE 0. "BATCH # 확인
            GT_DATA-ICON = ICON_LED_RED.
            GT_DATA-MSG = 'Batch No. dose not exists'.
            MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.
            CONTINUE.
          ENDIF.

          MOVE-CORRESPONDING GT_DATA TO LT_DATA.
          APPEND LT_DATA.
          CLEAR LT_DATA.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT LT_DATA BY CHARG UMLGO.

    LOOP AT LT_DATA.
      LV_INDEX = LV_INDEX + 1. "read next row
      READ TABLE LT_DATA INTO DATA(LS_DATA) INDEX LV_INDEX.
      IF SY-SUBRC EQ 0.
        "item table
        LT_RESERV-MATERIAL   = LT_DATA-MATNR.
        LT_RESERV-PLANT      = P_WERKS.
        LT_RESERV-STORE_LOC  = P_LGORT.       "from storage loc.
        LT_RESERV-BATCH      = LT_DATA-CHARG. "batch
        LT_RESERV-QUANTITY   = LT_DATA-BDMNG.
        LT_RESERV-UNIT       = LT_DATA-MEINS.
        LT_RESERV-REQ_DATE   = LT_DATA-BDTER.
        LT_RESERV-SHORT_TEXT = LT_DATA-SGTXT.
        LT_RESERV-MOVEMENT   = 'X'.
        APPEND LT_RESERV. CLEAR : LT_RESERV.

        IF LT_DATA-CHARG NE LS_DATA-CHARG OR LT_DATA-UMLGO NE LS_DATA-UMLGO.
          "----bapi
          "header to storage loc
          LS_HEADER-MOVE_STLOC = LT_DATA-UMLGO.

          CALL FUNCTION 'BAPI_RESERVATION_CREATE'
            EXPORTING
              RESERVATION_HEADER = LS_HEADER
            IMPORTING
              RESERVATION        = LV_RES_NO
            TABLES
              RESERVATION_ITEMS  = LT_RESERV
              RETURN             = LT_RETURN.

          READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
          IF SY-SUBRC = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            LT_DATA-ICON = ICON_LED_RED.
            LT_DATA-MSG  = LT_RETURN-MESSAGE.
            MODIFY LT_DATA TRANSPORTING ICON MSG WHERE CHARG = LT_DATA-CHARG
                                                   AND UMLGO = LT_DATA-UMLGO.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.
            LT_DATA-ICON = ICON_LED_GREEN.
            LT_DATA-MSG = 'Reservation Created'.
            LT_DATA-RSNUM = LV_RES_NO.
          ENDIF.
          MODIFY LT_DATA TRANSPORTING ICON MSG RSNUM WHERE CHARG = LT_DATA-CHARG
                                                       AND UMLGO = LT_DATA-UMLGO.

          "clear
          CLEAR : LT_RESERV, LT_RESERV[], LT_RETURN, LT_RETURN[], LS_DATA, LT_DATA.
        ENDIF.

      ELSE. " 마지막 라인
        "item table
        LT_RESERV-MATERIAL   = LT_DATA-MATNR.
        LT_RESERV-PLANT      = P_WERKS.
        LT_RESERV-STORE_LOC  = P_LGORT. "from storage loc.
        LT_RESERV-BATCH      = LT_DATA-CHARG. "batch
        LT_RESERV-QUANTITY   = LT_DATA-BDMNG.
        LT_RESERV-UNIT       = LT_DATA-MEINS.
        LT_RESERV-REQ_DATE   = LT_DATA-BDTER.
        LT_RESERV-SHORT_TEXT = LT_DATA-SGTXT.
        LT_RESERV-MOVEMENT   = 'X'.
        APPEND LT_RESERV. CLEAR : LT_RESERV.

        "----bapi
        "header to storage loc
        LS_HEADER-MOVE_STLOC = LT_DATA-UMLGO.

        CALL FUNCTION 'BAPI_RESERVATION_CREATE'
          EXPORTING
            RESERVATION_HEADER = LS_HEADER
          IMPORTING
            RESERVATION        = LV_RES_NO
          TABLES
            RESERVATION_ITEMS  = LT_RESERV
            RETURN             = LT_RETURN.

        READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          LT_DATA-ICON = ICON_LED_RED.
          LT_DATA-MSG = LT_RETURN-MESSAGE.
          MODIFY LT_DATA TRANSPORTING ICON MSG WHERE CHARG = LT_DATA-CHARG
                                                 AND UMLGO = LT_DATA-UMLGO.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
          LT_DATA-ICON = ICON_LED_GREEN.
          LT_DATA-MSG = 'Reservation Created'.
          LT_DATA-RSNUM = LV_RES_NO.
        ENDIF.
        MODIFY LT_DATA TRANSPORTING ICON MSG RSNUM WHERE CHARG = LT_DATA-CHARG
                                                     AND UMLGO = LT_DATA-UMLGO.
        "clear
        CLEAR : LT_RESERV, LT_RESERV[], LT_RETURN, LT_RETURN[], LS_DATA, LT_DATA.
      ENDIF.
    ENDLOOP.

    WAIT UP TO 1 SECONDS.

    "GT_DATA UPDATE
    IF LT_DATA[] IS NOT INITIAL.
      SELECT RSNUM, RSPOS, MATNR
        INTO TABLE @DATA(LT_RESB)
        FROM RESB
        FOR ALL ENTRIES IN @LT_DATA
        WHERE RSNUM EQ @LT_DATA-RSNUM
          AND MATNR EQ @LT_DATA-MATNR
          AND WERKS EQ @P_WERKS.
    ENDIF.

    SORT LT_RESB BY RSNUM MATNR.

    LOOP AT LT_DATA.
      READ TABLE LT_RESB INTO DATA(LS_RESB) WITH KEY RSNUM = LT_DATA-RSNUM
                                                     MATNR = LT_DATA-MATNR BINARY SEARCH.

      IF SY-SUBRC = 0.
        LT_DATA-RSPOS = LS_RESB-RSPOS.
        CLEAR LS_RESB.
      ENDIF.

      MODIFY GT_DATA FROM LT_DATA TRANSPORTING ICON MSG RSNUM RSPOS WHERE MATNR = LT_DATA-MATNR
                                                                      AND CHARG = LT_DATA-CHARG
                                                                      AND UMLGO = LT_DATA-UMLGO.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RESERV_DELETE
*&---------------------------------------------------------------------*
FORM RESERV_DELETE .
  DATA : LV_RSNUM   LIKE RESB-RSNUM, "bapi2093_res_key,
         LV_RSPOS   LIKE RESB-RSPOS,
         LT_RESERV  LIKE TABLE OF BAPI2093_RES_ITEM_CHANGE WITH HEADER LINE,
         LT_RESERVX LIKE TABLE OF BAPI2093_RES_ITEM_CHANGEX WITH HEADER LINE,
         LT_RETURN  LIKE TABLE OF BAPIRET2 WITH HEADER LINE,
         LT_ROWS    LIKE TABLE OF LVC_S_ROW WITH HEADER LINE,
         LT_TEMP    LIKE TABLE OF GT_DATA WITH HEADER LINE.

  CLEAR GV_ANSWER.
  PERFORM CONFIRM_POPUP USING 'Delete'
                              'Want to Delete? '
                              SPACE
                              SPACE
                        CHANGING GV_ANSWER.
  IF GV_ANSWER = 1.
    CLEAR : LT_ROWS[].
    CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = LT_ROWS[].
    IF LT_ROWS[] IS INITIAL.
      MESSAGE S014 DISPLAY LIKE C_E.
    ELSE.

      SORT LT_ROWS BY INDEX DESCENDING.

      LOOP AT LT_ROWS.
        CLEAR : LV_RSNUM, LV_RSPOS.
        READ TABLE GT_DATA INDEX LT_ROWS-INDEX.
        IF SY-SUBRC EQ 0.
          IF GT_DATA-KZEAR IS INITIAL. "Completed 되지 않았으면
            LV_RSNUM = GT_DATA-RSNUM.  "Reservation No.
            LV_RSPOS = GT_DATA-RSPOS.
            LT_RESERV-RES_ITEM = LV_RSPOS.
            LT_RESERVX-RES_ITEM = LV_RSPOS.

            LT_RESERV-DELETE_IND = C_X.
            LT_RESERVX-DELETE_IND = C_X.
            APPEND LT_RESERV. CLEAR LT_RESERV.
            APPEND LT_RESERVX. CLEAR LT_RESERVX.

            CALL FUNCTION 'BAPI_RESERVATION_CHANGE'
              EXPORTING
                RESERVATION               = LV_RSNUM
              TABLES
                RESERVATIONITEMS_CHANGED  = LT_RESERV
                RESERVATIONITEMS_CHANGEDX = LT_RESERVX
                RETURN                    = LT_RETURN.

            READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
            IF SY-SUBRC EQ 0.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              GT_DATA-ICON = ICON_LED_RED.
              GT_DATA-MSG  = LT_RETURN-MESSAGE.
              MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.
              WAIT UP TO 1 SECONDS.
              DELETE GT_DATA INDEX LT_ROWS-INDEX.
            ENDIF.

            CLEAR:LT_RETURN, LT_RETURN[],LT_RESERV, LT_RESERVX, LT_RESERV[], LT_RESERVX[].
          ELSE. "컴플리트 된 경우 삭제 불가능
            GT_DATA-ICON = ICON_LED_RED.
            GT_DATA-MSG = 'Reservation Completed'.
            MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.
          ENDIF.
        ENDIF.
      ENDLOOP.
*      PERFORM ALV_REFRESH.
    ENDIF.
  ELSE.
    MESSAGE S000 WITH 'Process Cancled.' DISPLAY LIKE C_E.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form RESERV_COMPLETE
*&---------------------------------------------------------------------*
FORM RESERV_COMPLETE .
  CLEAR GV_ANSWER.
  PERFORM CONFIRM_POPUP USING 'Withdraw'
                              'Want to Withdraw?'
                              SPACE
                              SPACE
                        CHANGING GV_ANSWER.
  IF GV_ANSWER = 1.

    DATA : LV_RSNUM   LIKE RESB-RSNUM, "bapi2093_res_key,
           LT_RESERV  LIKE TABLE OF BAPI2093_RES_ITEM_CHANGE WITH HEADER LINE,
           LT_RESERVX LIKE TABLE OF BAPI2093_RES_ITEM_CHANGEX WITH HEADER LINE,
           LT_RETURN  LIKE TABLE OF BAPIRET2 WITH HEADER LINE,
           LT_ROWS    LIKE TABLE OF LVC_S_ROW WITH HEADER LINE,
           LT_TEMP    LIKE TABLE OF GT_DATA WITH HEADER LINE,
           LT_RESB    LIKE TABLE OF RESB WITH HEADER LINE.
    CLEAR : LT_ROWS[].
    CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = LT_ROWS[].
    IF LT_ROWS[] IS INITIAL.
      MESSAGE S014 DISPLAY LIKE C_E.
    ELSE.
*      lt_temp[] = gt_data[].
*      SORT lt_temp BY rsnum matnr.
*      DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING rsnum matnr.
*      SELECT rsnum rspos matnr
*       FROM resb
*        INTO CORRESPONDING FIELDS OF TABLE lt_resb
*        FOR ALL ENTRIES IN lt_temp
*        WHERE rsnum EQ lt_temp-rsnum
*          AND matnr EQ lt_temp-matnr.

      LOOP AT LT_ROWS.

        READ TABLE GT_DATA INDEX LT_ROWS-INDEX.
        IF SY-SUBRC EQ 0.
          IF GT_DATA-KZEAR IS INITIAL.
            LV_RSNUM = GT_DATA-RSNUM.
*            CLEAR lt_resb.
*            READ TABLE lt_resb WITH KEY rsnum = gt_data-rsnum
*                                        matnr = gt_data-matnr.
*            IF sy-subrc = 0.
*              lt_reserv-res_item = lt_resb-rspos.
*              lt_reservx-res_item = lt_resb-rspos.
*            ENDIF.
*            LT_RESERV-RES_ITEM = GT_DATA-RSPOS.
*            LT_RESERVX-RES_ITEM = GT_DATA-RSPOS.
            LT_RESERV-WITHDRAWN = C_X.
            LT_RESERVX-WITHDRAWN = C_X.
            APPEND LT_RESERV. CLEAR LT_RESERV.
            APPEND LT_RESERVX. CLEAR LT_RESERVX.


            CALL FUNCTION 'BAPI_RESERVATION_CHANGE'
              EXPORTING
                RESERVATION               = LV_RSNUM
**   TESTRUN                         =
**   ATPCHECK                        =
              TABLES
                RESERVATIONITEMS_CHANGED  = LT_RESERV
                RESERVATIONITEMS_CHANGEDX = LT_RESERVX
**   RESERVATIONITEMS_NEW            =
                RETURN                    = LT_RETURN.
**   EXTENSIONIN                     =

            READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
            IF SY-SUBRC EQ 0.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              GT_DATA-ICON = ICON_LED_RED.
              GT_DATA-MSG  = LT_RETURN-MESSAGE.
              MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.

              GT_DATA-ICON = ICON_LED_GREEN.
              GT_DATA-MSG  = 'Completed'.
              GT_DATA-KZEAR = C_X.
              GT_DATA-CELL_MODE = C_I.
              MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG KZEAR CELL_MODE.
            ENDIF.
            CLEAR: LV_RSNUM, LT_RETURN, LT_RETURN[],LT_RESERV, LT_RESERVX, LT_RESERV[], LT_RESERVX[].

          ELSE.
            GT_DATA-ICON = ICON_LED_RED.
            GT_DATA-MSG  = 'Already Completed'.
            MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM ALV_CELL_STYLE.
      PERFORM ALV_REFRESH.
    ENDIF.
  ELSE.
    MESSAGE S000 WITH 'Process Cancled.' DISPLAY LIKE C_E.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA .
  IF P_UPLO IS NOT INITIAL.
    IF P_BWART IS INITIAL OR
       P_WERKS IS INITIAL OR
       P_LGORT IS INITIAL OR
       P_FILE  IS INITIAL.
      MESSAGE S000 WITH 'Fill the all required fields.' DISPLAY LIKE C_E.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    IF P_BWART IS INITIAL OR
       P_WERKS IS INITIAL OR
       P_LGORT IS INITIAL.

      MESSAGE S000 WITH 'Fill the all required fields.' DISPLAY LIKE C_E.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form RESERV_CHANGE
*&---------------------------------------------------------------------*
FORM RESERV_CHANGE .
  DATA : LV_RSNUM   LIKE RESB-RSNUM, "bapi2093_res_key,
         LT_RESERV  LIKE TABLE OF BAPI2093_RES_ITEM_CHANGE WITH HEADER LINE,
         LT_RESERVX LIKE TABLE OF BAPI2093_RES_ITEM_CHANGEX WITH HEADER LINE,
         LT_RETURN  LIKE TABLE OF BAPIRET2 WITH HEADER LINE,
         LT_ROWS    LIKE TABLE OF LVC_S_ROW WITH HEADER LINE,
         LT_TEMP    LIKE TABLE OF GT_DATA WITH HEADER LINE,
         LT_RESB    LIKE TABLE OF RESB WITH HEADER LINE.

  CLEAR GV_ANSWER.
  PERFORM CONFIRM_POPUP USING 'Change'
                              'Want to change?'
                              SPACE
                              SPACE
                        CHANGING GV_ANSWER.
  IF GV_ANSWER = 1.

    CLEAR : LT_ROWS[].
    CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = LT_ROWS[].
    IF LT_ROWS[] IS INITIAL.
      MESSAGE S014 DISPLAY LIKE C_E.
    ELSE.

      LOOP AT LT_ROWS.

        READ TABLE GT_DATA INDEX LT_ROWS-INDEX.
        IF SY-SUBRC EQ 0.
          IF GT_DATA-CHAN IS NOT INITIAL.
            LV_RSNUM = GT_DATA-RSNUM.

            LT_RESERV-ITEM_TEXT = GT_DATA-SGTXT.
            LT_RESERVX-ITEM_TEXT = C_X.

*            LT_RESERV-RES_ITEM = GT_DATA-RSPOS.
*            LT_RESERVX-RES_ITEM = GT_DATA-RSPOS.

            LT_RESERV-ENTRY_QNT = GT_DATA-BDMNG.
            LT_RESERVX-ENTRY_QNT = C_X.

            APPEND LT_RESERV. CLEAR LT_RESERV.
            APPEND LT_RESERVX. CLEAR LT_RESERVX.


            CALL FUNCTION 'BAPI_RESERVATION_CHANGE'
              EXPORTING
                RESERVATION               = LV_RSNUM
              TABLES
                RESERVATIONITEMS_CHANGED  = LT_RESERV
                RESERVATIONITEMS_CHANGEDX = LT_RESERVX
                RETURN                    = LT_RETURN.


            READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
            IF SY-SUBRC EQ 0.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              GT_DATA-ICON = ICON_LED_RED.
              GT_DATA-MSG  = LT_RETURN-MESSAGE.
              MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.

              GT_DATA-ICON = ICON_LED_GREEN.
              GT_DATA-MSG  = 'Changed'.
              MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG KZEAR.
            ENDIF.
            CLEAR: LV_RSNUM, LT_RETURN, LT_RETURN[],LT_RESERV, LT_RESERVX, LT_RESERV[], LT_RESERVX[].
          ELSE.
            GT_DATA-ICON = ICON_LED_RED.
            GT_DATA-MSG = 'Data not changed.'.
            MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.
          ENDIF.
        ENDIF.
      ENDLOOP.
      PERFORM ALV_REFRESH.
    ENDIF.
  ELSE.
    MESSAGE S000 WITH 'Process Cancled.' DISPLAY LIKE C_E.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED  USING PR_DATA_CHANGED
                                 TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA : LS_MOD_CELL TYPE LVC_S_MODI.
*         ls_data     LIKE gt_data.
  FIELD-SYMBOLS : <FIELD1> TYPE ANY.

  SORT PR_DATA_CHANGED->MT_MOD_CELLS BY ROW_ID .
  LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELL.
    ASSIGN COMPONENT LS_MOD_CELL-FIELDNAME OF STRUCTURE GT_DATA TO <FIELD1>.
    CLEAR: GT_DATA.
    READ TABLE GT_DATA INDEX LS_MOD_CELL-ROW_ID.

    CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
      EXPORTING
        I_ROW_ID    = LS_MOD_CELL-ROW_ID
        I_FIELDNAME = LS_MOD_CELL-FIELDNAME
      IMPORTING
        E_VALUE     = <FIELD1>.

    GT_DATA-CHAN = C_X. "Data가 변경된부분 체크
    MODIFY GT_DATA INDEX LS_MOD_CELL-ROW_ID.
  ENDLOOP.
  PERFORM ALV_REFRESH.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_POPUP
*&---------------------------------------------------------------------*
FORM CONFIRM_POPUP USING P_TITLE P_TEXT P_CANC P_TYPE
                    CHANGING PV_ANSWER.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = P_TITLE
      TEXT_QUESTION         = P_TEXT
      TEXT_BUTTON_1         = 'YES'
      ICON_BUTTON_1         = ' '
      TEXT_BUTTON_2         = 'NO'
      ICON_BUTTON_2         = ' '
      DEFAULT_BUTTON        = '2'
      DISPLAY_CANCEL_BUTTON = P_CANC
      START_COLUMN          = 25
      START_ROW             = 6
      POPUP_TYPE            = P_TYPE
    IMPORTING
      ANSWER                = PV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
ENDFORM.                    " CONFIRM_POPUP
*&---------------------------------------------------------------------*
*& Form ALV_CELL_COLOR
*&---------------------------------------------------------------------*
FORM ALV_CELL_COLOR .
  DATA : LS_COLOR TYPE LVC_S_SCOL,
         LT_COLOR TYPE LVC_T_SCOL.

  LOOP AT GT_DATA.
    CLEAR : GT_DATA-CELL_COLOR, LS_COLOR, LT_COLOR.
    LS_COLOR-FNAME = 'KWMENG'.
*    ls_color-color-col = '3'.
    LS_COLOR-COLOR-INT = '1'.
*      ls_color-color-inv = '0'.
    INSERT LS_COLOR INTO TABLE LT_COLOR.

    INSERT LINES OF LT_COLOR INTO TABLE GT_DATA-CELL_COLOR.
    MODIFY GT_DATA.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_CELL_STYLE
*&---------------------------------------------------------------------*
FORM ALV_CELL_STYLE .
  DATA : LS_STYLE TYPE LVC_S_STYL,
         LT_STYLE TYPE LVC_T_STYL.

  LOOP AT GT_DATA.
    CLEAR : GT_DATA-CELL_STYLE, LS_STYLE, LT_STYLE.
    CASE GT_DATA-CELL_MODE.
      WHEN 'I'. "처음 화면
        LS_STYLE-FIELDNAME = 'BDMNG'.
        LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT LS_STYLE INTO TABLE LT_STYLE.

        LS_STYLE-FIELDNAME = 'SGTXT'.
        LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        INSERT LS_STYLE INTO TABLE LT_STYLE.
      WHEN 'C'. "수정 모드
        LS_STYLE-FIELDNAME = 'SGTXT'.
        LS_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        INSERT LS_STYLE INTO TABLE LT_STYLE.
    ENDCASE.

    INSERT LINES OF LT_STYLE INTO TABLE GT_DATA-CELL_STYLE.
    MODIFY GT_DATA.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM HANDLE_TOOLBAR  USING P_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.
  DATA: LS_TOOLBAR TYPE STB_BUTTON.

  MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  MOVE ICON_REFRESH TO LS_TOOLBAR-ICON.
  MOVE 'REFR' TO LS_TOOLBAR-FUNCTION.
*  MOVE 'Refresh' TO ls_toolbar-text.
  MOVE 'Refresh' TO LS_TOOLBAR-QUICKINFO.
  MOVE ' ' TO LS_TOOLBAR-DISABLED.
  APPEND LS_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

ENDFORM.                    " HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND   USING P_UCOMM.
  CASE P_UCOMM.
    WHEN 'REFR'.
      PERFORM GET_DATA.
      PERFORM ALV_CELL_STYLE.
      PERFORM ALV_CELL_COLOR.
      PERFORM ALV_REFRESH.
  ENDCASE.
ENDFORM.                    " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    P_ROW
                                   P_COLUMN.

  CASE P_COLUMN.
    WHEN 'RSNUM'.
      CLEAR : GT_DATA.
      READ TABLE GT_DATA INDEX P_ROW.
      CHECK SY-SUBRC EQ 0.
      SET PARAMETER ID 'RES' FIELD GT_DATA-RSNUM.
      CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.

    WHEN 'VBELN'.
      CLEAR : GT_DATA.
      READ TABLE GT_DATA INDEX P_ROW.
      CHECK SY-SUBRC EQ 0.
      SET PARAMETER ID 'AUN' FIELD GT_DATA-VBELN.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    WHEN 'MBLNR'.
      CLEAR : GT_DATA.
      READ TABLE GT_DATA INDEX P_ROW.
      CHECK SY-SUBRC EQ 0.
      SET PARAMETER ID 'MBN' FIELD GT_DATA-MBLNR.
      CALL TRANSACTION 'MIGO_GO' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TRANS_POST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM TRANS_POST .
  CLEAR GV_ANSWER.
  PERFORM CONFIRM_POPUP USING 'Transfer Posting'
                              'Want to Transfer Posting?'
                              SPACE
                              SPACE
                        CHANGING GV_ANSWER.
  IF GV_ANSWER = 1.

    DATA : LT_ROWS         LIKE TABLE OF LVC_S_ROW WITH HEADER LINE,

           LS_HEADER       LIKE  BAPI2017_GM_HEAD_01,
           LS_CODE         LIKE  BAPI2017_GM_CODE,
           LS_HEADRET      LIKE  BAPI2017_GM_HEAD_RET,

           LV_GOODSMVT_DOC LIKE  BAPI2017_GM_HEAD_RET-MAT_DOC,
           LV_MAC_DOC      TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC,
           LV_DOC_YEAR     TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR,

           LT_ITEM         LIKE  BAPI2017_GM_ITEM_CREATE OCCURS  0 WITH  HEADER  LINE,
           LT_RETURN       LIKE  BAPIRET2 OCCURS  0 WITH  HEADER  LINE,
           LT_TEMP         LIKE TABLE OF  GT_DATA WITH HEADER LINE.

    DATA : LT_POPUP LIKE TABLE OF SVAL WITH HEADER LINE,
           LV_BUDAT TYPE SY-DATUM.


    CLEAR : LT_ROWS[].
    CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = LT_ROWS[].

    IF LT_ROWS[] IS INITIAL.
      MESSAGE S014 DISPLAY LIKE C_E.
    ELSE.
      "ERROR CHECK
      LOOP AT LT_ROWS.
        READ TABLE GT_DATA INDEX LT_ROWS-INDEX.
        IF GT_DATA-KZEAR IS NOT INITIAL. "COMPLETE 상태
          MESSAGE S000 WITH 'Already Completed' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        IF GT_DATA-CLABS LT GT_DATA-BDMNG. "재고가 처리 할 수량 적으면
          MESSAGE S000 WITH 'Check the stock' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        MOVE-CORRESPONDING GT_DATA TO LT_TEMP.
        APPEND LT_TEMP.

        CLEAR : GT_DATA, LT_TEMP.
      ENDLOOP.

      " 팝업창
      CLEAR LT_POPUP.
      LT_POPUP-TABNAME   = 'GOHEAD'.
      LT_POPUP-FIELDNAME = 'BUDAT'.
      LT_POPUP-VALUE     = SY-DATUM.
      APPEND LT_POPUP.

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          POPUP_TITLE = 'posting date'
        TABLES
          FIELDS      = LT_POPUP.

      " 입력받은 POST DATE
      CLEAR LT_POPUP.
      READ TABLE LT_POPUP WITH KEY FIELDNAME = 'BUDAT'.
      LV_BUDAT = LT_POPUP-VALUE.

      "----header
      CLEAR LS_HEADER.
      LS_HEADER-PSTNG_DATE   = LV_BUDAT. " 기표일자
      LS_HEADER-DOC_DATE     = SY-DATUM. " 증빙일자
      LS_CODE-GM_CODE        = '04'.     " 재고이동

      LOOP AT LT_TEMP.
        CLEAR: LT_ITEM, LT_RETURN.

        LT_ITEM-MOVE_TYPE  = P_BWART.
        LT_ITEM-RESERV_NO  = LT_TEMP-RSNUM.
        LT_ITEM-RES_ITEM   = LT_TEMP-RSPOS.

        LT_ITEM-MATERIAL   = LT_TEMP-MATNR.
        LT_ITEM-PLANT      = P_WERKS.
        LT_ITEM-STGE_LOC   = P_LGORT.
        LT_ITEM-BATCH      = LT_TEMP-CHARG.

        LT_ITEM-MOVE_MAT   = LT_TEMP-MATNR.
        LT_ITEM-MOVE_PLANT = P_WERKS.
        LT_ITEM-MOVE_STLOC = LT_TEMP-UMLGO.
        LT_ITEM-MOVE_BATCH = LT_TEMP-CHARG.

        LT_ITEM-ENTRY_QNT  = LT_TEMP-BDMNG.
        LT_ITEM-ENTRY_UOM  = LT_TEMP-MEINS.
        APPEND LT_ITEM.

        AT END OF RSNUM.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
            EXPORTING
              GOODSMVT_HEADER  = LS_HEADER
              GOODSMVT_CODE    = LS_CODE
            IMPORTING
              GOODSMVT_HEADRET = LS_HEADRET
            TABLES
              GOODSMVT_ITEM    = LT_ITEM
              RETURN           = LT_RETURN.

          READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
          IF SY-SUBRC = 0. "ERROR 있을 경우
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

            LT_TEMP-ICON = ICON_LED_RED.
            LT_TEMP-MSG  = LT_RETURN-MESSAGE.
            MODIFY LT_TEMP TRANSPORTING ICON MSG WHERE RSNUM = LT_TEMP-RSNUM.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.

            LT_TEMP-ICON = ICON_LED_GREEN.
            LT_TEMP-MSG  = 'Transfer posting complete.'.
            LT_TEMP-KZEAR = 'X'.
            LT_TEMP-CELL_MODE = C_I.
            MODIFY LT_TEMP TRANSPORTING ICON MSG KZEAR CELL_MODE WHERE RSNUM = LT_TEMP-RSNUM.
          ENDIF.

          CLEAR: LT_ITEM, LT_ITEM[], LT_TEMP.
        ENDAT.
      ENDLOOP.

      CLEAR LT_TEMP.
      LOOP AT LT_TEMP.
        MODIFY GT_DATA FROM LT_TEMP TRANSPORTING ICON MSG KZEAR CELL_MODE WHERE RSNUM = LT_TEMP-RSNUM
                                                                            AND MATNR = LT_TEMP-MATNR.
      ENDLOOP.
*      PERFORM ALV_CELL_STYLE.
*      PERFORM ALV_REFRESH.
    ENDIF.
  ELSE.
    MESSAGE S000 WITH 'Process Cancled.' DISPLAY LIKE C_E.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GI_OTHERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GI_OTHERS .
  CLEAR GV_ANSWER.
  PERFORM CONFIRM_POPUP USING 'GI OTHERS'
                              'Want to GI others?'
                              SPACE
                              SPACE
                        CHANGING GV_ANSWER.

  IF GV_ANSWER = 1.
    DATA : LT_ROWS    LIKE TABLE OF LVC_S_ROW WITH HEADER LINE,
           LT_POPUP   LIKE TABLE OF SVAL WITH HEADER LINE,
           LV_KOSTL   TYPE RKPF-KOSTL,
           LV_BUDAT   TYPE SY-DATUM,

           LS_HEADER  LIKE  BAPI2017_GM_HEAD_01,
           LS_CODE    LIKE  BAPI2017_GM_CODE,

           LS_HEADRET LIKE  BAPI2017_GM_HEAD_RET,
           LV_MAC_DOC TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC,

           LT_ITEM    LIKE  BAPI2017_GM_ITEM_CREATE OCCURS  0 WITH  HEADER  LINE,
           LT_RETURN  LIKE  BAPIRET2 OCCURS  0 WITH  HEADER  LINE,
           LT_TEMP    LIKE  TABLE OF GT_DATA WITH HEADER LINE,
           LV_CHK.

    CLEAR : LT_ROWS[].
    CALL METHOD GR_ALV_GRID->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = LT_ROWS[].

    IF LT_ROWS[] IS INITIAL. "선택 된 row 가 없을때
      MESSAGE S014 DISPLAY LIKE C_E.
    ELSE.

      CLEAR : LT_TEMP, LT_TEMP[], LV_CHK.
      LOOP AT LT_ROWS.
        READ TABLE GT_DATA INDEX LT_ROWS-INDEX.
        IF SY-SUBRC EQ 0.
          " 9001, 9002 외 row 를 선택 했을 경우
          IF GT_DATA-UMLGO NE '9001' AND GT_DATA-UMLGO NE '9002'.
            GT_DATA-ICON = ICON_LED_RED.
            GT_DATA-MSG = 'Olny To Stor loc. 9001 or 9002'.
            MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.

            LV_CHK = 'X'.
            CONTINUE.
          ENDIF.

          " complete 되지 않으면 error
          IF GT_DATA-KZEAR IS INITIAL.
            GT_DATA-ICON = ICON_LED_RED.
            GT_DATA-MSG = 'Reservation not completed'.
            MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.

            LV_CHK = 'X'.
            CONTINUE.
          ENDIF.

          IF GT_DATA-MBLNR IS NOT INITIAL.
            GT_DATA-ICON = ICON_LED_RED.
            GT_DATA-MSG = 'Already GI posted'.
            MODIFY GT_DATA INDEX LT_ROWS-INDEX TRANSPORTING ICON MSG.

            LV_CHK = 'X'.
            CONTINUE.
          ENDIF.

          MOVE-CORRESPONDING GT_DATA TO LT_TEMP.
          APPEND LT_TEMP.

          CLEAR : GT_DATA, LT_TEMP.
        ENDIF.
      ENDLOOP.

      IF LV_CHK IS INITIAL.
        " 팝업창
        CLEAR LT_POPUP.
        LT_POPUP-TABNAME = 'RKPF'.
        LT_POPUP-FIELDNAME = 'KOSTL'.
        LT_POPUP-VALUE     = '30010'.
        APPEND LT_POPUP.

        CLEAR LT_POPUP.
        LT_POPUP-TABNAME = 'GOHEAD'.
        LT_POPUP-FIELDNAME = 'BUDAT'.
        LT_POPUP-VALUE     = SY-DATUM.
        APPEND LT_POPUP.

        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            POPUP_TITLE = 'cost center & post date'
          TABLES
            FIELDS      = LT_POPUP.

        " 입력받은 COST CENTER
        CLEAR LT_POPUP.
        READ TABLE LT_POPUP WITH KEY FIELDNAME = 'KOSTL'.
        LV_KOSTL = LT_POPUP-VALUE.

        " 입력받은 POST DATE
        CLEAR LT_POPUP.
        READ TABLE LT_POPUP WITH KEY FIELDNAME = 'BUDAT'.
        LV_BUDAT = LT_POPUP-VALUE.

        "header
        LS_HEADER-PSTNG_DATE   = LV_BUDAT. " 기표일자
        LS_CODE-GM_CODE        = '03'.     " 재고이동


        LOOP AT LT_TEMP.
          CLEAR LT_ITEM.
          "item
          LT_ITEM-MATERIAL   = LT_TEMP-MATNR.

          LT_ITEM-ENTRY_QNT  = LT_TEMP-BDMNG. "qty
          LT_ITEM-ENTRY_UOM  = LT_TEMP-MEINS. "uom

          LT_ITEM-MOVE_TYPE  = 'Z01'.  "P_BWART.
          LT_ITEM-MOVE_PLANT = P_WERKS.

          LT_ITEM-BATCH      = LT_TEMP-CHARG.
          LT_ITEM-PLANT      = P_WERKS.
          LT_ITEM-STGE_LOC   = LT_TEMP-UMLGO.

          LT_ITEM-COSTCENTER = LV_KOSTL.

          APPEND LT_ITEM.

          AT END OF RSNUM.
            LS_HEADER-HEADER_TXT   = LT_TEMP-RSNUM. "header text : reservation no

            "bapi
            CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
              EXPORTING
                GOODSMVT_HEADER  = LS_HEADER
                GOODSMVT_CODE    = LS_CODE
              IMPORTING
                GOODSMVT_HEADRET = LS_HEADRET
                MATERIALDOCUMENT = LV_MAC_DOC
              TABLES
                GOODSMVT_ITEM    = LT_ITEM
                RETURN           = LT_RETURN.

            READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
            IF SY-SUBRC = 0. "ERROR 있을 경우
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

              LT_TEMP-ICON = ICON_LED_RED.
              LT_TEMP-MSG  = LT_RETURN-MESSAGE.
              MODIFY LT_TEMP TRANSPORTING ICON MSG WHERE RSNUM = LT_TEMP-RSNUM.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.

              LT_TEMP-ICON = ICON_LED_GREEN.
              LT_TEMP-MSG  = 'GI other post complete'.
              LT_TEMP-MBLNR = LV_MAC_DOC.
              MODIFY LT_TEMP TRANSPORTING ICON MSG MBLNR WHERE RSNUM = LT_TEMP-RSNUM.
            ENDIF.

            CLEAR: LT_ITEM, LT_ITEM[], LT_TEMP.
          ENDAT.
        ENDLOOP.

        CLEAR LT_TEMP.
        LOOP AT LT_TEMP.
          MODIFY GT_DATA FROM LT_TEMP TRANSPORTING ICON MSG MBLNR WHERE RSNUM = LT_TEMP-RSNUM
                                                                    AND MATNR = LT_TEMP-MATNR.
        ENDLOOP.

      ENDIF.
*      PERFORM ALV_CELL_STYLE.
*      PERFORM ALV_REFRESH.
    ENDIF.
  ELSE. "취소
    MESSAGE S000 WITH 'Process Cancled.' DISPLAY LIKE C_E.
  ENDIF.
ENDFORM.
