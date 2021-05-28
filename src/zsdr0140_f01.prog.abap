*&---------------------------------------------------------------------*
*& Include          ZSD1R0030_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  DATA : L_FUNCTXT TYPE SMP_DYNTXT.

  L_FUNCTXT-ICON_ID   = ICON_XLS.
  L_FUNCTXT-ICON_TEXT = TEXT-002.
  L_FUNCTXT-QUICKINFO = TEXT-002.
  SSCRFIELDS-FUNCTXT_01 = L_FUNCTXT.

  L_FUNCTXT-ICON_ID   = ICON_REPORT_CALL.
  L_FUNCTXT-ICON_TEXT = TEXT-004.
  L_FUNCTXT-QUICKINFO = TEXT-004.
  SSCRFIELDS-FUNCTXT_02 = L_FUNCTXT.

ENDFORM.

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

  LV_OBJECT = TEXT-005."'ZSDR0140'.
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
*& Form SELECT_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SELECT_FILE  CHANGING P_FILE.

  DATA : L_TITLE TYPE STRING,
         L_RC    TYPE SY-SUBRC,
         L_LEN   TYPE I,
         LT_FILE TYPE FILETABLE WITH HEADER LINE.

  CLEAR   : L_RC, LT_FILE, P_FILE, L_LEN.
  REFRESH : LT_FILE.

  L_TITLE = 'Select Upload File'.

*-- File open dialog
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = L_TITLE
      FILE_FILTER             = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
    CHANGING
      FILE_TABLE              = LT_FILE[]
      RC                      = L_RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF NOT LT_FILE[] IS INITIAL.
*-- The length of file path should be in 128
      READ TABLE LT_FILE INDEX 1.
      IF SY-SUBRC EQ 0.
        L_LEN = STRLEN( LT_FILE ).
        IF L_LEN GE 128.
          MESSAGE S000 WITH 'File path to be uploaded is too long.'
                       DISPLAY LIKE C_E.
          LEAVE LIST-PROCESSING.
        ELSE.
          P_FILE = LT_FILE.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE S000 WITH 'File was not selected.'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCEL_LAYOUT_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM EXCEL_LAYOUT_OUTPUT .

  DATA: LO_CONTAINER1  TYPE REF TO CL_GUI_CONTAINER,
        LO_CONTROL     TYPE REF TO I_OI_CONTAINER_CONTROL,
        LO_DOCUMENT    TYPE REF TO I_OI_DOCUMENT_PROXY,
        LO_ERROR       TYPE REF TO I_OI_ERROR OCCURS 0
                                      WITH HEADER LINE,
        LV_RETCODE     TYPE SOI_RET_STRING,
        LO_SPREADSHEET TYPE REF TO I_OI_SPREADSHEET,
        LV_INITIAL     TYPE C.

  DATA: LT_DOC_TABLE      LIKE W3MIME OCCURS 0,
        LV_DOC_SIZE       TYPE I,
        LV_DOC_TYPE(80)   VALUE SOI_DOCTYPE_EXCEL_SHEET,
        LV_DOC_FORMAT(80) TYPE C.

  DATA: LT_FIELDS_TABLE  LIKE RFC_FIELDS OCCURS 0 WITH HEADER LINE,
        LT_FIELDS_TABLE1 LIKE RFC_FIELDS OCCURS 0 WITH HEADER LINE,
        LS_FIELDS_TABLE  TYPE RFC_FIELDS,
        LV_TABNAME       LIKE X030L-TABNAME.

  DATA: LV_HANDLE         TYPE CNTL_HANDLE.

*  CLEAR : GV_ANSWER.
*  PERFORM POPUP_TO_CONFIRM USING TEXT-002 TEXT-M01
*                           CHANGING GV_ANSWER.
*
*  CHECK GV_ANSWER = '1'.

  CALL METHOD C_OI_CONTAINER_CONTROL_CREATOR=>GET_CONTAINER_CONTROL
    IMPORTING
      CONTROL = LO_CONTROL
      RETCODE = LV_RETCODE.

  IF LV_RETCODE NE C_OI_ERRORS=>RET_OK.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-E01.
    EXIT.
  ENDIF.

  CALL METHOD LO_CONTROL->INIT_CONTROL
    EXPORTING
      R3_APPLICATION_NAME     = TEXT-003
      INPLACE_ENABLED         = ''
      REGISTER_ON_CLOSE_EVENT = 'X'
      PARENT                  = LO_CONTAINER1
    IMPORTING
      RETCODE                 = LV_RETCODE.

  IF LV_RETCODE NE C_OI_ERRORS=>RET_OK.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-E01.
    EXIT.
  ENDIF.

  CALL METHOD LO_CONTROL->GET_DOCUMENT_PROXY
    EXPORTING
      DOCUMENT_TYPE   = 'Excel.Sheet'
      DOCUMENT_FORMAT = 'OLE'
    IMPORTING
      DOCUMENT_PROXY  = LO_DOCUMENT
      RETCODE         = LV_RETCODE.

  IF LV_RETCODE NE C_OI_ERRORS=>RET_OK.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-E01.
    EXIT.
  ENDIF.

* WEB 저장소의 화일 LOAD
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING
      OBJECT_ID        = 'ZSDR0140'
    IMPORTING
      DATA_SIZE        = LV_DOC_SIZE
      DOCUMENT_FORMAT  = LV_DOC_FORMAT
      DOCUMENT_TYPE    = LV_DOC_TYPE
    TABLES
      DATA_TABLE       = LT_DOC_TABLE
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      INTERNAL_ERROR   = 2
      OTHERS           = 3.

* GET DOCUMENT
  CHECK LV_DOC_SIZE > 0.
  CALL METHOD LO_CONTROL->GET_DOCUMENT_PROXY
    EXPORTING
      DOCUMENT_TYPE  = LV_DOC_TYPE
      NO_FLUSH       = 'X'
    IMPORTING
      DOCUMENT_PROXY = LO_DOCUMENT
      RETCODE        = LV_RETCODE.

  IF LV_RETCODE <> C_OI_ERRORS=>RET_OK.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-E01.
    EXIT.
  ENDIF.

  CALL METHOD LO_DOCUMENT->OPEN_DOCUMENT_FROM_TABLE
    EXPORTING
      DOCUMENT_TABLE = LT_DOC_TABLE[]
      DOCUMENT_SIZE  = LV_DOC_SIZE
    IMPORTING
      RETCODE        = LV_RETCODE.

  IF  LV_RETCODE <> C_OI_ERRORS=>RET_OK.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-E01.
    EXIT.
  ENDIF.

* GET HANDLE
  CALL METHOD LO_DOCUMENT->GET_DOCUMENT_HANDLE
    EXPORTING
      NO_FLUSH = 'X'
    IMPORTING
      HANDLE   = LV_HANDLE.

* SET EXCEL FULL 화면
  PERFORM SET_OLE2_PROCESS USING LV_HANDLE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM  USING    P_TITLE
                                P_TEXT
                       CHANGING P_ANSWER.

  CLEAR : P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = P_TITLE
      TEXT_QUESTION  = P_TEXT
      DEFAULT_BUTTON = '1'
    IMPORTING
      ANSWER         = P_ANSWER.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_OLE2_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_OLE2_PROCESS USING P_HANDLE TYPE CNTL_HANDLE.

  DATA: LV_WINDOW      TYPE OLE2_OBJECT,
        LV_APPLICATION TYPE OLE2_OBJECT.

* Excel Application 속성 얻기
  GET PROPERTY OF P_HANDLE-OBJ   'APPLICATION'  = LV_APPLICATION .
  SET PROPERTY OF LV_APPLICATION 'WINDOWSTATE'  = -4137. "최대화

* Excel Application에서 ActiveWindow 속성 얻기
  GET PROPERTY OF LV_APPLICATION 'ACTIVEWINDOW' = LV_WINDOW .
  SET PROPERTY OF LV_WINDOW      'WINDOWSTATE'  = -4137. "최대화

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAIN_PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM MAIN_PROCESS .

  PERFORM READ_EXCEL_DATA.
  PERFORM GET_DISP_DATA.
  PERFORM GET_EXTRA_DATA.
  PERFORM SET_DISP_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_EXCEL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM READ_EXCEL_DATA .

  DATA: LT_INTERN LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA: LV_COL    TYPE KCD_EX_COL_N.
  DATA: L_TYPE    TYPE C.
  FIELD-SYMBOLS <FS> TYPE ANY.

  PERFORM PROGRAM_USAGE IN PROGRAM ZSD1R0010.

  CLEAR: GT_EXCEL, GT_EXCEL[].
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 256
      I_END_ROW               = 20000
    TABLES
      INTERN                  = LT_INTERN
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC NE 0.
    SY-MSGTY = 'S'.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    LEAVE LIST-PROCESSING.
  ELSE.

    LOOP AT LT_INTERN.
      AT NEW ROW.
        CLEAR GT_EXCEL.
      ENDAT.

      LV_COL = LT_INTERN-COL.

      ASSIGN COMPONENT LV_COL OF STRUCTURE GT_EXCEL TO <FS>.
      DESCRIBE FIELD <FS> TYPE L_TYPE.
      IF L_TYPE = 'D'.
        REPLACE ALL OCCURRENCES OF '-' IN LT_INTERN-VALUE WITH ' '.
        REPLACE ALL OCCURRENCES OF '/' IN LT_INTERN-VALUE WITH ' '.
        REPLACE ALL OCCURRENCES OF '.' IN LT_INTERN-VALUE WITH ' '.
        CONDENSE LT_INTERN-VALUE NO-GAPS.
      ENDIF.
      <FS> = LT_INTERN-VALUE.

      AT END OF ROW.
        APPEND GT_EXCEL.
      ENDAT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DISP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_DISP_DATA .

  CLEAR : GT_DISP, GT_DISP[].

  CHECK GT_EXCEL[] IS NOT INITIAL.

  LOOP AT GT_EXCEL.
    MOVE-CORRESPONDING GT_EXCEL TO GT_DISP.

*    TRANSLATE GT_DISP-WAERK TO UPPER CASE.

    " Data Conversion
    IF GT_DISP-KUNNR IS NOT INITIAL.
      PERFORM CONVERSION_EXIT_ALPHA_INPUT USING GT_EXCEL-KUNNR
                                          CHANGING GT_DISP-KUNNR.
    ENDIF.
    IF GT_DISP-KUNNR1 IS NOT INITIAL.
      PERFORM CONVERSION_EXIT_ALPHA_INPUT USING GT_EXCEL-KUNNR1
                                          CHANGING GT_DISP-KUNNR1.
    ENDIF.
    IF GT_DISP-KUNNRO IS NOT INITIAL.
      PERFORM CONVERSION_EXIT_ALPHA_INPUT USING GT_EXCEL-KUNNRO
                                          CHANGING GT_DISP-KUNNRO.
    ENDIF.

    IF GT_DISP-MATNR IS NOT INITIAL.
      PERFORM CONVERSION_EXIT_MATN1_INPUT USING GT_EXCEL-MATNR
                                          CHANGING GT_DISP-MATNR.
    ENDIF.
*
*    IF GT_DISP-KBETR <> 0.
*      PERFORM CURRENCY_AMOUNT_IDOC_TO_SAP USING GT_DISP-WAERK
*                                                GT_DISP-KBETR
*                                          CHANGING GT_DISP-KBETR.
*    ENDIF.

    APPEND GT_DISP.
    CLEAR GT_DISP.
  ENDLOOP.

*  SORT GT_DISP BY ZUONR KUNNR DESCENDING.
  SORT GT_DISP BY BSTKD.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CONVERSION_EXIT_ALPHA_INPUT  USING P_INPUT
                                  CHANGING P_OUTPUT.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_INPUT
    IMPORTING
      OUTPUT = P_OUTPUT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERSION_EXIT_MATN1_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CONVERSION_EXIT_MATN1_INPUT  USING P_INPUT
                                  CHANGING P_OUTPUT.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT        = P_INPUT
    IMPORTING
      OUTPUT       = P_OUTPUT
    EXCEPTIONS
      LENGTH_ERROR = 1
      OTHERS       = 2.

  IF SY-SUBRC <> 0.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CURRENCY_AMOUNT_IDOC_TO_SAP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CURRENCY_AMOUNT_IDOC_TO_SAP  USING P_WAERK
                                        P_INPUT
                                  CHANGING P_OUTPUT.

  CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
    EXPORTING
      CURRENCY    = P_WAERK
      IDOC_AMOUNT = P_INPUT
    IMPORTING
      SAP_AMOUNT  = P_OUTPUT.

  CONDENSE P_OUTPUT NO-GAPS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EXTRA_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_EXTRA_DATA .


  DATA : LT_TEMP TYPE TABLE OF TY_DISP WITH HEADER LINE.

  CLEAR : GT_VBRP, GT_VBRP[], GT_VBAK, GT_VBAK[],
          GT_VBFA, GT_VBFA[], GT_VBPA, GT_VBPA[],
          GT_VBRK, GT_VBRK[].

* GET domain description
  CLEAR: GT_TVLVT, GT_TVLVT[].
  SELECT * INTO TABLE GT_TVLVT FROM TVLVT WHERE SPRAS = SY-LANGU.
  SORT GT_TVLVT BY ABRVW.



*>> BAPI Header & Item Data
*  CLEAR : LT_TEMP, LT_TEMP[].
*  LT_TEMP[] = GT_DISP[].
*  SORT LT_TEMP BY ZUONR.
*  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING ZUONR.
*
*  IF LT_TEMP[] IS NOT INITIAL.
*    SELECT A~VBELN A~ZUONR A~VKORG A~VTWEG
*      A~KUNAG A~KNUMV B~POSNR B~MATNR B~WERKS
*      INTO CORRESPONDING FIELDS OF TABLE GT_VBRP
*      FROM VBRK AS A INNER JOIN VBRP AS B
*      ON A~VBELN = B~VBELN
*      FOR ALL ENTRIES IN LT_TEMP
*      WHERE A~FKART = 'F2'
*        AND A~ZUONR = LT_TEMP-ZUONR
*        AND B~MATNR = LT_TEMP-MATNR.
*  ENDIF.

*>> S/O
*  CLEAR : R_BSTNK, R_BSTNK[].
*  LOOP AT LT_TEMP.
*    R_BSTNK-SIGN = 'I'.
*    R_BSTNK-OPTION = 'EQ'.
*    R_BSTNK-LOW = LT_TEMP-ZUONR.
*    APPEND R_BSTNK.
*    CLEAR R_BSTNK.
*  ENDLOOP.
*
*  SELECT VBELN BSTNK
*    INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
*    FROM VBAK
*    WHERE VBTYP = 'K'
*      AND BSTNK IN R_BSTNK.


*>> BAPI Partner Data
*  IF GT_VBRP[] IS NOT INITIAL.
*    SELECT RUUID VBELN VBELV
*      INTO CORRESPONDING FIELDS OF TABLE GT_VBFA
*      FROM VBFA
*      FOR ALL ENTRIES IN GT_VBRP
*      WHERE VBELN = GT_VBRP-VBELN
*        AND VBTYP_V = 'C'.
*
*    IF GT_VBFA[] IS NOT INITIAL.
*      SELECT VBELN POSNR PARVW KUNNR
*        INTO CORRESPONDING FIELDS OF TABLE GT_VBPA
*        FROM VBPA
*        FOR ALL ENTRIES IN GT_VBFA
*        WHERE VBELN = GT_VBFA-VBELV
*          AND PARVW IN ('AG', 'RE', 'RG', 'WE').
*    ENDIF.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DISP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_DISP_DATA .

* Get first row(default value) and delete
  READ TABLE GT_DISP INDEX 1.
  CLEAR:GS_DISP.  GS_DISP = GT_DISP.
  DELETE GT_DISP INDEX 1.

* main check logic
  LOOP AT GT_DISP.
    PERFORM SET_DEFAULT.
    MODIFY GT_DISP.
  ENDLOOP.

* GET CUSTOMER SKU
  CLEAR : GT_SKU_C, GT_SKU_C[].
  PERFORM GET_CUSTOMER_SKU.

* GET CUSTOMER SKU
  CLEAR : GT_USAGE, GT_USAGE[].
  PERFORM GET_USAGE.


* Check duplication / header difference
  PERFORM DUP_CHECK.

* main check logic
  LOOP AT GT_DISP.
    CHECK GT_DISP-ICON1 IS INITIAL.
*    GT_DISP-ICON1 = ICON_LED_GREEN.

*-- Check required field
    PERFORM REQUIRED_FIELD_CHECK USING : GT_DISP-AUART   TEXT-F01, "Quotation Type
                                         GT_DISP-VKORG   TEXT-F02, "Sales Org
                                         GT_DISP-VTWEG   TEXT-F03, "Distribution Channel
                                         GT_DISP-SPART   TEXT-F04, "Division
                                         GT_DISP-KUNNR   TEXT-F05, "Sold-to-Party
                                         GT_DISP-KUNNR1  TEXT-F06, "Ship-to-Party
                                         GT_DISP-KUNNRO  TEXT-F07, "Origin SoldTo
                                         GT_DISP-BSTKD   TEXT-F08, "Origin PO#(VBKD-BSTKD)
*S_2011/2/11 remove : move RDD By E00064
*                                         GT_DISP-BNDDT   TEXT-F09, "ValidTo
*E_2011/2/11 remove : move RDD By E00064
                                         GT_DISP-VDATU   TEXT-F10, "Requested Delivery Date
                                         GT_DISP-WERKS   TEXT-F11, "Plant
                                         GT_DISP-LGORT   TEXT-F12, "Storage Location
*                                         GT_DISP-VKAUS   TEXT-F13, "Usage
                                         GT_DISP-MATNR   TEXT-F14, "Material(SKU)
                                         GT_DISP-KWMENG  TEXT-F15, "Order QTY
                                         GT_DISP-MEINS   TEXT-F16. "UOM

*-- Check Order QTY
    IF GT_DISP-ICON1 IS INITIAL.
*    IF GT_DISP-ICON1 = ICON_LED_GREEN.
      PERFORM QTY_FIELD_CHECK USING : GT_DISP-KWMENG   TEXT-F15. "Order QTY
    ENDIF.

*-- Check Sales Org/Plant/Storage Location
    IF GT_DISP-ICON1 IS INITIAL.
      PERFORM VKORG_WERKS_LGORT_CHECK.
    ENDIF.

*-- get customer SKU
    READ TABLE GT_SKU_C WITH KEY MATNR = GT_DISP-MATNR.
    IF SY-SUBRC = 0. GT_DISP-KDMAT = GT_SKU_C-KDMAT. ENDIF.

*-- get usage
    READ TABLE GT_USAGE WITH KEY VKORG = GT_DISP-VKORG
                                 KVGR1 = GT_DISP-KVGR1.
    IF SY-SUBRC = 0. GT_DISP-VKAUS = GT_USAGE-VKAUS. ENDIF.

*-- get Usage Description
    READ TABLE GT_TVLVT WITH KEY ABRVW = GT_DISP-VKAUS.
    IF SY-SUBRC = 0. GT_DISP-BEZEI = GT_TVLVT-BEZEI.
    ELSE.
      IF GT_DISP-ICON1 IS INITIAL.
        GT_DISP-MESSAGE = TEXT-E09. "Usage Indicator not found
        GT_DISP-ICON1 = ICON_LED_RED.
      ELSE.  ENDIF.
    ENDIF.

    MODIFY GT_DISP.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM DISPLAY .

  DESCRIBE TABLE GT_DISP LINES SY-TFILL.
  IF SY-TFILL = 0.
    MESSAGE S001 DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE S014 WITH SY-TFILL.
    CALL SCREEN 0100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_OBJECTS_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_OBJECTS_0100  USING P_DOCKING TYPE REF TO CL_GUI_DOCKING_CONTAINER
                             P_GRID    TYPE REF TO LCL_ALV_GRID.

* CREATE DOCKING
  CREATE OBJECT P_DOCKING
    EXPORTING
      REPID     = SY-REPID
      DYNNR     = SY-DYNNR
      EXTENSION = 2000.

  CREATE OBJECT P_GRID
    EXPORTING
      I_PARENT = P_DOCKING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY .

  CLEAR : GT_FCAT1.

  PERFORM SET_FILL_FIELD_CATEGORY USING :
                'S'    'FIELDNAME'    'ICON1',
                ' '    'REF_TABLE'    'ICON',
                ' '    'REF_FIELD'    'ID',
                ' '    'KEY'          'X',
                ' '    'JUST'         'C',
                'E'    'COLTEXT'      TEXT-A01,   " ICON

                'S'    'FIELDNAME'    'MESSAGE',
                ' '    'KEY'          'X',
                'E'    'COLTEXT'      TEXT-A20,  " Message


*                'S'    'FIELDNAME'    'ICON2',
*                ' '    'REF_TABLE'    'ICON',
*                ' '    'REF_FIELD'    'ID',
*                ' '    'KEY'          'X',
*                ' '    'JUST'         'C',
*                'E'    'COLTEXT'      TEXT-A02,

                'S'    'FIELDNAME'    'AUART',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'AUART',
                ' '    'KEY'          'X',
                'E'    'COLTEXT'      TEXT-A02,  " Quotation Type

                'S'    'FIELDNAME'    'VKORG',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'VKORG',
                ' '    'KEY'          'X',
                'E'    'COLTEXT'      TEXT-A03,  " Sales Org

                'S'    'FIELDNAME'    'VTWEG',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'VTWEG',
                ' '    'KEY'          'X',
                'E'    'COLTEXT'      TEXT-A04,  " Distribution Channel

                'S'    'FIELDNAME'    'SPART',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'SPART',
                'E'    'COLTEXT'      TEXT-A05,  " Division

                'S'    'FIELDNAME'    'KUNNR',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'KUNNR',
                ' '    'QFIELDNAME'   'MEINS',
                'E'    'COLTEXT'      TEXT-A06,  " SoldTO

                'S'    'FIELDNAME'    'KUNNR1',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'KUNNR',
                'E'    'COLTEXT'      TEXT-A07,  " ShipTo

                'S'    'FIELDNAME'    'KUNNRO',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'KUNNR',
*                ' '    'CFIELDNAME'   'WAERK',
                'E'    'COLTEXT'      TEXT-A08,  " Origin SoldTo

                'S'    'FIELDNAME'    'BSTKD',
                ' '    'REF_TABLE'    'VBKD',
                ' '    'REF_FIELD'    'BSTKD',
                'E'    'COLTEXT'      TEXT-A09,  " Origin PO#

*S_2011/2/11 remove : move RDD By E00064
*                'S'    'FIELDNAME'    'BNDDT',
**                ' '    'REF_TABLE'    'VBRK',
*                ' '    'REF_TABLE'    'VBAK',
*                ' '    'REF_FIELD'    'BNDDT',
*                'E'    'COLTEXT'      TEXT-A10,  " ValidTo
*E_2011/2/11 remove : move RDD By E00064

                'S'    'FIELDNAME'    'VDATU',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'VDATU',
                'E'    'COLTEXT'      TEXT-A11,  " RDD : Requested Delivery Date

                'S'    'FIELDNAME'    'WERKS',
                ' '    'REF_TABLE'    'VBAP',
                ' '    'REF_FIELD'    'WERKS',
                'E'    'COLTEXT'      TEXT-A12,  " plant

                'S'    'FIELDNAME'    'LGORT',
                ' '    'REF_TABLE'    'VBAP',
                ' '    'REF_FIELD'    'LGORT',
                'E'    'COLTEXT'      TEXT-A13,  " stor.loc

                'S'    'FIELDNAME'    'VKAUS',
                ' '    'REF_TABLE'    'VBAP',
                ' '    'REF_FIELD'    'VKAUS',
                'E'    'COLTEXT'      TEXT-A14,  " usage

                'S'    'FIELDNAME'    'BEZEI',
                ' '    'REF_TABLE'    'TVLVT',
                ' '    'REF_FIELD'    'BEZEI',
                'E'    'COLTEXT'      TEXT-A15,  " usage Description

                'S'    'FIELDNAME'    'MATNR',
                ' '    'REF_TABLE'    'VBAP',
                ' '    'REF_FIELD'    'MATNR',
                'E'    'COLTEXT'      TEXT-A16,  " SKU

*                'S'    'FIELDNAME'    'MATNR',
*                ' '    'REF_TABLE'    'VBAP',
*                ' '    'REF_FIELD'    'MATNR',
*                'E'    'COLTEXT'      TEXT-A17,  " QTY

                'S'    'FIELDNAME'    'KWMENG',
                ' '    'REF_TABLE'    'VBAP',
                ' '    'REF_FIELD'    'KWMENG',
                ' '    'QFIELDNAME'   'MEINS',
                'E'    'COLTEXT'      TEXT-A17,  " QTY

                'S'    'FIELDNAME'    'MEINS',
                ' '    'REF_TABLE'    'VBAP',
                ' '    'REF_FIELD'    'MEINS',
                'E'    'COLTEXT'      TEXT-A18,  " UOM

*                'S'    'FIELDNAME'    'VBELN_B',
*                ' '    'REF_TABLE'    'VBRK',
*                ' '    'REF_FIELD'    'VBELN',
*                ' '    'STYLE'        '00000200',
*                'E'    'COLTEXT'      TEXT-A14,

*                'S'    'FIELDNAME'    'MESSAGE',
*                'E'    'COLTEXT'      TEXT-A20,  " Message

                'S'    'FIELDNAME'    'VBELN_S',
                ' '    'REF_TABLE'    'VBAK',
                ' '    'REF_FIELD'    'VBELN',
                ' '    'STYLE'        '00000200',
                'E'    'COLTEXT'      TEXT-A19,  " Quotation No.

                'S'    'FIELDNAME'    'POSNR',
                ' '    'REF_TABLE'    'VBAP',
                ' '    'REF_FIELD'    'POSNR',
                ' '    'STYLE'        '00000200',
                'E'    'COLTEXT'      TEXT-A22,  " Quotation Item No.

                'S'    'FIELDNAME'    'KDMAT',
                ' '    'REF_TABLE'    'VBAP',
                ' '    'REF_FIELD'    'KDMAT',
*                ' '    'STYLE'        '00000200',
                'E'    'COLTEXT'      TEXT-A21.  " customer SKU.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_FILL_FIELD_CATEGORY  USING P_GUB TYPE ANY
                                                                 P_FNAME  TYPE  ANY
                                                                 P_CON  TYPE  ANY.

  DATA  L_COL(40).
  FIELD-SYMBOLS  <FS>  TYPE  ANY.

  CONCATENATE 'GS_FCAT1-' P_FNAME INTO  L_COL.
  ASSIGN      (L_COL)                 TO   <FS>.
  MOVE         P_CON                  TO   <FS>.

  IF P_GUB = C_E.
    APPEND GS_FCAT1 TO GT_FCAT1.
    CLEAR GS_FCAT1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EXCLUDE_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_EXCLUDE_TOOLBAR .

  REFRESH GT_TOOLBAR1. CLEAR GT_TOOLBAR1.

  PERFORM APPEND_FCODE USING :
*-- 불필요 icon 제거.
                  CL_GUI_ALV_GRID=>MC_FC_DETAIL,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
*                  cl_gui_alv_grid=>mc_fc_filter,
                  CL_GUI_ALV_GRID=>MC_FC_GRAPH,
                  CL_GUI_ALV_GRID=>MC_FC_HELP,
                  CL_GUI_ALV_GRID=>MC_FC_INFO,
                  CL_GUI_ALV_GRID=>MC_FC_AUF,
*                  cl_gui_alv_grid=>mc_fc_average,
                  CL_GUI_ALV_GRID=>MC_FC_FIND,
*                  cl_gui_alv_grid=>mc_fc_subtot,
*                  cl_gui_alv_grid=>mc_fc_sum,
*                  cl_gui_alv_grid=>mc_fc_print,
*                  cl_gui_alv_grid=>mc_fc_print_prev,
*                  cl_gui_alv_grid=>mc_fc_expcrdata,
*                  cl_gui_alv_grid=>mc_fc_views,
*                  cl_gui_alv_grid=>mc_fc_load_variant,
*                  cl_gui_alv_grid=>mc_fc_maintain_variant,
*                  cl_gui_alv_grid=>mc_fc_save_variant,
*                  cl_gui_alv_grid=>mc_fc_filter,
*                  cl_gui_alv_grid=>mc_fc_graph,
*                  cl_gui_alv_grid=>mc_fc_help,
*                  cl_gui_alv_grid=>mc_fc_info,
*                  cl_gui_alv_grid=>mc_fc_load_variant,
*                  cl_gui_alv_grid=>mc_fc_subtot,
*                  cl_gui_alv_grid=>mc_fc_sum,
*                  cl_gui_alv_grid=>mc_fc_refresh,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FCODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM APPEND_FCODE  USING P_UCOMM.

  APPEND P_UCOMM TO GT_TOOLBAR1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT  USING PS_LAYO TYPE LVC_S_LAYO.

  PS_LAYO-ZEBRA      = 'X'.
  PS_LAYO-NO_ROWINS  = 'X'.
  PS_LAYO-NO_ROWMOVE = 'X'.
  PS_LAYO-CWIDTH_OPT = 'X'.
*  PS_LAYO-SEL_MODE   = 'A'.
  PS_LAYO-CTAB_FNAME   = 'CTAB'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_VARIANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_VARIANT .

  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-USERNAME = SY-UNAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CREATE_EVENT_RECEIVER  USING P_GRID TYPE REF TO LCL_ALV_GRID.

  P_GRID->REGISTER_EDIT_EVENT( P_GRID->MC_EVT_ENTER ).
  P_GRID->REGISTER_EDIT_EVENT( P_GRID->MC_EVT_MODIFIED ).

  CREATE OBJECT GCL_RECEIVER.
*  SET HANDLER GCL_RECEIVER->HANDLE_TOP_OF_PAGE FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_TOOLBAR        FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_USER_COMMAND   FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_DATA_CHANGED   FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR P_GRID.
  SET HANDLER GCL_RECEIVER->HANDLE_DOUBLE_CLICK   FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_HOTSPOT_CLICK  FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_ONF4           FOR P_GRID.

  " 엔터만 입력해도 'handle_data_changed'을 수행한다.
  CALL METHOD P_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

*  " 셀을 이동해도 이벤트를 수행하도록.
  CALL METHOD P_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_DISPLAY  USING P_GRID TYPE REF TO LCL_ALV_GRID
                       PS_LAYO    TYPE LVC_S_LAYO
                       PT_TOOLBAR TYPE UI_FUNCTIONS
                       P_TABNAME
                       PT_FCAT    TYPE LVC_T_FCAT
                       PT_SORT    TYPE LVC_T_SORT.
*
  FIELD-SYMBOLS <TABLE> TYPE STANDARD TABLE.
  DATA L_TABNAME TYPE SLIS_TABNAME.
  CLEAR L_TABNAME.
  L_TABNAME = P_TABNAME.
  ASSIGN (L_TABNAME) TO <TABLE>.

  CALL METHOD P_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_VARIANT           = GS_VARIANT
      I_SAVE               = C_A
      IS_LAYOUT            = PS_LAYO
      IT_TOOLBAR_EXCLUDING = PT_TOOLBAR[]
    CHANGING
      IT_OUTTAB            = <TABLE>
      IT_FIELDCATALOG      = PT_FCAT[]
      IT_SORT              = PT_SORT[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM REFRESH_TABLE_DISPLAY  USING P_GRID TYPE REF TO LCL_ALV_GRID.

* Refresh
  DATA L_REFRESH TYPE LVC_S_STBL.

  L_REFRESH-ROW = 'X'.
  L_REFRESH-COL = 'X'.

  CHECK P_GRID IS NOT INITIAL.

  CALL METHOD P_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = L_REFRESH
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.

  P_GRID->U_OPTIMIZE_ALL_COLS( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    SENDER
                                   E_ROW     TYPE	LVC_S_ROW
                                   E_COLUMN  TYPE	LVC_S_COL
                                   ES_ROW_NO TYPE	LVC_S_ROID.

  CASE SENDER.
    WHEN GCL_ALV1.
      READ TABLE GT_DISP INDEX E_ROW-INDEX.
      CASE E_COLUMN-FIELDNAME.
        WHEN 'VBELN_S'.
          CHECK GT_DISP-VBELN_S IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD GT_DISP-VBELN_S.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*        WHEN 'VBELN_B'.
*          CHECK GT_DISP-VBELN_B IS NOT INITIAL.
*          SET PARAMETER ID 'VF' FIELD GT_DISP-VBELN_B.
*          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDCASE.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CREATE_PROCESS .

*>> 라인 선택 ==> do not check
*  PERFORM GET_SELECTED_ROW.
  PERFORM CHECK_ERROR_ROW.
  CHECK GV_ERR IS INITIAL.

*>> POP-UP
  PERFORM POPUP_TO_CONFIRM USING TEXT-M01 TEXT-M02
                                 CHANGING GV_ANSWER.
  CHECK GV_ANSWER = '1'.

*>> S/O
*  PERFORM CREATE_SALES_ORDER.
  PERFORM CREATE_QUOTATION.

*>> B/L
*  PERFORM CREATE_BILLING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SELECTED_ROW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_SELECTED_ROW .

  DATA : LT_ROWS TYPE LVC_T_ROW,
         LS_ROWS TYPE LVC_S_ROW,
         L_LINE  TYPE I.

  CLEAR : GV_ERR, LT_ROWS, LS_ROWS, L_LINE.
*  CALL METHOD GCL_ALV1->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = LT_ROWS.
*
*  DESCRIBE TABLE LT_ROWS LINES L_LINE.
*
*  IF L_LINE = 0.
*    GV_ERR = 'X'.
*    MESSAGE S003 DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  GT_DISP-CHK = SPACE.
*  MODIFY GT_DISP TRANSPORTING CHK WHERE CHK IS NOT INITIAL.

*  LOOP AT LT_ROWS INTO LS_ROWS.
*    READ TABLE GT_DISP INDEX LS_ROWS-INDEX.
*    IF SY-SUBRC = 0.
*      IF GT_DISP-ICON1 <> ICON_LED_GREEN.
**        OR GT_DISP-ICON2 <> ICON_LED_GREEN.
*        GT_DISP-CHK = 'X'.
*        MODIFY GT_DISP INDEX LS_ROWS-INDEX.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_ERROR_ROW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CHECK_ERROR_ROW .

*  DATA : LT_ROWS TYPE LVC_T_ROW,
*         LS_ROWS TYPE LVC_S_ROW,
*         L_LINE  TYPE I.

  CLEAR : GV_ERR.

  LOOP AT GT_DISP.
*    IF GT_DISP-ICON1 <> ICON_LED_GREEN.   " ICON_LED_RED
    IF GT_DISP-ICON1 = ICON_LED_RED.   " ICON_LED_RED
      MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-E11. "Please check excel data.
      GV_ERR = 'X'. EXIT.
    ENDIF.
  ENDLOOP.
*  CALL METHOD GCL_ALV1->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = LT_ROWS.
*
*  DESCRIBE TABLE LT_ROWS LINES L_LINE.
*
*  IF L_LINE = 0.
*    GV_ERR = 'X'.
*    MESSAGE S003 DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  GT_DISP-CHK = SPACE.
*  MODIFY GT_DISP TRANSPORTING CHK WHERE CHK IS NOT INITIAL.

*  LOOP AT LT_ROWS INTO LS_ROWS.
*    READ TABLE GT_DISP INDEX LS_ROWS-INDEX.
*    IF SY-SUBRC = 0.
*      IF GT_DISP-ICON1 <> ICON_LED_GREEN.
**        OR GT_DISP-ICON2 <> ICON_LED_GREEN.
*        GT_DISP-CHK = 'X'.
*        MODIFY GT_DISP INDEX LS_ROWS-INDEX.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CREATE_QUOTATION.
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*FORM CREATE_SALES_ORDER .
FORM CREATE_QUOTATION. .

  DATA : LV_VBELN TYPE BAPIVBELN-VBELN. "BAPIVBELN-VBELN.
  DATA : LS_HEAD  TYPE BAPISDHD1,   "BAPISDHD1,
         LS_HEADX TYPE BAPISDHD1X.  "BAPISDHD1X.
*  DATA : LT_ITEM   TYPE TABLE OF BAPISDITM WITH HEADER LINE,
*         LT_ITEMX  TYPE TABLE OF BAPISDITMX WITH HEADER LINE,
*         LT_PART   TYPE TABLE OF BAPIPARNR WITH HEADER LINE,
*         LT_SCHDL  TYPE TABLE OF BAPISCHDL WITH HEADER LINE,
*         LT_SCHDLX TYPE TABLE OF BAPISCHDLX WITH HEADER LINE,
*         LT_COND   TYPE TABLE OF BAPICOND WITH HEADER LINE,
*         LT_CONDX  TYPE TABLE OF BAPICONDX WITH HEADER LINE,
*         LT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.
  DATA : LT_ITEM   TYPE TABLE OF BAPISDITM WITH HEADER LINE,
         LT_ITEMX  TYPE TABLE OF BAPISDITMX WITH HEADER LINE,
         LT_PART   TYPE TABLE OF BAPIPARNR WITH HEADER LINE,
         LT_SCHDL  TYPE TABLE OF BAPISCHDL WITH HEADER LINE,
         LT_SCHDLX TYPE TABLE OF BAPISCHDLX WITH HEADER LINE,
         LT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

  DATA : LV_POSNR TYPE VBAP-POSNR.

  CLEAR : GT_HEAD, GT_HEAD[].

  GT_HEAD[] = GT_DISP[].


*  DELETE GT_HEAD WHERE CHK IS INITIAL
*                    OR VBELN_S IS NOT INITIAL.
  SORT GT_HEAD BY BSTKD.
  DELETE ADJACENT DUPLICATES FROM GT_HEAD COMPARING BSTKD.

  LOOP AT GT_HEAD.
    CLEAR : LS_HEAD, LS_HEADX, LT_ITEM, LT_ITEM[],
            LT_ITEMX, LT_ITEMX[], LT_PART, LT_PART[],
            LT_SCHDL, LT_SCHDL[], LT_SCHDLX, LT_SCHDLX[],
*            LT_COND, LT_COND[], LT_CONDX[], LT_CONDX[],
            LT_RETURN, LT_RETURN[], LV_VBELN, GV_ERR, GV_MSG.

    " Header Data / Partner
    PERFORM SET_HEADER_DATA TABLES LT_PART
                            CHANGING LS_HEAD
                                     LS_HEADX.

    " Item Data
    PERFORM SET_ITEM_DATA TABLES LT_ITEM
                                 LT_ITEMX
                                 LT_SCHDL
                                 LT_SCHDLX.
*                                 LT_COND
*                                 LT_CONDX.


    CALL FUNCTION 'BAPI_QUOTATION_CREATEFROMDATA2'
      EXPORTING
*       SALESDOCUMENTIN                =
        QUOTATION_HEADER_IN            = LS_HEAD
        QUOTATION_HEADER_INX           = LS_HEADX
*       SENDER                         =
*       BINARY_RELATIONSHIPTYPE        = ' '
*       INT_NUMBER_ASSIGNMENT          = ' '
*       BEHAVE_WHEN_ERROR              = ' '
*       LOGIC_SWITCH                   =
*       TESTRUN                        =
*       CONVERT                        = ' '
     IMPORTING
       SALESDOCUMENT                   = LV_VBELN
      TABLES
        RETURN                         = LT_RETURN
        QUOTATION_ITEMS_IN             = LT_ITEM
        QUOTATION_ITEMS_INX            = LT_ITEMX
        QUOTATION_PARTNERS             = LT_PART
        QUOTATION_SCHEDULES_IN         = LT_SCHDL
        QUOTATION_SCHEDULES_INX        = LT_SCHDLX
*       QUOTATION_CONDITIONS_IN        =
*       QUOTATION_CONDITIONS_INX       =
*       QUOTATION_CFGS_REF             =
*       QUOTATION_CFGS_INST            =
*       QUOTATION_CFGS_PART_OF         =
*       QUOTATION_CFGS_VALUE           =
*       QUOTATION_CFGS_BLOB            =
*       QUOTATION_CFGS_VK              =
*       QUOTATION_CFGS_REFINST         =
*       QUOTATION_KEYS                 =
*       QUOTATION_TEXT                 =
*       EXTENSIONIN                    =
*       PARTNERADDRESSES               =
*       EXTENSIONEX                    =
*       NFMETALLITMS                   =
              .


*    CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
*      EXPORTING
*        SALES_HEADER_IN      = LS_HEAD
*        SALES_HEADER_INX     = LS_HEADX
*      IMPORTING
*        SALESDOCUMENT_EX     = LV_VBELN
*      TABLES
*        RETURN               = LT_RETURN
*        SALES_ITEMS_IN       = LT_ITEM
*        SALES_ITEMS_INX      = LT_ITEMX
*        SALES_PARTNERS       = LT_PART
*        SALES_SCHEDULES_IN   = LT_SCHDL
*        SALES_SCHEDULES_INX  = LT_SCHDLX
*        SALES_CONDITIONS_IN  = LT_COND
*        SALES_CONDITIONS_INX = LT_CONDX.

    READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      GV_ERR = C_X.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = LT_RETURN-ID
          MSGNR               = LT_RETURN-NUMBER
          MSGV1               = LT_RETURN-MESSAGE_V1
          MSGV2               = LT_RETURN-MESSAGE_V2
          MSGV3               = LT_RETURN-MESSAGE_V3
          MSGV4               = LT_RETURN-MESSAGE_V4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = GV_MSG.
    ELSE.
      IF LV_VBELN IS INITIAL.
        GV_ERR = C_X.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        GV_MSG = TEXT-E02.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = C_X.
      ENDIF.
    ENDIF.

    " Result
    CLEAR : GV_TABIX.
    CLEAR : LV_POSNR.
    LOOP AT GT_DISP WHERE BSTKD = GT_HEAD-BSTKD.
      GV_TABIX = SY-TABIX.
      IF GV_ERR IS INITIAL.
        CLEAR : GT_DISP-MESSAGE.
        GT_DISP-MESSAGE = TEXT-M03.
        GT_DISP-ICON1 = ICON_LED_GREEN.
        GT_DISP-VBELN_S = LV_VBELN.
        ADD 10 TO LV_POSNR.
        GT_DISP-POSNR = LV_POSNR.
      ELSE.
        GT_DISP-ICON1 = ICON_LED_RED.
        GT_DISP-MESSAGE = GV_MSG.
      ENDIF.
*      GT_DISP-ICON2 = ICON_LED_YELLOW.
      MODIFY GT_DISP INDEX GV_TABIX.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_HEADER_DATA  TABLES PT_PART STRUCTURE BAPIPARNR
                      CHANGING PS_HEAD STRUCTURE BAPISDHD1
                               PS_HEADX STRUCTURE BAPISDHD1X.

  " Header
*  IF SY-MANDT = 300.
*    PS_HEAD-DOC_TYPE = 'ZCM'.
*  ELSE.
*    PS_HEAD-DOC_TYPE = 'ZCMR'.
*  ENDIF.
  PS_HEAD-DOC_TYPE = GT_HEAD-AUART.     "Quotation Type
  PS_HEADX-DOC_TYPE = C_X.

  PS_HEAD-SALES_ORG = GT_HEAD-VKORG.    "Sales Org
  PS_HEADX-SALES_ORG = C_X.

  PS_HEAD-DISTR_CHAN = GT_HEAD-VTWEG.   "Distribution Channel
  PS_HEADX-DISTR_CHAN = C_X.

  PS_HEAD-DIVISION = GT_HEAD-SPART.     "Division '00'.
  PS_HEADX-DIVISION = C_X.

*  PS_HEAD-REF_DOC = GT_HEAD-ORGBL.
*  PS_HEAD-REFDOC_CAT = 'B'.
*  PS_HEAD-ORD_REASON = GT_HEAD-AUGRU.

  PS_HEAD-PURCH_NO_C = GT_HEAD-BSTKD.   "Customer PO #
  PS_HEADX-PURCH_NO_C = C_X.

*S_2011/2/11 remove : move RDD By E00064
*  PS_HEAD-QT_VALID_T = GT_HEAD-BNDDT.   "Valid to
  PS_HEAD-QT_VALID_T = GT_HEAD-VDATU.   "Valid to
*E_2011/2/11 remove : move RDD By E00064
  PS_HEADX-QT_VALID_T = C_X.

  PS_HEAD-REQ_DATE_H = GT_HEAD-VDATU.   "납품 요청일 RDD.
  PS_HEADX-REQ_DATE_H = C_X.

  "2021/04/14 S
  PS_HEAD-PRICE_DATE = SY-DATUM.
  PS_HEADX-PRICE_DATE = C_X.
  "2021/04/14 E

*  PS_HEADX-REF_DOC = C_X.
*  PS_HEADX-REFDOC_CAT = C_X.
*  PS_HEADX-ORD_REASON = C_X.

  " Partner
* 판매처 (Sold-to party)
  PT_PART-PARTN_ROLE = 'AG'.
  PT_PART-PARTN_NUMB = GT_HEAD-KUNNR.
  APPEND PT_PART. CLEAR PT_PART.
* 납품처 (Ship-to party)
  PT_PART-PARTN_ROLE = 'WE'.
  PT_PART-PARTN_NUMB = GT_HEAD-KUNNR1.
  APPEND PT_PART. CLEAR PT_PART.
* origin 판매처 (origin Sold-to party)
  PT_PART-PARTN_ROLE = 'SX'.
  PT_PART-PARTN_NUMB = GT_HEAD-KUNNRO.
  APPEND PT_PART. CLEAR PT_PART.


*  READ TABLE GT_VBFA WITH KEY VBELN = GT_HEAD-ORGBL.
*  IF SY-SUBRC = 0.
*    LOOP AT GT_VBPA WHERE VBELN = GT_VBFA-VBELV.
*      PT_PART-PARTN_ROLE = GT_VBPA-PARVW.
*      PT_PART-PARTN_NUMB = GT_VBPA-KUNNR.
*      APPEND PT_PART.
*      CLEAR PT_PART.
*    ENDLOOP.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ITEM_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_ITEM_DATA  TABLES   PT_ITEM STRUCTURE BAPISDITM
                             PT_ITEMX STRUCTURE BAPISDITMX
                             PT_SCHDL STRUCTURE BAPISCHDL
                             PT_SCHDLX STRUCTURE BAPISCHDLX.
*                             PT_COND STRUCTURE BAPICOND
*                             PT_CONDX STRUCTURE BAPICONDX.

  DATA : LV_POSNR TYPE VBAP-POSNR.
  DATA : L_BAPI_AMOUNT TYPE BAPICURR-BAPICURR,
         L_SAP_AMOUNT  TYPE BAPICURR-BAPICURR.




  CLEAR : LV_POSNR.
  LOOP AT GT_DISP WHERE BSTKD = GT_HEAD-BSTKD.
    ADD 10 TO LV_POSNR.

    " Item
    PT_ITEM-ITM_NUMBER = LV_POSNR.
    PT_ITEMX-ITM_NUMBER = LV_POSNR.
    PT_ITEM-MATERIAL = GT_DISP-MATNR.       "Material(SKU)
    PT_ITEMX-MATERIAL = C_X.
    PT_ITEM-TARGET_QTY = GT_DISP-KWMENG.    "Qty
    PT_ITEMX-TARGET_QTY = C_X.
    PT_ITEM-PLANT = GT_DISP-WERKS.          "Plant
    PT_ITEMX-PLANT = C_X.
    PT_ITEM-STORE_LOC = GT_DISP-LGORT.      "S. Loc
    PT_ITEMX-STORE_LOC = C_X.
    PT_ITEM-USAGE_IND = GT_DISP-VKAUS.      "Usage
    PT_ITEMX-USAGE_IND = C_X.

    PT_ITEM-SALES_UNIT = GT_DISP-MEINS.     "UON
    PT_ITEMX-SALES_UNIT = C_X.

    PT_ITEM-CUST_MAT35 = GT_DISP-KDMAT.     "Customer PO#
    PT_ITEMX-CUST_MAT35 = C_X.




*    PT_ITEM-REF_DOC_CA = 'B'.               "Document Category of Preceding SD Document
*    PT_ITEMX-REF_DOC_CA = C_X.

*    PT_ITEM-REF_DOC = GT_DISP-ORGBL.
*    READ TABLE GT_VBRP WITH KEY VBELN = GT_DISP-ORGBL
*                                MATNR = GT_DISP-MATNR.
*    IF SY-SUBRC = 0.
*      PT_ITEM-REF_DOC_IT = GT_VBRP-POSNR.
*      PT_ITEM-PLANT = GT_VBRP-WERKS.
*    ENDIF.
    APPEND PT_ITEM.
    CLEAR PT_ITEM.

*    PT_ITEMX-REF_DOC = C_X.
*    PT_ITEMX-REF_DOC_IT = C_X.
    APPEND PT_ITEMX.
    CLEAR PT_ITEMX.

    "Schedule
    PT_SCHDL-ITM_NUMBER = LV_POSNR.
    PT_SCHDLX-ITM_NUMBER = LV_POSNR.

*    PT_SCHDL-SCHED_LINE = 1.
    PT_SCHDL-REQ_DATE   = GT_DISP-VDATU.  "납품 요청일 .
    PT_SCHDLX-REQ_DATE    = C_X.
    PT_SCHDL-REQ_QTY    = GT_DISP-KWMENG. ""수량 .
    PT_SCHDLX-REQ_QTY    = C_X.
    APPEND PT_SCHDL.
    CLEAR PT_SCHDL.

*    PT_SCHDLX-SCHED_LINE = 1.
    APPEND PT_SCHDLX.
    CLEAR PT_SCHDLX.

    " Condition
*    PT_COND-ITM_NUMBER = LV_POSNR.
*    PT_COND-COND_TYPE = 'PR00'.
*    PT_COND-CURRENCY = GT_HEAD-WAERK.
*
*    CLEAR : L_BAPI_AMOUNT , L_SAP_AMOUNT.
*    L_SAP_AMOUNT = GT_DISP-KBETR.
*    IF GT_DISP-KBETR <> 0.
*      CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
*        EXPORTING
*          CURRENCY    = GT_DISP-WAERK
*          SAP_AMOUNT  = L_SAP_AMOUNT
*        IMPORTING
*          BAPI_AMOUNT = L_BAPI_AMOUNT.
*      PT_COND-COND_VALUE = L_BAPI_AMOUNT.
*    ENDIF.
*    APPEND PT_COND.
*    CLEAR PT_COND.

*    PT_CONDX-ITM_NUMBER = LV_POSNR.
*    PT_CONDX-COND_TYPE = 'PR00'.
*    PT_CONDX-CURRENCY = C_X.
*    PT_CONDX-COND_VALUE = C_X.
*    PT_CONDX-UPDATEFLAG = C_U.
*    APPEND PT_CONDX.
*    CLEAR PT_CONDX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_QUOTATION_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_QUOTATION_LIST .
  CALL TRANSACTION 'ZSDR0100'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CUSTOMER_SKU
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_CUSTOMER_SKU .

  DATA : LS_CMR_KNMT LIKE KNMT.

  GT_SKU_C[] = GT_DISP[].

* customer SKU는 유통경로 10에만 해당
  DELETE GT_SKU_C WHERE VTWEG NE '10' AND VTWEG NE '21' .

  SORT GT_SKU_C BY MATNR.
  DELETE ADJACENT DUPLICATES FROM GT_SKU_C COMPARING MATNR.

  LOOP AT GT_SKU_C.
    CLEAR: LS_CMR_KNMT.
    CALL FUNCTION 'RV_CUSTOMER_MATERIAL_READ'
      EXPORTING
        CMR_KDMAT            = ' '
        CMR_KUNNR            = GT_SKU_C-KUNNRO  " origin SOLDTO
        CMR_MATNR            = GT_SKU_C-MATNR   "' '
        CMR_SPART            = GT_SKU_C-SPART
        CMR_VKORG            = '2000' "GT_SKU_C-VKORG => 2000  고정값 : 판매법인
        CMR_VTWEG            = GT_SKU_C-VTWEG
      IMPORTING
        CMR_KNMT             = LS_CMR_KNMT
      EXCEPTIONS
        KNMT_NOT_FOUND       = 1
        OTHERS               = 2
              .
    IF SY-SUBRC <> 0.
*     Implement suitable error handling here
    ELSE.
      GT_SKU_C-KDMAT = LS_CMR_KNMT-KDMAT.
      MODIFY GT_SKU_C.
    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_USAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_USAGE.
  DATA: LT_DISP LIKE TABLE OF GT_DISP WITH HEADER LINE.

  DATA: BEGIN OF LT_KNVV OCCURS 0,
          KUNNR LIKE KNVV-KUNNR,
          VKORG LIKE KNVV-VKORG,
          VTWEG LIKE KNVV-VTWEG,
          SPART LIKE KNVV-SPART,
          KVGR1 LIKE KNVV-KVGR1,
        END OF LT_KNVV.

  DATA: BEGIN OF LT_0021 OCCURS 0,
          ZCM_CODE1 LIKE ZCOMMT0021-ZCM_CODE1,
          ZCM_CODE2 LIKE ZCOMMT0021-ZCM_CODE2,
          ZCM_CODE3 LIKE ZCOMMT0021-ZCM_CODE3,
        END OF LT_0021.

  DATA: LT_ZCOMMT0021 LIKE TABLE OF ZCOMMT0021 WITH HEADER LINE.

  CHECK GT_DISP[] IS NOT INITIAL.

  LT_DISP[] = GT_DISP[].

* knvv에서 shipto, sales org, dist.channel, division으로 Customer Group 1가져와서
* ZCOMMT0021의 SD, SD006으로 Usage가져와 excel upload후 보여줌
  SORT LT_DISP BY KUNNR1 VKORG VTWEG SPART.
  DELETE ADJACENT DUPLICATES FROM LT_DISP
    COMPARING KUNNR1 VKORG VTWEG SPART.


  SELECT KUNNR VKORG KVGR1 FROM KNVV
    INTO CORRESPONDING FIELDS OF TABLE LT_KNVV
    FOR ALL ENTRIES IN LT_DISP
   WHERE KUNNR = LT_DISP-KUNNR1
     AND VKORG = LT_DISP-VKORG
     AND VTWEG = LT_DISP-VTWEG
     AND SPART = LT_DISP-SPART
    .

  CHECK LT_KNVV[] IS NOT INITIAL.
  LOOP AT LT_KNVV.
    LT_0021-ZCM_CODE1 = LT_KNVV-VKORG.
    LT_0021-ZCM_CODE2 = LT_KNVV-KVGR1.
    COLLECT LT_0021. CLEAR LT_0021.

    GT_DISP-KVGR1 = LT_KNVV-KVGR1.
    MODIFY GT_DISP TRANSPORTING KVGR1
      WHERE KUNNR1 = LT_KNVV-KUNNR
        AND VKORG  = LT_KNVV-VKORG.
  ENDLOOP.

  SELECT ZCM_CODE1 ZCM_CODE2 ZCM_CODE3 FROM ZCOMMT0021
    INTO CORRESPONDING FIELDS OF TABLE LT_ZCOMMT0021
    FOR ALL ENTRIES IN LT_0021
   WHERE SPRAS = sy-langu
     AND ZMODULE = 'SD'
     AND ZCLASS = 'SD006'
     AND ZCM_CODE1 = LT_0021-ZCM_CODE1
     AND ZCM_CODE2 = LT_0021-ZCM_CODE2
    .


  LOOP AT LT_ZCOMMT0021.
    GT_USAGE-VKORG = LT_ZCOMMT0021-ZCM_CODE1. " Sales org
    GT_USAGE-KVGR1 = LT_ZCOMMT0021-ZCM_CODE2. " Customer group
    GT_USAGE-VKAUS = LT_ZCOMMT0021-ZCM_CODE3. " Usage

    APPEND GT_USAGE. CLEAR GT_USAGE.

  ENDLOOP.
  SORT GT_USAGE BY VKORG KVGR1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REQUIRED_FIELD_CHECK
*&---------------------------------------------------------------------*
FORM REQUIRED_FIELD_CHECK  USING  PV_DATA PV_TEXT.

  CHECK PV_DATA IS INITIAL.

  IF GT_DISP-MESSAGE IS INITIAL.
    GT_DISP-MESSAGE = PV_TEXT && TEXT-E04.
    GT_DISP-ICON1 = ICON_RED_LIGHT.
  ELSE.
    GT_DISP-MESSAGE = GT_DISP-MESSAGE && '/' && PV_TEXT && TEXT-E04.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form QTY_FIELD_CHECK
*&---------------------------------------------------------------------*
FORM QTY_FIELD_CHECK  USING    PV_DATA PV_TEXT.
DATA: LV_QTY      TYPE I.
  CHECK PV_DATA IS NOT INITIAL.

  LV_QTY = PV_DATA.
  CHECK LV_QTY LE 0.

  IF GT_DISP-MESSAGE IS INITIAL.
    GT_DISP-MESSAGE = PV_TEXT && TEXT-E05.
    GT_DISP-ICON1 = ICON_LED_RED. "ICON_RED_LIGHT.
  ELSE.
    GT_DISP-MESSAGE = GT_DISP-MESSAGE && '/' && PV_TEXT && TEXT-E05.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DUP_CHECK
*&---------------------------------------------------------------------*
FORM DUP_CHECK .
  DATA : LT_DISP LIKE TABLE OF GT_DISP WITH HEADER LINE.

  DATA : BEGIN OF LT_DUP OCCURS 0.
      INCLUDE STRUCTURE GT_DISP.
  DATA : CNT TYPE I.
  DATA : END   OF LT_DUP.
*  DATA : LT_DUP2 LIKE TABLE OF LT_DUP WITH HEADER LINE.

* SKU duplication check
  CLEAR : LT_DUP, LT_DUP[]. CLEAR : LT_DISP, LT_DISP[].
  LT_DISP[] = GT_DISP[].
  SORT LT_DISP BY BSTKD MATNR.
*  DELETE ADJACENT DUPLICATES FROM LT_DISP COMPARING BSTKD MATNR.
  LOOP AT LT_DISP.
*    MOVE-CORRESPONDING LT_DISP TO LT_DUP.
    LT_DUP-BSTKD = LT_DISP-BSTKD.
    LT_DUP-MATNR = LT_DISP-MATNR.
    LT_DUP-CNT = 1. COLLECT LT_DUP. CLEAR LT_DUP.
  ENDLOOP.
  DELETE LT_DUP WHERE CNT = 1.
  LOOP AT LT_DUP.
    LOOP AT GT_DISP WHERE BSTKD = LT_DUP-BSTKD.
      CHECK GT_DISP-ICON1 IS INITIAL.
      IF LT_DUP-MATNR = GT_DISP-MATNR.
        GT_DISP-MESSAGE = TEXT-E07. "Deplicated Origin PO# / Material
        GT_DISP-ICON1 = ICON_LED_RED. "ICON_RED_LIGHT.
        MODIFY GT_DISP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.


* header difference check
  CLEAR : LT_DUP, LT_DUP[]. CLEAR : LT_DISP, LT_DISP[].
  LT_DISP[] = GT_DISP[].
*S_2011/2/11 remove ValidTo : move RDD By E00064
*  SORT LT_DISP BY BSTKD AUART VKORG VTWEG SPART
*                  KUNNR KUNNR1 KUNNRO BNDDT VDATU.
*  DELETE ADJACENT DUPLICATES FROM LT_DISP
*    COMPARING BSTKD AUART VKORG VTWEG SPART
*                  KUNNR KUNNR1 KUNNRO BNDDT VDATU.
  SORT LT_DISP BY BSTKD AUART VKORG VTWEG SPART
                  KUNNR KUNNR1 KUNNRO VDATU.
  DELETE ADJACENT DUPLICATES FROM LT_DISP
    COMPARING BSTKD AUART VKORG VTWEG SPART
                  KUNNR KUNNR1 KUNNRO VDATU.
*E_2011/2/11 remove ValidTo : move RDD By E00064
  LOOP AT LT_DISP.
    LT_DUP-BSTKD = LT_DISP-BSTKD.
    LT_DUP-CNT = 1. COLLECT LT_DUP. CLEAR LT_DUP.
  ENDLOOP.
  DELETE LT_DUP WHERE CNT = 1.
  LOOP AT LT_DUP.
    LOOP AT GT_DISP WHERE BSTKD = LT_DUP-BSTKD.
      CHECK GT_DISP-ICON1 IS INITIAL.
*      GT_DISP-MESSAGE = TEXT-E08. "Different RDD for Origin PO#
      GT_DISP-MESSAGE = TEXT-E10. "Different header for Origin PO#
      GT_DISP-ICON1 = ICON_LED_RED. "ICON_RED_LIGHT.
      MODIFY GT_DISP.
    ENDLOOP.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form VKORG_WERKS_LGORT_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM VKORG_WERKS_LGORT_CHECK .
  CHECK GT_DISP-VKORG EQ '2011'.

  IF GT_DISP-WERKS NE '2011'.
    GT_DISP-MESSAGE = TEXT-E06.
    GT_DISP-ICON1 = ICON_RED_LIGHT.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DEFAULT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_DEFAULT .

  IF GT_DISP-AUART  IS INITIAL. GT_DISP-AUART = GS_DISP-AUART. ENDIF.
  IF GT_DISP-VKORG  IS INITIAL. GT_DISP-VKORG = GS_DISP-VKORG. ENDIF.
  IF GT_DISP-VTWEG  IS INITIAL. GT_DISP-VTWEG = GS_DISP-VTWEG. ENDIF.
  IF GT_DISP-SPART  IS INITIAL. GT_DISP-SPART = GS_DISP-SPART. ENDIF.
  IF GT_DISP-KUNNR  IS INITIAL. GT_DISP-KUNNR = GS_DISP-KUNNR. ENDIF.
  IF GT_DISP-KUNNR1 IS INITIAL. GT_DISP-KUNNR1 = GS_DISP-KUNNR1. ENDIF.
  IF GT_DISP-KUNNRO IS INITIAL. GT_DISP-KUNNRO = GS_DISP-KUNNRO. ENDIF.
*  IF GT_DISP-BSTKD  IS INITIAL. GT_DISP-BSTKD = GS_DISP-BSTKD. ENDIF.
*S_2011/2/11 remove ValidTo : move RDD By E00064
*  IF GT_DISP-BNDDT  IS INITIAL. GT_DISP-BNDDT = GS_DISP-BNDDT. ENDIF.
*E_2011/2/11 remove ValidTo : move RDD By E00064
  IF GT_DISP-VDATU  IS INITIAL. GT_DISP-VDATU = GS_DISP-VDATU. ENDIF.
  IF GT_DISP-WERKS  IS INITIAL. GT_DISP-WERKS = GS_DISP-WERKS. ENDIF.
  IF GT_DISP-LGORT  IS INITIAL. GT_DISP-LGORT = GS_DISP-LGORT. ENDIF.
  IF GT_DISP-VKAUS  IS INITIAL. GT_DISP-VKAUS = GS_DISP-VKAUS. ENDIF.
*  IF GT_DISP-MATNR  IS INITIAL. GT_DISP-MATNR = GS_DISP-MATNR. ENDIF.
  IF GT_DISP-MEINS  IS INITIAL. GT_DISP-MEINS = GS_DISP-MEINS. ENDIF.


ENDFORM.
