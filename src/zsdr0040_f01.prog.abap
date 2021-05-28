*----------------------------------------------------------------------*
***INCLUDE ZSDR0040_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INIT
*&---------------------------------------------------------------------*
*& 프로그램 첫 구동시 1회 수행하는 로직
*&---------------------------------------------------------------------*
*&      <-- SSCRFIELDS Screen 1000 Upload Format Download Button
*&---------------------------------------------------------------------*
FORM init CHANGING pv_fname      LIKE p_fname
                   ps_sscrfields TYPE sscrfields..
  "----// 조건유형 초기화
  p_kschl = 'PR00'.
  "----// 버전 초기화
  CONCATENATE '1' sy-datlo+4(02) INTO p_vrsio.
  "----// 영업조직 초기화
  p_vkorg = '2011'.
  "----// 플랜트 초기화
  p_werks = '2011'.
  "----// 계획연도 초기화
  p_gjahr = sy-datlo(4).
  "----// 엑셀 템플릿 다운로드 버튼 만들기
  CONCATENATE icon_xls TEXT-007 INTO ps_sscrfields-functxt_01.
  "----// 파일명 초기화
  IF pv_fname IS INITIAL.
    pv_fname = 'C:\'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
*& 모드 선택에 따른 1000번 선택화면 셋업
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM set_screen .
  LOOP AT SCREEN.
    IF     p_file EQ 'X'.
      CASE screen-group1.
        WHEN 'M01'.  screen-active = 1.
        WHEN 'M02'.  screen-active = 0.
        WHEN OTHERS.
          "
      ENDCASE.
    ELSEIF p_data EQ 'X'.
      CASE screen-group1.
        WHEN 'M01'.  screen-active = 0.
        WHEN 'M02'.  screen-active = 1.
        WHEN OTHERS.
          "
      ENDCASE.
    ELSE.
      CHECK 1 = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Include          ZMMR0060_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FILENAME_F4
*&---------------------------------------------------------------------*
*& 파일 열기 대화상자
*&---------------------------------------------------------------------*
*&      <-- P_FNAME  Excel File Name
*&---------------------------------------------------------------------*
FORM filename_f4  CHANGING  pv_fname TYPE text1024.
*  CALL FUNCTION 'WS_FILENAME_GET'
*    EXPORTING
*      DEF_FILENAME     = SPACE
*      DEF_PATH         = SPACE
*      MASK             = '*.XLSX.'
*      MODE             = 'O'
*      TITLE            = TEXT-004  "Select File
*    IMPORTING
*      FILENAME         = P_FNAME
*    EXCEPTIONS
*      INV_WINSYS       = 1
*      NO_BATCH         = 2
*      SELECTION_CANCEL = 3
*      SELECTION_ERROR  = 4.
  DATA: lv_window_title      TYPE string,
        lv_file_filter       TYPE string,
        lv_initial_directory TYPE string,
        lt_file_table        TYPE filetable,
        lv_rc                TYPE i,
        lv_file_name         TYPE string.
  lv_window_title      = TEXT-004.
  lv_file_filter       = TEXT-005.
  lv_initial_directory = TEXT-006.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_window_title
      default_filename        = space
      file_filter             = lv_file_filter
      initial_directory       = lv_initial_directory
      multiselection          = space
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 5.
  IF sy-subrc EQ 0.
    READ TABLE lt_file_table INTO lv_file_name INDEX 1.
    IF sy-subrc EQ 0.
      pv_fname  =  lv_file_name.
    ELSE.
      CHECK 1 = 1.
    ENDIF.
  ELSE.
    CHECK 1 = 1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form POP_UP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GV_ANSWER
*&---------------------------------------------------------------------*
FORM pop_up USING    pv_titlebar TYPE string
                     pv_question TYPE string
            CHANGING pv_answer   LIKE gv_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = pv_titlebar
      text_question         = pv_question
      text_button_1         = 'Yes'(027)
      icon_button_1         = ' '
      text_button_2         = 'Nein'(028)
      icon_button_2         = ' '
      default_button        = '1'
      display_cancel_button = 'X'
    IMPORTING
      answer                = pv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWN_FILENAME
*&---------------------------------------------------------------------*
*&      <-- GV_DN_FNAME File Name
*&      <-- GV_DN_FPATH Path
*&---------------------------------------------------------------------*
FORM down_filename CHANGING pv_fname TYPE string
                            pv_fpath TYPE string.
  DATA: lv_window_title      TYPE string,
        lv_default_extension TYPE string,
        lv_file_filter       TYPE string,
        lv_path              TYPE string.
  IF p_mont EQ 'X'.
    lv_window_title      = TEXT-029."SALES_PLAN_MONTHLY.xlsx
  ELSE.
    lv_window_title      = TEXT-030."SALES_PLAN_YEARLY.xlsx
  ENDIF.
  lv_default_extension = TEXT-025.  "XLSX
  lv_file_filter       = TEXT-026.  "Excel files (*.xlsx)|*.xlsx|Excel files (*.xls)|*.xls
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = lv_window_title
      default_extension = lv_default_extension
      default_file_name = pv_fname
      file_filter       = lv_file_filter
    CHANGING
      filename          = pv_fname
      path              = lv_path
      fullpath          = pv_fpath.
  IF sy-subrc <> 0.
    CHECK 1 = 1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWN_EXCEL_FORM
*&---------------------------------------------------------------------*
*& Upload Format Download
*&---------------------------------------------------------------------*
*&      <-- GV_RTN_CODE Return Code
*&---------------------------------------------------------------------*
FORM down_excel_form USING    pv_dn_fpath LIKE gv_dn_fpath
                     CHANGING pv_rtn_code LIKE gv_rtn_code.
  DATA: lv_dfname     LIKE rlgrap-filename,
        lv_objid      LIKE wwwdata-objid,
        ls_wwwdatatab LIKE wwwdatatab,
        lv_fname      LIKE rlgrap-filename,
        lv_tail       TYPE string,
        lv_filename   TYPE string.
  "----// 파일명, 템플릿명
  CLEAR: ls_wwwdatatab, lv_fname, lv_tail.
  pv_rtn_code = 'E'.
  lv_dfname   = pv_dn_fpath.  "엑셀양식다운로드 파일명 C:\BOM_FILE_LAYOUT.xlsx.
  IF p_mont EQ 'X'.
    lv_objid    = TEXT-029.   "템플릿명 ZSDR0040_01
  ELSE.
    lv_objid    = TEXT-030.   "템플릿명 ZSDR0040_02
  ENDIF.
  "----// 엑셀양식 찾기
  SELECT SINGLE  *
    INTO CORRESPONDING FIELDS OF ls_wwwdatatab
    FROM wwwdata              "WWW 오브젝트 저장에 대한 INDX-유형 테이블
   WHERE relid = 'MI'
     AND objid = lv_objid     "SAP WWW 게이트웨이 오브젝트 이름
     AND srtf2 = 0.
  IF sy-subrc NE 0.
    MESSAGE s018 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  "----// Web 템플릿(Excel Form) 다운로드
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key         = ls_wwwdatatab
      destination = lv_dfname.
  CLEAR: lv_fname, lv_tail.
  SPLIT lv_dfname AT ' ' INTO lv_fname lv_tail.
  IF  lv_dfname <> lv_fname.
    MESSAGE s018 WITH lv_dfname.
  ELSE.
    "----// Download 파일명에 공백이 없는 경우 Excel 실행
*    CALL FUNCTION 'WS_EXECUTE'
*      EXPORTING
*        commandline        = lv_dfname
*        program            = 'EXCEL.EXE'
*      EXCEPTIONS
*        frontend_error     = 1
*        no_batch           = 2
*        prog_not_found     = 3
*        illegal_option     = 4
*        gui_refuse_execute = 5
*        OTHERS             = 6.
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*    MESSAGE s017 WITH lv_dfname.
    CLEAR lv_filename.
    lv_filename = lv_dfname.
    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application            = 'excel.exe'
        parameter              = lv_filename
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9.
    IF sy-subrc EQ 0.
      MESSAGE s017 WITH lv_dfname.
    ELSE.
      pv_rtn_code = 'E'.
      EXIT.
    ENDIF.
  ENDIF.
  CLEAR: pv_rtn_code.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_EXCEL_DATA_W
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PT_FDATA  엑셀데이터
*&      --> PV_FNAME  파일명
*&---------------------------------------------------------------------*
FORM upload_excel_data_w  TABLES  pt_fdata_w LIKE gt_fdata_w
                          USING   pv_fname   LIKE p_fname
                                  pv_dcpfm   TYPE char01
                         CHANGING pv_retcd   TYPE char01
                                  pv_retxt   TYPE text100.
  CLEAR: pt_fdata_w[], pt_fdata_w.
  "----// 파일 존재여부  확인
  DATA: lv_fname TYPE string,
        lv_ret   TYPE c LENGTH 1.
  lv_fname = pv_fname.
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file   = lv_fname
    RECEIVING
      result = lv_ret
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    LEAVE LIST-PROCESSING.
  ENDIF.
  "----// 엑셀파일 업로드
  DATA: lt_intern TYPE zfi_alsmex_tabline OCCURS 0 WITH HEADER LINE,
        ld_index  TYPE i.
  FIELD-SYMBOLS: <fs>.
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = pv_fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 22
      i_end_row               = 65536
    TABLES
      intern                  = lt_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    LEAVE LIST-PROCESSING.
    pv_retcd = 'E'.
    pv_retxt = TEXT-016.
  ENDIF.
  IF lt_intern[] IS INITIAL.
    CHECK 1 = 1.
  ELSE.
    LOOP AT lt_intern.
      MOVE lt_intern-col TO ld_index.
      ASSIGN COMPONENT ld_index OF STRUCTURE pt_fdata_w TO <fs>.
      MOVE lt_intern-value TO <fs>.
      AT END OF row.
        PERFORM convert_number IN PROGRAM zmmr0070 IF FOUND
                                    USING pv_dcpfm:
                                 CHANGING pt_fdata_w-kwmeng_w00,
                                 CHANGING pt_fdata_w-kwmeng_w01,
                                 CHANGING pt_fdata_w-kwmeng_w02,
                                 CHANGING pt_fdata_w-kwmeng_w03,
                                 CHANGING pt_fdata_w-kwmeng_w04,
                                 CHANGING pt_fdata_w-kwmeng_w05,
                                 CHANGING pt_fdata_w-kwmeng_w06,
                                 CHANGING pt_fdata_w-kwmeng_w07,
                                 CHANGING pt_fdata_w-kwmeng_w08,
                                 CHANGING pt_fdata_w-kwmeng_w09,
                                 CHANGING pt_fdata_w-kwmeng_w10,
                                 CHANGING pt_fdata_w-kwmeng_w11,
                                 CHANGING pt_fdata_w-kwmeng_w12,
                                 CHANGING pt_fdata_w-kwmeng_w13,
                                 CHANGING pt_fdata_w-kwmeng_w14,
                                 CHANGING pt_fdata_w-kwmeng_w15.
        APPEND pt_fdata_w.
        CLEAR pt_fdata_w.
      ENDAT.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_EXCEL_DATA_M
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PT_FDATA  엑셀데이터
*&      --> PV_FNAME  파일명
*&---------------------------------------------------------------------*
FORM upload_excel_data_m  TABLES  pt_fdata_m LIKE gt_fdata_m
                          USING   pv_fname   LIKE p_fname
                                  pv_dcpfm   TYPE char01
                         CHANGING pv_retcd   TYPE char01
                                  pv_retxt   TYPE text100.
  CLEAR: pt_fdata_m[], pt_fdata_m.
  "----// 파일 존재여부  확인
  DATA: lv_fname TYPE string,
        lv_ret   TYPE c LENGTH 1.
  lv_fname = pv_fname.
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file   = lv_fname
    RECEIVING
      result = lv_ret
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    LEAVE LIST-PROCESSING.
  ENDIF.
  "----// 엑셀파일 업로드
  DATA: lt_intern TYPE zfi_alsmex_tabline OCCURS 0 WITH HEADER LINE,
        ld_index  TYPE i.
  FIELD-SYMBOLS: <fs>.
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = pv_fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 22
      i_end_row               = 65536
    TABLES
      intern                  = lt_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    LEAVE LIST-PROCESSING.
    pv_retcd = 'E'.
    pv_retxt = TEXT-016.
  ENDIF.
  IF lt_intern[] IS INITIAL.
    CHECK 1 = 1.
  ELSE.
    LOOP AT lt_intern.
      MOVE lt_intern-col TO ld_index.
      ASSIGN COMPONENT ld_index OF STRUCTURE pt_fdata_m TO <fs>.
      MOVE lt_intern-value TO <fs>.
      AT END OF row.
        PERFORM convert_number IN PROGRAM zmmr0070 IF FOUND
                                    USING pv_dcpfm:
                                 CHANGING pt_fdata_m-kwmeng_m00,
                                 CHANGING pt_fdata_m-kwmeng_m01,
                                 CHANGING pt_fdata_m-kwmeng_m02,
                                 CHANGING pt_fdata_m-kwmeng_m03,
                                 CHANGING pt_fdata_m-kwmeng_m04,
                                 CHANGING pt_fdata_m-kwmeng_m05,
                                 CHANGING pt_fdata_m-kwmeng_m06,
                                 CHANGING pt_fdata_m-kwmeng_m07,
                                 CHANGING pt_fdata_m-kwmeng_m08,
                                 CHANGING pt_fdata_m-kwmeng_m09,
                                 CHANGING pt_fdata_m-kwmeng_m10,
                                 CHANGING pt_fdata_m-kwmeng_m11.
        APPEND pt_fdata_m.
        CLEAR pt_fdata_m.
      ENDAT.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_MONTHLY_DATA_W
*&---------------------------------------------------------------------*
*& 월계획(S804) 생성용 주별단위 입력 데이터 생성
*&---------------------------------------------------------------------*
*&      --> GT_FDATA_W
*&      <-- GT_MAIN_W    ALV 리스트
*&---------------------------------------------------------------------*
FORM make_monthly_data_w  TABLES pt_fdata_w LIKE gt_fdata_w
                                 pt_main_w  LIKE gt_main_w
                           USING pv_spmon   TYPE spmon
                                 pv_stwae   TYPE stwae
                                 pv_basme   TYPE meins.
  "----// Variables
  DATA: BEGIN OF lt_vtweg_inf OCCURS 0,
          vtweg TYPE vtweg,
          vtext TYPE vtxtk,
        END OF lt_vtweg_inf,
        BEGIN OF lt_kunnr_key OCCURS 0,
          kunnr TYPE kunnr,
        END OF lt_kunnr_key,
        BEGIN OF lt_kunnr_inf OCCURS 0,
          kunnr TYPE kunnr,
          name1 TYPE name1,
        END OF lt_kunnr_inf,
        BEGIN OF lt_matnr_key OCCURS 0,
          matnr TYPE matnr,
        END OF lt_matnr_key,
        BEGIN OF lt_matnr_inf OCCURS 0,
          matnr TYPE matnr,
          vtweg TYPE vtweg,
          maktx TYPE maktx,
          vrkme TYPE vrkme,
          meins TYPE meins,
        END OF lt_matnr_inf,
        BEGIN OF lt_werks_inf OCCURS 0,
          werks TYPE werks_d,
          name1 TYPE name1,
        END OF lt_werks_inf,
        BEGIN OF lt_price_inf OCCURS 0,
          vtweg TYPE vtweg,      "channel
          kunnr TYPE kunnr,      "sold-to
          matnr TYPE matnr,      "material
          datab TYPE a305-datab, "begin data
          kbetr TYPE kbetr_kond, "Condition amount (83.80)
          konwa TYPE konwa,      "Currency (USD)
          kpein TYPE kpein,      "Condition Pricing Unit (1)
          kmein TYPE kmein,      "Condition Unit (EA)
        END OF lt_price_inf.
  DATA: lv_price TYPE kbetr_kond,
        lv_kpein TYPE menge_d,
        lv_menge TYPE menge_d.
  DATA: lt_cellstyl TYPE lvc_t_styl,
        lv_idx      TYPE sy-tabix.
  "----// Create ALV Data
  CLEAR: pt_main_w[], pt_main_w, wa_main_w.
  LOOP AT pt_fdata_w INTO wa_fdata_w.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_fdata_w-kunag
      IMPORTING
        output = wa_main_w-kunag.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_fdata_w-kunwe
      IMPORTING
        output = wa_main_w-kunwe.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = wa_fdata_w-matnr
      IMPORTING
        output       = wa_main_w-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    "wa_main_w-vkorg = p_vkorg.
    wa_main_w-vtweg = wa_fdata_w-vtweg.
    "wa_main_w-kunag = wa_fdata_w-kunag.
    "wa_main_w-kunwe = wa_fdata_w-kunwe.
    wa_main_w-werks = wa_fdata_w-werks.
    "wa_main_w-matnr = wa_fdata_w-matnr.
    wa_main_w-vkaus = wa_fdata_w-vkaus.
    wa_main_w-kwmeng_w00 = wa_fdata_w-kwmeng_w00.
    wa_main_w-kwmeng_w01 = wa_fdata_w-kwmeng_w01.
    wa_main_w-kwmeng_w02 = wa_fdata_w-kwmeng_w02.
    wa_main_w-kwmeng_w03 = wa_fdata_w-kwmeng_w03.
    wa_main_w-kwmeng_w04 = wa_fdata_w-kwmeng_w04.
    wa_main_w-kwmeng_w05 = wa_fdata_w-kwmeng_w05.
    wa_main_w-kwmeng_w06 = wa_fdata_w-kwmeng_w06.
    wa_main_w-kwmeng_w07 = wa_fdata_w-kwmeng_w07.
    wa_main_w-kwmeng_w08 = wa_fdata_w-kwmeng_w08.
    wa_main_w-kwmeng_w09 = wa_fdata_w-kwmeng_w09.
    wa_main_w-kwmeng_w10 = wa_fdata_w-kwmeng_w10.
    wa_main_w-kwmeng_w11 = wa_fdata_w-kwmeng_w11.
    wa_main_w-kwmeng_w12 = wa_fdata_w-kwmeng_w12.
    wa_main_w-kwmeng_w13 = wa_fdata_w-kwmeng_w13.
    wa_main_w-kwmeng_w14 = wa_fdata_w-kwmeng_w14.
    wa_main_w-kwmeng_w15 = wa_fdata_w-kwmeng_w15.
    "----// key insert
    lt_kunnr_key-kunnr = wa_main_w-kunag.
    COLLECT lt_kunnr_key.
    lt_kunnr_key-kunnr = wa_main_w-kunwe.
    COLLECT lt_kunnr_key.
    lt_matnr_key-matnr = wa_main_w-matnr.
    COLLECT lt_matnr_key.
    APPEND wa_main_w TO pt_main_w.
    CLEAR wa_main_w.
  ENDLOOP.
  "----// fetch text info
  SELECT vtweg,
         vtext
    INTO CORRESPONDING FIELDS OF TABLE @lt_vtweg_inf
    FROM tvtwt
   WHERE spras EQ @sy-langu.
  SORT lt_vtweg_inf BY vtweg.
  SELECT werks,
         name1
    INTO CORRESPONDING FIELDS OF TABLE @lt_werks_inf
    FROM t001w.
  SORT lt_werks_inf BY werks.
  IF lt_kunnr_key[] IS NOT INITIAL.
    SORT lt_kunnr_key BY kunnr.
    SELECT kunnr,
           name1
      INTO CORRESPONDING FIELDS OF TABLE @lt_kunnr_inf
      FROM kna1
       FOR ALL ENTRIES IN @lt_kunnr_key
     WHERE kunnr EQ @lt_kunnr_key-kunnr.
    SORT lt_kunnr_inf BY kunnr.
  ENDIF.
  IF lt_matnr_key[] IS NOT INITIAL.
    SORT lt_matnr_key BY matnr.
    SELECT t~matnr,
           v~vtweg,
           t~maktx,
           v~vrkme,
           m~meins
      INTO CORRESPONDING FIELDS OF TABLE @lt_matnr_inf
      FROM mara AS m INNER JOIN makt AS t ON t~matnr = m~matnr
                                         AND t~spras = @sy-langu
                     INNER JOIN mvke AS v ON v~matnr = m~matnr
                                         AND v~vkorg = @p_vkorg
       FOR ALL ENTRIES IN @lt_matnr_key
     WHERE m~matnr EQ @lt_matnr_key-matnr.
    SORT lt_matnr_inf BY matnr vtweg.
  ENDIF.
  "----// get price
  SELECT a~vtweg,
         a~kunnr,
         a~matnr,
         a~datab,
         k~kbetr,
         k~konwa,
         k~kpein,
         k~kmein
    INTO CORRESPONDING FIELDS OF TABLE @lt_price_inf
    FROM a305 AS a INNER JOIN konp AS k ON k~knumh = a~knumh
                                       AND k~kopos = '01'
   WHERE a~kappl    EQ 'V'
     AND a~kschl    EQ 'PR00'
     AND a~vkorg    EQ @p_vkorg
     AND a~datab    LE @sy-datlo
     AND a~datbi    GE @sy-datlo
     AND k~loevm_ko EQ @space.
  SORT lt_price_inf BY vtweg kunnr matnr datab DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_price_inf COMPARING vtweg kunnr matnr.
  "----// fill ALV data
  LOOP AT pt_main_w INTO wa_main_w.
    lv_idx = sy-tabix.
    "----// price
    READ TABLE lt_price_inf WITH KEY vtweg = wa_main_w-vtweg
                                     kunnr = wa_main_w-kunag
                                     matnr = wa_main_w-matnr
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-planp = lt_price_inf-kbetr.  "계획판가
      wa_main_w-kbetr = lt_price_inf-kbetr.  "판가
      wa_main_w-konwa = lt_price_inf-konwa.  "통화키
      wa_main_w-kpein = lt_price_inf-kpein.  "금액단위
      wa_main_w-kmein = lt_price_inf-kmein.  "수량단위
    ELSE.
      wa_main_w-planp = 0.                   "계획판가
      wa_main_w-kbetr = 0.                   "판가
      wa_main_w-konwa = pv_stwae.
      wa_main_w-kpein = 1.
      wa_main_w-kmein = pv_basme.
*      IF wa_main_w-status NE '1'.
*        wa_main_w-retxt  = TEXT-m41.     "Sales price is not registeded.
*        wa_main_w-status = '1'.          "red
*      ENDIF.
    ENDIF.
    "----// channel text
    READ TABLE lt_vtweg_inf WITH KEY vtweg = wa_main_w-vtweg BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-vtext = lt_vtweg_inf-vtext.
    ENDIF.
    "----// sold to text
    READ TABLE lt_kunnr_inf WITH KEY kunnr = wa_main_w-kunag BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-name1 = lt_kunnr_inf-name1.
    ENDIF.
    "----// ship to text
    READ TABLE lt_kunnr_inf WITH KEY kunnr = wa_main_w-kunwe BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-name2 = lt_kunnr_inf-name1.
    ENDIF.
    "----// plant text
    READ TABLE lt_werks_inf WITH KEY werks = wa_main_w-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-name3 = lt_werks_inf-name1.
    ENDIF.
    "----// material text
    READ TABLE lt_matnr_inf WITH KEY matnr = wa_main_w-matnr
                                     vtweg = wa_main_w-vtweg
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-maktx = lt_matnr_inf-maktx.
      IF lt_matnr_inf-vrkme IS NOT INITIAL.
        wa_main_w-vrkme = lt_matnr_inf-vrkme.
      ELSE.
        wa_main_w-vrkme = lt_matnr_inf-meins.
      ENDIF.
    ENDIF.
    "----// net value
    IF wa_main_w-vrkme EQ wa_main_w-kmein.
      lv_menge = wa_main_w-kpein.
      lv_price = wa_main_w-kbetr / lv_menge.
    ELSE.
      lv_kpein = wa_main_w-kpein.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = wa_main_w-matnr
          i_in_me              = wa_main_w-kmein
          i_out_me             = wa_main_w-vrkme
          i_menge              = lv_kpein
        IMPORTING
          e_menge              = lv_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        lv_menge = 1.
      ENDIF.
      lv_price = wa_main_w-kbetr / lv_menge.
    ENDIF.
    wa_main_w-netwr_w00 = wa_main_w-kwmeng_w00 * lv_price.
    wa_main_w-netwr_w01 = wa_main_w-kwmeng_w01 * lv_price.
    wa_main_w-netwr_w02 = wa_main_w-kwmeng_w02 * lv_price.
    wa_main_w-netwr_w03 = wa_main_w-kwmeng_w03 * lv_price.
    wa_main_w-netwr_w04 = wa_main_w-kwmeng_w04 * lv_price.
    wa_main_w-netwr_w05 = wa_main_w-kwmeng_w05 * lv_price.
    wa_main_w-netwr_w06 = wa_main_w-kwmeng_w06 * lv_price.
    wa_main_w-netwr_w07 = wa_main_w-kwmeng_w07 * lv_price.
    wa_main_w-netwr_w08 = wa_main_w-kwmeng_w08 * lv_price.
    wa_main_w-netwr_w09 = wa_main_w-kwmeng_w09 * lv_price.
    wa_main_w-netwr_w10 = wa_main_w-kwmeng_w10 * lv_price.
    wa_main_w-netwr_w11 = wa_main_w-kwmeng_w11 * lv_price.
    wa_main_w-netwr_w12 = wa_main_w-kwmeng_w12 * lv_price.
    wa_main_w-netwr_w13 = wa_main_w-kwmeng_w13 * lv_price.
    wa_main_w-netwr_w14 = wa_main_w-kwmeng_w14 * lv_price.
    wa_main_w-netwr_w15 = wa_main_w-kwmeng_w15 * lv_price.
    wa_main_w-waerk = wa_main_w-konwa.
    "----// cell style
    REFRESH lt_cellstyl.
    CLEAR wa_main_w-cellstyl.
    PERFORM fill_cellstyl_w CHANGING lt_cellstyl.
    INSERT LINES OF lt_cellstyl INTO TABLE wa_main_w-cellstyl.
    MODIFY pt_main_w FROM wa_main_w INDEX lv_idx.
  ENDLOOP.
  CHECK 1 = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_YEARLY_DATA_M
*&---------------------------------------------------------------------*
*& 월계획(S804) 생성용 주별단위 입력 데이터 생성
*&---------------------------------------------------------------------*
*&      --> GT_FDATA_M
*&      <-- GT_MAIN_M    ALV 리스트
*&---------------------------------------------------------------------*
FORM make_yearly_data_m  TABLES pt_fdata_m  LIKE gt_fdata_m
                                pt_main_m   LIKE gt_main_m
                           USING pv_spmon   TYPE spmon
                                 pv_stwae   TYPE stwae
                                 pv_basme   TYPE meins.
  "----// Variables
  DATA: BEGIN OF lt_vtweg_inf OCCURS 0,
          vtweg TYPE vtweg,
          vtext TYPE vtxtk,
        END OF lt_vtweg_inf,
        BEGIN OF lt_kunnr_key OCCURS 0,
          kunnr TYPE kunnr,
        END OF lt_kunnr_key,
        BEGIN OF lt_kunnr_inf OCCURS 0,
          kunnr TYPE kunnr,
          name1 TYPE name1,
        END OF lt_kunnr_inf,
        BEGIN OF lt_matnr_key OCCURS 0,
          matnr TYPE matnr,
        END OF lt_matnr_key,
        BEGIN OF lt_matnr_inf OCCURS 0,
          matnr TYPE matnr,
          vtweg TYPE vtweg,
          maktx TYPE maktx,
          vrkme TYPE vrkme,
          meins TYPE meins,
        END OF lt_matnr_inf,
        BEGIN OF lt_werks_inf OCCURS 0,
          werks TYPE werks_d,
          name1 TYPE name1,
        END OF lt_werks_inf,
        BEGIN OF lt_price_inf OCCURS 0,
          vtweg TYPE vtweg,      "channel
          kunnr TYPE kunnr,      "sold-to
          matnr TYPE matnr,      "material
          datab TYPE a305-datab, "begin data
          kbetr TYPE kbetr_kond, "Condition amount (83.80)
          konwa TYPE konwa,      "Currency (USD)
          kpein TYPE kpein,      "Condition Pricing Unit (1)
          kmein TYPE kmein,      "Condition Unit (EA)
        END OF lt_price_inf.
  DATA: lv_price TYPE kbetr_kond,
        lv_kpein TYPE menge_d,
        lv_menge TYPE menge_d.
  DATA: lt_cellstyl TYPE lvc_t_styl,
        lv_idx      TYPE sy-tabix.
  "----// Create ALV Data
  CLEAR: pt_main_m[], pt_main_m, wa_main_m.
  LOOP AT pt_fdata_m INTO wa_fdata_m.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_fdata_m-kunag
      IMPORTING
        output = wa_main_m-kunag.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_fdata_m-kunwe
      IMPORTING
        output = wa_main_m-kunwe.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = wa_fdata_m-matnr
      IMPORTING
        output       = wa_main_m-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    "wa_main_m-vkorg = p_vkorg.
    wa_main_m-vtweg = wa_fdata_m-vtweg.
    "wa_main_m-kunag = wa_fdata_m-kunag.
    "wa_main_m-kunwe = wa_fdata_m-kunwe.
    wa_main_m-werks = wa_fdata_m-werks.
    "wa_main_m-matnr = wa_fdata_m-matnr.
    wa_main_m-vkaus = wa_fdata_m-vkaus.
    wa_main_m-kwmeng_m00 = wa_fdata_m-kwmeng_m00.
    wa_main_m-kwmeng_m01 = wa_fdata_m-kwmeng_m01.
    wa_main_m-kwmeng_m02 = wa_fdata_m-kwmeng_m02.
    wa_main_m-kwmeng_m03 = wa_fdata_m-kwmeng_m03.
    wa_main_m-kwmeng_m04 = wa_fdata_m-kwmeng_m04.
    wa_main_m-kwmeng_m05 = wa_fdata_m-kwmeng_m05.
    wa_main_m-kwmeng_m06 = wa_fdata_m-kwmeng_m06.
    wa_main_m-kwmeng_m07 = wa_fdata_m-kwmeng_m07.
    wa_main_m-kwmeng_m08 = wa_fdata_m-kwmeng_m08.
    wa_main_m-kwmeng_m09 = wa_fdata_m-kwmeng_m09.
    wa_main_m-kwmeng_m10 = wa_fdata_m-kwmeng_m10.
    wa_main_m-kwmeng_m11 = wa_fdata_m-kwmeng_m11.
    "----// key insert
    lt_kunnr_key-kunnr = wa_main_m-kunag.
    COLLECT lt_kunnr_key.
    lt_kunnr_key-kunnr = wa_main_m-kunwe.
    COLLECT lt_kunnr_key.
    lt_matnr_key-matnr = wa_main_m-matnr.
    COLLECT lt_matnr_key.
    APPEND wa_main_m TO pt_main_m.
    CLEAR wa_main_m.
  ENDLOOP.
  "----// fetch text info
  SELECT vtweg,
         vtext
    INTO CORRESPONDING FIELDS OF TABLE @lt_vtweg_inf
    FROM tvtwt
   WHERE spras EQ @sy-langu.
  SORT lt_vtweg_inf BY vtweg.
  SELECT werks,
         name1
    INTO CORRESPONDING FIELDS OF TABLE @lt_werks_inf
    FROM t001w.
  SORT lt_werks_inf BY werks.
  IF lt_kunnr_key[] IS NOT INITIAL.
    SORT lt_kunnr_key BY kunnr.
    SELECT kunnr,
           name1
      INTO CORRESPONDING FIELDS OF TABLE @lt_kunnr_inf
      FROM kna1
       FOR ALL ENTRIES IN @lt_kunnr_key
     WHERE kunnr EQ @lt_kunnr_key-kunnr.
    SORT lt_kunnr_inf BY kunnr.
  ENDIF.
  IF lt_matnr_key[] IS NOT INITIAL.
    SORT lt_matnr_key BY matnr.
    SELECT t~matnr,
           v~vtweg,
           t~maktx,
           v~vrkme,
           m~meins
      INTO CORRESPONDING FIELDS OF TABLE @lt_matnr_inf
      FROM mara AS m INNER JOIN makt AS t ON t~matnr = m~matnr
                                         AND t~spras = @sy-langu
                     INNER JOIN mvke AS v ON v~matnr = m~matnr
                                         AND v~vkorg = @p_vkorg
       FOR ALL ENTRIES IN @lt_matnr_key
     WHERE m~matnr EQ @lt_matnr_key-matnr.
    SORT lt_matnr_inf BY matnr vtweg.
  ENDIF.
  "----// get price
  SELECT a~vtweg,
         a~kunnr,
         a~matnr,
         a~datab,
         k~kbetr,
         k~konwa,
         k~kpein,
         k~kmein
    INTO CORRESPONDING FIELDS OF TABLE @lt_price_inf
    FROM a305 AS a INNER JOIN konp AS k ON k~knumh = a~knumh
                                       AND k~kopos = '01'
   WHERE a~kappl    EQ 'V'
     AND a~kschl    EQ 'PR00'
     AND a~vkorg    EQ @p_vkorg
     AND a~datab    LE @sy-datlo
     AND a~datbi    GE @sy-datlo
     AND k~loevm_ko EQ @space.
  SORT lt_price_inf BY vtweg kunnr matnr datab DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_price_inf COMPARING vtweg kunnr matnr.
  "----// fill ALV data
  LOOP AT pt_main_m INTO wa_main_m.
    lv_idx = sy-tabix.
    "----// price
    READ TABLE lt_price_inf WITH KEY vtweg = wa_main_m-vtweg
                                     kunnr = wa_main_m-kunag
                                     matnr = wa_main_m-matnr
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-planp = lt_price_inf-kbetr.  "계획판가
      wa_main_m-kbetr = lt_price_inf-kbetr.  "판가
      wa_main_m-konwa = lt_price_inf-konwa.  "통화키
      wa_main_m-kpein = lt_price_inf-kpein.  "금액단위
      wa_main_m-kmein = lt_price_inf-kmein.  "수량단위
    ELSE.
      wa_main_m-planp = 0.                   "계획판가
      wa_main_m-kbetr = 0.
      wa_main_m-konwa = pv_stwae.
      wa_main_m-kpein = 1.
      wa_main_m-kmein = pv_basme.
*      IF wa_main_m-status NE '1'.
*        wa_main_m-retxt  = TEXT-m41.     "Sales price is not registeded.
*        wa_main_m-status = '1'.          "red
*      ENDIF.
    ENDIF.
    "----// channel text
    READ TABLE lt_vtweg_inf WITH KEY vtweg = wa_main_m-vtweg BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-vtext = lt_vtweg_inf-vtext.
    ENDIF.
    "----// sold to text
    READ TABLE lt_kunnr_inf WITH KEY kunnr = wa_main_m-kunag BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-name1 = lt_kunnr_inf-name1.
    ENDIF.
    "----// ship to text
    READ TABLE lt_kunnr_inf WITH KEY kunnr = wa_main_m-kunwe BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-name2 = lt_kunnr_inf-name1.
    ENDIF.
    "----// plant text
    READ TABLE lt_werks_inf WITH KEY werks = wa_main_m-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-name3 = lt_werks_inf-name1.
    ENDIF.
    "----// material text
    READ TABLE lt_matnr_inf WITH KEY matnr = wa_main_m-matnr
                                     vtweg = wa_main_m-vtweg
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-maktx = lt_matnr_inf-maktx.
      IF lt_matnr_inf-vrkme IS NOT INITIAL.
        wa_main_m-vrkme = lt_matnr_inf-vrkme.
      ELSE.
        wa_main_m-vrkme = lt_matnr_inf-meins.
      ENDIF.
    ENDIF.
    "----// net value
    IF wa_main_m-vrkme EQ wa_main_m-kmein.
      lv_menge = wa_main_m-kpein.
      lv_price = wa_main_m-kbetr / lv_menge.
    ELSE.
      lv_kpein = wa_main_m-kpein.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = wa_main_m-matnr
          i_in_me              = wa_main_m-kmein
          i_out_me             = wa_main_m-vrkme
          i_menge              = lv_kpein
        IMPORTING
          e_menge              = lv_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        lv_menge = 1.
      ENDIF.
      lv_price = wa_main_m-kbetr / lv_menge.
    ENDIF.
    wa_main_m-netwr_m00 = wa_main_m-kwmeng_m00 * lv_price.
    wa_main_m-netwr_m01 = wa_main_m-kwmeng_m01 * lv_price.
    wa_main_m-netwr_m02 = wa_main_m-kwmeng_m02 * lv_price.
    wa_main_m-netwr_m03 = wa_main_m-kwmeng_m03 * lv_price.
    wa_main_m-netwr_m04 = wa_main_m-kwmeng_m04 * lv_price.
    wa_main_m-netwr_m05 = wa_main_m-kwmeng_m05 * lv_price.
    wa_main_m-netwr_m06 = wa_main_m-kwmeng_m06 * lv_price.
    wa_main_m-netwr_m07 = wa_main_m-kwmeng_m07 * lv_price.
    wa_main_m-netwr_m08 = wa_main_m-kwmeng_m08 * lv_price.
    wa_main_m-netwr_m09 = wa_main_m-kwmeng_m09 * lv_price.
    wa_main_m-netwr_m10 = wa_main_m-kwmeng_m10 * lv_price.
    wa_main_m-netwr_m11 = wa_main_m-kwmeng_m11 * lv_price.
    wa_main_m-waerk = wa_main_m-konwa.
    "----// cell style
    REFRESH lt_cellstyl.
    CLEAR wa_main_m-cellstyl.
    PERFORM fill_cellstyl_m CHANGING lt_cellstyl.
    INSERT LINES OF lt_cellstyl INTO TABLE wa_main_m-cellstyl.
    MODIFY pt_main_m FROM wa_main_m INDEX lv_idx.
  ENDLOOP.
  CHECK 1 = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_VARIANT
*&---------------------------------------------------------------------*
*& ALV Variant
*&---------------------------------------------------------------------*
*&      <-- GS_VARIANT_0100 ALV 그리드 Variant
*&---------------------------------------------------------------------*
FORM alv_variant CHANGING ps_variant LIKE gs_variant_0100.
  CLEAR ps_variant.
  ps_variant-report   = sy-repid.
  ps_variant-username = sy-uname.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_LAYOUT
*&---------------------------------------------------------------------*
*& ALV Layout
*&---------------------------------------------------------------------*
*&      <-- GS_LAYOUT_0100 ALV 그리드 옵션
*&---------------------------------------------------------------------*
FORM alv_layout CHANGING ps_layout LIKE gs_layout_0100.
  CLEAR ps_layout.
  ps_layout-zebra      = 'X'.        "LINE COLOR
  ps_layout-cwidth_opt = ' '.        "ALV 제어: 열너비최적화
  ps_layout-no_rowmark = ' '.        "행선택 가능
  ps_layout-info_fname = 'INFO'.     "ROW COLOR.
  ps_layout-ctab_fname = 'CELLSCOL'. "CELL COLOR.
  ps_layout-stylefname = 'CELLSTYL'. "CELL STYLE
  ps_layout-sel_mode   = 'D'.
  ps_layout-edit       = ' '.        "편집가능
  IF sy-dynnr = '0100'.
    ps_layout-excp_fname = 'STATUS'.   "신호등 컬럼 필드'
  ELSE.
    ps_layout-excp_fname = ' '.
  ENDIF.
  ps_layout-excp_led   = ' '.        "'X' = display LED, else traffic lights
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_TOOLBAR_EXCLUDE
*&---------------------------------------------------------------------*
*& ALV 툴바 제외 버튼
*&---------------------------------------------------------------------*
*&      <-- GT_EXCLUDE_0100 제외 버튼 목록
*&---------------------------------------------------------------------*
FORM alv_toolbar_exclude TABLES pt_exclude LIKE gt_exclude_0100.
  DATA ls_exclude TYPE ui_func.
  CLEAR: ls_exclude, pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.           "Undo
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.           "Local: Copy
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.       "Local: Copy Row
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.            "Local: Cut
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.     "Local: Delete Row
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.     "Local: Append Row
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.     "Local: Insert Row
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.          "Local: Paste
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.  "Locally: Paste new Row
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.            "Refresh
  APPEND ls_exclude TO pt_exclude.

*  IF sy-dynnr EQ '0100'.
*    CHECK 1 = 1.
*  ELSE.
*    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.     "Local: Delete Row
*    APPEND ls_exclude TO pt_exclude.
*    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.     "Local: Append Row
*    APPEND ls_exclude TO pt_exclude.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_FIELDCATALOG
*&---------------------------------------------------------------------*
*& ALV 필드카탈로그
*&---------------------------------------------------------------------*
*&      <-- GT_FCAT_0100 필드카탈로그
*&---------------------------------------------------------------------*
FORM alv_fieldcatalog TABLES pt_fcat LIKE gt_fcat_0100.
  DATA: lt_fieldcatalog TYPE lvc_t_fcat,
        ls_fieldcatalog LIKE LINE OF lt_fieldcatalog.
  CLEAR pt_fcat.
  IF p_mont EQ 'X'.
    IF sy-dynnr EQ '0100'.
      PERFORM append_catalog TABLES pt_fcat USING:
        ' '  ' '  'RETXT'       TEXT-c01  'BAPIRET2'   'MESSAGE'      '     '  '     '  '10'  'L',
        "' '  ' '  'VKORG'       TEXT-c02  'TVKO'       'VKORG  '      '     '  '     '  '10'  'L',
        ' '  ' '  'VTWEG'       TEXT-c03  'TVTW'       'VTWEG'        '     '  '     '  '06'  'L',
        ' '  ' '  'VTEXT'       TEXT-c04  'TVTWT'      'VTEXT'        '     '  '     '  '10'  'L',
        ' '  ' '  'KUNAG'       TEXT-c05  'S804'       'KUNNR'        '     '  '     '  '06'  'L',
        ' '  ' '  'NAME1'       TEXT-c06  'KNA1'       'NAME1'        '     '  '     '  '10'  'L',
        ' '  ' '  'KUNWE'       TEXT-c07  'S804'       'PKUNWE'       '     '  '     '  '06'  'L',
        ' '  ' '  'NAME2'       TEXT-c08  'KNA1'       'NAME1'        '     '  '     '  '10'  'L',
        ' '  ' '  'WERKS'       TEXT-c09  'S804'       'WERKS'        '     '  '     '  '10'  'L',
        ' '  ' '  'NAME3'       TEXT-c10  'T001W'      'NAME1'        '     '  '     '  '10'  'L',
        ' '  ' '  'MATNR'       TEXT-c11  'MVKE'       'MATNR'        '     '  '     '  '10'  'L',
        ' '  ' '  'MAKTX'       TEXT-c12  'MAKT'       'MAKTX'        '     '  '     '  '20'  'L',
        ' '  ' '  'VKAUS'       TEXT-c13  'S804'       'VKAUS'        '     '  '     '  '06'  'L',
        ' '  ' '  'PLANP'       TEXT-c31  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R',
        ' '  ' '  'KBETR'       TEXT-c14  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R',
        ' '  ' '  'KONWA'       TEXT-c15  'KONP'       'KONWA'        '     '  '     '  '06'  'L',
        ' '  ' '  'KPEIN'       TEXT-c16  'KONP'       'KPEIN'        '     '  '     '  '06'  'R',
        ' '  ' '  'KMEIN'       TEXT-c17  'KONP'       'KMEIN'        'KMEIN'  '     '  '06'  'L',
        ' '  ' '  'KWMENG_W00'  TEXT-w00  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W00'   TEXT-n00  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W01'  TEXT-w01  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W01'   TEXT-n01  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W02'  TEXT-w02  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W02'   TEXT-n02  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W03'  TEXT-w03  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W03'   TEXT-n03  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W04'  TEXT-w04  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W04'   TEXT-n04  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W05'  TEXT-w05  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W05'   TEXT-n05  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W06'  TEXT-w06  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W06'   TEXT-n06  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W07'  TEXT-w07  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W07'   TEXT-n07  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W08'  TEXT-w08  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W08'   TEXT-n08  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W09'  TEXT-w09  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W09'   TEXT-n09  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W10'  TEXT-w10  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W10'   TEXT-n10  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W11'  TEXT-w11  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W11'   TEXT-n11  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W12'  TEXT-w12  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W12'   TEXT-n12  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W13'  TEXT-w13  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W13'   TEXT-n13  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W14'  TEXT-w14  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W14'   TEXT-n14  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_W15'  TEXT-w15  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_W15'   TEXT-n15  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'WAERK'       TEXT-c20  'S804'       'WAERK'        '     '  '     '  '05'  'L',
        ' '  ' '  'VRKME'       TEXT-c20  'S804'       'VRKME'        '     '  '     '  '05'  'L'.
    ELSE.
      PERFORM append_catalog TABLES pt_fcat USING:
        'X'  'X'  'MATNR'       TEXT-c11  'MVKE'       'MATNR'        '     '  '     '  '10'  'L',
        ' '  ' '  'PLNMG_W00'   TEXT-w00  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W01'   TEXT-w01  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W02'   TEXT-w02  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W03'   TEXT-w03  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W04'   TEXT-w04  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W05'   TEXT-w05  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W06'   TEXT-w06  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W07'   TEXT-w07  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W08'   TEXT-w08  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W09'   TEXT-w09  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W10'   TEXT-w10  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W11'   TEXT-w11  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W12'   TEXT-w12  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W13'   TEXT-w13  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W14'   TEXT-w14  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_W15'   TEXT-w15  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'MEINS'       TEXT-c20  'PBED'       'MEINS'        '     '  '     '  '05'  'L'.
    ENDIF.
  ELSE.
    IF sy-dynnr EQ '0100'.
      PERFORM append_catalog TABLES pt_fcat USING:
        ' '  ' '  'RETXT'       TEXT-c01  'BAPIRET2'   'MESSAGE'      '     '  '     '  '10'  'L',
        "' '  ' '  'VKORG'       TEXT-c02  'TVKO'       'VKORG  '      '     '  '     '  '10'  'L',
        ' '  ' '  'VTWEG'       TEXT-c03  'TVTW'       'VTWEG'        '     '  '     '  '06'  'L',
        ' '  ' '  'VTEXT'       TEXT-c04  'TVTWT'      'VTEXT'        '     '  '     '  '10'  'L',
        ' '  ' '  'KUNAG'       TEXT-c05  'S804'       'KUNNR'        '     '  '     '  '06'  'L',
        ' '  ' '  'NAME1'       TEXT-c06  'KNA1'       'NAME1'        '     '  '     '  '10'  'L',
        ' '  ' '  'KUNWE'       TEXT-c07  'S804'       'PKUNWE'       '     '  '     '  '06'  'L',
        ' '  ' '  'NAME2'       TEXT-c08  'KNA1'       'NAME1'        '     '  '     '  '10'  'L',
        ' '  ' '  'WERKS'       TEXT-c09  'S804'       'WERKS'        '     '  '     '  '10'  'L',
        ' '  ' '  'NAME3'       TEXT-c10  'T001W'      'NAME1'        '     '  '     '  '10'  'L',
        ' '  ' '  'MATNR'       TEXT-c11  'S804'       'MATNR'        '     '  '     '  '10'  'L',
        ' '  ' '  'MAKTX'       TEXT-c12  'MAKT'       'MAKTX'        '     '  '     '  '20'  'L',
        ' '  ' '  'VKAUS'       TEXT-c13  'S804'       'VKAUS'        '     '  '     '  '06'  'L',
        ' '  ' '  'PLANP'       TEXT-c31  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R',
        ' '  ' '  'KBETR'       TEXT-c14  'KONP'       'KBETR'        '     '  'KONWA'  '06'  'R',
        ' '  ' '  'KONWA'       TEXT-c15  'KONP'       'KONWA'        '     '  '     '  '06'  'L',
        ' '  ' '  'KPEIN'       TEXT-c16  'KONP'       'KPEIN'        '     '  '     '  '06'  'R',
        ' '  ' '  'KMEIN'       TEXT-c17  'KONP'       'KMEIN'        'KMEIN'  '     '  '06'  'L',
        ' '  ' '  'KWMENG_M00'  TEXT-m00  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M00'   TEXT-a00  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M01'  TEXT-m01  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M01'   TEXT-a01  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M02'  TEXT-m02  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M02'   TEXT-a02  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M03'  TEXT-m03  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M03'   TEXT-a03  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M04'  TEXT-m04  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M04'   TEXT-a04  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M05'  TEXT-m05  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M05'   TEXT-a05  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M06'  TEXT-m06  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M06'   TEXT-a06  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M07'  TEXT-m07  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M07'   TEXT-a07  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M08'  TEXT-m08  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M08'   TEXT-a08  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M09'  TEXT-m09  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M09'   TEXT-a09  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M10'  TEXT-m10  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M10'   TEXT-a10  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'KWMENG_M11'  TEXT-m11  'S804'       'KWMENG'       'VRKME'  '     '  '10'  'R',
        ' '  ' '  'NETWR_M11'   TEXT-a11  'S804'       'NETWR'        '     '  'WAERK'  '10'  'R',
        ' '  ' '  'WAERK'       TEXT-c20  'S804'       'WAERK'        '     '  '     '  '05'  'L',
        ' '  ' '  'VRKME'       TEXT-c20  'S804'       'VRKME'        '     '  '     '  '05'  'L'.
    ELSE.
      PERFORM append_catalog TABLES pt_fcat USING:
        'X'  'X'  'MATNR'       TEXT-c11  'MVKE'       'MATNR'        '     '  '     '  '10'  'L',
        ' '  ' '  'PLNMG_M00'   TEXT-m00  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M01'   TEXT-m01  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M02'   TEXT-m02  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M03'   TEXT-m03  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M04'   TEXT-m04  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M05'   TEXT-m05  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M06'   TEXT-m06  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M07'   TEXT-m07  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M08'   TEXT-m08  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M09'   TEXT-m09  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M10'   TEXT-m10  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'PLNMG_M11'   TEXT-m11  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
        ' '  ' '  'MEINS'       TEXT-c20  'PBED'       'MEINS'        '     '  '     '  '05'  'L'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_CATALOG
*&---------------------------------------------------------------------*
*& 필드카탈로그 칼럼 속성 정의
*&---------------------------------------------------------------------*
*&      <-> PT_FCAT_0100 필드카탈로그 목록
*&      --> P_KEY        ALV 키 - 색상이 헤더스타일
*&      --> P_FIX_COLUMN 고정칼럼 - 가로스크롤바 나오더라도 고정
*&      --> P_FIELDNAME  컬럼명
*&      --> P_COLTEXT    컬럼텍스트
*&      --> P_REF_TABLE  참조테이블
*&      --> P_REF_FIELD  참조필드
*&      --> P_QFIELDNAME 수량단위
*&      --> P_CFIELDNAME 화폐단위
*&      --> P_OUTPUTLEN  필드길이
*&      --> P_JUST       정렬(좌,중,우)
*&---------------------------------------------------------------------*
FORM append_catalog TABLES pt_fcat LIKE gt_fcat_0100
                    USING  "VALUE(P_COL_POS)
                           VALUE(p_key)
                           VALUE(p_fix_column)
                           VALUE(p_fieldname)
                           VALUE(p_coltext)
                           VALUE(p_ref_table)
                           VALUE(p_ref_field)
                           VALUE(p_qfieldname)
                           VALUE(p_cfieldname)
                           VALUE(p_outputlen)
                           VALUE(p_just).
  DATA: ls_fieldcat TYPE lvc_s_fcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-col_pos    = lines( pt_fcat[] ) + 1.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-fix_column = p_fix_column.
  ls_fieldcat-fieldname  = p_fieldname.
  ls_fieldcat-reptext    = p_coltext.
  ls_fieldcat-coltext    = p_coltext.
  ls_fieldcat-scrtext_l  = p_coltext.
  ls_fieldcat-scrtext_m  = p_coltext.
  ls_fieldcat-scrtext_s  = p_coltext.
  ls_fieldcat-ref_table  = p_ref_table.
  ls_fieldcat-ref_field  = p_ref_field.
  ls_fieldcat-qfieldname = p_qfieldname.
  ls_fieldcat-cfieldname = p_cfieldname.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-just       = p_just.
  IF sy-dynnr EQ '0100'.
    IF p_fieldname(5) EQ 'KWMEN' OR
       p_fieldname(5) EQ 'VTWEG' OR
       p_fieldname(5) EQ 'KUNAG' OR
       p_fieldname(5) EQ 'KUNWE' OR
       p_fieldname(5) EQ 'WERKS' OR
       p_fieldname(5) EQ 'MATNR' OR
       p_fieldname(5) EQ 'VKAUS' OR
       p_fieldname(5) EQ 'KBETR'.
      ls_fieldcat-edit = 'X'.
    ENDIF.
  ELSE.
    CHECK 1 = 1.
  ENDIF.
  APPEND ls_fieldcat TO pt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_CELLSTYL_W
*&---------------------------------------------------------------------*
*& 셀별 편집모드
*&---------------------------------------------------------------------*
*&      --> P_MODE
*&      <-- LT_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_cellstyl_w CHANGING pt_cellstyl TYPE lvc_t_styl.
  DATA: ls_cellstyl TYPE lvc_s_styl,
        lv_mode     TYPE raw4,
        lv_idx      TYPE numc2.
  REFRESH pt_cellstyl.
  CLEAR ls_cellstyl.
*  IF p_mode EQ 'RW'.
*    lv_mode = cl_gui_alv_grid=>mc_style_enabled.    "to enable the required fields
*  ELSE.                                "p_mode eq 'RO'
*    lv_mode = cl_gui_alv_grid=>mc_style_disabled.
*  ENDIF.
  ls_cellstyl-fieldname = 'VKORG'.  "channel
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'VTWEG'.  "channel
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'VTEXT'.  "channel text
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KUNAG'.  "sold-to party
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'NAME1'.  "sold-to party name
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KUNWE'.  "ship-to party
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'NAME2'.  "ship-to party name
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'WERKS'.  "plant
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'NAME3'.  "plant name
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'MATNR'.  "material
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'MAKTX'.  "material code
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'VKAUS'.  "usage
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'PLANP'.  "plan price
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_enabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KBETR'.  "unit price
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KONWA'.  "unit price currency key
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KPEIN'.  "unit price qty
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KMEIN'.  "unit price uom
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'VRKME'.  "uom
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'WAERK'.  "currency key
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'RETXT'.  "return text after processing
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  DO 16 TIMES.
    lv_idx = sy-index - 1.
    CONCATENATE 'KWMENG_W' lv_idx INTO ls_cellstyl-fieldname.  "수량
    ls_cellstyl-style = cl_gui_alv_grid=>mc_style_enabled.
    INSERT ls_cellstyl INTO TABLE pt_cellstyl.
    CONCATENATE 'NETWR_W' lv_idx INTO ls_cellstyl-fieldname.   "금액
    ls_cellstyl-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_CELLSTYL_M
*&---------------------------------------------------------------------*
*& 셀별 편집모드
*&---------------------------------------------------------------------*
*&      --> P_MODE
*&      <-- LT_CELLTAB
*&---------------------------------------------------------------------*
FORM fill_cellstyl_m CHANGING pt_cellstyl TYPE lvc_t_styl.
  DATA: ls_cellstyl TYPE lvc_s_styl,
        lv_mode     TYPE raw4,
        lv_idx      TYPE numc2.
  REFRESH pt_cellstyl.
  CLEAR ls_cellstyl.
*  IF p_mode EQ 'RW'.
*    lv_mode = cl_gui_alv_grid=>mc_style_enabled.    "to enable the required fields
*  ELSE.                                "p_mode eq 'RO'
*    lv_mode = cl_gui_alv_grid=>mc_style_disabled.
*  ENDIF.
  ls_cellstyl-fieldname = 'VKORG'.  "channel
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'VTWEG'.  "channel
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'VTEXT'.  "channel text
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KUNAG'.  "sold-to party
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'NAME1'.  "sold-to party name
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KUNWE'.  "ship-to party
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'NAME2'.  "ship-to party name
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'WERKS'.  "plant
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'NAME3'.  "plant name
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'MATNR'.  "material
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'MAKTX'.  "material code
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'VKAUS'.  "usage
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'PLANP'.  "plan price
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_enabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KBETR'.  "unit price
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KONWA'.  "unit price currency key
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KPEIN'.  "unit price qty
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'KMEIN'.  "unit price uom
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'VRKME'.  "uom
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'WAERK'.  "currency key
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ls_cellstyl-fieldname = 'RETXT'.  "return text after processing
  ls_cellstyl-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  DO 12 TIMES.
    lv_idx = sy-index - 1.
    CONCATENATE 'KWMENG_M' lv_idx INTO ls_cellstyl-fieldname.  "수량
    ls_cellstyl-style = cl_gui_alv_grid=>mc_style_enabled.
    INSERT ls_cellstyl INTO TABLE pt_cellstyl.
    CONCATENATE 'NETWR_M' lv_idx INTO ls_cellstyl-fieldname.   "금액
    ls_cellstyl-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_cellstyl INTO TABLE pt_cellstyl.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_OBJECT
*&      --> E_INTERACTIVE
*&---------------------------------------------------------------------*
FORM handle_toolbar USING po_object      TYPE REF TO cl_alv_event_toolbar_set
                          pv_interactive TYPE char01.

  DATA : ls_toolbar TYPE stb_button,
         lv_tabix   TYPE sy-tabix.
  IF sy-dynnr EQ '0100'.
*    "----// 추가 버튼 찾기
*    CLEAR: ls_toolbar.
*    READ TABLE po_object->mt_toolbar INTO ls_toolbar WITH KEY function = cl_gui_alv_grid=>mc_fc_loc_append_row.
*    lv_tabix = sy-tabix.
*    ls_toolbar-text = TEXT-t04.
*    MODIFY po_object->mt_toolbar FROM ls_toolbar INDEX lv_tabix.
*    "----// 삭제 버튼 찾기
*    CLEAR: ls_toolbar.
*    READ TABLE po_object->mt_toolbar INTO ls_toolbar WITH KEY function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    lv_tabix = sy-tabix.
*    ls_toolbar-text = TEXT-t03.
*    MODIFY po_object->mt_toolbar FROM ls_toolbar INDEX lv_tabix.

    "----// 저장 버튼 만들기
    lv_tabix = lv_tabix + 1.
    CLEAR: ls_toolbar.
    ls_toolbar-function  = TEXT-t01.
    ls_toolbar-icon      = icon_system_save.
    ls_toolbar-quickinfo = TEXT-t02.
    ls_toolbar-text      = TEXT-t02.
    ls_toolbar-disabled  = ' '.
    INSERT ls_toolbar INTO po_object->mt_toolbar INDEX lv_tabix.
    IF p_data EQ 'X'.
      "----// DM전송 버튼 만들기
      lv_tabix = lv_tabix + 1.
      CLEAR: ls_toolbar.
      ls_toolbar-function  = TEXT-t05.
      ls_toolbar-icon      = icon_ws_transfer.
      ls_toolbar-quickinfo = TEXT-t06.
      ls_toolbar-text      = TEXT-t06.
      ls_toolbar-disabled  = ' '.
      INSERT ls_toolbar INTO po_object->mt_toolbar INDEX lv_tabix.
      "----// DM조회 버튼 만들기
      lv_tabix = sy-tabix + 3.
      CLEAR: ls_toolbar.
      ls_toolbar-function  = TEXT-t07.
      ls_toolbar-icon      = icon_display.
      ls_toolbar-quickinfo = TEXT-t08.
      ls_toolbar-text      = TEXT-t08.
      ls_toolbar-disabled  = ' '.
      INSERT ls_toolbar INTO po_object->mt_toolbar INDEX lv_tabix.
    ENDIF.
  ELSE.
    CHECK 1 = 1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& 툴바 버튼 기능 구현
*&---------------------------------------------------------------------*
*&      --> E_UCOMM 클릭한 툴바 버튼
*&---------------------------------------------------------------------*
FORM handle_user_command  USING pv_ucomm TYPE sy-ucomm.
  DATA: lt_main_w TYPE TABLE OF ty_main_w,
        lt_main_m TYPE TABLE OF ty_main_m.
  DATA: lt_selected TYPE lvc_t_row WITH HEADER LINE,
        lv_retcd    TYPE char01,
        lv_retxt    TYPE text100.
*  CLEAR: lt_selected, lv_retcd, lv_retxt.
*  CALL METHOD go_grid_0100->get_selected_rows
*    IMPORTING
*      et_index_rows = lt_selected[].
*  IF lt_selected[] IS INITIAL.
*    MESSAGE s004.
*    EXIT.
*  ENDIF.
  IF p_mont EQ 'X'.
    CASE pv_ucomm.
      WHEN 'SAVE'.  "----// 영업계획 저장
        CLEAR: lt_selected, lv_retcd, lv_retxt.
        CALL METHOD go_grid_0100->get_selected_rows
          IMPORTING
            et_index_rows = lt_selected[].
        IF lt_selected[] IS INITIAL.
          MESSAGE s004.
          EXIT.
        ENDIF.
        PERFORM pop_up USING    TEXT-022  TEXT-020
                       CHANGING gv_answer.
        IF gv_answer = '1'.
          PERFORM save_monthly_data_w TABLES   gt_main_w  lt_selected
                                      USING    gv_spmon
                                      CHANGING lv_retcd   lv_retxt     gv_chged.
          IF lv_retcd EQ 'E'.
            CHECK 1 = 1.
          ELSE.
            MESSAGE s051(ma) INTO lv_retxt WITH p_vrsio.
          ENDIF.
          MESSAGE lv_retxt TYPE 'S' DISPLAY LIKE lv_retcd.
        ELSE.
          CHECK 1 = 1.
        ENDIF.
      WHEN 'DMTF'.  "----// DM 전송
        IF gv_chged EQ ' '.
          PERFORM pop_up USING    TEXT-022  TEXT-019
                         CHANGING gv_answer.
          IF gv_answer = '1'.
            PERFORM select_monthly_data_w TABLES lt_main_w
                                           USING gv_spmon  gv_stwae   gv_basme   'ALL'.
            PERFORM dmtf_monthly_data_w   TABLES lt_main_w  lt_selected
                                        CHANGING lv_retcd   lv_retxt.
            IF lv_retcd EQ 'E'.
              CHECK 1 = 1.
            ELSE.
              MESSAGE s311(6p) INTO lv_retxt.
            ENDIF.
            MESSAGE lv_retxt TYPE 'S' DISPLAY LIKE lv_retcd.
          ELSE.
            CHECK 1 = 1.
          ENDIF.
        ELSE.
          MESSAGE s006.
        ENDIF.
      WHEN 'DMDP'.  "----// DM 조회
        PERFORM get_ireq_w TABLES gt_ireq_w.
      WHEN OTHERS.
        CHECK 1 = 1.
    ENDCASE.
  ELSE.
    CASE pv_ucomm.
      WHEN 'SAVE'.
        CLEAR: lt_selected, lv_retcd, lv_retxt.
        CALL METHOD go_grid_0100->get_selected_rows
          IMPORTING
            et_index_rows = lt_selected[].
        IF lt_selected[] IS INITIAL.
          MESSAGE s004.
          EXIT.
        ENDIF.
        PERFORM pop_up USING    TEXT-022  TEXT-020
                       CHANGING gv_answer.
        IF gv_answer = '1'.
          PERFORM save_yearly_data_m TABLES   gt_main_m  lt_selected
                                     USING    gv_spmon
                                     CHANGING lv_retcd   lv_retxt     gv_chged.
          IF lv_retcd EQ 'E'.
            CHECK 1 = 1.
          ELSE.
            MESSAGE s051(ma) INTO lv_retxt WITH p_vrsio.
          ENDIF.
          MESSAGE lv_retxt TYPE 'S' DISPLAY LIKE lv_retcd.
        ELSE.
          CHECK 1 = 1.
        ENDIF.
      WHEN 'DMTF'.
        IF gv_chged EQ ' '.
          PERFORM pop_up USING    TEXT-022  TEXT-019
                         CHANGING gv_answer.
          IF gv_answer = '1'.
            PERFORM select_yearly_data_m  TABLES lt_main_m
                                          USING  gv_spmon   gv_stwae   gv_basme   'ALL'.
            PERFORM dmtf_yearly_data_m    TABLES lt_main_m  lt_selected
                                        CHANGING lv_retcd   lv_retxt.
            IF lv_retcd EQ 'E'.
              CHECK 1 = 1.
            ELSE.
              MESSAGE s311(6p) INTO lv_retxt.
            ENDIF.
            MESSAGE lv_retxt TYPE 'S' DISPLAY LIKE lv_retcd.
          ELSE.
            CHECK 1 = 1.
          ENDIF.
        ELSE.
          MESSAGE s006.
        ENDIF.
      WHEN 'DMDP'.
        PERFORM get_ireq_m TABLES gt_ireq_m.
      WHEN OTHERS.
        CHECK 1 = 1.
    ENDCASE.
  ENDIF.
  CALL METHOD go_grid_0100->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_MONTHLY_DATA_W
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN_W
*&---------------------------------------------------------------------*
FORM save_monthly_data_w TABLES   pt_main_w   LIKE gt_main_w
                                  pt_selected TYPE lvc_t_row
                         USING    pv_spmon    TYPE spmon
                         CHANGING pv_retcd    TYPE char01
                                  pv_retxt    TYPE text100
                                  pv_chged    TYPE c.
  DATA: lt_s804   TYPE TABLE OF s804 WITH HEADER LINE.
  DATA: wa_main_w TYPE ty_main_w,
        ls_s804e  TYPE s804e,
        lv_tabix  TYPE sy-tabix,
        lv_ident  TYPE tfacd-ident,
        lv_hocid  TYPE tfacd-hocid,
        lv_date   TYPE scal-date,
        lv_week00 TYPE scal-week,
        lv_week   TYPE scal-week,
        lv_mon    TYPE numc2,
        lv_fnm    TYPE char40.     "필드명
  DATA: lt_holidays TYPE TABLE OF iscal_day WITH HEADER LINE.
  FIELD-SYMBOLS: <fs_kwmeng>,      "수량
                 <fs_netwr>.       "금액
  "----// BDC
  DATA: lt_bdcdata TYPE TABLE OF bdcdata    WITH HEADER LINE.
  DATA: lt_messtab TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.
  DATA: ls_opt     TYPE ctu_params.
  CLEAR ls_opt.
  ls_opt-dismode = 'N'.
  ls_opt-updmode = 'S'.
  ls_opt-defsize = 'X'.
*  "///////////////////////////////////////////////////////////////////
*  "////
*  "//// 삭제
*  "////
*  "///////////////////////////////////////////////////////////////////
*  IF gt_del_w[] IS NOT INITIAL.
*    SELECT  *
*      INTO CORRESPONDING FIELDS OF TABLE lt_s804
*      FROM s804
*      FOR ALL ENTRIES IN gt_del_w
*     WHERE vrsio  EQ p_vrsio
*       AND vkorg  EQ gt_del_w-vkorg
*       AND vtweg  EQ gt_del_w-vtweg
*       AND kunnr  EQ gt_del_w-kunag
*       AND pkunwe EQ gt_del_w-kunwe
*       AND werks  EQ gt_del_w-werks
*       AND matnr  EQ gt_del_w-matnr
*       AND vkaus  EQ gt_del_w-vkaus.
*    DELETE s804 FROM TABLE lt_s804.
*    CLEAR: lt_s804[], gt_del_w[].
*  ENDIF.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Planning Hierarchy 생성
  "////
  "///////////////////////////////////////////////////////////////////
  LOOP AT pt_selected.
    READ TABLE pt_main_w INTO wa_main_w INDEX pt_selected-index.
    "----// 계획계층구조 이미 생성되어 있는지 확인
    SELECT SINGLE
           *
      INTO CORRESPONDING FIELDS OF @ls_s804e
      FROM s804e
     WHERE ssour  EQ @space
       AND vkorg  EQ @p_vkorg
       AND vtweg  EQ @wa_main_w-vtweg
       AND kunnr  EQ @wa_main_w-kunag
       AND pkunwe EQ @wa_main_w-kunag
       AND werks  EQ @wa_main_w-werks
       AND matnr  EQ @wa_main_w-matnr
       AND vkaus  EQ @wa_main_w-vkaus.
    IF sy-subrc EQ 0.
      CONTINUE.
    ELSE.
      CLEAR: lt_bdcdata[], lt_bdcdata,
             lt_messtab[], lt_messtab.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPMMCP3'              '0159'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RMCP2-MCINF'           'S804'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RMCP2-RADIO_PLANABLE'  ' '.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RMCP2-RADIO_ADD'       'X'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RMCP2-RADIO_DELETE'    ' '.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '=CONT'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'RMCA804H'              '1000'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0001-LOW'           p_vkorg.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0002-LOW'           wa_main_w-vtweg.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0003-LOW'           wa_main_w-kunag.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0004-LOW'           wa_main_w-kunwe.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0005-LOW'           wa_main_w-werks.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0006-LOW'           wa_main_w-matnr.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0007-LOW'           wa_main_w-vkaus.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'FLG_SIM'               ' '.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '=ONLI'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPMSSY0'              '0120'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '=BACK'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'RMCA804H'              '1000'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '/EE'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPMMCP3'              '0159'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '/EBAC2'.
      CALL TRANSACTION 'MC59' WITH AUTHORITY-CHECK USING lt_bdcdata MESSAGES INTO lt_messtab OPTIONS FROM ls_opt.
      READ TABLE lt_messtab WITH KEY msgtyp = 'E'.
      IF sy-subrc NE 0.
        wa_main_w-retcd  = 'S'.
        wa_main_w-retxt  = ' '.
        wa_main_w-status = '3'.
        MODIFY pt_main_w FROM wa_main_w INDEX pt_selected-index
                                        TRANSPORTING status
                                                     retcd
                                                     retxt.
      ELSE.
        wa_main_w-retcd  = 'E'.
        MESSAGE ID     lt_messtab-msgid
                TYPE   lt_messtab-msgtyp
                NUMBER lt_messtab-msgnr
                INTO   wa_main_w-retxt
                WITH   lt_messtab-msgv1  lt_messtab-msgv2  lt_messtab-msgv3  lt_messtab-msgv4.
        wa_main_w-status = '1'.
        MODIFY pt_main_w FROM wa_main_w INDEX pt_selected-index
                                        TRANSPORTING status
                                                     retcd
                                                     retxt.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF line_exists( pt_main_w[ status = '1' ] ).
    pv_retcd = 'E'.
    pv_retxt = TEXT-t41.
    EXIT.
  ENDIF.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Sales Plan 생성
  "////
  "///////////////////////////////////////////////////////////////////
  CLEAR: lv_date, lv_ident, lv_hocid.
  CONCATENATE p_gjahr p_vrsio+1(2) '01' INTO lv_date.
  SELECT SINGLE
         land1
    INTO lv_ident
    FROM t001w
   WHERE werks EQ p_werks.
  SELECT SINGLE
         hocid
    INTO lv_hocid
    FROM tfacd
   WHERE ident EQ lv_ident.
  DO 100 TIMES.
    CLEAR lt_holidays[].
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar           = lv_ident
        factory_calendar           = lv_hocid
        date_from                  = lv_date
        date_to                    = lv_date
      TABLES
        holidays                   = lt_holidays
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.
    IF sy-subrc EQ 0.
      IF lt_holidays[] IS INITIAL.
        EXIT.
      ELSE.
        lv_date = lv_date + 1.
        CONTINUE.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = lv_date
    IMPORTING
      week         = lv_week00
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CHECK 1 = 1.
  ENDIF.
  CLEAR: lt_s804[], lt_s804.
  LOOP AT pt_selected.
    READ TABLE pt_main_w INTO wa_main_w INDEX pt_selected-index.
    "t_s804-ssour  = '    '.
    lt_s804-vrsio  = p_vrsio.
    lt_s804-spmon  = pv_spmon.
    "t_s804-sptag  =
    "t_s804-spbup  = '000000'.
    lt_s804-vkorg  = p_vkorg.
    lt_s804-vtweg  = wa_main_w-vtweg.
    lt_s804-kunnr  = wa_main_w-kunag.
    lt_s804-pkunwe = wa_main_w-kunwe.
    lt_s804-werks  = wa_main_w-werks.
    lt_s804-matnr  = wa_main_w-matnr.
    lt_s804-vkaus  = wa_main_w-vkaus.
    "t_s804-periv  = '  '.
    "t_s804-vwdat  = '0000000000'.
    lt_s804-vrkme  = wa_main_w-vrkme.
    lt_s804-waerk  = wa_main_w-waerk.
    lv_week = lv_week00.
    DO 16 TIMES.
      lt_s804-spwoc  = lv_week.
      lv_mon = sy-index - 1.
      "----// 수량
      CLEAR lv_fnm.
      CONCATENATE 'WA_MAIN_W-KWMENG_W' lv_mon INTO lv_fnm.
      ASSIGN (lv_fnm) TO <fs_kwmeng>.
      lt_s804-kwmeng = <fs_kwmeng>.
      "----// 금액
      CLEAR lv_fnm.
      CONCATENATE 'WA_MAIN_W-NETWR_W' lv_mon INTO lv_fnm.
      ASSIGN (lv_fnm) TO <fs_netwr>.
      lt_s804-netwr  = <fs_netwr>.
      APPEND lt_s804.
      IF lv_week+4(2) = '53'.
        lv_week+0(4) = p_gjahr + 1.
        lv_week+4(2) = '00'.
      ENDIF.
      lv_week = lv_week + 1.
    ENDDO.
    CLEAR lt_s804.
  ENDLOOP.
  IF p_file EQ 'X'.
    "----// 버전 전체 삭제 후 저장
    DELETE FROM s804 WHERE vrsio = p_vrsio.
    INSERT s804 FROM TABLE lt_s804.
  ELSE.
    "----// 변경
    MODIFY s804 FROM TABLE lt_s804.
  ENDIF.
  pv_retcd = 'S'.
  pv_retxt = ' '.
  pv_chged = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_YEARLY_DATA_M
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN_M
*&---------------------------------------------------------------------*
FORM save_yearly_data_m TABLES   pt_main_m   LIKE gt_main_m
                                 pt_selected TYPE lvc_t_row
                        USING    pv_spmon    TYPE spmon
                        CHANGING pv_retcd    TYPE char01
                                 pv_retxt    TYPE text100
                                 pv_chged    TYPE c.
  DATA: lt_s805   TYPE TABLE OF s805 WITH HEADER LINE.
  DATA: wa_main_m TYPE ty_main_m,
        ls_s805e  TYPE s805e,
        lv_tabix  TYPE sy-tabix,
        lv_mon00  TYPE spmon,
        lv_month  TYPE spmon,
        lv_mon    TYPE numc2,
        lv_fnm    TYPE char40.     "필드명
  FIELD-SYMBOLS: <fs_kwmeng>,      "수량
                 <fs_netwr>.       "금액
  "----// BDC
  DATA: lt_bdcdata TYPE TABLE OF bdcdata    WITH HEADER LINE.
  DATA: lt_messtab TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.
  DATA: ls_opt     TYPE ctu_params.
  CLEAR ls_opt.
  ls_opt-dismode = 'N'.
  ls_opt-updmode = 'S'.
  ls_opt-defsize = 'X'.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// 삭제
  "////
  "///////////////////////////////////////////////////////////////////
  IF gt_del_m[] IS NOT INITIAL.
    SELECT  *
      INTO CORRESPONDING FIELDS OF TABLE lt_s805
      FROM s805
      FOR ALL ENTRIES IN gt_del_m
     WHERE vrsio  EQ p_vrsio
       AND vkorg  EQ p_vkorg
       AND vtweg  EQ gt_del_m-vtweg
       AND kunnr  EQ gt_del_m-kunag
       AND pkunwe EQ gt_del_m-kunwe
       AND werks  EQ gt_del_m-werks
       AND matnr  EQ gt_del_m-matnr
       AND vkaus  EQ gt_del_m-vkaus.
    DELETE s805 FROM TABLE lt_s805.
    CLEAR: lt_s805[], gt_del_m[].
  ENDIF.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Planning Hierarchy 생성
  "////
  "///////////////////////////////////////////////////////////////////
  LOOP AT pt_selected.
    READ TABLE pt_main_m INTO wa_main_m INDEX pt_selected-index.
    "----// 계획계층구조 이미 생성되어 있는지 확인
    SELECT SINGLE
           *
      INTO CORRESPONDING FIELDS OF @ls_s805e
      FROM s805e
     WHERE ssour  EQ @space
       AND vkorg  EQ @p_vkorg
       AND vtweg  EQ @wa_main_m-vtweg
       AND kunnr  EQ @wa_main_m-kunag
       AND pkunwe EQ @wa_main_m-kunag
       AND werks  EQ @wa_main_m-werks
       AND matnr  EQ @wa_main_m-matnr
       AND vkaus  EQ @wa_main_m-vkaus.
    IF sy-subrc EQ 0.
      CONTINUE.
    ELSE.
      CLEAR: lt_bdcdata[], lt_bdcdata,
             lt_messtab[], lt_messtab.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPMMCP3'              '0159'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RMCP2-MCINF'           'S805'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RMCP2-RADIO_PLANABLE'  ' '.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RMCP2-RADIO_ADD'       'X'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RMCP2-RADIO_DELETE'    ' '.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '=CONT'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'RMCA805H'              '1000'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0001-LOW'           p_vkorg.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0002-LOW'           wa_main_m-vtweg.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0003-LOW'           wa_main_m-kunag.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0004-LOW'           wa_main_m-kunwe.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0005-LOW'           wa_main_m-werks.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0006-LOW'           wa_main_m-matnr.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'SL_0007-LOW'           wa_main_m-vkaus.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'FLG_SIM'               ' '.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '=ONLI'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPMSSY0'              '0120'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '=BACK'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'RMCA805H'              '1000'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '/EE'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPMMCP3'              '0159'.
      PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'            '/EBAC2'.
      CALL TRANSACTION 'MC59' WITH AUTHORITY-CHECK USING lt_bdcdata MESSAGES INTO lt_messtab OPTIONS FROM ls_opt.
      READ TABLE lt_messtab WITH KEY msgtyp = 'E'.
      IF sy-subrc NE 0.
        wa_main_m-retcd  = 'S'.
        wa_main_m-retxt  = ' '.
        wa_main_m-status = '3'.
        MODIFY pt_main_m FROM wa_main_m INDEX pt_selected-index
                                        TRANSPORTING status
                                                     retcd
                                                     retxt.
      ELSE.
        wa_main_m-retcd  = 'E'.
        MESSAGE ID     lt_messtab-msgid
                TYPE   lt_messtab-msgtyp
                NUMBER lt_messtab-msgnr
                INTO   wa_main_m-retxt
                WITH   lt_messtab-msgv1  lt_messtab-msgv2  lt_messtab-msgv3  lt_messtab-msgv4.
        wa_main_m-status = '1'.
        MODIFY pt_main_m FROM wa_main_m INDEX pt_selected-index
                                        TRANSPORTING status
                                                     retcd
                                                     retxt.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF line_exists( pt_main_m[ status = '1' ] ).
    pv_retcd = 'E'.
    pv_retxt = TEXT-t41.
    EXIT.
  ENDIF.
  "///////////////////////////////////////////////////////////////////
  "////
  "//// Sales Plan 생성
  "////
  "///////////////////////////////////////////////////////////////////
  CONCATENATE p_gjahr '01' INTO lv_mon00.
  CLEAR: lt_s805[], lt_s805.
  LOOP AT pt_selected.
    READ TABLE pt_main_m INTO wa_main_m INDEX pt_selected-index.
    "t_s805-ssour  = '    '.
    lt_s805-vrsio  = p_vrsio.
    "lt_s805-spmon  = pv_spmon.
    "t_s805-spwoc  = '      '.
    "t_s805-sptag  =
    "t_s805-spbup  = '000000'.
    lt_s805-vkorg  = p_vkorg.
    lt_s805-vtweg  = wa_main_m-vtweg.
    lt_s805-kunnr  = wa_main_m-kunag.
    lt_s805-pkunwe = wa_main_m-kunwe.
    lt_s805-werks  = wa_main_m-werks.
    lt_s805-matnr  = wa_main_m-matnr.
    lt_s805-vkaus  = wa_main_m-vkaus.
    "t_s805-periv  = '  '.
    "t_s805-vwdat  = '0000000000'.
    lt_s805-vrkme  = wa_main_m-vrkme.
    lt_s805-waerk  = wa_main_m-waerk.
    lv_month = lv_mon00.
    DO 12 TIMES.
      lt_s805-spmon = lv_month.
      lv_mon = sy-index - 1.
      "----// 수량
      CLEAR lv_fnm.
      CONCATENATE 'WA_MAIN_M-KWMENG_M' lv_mon INTO lv_fnm.
      ASSIGN (lv_fnm) TO <fs_kwmeng>.
      lt_s805-kwmeng = <fs_kwmeng>.
      "----// 금액
      CLEAR lv_fnm.
      CONCATENATE 'WA_MAIN_M-NETWR_M' lv_mon INTO lv_fnm.
      ASSIGN (lv_fnm) TO <fs_netwr>.
      lt_s805-netwr  = <fs_netwr>.
      APPEND lt_s805.
      lv_month = lv_month + 1.
    ENDDO.
    CLEAR lt_s805.
  ENDLOOP.
  IF p_file EQ 'X'.
    "----// 버전 전체 삭제 후 저장
    DELETE FROM s805 WHERE vrsio = p_vrsio.
    INSERT s805 FROM TABLE lt_s805.
  ELSE.
    "----// 변경
    MODIFY s805 FROM TABLE lt_s805.
  ENDIF.
  pv_retcd = 'S'.
  pv_retxt = ' '.
  pv_chged = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_BDCDATA
*&---------------------------------------------------------------------*
*& BDCDATA 라인 추가
*&---------------------------------------------------------------------*
*&      --> P_DYNBEGIN
*&      --> P_FNAM
*&      --> P_FVAL
*&---------------------------------------------------------------------*
FORM append_bdcdata TABLES pt_bdcdata  STRUCTURE bdcdata
                    USING  pv_dynbegin "TYPE      bdcdata-dynbegin
                           pv_fnam     "TYPE      bdcdata-fnam
                           pv_fval.    "TYPE      bdcdata-fval.
  DATA: wa_bdcdata TYPE bdcdata.
  CLEAR wa_bdcdata.
  IF pv_dynbegin EQ 'X'.
    wa_bdcdata-dynbegin = 'X'.
    wa_bdcdata-program  = pv_fnam.
    wa_bdcdata-dynpro   = pv_fval.
  ELSE.
    wa_bdcdata-fnam     = pv_fnam.
    wa_bdcdata-fval     = pv_fval.
  ENDIF.
  APPEND wa_bdcdata TO pt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM header.
  DATA: lv_str1     TYPE char255,
        lv_str2     TYPE char255,
        lo_document TYPE REF TO cl_dd_document.
  "----// 텍스트
  CONCATENATE TEXT-t09 p_vkorg INTO lv_str1 SEPARATED BY space.
  CONCATENATE TEXT-t10 p_vrsio INTO lv_str2 SEPARATED BY space.
  "----// 문서객체
  CREATE OBJECT lo_document.
  CALL METHOD lo_document->add_text
    EXPORTING
      text         = lv_str1
      sap_style    = cl_dd_document=>small
      sap_color    = cl_dd_document=>list_heading_int
      sap_fontsize = cl_dd_document=>small
      sap_emphasis = cl_dd_document=>strong
      style_class  = space.
  CALL METHOD lo_document->new_line.
  CALL METHOD lo_document->add_text
    EXPORTING
      text         = lv_str2
      sap_style    = cl_dd_document=>small
      sap_color    = cl_dd_document=>list_heading_int
      sap_fontsize = cl_dd_document=>small
      sap_emphasis = cl_dd_document=>strong
      style_class  = space.
  CALL METHOD lo_document->display_document
    EXPORTING
      parent = go_container_0100_1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HEADER_SUB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM header_sub.
  DATA: lv_versb    TYPE versb,
        lv_vbtxt    TYPE vbtxt,
        lv_str1     TYPE char255,
        lo_document TYPE REF TO cl_dd_document.
  "----// 버전 결정
  PERFORM determ_version USING    p_vrsio
                         CHANGING lv_versb.
  SELECT SINGLE
         vbtxt
    INTO @lv_vbtxt
    FROM t459v
   WHERE spras EQ @sy-langu
     AND versb EQ @lv_versb.
  "----// 텍스트
  CONCATENATE TEXT-t11  lv_versb  '('  lv_vbtxt  ')'  INTO lv_str1 SEPARATED BY space.
  "----// 문서객체
  CREATE OBJECT lo_document.
  CALL METHOD lo_document->add_text
    EXPORTING
      text         = lv_str1
      sap_style    = cl_dd_document=>small
      sap_color    = cl_dd_document=>list_heading_int
      sap_fontsize = cl_dd_document=>small
      sap_emphasis = cl_dd_document=>strong
      style_class  = space.
  CALL METHOD lo_document->display_document
    EXPORTING
      parent = go_container_0200_1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_MONTHLY_DATA_W
*&---------------------------------------------------------------------*
*& 월계획 데이터 취합 (버전 16 주별)
*&---------------------------------------------------------------------*
*&      --> GT_MAIN_W
*&---------------------------------------------------------------------*
FORM select_monthly_data_w TABLES pt_main_w LIKE gt_main_w
                            USING pv_spmon  TYPE spmon
                                  pv_stwae  TYPE stwae
                                  pv_basme  TYPE meins
                                  pv_mode   TYPE char10.
  "----// RAW 데이터 취합용 변수
  DATA: BEGIN OF lt_s804_tmp OCCURS 0,
          vrsio  TYPE s804-vrsio,
          vkorg  TYPE s804-vkorg,
          vtweg  TYPE s804-vtweg,
          kunnr  TYPE s804-kunnr,
          pkunwe TYPE s804-pkunwe,
          werks  TYPE s804-werks,
          matnr  TYPE s804-matnr,
          vkaus  TYPE s804-vkaus,
          spwoc  TYPE s804-spwoc,
          vrkme  TYPE s804-vrkme,
          waerk  TYPE s804-waerk,
          kwmeng TYPE s804-kwmeng,
          netwr  TYPE s804-netwr,
        END OF lt_s804_tmp.
  DATA: lv_ident  TYPE tfacd-ident,
        lv_hocid  TYPE tfacd-hocid,
        lv_date   TYPE scal-date,
        lv_week00 TYPE scal-week,
        lv_week   TYPE scal-week,
        lv_mon    TYPE numc2,
        lv_fnm    TYPE char40.
  FIELD-SYMBOLS: <fs_kwmeng>,      "수량
                 <fs_netwr>.       "금액
  DATA: lt_holidays TYPE TABLE OF iscal_day WITH HEADER LINE.
  "----// 텍스트
  DATA: BEGIN OF lt_vtweg_inf OCCURS 0,
          vtweg TYPE vtweg,
          vtext TYPE vtxtk,
        END OF lt_vtweg_inf,
        BEGIN OF lt_kunnr_key OCCURS 0,
          kunnr TYPE kunnr,
        END OF lt_kunnr_key,
        BEGIN OF lt_kunnr_inf OCCURS 0,
          kunnr TYPE kunnr,
          name1 TYPE name1,
        END OF lt_kunnr_inf,
        BEGIN OF lt_matnr_key OCCURS 0,
          matnr TYPE matnr,
        END OF lt_matnr_key,
        BEGIN OF lt_matnr_inf OCCURS 0,
          matnr TYPE matnr,
          vtweg TYPE vtweg,
          maktx TYPE maktx,
          vrkme TYPE vrkme,
          meins TYPE meins,
        END OF lt_matnr_inf,
        BEGIN OF lt_werks_inf OCCURS 0,
          werks TYPE werks_d,
          name1 TYPE name1,
        END OF lt_werks_inf,
        BEGIN OF lt_price_inf OCCURS 0,
          vtweg TYPE vtweg,      "channel
          kunnr TYPE kunnr,      "sold-to
          matnr TYPE matnr,      "material
          datab TYPE a305-datab, "begin data
          kbetr TYPE kbetr_kond, "Condition amount (83.80)
          konwa TYPE konwa,      "Currency (USD)
          kpein TYPE kpein,      "Condition Pricing Unit (1)
          kmein TYPE kmein,      "Condition Unit (EA)
        END OF lt_price_inf.
  DATA: lv_price TYPE kbetr_kond,
        lv_kpein TYPE menge_d,
        lv_menge TYPE menge_d.
  DATA: lt_cellstyl TYPE lvc_t_styl,
        lv_idx      TYPE sy-tabix.
  RANGES: lr_vtweg FOR s804-vtweg,
          lr_kunnr FOR s804-kunnr,
          lr_matnr FOR s804-matnr,
          lr_vkaus FOR s804-vkaus.
  "----// RAW 데이터 취합
  IF pv_mode = 'SEL'.
    lr_vtweg[] = s_vtweg[].
    lr_kunnr[] = s_kunnr[].
    lr_matnr[] = s_matnr[].
    lr_vkaus[] = s_vkaus[].
  ELSE.
    CLEAR: lr_vtweg[],
           lr_kunnr[],
           lr_matnr[],
           lr_vkaus[].
  ENDIF.
  "----// RAW 데이터 취합
  SELECT *
    FROM s804
    INTO CORRESPONDING FIELDS OF TABLE @lt_s804_tmp
   WHERE vrsio  EQ @p_vrsio
     AND spmon  EQ @pv_spmon
     AND vkorg  EQ @p_vkorg
     AND vtweg  IN @lr_vtweg  "@s_vtweg
     AND kunnr  IN @lr_kunnr  "@s_kunnr
     AND werks  EQ @p_werks
     AND matnr  IN @lr_matnr  "@s_matnr
     AND vkaus  IN @lr_vkaus  "@s_vkaus
    ORDER BY vkorg, vtweg, kunnr, pkunwe, werks, matnr, vkaus, spwoc.
  "----// 기준 주차 세팅
  CLEAR: lv_date, lv_ident, lv_hocid.
  CONCATENATE p_gjahr p_vrsio+1(2) '01' INTO lv_date.
  SELECT SINGLE
         land1
    INTO lv_ident
    FROM t001w
   WHERE werks EQ p_werks.
  SELECT SINGLE
         hocid
    INTO lv_hocid
    FROM tfacd
   WHERE ident EQ lv_ident.
  DO 100 TIMES.
    CLEAR lt_holidays[].
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar           = lv_ident
        factory_calendar           = lv_hocid
        date_from                  = lv_date
        date_to                    = lv_date
      TABLES
        holidays                   = lt_holidays
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.
    IF sy-subrc EQ 0.
      IF lt_holidays[] IS INITIAL.
        EXIT.
      ELSE.
        lv_date = lv_date + 1.
        CONTINUE.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = lv_date
    IMPORTING
      week         = lv_week00
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CHECK 1 = 1.
  ENDIF.
  LOOP AT lt_s804_tmp.
    "wa_main_w-vkorg = lt_s804_tmp-vkorg.
    wa_main_w-vtweg = lt_s804_tmp-vtweg.
    wa_main_w-kunag = lt_s804_tmp-kunnr.
    wa_main_w-kunwe = lt_s804_tmp-pkunwe.
    wa_main_w-werks = lt_s804_tmp-werks.
    wa_main_w-matnr = lt_s804_tmp-matnr.
    wa_main_w-vkaus = lt_s804_tmp-vkaus.
    wa_main_w-vrkme = lt_s804_tmp-vrkme.
    wa_main_w-waerk = lt_s804_tmp-waerk.
    IF lt_s804_tmp-spwoc(4) EQ lv_week00(4).
      lv_mon = lt_s804_tmp-spwoc - lv_week00.
    ELSE.
      lv_mon = lt_s804_tmp-spwoc - lv_week00 - 47.
    ENDIF.
    "----// 수량
    CLEAR lv_fnm.
    CONCATENATE 'WA_MAIN_W-KWMENG_W' lv_mon INTO lv_fnm.
    ASSIGN (lv_fnm) TO <fs_kwmeng>.
    <fs_kwmeng> = lt_s804_tmp-kwmeng.
    "----// 금액
    CLEAR lv_fnm.
    CONCATENATE 'WA_MAIN_W-NETWR_W' lv_mon INTO lv_fnm.
    ASSIGN (lv_fnm) TO <fs_netwr>.
    <fs_netwr> = lt_s804_tmp-netwr.
    "----// key insert
    lt_kunnr_key-kunnr = wa_main_w-kunag.
    COLLECT lt_kunnr_key.
    lt_kunnr_key-kunnr = wa_main_w-kunwe.
    COLLECT lt_kunnr_key.
    lt_matnr_key-matnr = wa_main_w-matnr.
    COLLECT lt_matnr_key.
    AT END OF vkaus.
      APPEND wa_main_w TO pt_main_w.
      CLEAR wa_main_w.
    ENDAT.
  ENDLOOP.
  "----// fetch text info
  SELECT vtweg,
         vtext
    INTO CORRESPONDING FIELDS OF TABLE @lt_vtweg_inf
    FROM tvtwt
   WHERE spras EQ @sy-langu.
  SORT lt_vtweg_inf BY vtweg.
  SELECT werks,
         name1
    INTO CORRESPONDING FIELDS OF TABLE @lt_werks_inf
    FROM t001w.
  SORT lt_werks_inf BY werks.
  IF lt_kunnr_key[] IS NOT INITIAL.
    SORT lt_kunnr_key BY kunnr.
    SELECT kunnr,
           name1
      INTO CORRESPONDING FIELDS OF TABLE @lt_kunnr_inf
      FROM kna1
       FOR ALL ENTRIES IN @lt_kunnr_key
     WHERE kunnr EQ @lt_kunnr_key-kunnr.
    SORT lt_kunnr_inf BY kunnr.
  ENDIF.
  IF lt_matnr_key[] IS NOT INITIAL.
    SORT lt_matnr_key BY matnr.
    SELECT t~matnr,
           v~vtweg,
           t~maktx,
           v~vrkme,
           m~meins
      INTO CORRESPONDING FIELDS OF TABLE @lt_matnr_inf
      FROM mara AS m INNER JOIN makt AS t ON t~matnr = m~matnr
                                         AND t~spras = @sy-langu
                     INNER JOIN mvke AS v ON v~matnr = m~matnr
                                         AND v~vkorg = @p_vkorg
       FOR ALL ENTRIES IN @lt_matnr_key
     WHERE m~matnr EQ @lt_matnr_key-matnr.
    SORT lt_matnr_inf BY matnr vtweg.
  ENDIF.
  "----// get price
  SELECT a~vtweg,
         a~kunnr,
         a~matnr,
         a~datab,
         k~kbetr,
         k~konwa,
         k~kpein,
         k~kmein
    INTO CORRESPONDING FIELDS OF TABLE @lt_price_inf
    FROM a305 AS a INNER JOIN konp AS k ON k~knumh = a~knumh
                                       AND k~kopos = '01'
   WHERE a~kappl    EQ 'V'
     AND a~kschl    EQ 'PR00'
     AND a~vkorg    EQ @p_vkorg
     AND a~datab    LE @sy-datlo
     AND a~datbi    GE @sy-datlo
     AND k~loevm_ko EQ @space.
  SORT lt_price_inf BY vtweg kunnr matnr datab DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_price_inf COMPARING vtweg kunnr matnr.
  "----// fill ALV data
  DATA:lv_idex TYPE numc2,
       lv_afnm TYPE char40,
       lv_qfnm TYPE char40.
  LOOP AT pt_main_w INTO wa_main_w.
    lv_idx = sy-tabix.
    "----// price 계획판가
    DO 16 TIMES.
      lv_idex = sy-index - 1.
      CLEAR: lv_afnm, lv_qfnm.
      CONCATENATE 'WA_MAIN_W-NETWR_W'  lv_idex INTO lv_afnm.
      ASSIGN (lv_afnm) TO <fs_netwr>.
      CONCATENATE 'WA_MAIN_W-KWMENG_W' lv_idex INTO lv_qfnm.
      ASSIGN (lv_qfnm) TO <fs_kwmeng>.
      IF <fs_kwmeng> EQ 0.
        wa_main_w-planp = 0.
      ELSE.
        wa_main_w-planp = <fs_netwr> / <fs_kwmeng>.
        EXIT.
      ENDIF.
    ENDDO.
*    IF wa_main_w-kwmeng_w00 EQ 0.
*      wa_main_w-planp = 0.
*    ELSE.
*      wa_main_w-planp = wa_main_w-netwr_w00 / wa_main_w-kwmeng_w00.
*    ENDIF.
    "----// price 판가
    READ TABLE lt_price_inf WITH KEY vtweg = wa_main_w-vtweg
                                     kunnr = wa_main_w-kunag
                                     matnr = wa_main_w-matnr
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-kbetr = lt_price_inf-kbetr.
      wa_main_w-konwa = lt_price_inf-konwa.
      wa_main_w-kpein = lt_price_inf-kpein.
      wa_main_w-kmein = lt_price_inf-kmein.
    ELSE.
      wa_main_w-kbetr = 0.
      wa_main_w-konwa = pv_stwae.
      wa_main_w-kpein = 1.
      wa_main_w-kmein = pv_basme.
*      IF wa_main_w-status NE '1'.
*        wa_main_w-retxt  = TEXT-m41.     "Sales price is not registeded.
*        wa_main_w-status = '1'.          "red
*      ENDIF.
    ENDIF.
    "----// channel text
    READ TABLE lt_vtweg_inf WITH KEY vtweg = wa_main_w-vtweg BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-vtext = lt_vtweg_inf-vtext.
    ENDIF.
    "----// sold to text
    READ TABLE lt_kunnr_inf WITH KEY kunnr = wa_main_w-kunag BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-name1 = lt_kunnr_inf-name1.
    ENDIF.
    "----// ship to text
    READ TABLE lt_kunnr_inf WITH KEY kunnr = wa_main_w-kunwe BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-name2 = lt_kunnr_inf-name1.
    ENDIF.
    "----// plant text
    READ TABLE lt_werks_inf WITH KEY werks = wa_main_w-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-name3 = lt_werks_inf-name1.
    ENDIF.
    "----// material text
    READ TABLE lt_matnr_inf WITH KEY matnr = wa_main_w-matnr
                                     vtweg = wa_main_w-vtweg
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_w-maktx = lt_matnr_inf-maktx.
      IF lt_matnr_inf-vrkme IS NOT INITIAL.
        wa_main_w-vrkme = lt_matnr_inf-vrkme.
      ELSE.
        wa_main_w-vrkme = lt_matnr_inf-meins.
      ENDIF.
    ENDIF.
*    "----// net value
*    IF wa_main_w-vrkme EQ wa_main_w-konwa.
*      lv_menge = wa_main_w-kpein.
*      lv_price = wa_main_w-kbetr / lv_menge.
*    ELSE.
*      lv_kpein = wa_main_w-kpein.
*      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*        EXPORTING
*          i_matnr              = wa_main_w-matnr
*          i_in_me              = wa_main_w-kmein
*          i_out_me             = wa_main_w-vrkme
*          i_menge              = lv_kpein
*        IMPORTING
*          e_menge              = lv_menge
*        EXCEPTIONS
*          error_in_application = 1
*          error                = 2
*          OTHERS               = 3.
*      IF sy-subrc <> 0.
*        lv_menge = 1.
*      ENDIF.
*      lv_price = wa_main_w-kbetr / lv_menge.
*    ENDIF.
*    wa_main_w-netwr_w00 = wa_main_w-kwmeng_w00 * lv_price.
*    wa_main_w-netwr_w01 = wa_main_w-kwmeng_w01 * lv_price.
*    wa_main_w-netwr_w02 = wa_main_w-kwmeng_w02 * lv_price.
*    wa_main_w-netwr_w03 = wa_main_w-kwmeng_w03 * lv_price.
*    wa_main_w-netwr_w04 = wa_main_w-kwmeng_w04 * lv_price.
*    wa_main_w-netwr_w05 = wa_main_w-kwmeng_w05 * lv_price.
*    wa_main_w-netwr_w06 = wa_main_w-kwmeng_w06 * lv_price.
*    wa_main_w-netwr_w07 = wa_main_w-kwmeng_w07 * lv_price.
*    wa_main_w-netwr_w08 = wa_main_w-kwmeng_w08 * lv_price.
*    wa_main_w-netwr_w09 = wa_main_w-kwmeng_w09 * lv_price.
*    wa_main_w-netwr_w10 = wa_main_w-kwmeng_w10 * lv_price.
*    wa_main_w-netwr_w11 = wa_main_w-kwmeng_w11 * lv_price.
*    wa_main_w-netwr_w12 = wa_main_w-kwmeng_w12 * lv_price.
*    wa_main_w-netwr_w13 = wa_main_w-kwmeng_w13 * lv_price.
*    wa_main_w-netwr_w14 = wa_main_w-kwmeng_w14 * lv_price.
*    wa_main_w-netwr_w15 = wa_main_w-kwmeng_w15 * lv_price.
    wa_main_w-waerk = wa_main_w-konwa.
    "----// cell style
    REFRESH lt_cellstyl.
    CLEAR wa_main_w-cellstyl.
    PERFORM fill_cellstyl_w CHANGING lt_cellstyl.
    INSERT LINES OF lt_cellstyl INTO TABLE wa_main_w-cellstyl.
    MODIFY pt_main_w FROM wa_main_w INDEX lv_idx.
  ENDLOOP.
  CHECK 1 = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_YEARLY_DATA_M
*&---------------------------------------------------------------------*
*& 년계획 데이터 취합 (버전 12 월별)
*&---------------------------------------------------------------------*
*&      --> GT_MAIN_M
*&---------------------------------------------------------------------*
FORM select_yearly_data_m TABLES pt_main_m LIKE gt_main_m
                           USING pv_spmon  TYPE spmon
                                 pv_stwae  TYPE stwae
                                 pv_basme  TYPE meins
                                 pv_mode   TYPE char10.
  "----// RAW 데이터 취합용 변수
  DATA: BEGIN OF lt_s805_tmp OCCURS 0,
          vrsio  TYPE s805-vrsio,
          vkorg  TYPE s805-vkorg,
          vtweg  TYPE s805-vtweg,
          kunnr  TYPE s805-kunnr,
          pkunwe TYPE s805-pkunwe,
          werks  TYPE s805-werks,
          matnr  TYPE s805-matnr,
          vkaus  TYPE s805-vkaus,
          spmon  TYPE s805-spmon,
          vrkme  TYPE s805-vrkme,
          waerk  TYPE s805-waerk,
          kwmeng TYPE s805-kwmeng,
          netwr  TYPE s805-netwr,
        END OF lt_s805_tmp.
  DATA: lv_date  TYPE scal-date,
        lv_mon00 TYPE spmon,
        lv_month TYPE spmon,
        lv_mon   TYPE numc2,
        lv_fnm   TYPE char40.
  FIELD-SYMBOLS: <fs_kwmeng>,      "수량
                 <fs_netwr>.       "금액
  "----// 텍스트
  DATA: BEGIN OF lt_vtweg_inf OCCURS 0,
          vtweg TYPE vtweg,
          vtext TYPE vtxtk,
        END OF lt_vtweg_inf,
        BEGIN OF lt_kunnr_key OCCURS 0,
          kunnr TYPE kunnr,
        END OF lt_kunnr_key,
        BEGIN OF lt_kunnr_inf OCCURS 0,
          kunnr TYPE kunnr,
          name1 TYPE name1,
        END OF lt_kunnr_inf,
        BEGIN OF lt_matnr_key OCCURS 0,
          matnr TYPE matnr,
        END OF lt_matnr_key,
        BEGIN OF lt_matnr_inf OCCURS 0,
          matnr TYPE matnr,
          vtweg TYPE vtweg,
          maktx TYPE maktx,
          vrkme TYPE vrkme,
          meins TYPE meins,
        END OF lt_matnr_inf,
        BEGIN OF lt_werks_inf OCCURS 0,
          werks TYPE werks_d,
          name1 TYPE name1,
        END OF lt_werks_inf,
        BEGIN OF lt_price_inf OCCURS 0,
          vtweg TYPE vtweg,      "channel
          kunnr TYPE kunnr,      "sold-to
          matnr TYPE matnr,      "material
          datab TYPE a305-datab, "begin data
          kbetr TYPE kbetr_kond, "Condition amount (83.80)
          konwa TYPE konwa,      "Currency (USD)
          kpein TYPE kpein,      "Condition Pricing Unit (1)
          kmein TYPE kmein,      "Condition Unit (EA)
        END OF lt_price_inf.
  DATA: lv_price TYPE kbetr_kond,
        lv_kpein TYPE menge_d,
        lv_menge TYPE menge_d.
  DATA: lt_cellstyl TYPE lvc_t_styl,
        lv_idx      TYPE sy-tabix.
  RANGES: lr_vtweg FOR s804-vtweg,
          lr_kunnr FOR s804-kunnr,
          lr_matnr FOR s804-matnr,
          lr_vkaus FOR s804-vkaus.
  "----// RAW 데이터 취합
  IF pv_mode = 'SEL'.
    lr_vtweg[] = s_vtweg[].
    lr_kunnr[] = s_kunnr[].
    lr_matnr[] = s_matnr[].
    lr_vkaus[] = s_vkaus[].
  ELSE.
    CLEAR: lr_vtweg[],
           lr_kunnr[],
           lr_matnr[],
           lr_vkaus[].
  ENDIF.
  "----// RAW 데이터
  SELECT *
    FROM s805
    INTO CORRESPONDING FIELDS OF TABLE @lt_s805_tmp
   WHERE vrsio  EQ @p_vrsio
     "AND spmon  EQ @pv_spmon
     AND vkorg  EQ @p_vkorg
     AND vtweg  IN @lr_vtweg  "@s_vtweg
     AND kunnr  IN @lr_kunnr  "@s_kunnr
     AND werks  EQ @p_werks
     AND matnr  IN @lr_matnr  "@s_matnr
     AND vkaus  IN @lr_vkaus  "@s_vkaus
    ORDER BY vkorg, vtweg, kunnr, pkunwe, werks, matnr, vkaus, spmon.
  "----// 기준 주차 세팅
  LOOP AT lt_s805_tmp.
    "wa_main_m-vkorg = lt_s805_tmp-vkorg.
    wa_main_m-vtweg = lt_s805_tmp-vtweg.
    wa_main_m-kunag = lt_s805_tmp-kunnr.
    wa_main_m-kunwe = lt_s805_tmp-pkunwe.
    wa_main_m-werks = lt_s805_tmp-werks.
    wa_main_m-matnr = lt_s805_tmp-matnr.
    wa_main_m-vkaus = lt_s805_tmp-vkaus.
    wa_main_m-vrkme = lt_s805_tmp-vrkme.
    wa_main_m-waerk = lt_s805_tmp-waerk.
    lv_mon = lt_s805_tmp-spmon - 1.
    "----// 수량
    CLEAR lv_fnm.
    CONCATENATE 'WA_MAIN_M-KWMENG_M' lv_mon INTO lv_fnm.
    ASSIGN (lv_fnm) TO <fs_kwmeng>.
    <fs_kwmeng> = lt_s805_tmp-kwmeng.
    "----// 금액
    CLEAR lv_fnm.
    CONCATENATE 'WA_MAIN_M-NETWR_M' lv_mon INTO lv_fnm.
    ASSIGN (lv_fnm) TO <fs_netwr>.
    <fs_netwr> = lt_s805_tmp-netwr.
    "----// key insert
    lt_kunnr_key-kunnr = wa_main_m-kunag.
    COLLECT lt_kunnr_key.
    lt_kunnr_key-kunnr = wa_main_m-kunwe.
    COLLECT lt_kunnr_key.
    lt_matnr_key-matnr = wa_main_m-matnr.
    COLLECT lt_matnr_key.
    AT END OF vkaus.
      APPEND wa_main_m TO pt_main_m.
      CLEAR wa_main_m.
    ENDAT.
  ENDLOOP.
  "----// fetch text info
  SELECT vtweg,
         vtext
    INTO CORRESPONDING FIELDS OF TABLE @lt_vtweg_inf
    FROM tvtwt
   WHERE spras EQ @sy-langu.
  SORT lt_vtweg_inf BY vtweg.
  SELECT werks,
         name1
    INTO CORRESPONDING FIELDS OF TABLE @lt_werks_inf
    FROM t001w.
  SORT lt_werks_inf BY werks.
  IF lt_kunnr_key[] IS NOT INITIAL.
    SORT lt_kunnr_key BY kunnr.
    SELECT kunnr,
           name1
      INTO CORRESPONDING FIELDS OF TABLE @lt_kunnr_inf
      FROM kna1
       FOR ALL ENTRIES IN @lt_kunnr_key
     WHERE kunnr EQ @lt_kunnr_key-kunnr.
    SORT lt_kunnr_inf BY kunnr.
  ENDIF.
  IF lt_matnr_key[] IS NOT INITIAL.
    SORT lt_matnr_key BY matnr.
    SELECT t~matnr,
           v~vtweg,
           t~maktx,
           v~vrkme,
           m~meins
      INTO CORRESPONDING FIELDS OF TABLE @lt_matnr_inf
      FROM mara AS m INNER JOIN makt AS t ON t~matnr = m~matnr
                                         AND t~spras = @sy-langu
                     INNER JOIN mvke AS v ON v~matnr = m~matnr
                                         AND v~vkorg = @p_vkorg
       FOR ALL ENTRIES IN @lt_matnr_key
     WHERE m~matnr EQ @lt_matnr_key-matnr.
    SORT lt_matnr_inf BY matnr vtweg.
  ENDIF.
  "----// get price
  SELECT a~vtweg,
         a~kunnr,
         a~matnr,
         a~datab,
         k~kbetr,
         k~konwa,
         k~kpein,
         k~kmein
    INTO CORRESPONDING FIELDS OF TABLE @lt_price_inf
    FROM a305 AS a INNER JOIN konp AS k ON k~knumh = a~knumh
                                       AND k~kopos = '01'
   WHERE a~kappl    EQ 'V'
     AND a~kschl    EQ 'PR00'
     AND a~vkorg    EQ @p_vkorg
     AND a~datab    LE @sy-datlo
     AND a~datbi    GE @sy-datlo
     AND k~loevm_ko EQ @space.
  SORT lt_price_inf BY vtweg kunnr matnr datab DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_price_inf COMPARING vtweg kunnr matnr.
  "----// fill ALV data
  DATA:lv_idex TYPE numc2,
       lv_afnm TYPE char40,
       lv_qfnm TYPE char40.
  LOOP AT pt_main_m INTO wa_main_m.
    lv_idx = sy-tabix.
    "----// price 계획판가
    DO 12 TIMES.
      lv_idex = sy-index - 1.
      CLEAR: lv_afnm, lv_qfnm.
      CONCATENATE 'WA_MAIN_M-NETWR_M'  lv_idex INTO lv_afnm.
      ASSIGN (lv_afnm) TO <fs_netwr>.
      CONCATENATE 'WA_MAIN_M-KWMENG_M' lv_idex INTO lv_qfnm.
      ASSIGN (lv_qfnm) TO <fs_kwmeng>.
      IF <fs_kwmeng> EQ 0.
        wa_main_m-planp = 0.
      ELSE.
        wa_main_m-planp = <fs_netwr> / <fs_kwmeng>.
        EXIT.
      ENDIF.
    ENDDO.
*    IF wa_main_m-kwmeng_m00 EQ 0.
*      wa_main_m-planp = 0.
*    ELSE.
*      wa_main_m-planp = wa_main_m-netwr_m00 / wa_main_m-kwmeng_m00.
*    ENDIF.
    "----// price 판가
    READ TABLE lt_price_inf WITH KEY vtweg = wa_main_m-vtweg
                                     kunnr = wa_main_m-kunag
                                     matnr = wa_main_m-matnr
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-kbetr = lt_price_inf-kbetr.
      wa_main_m-konwa = lt_price_inf-konwa.
      wa_main_m-kpein = lt_price_inf-kpein.
      wa_main_m-kmein = lt_price_inf-kmein.
    ELSE.
      wa_main_m-kbetr = 0.                   "판가
      wa_main_m-konwa = pv_stwae.
      wa_main_m-kpein = 1.
      wa_main_m-kmein = pv_basme.
*      IF wa_main_m-status NE '1'.
*        wa_main_m-retxt  = TEXT-m41.     "Sales price is not registeded.
*        wa_main_m-status = '1'.          "red
*      ENDIF.
    ENDIF.
    "----// channel text
    READ TABLE lt_vtweg_inf WITH KEY vtweg = wa_main_m-vtweg BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-vtext = lt_vtweg_inf-vtext.
    ENDIF.
    "----// sold to text
    READ TABLE lt_kunnr_inf WITH KEY kunnr = wa_main_m-kunag BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-name1 = lt_kunnr_inf-name1.
    ENDIF.
    "----// ship to text
    READ TABLE lt_kunnr_inf WITH KEY kunnr = wa_main_m-kunwe BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-name2 = lt_kunnr_inf-name1.
    ENDIF.
    "----// plant text
    READ TABLE lt_werks_inf WITH KEY werks = wa_main_m-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-name3 = lt_werks_inf-name1.
    ENDIF.
    "----// material text
    READ TABLE lt_matnr_inf WITH KEY matnr = wa_main_m-matnr
                                     vtweg = wa_main_m-vtweg
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_main_m-maktx = lt_matnr_inf-maktx.
      IF lt_matnr_inf-vrkme IS NOT INITIAL.
        wa_main_m-vrkme = lt_matnr_inf-vrkme.
      ELSE.
        wa_main_m-vrkme = lt_matnr_inf-meins.
      ENDIF.
    ENDIF.
*    "----// net value
*    IF wa_main_m-vrkme EQ wa_main_m-kmein.
*      lv_menge = wa_main_m-kpein.
*      lv_price = wa_main_m-kbetr / lv_menge.
*    ELSE.
*      lv_kpein = wa_main_m-kpein.
*      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*        EXPORTING
*          i_matnr              = wa_main_m-matnr
*          i_in_me              = wa_main_m-kmein
*          i_out_me             = wa_main_m-vrkme
*          i_menge              = lv_kpein
*        IMPORTING
*          e_menge              = lv_menge
*        EXCEPTIONS
*          error_in_application = 1
*          error                = 2
*          OTHERS               = 3.
*      IF sy-subrc <> 0.
*        lv_menge = 1.
*      ENDIF.
*      lv_price = wa_main_m-kbetr / lv_menge.
*    ENDIF.
*    wa_main_m-netwr_m00 = wa_main_m-kwmeng_m00 * lv_price.
*    wa_main_m-netwr_m01 = wa_main_m-kwmeng_m01 * lv_price.
*    wa_main_m-netwr_m02 = wa_main_m-kwmeng_m02 * lv_price.
*    wa_main_m-netwr_m03 = wa_main_m-kwmeng_m03 * lv_price.
*    wa_main_m-netwr_m04 = wa_main_m-kwmeng_m04 * lv_price.
*    wa_main_m-netwr_m05 = wa_main_m-kwmeng_m05 * lv_price.
*    wa_main_m-netwr_m06 = wa_main_m-kwmeng_m06 * lv_price.
*    wa_main_m-netwr_m07 = wa_main_m-kwmeng_m07 * lv_price.
*    wa_main_m-netwr_m08 = wa_main_m-kwmeng_m08 * lv_price.
*    wa_main_m-netwr_m09 = wa_main_m-kwmeng_m09 * lv_price.
*    wa_main_m-netwr_m10 = wa_main_m-kwmeng_m10 * lv_price.
*    wa_main_m-netwr_m11 = wa_main_m-kwmeng_m11 * lv_price.
    wa_main_m-waerk = wa_main_m-konwa.
    "----// cell style
    REFRESH lt_cellstyl.
    CLEAR wa_main_m-cellstyl.
    PERFORM fill_cellstyl_m CHANGING lt_cellstyl.
    INSERT LINES OF lt_cellstyl INTO TABLE wa_main_m-cellstyl.
    MODIFY pt_main_m FROM wa_main_m INDEX lv_idx.
  ENDLOOP.
  CHECK 1 = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_EVENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GO_GRID_0100
*&---------------------------------------------------------------------*
FORM alv_event  USING po_grid TYPE REF TO cl_gui_alv_grid.
  "----// 이벤트 객체 생성
  CREATE OBJECT go_event_receiver_0100.
  "----// 이벤트 객체에 핸들러 등록
  SET HANDLER go_event_receiver_0100->handle_toolbar               FOR po_grid.
  SET HANDLER go_event_receiver_0100->handle_double_click          FOR po_grid.
  SET HANDLER go_event_receiver_0100->handle_data_changed          FOR po_grid.
  SET HANDLER go_event_receiver_0100->handle_data_changed_finished FOR po_grid.
  SET HANDLER go_event_receiver_0100->handle_user_command          FOR po_grid.

  "----// 편집 이벤트 등록
  CALL METHOD po_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  CALL METHOD po_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_LAYOUT_0100
*&      --> GS_VARIANT_0100
*&      --> GT_EXCLUDE_0100
*&      --> GT_FCAT_0100
*&      --> GT_SORT_0100
*&      --> GT_MAIN_W[]
*&---------------------------------------------------------------------*
FORM alv_display TABLES pt_fcat    LIKE gt_fcat_0100
                        pt_sort    LIKE gt_sort_0100
                        pt_tab     TYPE table
                 USING  ps_layout  TYPE lvc_s_layo
                        ps_variant TYPE disvariant
                        pt_exclude TYPE ui_functions
                        po_grid    TYPE REF TO cl_gui_alv_grid.
  "----// 편집모드 결정
  CALL METHOD po_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.
  "----// 그리드 출력
  CALL METHOD po_grid->set_table_for_first_display
    EXPORTING
      i_default            = 'X'
      is_layout            = ps_layout
      is_variant           = ps_variant
      it_toolbar_excluding = pt_exclude
      i_save               = 'A'
    CHANGING
      it_outtab            = pt_tab[]
      it_fieldcatalog      = pt_fcat[]
      it_sort              = pt_sort[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM handle_data_changed  USING  po_data_changed TYPE REF TO cl_alv_changed_data_protocol
                                 pv_ucomm        TYPE sy-ucomm.
  "----// 변경이 있었다는 플래그
  gv_chged = 'X'.
  "----// 변수 선언
  DATA: lt_s804 TYPE TABLE OF s804 WITH HEADER LINE,
        lt_s805 TYPE TABLE OF s804 WITH HEADER LINE.
  DATA: wa_mod_cell TYPE lvc_s_modi.
  DATA: wa_ins_rows TYPE lvc_s_moce,
        wa_del_rows TYPE lvc_s_moce.
  DATA: ls_deleted_rows LIKE LINE OF po_data_changed->mt_deleted_rows.
  DATA: lv_vkorg TYPE tvko-vkorg,
        lv_vtext TYPE tvtwt-vtext,
        lv_name1 TYPE kna1-name1,
        lv_name2 TYPE kna1-name1,
        lv_name3 TYPE t001w-name1,
        lv_maktx TYPE makt-maktx,
        lv_kbetr TYPE kbetr_kond,
        lv_konwa TYPE konwa,
        lv_kpein TYPE kpein,
        lv_kmein TYPE kmein.

  DATA: lv_fname TYPE lvc_fname,
        lv_netwr TYPE ty_main_w-netwr_w01,
        lv_mon   TYPE numc2.
  FIELD-SYMBOLS: <fs>.
  DATA: BEGIN OF lt_price_inf OCCURS 0,
          vtweg TYPE vtweg,      "channel
          kunnr TYPE kunnr,      "sold-to
          matnr TYPE matnr,      "material
          datab TYPE a305-datab, "begin data
          kbetr TYPE kbetr_kond, "Condition amount (83.80)
          konwa TYPE konwa,      "Currency (USD)
          kpein TYPE kpein,      "Condition Pricing Unit (1)
          kmein TYPE kmein,      "Condition Unit (EA)
        END OF lt_price_inf.
  IF p_mont EQ 'X'.  "<== 월계획
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// MODIFY 처리
    "////
    "/////////////////////////////////////////////////////////////////////
    LOOP AT po_data_changed->mt_good_cells INTO wa_mod_cell.
      READ TABLE gt_main_w INTO wa_main_w INDEX wa_mod_cell-row_id.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      CASE wa_mod_cell-fieldname(5).
        WHEN 'VTWEG'.
          CLEAR lv_vtext.
          SELECT SINGLE
                 vtext
            INTO @lv_vtext
            FROM tvtwt
           WHERE spras EQ @sy-langu
             AND vtweg EQ @wa_mod_cell-value.
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = 'VTEXT'
              i_value     = lv_vtext.
*          lv_vkorg = p_vkorg.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'VKORG'
*              i_value     = lv_vkorg.
        WHEN 'KUNAG'.
          CLEAR lv_name1.
          SELECT SINGLE
                 name1
            INTO @lv_name1
            FROM kna1
           WHERE kunnr EQ @wa_mod_cell-value.
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = 'NAME1'
              i_value     = lv_name1.
        WHEN 'KUNWE'.
          CLEAR lv_name2.
          SELECT SINGLE
                 name1
            INTO @lv_name2
            FROM kna1
           WHERE kunnr EQ @wa_mod_cell-value.
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = 'NAME2'
              i_value     = lv_name2.
        WHEN 'WERKS'.
          CLEAR lv_name3.
          SELECT SINGLE
                 name1
            INTO @lv_name3
            FROM t001w
           WHERE werks EQ @wa_mod_cell-value.
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = 'NAME3'
              i_value     = lv_name3.
*        WHEN 'MATNR'.
*          CLEAR lv_maktx.
*          SELECT SINGLE
*                 maktx
*            INTO @lv_maktx
*            FROM makt
*           WHERE spras EQ @sy-langu
*             AND matnr EQ @wa_mod_cell-value.
*          IF sy-subrc EQ 0.
*            "----// get price
*            SELECT a~vtweg,
*                   a~kunnr,
*                   a~matnr,
*                   a~datab,
*                   k~kbetr,
*                   k~konwa,
*                   k~kpein,
*                   k~kmein
*              INTO CORRESPONDING FIELDS OF TABLE @lt_price_inf
*              FROM a305 AS a INNER JOIN konp AS k ON k~knumh = a~knumh
*                                                 AND k~kopos = '01'
*             WHERE a~kappl    EQ 'V'
*               AND a~kschl    EQ 'PR00'
*               AND a~vkorg    EQ @p_vkorg
*               AND a~vtweg    EQ @wa_main_w-vtweg
*               AND a~matnr    EQ @wa_mod_cell-value
*               AND a~datab    LE @sy-datlo
*               AND k~loevm_ko EQ @space.
*            IF sy-subrc NE 0.
*              CONTINUE.
*            ENDIF.
*            SORT lt_price_inf BY vtweg kunnr matnr datab DESCENDING.
*            DELETE ADJACENT DUPLICATES FROM lt_price_inf COMPARING vtweg kunnr matnr.
*            READ TABLE lt_price_inf INDEX 1.
*            IF sy-subrc EQ 0.
*              lv_kbetr = lt_price_inf-kbetr.
*              lv_konwa = lt_price_inf-konwa.
*              lv_kpein = lt_price_inf-kpein.
*              lv_kmein = lt_price_inf-kmein.
*            ELSE.
*              lv_kbetr = 0.
*              lv_konwa = space.
*              lv_kpein = 0.
*              lv_kmein = space.
*            ENDIF.
*          ELSE.
*            lv_maktx = space.
*            lv_kbetr = 0.
*            lv_konwa = space.
*            lv_kpein = 0.
*            lv_kmein = space.
*          ENDIF.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'MAKTX'
*              i_value     = lv_maktx.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'KBETR'
*              i_value     = lv_kbetr.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'KONWA'
*              i_value     = lv_konwa.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'KPEIN'
*              i_value     = lv_kpein.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'KMEIN'
*              i_value     = lv_kmein.
*          "----// Curr.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'WAERK'
*              i_value     = lv_konwa.
*          "----// Unit
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'VRKME'
*              i_value     = lv_kmein.
*        WHEN 'KBETR'.
*          DO 16 TIMES.
*            lv_mon = sy-index - 1.
*            CLEAR: lv_fname.
*            CONCATENATE 'WA_MAIN_W-KWMENG_W' lv_mon INTO lv_fname.
*            ASSIGN (lv_fname) TO <fs>.  "수량
*            CLEAR: lv_fname, lv_netwr.
*            CONCATENATE 'NETWR_W' lv_mon INTO lv_fname.
*            lv_netwr = <fs> * wa_mod_cell-value.
*            "----// Amount
*            CALL METHOD po_data_changed->modify_cell
*              EXPORTING
*                i_row_id    = wa_mod_cell-row_id
*                i_fieldname = lv_fname
*                i_value     = lv_netwr.
*          ENDDO.
        WHEN 'PLANP'.
          DO 16 TIMES.
            lv_mon = sy-index - 1.
            CLEAR: lv_fname.
            CONCATENATE 'WA_MAIN_W-KWMENG_W' lv_mon INTO lv_fname.
            ASSIGN (lv_fname) TO <fs>.  "수량
            CLEAR: lv_fname, lv_netwr.
            CONCATENATE 'NETWR_W' lv_mon INTO lv_fname.
            lv_netwr = <fs> * wa_mod_cell-value.
            "----// Amount
            CALL METHOD po_data_changed->modify_cell
              EXPORTING
                i_row_id    = wa_mod_cell-row_id
                i_fieldname = lv_fname
                i_value     = lv_netwr.
          ENDDO.
        WHEN 'KWMEN'.
          CLEAR: lv_fname, lv_netwr.
          CONCATENATE 'NETWR' wa_mod_cell-fieldname+6(4) INTO lv_fname.
          lv_netwr = wa_mod_cell-value * wa_main_w-kbetr.
          "----// Amount
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = lv_fname
              i_value     = lv_netwr.
        WHEN OTHERS.
          "
      ENDCASE.
    ENDLOOP.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// INSERT 처리
    "////
    "/////////////////////////////////////////////////////////////////////
*    LOOP AT po_data_changed->mt_inserted_rows INTO wa_ins_rows.
*
*    ENDLOOP.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// DELETE 처리
    "////
    "/////////////////////////////////////////////////////////////////////
    LOOP AT po_data_changed->mt_deleted_rows INTO wa_del_rows.
      READ TABLE gt_main_w INTO wa_main_w INDEX wa_del_rows-row_id.
      READ TABLE gt_del_w INTO wa_del_w WITH KEY vtweg = wa_main_w-vtweg
                                                 kunag = wa_main_w-kunag
                                                 kunwe = wa_main_w-kunwe
                                                 werks = wa_main_w-werks
                                                 matnr = wa_main_w-matnr
                                                 vkaus = wa_main_w-vkaus.
      IF sy-subrc EQ 0.
        CHECK 1 = 1.
      ELSE.
        APPEND wa_main_w TO gt_del_w.
      ENDIF.
    ENDLOOP.
    DESCRIBE TABLE po_data_changed->mt_deleted_rows LINES sy-index.
    IF sy-index IS NOT INITIAL.
      gv_deleted = 'X'.
      gt_main_w_temp[] = gt_main_w[].
      CLEAR po_data_changed->mt_deleted_rows.
    ENDIF.
  ELSE.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// MODIFY 처리
    "////
    "/////////////////////////////////////////////////////////////////////
    LOOP AT po_data_changed->mt_good_cells INTO wa_mod_cell.
      READ TABLE gt_main_m INTO wa_main_m INDEX wa_mod_cell-row_id.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      CASE wa_mod_cell-fieldname(5).
        WHEN 'VTWEG'.
          CLEAR lv_vtext.
          SELECT SINGLE
                 vtext
            INTO @lv_vtext
            FROM tvtwt
           WHERE spras EQ @sy-langu
             AND vtweg EQ @wa_mod_cell-value.
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = 'VTEXT'
              i_value     = lv_vtext.
*          lv_vkorg = p_vkorg.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'VKORG'
*              i_value     = lv_vkorg.
        WHEN 'KUNAG'.
          CLEAR lv_name1.
          SELECT SINGLE
                 name1
            INTO @lv_name1
            FROM kna1
           WHERE kunnr EQ @wa_mod_cell-value.
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = 'NAME1'
              i_value     = lv_name1.
        WHEN 'KUNWE'.
          CLEAR lv_name2.
          SELECT SINGLE
                 name1
            INTO @lv_name2
            FROM kna1
           WHERE kunnr EQ @wa_mod_cell-value.
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = 'NAME2'
              i_value     = lv_name2.
        WHEN 'WERKS'.
          CLEAR lv_name3.
          SELECT SINGLE
                 name1
            INTO @lv_name3
            FROM t001w
           WHERE werks EQ @wa_mod_cell-value.
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = 'NAME3'
              i_value     = lv_name3.
*        WHEN 'MATNR'.
*          CLEAR lv_maktx.
*          SELECT SINGLE
*                 maktx
*            INTO @lv_maktx
*            FROM makt
*           WHERE spras EQ @sy-langu
*             AND matnr EQ @wa_mod_cell-value.
*          IF sy-subrc EQ 0.
*            CALL METHOD po_data_changed->modify_cell
*              EXPORTING
*                i_row_id    = wa_mod_cell-row_id
*                i_fieldname = 'MAKTX'
*                i_value     = lv_maktx.
*            "----// get price
*            SELECT a~vtweg,
*                   a~kunnr,
*                   a~matnr,
*                   a~datab,
*                   k~kbetr,
*                   k~konwa,
*                   k~kpein,
*                   k~kmein
*              INTO CORRESPONDING FIELDS OF TABLE @lt_price_inf
*              FROM a305 AS a INNER JOIN konp AS k ON k~knumh = a~knumh
*                                                 AND k~kopos = '01'
*             WHERE a~kappl    EQ 'V'
*               AND a~kschl    EQ 'PR00'
*               AND a~vkorg    EQ @p_vkorg
*               AND a~vtweg    EQ @wa_main_m-vtweg
*               AND a~matnr    EQ @wa_mod_cell-value
*               AND a~datab    LE @sy-datlo
*               AND k~loevm_ko EQ @space.
*            IF sy-subrc NE 0.
*              CONTINUE.
*            ENDIF.
*            SORT lt_price_inf BY vtweg kunnr matnr datab DESCENDING.
*            DELETE ADJACENT DUPLICATES FROM lt_price_inf COMPARING vtweg kunnr matnr.
*            READ TABLE lt_price_inf INDEX 1.
*            IF sy-subrc EQ 0.
*              lv_kbetr = lt_price_inf-kbetr.
*              lv_konwa = lt_price_inf-konwa.
*              lv_kpein = lt_price_inf-kpein.
*              lv_kmein = lt_price_inf-kmein.
*            ELSE.
*              lv_kbetr = 0.
*              lv_konwa = space.
*              lv_kpein = 0.
*              lv_kmein = space.
*            ENDIF.
*          ELSE.
*            lv_maktx = space.
*            lv_kbetr = 0.
*            lv_konwa = space.
*            lv_kpein = 0.
*            lv_kmein = space.
*          ENDIF.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'MAKTX'
*              i_value     = lv_maktx.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'KBETR'
*              i_value     = lv_kbetr.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'KONWA'
*              i_value     = lv_konwa.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'KPEIN'
*              i_value     = lv_kpein.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'KMEIN'
*              i_value     = lv_kmein.
*          "----// Curr.
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'WAERK'
*              i_value     = lv_konwa.
*          "----// Unit
*          CALL METHOD po_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = wa_mod_cell-row_id
*              i_fieldname = 'VRKME'
*              i_value     = lv_kmein.
*        WHEN 'KBETR'.
*          DO 12 TIMES.
*            lv_mon = sy-index - 1.
*            CLEAR: lv_fname.
*            CONCATENATE 'WA_MAIN_M-KWMENG_M' lv_mon INTO lv_fname.
*            ASSIGN (lv_fname) TO <fs>.  "수량
*            CLEAR: lv_fname, lv_netwr.
*            CONCATENATE 'NETWR_M' lv_mon INTO lv_fname.
*            lv_netwr = <fs> * wa_mod_cell-value.
*            "----// Amount
*            CALL METHOD po_data_changed->modify_cell
*              EXPORTING
*                i_row_id    = wa_mod_cell-row_id
*                i_fieldname = lv_fname
*                i_value     = lv_netwr.
*          ENDDO.
        WHEN 'PLANP'.
          DO 12 TIMES.
            lv_mon = sy-index - 1.
            CLEAR: lv_fname.
            CONCATENATE 'WA_MAIN_M-KWMENG_M' lv_mon INTO lv_fname.
            ASSIGN (lv_fname) TO <fs>.  "수량
            CLEAR: lv_fname, lv_netwr.
            CONCATENATE 'NETWR_M' lv_mon INTO lv_fname.
            lv_netwr = <fs> * wa_mod_cell-value.
            "----// Amount
            CALL METHOD po_data_changed->modify_cell
              EXPORTING
                i_row_id    = wa_mod_cell-row_id
                i_fieldname = lv_fname
                i_value     = lv_netwr.
          ENDDO.
        WHEN 'KWMEN'.
          CLEAR: lv_fname, lv_netwr.
          CONCATENATE 'NETWR' wa_mod_cell-fieldname+6(4) INTO lv_fname.
          lv_netwr = wa_mod_cell-value * wa_main_m-kbetr.
          "----// Amount
          CALL METHOD po_data_changed->modify_cell
            EXPORTING
              i_row_id    = wa_mod_cell-row_id
              i_fieldname = lv_fname
              i_value     = lv_netwr.
        WHEN OTHERS.
          "
      ENDCASE.
    ENDLOOP.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// INSERT 처리
    "////
    "/////////////////////////////////////////////////////////////////////
*    LOOP AT po_data_changed->mt_inserted_rows INTO wa_ins_rows.
*
*    ENDLOOP.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// DELETE 처리
    "////
    "/////////////////////////////////////////////////////////////////////
    LOOP AT po_data_changed->mt_deleted_rows INTO wa_del_rows.
      READ TABLE gt_main_m INTO wa_main_m INDEX wa_del_rows-row_id.
      READ TABLE gt_del_m INTO wa_del_m WITH KEY vtweg = wa_main_m-vtweg
                                                 kunag = wa_main_m-kunag
                                                 kunwe = wa_main_m-kunwe
                                                 werks = wa_main_m-werks
                                                 matnr = wa_main_m-matnr
                                                 vkaus = wa_main_m-vkaus.
      IF sy-subrc EQ 0.
        CHECK 1 = 1.
      ELSE.
        APPEND wa_main_m TO gt_del_m.
      ENDIF.
    ENDLOOP.
    DESCRIBE TABLE po_data_changed->mt_deleted_rows LINES sy-index.
    IF sy-index IS NOT INITIAL.
      gv_deleted = 'X'.
      gt_main_m_temp[] = gt_main_m[].
      CLEAR po_data_changed->mt_deleted_rows.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DMTF_MONTHLY_DATA_W
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN_W
*&      --> LT_SELECTED
*&      <-- LV_RETCD
*&      <-- LV_RETXT
*&---------------------------------------------------------------------*
FORM dmtf_monthly_data_w TABLES   pt_main_w   LIKE gt_main_w
                                  pt_selected TYPE lvc_t_row
                         CHANGING pv_retcd    TYPE char01
                                  pv_retxt    TYPE text100.
  "----// 주차별 수량 집계용 변수 선언
  DATA: lv_tabix  TYPE sy-tabix,
        lv_ident  TYPE tfacd-ident,
        lv_hocid  TYPE tfacd-hocid,
        lv_date   TYPE scal-date,
        lv_week00 TYPE scal-week,
        lv_week   TYPE scal-week,
        lv_mon    TYPE numc2,
        lv_fnm    TYPE char40.     "필드명
  FIELD-SYMBOLS: <fs_kwmeng>,      "수량
                 <fs_netwr>.       "금액
  DATA: lt_holidays TYPE TABLE OF iscal_day WITH HEADER LINE.
  "----// DMTF 레코드
  DATA: BEGIN OF lt_dmtf_w OCCURS 0,
          werks      TYPE werks_ext,  "Plant
          matnr      TYPE matnr,      "Material
          kwmeng_w00 TYPE kwmeng,     "Quantity w00
          kwmeng_w01 TYPE kwmeng,     "Quantity w01
          kwmeng_w02 TYPE kwmeng,     "Quantity w02
          kwmeng_w03 TYPE kwmeng,     "Quantity w03
          kwmeng_w04 TYPE kwmeng,     "Quantity w04
          kwmeng_w05 TYPE kwmeng,     "Quantity w05
          kwmeng_w06 TYPE kwmeng,     "Quantity w06
          kwmeng_w07 TYPE kwmeng,     "Quantity w07
          kwmeng_w08 TYPE kwmeng,     "Quantity w08
          kwmeng_w09 TYPE kwmeng,     "Quantity w09
          kwmeng_w10 TYPE kwmeng,     "Quantity w10
          kwmeng_w11 TYPE kwmeng,     "Quantity w11
          kwmeng_w12 TYPE kwmeng,     "Quantity w12
          kwmeng_w13 TYPE kwmeng,     "Quantity w13
          kwmeng_w14 TYPE kwmeng,     "Quantity w14
          kwmeng_w15 TYPE kwmeng,     "Quantity w15
          vrkme      TYPE vrkme,      "Sales unit
        END OF lt_dmtf_w.
  "----// DMTF BAPI용 변수 선언
  DATA: lv_versb      TYPE          versb,
        lv_req_number TYPE          bapisitemr-req_number,
        ls_dmitem     TYPE          bapisitemr,
        lt_schdin     TYPE TABLE OF bapisshdin WITH HEADER LINE,
        lt_return     TYPE TABLE OF bapiret1   WITH HEADER LINE.
  "----// 기존 저장되어 있는 레코드
  DATA: lv_pointer TYPE pbim-bdzei,     "소요량포인터.
        lt_pbed    TYPE TABLE OF pbed        WITH HEADER LINE.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 버전 결정
  "////
  "/////////////////////////////////////////////////////////////////////
  PERFORM determ_version USING    p_vrsio
                         CHANGING lv_versb.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 해당 버전 기 생성여부 확인
  "////
  "/////////////////////////////////////////////////////////////////////
  SELECT SINGLE
         bdzei
    INTO @lv_pointer
    FROM pbim
   WHERE versb EQ @lv_versb
     AND loevr EQ @space.
  IF sy-subrc EQ 0.
    PERFORM delete_version USING p_werks  lv_versb.
    COMMIT WORK AND WAIT.
    WAIT UP TO 2 SECONDS.
  ENDIF.
  "----// 주차
  CLEAR: lv_date, lv_req_number, lv_ident, lv_hocid.
  CONCATENATE p_gjahr p_vrsio+1(2) '01' INTO lv_date.
  lv_req_number = 'M' && p_vrsio+1(2) && '01' && p_gjahr.
  SELECT SINGLE
         land1
    INTO lv_ident
    FROM t001w
   WHERE werks EQ p_werks.
  SELECT SINGLE
         hocid
    INTO lv_hocid
    FROM tfacd
   WHERE ident EQ lv_ident.
  DO 100 TIMES.
    CLEAR lt_holidays[].
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar           = lv_ident
        factory_calendar           = lv_hocid
        date_from                  = lv_date
        date_to                    = lv_date
      TABLES
        holidays                   = lt_holidays
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.
    IF sy-subrc EQ 0.
      IF lt_holidays[] IS INITIAL.
        EXIT.
      ELSE.
        lv_date = lv_date + 1.
        CONTINUE.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CLEAR lv_week00.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = lv_date
    IMPORTING
      week         = lv_week00
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CHECK 1 = 1.
  ENDIF.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 플랜트/자재 별로 레코드 합산
  "////
  "/////////////////////////////////////////////////////////////////////
  CLEAR lt_dmtf_w[].
  LOOP AT pt_main_w.
    MOVE-CORRESPONDING pt_main_w TO lt_dmtf_w.
    COLLECT lt_dmtf_w.
  ENDLOOP.
  IF lt_dmtf_w[] IS INITIAL.
    pv_retcd = 'E'.
    pv_retxt = TEXT-t44.
    EXIT.
  ENDIF.
  SORT lt_dmtf_w BY werks matnr.
  LOOP AT lt_dmtf_w.
    CLEAR: ls_dmitem, lt_schdin[], lt_schdin, lt_return[], lt_return.
    "----// H 자재/플랜트/버전 정보
    ls_dmitem-material   = lt_dmtf_w-matnr.
    ls_dmitem-plant      = lt_dmtf_w-werks.
    ls_dmitem-requ_type  = 'VSF'.
    ls_dmitem-version    = lv_versb.
    ls_dmitem-vers_activ = space.
    ls_dmitem-req_number = lv_req_number.
    "----// I 스케쥴
    CLEAR: lv_week, lv_mon.
*    lv_week = lv_week00 - 1.
    DO 16 TIMES.
*      lv_week = lv_week + 1.
      lv_mon = sy-index - 1.
      CLEAR wa_week_map_to_per.
      READ TABLE gt_week_map_to_per INTO wa_week_map_to_per WITH KEY wkidx = lv_mon
                                                            BINARY SEARCH.
      lv_week = wa_week_map_to_per-perxx.
      "----// 수량
      CLEAR lv_fnm.
      CONCATENATE 'LT_DMTF_W-KWMENG_W' lv_mon INTO lv_fnm.
      ASSIGN (lv_fnm) TO <fs_kwmeng>.
      lt_schdin-date_type = '2'.     "week
      CALL FUNCTION 'WEEK_GET_FIRST_DAY'
        EXPORTING
          week         = lv_week
        IMPORTING
          date         = lv_date
        EXCEPTIONS
          week_invalid = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        CHECK 1 = 1.
      ENDIF.
      lt_schdin-req_date  = lv_date.
      lt_schdin-req_qty   = <fs_kwmeng>.
      lt_schdin-unit      = wa_main_w-vrkme.
      APPEND lt_schdin.
      CLEAR lt_schdin.
*      IF lv_week+4(2) = '53'.
*        lv_week+0(4) = p_gjahr + 1.
*        lv_week+4(2) = '00'.
*      ENDIF.
    ENDDO.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// DM Transfer
    "////
    "/////////////////////////////////////////////////////////////////////
    CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
      EXPORTING
        requirements_item        = ls_dmitem
      TABLES
        requirements_schedule_in = lt_schdin
        return                   = lt_return.
    READ TABLE lt_return WITH KEY type = 'E'.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      wa_main_w-retcd  = 'S'.
      wa_main_w-retxt  = ' '.
      wa_main_w-status = '3'.
    ELSE.
      wa_main_w-retcd  = 'E'.
      MESSAGE ID     lt_return-id
              TYPE   lt_return-type
              NUMBER lt_return-number
              INTO   wa_main_w-retxt
              WITH   lt_return-message_v1  lt_return-message_v2  lt_return-message_v3  lt_return-message_v4.
      wa_main_w-status = '1'.
    ENDIF.
    MODIFY pt_main_w FROM wa_main_w TRANSPORTING status
                                                 retcd
                                                 retxt
                                    WHERE werks EQ lt_dmtf_w-werks
                                      AND matnr EQ lt_dmtf_w-matnr.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DMTF_YEARLY_DATA_M
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MAIN_M
*&      --> LT_SELECTED
*&      <-- LV_RETCD
*&      <-- LV_RETXT
*&---------------------------------------------------------------------*
FORM dmtf_yearly_data_m TABLES   pt_main_m   LIKE gt_main_m
                                 pt_selected TYPE lvc_t_row
                        CHANGING pv_retcd    TYPE char01
                                 pv_retxt    TYPE text100.
  "----// 주차별 수량 집계용 변수 선언
  DATA: lv_tabix TYPE sy-tabix,
        lv_date  TYPE scal-date,
        lv_mon   TYPE numc2,
        lv_cur   TYPE numc2,
        lv_fnm   TYPE char40.     "필드명
  FIELD-SYMBOLS: <fs_kwmeng>,      "수량
                 <fs_netwr>.       "금액
  "----// DMTF 레코드
  DATA: BEGIN OF lt_dmtf_m OCCURS 0,
          werks      TYPE werks_ext,  "Plant
          matnr      TYPE matnr,      "Material
          kwmeng_m00 TYPE kwmeng,     "Quantity m00
          kwmeng_m01 TYPE kwmeng,     "Quantity m01
          kwmeng_m02 TYPE kwmeng,     "Quantity m02
          kwmeng_m03 TYPE kwmeng,     "Quantity m03
          kwmeng_m04 TYPE kwmeng,     "Quantity m04
          kwmeng_m05 TYPE kwmeng,     "Quantity m05
          kwmeng_m06 TYPE kwmeng,     "Quantity m06
          kwmeng_m07 TYPE kwmeng,     "Quantity m07
          kwmeng_m08 TYPE kwmeng,     "Quantity m08
          kwmeng_m09 TYPE kwmeng,     "Quantity m09
          kwmeng_m10 TYPE kwmeng,     "Quantity m10
          kwmeng_m11 TYPE kwmeng,     "Quantity m11
          vrkme      TYPE vrkme,      "Sales unit
        END OF lt_dmtf_m.
  "----// DMTF BAPI용 변수 선언
  DATA: lv_versb      TYPE          versb,
        lv_req_number TYPE bapisitemr-req_number,
        ls_dmitem     TYPE          bapisitemr,
        lt_schdin     TYPE TABLE OF bapisshdin WITH HEADER LINE,
        lt_return     TYPE TABLE OF bapiret1   WITH HEADER LINE.
  "----// 기존 저장되어 있는 레코드
  DATA: lv_pointer TYPE pbim-bdzei,     "소요량포인터.
        lt_pbed    TYPE TABLE OF pbed        WITH HEADER LINE.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 버전 결정
  "////
  "/////////////////////////////////////////////////////////////////////
  PERFORM determ_version USING    p_vrsio
                         CHANGING lv_versb.
  lv_req_number = 'Y' && '01' && '01' && p_gjahr.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 해당 버전 기 생성여부 확인
  "////
  "/////////////////////////////////////////////////////////////////////
  SELECT SINGLE
         bdzei
    INTO @lv_pointer
    FROM pbim
   WHERE versb EQ @lv_versb
     AND loevr EQ @space.
  IF sy-subrc EQ 0.
    PERFORM delete_version USING p_werks  lv_versb.
    COMMIT WORK AND WAIT.
    WAIT UP TO 2 SECONDS.
  ENDIF.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 플랜트/자재 별로 레코드 합산
  "////
  "/////////////////////////////////////////////////////////////////////
  CLEAR lt_dmtf_m[].
  LOOP AT pt_main_m.
    MOVE-CORRESPONDING pt_main_m TO lt_dmtf_m.
    COLLECT lt_dmtf_m.
  ENDLOOP.
  IF lt_dmtf_m[] IS INITIAL.
    pv_retcd = 'E'.
    pv_retxt = TEXT-t44.
    EXIT.
  ENDIF.
  SORT lt_dmtf_m BY werks matnr.
  LOOP AT lt_dmtf_m.
    CLEAR: ls_dmitem, lt_schdin[], lt_schdin, lt_return[], lt_return.
    "----// H 자재/플랜트/버전 정보
    ls_dmitem-material   = lt_dmtf_m-matnr.
    ls_dmitem-plant      = lt_dmtf_m-werks.
    ls_dmitem-requ_type  = 'VSF'.
    ls_dmitem-version    = lv_versb.
    ls_dmitem-vers_activ = space.
    ls_dmitem-req_number = lv_req_number.
    "----// I 스케쥴
    DO 12 TIMES.
      lv_mon = sy-index - 1.
      lv_cur = sy-index.
      "----// 수량
      CLEAR lv_fnm.
      CONCATENATE 'LT_DMTF_M-KWMENG_M' lv_mon INTO lv_fnm.
      ASSIGN (lv_fnm) TO <fs_kwmeng>.
      lt_schdin-date_type = '3'.     "month
      CONCATENATE p_gjahr lv_cur '01' INTO lv_date.
      lt_schdin-req_date  = lv_date.
      lt_schdin-req_qty   = <fs_kwmeng>.
      lt_schdin-unit      = wa_main_m-vrkme.
      APPEND lt_schdin.
      CLEAR lt_schdin.
    ENDDO.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// DM Transfer
    "////
    "/////////////////////////////////////////////////////////////////////
    CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
      EXPORTING
        requirements_item        = ls_dmitem
      TABLES
        requirements_schedule_in = lt_schdin
        return                   = lt_return.
    READ TABLE lt_return WITH KEY type = 'E'.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      wa_main_m-retcd  = 'S'.
      wa_main_m-retxt  = ' '.
      wa_main_m-status = '3'.
    ELSE.
      wa_main_m-retcd  = 'E'.
      MESSAGE ID     lt_return-id
              TYPE   lt_return-type
              NUMBER lt_return-number
              INTO   wa_main_m-retxt
              WITH   lt_return-message_v1  lt_return-message_v2  lt_return-message_v3  lt_return-message_v4.
      wa_main_m-status = '1'.
    ENDIF.
    MODIFY pt_main_m FROM wa_main_m TRANSPORTING status
                                                 retcd
                                                 retxt
                                    WHERE werks EQ lt_dmtf_m-werks
                                      AND matnr EQ lt_dmtf_m-matnr.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DETERM_VERSION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_VRSIO
*&      <-- LV_VERSB
*&---------------------------------------------------------------------*
FORM determ_version  USING    pv_vrsio TYPE vrsio
                     CHANGING pv_versb TYPE versb.
  IF pv_vrsio(1) EQ '1'.
    pv_versb(1) = 'M'.
    CASE pv_vrsio+1(02).
      WHEN '10'.  pv_versb+1(1) = 'A'.
      WHEN '11'.  pv_versb+1(1) = 'B'.
      WHEN '12'.  pv_versb+1(1) = 'C'.
      WHEN OTHERS.  pv_versb+1(1) = pv_vrsio+2(1).
        "
    ENDCASE.
  ELSE.
    pv_versb(1) = 'Y'.
    pv_versb+1(1) = pv_vrsio+2(01).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_VERSION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_WERKS
*&      --> LV_VERSB
*&---------------------------------------------------------------------*
FORM delete_version USING pv_werks TYPE werks_d
                          pv_versb TYPE versb.
  "----// BDC용 변수
  DATA: lt_bdcdata TYPE TABLE OF bdcdata    WITH HEADER LINE.
  DATA: lt_messtab TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.
  DATA: ls_opt     TYPE ctu_params.
  CLEAR ls_opt.
  ls_opt-dismode = 'N'.
  ls_opt-updmode = 'S'.
  ls_opt-defsize = 'X'.
  CLEAR: lt_bdcdata, lt_bdcdata[].
  PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPMM60X'     '0106'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'   '/00'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'AM60X-PBDAW'  'X'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'AM60X-PBDNR'  '*'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'AM60X-WERKS'   pv_werks.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'AM60X-VERAW'  'X'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'RM60X-VERSB'   pv_versb.
  PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPLM60E'     '0200'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'   '=ALMK'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPLM60E'     '0200'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'   '=POLO'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPLSPO1'     '0500'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'   '=OPT1'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING 'X'  'SAPLM60E'     '0200'.
  PERFORM append_bdcdata TABLES lt_bdcdata USING ' '  'BDC_OKCODE'   '=SICH'.
  CALL TRANSACTION 'MD62' WITH AUTHORITY-CHECK USING lt_bdcdata MESSAGES INTO lt_messtab OPTIONS FROM ls_opt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_MODIFIED
*&      --> ET_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM handle_data_changed_finished USING pv_modified   TYPE char01
                                        pt_good_cells TYPE lvc_t_modi.
  "----// 변수 선언
  DATA: lt_s804 TYPE TABLE OF s804 WITH HEADER LINE,
        lt_s805 TYPE TABLE OF s804 WITH HEADER LINE.
  IF p_mont EQ 'X'.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// MODIFY 처리
    "////
    "/////////////////////////////////////////////////////////////////////


    "/////////////////////////////////////////////////////////////////////
    "////
    "//// INSERT 처리
    "////
    "/////////////////////////////////////////////////////////////////////


    "/////////////////////////////////////////////////////////////////////
    "////
    "//// DELETE 처리
    "////
    "/////////////////////////////////////////////////////////////////////
    IF gv_deleted EQ 'X'.
      PERFORM pop_up USING    TEXT-022  TEXT-018
                     CHANGING gv_answer.
      IF gv_answer = '1'.
        SELECT  *
          INTO CORRESPONDING FIELDS OF TABLE lt_s804
          FROM s804
          FOR ALL ENTRIES IN gt_del_w
         WHERE vrsio  EQ p_vrsio
           AND vkorg  EQ p_vkorg
           AND vtweg  EQ gt_del_w-vtweg
           AND kunnr  EQ gt_del_w-kunag
           AND pkunwe EQ gt_del_w-kunwe
           AND werks  EQ gt_del_w-werks
           AND matnr  EQ gt_del_w-matnr
           AND vkaus  EQ gt_del_w-vkaus.
        DELETE s804 FROM TABLE lt_s804.
        CLEAR: lt_s804[], gt_del_w[].
        MESSAGE s007.  "Deleted successfully
        EXIT.
      ELSE.
*   data has been deleted than set the temp data back to the main table
*   and refresh the table display
        IF gv_deleted = 'X'.
          gt_main_w[] = gt_main_w_temp[].
          CLEAR: gv_deleted, gt_main_w_temp[].
        ENDIF.
        "----// ALV 다시 그리기
        CALL METHOD go_grid_0100->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
      ENDIF.
    ENDIF.
  ELSE.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// MODIFY 처리
    "////
    "/////////////////////////////////////////////////////////////////////


    "/////////////////////////////////////////////////////////////////////
    "////
    "//// INSERT 처리
    "////
    "/////////////////////////////////////////////////////////////////////


    "/////////////////////////////////////////////////////////////////////
    "////
    "//// DELETE 처리
    "////
    "/////////////////////////////////////////////////////////////////////
    IF gv_deleted EQ 'X'.
      PERFORM pop_up USING    TEXT-022  TEXT-018
                     CHANGING gv_answer.
      IF gv_answer = '1'.
        SELECT  *
          INTO CORRESPONDING FIELDS OF TABLE lt_s805
          FROM s805
          FOR ALL ENTRIES IN gt_del_m
         WHERE vrsio  EQ p_vrsio
           AND vkorg  EQ p_vkorg
           AND vtweg  EQ gt_del_m-vtweg
           AND kunnr  EQ gt_del_m-kunag
           AND pkunwe EQ gt_del_m-kunwe
           AND werks  EQ gt_del_m-werks
           AND matnr  EQ gt_del_m-matnr
           AND vkaus  EQ gt_del_m-vkaus.
        DELETE s805 FROM TABLE lt_s805.
        CLEAR: lt_s805[], gt_del_m[].
        MESSAGE s007.  "Deleted successfully
        EXIT.
      ELSE.
*   data has been deleted than set the temp data back to the main table
*   and refresh the table display
        IF gv_deleted = 'X'.
          gt_main_m[] = gt_main_m_temp[].
          CLEAR: gv_deleted, gt_main_m_temp[].
        ENDIF.
        "----// ALV 다시 그리기
        CALL METHOD go_grid_0100->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_IREQ_W
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_IREQ_W
*&---------------------------------------------------------------------*
FORM get_ireq_w TABLES pt_ireq_w LIKE gt_ireq_w.
  DATA: lv_versb TYPE versb,
        lv_idx   TYPE numc2,
        lv_fnm   TYPE string.
  FIELD-SYMBOLS: <fs>.
  DATA: BEGIN OF lt_ireq OCCURS 0,
          werks TYPE werks_d,
          matnr TYPE matnr,
          perxx TYPE perxx,
          plnmg TYPE plnmg,
          meins TYPE meins,
        END OF lt_ireq.
  "----// 1년주차==WEEK컬럼명 맵핑 찾기
  DATA: lv_date       TYPE scal-date,
        lv_req_number TYPE bapisitemr-req_number,
        lv_ident      TYPE tfacd-ident,
        lv_hocid      TYPE tfacd-hocid,
        lv_week00     TYPE scal-week,
        lv_week99     TYPE scal-week.
  DATA: lt_holidays TYPE TABLE OF iscal_day WITH HEADER LINE.

  CLEAR: lv_date, lv_req_number, lv_ident, lv_hocid.
  CONCATENATE p_gjahr p_vrsio+1(2) '01' INTO lv_date.
  lv_req_number = 'M' && p_vrsio+1(2) && '01' && p_gjahr.
  SELECT SINGLE
         land1
    INTO lv_ident
    FROM t001w
   WHERE werks EQ p_werks.
  SELECT SINGLE
         hocid
    INTO lv_hocid
    FROM tfacd
   WHERE ident EQ lv_ident.
  DO 100 TIMES.
    CLEAR lt_holidays[].
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar           = lv_ident
        factory_calendar           = lv_hocid
        date_from                  = lv_date
        date_to                    = lv_date
      TABLES
        holidays                   = lt_holidays
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.
    IF sy-subrc EQ 0.
      IF lt_holidays[] IS INITIAL.
        EXIT.
      ELSE.
        lv_date = lv_date + 1.
        CONTINUE.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  "----// 해당 버전의 첫번째 주 찾기 lv_week00
  CLEAR lv_week00.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = lv_date
    IMPORTING
      week         = lv_week00
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CHECK 1 = 1.
  ENDIF.
  "----// 해당 버전의 마지막 주 찾기 lv_week99
  CONCATENATE p_gjahr p_vrsio+1(2) '31' INTO lv_date.
  CLEAR lv_week99.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = lv_date
    IMPORTING
      week         = lv_week99
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CHECK 1 = 1.
  ENDIF.



  "----// 버전 결정
  PERFORM determ_version USING    p_vrsio
                         CHANGING lv_versb.
  "----// Fetch
  SELECT pbim~werks,
         pbim~matnr,
         pbed~perxx,
         pbed~plnmg,
         pbed~meins
    INTO CORRESPONDING FIELDS OF TABLE @lt_ireq
    FROM pbim INNER JOIN pbed ON pbed~bdzei = pbim~bdzei
   WHERE pbim~werks EQ @p_werks
     AND pbim~bedae EQ 'VSF'
     AND pbim~versb EQ @lv_versb
     AND pbim~loevr EQ @space.
  "----// 레포트 데이터 만들기
  SORT lt_ireq BY werks matnr perxx.
  CLEAR pt_ireq_w[].
  LOOP AT lt_ireq.
    CLEAR wa_per_map_to_week.
    READ TABLE gt_per_map_to_week INTO wa_per_map_to_week WITH KEY perxx = lt_ireq-perxx.
    CLEAR lv_fnm.
    CONCATENATE 'WA_IREQ_W-PLNMG_W' wa_per_map_to_week-wkidx INTO lv_fnm.
*    lv_idx = lt_ireq-perxx+4(2) - lv_week00+4(2).
*    CLEAR lv_fnm.
*    CONCATENATE 'WA_IREQ_W-PLNMG_W' lv_idx INTO lv_fnm.
    ASSIGN (lv_fnm) TO <fs>.
    <fs>            = lt_ireq-plnmg.
    wa_ireq_w-meins = lt_ireq-meins.
*    lv_idx = lv_idx + 1.
    AT END OF matnr.
      wa_ireq_w-werks = lt_ireq-werks.
      wa_ireq_w-matnr = lt_ireq-matnr.
      APPEND wa_ireq_w TO pt_ireq_w.
      CLEAR wa_ireq_w.
      CLEAR lv_idx.
    ENDAT.
  ENDLOOP.
  "----// 화면 호출
*  CALL SCREEN 0200 STARTING AT 20 02 ENDING AT 135 18.
  PERFORM screen_0200.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_IREQ_M
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_IREQ_M
*&---------------------------------------------------------------------*
FORM get_ireq_m TABLES pt_ireq_m LIKE gt_ireq_m.
  DATA: lv_versb TYPE versb,
        lv_idx   TYPE numc2.
  FIELD-SYMBOLS: <fs>.
  DATA: BEGIN OF lt_ireq OCCURS 0,
          werks TYPE werks_d,
          matnr TYPE matnr,
          perxx TYPE perxx,
          plnmg TYPE plnmg,
          meins TYPE meins,
        END OF lt_ireq.
  "----// 버전 결정
  PERFORM determ_version USING    p_vrsio
                         CHANGING lv_versb.
  "----// Fetch
  SELECT pbim~werks,
         pbim~matnr,
         pbed~perxx,
         pbed~plnmg,
         pbed~meins
    INTO CORRESPONDING FIELDS OF TABLE @lt_ireq
    FROM pbim INNER JOIN pbed ON pbed~bdzei = pbim~bdzei
   WHERE pbim~werks EQ @p_werks
     AND pbim~bedae EQ 'VSF'
     AND pbim~versb EQ @lv_versb
     AND pbim~loevr EQ @space.

  "----// 레포트 데이터 만들기
  SORT lt_ireq BY werks matnr perxx.
  CLEAR pt_ireq_m[].

  LOOP AT lt_ireq.
    lv_idx = lt_ireq-perxx+4(2) - 1.
    CLEAR lv_fnm.
    CONCATENATE 'WA_IREQ_M-PLNMG_M' lv_idx INTO lv_fnm.
    ASSIGN (lv_fnm) TO <fs>.
    <fs>            = lt_ireq-plnmg.
    wa_ireq_m-meins = lt_ireq-meins.
*    lv_idx = lv_idx + 1.
    AT END OF matnr.
      wa_ireq_m-werks = lt_ireq-werks.
      wa_ireq_m-matnr = lt_ireq-matnr.
      APPEND wa_ireq_m TO pt_ireq_m.
      CLEAR wa_ireq_m.
      CLEAR lv_idx.
    ENDAT.
  ENDLOOP.

  "----// 화면 호출
*  CALL SCREEN 0200 STARTING AT 20 02 ENDING AT 135 18.
  PERFORM screen_0200.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CONSTANT_VARIABLES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GV_SPMON
*&---------------------------------------------------------------------*
FORM set_constant_variables CHANGING pv_spmon
                                     pv_stwae
                                     pv_basme.
  IF p_mont EQ 'X'.
    pv_spmon = p_gjahr && p_vrsio+1(2).
    SELECT SINGLE
           stwae,
           basme
      INTO (@pv_stwae, @pv_basme)
      FROM t445a
     WHERE gstru EQ 'S804'.
  ELSE.
    pv_spmon = p_gjahr && '01'.
    SELECT SINGLE
           stwae,
           basme
      INTO (@pv_stwae, @pv_basme)
      FROM t445a
     WHERE gstru EQ 'S805'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILE_EXISTENCE_CHECK
*&---------------------------------------------------------------------*
*& 파일 경로명 및 이름 체크
*&---------------------------------------------------------------------*
*&      --> 파일명
*&      <-- 파일 존재 체크
*&---------------------------------------------------------------------*
FORM file_existence_check  USING    p_fname         TYPE text1024
                           CHANGING pv_file_checked TYPE c.
  "----// 변수선언
  DATA: lv_file   TYPE string,
        lv_result TYPE abap_bool.
  lv_file = p_fname.
  CLEAR pv_file_checked.
  "----// 파일 존재 체크 함수
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file            = lv_file
    RECEIVING
      result          = lv_result
    EXCEPTIONS
      cntl_error      = 1
      error_no_gui    = 2
      wrong_parameter = 3
      OTHERS          = 4.
  pv_file_checked = lv_result.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_EVENT_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GO_GRID_0200
*&      --> GO_DIALOG_0200
*&---------------------------------------------------------------------*
FORM alv_event_200   USING po_grid TYPE REF TO cl_gui_alv_grid
                            po_dialog_0200 TYPE REF TO cl_gui_dialogbox_container.
  "----// 이벤트 객체 생성
  CREATE OBJECT go_event_receiver_0100.
  "----// 이벤트 객체에 핸들러 등록
  SET HANDLER go_event_receiver_0100->handle_toolbar               FOR po_grid.
  SET HANDLER go_event_receiver_0100->handle_double_click          FOR po_grid.
  SET HANDLER go_event_receiver_0100->handle_data_changed          FOR po_grid.
  SET HANDLER go_event_receiver_0100->handle_data_changed_finished FOR po_grid.
  SET HANDLER go_event_receiver_0100->handle_user_command          FOR po_grid.

  "----// 편집 이벤트 등록
  CALL METHOD po_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  CALL METHOD po_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CREATE OBJECT go_event_handler.
  SET HANDLER go_event_handler->on_close                     FOR po_dialog_0200.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCREEN_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM screen_0200 .

  CLEAR : gt_fcat_0200, gt_sort_0200, gs_layout_0200,
          gs_variant_0200, gt_exclude_0200.

  CREATE OBJECT go_dialog_0200
    EXPORTING
      parent  = cl_gui_dialogbox_container=>screen0
*     REPID   = SY-REPID
      width   = '1200'                      " Width of This Container
      height  = '300'                      " Height of This Container
      top     = 40                " Top Position of Dialog Box
      left    = 25                " Left Position of Dialog Box
      caption = 'Sales Plan Managememt'.  " Dialog Box Caption

  "----// 도킹 상하로 나누기
  CREATE OBJECT go_splitter_0200
    EXPORTING
      parent  = go_dialog_0200
*     PARENT  = GO_DOCKING_0200
      rows    = 2
      columns = 1.
  "----// 상단 컨테이너 (Header)
  CALL METHOD go_splitter_0200->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = go_container_0200_1.
  "----// 하단 컨테이너 (ALV)
  CALL METHOD go_splitter_0200->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = go_container_0200_2.
  "----// 상단 높이
  CALL METHOD go_splitter_0200->set_row_height
    EXPORTING
      id     = 1
      height = 10.
  "----// 헤더 텍스트
  PERFORM header_sub.
  "----// 하단 컨테이너 위에 ALV 그리드 배치
  CREATE OBJECT go_grid_0200
    EXPORTING
      i_parent = go_container_0200_2.
  "----// Variant
  PERFORM alv_variant CHANGING gs_variant_0200.
  "----// 레이아웃
  PERFORM alv_layout_0200  CHANGING gs_layout_0200.
  "----// 툴바 제외 버튼
  PERFORM alv_toolbar_exclude TABLES gt_exclude_0200.
  "----// 필드카탈로그
  PERFORM alv_fieldcatalog_0200 TABLES gt_fcat_0200.
  "----// 이벤트 등록
  PERFORM alv_event_200 USING go_grid_0200
                              go_dialog_0200.
  "----// 그리드
  IF p_mont EQ 'X'.
    lv_fnm = 'GT_IREQ_W'.
  ELSE.
    lv_fnm = 'GT_IREQ_M'.
  ENDIF.
  ASSIGN (lv_fnm) TO <fs_tab>.
  PERFORM alv_display TABLES gt_fcat_0200
                             gt_sort_0200
                             <fs_tab>[]
                      USING  gs_layout_0200
                             gs_variant_0200
                             gt_exclude_0200
                             go_grid_0200.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_FIELDCATALOG_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_FCAT_0200
*&---------------------------------------------------------------------*
FORM alv_fieldcatalog_0200  TABLES pt_fcat LIKE gt_fcat_0100.
  DATA: lt_fieldcatalog TYPE lvc_t_fcat,
        ls_fieldcatalog LIKE LINE OF lt_fieldcatalog.
  CLEAR pt_fcat.
  IF p_mont EQ 'X'.
    PERFORM append_catalog_0200 TABLES pt_fcat USING:
      'X'  'X'  'MATNR'       TEXT-c11  'MVKE'       'MATNR'        '     '  '     '  '10'  'L',
      ' '  ' '  'PLNMG_W00'   TEXT-w00  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W01'   TEXT-w01  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W02'   TEXT-w02  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W03'   TEXT-w03  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W04'   TEXT-w04  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W05'   TEXT-w05  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W06'   TEXT-w06  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W07'   TEXT-w07  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W08'   TEXT-w08  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W09'   TEXT-w09  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W10'   TEXT-w10  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W11'   TEXT-w11  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W12'   TEXT-w12  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W13'   TEXT-w13  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W14'   TEXT-w14  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_W15'   TEXT-w15  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'MEINS'       TEXT-c20  'PBED'       'MEINS'        '     '  '     '  '05'  'L'.
  ELSE.
    PERFORM append_catalog_0200 TABLES pt_fcat USING:
      'X'  'X'  'MATNR'       TEXT-c11  'MVKE'       'MATNR'        '     '  '     '  '10'  'L',
      ' '  ' '  'PLNMG_M00'   TEXT-m00  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M01'   TEXT-m01  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M02'   TEXT-m02  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M03'   TEXT-m03  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M04'   TEXT-m04  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M05'   TEXT-m05  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M06'   TEXT-m06  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M07'   TEXT-m07  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M08'   TEXT-m08  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M09'   TEXT-m09  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M10'   TEXT-m10  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'PLNMG_M11'   TEXT-m11  'PBED'       'PLNMG'        'MEINS'  '     '  '10'  'R',
      ' '  ' '  'MEINS'       TEXT-c20  'PBED'       'MEINS'        '     '  '     '  '05'  'L'.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_LAYOUT_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GS_LAYOUT_0200
*&---------------------------------------------------------------------*
FORM alv_layout_0200  CHANGING ps_layout LIKE gs_layout_0100.
  CLEAR ps_layout.
  ps_layout-zebra      = 'X'.        "LINE COLOR
  ps_layout-cwidth_opt = ' '.        "ALV 제어: 열너비최적화
  ps_layout-no_rowmark = ' '.        "행선택 가능
  ps_layout-info_fname = 'INFO'.     "ROW COLOR.
  ps_layout-ctab_fname = 'CELLSCOL'. "CELL COLOR.
  ps_layout-stylefname = 'CELLSTYL'. "CELL STYLE
  ps_layout-sel_mode   = 'D'.
  ps_layout-edit       = ' '.        "편집가능
  ps_layout-excp_fname = ' '.
  ps_layout-no_toolbar = 'X'.
  ps_layout-excp_led   = ' '.        "'X' = display LED, else traffic lights
ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_CATALOG_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_FCAT
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> TEXT_C11
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM append_catalog_0200  TABLES pt_fcat LIKE gt_fcat_0100
                    USING  "VALUE(P_COL_POS)
                           VALUE(p_key)
                           VALUE(p_fix_column)
                           VALUE(p_fieldname)
                           VALUE(p_coltext)
                           VALUE(p_ref_table)
                           VALUE(p_ref_field)
                           VALUE(p_qfieldname)
                           VALUE(p_cfieldname)
                           VALUE(p_outputlen)
                           VALUE(p_just).
  DATA: ls_fieldcat TYPE lvc_s_fcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-col_pos    = lines( pt_fcat[] ) + 1.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-fix_column = p_fix_column.
  ls_fieldcat-fieldname  = p_fieldname.
  ls_fieldcat-reptext    = p_coltext.
  ls_fieldcat-coltext    = p_coltext.
  ls_fieldcat-scrtext_l  = p_coltext.
  ls_fieldcat-scrtext_m  = p_coltext.
  ls_fieldcat-scrtext_s  = p_coltext.
  ls_fieldcat-ref_table  = p_ref_table.
  ls_fieldcat-ref_field  = p_ref_field.
  ls_fieldcat-qfieldname = p_qfieldname.
  ls_fieldcat-cfieldname = p_cfieldname.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-just       = p_just.

  APPEND ls_fieldcat TO pt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PERIOD_MAP_TO_WEEK
*&---------------------------------------------------------------------*
*& 시스템기간 --> 주차
*&---------------------------------------------------------------------*
*&      --> GT_PER_MAP_TO_WEEK
*&---------------------------------------------------------------------*
FORM period_map_to_week TABLES pt_per_map_to_week LIKE gt_per_map_to_week.
  "----// 변수 선언
  DATA: lv_date     TYPE scal-date,
        lv_ident    TYPE tfacd-ident,
        lv_hocid    TYPE tfacd-hocid,
        lv_week00   TYPE scal-week,
        lv_week99   TYPE scal-week,
        lv_weekend  TYPE scal-week,
        lv_perxx    TYPE perxx,
        lv_idx      TYPE numc2,
        lt_holidays TYPE TABLE OF iscal_day WITH HEADER LINE.
  "----// 변수 초기화
  CLEAR: lv_date, lv_ident, lv_hocid, lv_week00, lv_weekend, lt_holidays[], lv_perxx, lv_idx, lt_holidays.
  CLEAR: pt_per_map_to_week[].
  "----// 해당월 1일 변수 세팅
  CONCATENATE p_gjahr p_vrsio+1(2) '01' INTO lv_date.
  "----// 해당월의 첫번째 워킹데이 찾기
  SELECT SINGLE
         land1
    INTO lv_ident
    FROM t001w
   WHERE werks EQ p_werks.
  SELECT SINGLE
         hocid
    INTO lv_hocid
    FROM tfacd
   WHERE ident EQ lv_ident.
  DO 100 TIMES.
    CLEAR lt_holidays[].
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar           = lv_ident
        factory_calendar           = lv_hocid
        date_from                  = lv_date
        date_to                    = lv_date
      TABLES
        holidays                   = lt_holidays
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.
    IF sy-subrc EQ 0.
      IF lt_holidays[] IS INITIAL.
        EXIT.
      ELSE.
        lv_date = lv_date + 1.
        CONTINUE.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  "----// 해당 버전의 첫번째 주 찾기 lv_week00
  CLEAR lv_week00.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = lv_date
    IMPORTING
      week         = lv_week00
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CHECK 1 = 1.
  ENDIF.
  "----// 해당 해의 마지막 날 주 찾기 lv_weekend
  CONCATENATE p_gjahr '1231' INTO lv_date.
  CLEAR lv_weekend.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = lv_date
    IMPORTING
      week         = lv_weekend
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    CHECK 1 = 1.
  ENDIF.
  "----// 해당 버전의 마지막 주 찾기 lv_week00
  lv_week99 = lv_week00 + 15.
  IF lv_week99 > lv_weekend.
    lv_week99(4)   = lv_week99(4) + 1.
    lv_week99+4(2) = lv_week99+4(2) - lv_weekend+4(2).
  ENDIF.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 맵핑 정보 만들기
  "////
  "/////////////////////////////////////////////////////////////////////
  lv_perxx = lv_week00.
  DO 16 TIMES.
    IF lv_perxx(4) EQ p_gjahr AND lv_perxx > lv_week99.
      lv_perxx+0(4) = p_gjahr + 1.
      lv_perxx+4(2) = '01'.
    ENDIF.
    pt_per_map_to_week-perxx = lv_perxx.
    pt_per_map_to_week-wkidx = lv_idx.
    APPEND pt_per_map_to_week.
    CLEAR pt_per_map_to_week.
    lv_perxx = lv_perxx + 1.
    lv_idx = lv_idx + 1.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form WEEK_MAP_TO_PERIOD
*&---------------------------------------------------------------------*
*& 주차 --> 시스템기간
*&---------------------------------------------------------------------*
*&      --> GT_PER_MAP_TO_WEEK
*&---------------------------------------------------------------------*
FORM week_map_to_period TABLES pt_week_map_to_per LIKE gt_week_map_to_per.
  LOOP AT gt_per_map_to_week INTO wa_per_map_to_week.
    MOVE-CORRESPONDING wa_per_map_to_week TO pt_week_map_to_per.
    APPEND pt_week_map_to_per.
    CLEAR pt_week_map_to_per.
  ENDLOOP.
ENDFORM.
