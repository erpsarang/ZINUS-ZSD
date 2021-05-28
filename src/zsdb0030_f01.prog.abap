*&---------------------------------------------------------------------*
*& Include          ZSD1R0040_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM initialization .

  DATA : l_functxt TYPE smp_dyntxt.

  l_functxt-icon_id   = icon_xls.
  l_functxt-icon_text = TEXT-002.
  l_functxt-quickinfo = TEXT-002.
  sscrfields-functxt_01 = l_functxt.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM select_file  CHANGING p_file.

  DATA : l_title TYPE string,
         l_rc    TYPE sy-subrc,
         l_len   TYPE i,
         lt_file TYPE filetable WITH HEADER LINE.

  CLEAR   : l_rc, lt_file, p_file, l_len.
  REFRESH : lt_file.

  l_title = 'Select Upload File'.

*-- File open dialog
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = l_title
      file_filter             = cl_gui_frontend_services=>filetype_excel
    CHANGING
      file_table              = lt_file[]
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF NOT lt_file[] IS INITIAL.
*-- The length of file path should be in 128
      READ TABLE lt_file INDEX 1.
      IF sy-subrc EQ 0.
        l_len = strlen( lt_file ).
        IF l_len GE 128.
          MESSAGE s000 WITH 'File path to be uploaded is too long.'
                       DISPLAY LIKE c_e.
          LEAVE LIST-PROCESSING.
        ELSE.
          p_file = lt_file.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE s000 WITH 'File was not selected.'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCEL_LAYOUT_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM excel_layout_output .

  DATA: lo_container1  TYPE REF TO cl_gui_container,
        lo_control     TYPE REF TO i_oi_container_control,
        lo_document    TYPE REF TO i_oi_document_proxy,
        lo_error       TYPE REF TO i_oi_error OCCURS 0
                                      WITH HEADER LINE,
        lv_retcode     TYPE soi_ret_string,
        lo_spreadsheet TYPE REF TO i_oi_spreadsheet,
        lv_initial     TYPE c.

  DATA: lt_doc_table      LIKE w3mime OCCURS 0,
        lv_doc_size       TYPE i,
        lv_doc_type(80)   VALUE soi_doctype_excel_sheet,
        lv_doc_format(80) TYPE c.

  DATA: lt_fields_table  LIKE rfc_fields OCCURS 0 WITH HEADER LINE,
        lt_fields_table1 LIKE rfc_fields OCCURS 0 WITH HEADER LINE,
        ls_fields_table  TYPE rfc_fields,
        lv_tabname       LIKE x030l-tabname.

  DATA: lv_handle         TYPE cntl_handle.

*  CLEAR : GV_ANSWER.
*  PERFORM POPUP_TO_CONFIRM USING TEXT-002 TEXT-M01
*                           CHANGING GV_ANSWER.
*
*  CHECK GV_ANSWER = '1'.

  CALL METHOD c_oi_container_control_creator=>get_container_control
    IMPORTING
      control = lo_control
      retcode = lv_retcode.

  IF lv_retcode NE c_oi_errors=>ret_ok.
    MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-e01.
    EXIT.
  ENDIF.

  CALL METHOD lo_control->init_control
    EXPORTING
      r3_application_name     = TEXT-003
      inplace_enabled         = ''
      register_on_close_event = 'X'
      parent                  = lo_container1
    IMPORTING
      retcode                 = lv_retcode.

  IF lv_retcode NE c_oi_errors=>ret_ok.
    MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-e01.
    EXIT.
  ENDIF.

  CALL METHOD lo_control->get_document_proxy
    EXPORTING
      document_type   = 'Excel.Sheet'
      document_format = 'OLE'
    IMPORTING
      document_proxy  = lo_document
      retcode         = lv_retcode.

  IF lv_retcode NE c_oi_errors=>ret_ok.
    MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-e01.
    EXIT.
  ENDIF.

* WEB 저장소의 화일 LOAD
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING
      object_id        = 'ZSD1R0040'
    IMPORTING
      data_size        = lv_doc_size
      document_format  = lv_doc_format
      document_type    = lv_doc_type
    TABLES
      data_table       = lt_doc_table
    EXCEPTIONS
      object_not_found = 1
      internal_error   = 2
      OTHERS           = 3.

* GET DOCUMENT
  CHECK lv_doc_size > 0.
  CALL METHOD lo_control->get_document_proxy
    EXPORTING
      document_type  = lv_doc_type
      no_flush       = 'X'
    IMPORTING
      document_proxy = lo_document
      retcode        = lv_retcode.

  IF lv_retcode <> c_oi_errors=>ret_ok.
    MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-e01.
    EXIT.
  ENDIF.

  CALL METHOD lo_document->open_document_from_table
    EXPORTING
      document_table = lt_doc_table[]
      document_size  = lv_doc_size
    IMPORTING
      retcode        = lv_retcode.

  IF  lv_retcode <> c_oi_errors=>ret_ok.
    MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-e01.
    EXIT.
  ENDIF.

* GET HANDLE
  CALL METHOD lo_document->get_document_handle
    EXPORTING
      no_flush = 'X'
    IMPORTING
      handle   = lv_handle.

* SET EXCEL FULL 화면
  PERFORM set_ole2_process USING lv_handle.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM popup_to_confirm  USING    p_title
                                p_text
                       CHANGING p_answer.

  CLEAR : p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = p_title
      text_question  = p_text
      default_button = '1'
    IMPORTING
      answer         = p_answer.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_OLE2_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_ole2_process USING p_handle TYPE cntl_handle.

  DATA: lv_window      TYPE ole2_object,
        lv_application TYPE ole2_object.

* Excel Application 속성 얻기
  GET PROPERTY OF p_handle-obj   'APPLICATION'  = lv_application .
  SET PROPERTY OF lv_application 'WINDOWSTATE'  = -4137. "최대화

* Excel Application에서 ActiveWindow 속성 얻기
  GET PROPERTY OF lv_application 'ACTIVEWINDOW' = lv_window .
  SET PROPERTY OF lv_window      'WINDOWSTATE'  = -4137. "최대화

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAIN_PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM main_process .

  PERFORM read_excel_data.
  PERFORM get_disp_data.
  PERFORM get_extra_data.
  PERFORM set_disp_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_EXCEL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM read_excel_data .

  DATA: lt_intern LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.
  DATA: lv_col    TYPE kcd_ex_col_n.
  DATA: l_type    TYPE c.
  FIELD-SYMBOLS <fs> TYPE any.

  PERFORM PROGRAM_USAGE IN PROGRAM ZSD1R0010.

  CLEAR: gt_excel, gt_excel[].
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 256
      i_end_row               = 20000
    TABLES
      intern                  = lt_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc NE 0.
    sy-msgty = 'S'.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    LEAVE LIST-PROCESSING.
  ELSE.

    LOOP AT lt_intern.
      AT NEW row.
        CLEAR gt_excel.
      ENDAT.

      lv_col = lt_intern-col.

      ASSIGN COMPONENT lv_col OF STRUCTURE gt_excel TO <fs>.
      DESCRIBE FIELD <fs> TYPE l_type.
      IF l_type = 'D'.
        REPLACE ALL OCCURRENCES OF '-' IN lt_intern-value WITH ' '.
        REPLACE ALL OCCURRENCES OF '/' IN lt_intern-value WITH ' '.
        REPLACE ALL OCCURRENCES OF '.' IN lt_intern-value WITH ' '.
        CONDENSE lt_intern-value NO-GAPS.
      ENDIF.
      <fs> = lt_intern-value.

      AT END OF row.
        APPEND gt_excel.
      ENDAT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DISP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_disp_data .

  CLEAR : gt_disp, gt_disp[].

  CHECK gt_excel[] IS NOT INITIAL.

  LOOP AT gt_excel.
    MOVE-CORRESPONDING gt_excel TO gt_disp.

    " Data Conversion
    IF gt_disp-kunnr IS NOT INITIAL.
      PERFORM conversion_exit_alpha_input USING gt_excel-kunnr
                                          CHANGING gt_disp-kunnr.
    ENDIF.

    IF gt_disp-matnr IS NOT INITIAL.
      PERFORM conversion_exit_matn1_input USING gt_excel-matnr
                                          CHANGING gt_disp-matnr.
    ENDIF.

    IF gt_disp-kdmat IS NOT INITIAL.
      PERFORM conversion_exit_matn1_input USING gt_excel-kdmat
                                          CHANGING gt_disp-kdmat.
    ENDIF.
    APPEND gt_disp.
    CLEAR gt_disp.
  ENDLOOP.

  SORT gt_disp BY kunnr vkorg vtweg matnr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM conversion_exit_alpha_input  USING p_input
                                  CHANGING p_output.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_input
    IMPORTING
      output = p_output.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERSION_EXIT_MATN1_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM conversion_exit_matn1_input  USING p_input
                                  CHANGING p_output.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = p_input
    IMPORTING
      output       = p_output
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EXTRA_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_extra_data .

  DATA : lt_temp TYPE TABLE OF ty_disp WITH HEADER LINE.

  CLEAR : gt_knvv, gt_knvv[], gt_mvke, gt_mvke[].

  CHECK gt_disp[] IS NOT INITIAL.

  " Check Customer
  CLEAR : lt_temp, lt_temp[].
  lt_temp[] = gt_disp[].
  SORT lt_temp BY kunnr vkorg vtweg.
  DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING kunnr vkorg vtweg.

  IF lt_temp[] IS NOT INITIAL.
    SELECT kunnr vkorg vtweg spart
      INTO CORRESPONDING FIELDS OF TABLE gt_knvv
      FROM knvv
      FOR ALL ENTRIES IN lt_temp
      WHERE kunnr = lt_temp-kunnr
        AND vkorg = lt_temp-vkorg
        AND vtweg = lt_temp-vtweg
        AND spart = '00'.
  ENDIF.

  " Check ZINUS SKU
  CLEAR : lt_temp, lt_temp[].
  lt_temp[] = gt_disp[].
  SORT lt_temp BY matnr vkorg vtweg.
  DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING matnr vkorg vtweg.

  IF lt_temp[] IS NOT INITIAL.
    SELECT matnr vkorg vtweg
      INTO CORRESPONDING FIELDS OF TABLE gt_mvke
      FROM mvke
      FOR ALL ENTRIES IN lt_temp
      WHERE matnr = lt_temp-matnr
        AND vkorg = lt_temp-vkorg
        AND vtweg = lt_temp-vtweg.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DISP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_disp_data .

  LOOP AT gt_disp.

    " Check Customer
    READ TABLE gt_knvv WITH KEY kunnr = gt_disp-kunnr
                                vkorg = gt_disp-vkorg
                                vtweg = gt_disp-vtweg.
    IF sy-subrc <> 0.
      gt_disp-icon = icon_led_red.
      gt_disp-message = TEXT-e02.
    ENDIF.

    " Check ZINUS SKU
    READ TABLE gt_mvke WITH KEY matnr = gt_disp-matnr
                                vkorg = gt_disp-vkorg
                                vtweg = gt_disp-vtweg.
    IF sy-subrc <> 0.
      gt_disp-icon = icon_led_red.
      gt_disp-message = TEXT-e03.
    ENDIF.

    MODIFY gt_disp.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM display .

  DESCRIBE TABLE gt_disp LINES sy-tfill.
  IF sy-tfill = 0.
    MESSAGE s002 DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE s006 WITH sy-tfill.
    CALL SCREEN 0100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_OBJECTS_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_objects_0100  USING p_docking TYPE REF TO cl_gui_docking_container
                             p_grid    TYPE REF TO lcl_alv_grid.

* CREATE DOCKING
  CREATE OBJECT p_docking
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      extension = 2000.

  CREATE OBJECT p_grid
    EXPORTING
      i_parent = p_docking.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM fill_field_category .

  CLEAR : gt_fcat1.

  PERFORM set_fill_field_category USING :
                'S'    'FIELDNAME'    'ICON',
                ' '    'REF_TABLE'    'ICON',
                ' '    'REF_FIELD'    'ID',
                ' '    'KEY'          'X',
                ' '    'JUST'         'C',
                'E'    'COLTEXT'      TEXT-a01,

                'S'    'FIELDNAME'    'KUNNR',
                ' '    'REF_TABLE'    'KNMT',
                ' '    'REF_FIELD'    'KUNNR',
                ' '    'KEY'          'X',
                'E'    'COLTEXT'      TEXT-a02,

                'S'    'FIELDNAME'    'VKORG',
                ' '    'REF_TABLE'    'KNMT',
                ' '    'REF_FIELD'    'VKORG',
                ' '    'KEY'          'X',
                'E'    'COLTEXT'      TEXT-a03,

                'S'    'FIELDNAME'    'VTWEG',
                ' '    'REF_TABLE'    'KNMT',
                ' '    'REF_FIELD'    'VTWEG',
                ' '    'KEY'          'X',
                'E'    'COLTEXT'      TEXT-a04,

                'S'    'FIELDNAME'    'MATNR',
                ' '    'REF_TABLE'    'KNMT',
                ' '    'REF_FIELD'    'MATNR',
                ' '    'KEY'          'X',
                'E'    'COLTEXT'      TEXT-a05,

                'S'    'FIELDNAME'    'KDMAT',
                ' '    'REF_TABLE'    'KNMT',
                ' '    'REF_FIELD'    'MATNR',
                'E'    'COLTEXT'      TEXT-a06,

                'S'    'FIELDNAME'    'CLASS',
                ' '    'REF_TABLE'    'RMCLF',
                ' '    'REF_FIELD'    'CLASS',
                'E'    'COLTEXT'      TEXT-a07,

                'S'    'FIELDNAME'    'MWERT',
                ' '    'REF_TABLE'    'RCTMS',
                ' '    'REF_FIELD'    'MWERT',
                'E'    'COLTEXT'      TEXT-a08,

                'S'    'FIELDNAME'    'MESSAGE',
                'E'    'COLTEXT'      TEXT-a09.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_fill_field_category  USING p_gub TYPE any
                                                                 p_fname  TYPE  any
                                                                 p_con  TYPE  any.

  DATA  l_col(40).
  FIELD-SYMBOLS  <fs>  TYPE  any.

  CONCATENATE 'GS_FCAT1-' p_fname INTO  l_col.
  ASSIGN      (l_col)                 TO   <fs>.
  MOVE         p_con                  TO   <fs>.

  IF p_gub = c_e.
    APPEND gs_fcat1 TO gt_fcat1.
    CLEAR gs_fcat1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EXCLUDE_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_exclude_toolbar .

  REFRESH gt_toolbar1. CLEAR gt_toolbar1.

  PERFORM append_fcode USING :
*-- 불필요 icon 제거.
                  cl_gui_alv_grid=>mc_fc_detail,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_copy,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_loc_paste,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row,
*                  cl_gui_alv_grid=>mc_fc_filter,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_help,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_auf,
*                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_find,
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
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_delete_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_copy,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_paste,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row,
                  cl_gui_alv_grid=>mc_fc_loc_undo.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FCODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM append_fcode  USING p_ucomm.

  APPEND p_ucomm TO gt_toolbar1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_layout  USING ps_layo TYPE lvc_s_layo.

  ps_layo-zebra      = 'X'.
  ps_layo-no_rowins  = 'X'.
  ps_layo-no_rowmove = 'X'.
  ps_layo-cwidth_opt = 'X'.
  ps_layo-sel_mode   = 'A'.
  ps_layo-ctab_fname   = 'CTAB'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_VARIANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_variant .

  gs_variant-report = sy-repid.
  gs_variant-username = sy-uname.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM create_event_receiver  USING p_grid TYPE REF TO lcl_alv_grid.

  p_grid->register_edit_event( p_grid->mc_evt_enter ).
  p_grid->register_edit_event( p_grid->mc_evt_modified ).

  CREATE OBJECT gcl_receiver.
*  SET HANDLER GCL_RECEIVER->HANDLE_TOP_OF_PAGE FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_TOOLBAR        FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_USER_COMMAND   FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_DATA_CHANGED   FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR P_GRID.
  SET HANDLER gcl_receiver->handle_double_click   FOR p_grid.
*  SET HANDLER GCL_RECEIVER->HANDLE_HOTSPOT_CLICK  FOR P_GRID.
*  SET HANDLER GCL_RECEIVER->HANDLE_ONF4           FOR P_GRID.

  " 엔터만 입력해도 'handle_data_changed'을 수행한다.
  CALL METHOD p_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  " 셀을 이동해도 이벤트를 수행하도록.
  CALL METHOD p_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_display  USING p_grid TYPE REF TO lcl_alv_grid
                       ps_layo    TYPE lvc_s_layo
                       pt_toolbar TYPE ui_functions
                       p_tabname
                       pt_fcat    TYPE lvc_t_fcat
                       pt_sort    TYPE lvc_t_sort.
*
  FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
  DATA l_tabname TYPE slis_tabname.
  CLEAR l_tabname.
  l_tabname = p_tabname.
  ASSIGN (l_tabname) TO <table>.

  CALL METHOD p_grid->set_table_for_first_display
    EXPORTING
      is_variant           = gs_variant
      i_save               = c_a
      is_layout            = ps_layo
      it_toolbar_excluding = pt_toolbar[]
    CHANGING
      it_outtab            = <table>
      it_fieldcatalog      = pt_fcat[]
      it_sort              = pt_sort[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM refresh_table_display  USING p_grid TYPE REF TO lcl_alv_grid.

* Refresh
  DATA l_refresh TYPE lvc_s_stbl.

  l_refresh-row = 'X'.
  l_refresh-col = 'X'.

  CHECK p_grid IS NOT INITIAL.

  CALL METHOD p_grid->refresh_table_display
    EXPORTING
      is_stable = l_refresh
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

  p_grid->u_optimize_all_cols( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM create_process .

*>> 라인 선택
  PERFORM get_selected_row.
  CHECK gv_err IS INITIAL.

*>> POP-UP
  PERFORM popup_to_confirm USING TEXT-m01 TEXT-m02
                                 CHANGING gv_answer.
  CHECK gv_answer = '1'.

*>> BDC
  PERFORM bdc_vd51.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SELECTED_ROW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_selected_row .

  DATA : lt_rows TYPE lvc_t_row,
         ls_rows TYPE lvc_s_row,
         l_line  TYPE i.

  CLEAR : gv_err, lt_rows, ls_rows, l_line.
  CALL METHOD gcl_alv1->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  DESCRIBE TABLE lt_rows LINES l_line.

  IF l_line = 0.
    gv_err = 'X'.
    MESSAGE s003 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  gt_disp-chk = space.
  MODIFY gt_disp TRANSPORTING chk WHERE chk IS NOT INITIAL.

  LOOP AT lt_rows INTO ls_rows.
    READ TABLE gt_disp INDEX ls_rows-index.
    IF sy-subrc = 0.
      IF gt_disp-icon = space.
        gt_disp-chk = 'X'.
        MODIFY gt_disp INDEX ls_rows-index.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_VD51
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM bdc_vd51 .

  DATA : lt_temp TYPE TABLE OF ty_disp WITH HEADER LINE,
         lt_knmt TYPE TABLE OF knmt WITH HEADER LINE.

  CLEAR : gs_ctu_params, lt_knmt, lt_knmt[], gv_tabix,
          gv_cnt, gv_scnt, gv_ecnt.

  " 생성 여부
  CLEAR : lt_temp, lt_temp[].
  lt_temp[] = gt_disp[].
  DELETE lt_temp WHERE chk = space.
  DESCRIBE TABLE lt_temp LINES gv_cnt.

  IF lt_temp[] IS NOT INITIAL.
    SELECT kunnr vkorg vtweg matnr
      INTO CORRESPONDING FIELDS OF TABLE lt_knmt
      FROM knmt
      FOR ALL ENTRIES IN lt_temp
      WHERE kunnr = lt_temp-kunnr
        AND vkorg = lt_temp-vkorg
        AND vtweg = lt_temp-vtweg
        AND matnr = lt_temp-matnr.
  ENDIF.

** BDC MODE
  gs_ctu_params-dismode = 'N'.
  gs_ctu_params-updmode = 'S'.
  gs_ctu_params-racommit = 'X'.
  gs_ctu_params-nobinpt = 'X'.

** BDC DATA SET
  LOOP AT gt_disp WHERE chk = 'X'.

    gv_tabix = sy-tabix.

    CLEAR : gt_bdcdata, gt_bdcdata[], gt_bdcmsg, gt_bdcmsg[],
            gv_msg.

    PERFORM bdcdata_set USING :
      'X'  'SAPMV10A'         '0100',
      ' '  'MV10A-KUNNR'      gt_disp-kunnr,
      ' '  'MV10A-VKORG'      gt_disp-vkorg,
      ' '  'MV10A-VTWEG'      gt_disp-vtweg,
      ' '  'BDC_OKCODE'       '=ENT1'.

    PERFORM bdcdata_set USING :
      'X'  'SAPMV10A'         '0200',
      ' '  'MV10A-MATNR(01)'  gt_disp-matnr,
      ' '  'MV10A-KDMAT(01)'  gt_disp-kdmat,
      ' '  'BDC_OKCODE'       '/00'.

    PERFORM bdcdata_set USING :
      'X'  'SAPMV10A'         '0200',
      ' '  'BDC_OKCODE'       '=KLAS'.

    " 신규생성
    READ TABLE lt_knmt WITH KEY kunnr = gt_disp-kunnr
                                vkorg = gt_disp-vkorg
                                vtweg = gt_disp-vtweg
                                matnr = gt_disp-matnr.
    IF sy-subrc <> 0.
      PERFORM bdcdata_set USING :
        'X'  'SAPLCLFM'         '1101',
        ' '  'RMCLF-CLASS(01)'  gt_disp-class,
        ' '  'BDC_OKCODE'       '/00'.
    ENDIF.

    PERFORM bdcdata_set USING :
      'X'  'SAPLCLFM'         '1101',
      ' '  'RCTMS-MWERT(01)'  gt_disp-mwert,
      ' '  'BDC_OKCODE'       '/00'.

    PERFORM bdcdata_set USING :
      'X'  'SAPLCLFM'         '1101',
      ' '  'BDC_OKCODE'       '=ENDE'.

    PERFORM bdcdata_set USING :
      'X'  'SAPMV10A'         '0200',
      ' '  'BDC_OKCODE'       '=SICH'.

** CALL TRANSACTION
    CLEAR : gt_bdcmsg, gt_bdcmsg[].
    CALL TRANSACTION 'VD51'
               USING gt_bdcdata
               OPTIONS FROM gs_ctu_params
               MESSAGES INTO gt_bdcmsg.

    READ TABLE gt_bdcmsg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = gt_bdcmsg-msgid
          msgnr               = gt_bdcmsg-msgnr
          msgv1               = gt_bdcmsg-msgv1
          msgv2               = gt_bdcmsg-msgv2
          msgv3               = gt_bdcmsg-msgv3
          msgv4               = gt_bdcmsg-msgv4
        IMPORTING
          message_text_output = gv_msg.
      ADD 1 TO gv_ecnt.
      gt_disp-icon = icon_led_red.
      gt_disp-message = gv_msg.
    ELSE.
      ADD 1 TO gv_scnt.
      CLEAR : gt_disp-message.
      gt_disp-icon = icon_led_green.
    ENDIF.

    MODIFY gt_disp INDEX gv_tabix.
  ENDLOOP.

  MESSAGE s007 WITH gv_cnt gv_scnt gv_ecnt.

  gt_disp-chk = space.
  MODIFY gt_disp TRANSPORTING chk WHERE chk IS NOT INITIAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDCDATA_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM bdcdata_set  USING p_start p_object p_value.

  CLEAR gt_bdcdata.

  IF p_start = 'X'.
    gt_bdcdata-dynbegin = p_start.
    gt_bdcdata-program = p_object.
    gt_bdcdata-dynpro = p_value.
  ELSE.
    gt_bdcdata-fnam = p_object.
    gt_bdcdata-fval = p_value.
  ENDIF.

  APPEND gt_bdcdata.
  CLEAR gt_bdcdata.

ENDFORM.
