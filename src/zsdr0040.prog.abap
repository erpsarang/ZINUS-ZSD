**&---------------------------------------------------------------------*
**& Report ZSDR0040
**&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program ID  : ZSDR0040                                               *
* Title       : Sales Plan Management, DM Transfer - LIS               *
* Author      : Han Sang Yeul                                          *
* Create Date : 2020.10.22                                             *
* T_CODE      : ZSDR0040                                               *
* Description : 판매계획 등록 및 관리, DM 전송이                       *
*               하나의 화면에서 처리되도록 프로그램 구현 필요          *
* Interface   :                                                        *
*----------------------------------------------------------------------*
* Date         Authors        Description                              *
*----------------------------------------------------------------------*
* 2020.10.22   Han Sang Yeul  Initial Release                          *
*----------------------------------------------------------------------*
REPORT zsdr0040 MESSAGE-ID zmsd NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE zsdr0040_top.
INCLUDE zsdr0040_scr.
INCLUDE zsdr0040_cls.
INCLUDE zsdr0040_pbo.
INCLUDE zsdr0040_pai.
INCLUDE zsdr0040_f01.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init CHANGING p_fname sscrfields.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM set_screen.
*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN F4
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM filename_f4 CHANGING p_fname.
*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON
*&---------------------------------------------------------------------*
*AT SELECTION-SCREEN ON p_fname.
*  PERFORM file_existence_check USING    p_fname
*                               CHANGING gv_file_checked.
*  IF gv_file_checked EQ ' '.
*    MESSAGE s008 DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  "----// Version Check
  IF p_mont EQ 'X'.
    IF p_vrsio+0(1) NE '1'.
      MESSAGE e000 WITH TEXT-e01.  "Version Error'.
    ENDIF.
    IF p_vrsio+1(2) GT '12'.
      MESSAGE e000 WITH TEXT-e01.  "Version Error'.
    ENDIF.
  ELSE.
    IF p_vrsio+0(1) NE '5'.
      MESSAGE e000 WITH TEXT-e01.  "Version Error'.
    ENDIF.
    IF p_vrsio+1(2) LT '20'.
      MESSAGE e000 WITH TEXT-e01.  "Version Error'.
    ENDIF.
    IF p_vrsio+1(2) NE p_gjahr+2(2).
      MESSAGE e000 WITH TEXT-e02.  "Version Error'.
    ENDIF.
  ENDIF.
  CASE sy-ucomm.
    WHEN 'FC01'.
      CLEAR: gv_answer, gv_dn_fname, gv_rtn_code.
      PERFORM pop_up USING    TEXT-022  TEXT-021
                     CHANGING gv_answer.
      IF gv_answer EQ '1'.
        IF p_mont EQ 'X'.
          gv_dn_fname = TEXT-023.  "SALES_PLAN_MONTHLY.xlsx
        ELSE.
          gv_dn_fname = TEXT-024.  "SALES_PLAN_YEARLY.xlsx
        ENDIF.
        PERFORM down_filename   CHANGING gv_dn_fname
                                         gv_dn_fpath.
        IF gv_dn_fpath IS NOT INITIAL.
          PERFORM down_excel_form    USING gv_dn_fpath
                                  CHANGING gv_rtn_code.
          IF gv_rtn_code EQ 'E'.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ELSE.
          CHECK 1 = 1.
        ENDIF.
      ELSE.
        CHECK 1 = 1.
      ENDIF.
    WHEN OTHERS.
      CHECK 1 = 1.
  ENDCASE.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM set_constant_variables CHANGING gv_spmon
                                          gv_stwae
                                          gv_basme.

  PERFORM file_existence_check USING    p_fname
                               CHANGING gv_file_checked.
  IF     p_file EQ 'X'.
    "----// 파일 존재 여부 체크
    IF gv_file_checked EQ ' '.
      MESSAGE s008 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    IF p_mont EQ 'X'. "월계획
      PERFORM upload_excel_data_w TABLES gt_fdata_w
                                   USING p_fname     gv_dcpfm
                                CHANGING gv_retcd   gv_retxt.
      IF gv_retcd EQ 'E'.
        MESSAGE gv_retxt TYPE 'S' DISPLAY LIKE gv_retcd.
      ELSE.
        PERFORM make_monthly_data_w TABLES gt_fdata_w  gt_main_w
                                     USING gv_spmon    gv_stwae   gv_basme.
      ENDIF.
    ELSE. "연계획
      PERFORM upload_excel_data_m TABLES gt_fdata_m
                                   USING p_fname     gv_dcpfm
                                CHANGING gv_retcd   gv_retxt.
      IF gv_retcd EQ 'E'.
        MESSAGE gv_retxt TYPE 'S' DISPLAY LIKE gv_retcd.
      ELSE.
        PERFORM make_yearly_data_m  TABLES gt_fdata_m  gt_main_m
                                     USING gv_spmon    gv_stwae   gv_basme.
      ENDIF.
    ENDIF.
  ELSEIF p_data EQ 'X'.
    IF p_mont EQ 'X'.
      PERFORM select_monthly_data_w TABLES gt_main_w
                                     USING gv_spmon  gv_stwae   gv_basme   'SEL'.
    ELSE.
      PERFORM select_yearly_data_m  TABLES gt_main_m
                                     USING gv_spmon  gv_stwae   gv_basme   'SEL'.
    ENDIF.
  ELSE.
    CHECK 1 = 1.
  ENDIF.
*----------------------------------------------------------------------*
* END-OF-SLELCTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  "----// 기간=>주차 컨버전 정보 추출
  PERFORM period_map_to_week TABLES gt_per_map_to_week.
  PERFORM week_map_to_period TABLES gt_week_map_to_per.

  IF gt_main_w[] IS NOT INITIAL OR gt_main_m[] IS NOT INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE s004.
  ENDIF.
