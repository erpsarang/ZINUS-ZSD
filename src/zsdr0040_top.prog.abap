*&---------------------------------------------------------------------*
*& Include          ZSDR0040_TOP
*&---------------------------------------------------------------------*
TABLES: s804,                        "usa mfg mon v1
        s805,                        "usa mfg year v1
        sscrfields.                  "선택화면의 필드
*----// 엑셀양식 다운로드
DATA: gv_answer   TYPE c.            "팝업 사용자 클릭 버튼
DATA: gv_dn_fname TYPE string.       "엑셀 다운로드 파일명 디폴트
DATA: gv_dn_fpath TYPE string.       "엑셀 다운로드 파일명 사용자 선택
DATA: gv_rtn_code TYPE c.            "엑셀양식 레이아웃 다운로드 리턴코드
*----// 엑셀 업로드
TYPES: BEGIN OF ty_fdata_w,          "---- 월 계획을 주단위로 입력
         vtweg      TYPE string,     "Distribution Channel
         kunag      TYPE string,     "Sold to party
         kunwe      TYPE string,     "Ship to party
         werks      TYPE string,     "Plant
         matnr      TYPE string,     "Material
         vkaus      TYPE string,     "Usage
         kwmeng_w00 TYPE string,     "Quantity w00
         kwmeng_w01 TYPE string,     "Quantity w01
         kwmeng_w02 TYPE string,     "Quantity w02
         kwmeng_w03 TYPE string,     "Quantity w03
         kwmeng_w04 TYPE string,     "Quantity w04
         kwmeng_w05 TYPE string,     "Quantity w05
         kwmeng_w06 TYPE string,     "Quantity w06
         kwmeng_w07 TYPE string,     "Quantity w07
         kwmeng_w08 TYPE string,     "Quantity w08
         kwmeng_w09 TYPE string,     "Quantity w09
         kwmeng_w10 TYPE string,     "Quantity w10
         kwmeng_w11 TYPE string,     "Quantity w11
         kwmeng_w12 TYPE string,     "Quantity w12
         kwmeng_w13 TYPE string,     "Quantity w13
         kwmeng_w14 TYPE string,     "Quantity w14
         kwmeng_w15 TYPE string,     "Quantity w15
       END OF ty_fdata_w,
       BEGIN OF ty_fdata_m,          "---- 년 계획을 월단위로 입력
         vtweg      TYPE string,     "Distribution Channel
         kunag      TYPE string,     "Sold to party
         kunwe      TYPE string,     "Ship to party
         werks      TYPE string,     "Plant
         matnr      TYPE string,     "Material
         vkaus      TYPE string,     "Usage
         kwmeng_m00 TYPE string,     "Quantity m00
         kwmeng_m01 TYPE string,     "Quantity m01
         kwmeng_m02 TYPE string,     "Quantity m02
         kwmeng_m03 TYPE string,     "Quantity m03
         kwmeng_m04 TYPE string,     "Quantity m04
         kwmeng_m05 TYPE string,     "Quantity m05
         kwmeng_m06 TYPE string,     "Quantity m06
         kwmeng_m07 TYPE string,     "Quantity m07
         kwmeng_m08 TYPE string,     "Quantity m08
         kwmeng_m09 TYPE string,     "Quantity m09
         kwmeng_m10 TYPE string,     "Quantity m10
         kwmeng_m11 TYPE string,     "Quantity m11
       END OF ty_fdata_m,
       BEGIN OF ty_main_w,
*         vkorg      TYPE vkorg,      "Sales Org.
         vtweg      TYPE vtweg,      "Distribution Channel
         vtext      TYPE tvtwt-vtext, "Short Name
         kunag      TYPE kunag,      "Sold-to party
         name1      TYPE kna1-name1, "Sold-to party Name
         kunwe      TYPE kunwe,      "Ship-to party
         name2      TYPE kna1-name1, "Ship-to party Name
         werks      TYPE werks_ext,  "Plant
         name3      TYPE name1,      "Plant name
         matnr      TYPE matnr,      "Material
         maktx      TYPE maktx,      "Material description
         vkaus      TYPE abrvw,      "Usage Indicator
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
         netwr_w00  TYPE netwr_ak,   "Net Value w00
         netwr_w01  TYPE netwr_ak,   "Net Value w00
         netwr_w02  TYPE netwr_ak,   "Net Value w00
         netwr_w03  TYPE netwr_ak,   "Net Value w00
         netwr_w04  TYPE netwr_ak,   "Net Value w00
         netwr_w05  TYPE netwr_ak,   "Net Value w00
         netwr_w06  TYPE netwr_ak,   "Net Value w00
         netwr_w07  TYPE netwr_ak,   "Net Value w00
         netwr_w08  TYPE netwr_ak,   "Net Value w00
         netwr_w09  TYPE netwr_ak,   "Net Value w00
         netwr_w10  TYPE netwr_ak,   "Net Value w00
         netwr_w11  TYPE netwr_ak,   "Net Value w00
         netwr_w12  TYPE netwr_ak,   "Net Value w00
         netwr_w13  TYPE netwr_ak,   "Net Value w00
         netwr_w14  TYPE netwr_ak,   "Net Value w00
         netwr_w15  TYPE netwr_ak,   "Net Value w00
         planp      TYPE kbetr_kond, "Plan Condition amount (83.80)
         kbetr      TYPE kbetr_kond, "Condition amount (83.80)
         konwa      TYPE konwa,      "Currency (USD)
         kpein      TYPE kpein,      "Condition Pricing Unit (1)
         kmein      TYPE kmein,      "Condition Unit (EA)
         vrkme      TYPE vrkme,      "Sales unit
         waerk      TYPE waerk,      "SD document currency
         retcd      TYPE char01,     "리턴코드
         retxt      TYPE bapiret2-message,  "리턴텍스트
         sdicon     TYPE icon-id,    "영업계획 아이콘
         ppicon     TYPE icon-id,    "생산계획 아이콘
         status     TYPE char01,     "신호등
         cellstyl   TYPE lvc_t_styl, "셀스타일
         cellscol   TYPE lvc_t_scol, "셀컬러
       END OF ty_main_w,
       BEGIN OF ty_main_m,
*         vkorg      TYPE vkorg,      "Sales Org.
         vtweg      TYPE vtweg,      "Distribution Channel
         vtext      TYPE tvtwt-vtext, "Short Name
         kunag      TYPE kunag,      "Sold-to party
         name1      TYPE kna1-name1, "Sold-to party Name
         kunwe      TYPE kunwe,      "Ship-to party
         name2      TYPE kna1-name1, "Ship-to party Name
         werks      TYPE werks_ext,  "Plant
         name3      TYPE name1,      "Plant name
         matnr      TYPE matnr,      "Material
         maktx      TYPE maktx,      "Material description
         vkaus      TYPE abrvw,      "Usage Indicator
         kwmeng_m00 TYPE kwmeng,     "Quantity w00
         kwmeng_m01 TYPE kwmeng,     "Quantity w01
         kwmeng_m02 TYPE kwmeng,     "Quantity w02
         kwmeng_m03 TYPE kwmeng,     "Quantity w03
         kwmeng_m04 TYPE kwmeng,     "Quantity w04
         kwmeng_m05 TYPE kwmeng,     "Quantity w05
         kwmeng_m06 TYPE kwmeng,     "Quantity w06
         kwmeng_m07 TYPE kwmeng,     "Quantity w07
         kwmeng_m08 TYPE kwmeng,     "Quantity w08
         kwmeng_m09 TYPE kwmeng,     "Quantity w09
         kwmeng_m10 TYPE kwmeng,     "Quantity w10
         kwmeng_m11 TYPE kwmeng,     "Quantity w11
         netwr_m00  TYPE netwr_ak,   "Net Value w00
         netwr_m01  TYPE netwr_ak,   "Net Value w00
         netwr_m02  TYPE netwr_ak,   "Net Value w00
         netwr_m03  TYPE netwr_ak,   "Net Value w00
         netwr_m04  TYPE netwr_ak,   "Net Value w00
         netwr_m05  TYPE netwr_ak,   "Net Value w00
         netwr_m06  TYPE netwr_ak,   "Net Value w00
         netwr_m07  TYPE netwr_ak,   "Net Value w00
         netwr_m08  TYPE netwr_ak,   "Net Value w00
         netwr_m09  TYPE netwr_ak,   "Net Value w00
         netwr_m10  TYPE netwr_ak,   "Net Value w00
         netwr_m11  TYPE netwr_ak,   "Net Value w00
         planp      TYPE kbetr_kond, "Plan Condition amount (83.80)
         kbetr      TYPE kbetr_kond, "Condition amount (83.80)
         konwa      TYPE konwa,      "Currency (USD)
         kpein      TYPE kpein,      "Condition Pricing Unit (1)
         kmein      TYPE kmein,      "Condition Unit (EA)
         vrkme      TYPE vrkme,      "Sales unit
         waerk      TYPE waerk,      "SD document currency
         retcd      TYPE char01,     "리턴코드
         retxt      TYPE bapiret2-message,  "리턴텍스트
         sdicon     TYPE icon-id,    "영업계획 아이콘
         ppicon     TYPE icon-id,    "생산계획 아이콘
         status     TYPE char01,     "신호등
         cellstyl   TYPE lvc_t_styl, "셀스타일
         cellscol   TYPE lvc_t_scol, "셀컬러
       END OF ty_main_m.
DATA : gt_fdata_w     TYPE STANDARD TABLE OF ty_fdata_w,
       wa_fdata_w     TYPE ty_fdata_w,
       gt_fdata_m     TYPE STANDARD TABLE OF ty_fdata_m,
       wa_fdata_m     TYPE ty_fdata_m,
       gt_main_w      TYPE STANDARD TABLE OF ty_main_w,
       gt_main_w_temp TYPE STANDARD TABLE OF ty_main_w,
       gt_del_w       TYPE STANDARD TABLE OF ty_main_w,
       wa_del_w       TYPE ty_main_w,
       wa_main_w      TYPE ty_main_w,
       gt_main_m      TYPE STANDARD TABLE OF ty_main_m,
       gt_main_m_temp TYPE STANDARD TABLE OF ty_main_m,
       gt_del_m       TYPE STANDARD TABLE OF ty_main_m,
       wa_del_m       TYPE ty_main_m,
       wa_main_m      TYPE ty_main_m.
"----// 생산 DM 조회용
TYPES: BEGIN OF ty_ireq_w,
         matnr     TYPE matnr,
         werks     TYPE werks_d,
         plnmg_w00 TYPE plnmg,
         plnmg_w01 TYPE plnmg,
         plnmg_w02 TYPE plnmg,
         plnmg_w03 TYPE plnmg,
         plnmg_w04 TYPE plnmg,
         plnmg_w05 TYPE plnmg,
         plnmg_w06 TYPE plnmg,
         plnmg_w07 TYPE plnmg,
         plnmg_w08 TYPE plnmg,
         plnmg_w09 TYPE plnmg,
         plnmg_w10 TYPE plnmg,
         plnmg_w11 TYPE plnmg,
         plnmg_w12 TYPE plnmg,
         plnmg_w13 TYPE plnmg,
         plnmg_w14 TYPE plnmg,
         plnmg_w15 TYPE plnmg,
         meins     TYPE meins,
         status    TYPE char01,     "신호등
         cellstyl  TYPE lvc_t_styl, "셀스타일
         cellscol  TYPE lvc_t_scol, "셀컬러
       END OF ty_ireq_w,
       BEGIN OF ty_ireq_m,
         matnr     TYPE matnr,
         werks     TYPE werks_d,
         plnmg_m00 TYPE plnmg,
         plnmg_m01 TYPE plnmg,
         plnmg_m02 TYPE plnmg,
         plnmg_m03 TYPE plnmg,
         plnmg_m04 TYPE plnmg,
         plnmg_m05 TYPE plnmg,
         plnmg_m06 TYPE plnmg,
         plnmg_m07 TYPE plnmg,
         plnmg_m08 TYPE plnmg,
         plnmg_m09 TYPE plnmg,
         plnmg_m10 TYPE plnmg,
         plnmg_m11 TYPE plnmg,
         meins     TYPE meins,
         status    TYPE char01,     "신호등
         cellstyl  TYPE lvc_t_styl, "셀스타일
         cellscol  TYPE lvc_t_scol, "셀컬러
       END OF ty_ireq_m.
DATA : gt_ireq_w TYPE STANDARD TABLE OF ty_ireq_w,
       wa_ireq_w TYPE                   ty_ireq_w,
       gt_ireq_m TYPE STANDARD TABLE OF ty_ireq_m,
       wa_ireq_m TYPE                   ty_ireq_m.
*----// 글로벌 변수      .
DATA : ok_code         TYPE sy-ucomm,     "OKCODE
       gv_file_checked TYPE c,            "파일 존재 확인
       gv_retcd        TYPE char01,       "리턴코드
       gv_retxt        TYPE text100,      "리턴텍스트
       gv_chged        TYPE c,            "필드 변경 플래그
       gv_deleted      TYPE c,            "라인 삭제 플래그
       gv_spmon        TYPE s804-spmon,   "월계획 고정값
       gv_stwae        TYPE t445a-stwae,  "Statistics Currency
       gv_basme        TYPE t445a-basme.  "Base Unit of Measure
DATA : gv_ucomm        TYPE sy-ucomm.
"----// 기간-주차 맵핑 ★★★★★
TYPES: BEGIN OF ty_week_map_to_per,
         wkidx TYPE numc2,
         perxx TYPE perxx,
       END OF ty_week_map_to_per,
       BEGIN OF ty_per_map_to_week,
         perxx TYPE perxx,
         wkidx TYPE numc2,
       END OF ty_per_map_to_week.
DATA : gt_week_map_to_per TYPE TABLE OF ty_week_map_to_per,
       wa_week_map_to_per TYPE          ty_week_map_to_per,
       gt_per_map_to_week TYPE TABLE OF ty_per_map_to_week,
       wa_per_map_to_week TYPE          ty_per_map_to_week.
*----// ALV 변수
"---- 영업계획 메인
DATA : go_docking_0100          TYPE REF TO cl_gui_docking_container.
DATA : go_splitter_0100         TYPE REF TO cl_gui_splitter_container.
DATA : go_container_0100_1      TYPE REF TO cl_gui_container.
DATA : go_container_0100_2      TYPE REF TO cl_gui_container.
DATA : go_grid_0100             TYPE REF TO cl_gui_alv_grid.
DATA : gs_layout_0100           TYPE lvc_s_layo.
DATA : gs_variant_0100          TYPE disvariant.
DATA : gt_exclude_0100          TYPE ui_functions.
DATA : gt_sort_0100             TYPE TABLE OF lvc_s_sort.
DATA : gt_fcat_0100             TYPE lvc_t_fcat.
"----// 독립소요량 팝업
DATA : go_docking_0200          TYPE REF TO cl_gui_docking_container.
DATA : go_splitter_0200         TYPE REF TO cl_gui_splitter_container.
DATA : go_container_0200_1      TYPE REF TO cl_gui_container.
DATA : go_container_0200_2      TYPE REF TO cl_gui_container.
DATA : go_grid_0200             TYPE REF TO cl_gui_alv_grid.
DATA : gs_layout_0200           TYPE lvc_s_layo.
DATA : gs_variant_0200          TYPE disvariant.
DATA : gt_exclude_0200          TYPE ui_functions.
DATA : gt_sort_0200             TYPE TABLE OF lvc_s_sort.
DATA : gt_fcat_0200             TYPE lvc_t_fcat.

CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA : go_event_receiver_0100   TYPE REF TO lcl_event_receiver. "ALVGRID EVENT용
DATA : go_event_receiver_0200   TYPE REF TO lcl_event_receiver. "ALVGRID EVENT용

"2021/03/19 추가 s
DATA : go_dialog_0200           TYPE REF TO cl_gui_dialogbox_container.

CLASS lcl_event_handler DEFINITION DEFERRED.
DATA : go_event_handler  TYPE REF TO lcl_event_handler.
"2021/03/19 추가 e

*----// 숫자 컨버전
SELECT SINGLE dcpfm
  FROM usr01
  INTO @DATA(gv_dcpfm)
  WHERE bname = @sy-uname.
