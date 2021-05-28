*&---------------------------------------------------------------------*
*& Include          ZSDR0040_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 1.                               "엑셀양식 다운로드 버튼 week
SELECTION-SCREEN FUNCTION KEY 2.                               "엑셀양식 다운로드 버튼 month
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.  "[ 모드 ]
SELECTION-SCREEN : BEGIN OF LINE.
PARAMETERS: p_file RADIOBUTTON GROUP rd1 USER-COMMAND uc1 DEFAULT 'X'.  "파일 업로드
SELECTION-SCREEN : COMMENT 11(10) TEXT-011 FOR FIELD p_file.
SELECTION-SCREEN POSITION 31.
PARAMETERS: p_data RADIOBUTTON GROUP rd1.                               "데이터 편집
SELECTION-SCREEN : COMMENT 32(10) TEXT-012 FOR FIELD p_data.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-015.  "[ 기간 ]
SELECTION-SCREEN : BEGIN OF LINE.
PARAMETERS: p_mont RADIOBUTTON GROUP rd2 USER-COMMAND uc1 DEFAULT 'X'.  "월계획
SELECTION-SCREEN : COMMENT 11(10) TEXT-013 FOR FIELD p_mont.
SELECTION-SCREEN POSITION 31.
PARAMETERS: p_year RADIOBUTTON GROUP rd2.                               "년계획
SELECTION-SCREEN : COMMENT 32(10) TEXT-014 FOR FIELD p_year.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.  "[ 데이터 ]
PARAMETERS: p_vkorg LIKE s804-vkorg OBLIGATORY MEMORY ID vko,     "Sales Org.
            p_werks LIKE s804-werks OBLIGATORY MEMORY ID wrk,     "Plant
            p_kschl LIKE t685a-kschl OBLIGATORY DEFAULT 'PR00',   "조건 유형 2016.01.18
            p_vrsio TYPE vrsio OBLIGATORY,                        "계획버전
            p_gjahr TYPE gjahr OBLIGATORY.                        "계획연도
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.  "[ 선택조건 ]
PARAMETERS: p_fname TYPE text1024 OBLIGATORY DEFAULT 'C:\' MODIF ID m01.  "File Path
SELECT-OPTIONS: s_vtweg FOR s804-vtweg MODIF ID m02,             "Channel
                s_kunnr FOR s804-kunnr MODIF ID m02,             "Sold to party
                s_matnr FOR s804-matnr MODIF ID m02,             "Material
                s_vkaus FOR s804-vkaus MODIF ID m02.             "Usage
SELECTION-SCREEN END OF BLOCK b3.
