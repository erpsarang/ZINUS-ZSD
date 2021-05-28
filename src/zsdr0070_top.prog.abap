*&---------------------------------------------------------------------*
*& Include          ZSDR0070_TOP
*&---------------------------------------------------------------------*
TABLES: VTTK,                        "Shipmentt Header
        SSCRFIELDS.                  "선택화면의 필드
*----// ALV MAIN
TYPES: BEGIN OF TY_MAIN,
         FKNUM    TYPE VFKK-FKNUM, "Shipment Cost Number
         STBER    TYPE VFKK-STBER, "Status of calculation (shipment costs header data)
         STFRE    TYPE VFKK-STFRE, "Status of account determination (shipment costs header data)
         STABR    TYPE VFKK-STABR, "Status of transfer (shipment costs header data)
         TKNUM    TYPE VTTK-TKNUM, "Shipment Number
         ADD02    TYPE VTTK-ADD02, "Trailer overall length
         STTRG    TYPE VTTK-STTRG, "Overall transportation status
         DDTEXT   TYPE VAL_TEXT,   "Overall transportation status TEXT
         TDLNR    TYPE VTTK-TDLNR, "Number of forwarding agent
*         vbeln    TYPE vttp-vbeln, "Delivery No.
         KUNWE    TYPE LIKP-KUNNR, "Ship-to party
         ZFR4P    TYPE KONP-KBETR, "Base Freight 기본운임
         ZFR5P    TYPE KONP-KBETR, "Surchage     추가운임
         ZFR8P    TYPE KONP-KBETR, "Via          경유운임
         ZFR9P    TYPE KONP-KBETR, "Etc Fee      기타운임
         SUMMP    TYPE KONP-KBETR, "Summation    합계
         KONWA    TYPE KONP-KONWA, "Condition unit (currency or percentage) 통화키
         KPEIN    TYPE KONP-KPEIN, "Condition Pricing Unit 가격단위
         KMEIN    TYPE KONP-KMEIN, "Condition Unit 수량단위
         EBELN    TYPE VFKP-EBELN, "PO
         EBELP    TYPE VFKP-EBELP, "PO Item
*S_2021/2/25 ADD VENDOR I/V BY E00064
         EXTI2    TYPE VFKK-EXTI2, "Vendor I/V
*         SGTXT    TYPE BSEG-SGTXT, "VENDOR I/V.
*E_2021/2/25 ADD VENDOR I/V BY E00064
         BELNR    TYPE EKBE-BELNR, "IR-L No.
         GJAHR    TYPE EKBE-GJAHR, "Fiscal year
         BUDAT    TYPE EKBE-BUDAT, "Post date
         LBLNI    TYPE VFKP-LBLNI, "Entry Sheet Number
         RETCD    TYPE CHAR01,     "리턴코드
         RETXT    TYPE BAPIRET2-MESSAGE,  "리턴텍스트
         STATUS   TYPE CHAR01,     "신호등
         CELLSTYL TYPE LVC_T_STYL, "셀스타일
         CELLSCOL TYPE LVC_T_SCOL, "셀컬러
       END OF TY_MAIN.
DATA : GT_MAIN TYPE TABLE OF TY_MAIN,
       WA_MAIN TYPE          TY_MAIN.
*----// 글로벌 변수      .
DATA : GV_ANSWER  TYPE C.          "팝업 사용자 클릭 버튼
DATA : OK_CODE  TYPE SY-UCOMM,     "OKCODE
       GV_RETCD TYPE CHAR01,       "리턴코드
       GV_RETXT TYPE TEXT100,      "리턴텍스트
       GV_CHGED TYPE C.            "필드 변경 플래그
*----// ALV 변수
DATA : GO_DOCKING_0100          TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA : GO_SPLITTER_0100         TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA : GO_CONTAINER_0100_1      TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_CONTAINER_0100_2      TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_GRID_0100             TYPE REF TO CL_GUI_ALV_GRID.
DATA : GS_LAYOUT_0100           TYPE LVC_S_LAYO.
DATA : GS_VARIANT_0100          TYPE DISVARIANT.
DATA : GT_EXCLUDE_0100          TYPE UI_FUNCTIONS.
DATA : GT_SORT_0100             TYPE TABLE OF LVC_S_SORT.
DATA : GT_FCAT_0100             TYPE LVC_T_FCAT.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
DATA : GO_EVENT_RECEIVER_0100   TYPE REF TO LCL_EVENT_RECEIVER. "ALVGRID EVENT용
*----// 숫자 컨버전
SELECT SINGLE DCPFM FROM USR01 INTO @DATA(GV_DCPFM) WHERE BNAME = @SY-UNAME.
