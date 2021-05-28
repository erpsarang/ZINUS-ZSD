*&---------------------------------------------------------------------*
*& Include          ZSDR0060_TOP
*&---------------------------------------------------------------------*

*---ALV
DATA : OK_CODE TYPE SY-UCOMM.
DATA : SAVE_OK LIKE SY-UCOMM.

DATA : GR_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER,
       GR_ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
       GT_FCAT              TYPE LVC_T_FCAT,
       GS_FCAT              TYPE LVC_S_FCAT.

DATA : GS_LAYOUT TYPE LVC_S_LAYO.
DATA : GS_VARIANT TYPE DISVARIANT.
DATA : GT_SORT TYPE LVC_T_SORT.
DATA : GT_EXCLUDE TYPE UI_FUNCTIONS.

DATA : GT_EXCLUDING TYPE UI_FUNCTIONS.
DATA : GS_EXCLUDING TYPE UI_FUNC.

DATA : GR_SPLITTER      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       GR_EASY_SPLITTER TYPE REF TO CL_GUI_EASY_SPLITTER_CONTAINER,
       GR_PARENT_HTML   TYPE REF TO CL_GUI_CONTAINER,
       GR_PARENT_GRID   TYPE REF TO CL_GUI_CONTAINER,
       GR_HTML          TYPE REF TO CL_GUI_HTML_VIEWER,
       GR_DOCUMENT      TYPE REF TO CL_DD_DOCUMENT.

DATA: ET_INDEX_ROWS TYPE LVC_T_ROW,
      ES_INDEX_ROWS TYPE LVC_S_ROW,
      ET_ROW_NO     TYPE LVC_T_ROID,
      ES_ROW_NO     TYPE LVC_S_ROID.

DATA : GS_ROW  TYPE LVC_S_ROW,
       GT_ROWS TYPE LVC_T_ROW.

DATA : GV_ANSWER.
DATA : GV_SWITCH.

DATA : GS_FUNCTEXT TYPE SMP_DYNTXT.

*---IT
DATA : BEGIN OF GT_DATA OCCURS 0,
         ICON(30),
         MSG(150),
         TDLNR    LIKE A908-TDLNR,      "Service Agent
         TDLNR_T  LIKE LFA1-NAME1,      "Service Agent
         ADD02    LIKE A908-ADD02,      "Suppl. 2 : Trailer overall length
         KUNWE    LIKE A908-KUNWE,      "Shipto
         KUNWE_T  LIKE KNA1-NAME1,
*         KSCHL    LIKE A907-KSCHL,      "Condition type
*         KSCHL_T  LIKE T685T-VTEXT,     "Condition type
         KBETR    LIKE KONP-KBETR,      "Base Freight Amount
         KBETR1   LIKE KONP-KBETR,      "Surcharge Amount
         KONWA    LIKE KONP-KONWA,      "Currency
         DATAB    LIKE A908-DATAB,      "Validity start date
         DATBI    LIKE A908-DATBI,      "Validity end date
         DATAB1   LIKE A908-DATAB,      "Base Freight Validity start date
         DATBI1   LIKE A908-DATBI,      "Surcharge Validity end date
       END OF GT_DATA.

* For Surcharge Amount
DATA : BEGIN OF GT_DATA1 OCCURS 0,
*         ICON(30),
*         MSG(150),
         TDLNR    LIKE A908-TDLNR,      "Service Agent
         TDLNR_T  LIKE LFA1-NAME1,      "Service Agent
         ADD02    LIKE A908-ADD02,      "Trailer overall length
         KUNWE    LIKE A908-KUNWE,      "Shipto
         KUNWE_T  LIKE KNA1-NAME1,
*         KSCHL    LIKE A907-KSCHL,      "Condition type
*         KSCHL_T  LIKE T685T-VTEXT,     "Condition type
         KBETR    LIKE KONP-KBETR,      "Base Freight Amount
         KBETR1   LIKE KONP-KBETR,      "Surcharge Amount
         KONWA    LIKE KONP-KONWA,      "Currency
         DATAB    LIKE A908-DATAB,      "Validity start date
         DATBI    LIKE A908-DATBI,      "Validity end date
         DATAB1   LIKE A908-DATAB,      "Base Freight Validity start date
         DATBI1   LIKE A908-DATBI,      "Surcharge Validity end date
       END OF GT_DATA1.




*DATA : BEGIN OF gt_data OCCURS 0,
*         icon(30),
*         msg(100),
*         kunwe    LIKE a922-kunwe,      "Shipto
*         kunwe_t  LIKE kna1-name1,
*         kschl    LIKE a922-kschl,      "Condition type
*         kschl_t  LIKE t685t-vtext,
*         kbetr    LIKE konp-kbetr,      "Amount
*         konwa    LIKE konp-konwa,      "Currency
*         kpein    LIKE konp-kpein,     "Condition Pricing Unit
*         kmein    LIKE konp-kmein,      "Condition unit
*         kmein_t  LIKE t006a-msehl,
*         datab    LIKE a922-datab,      "Validity start date
*         datbi    LIKE a922-datbi,      "Validity end date
*       END OF gt_data.

*---Text tab
DATA: GT_KNA1  TYPE TABLE OF KNA1 WITH HEADER LINE,
      GT_LFA1  TYPE TABLE OF LFA1 WITH HEADER LINE,
      GT_T685T TYPE TABLE OF T685T WITH HEADER LINE,
      GT_T006A TYPE TABLE OF T006A WITH HEADER LINE.

*---EXCEL UPLOAD
CONSTANTS : GV_SOI_DOCTYPE_WORD97_DOCUMENT(15) TYPE C
                                               VALUE  'Word.Document.8'.
DATA: I_XLS_DATA TYPE STANDARD TABLE OF ALSMEX_TABLINE WITH HEADER LINE,
      I_S_COL    TYPE I,         " EXCEL SHEET에서 실제 DATA가 위치한 COL.
      I_S_ROW    TYPE I,         " EXCEL SHEET에서 실제 DATA가 위치한 ROW.
      I_E_COL    TYPE I,         " EXCEL SHEET의 총 COL.
      I_E_ROW    TYPE I.         " EXCEL SHEET의 총 ROW.
FIELD-SYMBOLS: <I_FS0>,
               <I_FS1>.

DATA: BEGIN OF GT_UPLOAD OCCURS 0,
        TDLNR    LIKE A908-TDLNR,      "Service Agent
        ADD02    LIKE A908-ADD02,      "Suppl. 2 : Trailer overall length
        KUNWE    LIKE A908-KUNWE,      "Shipto
        KBETR    LIKE KONP-KBETR,      "Base Freight Amount
        KBETR1   LIKE KONP-KBETR,      "Amount
        KONWA    LIKE KONP-KONWA,      "Currency
        DATAB    LIKE A908-DATAB,      "Base Freight Validity start date
        DATBI    LIKE A908-DATBI,      "Base Freight Validity end date
        DATAB1   LIKE A908-DATAB,      "Surcharge Validity start date
        DATBI1   LIKE A908-DATBI,      "Surcharge Validity end date
      END OF GT_UPLOAD.


*DATA: BEGIN OF gt_upload OCCURS 0,
*        kunwe LIKE a922-kunwe,      "Shipto
*        kschl LIKE a922-kschl,      "Condition type
*        kbetr LIKE konp-kbetr,      "Amount
*        konwa LIKE konp-konwa,      "Currency
*        kpein LIKE konp-kpein,      "Condition Pricing Unit
*        kmein LIKE konp-kmein,      "Condition unit
*        datab LIKE a922-datab,      "Validity start date
*        datbi LIKE a922-datbi,      "Validity end date
*      END OF gt_upload.

DATA: G_FILENAME(80),
      G_FILENAME1 LIKE RLGRAP-FILENAME.

DATA: DOC_TABLE      LIKE W3MIME OCCURS 0,
      DOC_SIZE       TYPE I,
      DOC_TYPE(80)   VALUE SOI_DOCTYPE_EXCEL_SHEET,
      DOC_FORMAT(80) TYPE C.
*---BDC
DATA : CTU_PARAMS LIKE CTU_PARAMS,
       GT_BDC     LIKE TABLE OF BDCDATA WITH HEADER LINE,
       GT_MSG     LIKE TABLE OF BDCMSGCOLL WITH HEADER LINE.

************************************************************************
* Constants
************************************************************************
CONSTANTS:
  C_SPACE          TYPE C        VALUE ' ',
  C_NON            TYPE C        VALUE '',
  C_LANGU_KR       TYPE C        VALUE '3',
  C_LANGU_EN(2)    TYPE C        VALUE 'EN',
  C_YES(1)         TYPE C        VALUE '1',
  C_NO(1)          TYPE C        VALUE '2',
  C_CANC(1)        TYPE C        VALUE 'A',
  C_USD(3)         TYPE C        VALUE 'USD',
  C_BTN_TXT_YES(3) TYPE C        VALUE 'YES',
  C_BTN_TXT_NO(3)  TYPE C        VALUE 'NO',
  C_FC01(4)        TYPE C        VALUE 'FC01',
  C_XLS(3)         TYPE C        VALUE 'C:/',
  C_BT(2)          TYPE C        VALUE 'BT',
  C_M3(2)          TYPE C        VALUE 'M3',
  C_0(1)           TYPE C        VALUE '0',
  C_1(1)           TYPE C        VALUE '1',
  C_2(1)           TYPE C        VALUE '2',
  C_3(1)           TYPE C        VALUE '3',
  C_4(1)           TYPE C        VALUE '4',
  C_5(1)           TYPE C        VALUE '5',
  C_6(1)           TYPE C        VALUE '6',
  C_7(1)           TYPE C        VALUE '7',
  C_8(1)           TYPE C        VALUE '8',
  C_9(1)           TYPE C        VALUE '9',
  C_00(2)          TYPE C        VALUE '00',
  C_01(2)          TYPE C        VALUE '01',
  C_02(2)          TYPE C        VALUE '02',
  C_03(2)          TYPE C        VALUE '03',
  C_04(2)          TYPE C        VALUE '04',
  C_05(2)          TYPE C        VALUE '05',
  C_06(2)          TYPE C        VALUE '06',
  C_07(2)          TYPE C        VALUE '07',
  C_08(2)          TYPE C        VALUE '08',
  C_09(2)          TYPE C        VALUE '09',
  C_10(2)          TYPE C        VALUE '10',
  C_11(2)          TYPE C        VALUE '11',
  C_12(2)          TYPE C        VALUE '12',
  C_13(2)          TYPE C        VALUE '13',
  C_14(2)          TYPE C        VALUE '14',
  C_15(2)          TYPE C        VALUE '15',
  C_16(2)          TYPE C        VALUE '16',
  C_17(2)          TYPE C        VALUE '17',
  C_18(2)          TYPE C        VALUE '18',
  C_19(2)          TYPE C        VALUE '19',
  C_20(2)          TYPE C        VALUE '20',
  C_21(2)          TYPE C        VALUE '21',
  C_23(2)          TYPE C        VALUE '23',
  C_24(2)          TYPE C        VALUE '24',
  C_25(2)          TYPE C        VALUE '25',
  C_30(2)          TYPE C        VALUE '30',
  C_31(2)          TYPE C        VALUE '31',
  C_33(2)          TYPE C        VALUE '33',
  C_35(2)          TYPE C        VALUE '35',
  C_37(2)          TYPE C        VALUE '37',
  C_40(2)          TYPE C        VALUE '40',
  C_47(2)          TYPE C        VALUE '47',
  C_49(2)          TYPE C        VALUE '49',
  C_50(2)          TYPE C        VALUE '50',
  C_60(2)          TYPE C        VALUE '60',
  C_62(2)          TYPE C        VALUE '62',
  C_63(2)          TYPE C        VALUE '63',
  C_68(2)          TYPE C        VALUE '68',
  C_69(2)          TYPE C        VALUE '69',
  C_70(2)          TYPE C        VALUE '70',
  C_71(2)          TYPE C        VALUE '71',
  C_72(2)          TYPE C        VALUE '72',
  C_73(2)          TYPE C        VALUE '73',
  C_74(2)          TYPE C        VALUE '74',
  C_75(2)          TYPE C        VALUE '75',
  C_76(2)          TYPE C        VALUE '76',
  C_80(2)          TYPE C        VALUE '80',
  C_90(2)          TYPE C        VALUE '90',
  C_95(2)          TYPE C        VALUE '95',
  C_99(2)          TYPE C        VALUE '99',
  C_000(3)         TYPE C        VALUE '000',
  C_001(3)         TYPE C        VALUE '001',
  C_002(3)         TYPE C        VALUE '002',
  C_003(3)         TYPE C        VALUE '003',
  C_004(3)         TYPE C        VALUE '004',
  C_100(3)         TYPE C        VALUE '100',
  C_110(3)         TYPE C        VALUE '110',
  C_120(3)         TYPE C        VALUE '120',
  C_2011(4)        TYPE C        VALUE '2011',
  C_A(1)           TYPE C        VALUE 'A',
  C_B(1)           TYPE C        VALUE 'B',
  C_C(1)           TYPE C        VALUE 'C',
  C_D(1)           TYPE C        VALUE 'D',
  C_E(1)           TYPE C        VALUE 'E',
  C_F(1)           TYPE C        VALUE 'F',
  C_G(1)           TYPE C        VALUE 'G',
  C_H(1)           TYPE C        VALUE 'H',
  C_I(1)           TYPE C        VALUE 'I',
  C_J(1)           TYPE C        VALUE 'J',
  C_K(1)           TYPE C        VALUE 'K',
  C_L(1)           TYPE C        VALUE 'L',
  C_M(1)           TYPE C        VALUE 'M',
  C_N(1)           TYPE C        VALUE 'N',
  C_O(1)           TYPE C        VALUE 'O',
  C_P(1)           TYPE C        VALUE 'P',
  C_Q(1)           TYPE C        VALUE 'Q',
  C_R(1)           TYPE C        VALUE 'R',
  C_S(1)           TYPE C        VALUE 'S',
  C_T(1)           TYPE C        VALUE 'T',
  C_U(1)           TYPE C        VALUE 'U',
  C_V(1)           TYPE C        VALUE 'V',
  C_W(1)           TYPE C        VALUE 'W',
  C_X(1)           TYPE C        VALUE 'X',
  C_Y(1)           TYPE C        VALUE 'Y',
  C_Z(1)           TYPE C        VALUE 'Z'.
