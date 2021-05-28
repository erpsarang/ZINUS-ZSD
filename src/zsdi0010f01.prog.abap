*&---------------------------------------------------------------------*
*& Include          ZSDI0010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT
*&---------------------------------------------------------------------*
FORM SET_INIT .

  DATA: LS_DYNTXT TYPE SMP_DYNTXT.

* Text of Templet Download button Function key 1
  CLEAR: LS_DYNTXT.
  MOVE: 'ASIN_Code'     TO LS_DYNTXT-TEXT,
        ICON_VIEW_TABLE TO LS_DYNTXT-ICON_ID,
        'ASIN_Code'     TO LS_DYNTXT-ICON_TEXT,
        'ASIN_Code'     TO LS_DYNTXT-QUICKINFO.

  SSCRFIELDS-FUNCTXT_01 = LS_DYNTXT.

  CLEAR GV_LAST_DATE.
  SELECT MAX( SELTSP )
    INTO GV_LAST_DATE
    FROM ZMMT0010
   WHERE RESEND   EQ SPACE
     AND ZSAP_TYP EQ 'S'
     AND ZCONT    EQ '2'
     AND WERKS    IN S_PLANT.

  CONVERT TIME STAMP GV_LAST_DATE TIME ZONE 'UTC'
  INTO DATE P_DATE TIME P_TIME.

  IF P_DATE IS INITIAL.
    P_DATE = '20201231'.
  ENDIF.

  _CLEAR S_CDATE.
  S_CDATE-LOW  = P_DATE.
  S_CDATE-HIGH = SY-DATLO.
  S_CDATE-SIGN = 'I'.
  S_CDATE-OPTION = 'BT'.
  APPEND S_CDATE. CLEAR S_CDATE.

ENDFORM. " SET_INIT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.



ENDFORM. " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND USING P_UCOMM.



ENDFORM. " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& Form POPUP_MSG
*&---------------------------------------------------------------------*
FORM POPUP_MSG USING P_MSG1 P_MSG2 PV_CHECK.

  CLEAR PV_CHECK.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = P_MSG1
      TEXT_QUESTION  = P_MSG2
      TEXT_BUTTON_1  = 'YES'
      TEXT_BUTTON_2  = 'NO'
    IMPORTING
      ANSWER         = PV_CHECK
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING P_ROW_ID
                                 P_COLUMN_ID.

  CLEAR GT_LIST.
  READ TABLE GT_LIST INDEX P_ROW_ID.
  CASE P_COLUMN_ID.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_INPUT
*&---------------------------------------------------------------------*
FORM ALPHA_INPUT  USING    P_DATA.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_DATA
    IMPORTING
      OUTPUT = P_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_OUTPUT
*&---------------------------------------------------------------------*
FORM ALPHA_OUTPUT  USING    P_DATA.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = P_DATA
    IMPORTING
      OUTPUT = P_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM HANDLE_TOOLBAR   USING PE_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                            PE_INTERACTIVE.

  DATA: LS_TOOLBAR TYPE STB_BUTTON.
  DATA: LS_UPLOAD  TYPE STB_BUTTON.

  CLEAR LS_UPLOAD.
  MOVE 3 TO LS_UPLOAD-BUTN_TYPE.
  APPEND LS_UPLOAD TO PE_OBJECT->MT_TOOLBAR.

*  LS_TOOLBAR-FUNCTION  = 'ADD'.
*  LS_TOOLBAR-ICON      = ICON_INSERT_ROW.
*  LS_TOOLBAR-BUTN_TYPE = SPACE.
*  LS_TOOLBAR-DISABLED  = SPACE.
*  LS_TOOLBAR-TEXT      = 'Add Line'.
*  LS_TOOLBAR-QUICKINFO = 'Add Line'.
*  LS_TOOLBAR-CHECKED   = SPACE.
*  APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.
*
*  CLEAR LS_UPLOAD.
*  MOVE 3 TO LS_UPLOAD-BUTN_TYPE.
*  APPEND LS_UPLOAD TO PE_OBJECT->MT_TOOLBAR.
*
*  LS_TOOLBAR-FUNCTION  = 'DELETE'.
*  LS_TOOLBAR-ICON      = ICON_DELETE_ROW.
*  LS_TOOLBAR-BUTN_TYPE = SPACE.
*  LS_TOOLBAR-DISABLED  = SPACE.
*  LS_TOOLBAR-TEXT      = 'Delete Line'.
*  LS_TOOLBAR-QUICKINFO = 'Delete Line'.
*  LS_TOOLBAR-CHECKED   = SPACE.
*  APPEND LS_TOOLBAR TO PE_OBJECT->MT_TOOLBAR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING    PE_ROW
                                   PE_COLUMN.

  DATA: LV_TBLNAME TYPE DD02L-TABNAME.
  DATA: T_REF TYPE REF TO DATA.

  IF GT_LIST[] IS NOT INITIAL.
    CASE PE_COLUMN.
      WHEN 'VBELN'.
        CLEAR : GT_LIST.
        READ TABLE GT_LIST INDEX PE_ROW.
        CHECK SY-SUBRC EQ 0.
        SET PARAMETER ID 'AUN' FIELD GT_LIST-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

      WHEN 'VBELN_I'.
        CLEAR : GT_LIST.
        READ TABLE GT_LIST INDEX PE_ROW.
        CHECK SY-SUBRC EQ 0.
        SET PARAMETER ID 'AUN' FIELD GT_LIST-VBELN_I.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.

  IF GT_9100[] IS NOT INITIAL.
    CLEAR LV_TBLNAME.
    LV_TBLNAME = 'ZSDT0080'.

    UNASSIGN <GT_LOG_D>.
    CREATE DATA T_REF TYPE TABLE OF (LV_TBLNAME).
    ASSIGN T_REF->* TO <GT_LOG_D>.

    READ TABLE GT_9100 INTO DATA(LS_LOG_H) INDEX PE_ROW.
    IF SY-SUBRC EQ 0.
      SELECT *
        INTO TABLE @<GT_LOG_D>
        FROM (LV_TBLNAME)
       WHERE ZCONT EQ @LS_LOG_H-ZCONT
         AND WERKS EQ @LS_LOG_H-WERKS
         AND IFTSP EQ @LS_LOG_H-IFTSP.

      IF <GT_LOG_D>[] IS NOT INITIAL.
        PERFORM SHOW_DETAIL_POPUP USING LV_TBLNAME.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM HANDLE_TOP_OF_PAGE  USING PE_DYNDOC_ID TYPE REF TO CL_DD_DOCUMENT.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form REQUIRED_FIELD_CHECK
*&---------------------------------------------------------------------*
FORM REQUIRED_FIELD_CHECK   USING  PV_DATA PV_TEXT.

  CHECK PV_DATA IS INITIAL.

  IF GT_LIST-MESSAGE IS INITIAL.
    GT_LIST-MESSAGE = '(' && PV_TEXT && ')' && 'is required field'.
    GT_LIST-ICON = ICON_RED_LIGHT.
  ELSE.
    GT_LIST-MESSAGE = GT_LIST-MESSAGE && '/' && '(' && PV_TEXT && ')'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .

  DATA: L_STAMP_LOW  TYPE TIMESTAMPL,
        L_STAMP_HIGH TYPE TIMESTAMPL.


  IF P_RESEND = 'X'.
    CLEAR : L_STAMP_LOW, L_STAMP_HIGH.
    CONVERT DATE S_CDATE-LOW
            INTO TIME STAMP L_STAMP_LOW TIME ZONE 'UTC'.

    IF S_CDATE-HIGH IS INITIAL.
      S_CDATE-HIGH = S_CDATE-LOW.
    ENDIF.

    PERFORM SET_NEXT_DAY CHANGING S_CDATE-HIGH.
    CONVERT DATE S_CDATE-HIGH
            INTO TIME STAMP L_STAMP_HIGH TIME ZONE 'UTC'.

    _STAMPL: GR_DATE L_STAMP_LOW L_STAMP_HIGH.
  ELSE.
    CLEAR GV_SEL_TSP.
    PERFORM GET_TIMESTAMP CHANGING GV_SEL_TSP.
    _CLEAR GR_DATE.
    GR_DATE-SIGN = 'I'.
    GR_DATE-OPTION = 'GT'.
    GR_DATE-LOW = GV_LAST_DATE.
    APPEND GR_DATE.
  ENDIF.

  _CLEAR GT_LIST.
  SELECT A~VBELN, A~KUNNR, A~AUART,
         A~AUDAT, A~VDATU, A~WAERK,
         A~VKORG, A~VTWEG, A~SPART,
         A~BUKRS_VF AS BUKRS,
         B~POSNR, B~MATNR, B~ARKTX AS MATNR_TXT,
         B~CHARG, B~ABGRU, B~KDMAT,
         B~WERKS, B~LGORT, B~KWMENG,
         B~VRKME, B~NETPR, B~WAERK AS WAERK_2,
         B~KPEIN, B~KMEIN, B~NETWR,
         B~POSEX, B~NTGEW, B~GEWEI,
         B~BRGEW, B~VOLUM, B~VOLEH,
         B~NTGEW AS NTGEW_G,
         B~BRGEW AS BRGEW_G,
         B~VOLUM AS VOLUM_G,
         B~STAWN AS CCNGN,
         B~PSTYV,
         C~BSTKD, C~ZTERM, C~INCO1, C~INCO2, C~IHREZ,
         D~ZZBIG, D~ZZMID, D~ZZSIN, D~GROES, D~EAN11,
         E~BEZEI AS AUART_TXT,
         F~VTEXT AS VKORG_TXT,
         G~VTEXT AS VTWEG_TXT,
         H~VTEXT AS SPART_TXT,
         J~ZBIGTX,
         K~ZMIDTX,
         L~ZSINGTX,
         M~VTEXT AS ZTERM_TXT,
         N~BUTXT AS BUKRS_TXT,
         O~KUNNR AS KUNWE,
         P~LGOBE AS LGORT_TXT
  INTO CORRESPONDING FIELDS OF TABLE @GT_LIST
  FROM VBAK AS A INNER JOIN VBAP     AS B ON A~VBELN = B~VBELN
                 INNER JOIN VBKD     AS C ON A~VBELN = C~VBELN
                                         AND C~POSNR = '000000'
                 INNER JOIN MARA     AS D ON B~MATNR = D~MATNR
                 INNER JOIN TVAKT    AS E ON A~AUART = E~AUART
                                         AND E~SPRAS = @SY-LANGU
                 INNER JOIN TVKOT    AS F ON A~VKORG = F~VKORG
                                         AND F~SPRAS = @SY-LANGU
                 INNER JOIN TVTWT    AS G ON A~VTWEG = G~VTWEG
                                         AND G~SPRAS = @SY-LANGU
                 INNER JOIN TSPAT    AS H ON A~SPART = H~SPART
                                         AND H~SPRAS = @SY-LANGU
                 LEFT OUTER JOIN ZMDT0180 AS J ON D~ZZBIG = J~ZBIG
                 LEFT OUTER JOIN ZMDT0181 AS K ON D~ZZMID = K~ZMID
                                              AND D~ZZBIG = K~ZBIG
                 LEFT OUTER JOIN ZMDT0182 AS L ON D~ZZSIN = L~ZSING
                                              AND D~ZZMID = L~ZMID
                 INNER JOIN TVZBT    AS M ON C~ZTERM = M~ZTERM
                                         AND M~SPRAS = @SY-LANGU
                 INNER JOIN T001     AS N ON A~BUKRS_VF = N~BUKRS
                 INNER JOIN VBPA     AS O ON B~VBELN = O~VBELN
                                         AND O~POSNR = '000000'
                                         AND O~PARVW = 'WE'
                 LEFT OUTER JOIN T001L   AS P ON B~LGORT = P~LGORT
                                         AND B~WERKS = P~WERKS
  WHERE   A~AUART EQ 'ZOR'
    AND   C~BSTKD IN @S_BSTKD
    AND   A~VBELN IN @S_VBELN
    AND   A~UPD_TMSTMP IN @GR_DATE
    AND   B~WERKS IN @S_PLANT.

  SORT GT_LIST BY VBELN POSNR.

*ERROR로 인해 보내지지 않은 건들 리스트에 합해줌.
  IF P_NOSENT IS NOT INITIAL.
    PERFORM ADD_NOTSENT_SO.
  ENDIF.
  CHECK GT_LIST[] IS NOT INITIAL.

  PERFORM GET_DETAIL_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SELECTION
*&---------------------------------------------------------------------*
FORM SET_SELECTION .

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'Z01'.
      SCREEN-INPUT = 0.
    ENDIF.

    CASE 'X'.
      WHEN P_RESEND OR P_LOG.
        IF SCREEN-GROUP1 EQ 'Z02'.
          SCREEN-INPUT = 1.
        ENDIF.
      WHEN OTHERS.
        IF SCREEN-GROUP1 EQ 'Z02'.
          SCREEN-INPUT =  0.
        ENDIF.
    ENDCASE.

*    CASE P_RESEND.
*      WHEN 'X'.
*        IF SCREEN-GROUP1 EQ 'Z02'.
*          SCREEN-INPUT = 1.
*        ENDIF.
*      WHEN SPACE.
*        IF SCREEN-GROUP1 EQ 'Z02'.
*          SCREEN-INPUT =  0.
*        ENDIF.
*    ENDCASE.

    IF P_DATE IS INITIAL.
      IF SCREEN-NAME = 'S_CDATE-LOW'.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DETAIL_DATA
*&---------------------------------------------------------------------*
FORM GET_DETAIL_DATA .

  DATA : BEGIN OF LT_DELE OCCURS 0,
           OBJECTID LIKE CDHDR-OBJECTID,
         END OF LT_DELE,
         LT_DELE_KEY LIKE TABLE OF LT_DELE WITH HEADER LINE.

  DATA : BEGIN OF LT_BP OCCURS 0,
           PARTNER    LIKE BUT000-PARTNER,
           NAME_ORG1  LIKE BUT000-NAME_ORG1,
           NATION     LIKE BUT020-NATION,
           LAND1      LIKE T005T-LAND1,
           LANDX      LIKE T005T-LANDX,
           STREET     LIKE ADRC-STREET,
           CITY1      LIKE ADRC-CITY1,
           POST_CODE1 LIKE ADRC-POST_CODE1,
           HOUSE_NUM1 LIKE ADRC-HOUSE_NUM1,
         END OF LT_BP,
         LT_BP_KEY LIKE TABLE OF LT_BP WITH HEADER LINE.

  DATA : BEGIN OF LT_CUSTPO OCCURS 0,
           EBELN   LIKE EKPO-EBELN,
           EBELP   LIKE EKPO-EBELP,
           POWERKS LIKE EKPO-WERKS,  "Customer PO plant
           BANFN   LIKE EKPO-BANFN,
           BNFPO   LIKE EKPO-BNFPO,
           EINDT   LIKE EKET-EINDT,  "Customer PO DELIVERY DATE
           VERKF   LIKE EKKO-VERKF,  "Customer PO (Ship To)
         END OF LT_CUSTPO,
         LT_CUSTPO_KEY LIKE TABLE OF LT_CUSTPO WITH HEADER LINE.

  DATA : LV_POSNR     LIKE VBAP-POSNR,
         LV_POSNR2(6),
         LV_POSNR3(6).
  DATA : LT_VBEP_KEY LIKE TABLE OF VBEP WITH HEADER LINE.
  DATA : LT_T001W    LIKE TABLE OF T001W WITH HEADER LINE.
  DATA : LV_KUNNR LIKE KNA1-KUNNR,
         LV_COUNT TYPE I.
  DATA : LT_TEMP LIKE TABLE OF GT_LIST WITH HEADER LINE.

  RANGES : LR_VBELN FOR VBAP-VBELN,
           LR_MATNR FOR ZMDT0070-MATNO.

  CHECK GT_LIST[] IS NOT INITIAL.

  _CLEAR : LT_BP_KEY, LT_CUSTPO_KEY, LT_DELE_KEY, LR_VBELN.

  LOOP AT GT_LIST.
    LT_BP_KEY-PARTNER = GT_LIST-KUNNR.
    COLLECT LT_BP_KEY. CLEAR LT_BP_KEY.
    LT_BP_KEY-PARTNER = GT_LIST-KUNWE.
    COLLECT LT_BP_KEY. CLEAR LT_BP_KEY.
    LT_CUSTPO_KEY-EBELN = GT_LIST-BSTKD.
    COLLECT LT_CUSTPO_KEY. CLEAR LT_CUSTPO_KEY.
*    LT_DELE_KEY-OBJECTID = GT_LIST-VBELN.
*    COLLECT LT_DELE_KEY. CLEAR LT_DELE_KEY.
    _RANGE LR_VBELN 'I' 'EQ' GT_LIST-VBELN ''.
    _RANGE LR_MATNR 'I' 'EQ' GT_LIST-MATNR ''.
  ENDLOOP.

  SELECT A~VBELN,
         A~PARVW,
         B~COUNTRY,
         B~NAME1,
         B~NAME2,
         B~NAME3,
         B~CITY1,
         B~POST_CODE1
  INTO TABLE @DATA(LT_ADRC)
  FROM VBPA AS A INNER JOIN ADRC AS B ON A~ADRNR EQ B~ADDRNUMBER
  WHERE A~VBELN IN @LR_VBELN
  AND A~POSNR EQ '000000'
  AND A~PARVW IN  ( 'AG', 'WE' ).
  SORT LT_ADRC BY VBELN  PARVW.

  _CLEAR LT_T001W.
  SELECT WERKS NAME1
    INTO CORRESPONDING FIELDS OF TABLE LT_T001W
    FROM T001W
   WHERE SPRAS = SY-LANGU.
  SORT LT_T001W BY WERKS.

  IF LT_BP_KEY[] IS NOT INITIAL.
    _CLEAR LT_BP.
    SELECT A~PARTNER
           A~NAME_ORG1
           A~TITLE_LET
           C~STREET
           C~CITY1
           C~HOUSE_NUM1
           C~POST_CODE1
           D~LAND1
           D~LANDX
    INTO CORRESPONDING FIELDS OF TABLE LT_BP
    FROM BUT000 AS A    INNER JOIN BUT020 AS B ON A~PARTNER = B~PARTNER
                        INNER JOIN ADRC   AS C ON B~ADDRNUMBER = C~ADDRNUMBER
                        INNER JOIN T005T  AS D ON C~COUNTRY = D~LAND1
                                              AND D~SPRAS = SY-LANGU
     FOR ALL ENTRIES IN LT_BP_KEY
      WHERE A~PARTNER   EQ LT_BP_KEY-PARTNER
        AND B~NATION    IN ( 'I' , SPACE ).
*--NATION 'I'가 있으면 space는 제거, 'I'가 없으면 space를 리스트에..
    SORT LT_BP BY PARTNER ASCENDING NATION DESCENDING.
    DELETE ADJACENT DUPLICATES FROM LT_BP COMPARING PARTNER NATION.
    SORT LT_BP BY PARTNER.
  ENDIF.

  IF LT_CUSTPO_KEY[] IS NOT INITIAL.
    _CLEAR LT_CUSTPO.
    SELECT A~EBELN,
           A~EBELP,
           A~WERKS  AS POWERKS,
           A~BANFN,
           A~BNFPO,
           B~VERKF,
           C~EINDT
    INTO CORRESPONDING FIELDS OF TABLE @LT_CUSTPO
    FROM EKPO AS A INNER JOIN EKKO AS B ON A~EBELN = B~EBELN
                   INNER JOIN EKET AS C ON A~EBELN = C~EBELN
    FOR ALL ENTRIES IN @LT_CUSTPO_KEY
    WHERE A~EBELN = @LT_CUSTPO_KEY-EBELN.
    SORT LT_CUSTPO BY EBELN.

    _CLEAR : LT_VBEP_KEY.
    LOOP AT LT_CUSTPO.
      IF LT_CUSTPO-BANFN IS NOT INITIAL.
        LT_VBEP_KEY-BANFN = LT_CUSTPO-BANFN.
        LT_VBEP_KEY-BNFPO = LT_CUSTPO-BNFPO.
        COLLECT LT_VBEP_KEY. CLEAR LT_VBEP_KEY.
      ENDIF.
    ENDLOOP.
    IF LT_VBEP_KEY[] IS NOT INITIAL.
*-US SO와 SET가져오는 QUERY
      SELECT U~VBELN,
             U~POSNR,
             U~POSEX,
             U~KDMAT,
             R~BSTKD AS US_REF,
             M~EBELN,
             M~EBELP,
             K~VBELN AS HQ_VBELN,
             S~VBELN AS SET_VBELN,
             S~POSNR AS SET_POSNR,
             S~MATNR AS SET_MATNR,
             S~KDMAT AS SET_KDMAT
     INTO TABLE @DATA(LT_US)
      FROM VBEP AS A INNER JOIN VBAP AS U ON A~VBELN = U~VBELN
                                         AND A~POSNR = U~POSNR
                     INNER JOIN VBKD AS R ON A~VBELN = R~VBELN
                     INNER JOIN EKPO AS M ON A~BANFN = M~BANFN
                                         AND A~BNFPO = M~BNFPO
                     INNER JOIN VBKD AS K ON M~EBELN = K~BSTKD
                LEFT OUTER JOIN VBAP AS S ON U~VBELN = S~VBELN
                                         AND S~PSTYV = 'ZAP'
      FOR ALL ENTRIES IN @LT_VBEP_KEY
      WHERE A~BANFN EQ @LT_VBEP_KEY-BANFN
        AND A~BNFPO EQ @LT_VBEP_KEY-BNFPO.

      LOOP AT LT_US INTO DATA(LS_USA).
        CLEAR : LV_POSNR, LV_POSNR2.
        LV_POSNR2  = LS_USA-POSNR.
        LV_POSNR3 = LS_USA-SET_POSNR.
        PERFORM ALPHA_OUTPUT USING : LV_POSNR2, LV_POSNR3.
        IF LV_POSNR2(1) NE LV_POSNR3(1).
          CLEAR : LS_USA-SET_MATNR, LS_USA-SET_KDMAT.
          MODIFY LT_US FROM LS_USA TRANSPORTING SET_MATNR SET_KDMAT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*  IF LT_DELE_KEY[] IS NOT INITIAL.
*    _CLEAR LT_DELE.
*    SELECT A~OBJECTID
*    INTO TABLE LT_DELE
*    FROM CDHDR AS A INNER JOIN CDPOS AS B ON A~OBJECTCLAS = B~OBJECTCLAS
*                                         AND A~OBJECTID   = B~OBJECTID
*     FOR ALL ENTRIES IN LT_DELE_KEY
*    WHERE A~OBJECTCLAS = 'VERKBELEG'
*    AND A~TCODE = 'VA02'
*    AND A~OBJECTID = LT_DELE_KEY-OBJECTID
*    AND B~TABNAME = 'VBAP'
*    AND B~CHNGIND = 'D'.
*
*    SORT LT_DELE.
*  ENDIF.

  SELECT A~ZKUNNR_IC,
         A~VTWEG,
         A~KUNWE,
         A~KUNNR,
         B~TITLE_LET,
         C~MATNO,
         C~ZASIN
   FROM ZSDT0060 AS A INNER JOIN BUT000 AS B ON A~KUNNR = B~PARTNER
                      INNER JOIN ZMDT0070 AS C ON B~TITLE_LET = C~CHANNEL
   INTO TABLE @DATA(LT_ZMDT0070)
   WHERE C~MATNO IN @LR_MATNR
     AND C~ZASIN NE @SPACE.

  SORT LT_ZMDT0070 BY ZKUNNR_IC VTWEG MATNO.

  SELECT ZKUNNR_IC,
         VTWEG,
         KUNNR
    FROM ZSDT0060 INTO TABLE @DATA(LT_0060)
    WHERE KUNNR NE @SPACE.

  SORT LT_0060 BY ZKUNNR_IC VTWEG.

  SELECT ZKUNNR_IC,
         ZSYSTEM
    FROM ZSDT0040 INTO TABLE @DATA(LT_0040).

  SORT LT_0040 BY ZKUNNR_IC.

  SELECT KUNNR, MATNR, ZASIN
   FROM ZSDT0120
   INTO TABLE @DATA(LT_0120)
   WHERE MATNR IN @LR_MATNR.

  SORT LT_0120 BY KUNNR MATNR.

*Overstock, wayfair// customer SKU
*wayfair  10012
*overstock  10462
  SELECT KUNNR,
         MATNR,
         KDMAT
    FROM KNMT
    INTO TABLE @DATA(LT_KNMT)
  WHERE MATNR IN @LR_MATNR
    AND VKORG = '2000'
    AND VTWEG = '21'
    AND KUNNR IN ('0000010012' , '0000010013' ,
                  '0000010462' , '0000010521').

  SORT LT_KNMT BY KUNNR MATNR.

  LOOP AT GT_LIST.

    GT_LIST-VBELN_I = GT_LIST-VBELN.
    GT_LIST-BSTKD_2 = GT_LIST-BSTKD.
    IF GT_LIST-POSEX IS INITIAL.
      GT_LIST-POSEX   = GT_LIST-POSNR.
    ENDIF.
*    CLEAR LT_DELE.
*    READ TABLE LT_DELE WITH KEY OBJECTID = GT_LIST-VBELN BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      GT_LIST-ZDELE = 'X'.
*    ENDIF.

    CASE GT_LIST-ABGRU.
      WHEN 'ZA'.
        GT_LIST-ZCOMPLETE = 'X'.
      WHEN 'ZB'.
        GT_LIST-ZDELE = 'X'.
    ENDCASE.

*SOLD-TO INFORMATION
    CLEAR LT_BP.
    READ TABLE LT_BP WITH KEY PARTNER = GT_LIST-KUNNR BINARY SEARCH.
    CONCATENATE LT_BP-STREET LT_BP-HOUSE_NUM1 LT_BP-CITY1 LT_BP-POST_CODE1 LT_BP-LANDX INTO
    GT_LIST-KUNNR_ADDRESS SEPARATED BY SPACE.
    GT_LIST-KUNNR_LAND = LT_BP-LAND1.
    GT_LIST-KUNNRT     = LT_BP-NAME_ORG1.


*SHIP-TO INFORMATION
    CLEAR LT_BP.
    READ TABLE LT_BP WITH KEY PARTNER = GT_LIST-KUNWE BINARY SEARCH.
    GT_LIST-LAND1      = LT_BP-LAND1.
    GT_LIST-KUNWET     = LT_BP-NAME_ORG1.

    READ TABLE LT_ADRC INTO DATA(LS_ADRC2) WITH KEY VBELN = GT_LIST-VBELN
                                                    PARVW = 'WE' BINARY SEARCH.
    CONCATENATE LS_ADRC2-NAME1 LS_ADRC2-NAME2
    LS_ADRC2-CITY1 LS_ADRC2-POST_CODE1 LT_BP-LANDX
    INTO GT_LIST-KUNWE_ADDRESS SEPARATED BY SPACE.
    GT_LIST-KUNWE_LAND = LS_ADRC2-COUNTRY.
    CLEAR LS_ADRC2.
    IF GT_LIST-KUNNR = GT_LIST-KUNWE.
      GT_LIST-KUNWE_ADDRESS = GT_LIST-KUNNR_ADDRESS.
    ENDIF.

*HSCODE
    READ TABLE LT_ADRC INTO DATA(LS_ADRC) WITH KEY VBELN = GT_LIST-VBELN
                                                   PARVW = 'AG'  BINARY SEARCH.
    PERFORM GET_HSCODE_TEXT USING LS_ADRC-COUNTRY.

*Customer Plant
    CLEAR LT_CUSTPO.
    READ TABLE LT_CUSTPO WITH KEY EBELN = GT_LIST-BSTKD BINARY SEARCH.
    GT_LIST-CWERK    = LT_CUSTPO-POWERKS.
    GT_LIST-CEIND    = LT_CUSTPO-EINDT.

    CLEAR LV_KUNNR.
    CASE GT_LIST-CWERK.
      WHEN '2800'. "overstock
        LV_KUNNR = '0000010462'.
        READ TABLE LT_KNMT INTO DATA(LS_KNMT) WITH KEY KUNNR = LV_KUNNR
                                                       MATNR = GT_LIST-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-ZWFSKU =  LS_KNMT-KDMAT.
        ENDIF.
      WHEN '2900'. "wayfair

        IF GT_LIST-IHREZ = 'OPEN_PO'.
          PERFORM SET_COUNTY_CODE USING    GT_LIST-BSTKD
                                 CHANGING  GT_LIST-KUNWE_LAND.
        ENDIF.
        IF GT_LIST-KUNWE_LAND = 'US'.
          LV_KUNNR = '0000010012'.
        ELSEIF GT_LIST-KUNWE_LAND = 'GB'.
          LV_KUNNR = '0000010013'.
        ELSEIF GT_LIST-KUNWE_LAND = 'DE'.
          LV_KUNNR = '0000010521'.
        ENDIF.

        READ TABLE LT_KNMT INTO DATA(LS_KNMT2) WITH KEY KUNNR = LV_KUNNR
                                                        MATNR = GT_LIST-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-ZWFSKU =  LS_KNMT2-KDMAT.
        ENDIF.
      WHEN OTHERS.
        GT_LIST-ZWFSKU = GT_LIST-KDMAT.
    ENDCASE.


    PERFORM ALPHA_INPUT USING GT_LIST-KUNNR.
    IF GT_LIST-KUNNR = '0000002000'.
      CASE GT_LIST-VTWEG.
*AMAZON PO, ITEM
        WHEN '10'.
          READ TABLE LT_US INTO DATA(LS_US) WITH KEY EBELN = GT_LIST-BSTKD
                                                     EBELP = GT_LIST-POSEX.
          IF SY-SUBRC = 0.
            GT_LIST-BSTKD_AZ = LS_US-US_REF.
            GT_LIST-POSEX_AZ = LS_US-POSEX.
            IF LS_US-SET_MATNR IS NOT INITIAL.
              IF LS_US-SET_KDMAT IS INITIAL.
                GT_LIST-ZWFSKU   = LS_US-SET_MATNR.
              ELSE.
                GT_LIST-ZWFSKU   = LS_US-SET_KDMAT.
              ENDIF.
            ELSE.
              GT_LIST-ZWFSKU   = LS_US-KDMAT.
            ENDIF.
          ENDIF.

*WAYFAIR PO, ITEM
        WHEN '21'.
          CLEAR LT_CUSTPO.
          READ TABLE LT_CUSTPO WITH KEY EBELN = GT_LIST-BSTKD BINARY SEARCH.
          GT_LIST-ZWFPOH   = LT_CUSTPO-VERKF.
          IF GT_LIST-ZWFPOH IS NOT INITIAL.
            GT_LIST-ZWFPOI = GT_LIST-POSEX.
          ENDIF.
      ENDCASE.
    ELSE.
*미국이 아닌 다른 국가의 AMAZON PO를 넣어준다
      READ TABLE LT_0040 INTO DATA(LS_0040) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR BINARY SEARCH.
      IF LS_0040-ZSYSTEM NE 'SAP'.
        READ TABLE LT_0060 INTO DATA(LS_0060A) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR
                                                         VTWEG    = GT_LIST-VTWEG BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-BSTKD_AZ = GT_LIST-BSTKD.
          GT_LIST-POSEX_AZ = GT_LIST-POSEX.
        ENDIF.
      ENDIF.
    ENDIF.

*ASIN
    READ TABLE LT_ZMDT0070 INTO DATA(LS_0070) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR
                                                       VTWEG     = GT_LIST-VTWEG
                                                       MATNO     = GT_LIST-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_LIST-ZASIN = LS_0070-ZASIN.
    ELSE.

      READ TABLE LT_0060 INTO DATA(LS_0060) WITH KEY ZKUNNR_IC = GT_LIST-KUNNR
                                                     VTWEG     = GT_LIST-VTWEG BINARY SEARCH.
      IF SY-SUBRC = 0.
        READ TABLE LT_0120 INTO DATA(LS_0120) WITH KEY KUNNR = LS_0060-KUNNR
                                                       MATNR = GT_LIST-MATNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          GT_LIST-ZASIN = LS_0120-ZASIN.
        ELSE.
          GT_LIST-ICON = ICON_RED_LIGHT.
          GT_LIST-MESSAGE = TEXT-M11.
        ENDIF.
      ENDIF.
    ENDIF.

*PLANT text
    CLEAR LT_T001W.
    READ TABLE LT_T001W WITH KEY WERKS = GT_LIST-WERKS BINARY SEARCH.
    GT_LIST-WERKS_TXT = LT_T001W-NAME1.
    CLEAR LT_T001W.
    READ TABLE LT_T001W WITH KEY WERKS = GT_LIST-CWERK BINARY SEARCH.
    GT_LIST-CWERK_TXT = LT_T001W-NAME1.

*-Ship-to + Country
    PERFORM ALPHA_OUTPUT USING GT_LIST-KUNWE.
    GT_LIST-ZSHIP_LAND = GT_LIST-KUNWE && GT_LIST-KUNWE_LAND.
    PERFORM ALPHA_INPUT USING GT_LIST-KUNWE.

    IF GT_LIST-ICON IS INITIAL.
      GT_LIST-ICON = ICON_LIGHT_OUT.
    ENDIF.

    MODIFY GT_LIST TRANSPORTING ZCOMPLETE ZDELE VBELN_I BSTKD_2 POSEX
                                KUNNR_ADDRESS KUNNR_LAND KUNNRT
                                KUNWE_ADDRESS KUNWE_LAND KUNWET
                                ZWFPOH ZWFPOI ZWFSKU ZASIN
                                CWERK BSTKD_AZ POSEX_AZ CEIND
                                WERKS_TXT CWERK_TXT
                                CCNGN CCNGNX ZSHIP_LAND
                                ICON MESSAGE.
  ENDLOOP.


*TO CONVERT SET MATERIAL
  _CLEAR LT_TEMP.
  LT_TEMP[] = GT_LIST[].


*SET는 전송 대상이 아니다.
  DELETE GT_LIST WHERE PSTYV EQ 'TAP'.
  DELETE LT_TEMP WHERE PSTYV NE 'TAP'.

  SORT GT_LIST BY VBELN POSNR.

**SET 구성품은 SET의 SKU를 ZWFSKU에 넣어준다.
  LOOP AT LT_TEMP.
    CLEAR LV_COUNT.
    DO 9 TIMES.
      CLEAR LV_POSNR.
      ADD 1 TO LV_COUNT.
      LV_POSNR = LT_TEMP-POSNR.
      LV_POSNR+5(1) = LV_COUNT.
      READ TABLE GT_LIST WITH KEY VBELN = LT_TEMP-VBELN
                                  POSNR = LV_POSNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_LIST-ZWFSKU = LT_TEMP-MATNR.
        MODIFY GT_LIST INDEX SY-TABIX TRANSPORTING ZWFSKU.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.


*--동일 SO 가 오류면 모두 에러처리
  _CLEAR LT_TEMP.
  LT_TEMP[] = GT_LIST[].
  LOOP AT LT_TEMP WHERE ICON = ICON_RED_LIGHT.
    LOOP AT GT_LIST WHERE VBELN = LT_TEMP-VBELN
                      AND ICON = ICON_LIGHT_OUT.
      GT_LIST-ICON = ICON_RED_LIGHT.
      GT_LIST-MESSAGE = LT_TEMP-MESSAGE.
      MODIFY GT_LIST TRANSPORTING ICON MESSAGE.
    ENDLOOP.
  ENDLOOP.

  SORT GT_LIST BY VBELN POSEX.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEND_DATA
*&---------------------------------------------------------------------*
FORM SEND_DATA .

  DATA : LT_SEND LIKE VBAK-VBELN OCCURS 0 WITH HEADER LINE.

  CASE P_RESEND.
    WHEN 'X'.
      CLEAR: GT_ROWS[], GS_ROWS.
      CALL METHOD GO_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = GT_ROWS.
      IF GT_ROWS[] IS INITIAL.
        MESSAGE S004 DISPLAY LIKE 'E'.
      ELSE.
        LOOP AT GT_ROWS INTO GS_ROWS.
          READ TABLE GT_LIST INDEX GS_ROWS-INDEX.
          MOVE GT_LIST-VBELN TO LT_SEND.
          COLLECT LT_SEND. CLEAR LT_SEND.
        ENDLOOP.

*한 SO만 선택해도 모든 아이템 전송
        LOOP AT LT_SEND.
          LOOP AT GT_LIST WHERE VBELN = LT_SEND.
            MOVE-CORRESPONDING GT_LIST TO GT_SEND.
            APPEND GT_SEND. CLEAR GT_SEND.
          ENDLOOP.
        ENDLOOP.
      ENDIF.

    WHEN SPACE.
      _CLEAR GT_SEND.
      GT_SEND[] = GT_LIST[].

  ENDCASE.

  DELETE GT_SEND WHERE ICON = ICON_RED_LIGHT.
  CHECK GT_SEND[] IS NOT INITIAL.

  PERFORM PREPARE_SEND.
  PERFORM CALL_API.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PREPARE_SEND
*&---------------------------------------------------------------------*
FORM PREPARE_SEND .

  _CLEAR : GT_HEAD, GT_ITEM.
  MOVE-CORRESPONDING GT_SEND[] TO GT_HEAD[].
  SORT GT_HEAD BY VBELN.
  DELETE ADJACENT DUPLICATES FROM GT_HEAD COMPARING VBELN.

  LOOP AT GT_HEAD INTO GS_HEAD.
    CLEAR GS_HEAD-ZDELE.
    MODIFY GT_HEAD FROM GS_HEAD TRANSPORTING ZDELE.
  ENDLOOP.

  LOOP AT GT_SEND.
    CLEAR GS_ITEM.
    MOVE-CORRESPONDING GT_SEND TO GS_ITEM.
    GS_ITEM-ARKTX = GT_SEND-MATNR_TXT.
    GS_ITEM-WAERK = GT_SEND-WAERK_2.
    APPEND GS_ITEM TO GT_ITEM.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_API
*&---------------------------------------------------------------------*
FORM CALL_API .

  DATA: LO_CLIENT  TYPE REF TO IF_HTTP_CLIENT,
        LV_CONTENT TYPE STRING,
        LV_URL     TYPE STRING,
        LV_JSON    TYPE STRING,
        LV_RESULT  TYPE STRING.

  CLEAR GV_IF_TSP.
  PERFORM GET_TIMESTAMP CHANGING GV_IF_TSP.

  CLEAR GV_API.
  CASE SY-SYSID.
    WHEN 'ZUD'.
      GV_API = GV_API_D.
    WHEN 'ZUQ'.
      GV_API = GV_API_Q.
    WHEN 'ZUP'.
      GV_API = GV_API_P.
  ENDCASE.

  CLEAR LO_CLIENT.
  CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
    EXPORTING
      URL                = GV_API
    IMPORTING
      CLIENT             = LO_CLIENT
    EXCEPTIONS
      ARGUMENT_NOT_FOUND = 1
      PLUGIN_NOT_ACTIVE  = 2
      INTERNAL_ERROR     = 3
      OTHERS             = 4.

  IF SY-SUBRC <> 0.
    MESSAGE S012 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* METHOD
  CALL METHOD LO_CLIENT->REQUEST->SET_METHOD
    EXPORTING
      METHOD = LO_CLIENT->REQUEST->CO_REQUEST_METHOD_POST.

* JSON TYPE
  CALL METHOD LO_CLIENT->REQUEST->IF_HTTP_ENTITY~SET_CONTENT_TYPE
    EXPORTING
      CONTENT_TYPE = GC_CON_TYP.

* HEADER FIELD
  CALL METHOD LO_CLIENT->REQUEST->SET_HEADER_FIELD
    EXPORTING
      NAME  = GC_API_KEY
      VALUE = GC_API_VAL.

* BODY DATA
  PERFORM SET_BODY CHANGING LV_JSON.

  CALL METHOD LO_CLIENT->REQUEST->SET_CDATA
    EXPORTING
      DATA = LV_JSON.

* CALL API(REQUEST)
  LO_CLIENT->SEND(
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      HTTP_INVALID_TIMEOUT       = 4
      OTHERS                     = 5 ).

  IF SY-SUBRC <> 0.
    GV_IF_STATUS = 'E'.
    CASE SY-SUBRC.
      WHEN 1. GV_IF_MSG = TEXT-M05.
      WHEN 2. GV_IF_MSG = TEXT-M06.
      WHEN 3. GV_IF_MSG = TEXT-M07.
      WHEN 4. GV_IF_MSG = TEXT-M08.
      WHEN 5. GV_IF_MSG = TEXT-M04.
    ENDCASE.

    PERFORM SAVE_LOG.
    EXIT.
  ENDIF.

* RESPONSE
  LO_CLIENT->RECEIVE(
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      OTHERS                     = 4 ).

  IF SY-SUBRC <> 0.
    GV_IF_STATUS = 'E'.
    CASE SY-SUBRC.
      WHEN 1. GV_IF_MSG = TEXT-M05.
      WHEN 2. GV_IF_MSG = TEXT-M06.
      WHEN 3. GV_IF_MSG = TEXT-M07.
      WHEN 4. GV_IF_MSG = TEXT-M04.
    ENDCASE.

    PERFORM SAVE_LOG.
    PERFORM SAVE_NOT_SENTSO.
    EXIT.
  ENDIF.

* RESPONSE DATA
  LV_RESULT = LO_CLIENT->RESPONSE->GET_CDATA( ).

  IF LV_RESULT IS NOT INITIAL.
    PERFORM SET_RESPONSE USING LV_RESULT.
  ENDIF.
  PERFORM SAVE_LOG.

  CASE GS_RETURN-STATUS.
    WHEN '200'.
      MESSAGE S013. "data has been sent
      LOOP AT GT_HEAD INTO DATA(LS_HEAD).
        GT_LIST-ICON = ICON_GREEN_LIGHT.
        GT_LIST-MESSAGE = TEXT-M09.
        MODIFY GT_LIST TRANSPORTING ICON MESSAGE WHERE VBELN = LS_HEAD-VBELN.
      ENDLOOP.
    WHEN OTHERS.
      MESSAGE S012 DISPLAY LIKE 'E'. "An error has occurred.
      LOOP AT GT_HEAD INTO DATA(LS_HEAD2).
        GT_LIST-ICON = ICON_RED_LIGHT.
        GT_LIST-MESSAGE = GS_RETURN-MSG.
        MODIFY GT_LIST TRANSPORTING ICON MESSAGE WHERE VBELN = LS_HEAD2-VBELN.
      ENDLOOP.
  ENDCASE.

  PERFORM SAVE_NOT_SENTSO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TIMESTAMP
*&---------------------------------------------------------------------*
FORM GET_TIMESTAMP   CHANGING PV_TSP.

  GET TIME STAMP FIELD PV_TSP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_LOG
*&---------------------------------------------------------------------*
FORM SAVE_LOG .

  DATA: LT_ZMMT0010 LIKE TABLE OF ZMMT0010 WITH HEADER LINE,
        LT_ZSDT0080 LIKE TABLE OF ZSDT0080 WITH HEADER LINE.

  DESCRIBE TABLE GT_ITEM LINES DATA(LV_ROWS).

  _CLEAR: LT_ZMMT0010, LT_ZSDT0080.
  LOOP AT GT_SEND.
    MOVE-CORRESPONDING GT_SEND TO LT_ZMMT0010.
    LT_ZMMT0010-ZCONT   = '2'.
    LT_ZMMT0010-IFTSP   = GV_IF_TSP.
    LT_ZMMT0010-SELTSP  = GV_SEL_TSP.
    LT_ZMMT0010-IFCOUNT = LV_ROWS.
    LT_ZMMT0010-IFNAM   = SY-UNAME.
    LT_ZMMT0010-ERDAT   = SY-DATLO.
    LT_ZMMT0010-ERZET   = SY-TIMLO.
    LT_ZMMT0010-ERNAM   = SY-UNAME.

    IF GV_IF_STATUS IS NOT INITIAL.
      LT_ZMMT0010-ZSAP_TYP  = GV_IF_STATUS.
      LT_ZMMT0010-MSG       = GV_IF_MSG.
      LT_ZMMT0010-SENDER_ID = GT_LIST-BUKRS.
    ELSE.
      MOVE-CORRESPONDING GS_RETURN TO LT_ZMMT0010.

      CASE GS_RETURN-STATUS.
        WHEN '200'.
          LT_ZMMT0010-ZSAP_TYP = 'S'.
        WHEN OTHERS.
          LT_ZMMT0010-ZSAP_TYP = 'E'.
      ENDCASE.
    ENDIF.

    APPEND LT_ZMMT0010. CLEAR LT_ZMMT0010.
    MOVE-CORRESPONDING GT_SEND TO LT_ZSDT0080.
    LT_ZSDT0080-ZCONT   = '2'.
    LT_ZSDT0080-IFTSP   = GV_IF_TSP.
    LT_ZSDT0080-ERDAT   = SY-DATLO.
    LT_ZSDT0080-ERZET   = SY-TIMLO.
    LT_ZSDT0080-ERNAM   = SY-UNAME.
    APPEND LT_ZSDT0080. CLEAR LT_ZSDT0080.
  ENDLOOP.

  MODIFY ZMMT0010 FROM TABLE LT_ZMMT0010.
  MODIFY ZSDT0080 FROM TABLE LT_ZSDT0080.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RESPONSE
*&---------------------------------------------------------------------*
FORM SET_RESPONSE  USING PV_RESULT.

  CLEAR GS_RETURN.

  /UI2/CL_JSON=>DESERIALIZE(
    EXPORTING
      JSON = PV_RESULT
      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-NONE
    CHANGING
      DATA = GS_RETURN ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_BODY
*&---------------------------------------------------------------------*
FORM SET_BODY CHANGING PV_JSON.

  TYPES: BEGIN OF LY_JSON,
           API_DOC_ID TYPE STRING,
           SENDER_ID  TYPE EKKO-BUKRS,
           GT_HEAD    TYPE /UI2/CL_JSON=>JSON,
           GT_ITEM    TYPE /UI2/CL_JSON=>JSON,
         END OF LY_JSON.

  DATA: LS_JSON TYPE LY_JSON.

  CLEAR LS_JSON.
  LS_JSON-API_DOC_ID = GV_IF_TSP.
  CONDENSE LS_JSON-API_DOC_ID.
  READ TABLE GT_HEAD INTO DATA(LS_HEAD) INDEX 1.
  LS_JSON-SENDER_ID = LS_HEAD-BUKRS.

  LS_JSON-GT_HEAD =
  /UI2/CL_JSON=>SERIALIZE(
    EXPORTING
      DATA        = GT_HEAD
      COMPRESS    = ABAP_TRUE
      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-NONE ).

  LS_JSON-GT_ITEM =
  /UI2/CL_JSON=>SERIALIZE(
    EXPORTING
      DATA        = GT_ITEM
      COMPRESS    = ABAP_TRUE
      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-NONE ).

* SET DATA OF JSON TYPE
  PV_JSON =
  /UI2/CL_JSON=>SERIALIZE(
    EXPORTING
      DATA             = LS_JSON
      COMPRESS         = ABAP_TRUE
      ASSOC_ARRAYS     = ABAP_TRUE
      ASSOC_ARRAYS_OPT = ABAP_TRUE
      PRETTY_NAME      = /UI2/CL_JSON=>PRETTY_MODE-NONE ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RETURN_INFO
*&---------------------------------------------------------------------*
FORM SET_RETURN_INFO .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FRAME_9100
*&---------------------------------------------------------------------*
FORM SET_FRAME_9100 .

  CREATE OBJECT GO_CUSTOM1
    EXPORTING
      CONTAINER_NAME = 'GO_CON1'.

  CREATE OBJECT GO_GRID1
    EXPORTING
      I_PARENT = GO_CUSTOM1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_VARIANT_9100
*&---------------------------------------------------------------------*
FORM SET_VARIANT_9100 .

  CLEAR: GS_VARIANT1.
  GS_VARIANT1-REPORT = SY-REPID.
  GS_VARIANT1-USERNAME = SY-UNAME.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_SORT_9100
*&---------------------------------------------------------------------*
FORM SET_SORT_9100 .
  _CLEAR GT_SORT1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT_9100
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_9100 .

  CLEAR GS_LAYOUT1.
  GS_LAYOUT1-SEL_MODE   = 'A'.
  GS_LAYOUT1-ZEBRA      = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELDCAT_9100
*&---------------------------------------------------------------------*
FORM SET_FIELDCAT_9100 .

  CLEAR: GT_FCAT1[], GS_FCAT.

  GS_FCAT-FIELDNAME  = 'ZCONT'.
  GS_FCAT-COLTEXT    = 'CONTENT'.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'WERKS'.
  GS_FCAT-COLTEXT    = TEXT-F72.
  GS_FCAT-OUTPUTLEN  = '5'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'WERKS_TXT'.
  GS_FCAT-COLTEXT    = TEXT-F66.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'IFDAT'.
  GS_FCAT-COLTEXT    = TEXT-F77.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'IFZET'.
  GS_FCAT-COLTEXT    = TEXT-F78.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'IFNAM'.
  GS_FCAT-COLTEXT    = TEXT-F79.
  GS_FCAT-OUTPUTLEN  = '10'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

  GS_FCAT-FIELDNAME  = 'IFCOUNT'.
  GS_FCAT-COLTEXT    = 'Count'.
  GS_FCAT-OUTPUTLEN  = '6'.
  APPEND GS_FCAT TO GT_FCAT1. CLEAR: GS_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DISPLAY_9100
*&---------------------------------------------------------------------*
FORM SET_DISPLAY_9100 .

  FIELD-SYMBOLS: <GT_TAB> TYPE TABLE.
  UNASSIGN: <GT_TAB>.

  ASSIGN: GT_9100[] TO <GT_TAB>.

  CALL METHOD GO_GRID1->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

  CALL METHOD GO_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT1
      IS_VARIANT           = GS_VARIANT1
      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
      I_SAVE               = 'A'
    CHANGING
      IT_OUTTAB            = <GT_TAB>[]
      IT_FIELDCATALOG      = GT_FCAT1
      IT_SORT              = GT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LOG_DATA
*&---------------------------------------------------------------------*
FORM GET_LOG_DATA .

  DATA: LV_STAMP_LOW  TYPE TIMESTAMPL,
        LV_STAMP_HIGH TYPE TIMESTAMPL,
        LV_TIMEZONE   TYPE TIMEZONE.

  CLEAR : LV_STAMP_LOW, LV_STAMP_HIGH, LV_TIMEZONE.
  CONVERT DATE S_CDATE-LOW
          INTO TIME STAMP LV_STAMP_LOW TIME ZONE SY-ZONLO.

  PERFORM SET_NEXT_DAY CHANGING S_CDATE-HIGH.

  CONVERT DATE S_CDATE-HIGH
          INTO TIME STAMP LV_STAMP_HIGH TIME ZONE SY-ZONLO.

  _CLEAR GT_9100.
  SELECT  A~ZCONT
          A~WERKS
          B~NAME1 AS WERKS_TXT
          A~IFTSP
          A~IFNAM
          A~IFCOUNT
    INTO CORRESPONDING FIELDS OF TABLE GT_9100
    FROM ZMMT0010 AS A INNER JOIN T001W AS B ON A~WERKS = B~WERKS
                                            AND B~SPRAS = SY-LANGU
   WHERE A~ZCONT EQ '2'
     AND A~WERKS IN S_PLANT
     AND A~IFTSP BETWEEN LV_STAMP_LOW AND LV_STAMP_HIGH.

  LOOP AT GT_9100.
    CLEAR LV_STAMP_LOW.
    LV_STAMP_LOW = GT_9100-IFTSP.

    CONVERT TIME STAMP LV_STAMP_LOW TIME ZONE SY-ZONLO
                INTO DATE GT_9100-IFDAT TIME GT_9100-IFZET.
    MODIFY GT_9100 TRANSPORTING IFDAT IFZET.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_NEXT_DAY
*&---------------------------------------------------------------------*
FORM SET_NEXT_DAY  CHANGING PV_DAY.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = PV_DAY
      DAYS      = 1
      MONTHS    = 0
      SIGNUM    = '+'
      YEARS     = 0
    IMPORTING
      CALC_DATE = PV_DAY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_HSCODE_TEXT
*&---------------------------------------------------------------------*
FORM GET_HSCODE_TEXT USING PV_COUNTRY.

  DATA LT_DATA LIKE ZMMS0150 OCCURS 0 WITH HEADER LINE.
  DATA: LV_LAISO TYPE LAISO.
  CLEAR: LV_LAISO.
  _CONV_ISOLA_OUTPUT SY-LANGU LV_LAISO.

  CALL FUNCTION 'ZMM_IF_HSCODE'
    EXPORTING
      I_LAND1 = PV_COUNTRY
      I_LAISO = LV_LAISO
      I_MATNR = GT_LIST-MATNR
    TABLES
      GT_DATA = LT_DATA.


  SORT LT_DATA BY LAND1 MATNR.
  READ TABLE LT_DATA WITH KEY LAND1 = GT_LIST-LAND1
                              MATNR = GT_LIST-MATNR BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GT_LIST-CCNGN  = LT_DATA-CCNGN.
    GT_LIST-CCNGNX = LT_DATA-CCNGNX.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_DETAIL_POPUP
*&---------------------------------------------------------------------*
FORM SHOW_DETAIL_POPUP   USING PV_TBLNAME.

  DATA: LT_FIELDCAT TYPE LVC_T_FCAT,
        POPUP_GRID  TYPE REF TO CL_RS_ALV_GRID_POPUP.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = PV_TBLNAME
    CHANGING
      CT_FIELDCAT      = LT_FIELDCAT.

  CREATE OBJECT POPUP_GRID
    EXPORTING
      I_T_FIELDCATALOG = LT_FIELDCAT
      I_LEFT           = 50
      I_TOP            = 50
      I_HEIGHT         = 500
      I_WIDTH          = 1000.

  CALL METHOD POPUP_GRID->SHOW_DATA
    EXPORTING
      I_T_DATA = <GT_LOG_D>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORIZATION_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORIZATION_CHECK .

  DATA : LS_RETURN LIKE BAPIRETURN1 .

  SELECT WERKS
  FROM T001W INTO TABLE @DATA(LT_T001W)
  WHERE WERKS IN @S_PLANT.

  LOOP AT LT_T001W INTO DATA(LS_T001W).
    CALL FUNCTION 'ZBC_AUTHORIZATION_CHECK'
      EXPORTING
        I_WERKS   = LS_T001W-WERKS
      IMPORTING
        ES_RETURN = LS_RETURN.

    IF LS_RETURN-TYPE = 'E'.
      MESSAGE E000 WITH LS_RETURN-MESSAGE.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COUNTY_CODE
*&---------------------------------------------------------------------*
FORM SET_COUNTY_CODE  USING    PV_PO
                      CHANGING PV_LAND.

*Customer PO 번호를 기준으로 다음과 같은 문자를 찾는다
*Ex) WFUK60107, WFDE60001
*UK 가 있으면 국가코드를 GB로 변경
*DE 가 있으면 국가코드는 DE
*아니면 US

  FIND 'UK' IN PV_PO IN CHARACTER MODE.
  IF SY-SUBRC = 0.
    PV_LAND = 'GB'.
  ELSE.
    FIND 'DE' IN PV_PO IN CHARACTER MODE.
    IF SY-SUBRC = 0.
      PV_LAND = 'DE'.
    ELSE.
      PV_LAND = 'US'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_VIEW
*&---------------------------------------------------------------------*
FORM CALL_VIEW  USING PV_VIEW_NAME LIKE  DD02V-TABNAME.


  DATA: BEGIN OF ACT_SELLIST OCCURS 10.
      INCLUDE STRUCTURE VIMSELLIST.
  DATA: END OF ACT_SELLIST.

  DATA: BEGIN OF ACT_EXCLFUN OCCURS 10.
      INCLUDE STRUCTURE VIMEXCLFUN.
  DATA: END OF ACT_EXCLFUN.


  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      ACTION                       = 'S'
      CORR_NUMBER                  = ' '
*     show_selection_popup         = 'X'
      VIEW_NAME                    = PV_VIEW_NAME
    TABLES
      DBA_SELLIST                  = ACT_SELLIST
      EXCL_CUA_FUNCT               = ACT_EXCLFUN
    EXCEPTIONS
      CLIENT_REFERENCE             = 01
      FOREIGN_LOCK                 = 02
      INVALID_ACTION               = 03
      NO_CLIENTINDEPENDENT_AUTH    = 04
      NO_DATABASE_FUNCTION         = 05
      NO_EDITOR_FUNCTION           = 06
      NO_SHOW_AUTH                 = 07
      NO_TVDIR_ENTRY               = 08
      NO_UPD_AUTH                  = 09
      ONLY_SHOW_ALLOWED            = 10
      SYSTEM_FAILURE               = 11
      UNKNOWN_FIELD_IN_DBA_SELLIST = 12
      VIEW_NOT_FOUND               = 13.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_NOTSENT_SO
*&---------------------------------------------------------------------*
FORM ADD_NOTSENT_SO .

  _CLEAR GT_ZSDT0081.
  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE GT_ZSDT0081
  FROM ZSDT0081
  WHERE ZSEND = SPACE.

  CHECK GT_ZSDT0081[] IS NOT INITIAL.

  SELECT A~VBELN, A~KUNNR, A~AUART,
         A~AUDAT, A~VDATU, A~WAERK,
         A~VKORG, A~VTWEG, A~SPART,
         A~BUKRS_VF AS BUKRS,
         B~POSNR, B~MATNR, B~ARKTX AS MATNR_TXT,
         B~CHARG, B~ABGRU, B~KDMAT,
         B~WERKS, B~LGORT, B~KWMENG,
         B~VRKME, B~NETPR, B~WAERK AS WAERK_2,
         B~KPEIN, B~KMEIN, B~NETWR,
         B~POSEX, B~NTGEW, B~GEWEI,
         B~BRGEW, B~VOLUM, B~VOLEH,
         B~NTGEW AS NTGEW_G,
         B~BRGEW AS BRGEW_G,
         B~VOLUM AS VOLUM_G,
         B~STAWN AS CCNGN,
         B~PSTYV,
         C~BSTKD, C~ZTERM, C~INCO1, C~INCO2, C~IHREZ,
         D~ZZBIG, D~ZZMID, D~ZZSIN, D~GROES, D~EAN11,
         E~BEZEI AS AUART_TXT,
         F~VTEXT AS VKORG_TXT,
         G~VTEXT AS VTWEG_TXT,
         H~VTEXT AS SPART_TXT,
         J~ZBIGTX,
         K~ZMIDTX,
         L~ZSINGTX,
         M~VTEXT AS ZTERM_TXT,
         N~BUTXT AS BUKRS_TXT,
         O~KUNNR AS KUNWE,
         P~LGOBE AS LGORT_TXT
  FROM VBAK AS A INNER JOIN VBAP     AS B ON A~VBELN = B~VBELN
                 INNER JOIN VBKD     AS C ON A~VBELN = C~VBELN
                                         AND C~POSNR = '000000'
                 INNER JOIN MARA     AS D ON B~MATNR = D~MATNR
                 INNER JOIN TVAKT    AS E ON A~AUART = E~AUART
                                         AND E~SPRAS = @SY-LANGU
                 INNER JOIN TVKOT    AS F ON A~VKORG = F~VKORG
                                         AND F~SPRAS = @SY-LANGU
                 INNER JOIN TVTWT    AS G ON A~VTWEG = G~VTWEG
                                         AND G~SPRAS = @SY-LANGU
                 INNER JOIN TSPAT    AS H ON A~SPART = H~SPART
                                         AND H~SPRAS = @SY-LANGU
                 LEFT OUTER JOIN ZMDT0180 AS J ON D~ZZBIG = J~ZBIG
                 LEFT OUTER JOIN ZMDT0181 AS K ON D~ZZMID = K~ZMID
                                              AND D~ZZBIG = K~ZBIG
                 LEFT OUTER JOIN ZMDT0182 AS L ON D~ZZSIN = L~ZSING
                                              AND D~ZZMID = L~ZMID
                 INNER JOIN TVZBT    AS M ON C~ZTERM = M~ZTERM
                                         AND M~SPRAS = @SY-LANGU
                 INNER JOIN T001     AS N ON A~BUKRS_VF = N~BUKRS
                 INNER JOIN VBPA     AS O ON B~VBELN = O~VBELN
                                         AND O~POSNR = '000000'
                                         AND O~PARVW = 'WE'
                 LEFT OUTER JOIN T001L   AS P ON B~LGORT = P~LGORT
                                         AND B~WERKS = P~WERKS
  INTO TABLE @DATA(LT_SEND)
  FOR ALL ENTRIES IN @GT_ZSDT0081
  WHERE A~VBELN = @GT_ZSDT0081-VBELN
    AND B~POSNR = @GT_ZSDT0081-POSNR.

  LOOP AT LT_SEND INTO DATA(LS_SEND).
    MOVE-CORRESPONDING LS_SEND TO GT_LIST.
    APPEND GT_LIST. CLEAR GT_LIST.
  ENDLOOP.

  SORT GT_LIST BY VBELN POSNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_NOT_SENTSO
*&---------------------------------------------------------------------*
FORM SAVE_NOT_SENTSO .

  DATA : LT_0081 LIKE TABLE OF ZSDT0081 WITH HEADER LINE.

  SORT GT_ZSDT0081 BY VBELN POSNR.

*	1. SELECT하고 전송했을때 ERROR로 인해 전송하지 못한 대상은 CBO테이블에
*	  다음에 조회했을때 계속 리스트에서 보여줌
*	  삭제를 했을경우에 전송 FLAG.

  _CLEAR LT_0081.
  LOOP AT GT_LIST.
    CLEAR LT_0081.
    LT_0081-VBELN   = GT_LIST-VBELN.
    LT_0081-POSNR   = GT_LIST-POSNR.
    IF GT_LIST-ICON EQ ICON_GREEN_LIGHT.
      READ TABLE GT_ZSDT0081 WITH KEY VBELN = GT_LIST-VBELN
                                      POSNR = GT_LIST-POSNR BINARY SEARCH
                                      TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        LT_0081-ZSEND = 'X'.
      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      GT_ZSDT0081-ERDAT   = SY-DATLO.
      GT_ZSDT0081-ERZET   = SY-TIMLO.
      GT_ZSDT0081-ERNAM   = SY-UNAME.
    ENDIF.

    APPEND LT_0081.
  ENDLOOP.

  MODIFY ZSDT0081 FROM TABLE LT_0081.

ENDFORM.
