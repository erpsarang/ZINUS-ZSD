*----------------------------------------------------------------------*
***INCLUDE LZSDFG_0003F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_REQUIRED
*&---------------------------------------------------------------------*
FORM CHECK_REQUIRED  USING P_VALUE P_FIELD
                  CHANGING P_TYPE P_MSG.

  CHECK P_TYPE IS INITIAL.

  IF P_VALUE IS INITIAL.
    P_TYPE = GC_E.
    CONCATENATE P_FIELD TEXT-M01 INTO P_MSG SEPARATED BY SPACE.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_DEF_DELIV_TIME
*&---------------------------------------------------------------------*
FORM SET_DEF_DELIV_TIME USING    PS_LFM1 STRUCTURE GS_LFM1
                        CHANGING PS_DATA STRUCTURE ZMMS0070.

  IF PS_DATA-APLFZ EQ 0.
    IF PS_LFM1-PLIFZ EQ 0.
      PS_DATA-APLFZ = 1.
    ELSE.
      PS_DATA-APLFZ = PS_LFM1-PLIFZ.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_QTY
*&---------------------------------------------------------------------*
FORM SET_QTY CHANGING PS_DATA STRUCTURE ZMMS0070.

  IF PS_DATA-NORBM IS INITIAL.
    PS_DATA-NORBM = 1.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_COND_PICK_NUM
*&---------------------------------------------------------------------*
FORM SET_COND_PICK_NUM USING PS_DATA TYPE ZMMS0070.

  IF GV_KNUMH IS NOT INITIAL.
    CLEAR: GS_A017, GV_LINECNT.
    LOOP AT GT_A017 INTO GS_A017 WHERE LIFNR =  PS_DATA-LIFNR
                                   AND MATNR =  PS_DATA-MATNR
                                   AND EKORG =  PS_DATA-EKORG
                                   AND WERKS =  PS_DATA-WERKS
                                   AND ESOKZ =  PS_DATA-ESOKZ
                                   AND DATAB <= PS_DATA-DATAB.
      GV_LINECNT = GV_LINECNT + 1.
    ENDLOOP.

    IF GV_LINECNT IS INITIAL.
      CLEAR: GS_A017, GV_LINECNT.
      LOOP AT GT_A018 INTO GS_A018 WHERE LIFNR =  PS_DATA-LIFNR
                                     AND MATNR =  PS_DATA-MATNR
                                     AND EKORG =  PS_DATA-EKORG
                                     AND ESOKZ =  PS_DATA-ESOKZ
                                     AND DATAB <= PS_DATA-DATAB.
        GV_LINECNT = GV_LINECNT + 1.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXECUTE
*&---------------------------------------------------------------------*
FORM EXECUTE USING PV_TCODE
                   PS_DATA TYPE ZMMS0070.

  DATA: LT_BDCDATA TYPE TABLE OF BDCDATA.

  DATA: LS_OPTION TYPE CTU_PARAMS.

  DATA: LV_NETPR(30),
        LV_APLFZ(3),
        LV_NORBM(13),
        LV_DATAB(8),
        LV_DATBI(8).

  DATA: LV_LINETXT(30).


  CLEAR: LS_OPTION, LV_NETPR, LV_APLFZ, LV_NORBM.
  CLEAR: LT_BDCDATA, LT_BDCDATA[], GT_MESSAGE, GT_MESSAGE[].

  WRITE PS_DATA-KBETR TO LV_NETPR CURRENCY PS_DATA-KONWA LEFT-JUSTIFIED.
  WRITE PS_DATA-NORBM TO LV_NORBM UNIT PS_DATA-KMEIN LEFT-JUSTIFIED.
  WRITE PS_DATA-APLFZ TO LV_APLFZ LEFT-JUSTIFIED.
  WRITE PS_DATA-DATAB TO LV_DATAB LEFT-JUSTIFIED.
  WRITE PS_DATA-DATBI TO LV_DATBI LEFT-JUSTIFIED.

  CLEAR LV_LINETXT.
  CONCATENATE 'VAKE-DATAB(' GV_LINECNT ')' INTO LV_LINETXT.

  CASE PV_TCODE.
    WHEN GC_ME11.
      LT_BDCDATA = VALUE #(
        ( PROGRAM = 'SAPMM06I' DYNPRO = '0100' DYNBEGIN = 'X' )
        ( FNAM = 'EINA-LIFNR'  FVAL = PS_DATA-LIFNR )
        ( FNAM = 'EINA-MATNR'  FVAL = PS_DATA-MATNR )
        ( FNAM = 'EINE-EKORG'  FVAL = PS_DATA-EKORG )
        ( FNAM = 'EINE-WERKS'  FVAL = PS_DATA-WERKS )
        ( FNAM = 'EINA-INFNR'  FVAL = '' )
        ( FNAM = 'BDC_OKCODE'  FVAL = '/00' )

        ( PROGRAM = 'SAPMM06I' DYNPRO = '0101' DYNBEGIN = 'X' )
        ( FNAM = 'EINA-IDNLF'  FVAL = PS_DATA-IDNLF )
        ( FNAM = 'BDC_OKCODE'  FVAL = '=EINE' )

        ( PROGRAM = 'SAPMM06I' DYNPRO = '0102' DYNBEGIN = 'X' )
        ( FNAM = 'EINE-APLFZ'  FVAL = LV_APLFZ )
*        ( FNAM = 'EINE-EKGRP'  FVAL = PS_DATA-EKGRP )
        ( FNAM = 'EINE-NORBM'  FVAL = LV_NORBM )
        ( FNAM = 'EINE-MWSKZ'  FVAL = PS_DATA-MWSKZ )
        ( FNAM = 'EINE-NETPR'  FVAL = LV_NETPR )
        ( FNAM = 'EINE-WAERS'  FVAL = PS_DATA-KONWA )
        ( FNAM = 'MMPUR_INCOTERMS_INFORECORDS-INCO1'   FVAL = PS_DATA-INCO1 )
        ( FNAM = 'MMPUR_INCOTERMS_INFORECORDS-INCO2_L' FVAL = PS_DATA-INCO2 )
        ( FNAM = 'BDC_OKCODE'  FVAL = '=KO' )

        ( PROGRAM = 'SAPMV13A' DYNPRO = '1017' DYNBEGIN = 'X' )
        ( FNAM = 'RV13A-DATAB(01)' FVAL = LV_DATAB )
        ( FNAM = 'RV13A-DATBI(01)' FVAL = LV_DATBI )
        ( FNAM = 'BDC_OKCODE'      FVAL = '=SICH' )
       ).

    WHEN GC_ME12.
      IF GV_KNUMH IS INITIAL.
        LT_BDCDATA = VALUE #(
          ( PROGRAM = 'SAPMM06I' DYNPRO = '0100' DYNBEGIN = 'X' )
          ( FNAM = 'EINA-LIFNR'  FVAL = PS_DATA-LIFNR )
          ( FNAM = 'EINA-MATNR'  FVAL = PS_DATA-MATNR )
          ( FNAM = 'EINE-EKORG'  FVAL = PS_DATA-EKORG )
          ( FNAM = 'EINE-WERKS'  FVAL = PS_DATA-WERKS )
          ( FNAM = 'EINA-INFNR'  FVAL = '' )
          ( FNAM = 'BDC_OKCODE'  FVAL = '/00' )

          ( PROGRAM = 'SAPMM06I' DYNPRO = '0101' DYNBEGIN = 'X' )
          ( FNAM = 'EINA-IDNLF'  FVAL = PS_DATA-IDNLF )
          ( FNAM = 'BDC_OKCODE'  FVAL = '=EINE' )

          ( PROGRAM = 'SAPMM06I' DYNPRO = '0102' DYNBEGIN = 'X' )
          ( FNAM = 'EINE-APLFZ'  FVAL = LV_APLFZ )
*          ( FNAM = 'EINE-EKGRP'  FVAL = PS_DATA-EKGRP )
          ( FNAM = 'EINE-NORBM'  FVAL = LV_NORBM )
          ( FNAM = 'EINE-MWSKZ'  FVAL = PS_DATA-MWSKZ )
          ( FNAM = 'MMPUR_INCOTERMS_INFORECORDS-INCO1'   FVAL = PS_DATA-INCO1 )
          ( FNAM = 'MMPUR_INCOTERMS_INFORECORDS-INCO2_L' FVAL = PS_DATA-INCO2 )
          ( FNAM = 'BDC_OKCODE'  FVAL = '=KO' )

          ( PROGRAM = 'SAPLV14A' DYNPRO = '0102' DYNBEGIN = 'X' )
          ( FNAM = 'BDC_OKCODE'  FVAL = '=NEWD' )

          ( PROGRAM = 'SAPMV13A' DYNPRO = '1017' DYNBEGIN = 'X' )
          ( FNAM = 'KONP-KBETR(01)'  FVAL = LV_NETPR )
          ( FNAM = 'RV13A-DATAB(01)' FVAL = LV_DATAB )
          ( FNAM = 'RV13A-DATBI(01)' FVAL = LV_DATBI )
          ( FNAM = 'BDC_OKCODE'      FVAL = '=SICH' )
         ).
      ELSE.
        LT_BDCDATA = VALUE #(
           ( PROGRAM = 'SAPMM06I' DYNPRO = '0100' DYNBEGIN = 'X' )
           ( FNAM = 'EINA-LIFNR'  FVAL = PS_DATA-LIFNR )
           ( FNAM = 'EINA-MATNR'  FVAL = PS_DATA-MATNR )
           ( FNAM = 'EINE-EKORG'  FVAL = PS_DATA-EKORG )
           ( FNAM = 'EINE-WERKS'  FVAL = PS_DATA-WERKS )
           ( FNAM = 'EINA-INFNR'  FVAL = '' )
           ( FNAM = 'BDC_OKCODE'  FVAL = '/00' )

           ( PROGRAM = 'SAPMM06I' DYNPRO = '0101' DYNBEGIN = 'X' )
           ( FNAM = 'EINA-IDNLF'  FVAL = PS_DATA-IDNLF )
           ( FNAM = 'BDC_OKCODE'  FVAL = '=EINE' )

           ( PROGRAM = 'SAPMM06I' DYNPRO = '0102' DYNBEGIN = 'X' )
           ( FNAM = 'EINE-APLFZ'  FVAL = LV_APLFZ )
*           ( FNAM = 'EINE-EKGRP'  FVAL = PS_DATA-EKGRP )
           ( FNAM = 'EINE-NORBM'  FVAL = LV_NORBM )
           ( FNAM = 'EINE-MWSKZ'  FVAL = PS_DATA-MWSKZ )
           ( FNAM = 'MMPUR_INCOTERMS_INFORECORDS-INCO1'   FVAL = PS_DATA-INCO1 )
           ( FNAM = 'MMPUR_INCOTERMS_INFORECORDS-INCO2_L' FVAL = PS_DATA-INCO2 )
           ( FNAM = 'BDC_OKCODE'  FVAL = '=KO' )

           ( PROGRAM = 'SAPLV14A' DYNPRO = '0102' DYNBEGIN = 'X' )
           ( FNAM = 'BDC_CURSOR'  FVAL = LV_LINETXT )
           ( FNAM = 'BDC_OKCODE'  FVAL = '=PICK' )

           ( PROGRAM = 'SAPMV13A' DYNPRO = '1017' DYNBEGIN = 'X' )
           ( FNAM = 'KONP-KBETR(01)'  FVAL = LV_NETPR )
           ( FNAM = 'RV13A-DATAB(01)' FVAL = LV_DATAB )
           ( FNAM = 'RV13A-DATBI(01)' FVAL = LV_DATBI )
           ( FNAM = 'BDC_OKCODE'      FVAL = '=SICH' )
          ).
      ENDIF.

    WHEN GC_ME13.
      DATA: LV_LIFNR TYPE BAPIEINA-VENDOR,
            LV_MATNR TYPE BAPIEINA-MATERIAL,
            LV_IDNLF TYPE BAPIEINA-VEND_MAT,
            LV_EKORG TYPE BAPIEINE-PURCH_ORG,
            LV_ESOKZ TYPE BAPIEINE-INFO_TYPE,
            LV_WERKS TYPE BAPIEINE-PLANT,
            LV_EKGRP TYPE BAPIEINE-PUR_GROUP,
            LV_INFNR TYPE BAPIEINE-INFO_REC.

      LV_LIFNR = PS_DATA-LIFNR.
      LV_MATNR = PS_DATA-MATNR.
      LV_IDNLF = PS_DATA-IDNLF.
      LV_EKORG = PS_DATA-EKORG.
      LV_ESOKZ = PS_DATA-ESOKZ.
      LV_WERKS = PS_DATA-WERKS.
      LV_EKGRP = PS_DATA-EKGRP.
      LV_INFNR = PS_DATA-INFNR.

      CALL FUNCTION 'BAPI_INFORECORD_GETLIST'
        EXPORTING
          VENDOR              = LV_LIFNR
          MATERIAL            = LV_MATNR
          VEND_MAT            = LV_IDNLF
          PURCH_ORG           = LV_EKORG
          INFO_TYPE           = LV_ESOKZ
          PLANT               = LV_WERKS
          PUR_GROUP           = LV_EKGRP
          PURCHASINGINFOREC   = LV_INFNR
          DELETED_INFORECORDS = PS_DATA-LOEKZ_E
          PURCHORG_DATA       = GC_X
          GENERAL_DATA        = GC_X
        TABLES
          INFORECORD_GENERAL  = GT_BAPIEINA
          INFORECORD_PURCHORG = GT_BAPIEINE
          RETURN              = GT_RETURN.

  ENDCASE.

  CHECK PV_TCODE NE GC_ME13.

  LS_OPTION-DISMODE = GV_BDC_MODE.
  LS_OPTION-UPDMODE = GC_S.
  LS_OPTION-DEFSIZE = GC_X.
*  LS_OPTION-DISMODE = 'A'.
*  LS_OPTION-DISMODE = 'N'.

  CALL TRANSACTION PV_TCODE WITH AUTHORITY-CHECK
                            USING LT_BDCDATA OPTIONS FROM LS_OPTION
                            MESSAGES INTO GT_MESSAGE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_MESSAGE
*&---------------------------------------------------------------------*
FORM GET_MESSAGE  USING PS_MSG STRUCTURE BDCMSGCOLL.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = PS_MSG-MSGID
      MSGNR               = PS_MSG-MSGNR
      MSGV1               = PS_MSG-MSGV1
      MSGV2               = PS_MSG-MSGV2
      MSGV3               = PS_MSG-MSGV3
      MSGV4               = PS_MSG-MSGV4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = GV_RESULT_TXT.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
FORM CHECK_DATA TABLES PT_DATA STRUCTURE ZMMS0070.

  DATA: LV_TABIX LIKE SY-TABIX.

  LOOP AT PT_DATA ASSIGNING FIELD-SYMBOL(<LS_DATA>).

    CLEAR : LV_TABIX.
    LV_TABIX = SY-TABIX.

* Purchasing organization
    IF <LS_DATA>-EKORG IS NOT INITIAL.
      PERFORM CHECK_PURCHASE_ORG USING    <LS_DATA>-EKORG
                                 CHANGING <LS_DATA>-TYPE
                                          <LS_DATA>-MESSAGE.
    ENDIF.

* Plant Data for Material
    IF <LS_DATA>-MATNR IS NOT INITIAL.
      PERFORM CHECK_PLANT_MATERIAL USING    <LS_DATA>-MATNR
                                            <LS_DATA>-WERKS
                                   CHANGING <LS_DATA>-TYPE
                                            <LS_DATA>-MESSAGE.
    ENDIF.

*** Remove vendor check 2020.10.26 - Start
* Vendor master record purchasing organization data
    IF <LS_DATA>-MATNR IS NOT INITIAL.
      PERFORM CHECK_VENDOR_PUR_ORG USING    <LS_DATA>-LIFNR
                                            <LS_DATA>-EKORG
                                   CHANGING <LS_DATA>-TYPE
                                            <LS_DATA>-MESSAGE.
    ENDIF.
** Remove vendor check 2020.10.26 - End

* Purchasing info record category
    IF <LS_DATA>-ESOKZ IS NOT INITIAL.
      PERFORM CHECK_INFO_RECORD_CAT USING    <LS_DATA>-ESOKZ
                                    CHANGING <LS_DATA>-TYPE
                                             <LS_DATA>-MESSAGE.
    ENDIF.

* Plant
    IF <LS_DATA>-WERKS IS NOT INITIAL.
      PERFORM CHECK_PLANT USING    <LS_DATA>-WERKS
                          CHANGING <LS_DATA>-TYPE
                                   <LS_DATA>-MESSAGE.
    ENDIF.

* Purchasing Group
    IF <LS_DATA>-EKGRP IS NOT INITIAL.
      PERFORM CHECK_PURCHASE_GROUP USING    <LS_DATA>-EKGRP
                                   CHANGING <LS_DATA>-TYPE
                                            <LS_DATA>-MESSAGE.
    ENDIF.

* Tax Code
    IF <LS_DATA>-MWSKZ IS NOT INITIAL.
      PERFORM CHECK_TAX_CODE USING    <LS_DATA>-EKORG
                                      <LS_DATA>-WERKS
                                      <LS_DATA>-MWSKZ
                             CHANGING <LS_DATA>-TYPE
                                      <LS_DATA>-MESSAGE.
    ENDIF.

* Confirmation Control Key
    IF <LS_DATA>-BSTAE IS NOT INITIAL.
      PERFORM CHECK_CONF_CRL_KEY USING    <LS_DATA>-BSTAE
                                 CHANGING <LS_DATA>-TYPE
                                          <LS_DATA>-MESSAGE.
    ENDIF.

* Check Message
    IF <LS_DATA>-MESSAGE IS NOT INITIAL.
      <LS_DATA>-TYPE = GC_E.
      CONCATENATE <LS_DATA>-MESSAGE TEXT-M02 INTO <LS_DATA>-MESSAGE SEPARATED BY SPACE.
    ELSE.
      CASE GV_TCODE.
        WHEN GC_ME11 OR GC_ME12.
* SET DEFAULT (Pl.Deliv.Time)
          IF GS_LFM1 IS NOT INITIAL.
            PERFORM SET_DEF_DELIV_TIME USING GS_LFM1 CHANGING <LS_DATA>.
          ENDIF.
* SET QTY (Standard Qty)
          PERFORM SET_QTY CHANGING <LS_DATA>.
* SET CONDITION PICK NUMBER
          PERFORM SET_COND_PICK_NUM USING <LS_DATA>.
      ENDCASE.
    ENDIF.

*    MODIFY PT_DATA FROM <LS_DATA> INDEX LV_TABIX
*                   TRANSPORTING MESSAGE TYPE.

    CLEAR <LS_DATA>.
    CLEAR : GS_LFM1.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_BASIC_DATA
*&---------------------------------------------------------------------*
FORM GET_BASIC_DATA TABLES PT_DATA STRUCTURE ZMMS0070.

  DATA: LT_DD07V TYPE TABLE OF DD07V,
        LS_DD07V TYPE DD07V.

  DATA : BEGIN OF LT_WERKS_C OCCURS 0,
           WERKS TYPE T001W-WERKS,
         END OF LT_WERKS_C.

  DATA : BEGIN OF LT_EKGRP_C OCCURS 0,
           EKGRP TYPE T024-EKGRP,
         END OF LT_EKGRP_C.

  DATA : BEGIN OF LT_EKORG_C OCCURS 0,
           EKORG TYPE T024W-EKORG,
         END OF LT_EKORG_C.

  DATA : BEGIN OF LT_TAXCD_C OCCURS 0,
           KALSM TYPE T007A-KALSM,
           MWSKZ TYPE T007A-MWSKZ,
         END OF LT_TAXCD_C.

  DATA : BEGIN OF LT_BSTAE_C OCCURS 0,
           BSTAE TYPE T163L-BSTAE,
         END OF LT_BSTAE_C.

  CLEAR: GT_MARC_C, GT_MARC_C[].
  GT_MARC_C[] = CORRESPONDING #( PT_DATA[] ).

  CLEAR: GT_LFM1_C, GT_LFM1_C[].
  GT_LFM1_C[] = CORRESPONDING #( PT_DATA[] ).

  SORT GT_MARC_C BY MATNR WERKS.
  DELETE ADJACENT DUPLICATES FROM GT_MARC_C COMPARING MATNR WERKS.

  SORT GT_LFM1_C BY EKORG.
  DELETE ADJACENT DUPLICATES FROM GT_LFM1_C COMPARING EKORG.

  IF GT_MARC_C[] IS NOT INITIAL.
    SELECT
      RC~MATNR,
      RC~WERKS
      INTO CORRESPONDING FIELDS OF TABLE @GT_MARC
      FROM MARA AS RA INNER JOIN MARC AS RC
                              ON RA~MATNR = RC~MATNR
      FOR ALL ENTRIES IN @GT_MARC_C
     WHERE RC~MATNR EQ @GT_MARC_C-MATNR
       AND RC~WERKS EQ @GT_MARC_C-WERKS
       AND RC~LVORM EQ @ABAP_OFF.
    IF SY-SUBRC = 0.
      SORT GT_MARC BY MATNR WERKS.
    ENDIF.
  ENDIF.

  IF GT_LFM1_C[] IS NOT INITIAL.
    SELECT
      LIFNR,
      EKORG,
      PLIFZ
      INTO CORRESPONDING FIELDS OF TABLE @GT_LFM1
      FROM LFM1
      FOR ALL ENTRIES IN @GT_LFM1_C
     WHERE EKORG EQ @GT_LFM1_C-EKORG
       AND LOEVM EQ @ABAP_OFF.
    IF SY-SUBRC = 0.
      SORT GT_LFM1 BY LIFNR EKORG.
    ENDIF.
  ENDIF.

* Get domain value - Purchasing info record category
  CLEAR : GS_INFOCAT, GT_INFOCAT[].
  CLEAR : LT_DD07V, LT_DD07V[].
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      DOMNAME         = 'ESOKZ'
    TABLES
      VALUES_TAB      = LT_DD07V
    EXCEPTIONS
      NO_VALUES_FOUND = 1
      OTHERS          = 2.
  IF LT_DD07V[] IS NOT INITIAL.
    LOOP AT LT_DD07V INTO LS_DD07V.
      CLEAR : GS_INFOCAT.
      GS_INFOCAT-ESOKZ = LS_DD07V-DOMVALUE_L.
      APPEND GS_INFOCAT TO GT_INFOCAT.

      CLEAR : LS_DD07V.
    ENDLOOP.

    SORT GT_INFOCAT BY ESOKZ.
  ENDIF.

* Check plant
  LT_WERKS_C[] = CORRESPONDING #( PT_DATA[] ).
  SORT LT_WERKS_C BY WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_WERKS_C COMPARING WERKS.

  IF LT_WERKS_C[] IS NOT INITIAL.
    SELECT WERKS
           INTO CORRESPONDING FIELDS OF TABLE GT_T001W
           FROM T001W
           FOR ALL ENTRIES IN LT_WERKS_C
           WHERE WERKS EQ LT_WERKS_C-WERKS.
    IF SY-SUBRC = 0.
      SORT GT_T001W BY WERKS.
    ENDIF.
  ENDIF.

* Check Purchasing Group
  LT_EKGRP_C[] = CORRESPONDING #( PT_DATA[] ).
  SORT LT_EKGRP_C BY EKGRP.
  DELETE ADJACENT DUPLICATES FROM LT_EKGRP_C COMPARING EKGRP.

  IF LT_EKGRP_C[] IS NOT INITIAL.
    SELECT EKGRP
           INTO CORRESPONDING FIELDS OF TABLE GT_EKGRP
           FROM T024
           FOR ALL ENTRIES IN LT_EKGRP_C
           WHERE EKGRP EQ LT_EKGRP_C-EKGRP.
    IF SY-SUBRC = 0.
      SORT GT_EKGRP BY EKGRP.
    ENDIF.
  ENDIF.

* Check Tax Code
  LT_EKORG_C[] = CORRESPONDING #( PT_DATA[] ).
  SORT LT_EKORG_C BY EKORG.
  DELETE ADJACENT DUPLICATES FROM LT_EKORG_C COMPARING EKORG.

  IF LT_TAXCD_C[] IS NOT INITIAL.
    SELECT A~EKORG, A~WERKS, D~MWSKZ, B~LAND1, D~KALSM
           INTO CORRESPONDING FIELDS OF TABLE @GT_TAXCD
           FROM  T024W AS A LEFT JOIN T001W AS B
           ON    A~WERKS EQ B~WERKS
           LEFT  JOIN T005 AS C
           ON    B~LAND1 EQ C~LAND1
           LEFT JOIN T007A AS D
           ON    C~KALSM EQ D~KALSM
           FOR ALL ENTRIES IN @LT_EKORG_C
           WHERE A~EKORG EQ @LT_EKORG_C-EKORG.
    IF SY-SUBRC = 0.
      SORT GT_TAXCD BY EKORG WERKS MWSKZ.
    ENDIF.
  ENDIF.

* Check Confirmation Control Key
  LT_BSTAE_C[] = CORRESPONDING #( PT_DATA[] ).
  SORT LT_BSTAE_C BY BSTAE.
  DELETE ADJACENT DUPLICATES FROM LT_BSTAE_C COMPARING BSTAE.

  IF LT_BSTAE_C[] IS NOT INITIAL.
    SELECT BSTAE
           INTO CORRESPONDING FIELDS OF TABLE GT_CONFKEY
           FROM T163L
           FOR ALL ENTRIES IN LT_BSTAE_C
           WHERE BSTAE EQ LT_BSTAE_C-BSTAE.
    IF SY-SUBRC = 0.
      SORT GT_CONFKEY BY BSTAE.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_INFO_RECORD_DATA
*&---------------------------------------------------------------------*
FORM GET_INFO_RECORD_DATA TABLES PT_DATA STRUCTURE ZMMS0070.

  CLEAR: GT_EINE, GT_EINE[].

  SELECT
    NA~MATNR,
    NA~LIFNR,
    NE~INFNR,
    NE~EKORG,
    NE~ESOKZ,
    NE~WERKS,
    NE~NETPR,
    NE~WAERS,
    NE~PEINH,
    NE~BPRME
    INTO TABLE @GT_EINE
    FROM EINA AS NA INNER JOIN EINE AS NE
                            ON NA~INFNR = NE~INFNR
    FOR ALL ENTRIES IN @PT_DATA
   WHERE NA~MATNR EQ @PT_DATA-MATNR
*     AND NA~LIFNR EQ @PT_DATA-LIFNR  " 2020.10.26 - Remove logic
     AND NE~EKORG EQ @PT_DATA-EKORG
     AND NE~ESOKZ EQ @PT_DATA-ESOKZ
     AND NE~WERKS EQ @PT_DATA-WERKS.
  IF SY-SUBRC = 0.
    SORT GT_EINE BY MATNR LIFNR EKORG ESOKZ WERKS.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_CONDITION_DATA
*&---------------------------------------------------------------------*
FORM GET_CONDITION_DATA TABLES PT_DATA STRUCTURE ZMMS0070.

  DATA : BEGIN OF LT_KNUMH_C OCCURS 0,
           KNUMH TYPE KONP-KNUMH,
         END OF LT_KNUMH_C.

  CLEAR: GT_KONH, GT_KONH[],
         GT_A017, GT_A017[],
         GT_A018, GT_A018[].

  SELECT
    KAPPL,
    KSCHL,
    LIFNR,
    MATNR,
    EKORG,
    WERKS,
    ESOKZ,
    DATBI,
    DATAB,
    KNUMH
    INTO CORRESPONDING FIELDS OF TABLE @GT_A017
    FROM A017
    FOR ALL ENTRIES IN @PT_DATA
*   WHERE LIFNR EQ @PT_DATA-LIFNR   - 2020.10.26 - Remove logic
    WHERE MATNR EQ @PT_DATA-MATNR
     AND  EKORG EQ @PT_DATA-EKORG
     AND  WERKS EQ @PT_DATA-WERKS
     AND  ESOKZ EQ @PT_DATA-ESOKZ
     AND  ( DATBI >= @PT_DATA-DATAB AND DATAB <= @PT_DATA-DATAB ).
  IF SY-SUBRC = 0.
    SORT GT_A017 BY LIFNR MATNR EKORG WERKS ESOKZ. "DATBI.
  ENDIF.

  CLEAR : LT_KNUMH_C, LT_KNUMH_C[].
  IF GT_A017[] IS NOT INITIAL.

    LT_KNUMH_C[] = CORRESPONDING #( GT_A017[] ).
    SORT LT_KNUMH_C BY KNUMH.
    DELETE ADJACENT DUPLICATES FROM LT_KNUMH_C COMPARING KNUMH.

    IF LT_KNUMH_C[] IS NOT INITIAL.
      SELECT
        NH~KNUMH,
        NH~DATAB,
        NH~DATBI,
        NP~KOPOS,
        NP~KBETR,
        NP~KONWA,
        NP~KPEIN,
        NP~KMEIN
        INTO CORRESPONDING FIELDS OF TABLE @GT_KONH
        FROM KONH AS NH INNER JOIN KONP AS NP
                                ON NH~KNUMH = NP~KNUMH
        FOR ALL ENTRIES IN @LT_KNUMH_C
        WHERE NH~KNUMH EQ @LT_KNUMH_C-KNUMH.
    ENDIF.
  ENDIF.

  SELECT
    KAPPL,
    KSCHL,
    LIFNR,
    MATNR,
    EKORG,
    ESOKZ,
    DATBI,
    DATAB,
    KNUMH
    INTO CORRESPONDING FIELDS OF TABLE @GT_A018
    FROM A018
    FOR ALL ENTRIES IN @PT_DATA
*   WHERE LIFNR EQ @PT_DATA-LIFNR  - 2020.10.26 - Remove logic
     WHERE MATNR EQ @PT_DATA-MATNR
     AND   EKORG EQ @PT_DATA-EKORG
     AND   ESOKZ EQ @PT_DATA-ESOKZ
     AND ( DATBI >= @PT_DATA-DATAB AND DATAB <= @PT_DATA-DATAB ).
  IF SY-SUBRC = 0.
    SORT GT_A018 BY LIFNR MATNR EKORG ESOKZ. "DATBI.
  ENDIF.

  CLEAR : LT_KNUMH_C, LT_KNUMH_C[].
  IF GT_A018[] IS NOT INITIAL.

    LT_KNUMH_C[] = CORRESPONDING #( GT_A018[] ).
    SORT LT_KNUMH_C BY KNUMH.
    DELETE ADJACENT DUPLICATES FROM LT_KNUMH_C COMPARING KNUMH.

    IF LT_KNUMH_C[] IS NOT INITIAL.
      SELECT
        NH~KNUMH,
        NH~DATAB,
        NH~DATBI,
        NP~KOPOS,
        NP~KBETR,
        NP~KONWA,
        NP~KPEIN,
        NP~KMEIN
        APPENDING CORRESPONDING FIELDS OF TABLE @GT_KONH
        FROM KONH AS NH INNER JOIN KONP AS NP
                                ON NH~KNUMH = NP~KNUMH
        FOR ALL ENTRIES IN @LT_KNUMH_C
        WHERE NH~KNUMH EQ @LT_KNUMH_C-KNUMH.
    ENDIF.
  ENDIF.

  IF GT_KONH[] IS NOT INITIAL.
    SORT GT_KONH BY KNUMH.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PURCHASE_ORG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_EKORG
*&      <-- LS_DATA_MESSAGE
*&---------------------------------------------------------------------*
FORM CHECK_PURCHASE_ORG  USING    PV_EKORG
                         CHANGING PV_TYPE
                                  PV_MESSAGE.

  READ TABLE GT_EKORG WITH KEY EKORG = PV_EKORG
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    PV_TYPE = GC_E.

    IF PV_MESSAGE IS INITIAL.
      PV_MESSAGE = GC_EKORG.
    ELSE.
      CONCATENATE PV_MESSAGE GC_EKORG INTO PV_MESSAGE SEPARATED BY '/'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PLANT_MATERIAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_DATA>_MATNR
*&      --> <LS_DATA>_WERKS
*&      <-- <LS_DATA>_TYPE
*&      <-- <LS_DATA>_MESSAGE
*&---------------------------------------------------------------------*
FORM CHECK_PLANT_MATERIAL  USING    PV_MATNR
                                    PV_WERKS
                           CHANGING PV_TYPE
                                    PV_MESSAGE.

  READ TABLE GT_MARC WITH KEY MATNR = PV_MATNR
                              WERKS = PV_WERKS
                              BINARY SEARCH
                              TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    PV_TYPE = GC_E.

    IF PV_MESSAGE IS INITIAL.
      PV_MESSAGE = GC_MATNR.
    ELSE.
      CONCATENATE PV_MESSAGE GC_MATNR INTO PV_MESSAGE SEPARATED BY '/'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INFO_RECORD_CAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_DATA>_ESOKZ
*&      <-- <LS_DATA>_TYPE
*&      <-- <LS_DATA>_MESSAGE
*&---------------------------------------------------------------------*
FORM CHECK_INFO_RECORD_CAT  USING    PV_ESOKZ
                            CHANGING PV_TYPE
                                     PV_MESSAGE.

  READ TABLE GT_INFOCAT WITH KEY ESOKZ = PV_ESOKZ
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    PV_TYPE = GC_E.

    IF PV_MESSAGE IS INITIAL.
      PV_MESSAGE = GC_ESOKZ.
    ELSE.
      CONCATENATE PV_MESSAGE GC_ESOKZ INTO PV_MESSAGE SEPARATED BY '/'.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PLANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_WERKS
*&      <-- LS_DATA_MESSAGE
*&---------------------------------------------------------------------*
FORM CHECK_PLANT  USING    PV_WERKS
                  CHANGING PV_TYPE
                           PV_MESSAGE.

  READ TABLE GT_T001W WITH KEY WERKS = PV_WERKS
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    PV_TYPE = GC_E.

    IF PV_MESSAGE IS INITIAL.
      PV_MESSAGE = GC_WERKS.
    ELSE.
      CONCATENATE PV_MESSAGE GC_WERKS INTO PV_MESSAGE SEPARATED BY '/'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PURCHASE_GROUP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_EKGRP
*&      <-- LS_DATA_MESSAGE
*&---------------------------------------------------------------------*
FORM CHECK_PURCHASE_GROUP  USING    PV_EKGRP
                           CHANGING PV_TYPE
                                    PV_MESSAGE.

  READ TABLE GT_EKGRP WITH KEY EKGRP = PV_EKGRP
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    PV_TYPE = GC_E.

    IF PV_MESSAGE IS INITIAL.
      PV_MESSAGE = GC_EKGRP.
    ELSE.
      CONCATENATE PV_MESSAGE GC_EKGRP INTO PV_MESSAGE SEPARATED BY '/'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_TAX_CODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_MWSKZ
*&      <-- LS_DATA_MESSAGE
*&---------------------------------------------------------------------*
FORM CHECK_TAX_CODE  USING    PV_EKORG
                              PV_WERKS
                              PV_MWSKZ
                     CHANGING PV_TYPE
                              PV_MESSAGE.

  IF PV_WERKS IS INITIAL.
    READ TABLE GT_TAXCD WITH KEY EKORG = PV_EKORG
                                 MWSKZ = PV_MWSKZ
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      PV_TYPE = GC_E.

      IF PV_MESSAGE IS INITIAL.
        PV_MESSAGE = GC_MWSKZ.
      ELSE.
        CONCATENATE PV_MESSAGE GC_MWSKZ INTO PV_MESSAGE SEPARATED BY '/'.
      ENDIF.
    ENDIF.
  ELSE.
    READ TABLE GT_TAXCD WITH KEY EKORG = PV_EKORG
                                 WERKS = PV_WERKS
                                 MWSKZ = PV_MWSKZ
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      PV_TYPE = GC_E.

      IF PV_MESSAGE IS INITIAL.
        PV_MESSAGE = GC_MWSKZ.
      ELSE.
        CONCATENATE PV_MESSAGE GC_MWSKZ INTO PV_MESSAGE SEPARATED BY '/'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CONF_CRL_KEY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_BSTAE
*&      <-- LS_DATA_MESSAGE
*&---------------------------------------------------------------------*
FORM CHECK_CONF_CRL_KEY  USING    PV_BSTAE "T163L
                         CHANGING PV_TYPE
                                  PV_MESSAGE.



  READ TABLE GT_CONFKEY WITH KEY BSTAE = PV_BSTAE
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    PV_TYPE = GC_E.

    IF PV_MESSAGE IS INITIAL.
      PV_MESSAGE = GC_BSTAE.
    ELSE.
      CONCATENATE PV_MESSAGE GC_BSTAE INTO PV_MESSAGE SEPARATED BY '/'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VENDOR_PUR_ORG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_DATA>_LIFNR
*&      --> <LS_DATA>_EKORG
*&      <-- <LS_DATA>_TYPE
*&      <-- <LS_DATA>_MESSAGE
*&---------------------------------------------------------------------*
FORM CHECK_VENDOR_PUR_ORG  USING    PV_LIFNR
                                    PV_EKORG
                           CHANGING PV_TYPE
                                    PV_MESSAGE.

  READ TABLE GT_LFM1 INTO GS_LFM1 WITH KEY LIFNR = PV_LIFNR
                                           EKORG = PV_EKORG
                                           BINARY SEARCH.
  IF SY-SUBRC NE 0.
    PV_TYPE = GC_E.

    IF PV_MESSAGE IS INITIAL.
      PV_MESSAGE = GC_LIFNR.
    ELSE.
      CONCATENATE PV_MESSAGE GC_LIFNR INTO PV_MESSAGE SEPARATED BY '/'.
    ENDIF.
  ENDIF.

ENDFORM.
