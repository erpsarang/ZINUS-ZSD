*&---------------------------------------------------------------------*
*& Include          ZSDR0080_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  CLEAR: GS_STBL, GS_STBL_200, GS_STBL_300,
         GS_STBL_400, GS_STBL_500, GS_STBL_600,
         GS_STBL_700.

  GS_STBL-ROW = 'X'. GS_STBL-COL = 'X'.
  GS_STBL_200-ROW = 'X'. GS_STBL_200-COL = 'X'.
  GS_STBL_300-ROW = 'X'. GS_STBL_300-COL = 'X'.
  GS_STBL_400-ROW = 'X'. GS_STBL_400-COL = 'X'.
  GS_STBL_500-ROW = 'X'. GS_STBL_500-COL = 'X'.
  GS_STBL_600-ROW = 'X'. GS_STBL_600-COL = 'X'.
  GS_STBL_700-ROW = 'X'. GS_STBL_700-COL = 'X'.

  SET PARAMETER ID 'CAC' FIELD '2000'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BASE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_BASE_DATA .
  DATA: BEGIN OF LT_VBELN OCCURS 0,
          VBELN LIKE VBAP-VBELN,
        END OF LT_VBELN.

  DATA: BEGIN OF LT_SALES OCCURS 0,
          VBELN LIKE VBAP-VBELN,
          POSNR LIKE VBAP-POSNR,
        END OF LT_SALES.

  DATA: BEGIN OF LT_KNUMV OCCURS 0,
          KNUMV LIKE VBAK-KNUMV,
          POSNR LIKE VBAP-POSNR,
        END OF LT_KNUMV.

  DATA: BEGIN OF LT_REC OCCURS 0,
          VGBEL     LIKE LIPS-VGBEL,
          VGPOS     LIKE LIPS-VGPOS,
          VBELN     LIKE LIPS-VBELN,
          POSNR     LIKE LIPS-POSNR,
          MATNR     LIKE LIPS-MATNR,
          MATWA     LIKE LIPS-MATWA,
          LFIMG     LIKE LIPS-LFIMG,
          WADAT_IST LIKE LIKP-WADAT_IST,
          PDSTK     LIKE LIKP-PDSTK,
          PODAT     LIKE LIKP-PODAT,
          BLDAT     LIKE LIKP-BLDAT,
          LIFEX     LIKE LIKP-LIFEX,
        END OF LT_REC.


  DATA: BEGIN OF LT_DELIV OCCURS 0,
          VBELN LIKE LIPS-VBELN,
        END OF LT_DELIV.

  DATA: BEGIN OF LT_OBJCT OCCURS 0,
          VBELN    LIKE LIPS-VBELN,
          VPOBJKEY LIKE VEKP-VPOBJKEY,
        END OF LT_OBJCT.

  DATA: BEGIN OF LT_LIPS OCCURS 0,
          VBELN LIKE LIPS-VBELN,
          POSNR LIKE LIPS-POSNR,
        END OF LT_LIPS.

  DATA: LV_PO TYPE SY-INDEX,
        LV_CO TYPE SY-INDEX.

  DATA : LS_COND     LIKE GS_COND,
         LT_COND     LIKE TABLE OF LS_COND,
         LT_COND_TMP LIKE TABLE OF LS_COND WITH HEADER LINE.

  DATA : LT_VBUV_H LIKE TABLE OF GS_VBUV_H WITH HEADER LINE,
         LT_VBUV   LIKE TABLE OF GS_VBUV   WITH HEADER LINE.

  DATA : BEGIN OF LT_WAVE OCCURS 0,
           SAMMG LIKE VBSK-SAMMG,
         END OF LT_WAVE.

* SALES ORDER DATA
  PERFORM SELECT_DATA.

  CLEAR: GT_SALES. SORT GT_SALES BY VBELN POSNR.
  _CLEAR: LT_SALES, GT_BILLI.
  LOOP AT GT_SALES.
* ????????????
    MOVE-CORRESPONDING GT_SALES TO LT_VBELN.
    _APPEND LT_VBELN.
* ????????????&?????????
    MOVE-CORRESPONDING GT_SALES TO LT_SALES.
    _APPEND LT_SALES.
* ????????????&?????????
    MOVE-CORRESPONDING GT_SALES TO LT_KNUMV.
    _APPEND LT_KNUMV.
    CLEAR : GT_SALES.
  ENDLOOP.

  IF LT_KNUMV[] IS NOT INITIAL.
    _CLEAR: GT_COND.
    _CLEAR: LT_COND_TMP.
    _CLEAR: LT_COND.
    CLEAR: LT_KNUMV. SORT LT_KNUMV BY KNUMV POSNR.
    DELETE ADJACENT DUPLICATES FROM LT_KNUMV.

    SELECT KNUMV
           KPOSN
           KSCHL
           KBETR
           WAERS
      INTO CORRESPONDING FIELDS OF TABLE LT_COND_TMP
      FROM PRCD_ELEMENTS FOR ALL ENTRIES IN LT_KNUMV
     WHERE KNUMV EQ LT_KNUMV-KNUMV
       AND KPOSN EQ LT_KNUMV-POSNR
       AND KBETR NE 0.
    IF SY-SUBRC = 0.
      LOOP AT LT_COND_TMP.

        MOVE-CORRESPONDING LT_COND_TMP TO LS_COND.
        COLLECT LS_COND INTO LT_COND.

      ENDLOOP.
      IF LT_COND[] IS NOT INITIAL.
        GT_COND[] = LT_COND[].
      ENDIF.
    ENDIF.
  ENDIF.

  IF LT_VBELN[] IS NOT INITIAL.
    CLEAR: LT_VBELN.
    SORT LT_VBELN BY VBELN.
    DELETE ADJACENT DUPLICATES FROM LT_VBELN COMPARING VBELN.
* ???????????? ????????? ?????? ???????????? ??????
    LOOP AT LT_VBELN.
      _CLEAR: GT_DEL.
      CLEAR: LV_PO, LV_CO.
      SELECT COUNT( * ) INTO  LV_PO FROM  VBAP
                        WHERE VBELN EQ LT_VBELN-VBELN.

      SELECT COUNT( * ) INTO  LV_CO FROM VBAP
                        WHERE VBELN EQ LT_VBELN-VBELN
                        AND   ABGRU NE ABAP_OFF.
      IF LV_PO = LV_CO.
        GS_DEL-VBELN = LT_VBELN-VBELN.
        INSERT GS_DEL INTO TABLE GT_DEL.
      ENDIF.

      CLEAR : LT_VBELN.
    ENDLOOP.
* SHIP TO -> ONE TIME ??? ?????? ?????? ??????
    _CLEAR: GT_SH.
    SELECT A~VBELN
           A~KUNNR AS KUNWE
           B~NAME1 AS SAME1
           B~STREET
           B~POST_CODE1
           B~REGION
           B~CITY1
           B~TEL_NUMBER
      INTO CORRESPONDING FIELDS OF TABLE GT_SH
      FROM VBPA AS A INNER JOIN ADRC AS B
                        ON A~ADRNR = B~ADDRNUMBER
       FOR ALL ENTRIES IN LT_VBELN
     WHERE A~VBELN EQ LT_VBELN-VBELN
       AND A~POSNR EQ '000000'
       AND A~PARVW EQ 'WE'.
*-- Carrier ?????? ??????
    _CLEAR: GT_SP.
    SELECT A~VBELN
           B~LIFNR
           B~NAME1 AS LAME1
      INTO CORRESPONDING FIELDS OF TABLE GT_SP
      FROM VBPA AS A INNER JOIN LFA1 AS B
                        ON A~LIFNR = B~LIFNR
       FOR ALL ENTRIES IN LT_VBELN
     WHERE A~VBELN EQ LT_VBELN-VBELN
       AND A~PARVW EQ 'SP'.
*-- Incomplete ?????? ??????
    _CLEAR: GT_VBUV,   GT_VBUV_H.
    _CLEAR: LT_VBUV_H, LT_VBUV_H.
    _CLEAR: LT_VBUV,   LT_VBUV.
    SELECT VBELN,
           POSNR,
           FDNAM
           INTO TABLE @DATA(LT_VBUV_TMP)
           FROM VBUV
           FOR ALL ENTRIES IN @LT_VBELN
           WHERE VBELN EQ @LT_VBELN-VBELN.
    IF SY-SUBRC = 0.
      SORT LT_VBUV_TMP BY VBELN POSNR.
      LOOP AT LT_VBUV_TMP INTO DATA(LS_VBUV_TMP).
        IF LS_VBUV_TMP-POSNR = '000000'.
          MOVE-CORRESPONDING LS_VBUV_TMP TO LT_VBUV_H.
          _APPEND LT_VBUV_H.
        ELSE.
          MOVE-CORRESPONDING LS_VBUV_TMP TO LT_VBUV.
          _APPEND LT_VBUV.
        ENDIF.

        IF LT_VBUV_H[] IS NOT INITIAL.
          SORT LT_VBUV_H.
          DELETE ADJACENT DUPLICATES FROM LT_VBUV_H COMPARING VBELN.

          IF LT_VBUV_H[] IS NOT INITIAL.
            GT_VBUV_H[] = LT_VBUV_H[].
          ENDIF.
        ENDIF.

        IF LT_VBUV[] IS NOT INITIAL.
          SORT LT_VBUV.
          DELETE ADJACENT DUPLICATES FROM LT_VBUV COMPARING VBELN POSNR.

          IF LT_VBUV[] IS NOT INITIAL.
            GT_VBUV[] = LT_VBUV[].
          ENDIF.
        ENDIF.

        CLEAR : LS_VBUV_TMP.
      ENDLOOP.
    ENDIF.
*-- Salse Order ?????? ?????? ??????
    _CLEAR: GT_0021.
    SELECT VBELN,
           DOCNUM,
           ZIFTYPE
           INTO TABLE @DATA(LT_0021)
           FROM ZSD1T0021
           FOR ALL ENTRIES IN @LT_VBELN
           WHERE VBELN EQ @LT_VBELN-VBELN.
    IF SY-SUBRC = 0.
      SORT LT_0021 BY VBELN.
      DELETE ADJACENT DUPLICATES FROM LT_0021 COMPARING VBELN.

      IF LT_0021[] IS NOT INITIAL.
        GT_0021[] = LT_0021[].
      ENDIF.
    ENDIF.
*-- 855 ACK OUT BOUND ?????? ??????
    _CLEAR: GT_0023.
    SELECT VBELN,
           DOCNUM
           INTO TABLE @DATA(LT_0023)
           FROM ZSD1T0023
           FOR ALL ENTRIES IN @LT_VBELN
           WHERE VBELN EQ @LT_VBELN-VBELN.
    IF SY-SUBRC = 0.
      SORT LT_0023 BY VBELN.
      DELETE ADJACENT DUPLICATES FROM LT_0023 COMPARING VBELN.

      IF LT_0023[] IS NOT INITIAL.
        GT_0023[] = LT_0023[].
      ENDIF.
    ENDIF.
*-- 810 INVOICE OUT BOUND ?????? ??????
    _CLEAR: GT_0024.
    SELECT VBELN,
           DOCNUM,
           VBEVF
           INTO TABLE @DATA(LT_0024)
           FROM ZSD1T0024
           FOR ALL ENTRIES IN @LT_VBELN
           WHERE VBELN EQ @LT_VBELN-VBELN.
    IF SY-SUBRC = 0.
      SORT LT_0024 BY VBELN ASCENDING DOCNUM DESCENDING.
      DELETE ADJACENT DUPLICATES FROM LT_0024 COMPARING VBELN.

      IF LT_0024[] IS NOT INITIAL.
        GT_0024[] = LT_0024[].
      ENDIF.
    ENDIF.
* ?????? ?????? ?????? ??????
    IF LT_SALES[] IS NOT INITIAL.
      _CLEAR: LT_REC.
      SELECT A~VGBEL
             A~VGPOS
             A~VBELN
             A~POSNR
             A~MATNR
             A~MATWA
             A~LFIMG
             B~WADAT_IST
             B~PDSTK
             B~PODAT
             B~BLDAT
             B~LIFEX
             INTO CORRESPONDING FIELDS OF TABLE LT_REC
             FROM LIPS AS A INNER JOIN LIKP AS B
             ON   A~VBELN = B~VBELN
             FOR ALL ENTRIES IN LT_SALES
             WHERE A~VGBEL     EQ LT_SALES-VBELN
               AND A~VGPOS     EQ LT_SALES-POSNR
               AND B~WADAT_IST IN S_WADAT.
    ENDIF.
    _CLEAR: GT_DELIV.
    CLEAR: LT_REC.
    SORT: LT_REC BY VGBEL VGPOS.
    _CLEAR GT_REMAIN.
    LOOP AT LT_REC.

      MOVE-CORRESPONDING LT_REC TO GT_DELIV.
      _APPEND GT_DELIV.

*?????? ?????? ?????????
      MOVE-CORRESPONDING LT_REC TO GT_REMAIN.
      COLLECT GT_REMAIN. CLEAR GT_REMAIN.
    ENDLOOP.

  ENDIF.

  CLEAR : GS_VBEP, GT_VBEP[].
  IF LT_SALES[] IS NOT INITIAL.
    CLEAR: LT_SALES.
    SORT LT_SALES BY VBELN POSNR.
    DELETE ADJACENT DUPLICATES FROM LT_SALES COMPARING VBELN POSNR.
* First Delivery Date
    SELECT VBELN,
           POSNR,
           ETENR,
           EDATU,
           LDDAT
      INTO TABLE @DATA(LT_VBEP_TMP)
      FROM VBEP
       FOR ALL ENTRIES IN @LT_SALES
     WHERE VBELN EQ @LT_SALES-VBELN
       AND POSNR EQ @LT_SALES-POSNR.
    IF SY-SUBRC = 0.

      SORT LT_VBEP_TMP BY VBELN POSNR ETENR DESCENDING.
      DELETE ADJACENT DUPLICATES FROM LT_VBEP_TMP COMPARING VBELN POSNR.

      IF LT_VBEP_TMP[] IS NOT INITIAL.
        GT_VBEP[] = LT_VBEP_TMP[].
      ENDIF.

    ENDIF.
* ???????????? ?????? ??????
    _CLEAR: GT_0025.
    SELECT VBELN,
           POSNR,
           DOCNUM,
           BSTKD,
           POSEX
           INTO TABLE @DATA(LT_0025)
           FROM ZSD1T0025
           FOR ALL ENTRIES IN @LT_SALES
           WHERE VBELN EQ @LT_SALES-VBELN
           AND POSNR   EQ @LT_SALES-POSNR.
    IF SY-SUBRC = 0.
      SORT LT_0025 BY VBELN POSNR.
      DELETE ADJACENT DUPLICATES FROM LT_0025 COMPARING VBELN POSNR.

      IF LT_0025[] IS NOT INITIAL.
        GT_0025[] = LT_0025.
      ENDIF.
    ENDIF.
*Billing ?????? ??????
    SELECT A~VGBEL, A~VGPOS, A~AUBEL, A~AUPOS,
           A~VBELN, A~POSNR, A~NETWR,
           A~MATNR, A~ARKTX, A~FKIMG, A~VRKME, A~MWSBP, A~KZWI1,
           B~SFAKN, B~FKSTO, B~FKDAT, B~ZZKOINV,
           B~KUNRG, B~WAERK,
           C~NAME_ORG1
           INTO CORRESPONDING FIELDS OF TABLE @GT_BILLI
           FROM VBRP AS A INNER JOIN VBRK   AS B ON A~VBELN = B~VBELN
                     LEFT OUTER JOIN BUT000 AS C ON B~KUNRG = C~PARTNER
           FOR ALL ENTRIES IN @LT_SALES
           WHERE A~VGBEL EQ @LT_SALES-VBELN
           AND   A~VGPOS EQ @LT_SALES-POSNR.

  ENDIF.

  CLEAR: GT_DELIV.
  SORT GT_DELIV BY VBELN POSNR.
  _CLEAR: LT_DELIV, LT_OBJCT, LT_LIPS.

  LOOP AT GT_DELIV.
* ????????????
    CLEAR : LT_DELIV.
    MOVE-CORRESPONDING GT_DELIV TO LT_DELIV.
    _APPEND LT_DELIV.
* ???????????? & ???????????? ??????
    CLEAR : LT_OBJCT.
    MOVE-CORRESPONDING GT_DELIV TO LT_OBJCT.
    LT_OBJCT-VPOBJKEY = LT_OBJCT-VBELN.
    _APPEND LT_OBJCT.
* ???????????? & ?????????
    CLEAR : LT_LIPS.
    MOVE-CORRESPONDING GT_DELIV TO LT_LIPS.
    _APPEND LT_LIPS.

    CLEAR : GT_DELIV.
  ENDLOOP.

  IF LT_DELIV[] IS NOT INITIAL.
    CLEAR: LT_DELIV.
    SORT LT_DELIV BY VBELN.
    DELETE ADJACENT DUPLICATES FROM LT_DELIV.
* Shipment ?????? ??????
    _CLEAR: GT_TKNUM.
    SELECT A~VBELN, B~TKNUM, B~DALEN, B~ERDAT, B~EXTI2      "ARN No
           INTO TABLE @DATA(LT_TKNUM)
           FROM VTTP AS A INNER JOIN VTTK AS B
           ON    A~TKNUM = B~TKNUM
           FOR ALL ENTRIES IN @LT_DELIV
           WHERE A~VBELN EQ @LT_DELIV-VBELN.
    IF SY-SUBRC = 0.
      SORT LT_TKNUM BY VBELN.
      DELETE ADJACENT DUPLICATES FROM LT_TKNUM COMPARING VBELN.

      IF LT_TKNUM[] IS NOT INITIAL.
        GT_TKNUM[] = LT_TKNUM[].
      ENDIF.
    ENDIF.

* UPS & FEDEX Tracking ?????? ??????
    _CLEAR: GT_0072.
    SELECT VBELN INHALT ZNUM ZCODE ZDESC ZTIME
           INTO CORRESPONDING FIELDS OF TABLE GT_0072
           FROM  ZSD1T0072
           FOR ALL ENTRIES IN LT_DELIV
           WHERE VBELN EQ LT_DELIV-VBELN.

* Loading ??????
    _CLEAR: GT_VBSS.
    SELECT DISTINCT A~VBELN,
                    C~REFNR,
                    C~QDATU
                    INTO TABLE @DATA(LT_VBSS)
                    FROM VBSS AS A INNER JOIN VBSK AS B
                    ON    A~SAMMG = B~SAMMG
                    INNER JOIN LTAK AS C
                    ON    B~LGNUM = C~LGNUM
                    AND   B~SAMMG = C~REFNR
                    INNER JOIN LTAP AS D
                    ON    C~LGNUM = D~LGNUM
                    AND   C~TANUM = D~TANUM
                    FOR ALL ENTRIES IN @LT_DELIV
                    WHERE A~VBELN EQ @LT_DELIV-VBELN
                    AND   B~SMART EQ 'W'
                    AND   C~BWLVS EQ '850'
                    AND   D~VORGA NOT IN ('SL','ST').
    IF SY-SUBRC = 0.
      SORT LT_VBSS BY VBELN ASCENDING QDATU DESCENDING.
      DELETE ADJACENT DUPLICATES FROM LT_VBSS COMPARING VBELN.

      IF LT_VBSS[] IS NOT INITIAL.
        GT_VBSS[] = LT_VBSS[].
      ENDIF.
    ENDIF.

  ENDIF.

  IF LT_OBJCT[] IS NOT INITIAL.
    CLEAR: LT_OBJCT.
    SORT LT_OBJCT BY VBELN.
    DELETE ADJACENT DUPLICATES FROM LT_OBJCT.
* Tracking ??????
    _CLEAR: GT_VEKP.
    SELECT VPOBJKEY, EXIDV2, INHALT
           INTO TABLE @DATA(LT_VEKP_TMP)
           FROM VEKP
           FOR ALL ENTRIES IN @LT_OBJCT
           WHERE VPOBJKEY EQ @LT_OBJCT-VPOBJKEY
           AND   STATUS   NE '0060'.

    LOOP AT LT_VEKP_TMP INTO DATA(LS_VEKP).
      CLEAR : GS_VEKP.
      MOVE-CORRESPONDING LS_VEKP TO GS_VEKP.
      GS_VEKP-VBELN = LS_VEKP-VPOBJKEY.
      INSERT GS_VEKP INTO TABLE GT_VEKP.

      CLEAR : LS_VEKP.
    ENDLOOP.
  ENDIF.

  IF LT_LIPS[] IS NOT INITIAL.
    CLEAR: LT_LIPS.
    SORT LT_LIPS BY VBELN POSNR.
    DELETE ADJACENT DUPLICATES FROM LT_LIPS.
* Billing ??????
    SELECT A~VGBEL, A~VGPOS, A~AUBEL, A~AUPOS,
           A~VBELN, A~POSNR, A~NETWR,
           A~MATNR, A~ARKTX, A~FKIMG, A~VRKME, A~MWSBP, A~KZWI1,
           B~SFAKN, B~FKSTO, B~FKDAT, B~ZZKOINV,
           B~KUNRG, B~WAERK,
           C~NAME_ORG1
           APPENDING CORRESPONDING FIELDS OF TABLE @GT_BILLI
           FROM VBRP AS A INNER JOIN VBRK   AS B ON A~VBELN = B~VBELN
                     LEFT OUTER JOIN BUT000 AS C ON B~KUNRG = C~PARTNER
           FOR ALL ENTRIES IN @LT_LIPS
           WHERE A~VGBEL EQ @LT_LIPS-VBELN
           AND   A~VGPOS EQ @LT_LIPS-POSNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_STATUS_VBELN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_STATUS_VBELN .
*  DATA: LT_DD07V TYPE TABLE OF DD07V WITH HEADER LINE.

  DATA : BEGIN OF LS_DD07V,
           DOMVALUE_L LIKE DD07V-DOMVALUE_L,
           DDTEXT     LIKE DD07V-DDTEXT,
         END OF LS_DD07V.
  DATA : LT_DD07V LIKE HASHED TABLE OF LS_DD07V WITH UNIQUE KEY DOMVALUE_L.

  DATA: LS_VBUP  LIKE VBUP,
        LV_TABIX LIKE SY-TABIX,
        LV_ERDAT LIKE VTTK-ERDAT,
        LV_TIME  LIKE ZSD1T0072-ZTIME.

  DATA: LT_DISP LIKE GT_DISP OCCURS 0 WITH HEADER LINE,
        LT_COPY LIKE GT_DISP OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF LT_VBELB_C OCCURS 0,
           VBELB LIKE VBRK-VBELN,
         END OF LT_VBELB_C.

  DATA : BEGIN OF LS_VBRK,
           VBELN    LIKE VBRK-VBELN,
           ZZDOCNUM LIKE VBRK-ZZDOCNUM,
         END OF LS_VBRK.
  DATA : LT_VBRK LIKE HASHED TABLE OF LS_VBRK WITH UNIQUE KEY VBELN.

  DATA : LV_EAN11 LIKE MARA-EAN11.

*-- Final Sales Order Acknowledge
* Sales Order Acknowledge - 855
  DATA: BEGIN OF LT_0023 OCCURS 0,
          VBELN  LIKE VBAK-VBELN,
          DOCNUM TYPE EDI_DOCNUM,
        END OF LT_0023.
* Delivery Order
  DATA: BEGIN OF LT_DELIV OCCURS 0,
          VGBEL LIKE LIPS-VGBEL,
          VBELN LIKE LIPS-VBELN,
          OBJKY LIKE NAST-OBJKY,
        END OF LT_DELIV.

  DATA: BEGIN OF LT_NAST OCCURS 0,
          VBELN  LIKE LIPS-VBELN,
          CMFPNR LIKE NAST-CMFPNR,
        END OF LT_NAST.
  DATA : LT_NAST_TMP LIKE TABLE OF LT_NAST WITH HEADER LINE.
  DATA : LS_NAST_H LIKE LT_NAST,
         LT_NAST_H LIKE HASHED TABLE OF LS_NAST_H WITH UNIQUE KEY VBELN.

  DATA: BEGIN OF LT_CMFP OCCURS 0,
          CMFPNR LIKE NAST-CMFPNR,
          MSGV1  LIKE CMFP-MSGV1,
        END OF LT_CMFP.
  DATA : LS_CMFP_H LIKE LT_CMFP,
         LT_CMFP_H LIKE HASHED TABLE OF LS_CMFP_H WITH UNIQUE KEY CMFPNR.

  DATA: LT_ASN  LIKE LT_0023 OCCURS 0 WITH HEADER LINE,
        LT_0024 LIKE LT_0023 OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF LT_IDOC OCCURS 0,
          DOCNUM TYPE EDI_DOCNUM,
        END OF LT_IDOC.

  DATA: BEGIN OF LS_STA,
          DOCNUM TYPE EDI_DOCNUM,
          ACKID  LIKE ZIF1T0010-ACKID,
          CUSNM  LIKE ZIF1T0050-CUSNM,
        END OF LS_STA.
  DATA : LT_STA LIKE HASHED TABLE OF LS_STA WITH UNIQUE KEY DOCNUM.

  DATA: BEGIN OF LT_FIN OCCURS 0,
          VBELN LIKE VBAK-VBELN,
          855   TYPE EDI_DOCNUM,
          855_S LIKE ICON-ID,
          ASN   TYPE EDI_DOCNUM,
          ASN_S LIKE ICON-ID,
          810   TYPE EDI_DOCNUM,
          810_S LIKE ICON-ID,
        END OF LT_FIN.

  DATA: LT_RES LIKE LT_FIN OCCURS 0 WITH HEADER LINE.

  _CLEAR: GT_DISP.
  CLEAR: GT_SALES, GT_DELIV, GT_0072,
         GT_BILLI,
         LV_ERDAT. " GT_COND
* gt_pick,
  SORT: GT_SALES  BY VBELN POSNR,
        GT_DELIV  BY VGBEL VGPOS,
        GT_0072   BY VBELN INHALT ZNUM,

        GT_BILLI  BY VBELN POSNR.

  DELETE ADJACENT DUPLICATES FROM GT_BILLI COMPARING VBELN POSNR.
  SORT: GT_BILLI  BY VGBEL VGPOS.

* gt_pick  BY vbelv posnv lgnum,

  _CLEAR: LT_DD07V.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_DD07V
    FROM DD07V
   WHERE DOMNAME EQ 'ZDOSTST'
     AND DDLANGUAGE EQ SY-LANGU.

  SORT GT_SALES BY VBELN POSNR.
  DELETE ADJACENT DUPLICATES FROM GT_SALES COMPARING VBELN POSNR.

*????????????
  LOOP AT GT_REMAIN.
    READ TABLE GT_SALES WITH KEY VBELN = GT_REMAIN-VGBEL
                                 POSNR = GT_REMAIN-VGPOS BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_REMAIN-LFIMG = GT_SALES-KWMENG - GT_REMAIN-LFIMG.
      MODIFY GT_REMAIN TRANSPORTING LFIMG.
    ENDIF.
  ENDLOOP.

  LOOP AT GT_SALES.
    CLEAR: GT_DISP.
    MOVE-CORRESPONDING GT_SALES TO GT_DISP.
*-- Item Type
*VBAP-POSNR Item Category(PSTYV) : "TAN/ZSTS"   --> "Normal"
*VBAP-POSNR Item Category(PSTYV) : "ZSIT/ZSIS"  --> "Normal-SiT"
*VBAP-POSNR Item Category(PSTYV) : "TAP"        --> "SET"
*VBAP-POSNR Item Category(PSTYV) : "TAX"        --> "Substitute"
*VBAP-POSNR Item Category(PSTYV) : "ZKLN"       --> "Replace"
    CASE GT_DISP-PSTYV.
      WHEN 'TAN'.  GT_DISP-VTEXT = TEXT-V01. " Normal
      WHEN 'ZSTS'. GT_DISP-VTEXT = TEXT-V01. " Normal
      WHEN 'ZSIT'. GT_DISP-VTEXT = TEXT-V02. " Normal-SiT
      WHEN 'ZSIS'. GT_DISP-VTEXT = TEXT-V02. " Normal-SiT
      WHEN 'TAP'.  GT_DISP-VTEXT = TEXT-V03. " SET
      WHEN 'TAX'.  GT_DISP-VTEXT = TEXT-V04. " Substitute
      WHEN 'ZKLN'. GT_DISP-VTEXT = TEXT-V05. " Replace
    ENDCASE.

    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                              KPOSN = GT_SALES-POSNR
                                              KSCHL = 'PR00'.
    IF SY-SUBRC = 0.
      GT_DISP-PR00 = GS_COND-KBETR.
    ENDIF.

    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                              KPOSN = GT_SALES-POSNR
                                              KSCHL = 'PR01'.
    IF SY-SUBRC = 0.
      GT_DISP-PR01 = GS_COND-KBETR.
    ENDIF.


    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                          KPOSN     = GT_SALES-POSNR
                                          KSCHL     = 'EDI1'.
    IF SY-SUBRC = 0.
      GT_DISP-EDI1 = GS_COND-KBETR.
    ENDIF.

    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                          KPOSN     = GT_SALES-POSNR
                                          KSCHL     = 'MWSI'.
    IF SY-SUBRC = 0.
      GT_DISP-MWSI = GS_COND-KBETR.
    ENDIF.

    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                          KPOSN     = GT_SALES-POSNR
                                          KSCHL     = 'ZDRV'.
    IF SY-SUBRC = 0.
      GT_DISP-ZDRV = GS_COND-KBETR.
    ENDIF.

    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                          KPOSN     = GT_SALES-POSNR
                                          KSCHL     = 'ZDRX'.
    IF SY-SUBRC = 0.
      GT_DISP-ZDRX = GS_COND-KBETR.
    ENDIF.

    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                          KPOSN     = GT_SALES-POSNR
                                          KSCHL     = 'RC00'.
    IF SY-SUBRC = 0.
      GT_DISP-RC00 = GS_COND-KBETR.
    ENDIF.

    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                          KPOSN     = GT_SALES-POSNR
                                          KSCHL     = 'EDID'.
    IF SY-SUBRC = 0.
      GT_DISP-EDID = GS_COND-KBETR.
    ENDIF.

    CLEAR : GS_COND.
    READ TABLE GT_COND  INTO GS_COND WITH KEY KNUMV = GT_SALES-KNUMV
                                              KPOSN = GT_SALES-POSNR
                                              KSCHL = 'CTAX'.
    IF SY-SUBRC = 0.
      GT_DISP-CTAX = GS_COND-KBETR.
    ENDIF.

    GT_DISP-DIFF = GT_DISP-PR00 - GT_DISP-EDI1.

    CLEAR : GS_VBEP.
    READ TABLE GT_VBEP INTO GS_VBEP WITH KEY VBELN = GT_DISP-VBELN
                                             POSNR = GT_DISP-POSNR.
    IF SY-SUBRC = 0.
      GT_DISP-EDATU = GS_VBEP-EDATU.
      GT_DISP-LDDAT = GS_VBEP-LDDAT.
    ENDIF.

*-- LDDAT Loding Planned
    IF GT_DISP-LDDAT NOT IN S_LDDAT.
      CONTINUE.
    ENDIF.

*--?????? ??????
    CLEAR GT_REMAIN.
    READ TABLE GT_REMAIN WITH KEY VGBEL = GT_DISP-VBELN
                                  VGPOS = GT_DISP-POSNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DISP-REMAIN_QTY = GT_REMAIN-LFIMG.
    ENDIF.

*-- Ship TO Name
    CLEAR : GS_SH.
    READ TABLE GT_SH INTO GS_SH WITH KEY VBELN = GT_DISP-VBELN.
    IF SY-SUBRC = 0.
      GT_DISP-KUNWE      = GS_SH-KUNWE.
      GT_DISP-SAME1      = GS_SH-SAME1.
      GT_DISP-STREET     = GS_SH-STREET.
      GT_DISP-POST_CODE1 = GS_SH-POST_CODE1.
      GT_DISP-CITY1      = GS_SH-CITY1.
      GT_DISP-REGION     = GS_SH-REGION.
      GT_DISP-TEL_NUMBER = GS_SH-TEL_NUMBER.
    ENDIF.
*-- Carrier Name
    CLEAR : GS_SP.
    READ TABLE GT_SP INTO GS_SP WITH KEY VBELN = GT_DISP-VBELN.
    IF SY-SUBRC = 0.
      GT_DISP-LIFNR = GS_SP-LIFNR.
      GT_DISP-LAME1 = GS_SP-LAME1.
    ENDIF.

    IF GT_DISP-ABGRU IS INITIAL.
*-- Sales order Incomplete
      READ TABLE GT_VBUV WITH KEY VBELN = GT_DISP-VBELN
                                  POSNR = GT_DISP-POSNR
                                  TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        GT_DISP-ICON_I = GC_I_LIST.
      ENDIF.
    ELSE.
      CLEAR : GS_DEL.
      READ TABLE GT_DEL INTO GS_DEL WITH KEY VBELN = GT_DISP-VBELN.
      IF SY-SUBRC = 0.
      ELSE.
        READ TABLE GT_VBUV_H WITH KEY VBELN = GT_DISP-VBELN
                                      TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          GT_DISP-ICON_I = GC_I_LIST.
        ENDIF.
      ENDIF.
    ENDIF.

*-- Sales Order EDI INFO
    CLEAR : GS_0021.
    READ TABLE GT_0021 INTO GS_0021 WITH KEY VBELN = GT_DISP-VBELN.
    IF SY-SUBRC = 0.
      GT_DISP-DOCNUM  = GS_0021-DOCNUM.  " EDI Number
      GT_DISP-ZIFTYPE = GS_0021-ZIFTYPE. " EDI Type
    ENDIF.

*-- Sales Order Acknowledge
    READ TABLE GT_0023 WITH KEY VBELN = GT_DISP-VBELN
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      GT_DISP-ICON_S = GC_I_LIST.
    ELSE.

*-- Change Sales Order Acknowledge - 865
      READ TABLE GT_0025 WITH KEY VBELN = GT_DISP-VBELN
                                  POSNR = GT_DISP-POSNR
                                  TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        GT_DISP-ICON_S = GC_I_LIST.
      ENDIF.
    ENDIF.
*-- Delivery order
    READ TABLE GT_DELIV WITH KEY VGBEL = GT_DISP-VBELN
                                 VGPOS = GT_DISP-POSNR
                                 BINARY SEARCH.
    IF SY-SUBRC = 0.
      LOOP AT GT_DELIV FROM SY-TABIX.
        IF GT_DELIV-VGBEL = GT_DISP-VBELN AND
           GT_DELIV-VGPOS = GT_DISP-POSNR.

          GT_DISP-VBELV     = GT_DELIV-VBELN.
          GT_DISP-POSNV     = GT_DELIV-POSNR.
          IF GT_DISP-PSTYV NE 'TAP'.
            GT_DISP-LFIMG     = GT_DELIV-LFIMG.  "--> ?????? ?????? 20190205
          ELSE.
            CLEAR GT_DISP-LFIMG.
          ENDIF.
          GT_DISP-WADAT_IST = GT_DELIV-WADAT_IST.
          IF GT_DELIV-PDSTK = 'C'.
            GT_DISP-PODAT     = GT_DELIV-PODAT.
          ENDIF.
* Shipment & Loaded.
          CLEAR : GS_TKNUM.
          READ TABLE GT_TKNUM INTO GS_TKNUM WITH KEY VBELN = GT_DISP-VBELV.
          IF SY-SUBRC = 0.
            GT_DISP-TKNUM = GS_TKNUM-TKNUM.
            GT_DISP-DALEN = GS_TKNUM-DALEN.
            LV_ERDAT      = GS_TKNUM-ERDAT.
            GT_DISP-EXTI2 = GS_TKNUM-EXTI2.
          ENDIF.
* Tracking
* Shipping Conditions??? 'LT'?????? Carrier??? ONTRAC??? ?????? ??????
* LIKP-LIFEX??? Tracking?????? ?????????.
          IF GT_DISP-VSBED = 'LT'.
            IF GT_DISP-LIFNR = '0000080038'.
              READ TABLE GT_VEKP INTO GS_VEKP WITH KEY VBELN = GT_DISP-VBELV.
              IF SY-SUBRC = 0.
                GT_DISP-ICON_T    = GC_I_LIST.
                GT_DISP-MASTER_TR = GS_VEKP-EXIDV2.
              ENDIF.
            ELSE.
              IF GT_DELIV-LIFEX IS NOT INITIAL.
                GT_DISP-ICON_T = GC_I_LIST.
              ENDIF.
            ENDIF.
          ELSE.
            READ TABLE GT_VEKP INTO GS_VEKP WITH KEY VBELN = GT_DISP-VBELV.
            IF SY-SUBRC = 0.
              GT_DISP-ICON_T = GC_I_LIST.
              GT_DISP-MASTER_TR = GS_VEKP-EXIDV2.
            ENDIF.
          ENDIF.

          IF GT_DISP-VTEXT = TEXT-V03.
          ELSEIF GT_DISP-VTEXT = TEXT-V04.
          ELSE.
            CLEAR : GS_VBSS.
            READ TABLE GT_VBSS INTO GS_VBSS WITH KEY VBELN = GT_DISP-VBELV.
            IF SY-SUBRC = 0 AND
               GS_VBSS-REFNR IS NOT INITIAL.
              GT_DISP-RFMNG = GT_DELIV-LFIMG.
              GT_DISP-QDATU = GS_VBSS-QDATU.
            ENDIF.
          ENDIF.
* Transp & Carrier First Scanned
          READ TABLE GT_0072 WITH KEY VBELN = GT_DISP-VBELV
                                  BINARY SEARCH.
          IF SY-SUBRC = 0.
* Transp
            GT_DISP-ICON_P = GC_I_LIST.
* Carrier First Scanned
            LOOP AT GT_0072 FROM SY-TABIX.
              IF GT_0072-VBELN = GT_DISP-VBELV.
                IF GT_0072-ZCODE = 'AR'.
                  CLEAR: LV_TIME.
                  LV_TIME = GT_0072-ZTIME.
                  CONDENSE LV_TIME NO-GAPS.
                  CONCATENATE LV_TIME+0(4) LV_TIME+4(2) LV_TIME+6(2) INTO GT_DISP-N_PTIME.
                  EXIT.
                ENDIF.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.

* Billing
          READ TABLE GT_BILLI WITH KEY VGBEL = GT_DISP-VBELV
                                       VGPOS = GT_DISP-POSNV
                                       BINARY SEARCH
                                       TRANSPORTING NO FIELDS.
          IF SY-SUBRC = 0.
            GT_DISP-VBELF = GC_I_LIST.
          ENDIF.

* IV Send
          READ TABLE GT_0024 WITH KEY VBELN = GT_DISP-VBELN
                                      TRANSPORTING NO FIELDS.
          IF SY-SUBRC = 0.
            GT_DISP-ICON_D = GC_I_LIST.
          ENDIF.

          GT_DISP-N_BLDAT = GT_DELIV-BLDAT.
          GT_DISP-N_DTDIS = LV_ERDAT.
          GT_DISP-N_QDATU = GT_DISP-QDATU.
          GT_DISP-N_DALEN = GT_DISP-DALEN.
          GT_DISP-N_WADAT = GT_DELIV-WADAT_IST.
          GT_DISP-N_PODAT = GT_DELIV-PODAT.

          READ TABLE GT_BILLI WITH KEY VGBEL = GT_DISP-VBELV
                                       VGPOS = GT_DISP-POSNV
                                       BINARY SEARCH.
          IF SY-SUBRC = 0.
            LOOP AT GT_BILLI FROM SY-TABIX.
              IF GT_BILLI-VGBEL = GT_DISP-VBELV AND
                 GT_BILLI-VGPOS = GT_DISP-POSNV.
                IF GT_BILLI-SFAKN = ABAP_OFF AND
                   GT_BILLI-FKSTO = ABAP_OFF.
                  GT_DISP-N_FKDAT = GT_BILLI-FKDAT.
                  GT_DISP-VBELB = GT_BILLI-VBELN.
                  GT_DISP-POSNB = GT_BILLI-POSNR.
                  GT_DISP-ZZKOINV = GT_BILLI-ZZKOINV.
                  GT_DISP-MWSBP = GT_BILLI-MWSBP.
                ENDIF.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
            CLEAR : GT_DISP-N_FKDAT, GT_DISP-VBELB, GT_DISP-POSNB, GT_DISP-ZZKOINV.
          ENDIF.

          READ TABLE GT_BILLI WITH KEY VGBEL = GT_DISP-VBELV
                                       VGPOS = GT_DISP-POSNV
                            BINARY SEARCH.
          IF SY-SUBRC = 0.
            LOOP AT GT_BILLI FROM SY-TABIX.
              IF GT_BILLI-VGBEL = GT_DISP-VBELV AND
                 GT_BILLI-VGPOS = GT_DISP-POSNV.
                IF GT_BILLI-SFAKN = ABAP_OFF AND
                   GT_BILLI-FKSTO = ABAP_OFF.
                  IF GT_DISP-PSTYV = 'TAP' OR
                     GT_DISP-PSTYV = 'TAX'.
                    IF GT_BILLI-VGPOS+4(1) = GT_DISP-POSNV+4(1).
                      GT_DISP-BETWR = GT_DISP-BETWR + GT_BILLI-NETWR.
                    ENDIF.
                  ELSE.
*                    IF GT_DISP-AUART = 'ZDIM'.
*                      GT_DISP-BETWR = GT_BILLI-NETWR + GT_DISP-BETWR.
*                    ELSEIF GT_DISP-AUART = 'ZDIR'.
*                      GT_DISP-BETWR = GT_BILLI-NETWR + GT_DISP-BETWR.
*                    ELSE.
                    GT_DISP-BETWR = GT_BILLI-NETWR.
*                    ENDIF.
                  ENDIF.

                ENDIF.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.

          CASE GT_DISP-AUART.
*--//RETURN
            WHEN 'ZRE'.
              PERFORM RETURN_NUMBER USING GT_DISP-KWMENG.
              PERFORM RETURN_NUMBER USING GT_DISP-REMAIN_QTY.
              PERFORM RETURN_NUMBER USING GT_DISP-LFIMG.
              PERFORM RETURN_NUMBER USING GT_DISP-PR00.
              PERFORM RETURN_NUMBER USING GT_DISP-PR01.
              PERFORM RETURN_NUMBER USING GT_DISP-EDI1.
              PERFORM RETURN_NUMBER USING GT_DISP-MWSI.
              PERFORM RETURN_NUMBER USING GT_DISP-ZDRV.
              PERFORM RETURN_NUMBER USING GT_DISP-ZDRX.
              PERFORM RETURN_NUMBER USING GT_DISP-NETWR.
              PERFORM RETURN_NUMBER USING GT_DISP-BETWR.
              PERFORM RETURN_NUMBER USING GT_DISP-MWSBP.
          ENDCASE.


          GT_DISP-LIFEX = GT_DELIV-LIFEX.
          APPEND GT_DISP.

          CLEAR : LT_VBELB_C.
          LT_VBELB_C-VBELB = GT_DISP-VBELB.
          COLLECT LT_VBELB_C.
        ELSE.
          EXIT.
        ENDIF.

      ENDLOOP.

    ELSE.
      READ TABLE GT_BILLI WITH KEY VGBEL = GT_DISP-VBELN
                                   VGPOS = GT_DISP-POSNR
                                   TRANSPORTING NO FIELDS
                                   BINARY SEARCH.
      IF SY-SUBRC = 0.
        GT_DISP-VBELF = GC_I_LIST.
      ENDIF.

* IV Send
      READ TABLE GT_0024_CK WITH KEY VBELN = GT_DISP-VBELN
                                 TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        GT_DISP-ICON_D = GC_I_LIST.
      ENDIF.

      READ TABLE GT_BILLI WITH KEY VGBEL = GT_DISP-VBELN
                                   VGPOS = GT_DISP-POSNR
                                   BINARY SEARCH.
      IF SY-SUBRC = 0.
        LOOP AT GT_BILLI FROM SY-TABIX.
          IF GT_BILLI-VGBEL = GT_DISP-VBELN AND
             GT_BILLI-VGPOS = GT_DISP-POSNR.
            IF GT_BILLI-SFAKN = ABAP_OFF AND
               GT_BILLI-FKSTO = ABAP_OFF.
              GT_DISP-N_FKDAT = GT_BILLI-FKDAT.
              GT_DISP-VBELB = GT_BILLI-VBELN.
              GT_DISP-POSNB = GT_BILLI-POSNR.
              GT_DISP-ZZKOINV = GT_BILLI-ZZKOINV.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
        CLEAR : GT_DISP-N_FKDAT, GT_DISP-VBELB, GT_DISP-POSNB, GT_DISP-ZZKOINV.
      ENDIF.

      READ TABLE GT_BILLI WITH KEY VGBEL = GT_DISP-VBELN
                                   VGPOS = GT_DISP-POSNR
                        BINARY SEARCH.
      IF SY-SUBRC = 0.
        LOOP AT GT_BILLI FROM SY-TABIX.
          IF GT_BILLI-VGBEL = GT_DISP-VBELN AND
             GT_BILLI-VGPOS = GT_DISP-POSNR.
            IF GT_BILLI-SFAKN = ABAP_OFF AND
               GT_BILLI-FKSTO = ABAP_OFF.
              IF GT_DISP-PSTYV = 'TAP' OR
                 GT_DISP-PSTYV = 'TAX'.
                IF GT_BILLI-VGPOS+4(1) = GT_DISP-POSNR+4(1).
                  GT_DISP-BETWR = GT_DISP-BETWR + GT_BILLI-NETWR.
                ENDIF.


              ELSE.
*                IF GT_DISP-AUART = 'ZDIM'.
*                  GT_DISP-BETWR = GT_BILLI-NETWR + GT_DISP-BETWR.
*                ELSEIF GT_DISP-AUART = 'ZDIR'.
*                  GT_DISP-BETWR = GT_BILLI-NETWR + GT_DISP-BETWR.
*                ELSE.
                GT_DISP-BETWR = GT_BILLI-NETWR.
*                ENDIF.
              ENDIF.

            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CASE GT_DISP-AUART.
*DEBIT, CREDIT MEMO
        WHEN 'ZDR' OR 'ZCR'.
          CLEAR : GT_DISP-KWMENG,  GT_DISP-REMAIN_QTY, GT_DISP-LFIMG.
          PERFORM RETURN_NUMBER USING GT_DISP-BETWR.
          PERFORM RETURN_NUMBER USING GT_DISP-MWSBP.
*RETURN
        WHEN 'ZRE'.
          PERFORM RETURN_NUMBER USING GT_DISP-KWMENG.
          PERFORM RETURN_NUMBER USING GT_DISP-REMAIN_QTY.
          PERFORM RETURN_NUMBER USING GT_DISP-LFIMG.
          PERFORM RETURN_NUMBER USING GT_DISP-PR00.
          PERFORM RETURN_NUMBER USING GT_DISP-PR01.
          PERFORM RETURN_NUMBER USING GT_DISP-EDI1.
          PERFORM RETURN_NUMBER USING GT_DISP-MWSI.
          PERFORM RETURN_NUMBER USING GT_DISP-ZDRV.
          PERFORM RETURN_NUMBER USING GT_DISP-ZDRX.
          PERFORM RETURN_NUMBER USING GT_DISP-NETWR.
          PERFORM RETURN_NUMBER USING GT_DISP-BETWR.
          PERFORM RETURN_NUMBER USING GT_DISP-MWSBP.
      ENDCASE.

      APPEND GT_DISP.

      CLEAR : LT_VBELB_C.
      LT_VBELB_C-VBELB = GT_DISP-VBELB.
      COLLECT LT_VBELB_C.

    ENDIF.

    CLEAR : GT_SALES.

  ENDLOOP.

  CLEAR : LT_VBRK, LT_VBRK.
  IF LT_VBELB_C[] IS NOT INITIAL.
    SELECT VBELN ZZDOCNUM
           INTO CORRESPONDING FIELDS OF TABLE LT_VBRK
           FROM VBRK
           FOR ALL ENTRIES IN LT_VBELB_C
           WHERE VBELN EQ LT_VBELB_C-VBELB.
  ENDIF.



  _CLEAR: LT_0023, LT_DELIV, LT_NAST, LT_CMFP,
          LT_ASN, LT_0024, LT_IDOC, LT_STA,
          LT_FIN, LT_RES.

  LOOP AT GT_0023 INTO GS_0023.
    MOVE-CORRESPONDING GS_0023 TO LT_0023.
    _APPEND LT_0023.
  ENDLOOP.
  CLEAR: LT_0023. SORT LT_0023 BY VBELN DOCNUM DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_0023 COMPARING VBELN.
  CLEAR: LT_0023. SORT LT_0023 BY VBELN.
*-- Final Delivery
  _CLEAR: LT_DELIV.
  LOOP AT GT_DELIV.
    MOVE-CORRESPONDING GT_DELIV TO LT_DELIV.
    LT_DELIV-OBJKY = LT_DELIV-VBELN.
    _APPEND LT_DELIV.
  ENDLOOP.

  CLEAR : LS_NAST_H, LT_NAST_H,
          LT_CMFP,   LT_CMFP[].
  IF LT_DELIV[] IS NOT INITIAL.
    CLEAR: LT_DELIV. SORT LT_DELIV BY VBELN.
    DELETE ADJACENT DUPLICATES FROM LT_DELIV.
    CLEAR: LT_DELIV. SORT LT_DELIV BY VBELN.

    SELECT OBJKY AS VBELN
           CMFPNR
           INTO CORRESPONDING FIELDS OF TABLE LT_NAST
           FROM NAST FOR ALL ENTRIES IN LT_DELIV
           WHERE KAPPL  EQ 'V2'
           AND OBJKY  EQ LT_DELIV-OBJKY
           AND KSCHL  EQ 'ZASN'
           AND NACHA  EQ '6'.

    IF LT_NAST[] IS NOT INITIAL.
      CLEAR: LT_NAST.
      SORT LT_NAST BY VBELN CMFPNR.

      CLEAR : LT_NAST_TMP, LT_NAST_TMP.
      LT_NAST_TMP[] = LT_NAST[].
      SORT LT_NAST_TMP BY VBELN.
      DELETE ADJACENT DUPLICATES FROM LT_NAST_TMP COMPARING VBELN.

      IF LT_NAST_TMP[] IS NOT INITIAL.
        LT_NAST_H[] = LT_NAST_TMP[].
      ENDIF.

      SELECT NR AS CMFPNR
             MSGV1
        INTO CORRESPONDING FIELDS OF TABLE LT_CMFP
        FROM CMFP FOR ALL ENTRIES IN LT_NAST
       WHERE APLID  EQ 'WFMC'
         AND NR     EQ LT_NAST-CMFPNR
         AND MSGTY  EQ 'I'
         AND MSGNR  EQ '097'.

      CLEAR: LT_CMFP.
      SORT LT_CMFP BY CMFPNR MSGV1 DESCENDING.
      DELETE ADJACENT DUPLICATES FROM LT_CMFP COMPARING CMFPNR.

      CLEAR : LT_CMFP_H, LS_CMFP_H.
      IF LT_CMFP[] IS NOT INITIAL.
        LT_CMFP_H[] = LT_CMFP[].
      ENDIF.

    ENDIF.

    LOOP AT LT_DELIV.

      CLEAR : LS_NAST_H.
      READ TABLE LT_NAST_H INTO LS_NAST_H WITH KEY VBELN = LT_DELIV-VBELN.
      IF SY-SUBRC = 0.
        CLEAR : LS_CMFP_H.
        READ TABLE LT_CMFP_H INTO LS_CMFP_H WITH KEY CMFPNR = LS_NAST_H-CMFPNR.
        IF SY-SUBRC = 0.
          LT_ASN-VBELN  = LT_DELIV-VGBEL.
          CONDENSE LS_CMFP_H-MSGV1 NO-GAPS.
          LT_ASN-DOCNUM = LS_CMFP_H-MSGV1.
          _APPEND LT_ASN.
        ENDIF.
      ENDIF.

      CLEAR : LT_DELIV.
    ENDLOOP.
  ENDIF.
  CLEAR: LT_ASN. SORT LT_ASN BY VBELN.

*-- IDOC ??????
  CLEAR: LT_0023. SORT LT_0023 BY VBELN.
  CLEAR: LT_0024. SORT LT_0024 BY VBELN.
  CLEAR: LT_ASN.  SORT LT_ASN BY VBELN.

  LOOP AT LT_0023.
    MOVE-CORRESPONDING LT_0023 TO LT_IDOC.
    _APPEND LT_IDOC.
  ENDLOOP.

  LOOP AT GT_0024 INTO GS_0024.
    MOVE-CORRESPONDING GS_0024 TO LT_IDOC.
    _APPEND LT_IDOC.

    CLEAR : GS_0024.
  ENDLOOP.

  LOOP AT LT_ASN.
    MOVE-CORRESPONDING LT_ASN TO LT_IDOC.
    _APPEND LT_IDOC.
  ENDLOOP.

  CLEAR : LT_STA.
  IF LT_IDOC[] IS NOT INITIAL.
    CLEAR: LT_IDOC. SORT LT_IDOC BY DOCNUM.
    DELETE ADJACENT DUPLICATES FROM LT_IDOC.

    SELECT A~DOCNUM,
           A~ACKID,
           B~CUSNM
           INTO TABLE @DATA(LT_STA_TMP)
           FROM ZIF1T0010 AS A LEFT OUTER JOIN ZIF1T0050 AS B
           ON  A~CUSNM   = B~CUSNM
           AND A~IFTYP   = B~IFTYP
           AND A~ERDAT   = B~ERDAT
           AND A~SEQ     = B~SEQ
           FOR ALL ENTRIES IN @LT_IDOC
           WHERE DOCNUM EQ @LT_IDOC-DOCNUM.
    IF SY-SUBRC = 0.
      SORT LT_STA_TMP BY DOCNUM.
      DELETE ADJACENT DUPLICATES FROM LT_STA_TMP COMPARING DOCNUM.

      IF LT_STA_TMP[] IS NOT INITIAL.
        LT_STA[] = LT_STA_TMP[].
      ENDIF.
    ENDIF.

  ENDIF.

  CLEAR: LT_0023. SORT LT_0023 BY VBELN.
  CLEAR: LT_ASN.  SORT LT_ASN BY VBELN.

  LOOP AT LT_0023.
    CLEAR : LS_STA.
    READ TABLE LT_STA INTO LS_STA WITH KEY DOCNUM = LT_0023-DOCNUM.
    IF SY-SUBRC = 0.
      LT_FIN-VBELN = LT_0023-VBELN.
      LT_FIN-855   = LS_STA-DOCNUM.
      LT_FIN-855_S = GC_I_YEL.
      IF LS_STA-ACKID = ABAP_ON.
        LT_FIN-855_S = GC_I_GRE.
      ELSE.
        IF LS_STA-CUSNM IS INITIAL.
          LT_FIN-855_S = GC_I_RED.
        ENDIF.
      ENDIF.
      _APPEND LT_FIN.
    ENDIF.

    CLEAR : LT_0023.
  ENDLOOP.

  LOOP AT LT_ASN.
    CLEAR : LS_STA.
    READ TABLE LT_STA INTO LS_STA WITH KEY DOCNUM = LT_ASN-DOCNUM.
    IF SY-SUBRC = 0.
      LT_FIN-VBELN = LT_ASN-VBELN.
      LT_FIN-ASN   = LS_STA-DOCNUM.
      LT_FIN-ASN_S = GC_I_YEL.
      IF LS_STA-ACKID = ABAP_ON.
        LT_FIN-ASN_S = GC_I_GRE.
      ELSE.
        IF LS_STA-CUSNM IS INITIAL.
          LT_FIN-ASN_S = GC_I_RED.
        ENDIF.
      ENDIF.
      _APPEND LT_FIN.
    ENDIF.

    CLEAR : LT_ASN.
  ENDLOOP.

  CLEAR : GS_0024.
  LOOP AT GT_0024 INTO GS_0024.
    CLEAR : LS_STA.
    READ TABLE LT_STA INTO LS_STA WITH KEY DOCNUM = GS_0024-DOCNUM.
    IF SY-SUBRC = 0.
      LT_FIN-VBELN = GS_0024-VBELN.
      LT_FIN-810   = LS_STA-DOCNUM.
      LT_FIN-810_S = GC_I_YEL.
      IF LS_STA-ACKID = ABAP_ON.
        LT_FIN-810_S = GC_I_GRE.
      ELSE.
        IF LS_STA-CUSNM IS INITIAL.
          LT_FIN-810_S = GC_I_RED.
        ENDIF.
      ENDIF.
      _APPEND LT_FIN.
    ENDIF.

    CLEAR : GS_0024.
  ENDLOOP.

  CLEAR: LT_FIN. SORT LT_FIN BY VBELN.
  LOOP AT LT_FIN.
    AT NEW VBELN.
      LT_RES-VBELN = LT_FIN-VBELN.
    ENDAT.

    IF LT_FIN-855 IS NOT INITIAL.
      LT_RES-855   = LT_FIN-855.
    ENDIF.
    IF LT_FIN-855_S IS NOT INITIAL.
      LT_RES-855_S = LT_FIN-855_S.
    ENDIF.
    IF LT_FIN-ASN IS NOT INITIAL.
      LT_RES-ASN   = LT_FIN-ASN.
    ENDIF.
    IF LT_FIN-ASN_S IS NOT INITIAL.
      LT_RES-ASN_S = LT_FIN-ASN_S.
    ENDIF.
    IF LT_FIN-810 IS NOT INITIAL.
      LT_RES-810   = LT_FIN-810.
    ENDIF.
    IF LT_FIN-810_S IS NOT INITIAL.
      LT_RES-810_S = LT_FIN-810_S.
    ENDIF.

    AT END OF VBELN.
      _APPEND LT_RES.
    ENDAT.
  ENDLOOP.

  CLEAR: LT_RES. SORT LT_RES BY VBELN.

  CLEAR: LV_TABIX.
  LOOP AT GT_DISP.
    LV_TABIX = SY-TABIX.
    READ TABLE LT_RES WITH KEY VBELN = GT_DISP-VBELN
                               BINARY SEARCH.
    IF SY-SUBRC = 0.
      GT_DISP-855   = LT_RES-855.
      GT_DISP-855_S = LT_RES-855_S.
      GT_DISP-ASN   = LT_RES-ASN.
      GT_DISP-ASN_S = LT_RES-ASN_S.
      GT_DISP-810   = LT_RES-810.
      GT_DISP-810_S = LT_RES-810_S.
    ENDIF.

    IF GT_DISP-810 IS INITIAL.
      CLEAR : LS_VBRK.
      READ TABLE LT_VBRK INTO LS_VBRK WITH KEY VBELN = GT_DISP-VBELB.
      IF SY-SUBRC = 0.
        GT_DISP-810 = LS_VBRK-ZZDOCNUM.
      ENDIF.
    ENDIF.

    IF GT_DISP-ABGRU IS NOT INITIAL.
      CLEAR GT_DISP-STATUS.   " Reject
    ELSE.
      IF GT_DISP-ICON_I = GC_I_LIST.
        GT_DISP-STATUS = 1.   " General Incompletion
      ELSE.
        GT_DISP-STATUS = 2.   " Salse order relase

        IF GT_DISP-TKNUM IS NOT INITIAL.
          GT_DISP-STATUS = 3. " Shipment
        ENDIF.
***-- Sales order Status
        IF GT_DISP-QDATU <> '00000000'.
          GT_DISP-STATUS = 4. " Picking complete
        ENDIF.

        IF GT_DISP-DALEN  <> '00000000'.
          GT_DISP-STATUS = 5. " W/H Task
        ENDIF.

        IF GT_DISP-WADAT_IST  <> '00000000'.
          GT_DISP-STATUS = 6. " Goods movement
        ENDIF.

        IF GT_DISP-PODAT  <> '00000000'.
          GT_DISP-STATUS = 7. " POD
        ENDIF.

        IF GT_DISP-N_FKDAT  <> '00000000'.
          GT_DISP-STATUS = 8.  " Billing
        ENDIF.

      ENDIF.

    ENDIF.

*-- Sales order Status DEC
    CLEAR : LS_DD07V.
    READ TABLE LT_DD07V INTO LS_DD07V WITH KEY DOMVALUE_L = GT_DISP-STATUS.
    IF SY-SUBRC = 0.
      GT_DISP-DDTEXT = LS_DD07V-DDTEXT.
    ENDIF.

* Add 06.03.2019 by ZEN20 - S Request Mark
    GT_DISP-SALES_AMT = GT_DISP-LFIMG * GT_DISP-EDI1.

*1. D/O Value  = [DO Qty (LFIMG) ]*[Net Value] / [Qty.]
    TRY.
        GT_DISP-DO_VALUE = GT_DISP-LFIMG * GT_DISP-NETWR / GT_DISP-KWMENG.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.
*2. D/O Volume = [DO Qty]*[Volume]/[QTY.]
    TRY.
        GT_DISP-DO_VOLUME = GT_DISP-LFIMG * GT_DISP-VOLUM / GT_DISP-KWMENG.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

*3. D/O Weight = [DO Qty]*[Gross Weight]/[QTY.]
    TRY.
        GT_DISP-DO_WEIGHT = GT_DISP-LFIMG * GT_DISP-BRGEW / GT_DISP-KWMENG.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    GT_DISP-SALES_TOT = GT_DISP-PR00 * GT_DISP-LFIMG.
    GT_DISP-SALES_DIFF = GT_DISP-SALES_TOT - GT_DISP-SALES_AMT.

* Add 06.03.2019 by ZEN20 - E

    MODIFY GT_DISP INDEX LV_TABIX TRANSPORTING STATUS DDTEXT
                                               SALES_AMT
                                               DO_VALUE
                                               DO_VOLUME
                                               DO_WEIGHT
                                               855 855_S
                                               ASN ASN_S
                                               810 810_S
                                               SALES_TOT
                                               SALES_DIFF.
  ENDLOOP.

  IF S_STAT[] IS NOT INITIAL.
    DELETE GT_DISP WHERE STATUS NOT IN S_STAT.
  ENDIF.

  CLEAR: GT_DISP, LV_TABIX.
  SORT GT_DISP BY KUNNR VBELN POSNR.
  LOOP AT GT_DISP WHERE ABGRU IS INITIAL.
    LV_TABIX = SY-TABIX.
* Order Creation Date - First Delivery Date
    GT_DISP-GD = GT_DISP-EDATU - GT_DISP-ERDAT.
* Carrier Planned??? ???????????? ?????????  First Delivery Date - ????????????
* First Delivery Date - Carrier Planned
    IF GT_DISP-N_DTDIS = '00000000'.
      GT_DISP-GS = SY-DATLO - GT_DISP-EDATU.
    ELSE.
      GT_DISP-GS = GT_DISP-N_DTDIS - GT_DISP-EDATU.
    ENDIF.
* Goods Issued??? ???????????? ?????????  First Delivery Date - ????????????
* First Delivery Date - Goods Issued
    IF GT_DISP-N_WADAT = '00000000'.
      GT_DISP-GA = SY-DATLO - GT_DISP-EDATU.
    ELSE.
      GT_DISP-GA = GT_DISP-N_WADAT - GT_DISP-EDATU.
    ENDIF.
    MODIFY GT_DISP INDEX LV_TABIX TRANSPORTING GD GS GA.
  ENDLOOP.

  IF S_FKDAT[] IS NOT INITIAL.
    DELETE GT_DISP WHERE N_FKDAT NOT IN S_FKDAT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_TRACKING_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_TRACKING_INFO .
  DATA: LT_ROWS  TYPE LVC_T_ROW,
        LS_ROWS  TYPE LVC_S_ROW,
        LV_COT   TYPE SY-INDEX,
        LV_TABIX TYPE SY-TABIX,
        LV_TIME  LIKE ZSD1T0071-ZTIME.

  DATA: LS_DATA LIKE GT_DISP.

  DATA: BEGIN OF LT_BASE OCCURS 0,
          BSTKD	LIKE VBKD-BSTKD,          " Customer PO
          VBELN	LIKE VBAP-VBELN,          " SO
          VBELV	LIKE LIPS-VBELN,          " DO
          LIFNR LIKE VBPA-LIFNR,          " Carrier
          LAME1 LIKE LFA1-NAME1,          " Carrier Name
        END OF LT_BASE.

  CLEAR: LS_ROWS.
  _CLEAR: LT_ROWS, LT_BASE, GT_INFO.

  CALL METHOD GC_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS.

  IF LINES( LT_ROWS ) EQ 0.
    MESSAGE S000(ZMCSD1) WITH TEXT-E01 DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    SORT LT_ROWS BY INDEX.
    LOOP AT LT_ROWS INTO LS_ROWS.
      READ TABLE GT_DISP INTO LS_DATA INDEX LS_ROWS-INDEX.
      IF SY-SUBRC EQ 0 AND LS_DATA-ABGRU IS INITIAL.
        MOVE-CORRESPONDING LS_DATA TO LT_BASE.
        _APPEND LT_BASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF LT_BASE[] IS NOT INITIAL.
    CLEAR: LT_BASE. SORT LT_BASE BY BSTKD VBELN VBELV.
    CLEAR: GT_0072. SORT GT_0072 BY VBELN INHALT ZNUM.

    LOOP AT LT_BASE.
      READ TABLE GT_0072 WITH KEY VBELN = LT_BASE-VBELV
                              BINARY SEARCH.
      IF SY-SUBRC = 0.
        LOOP AT GT_0072 FROM SY-TABIX.
          IF GT_0072-VBELN = LT_BASE-VBELV.
            MOVE-CORRESPONDING LT_BASE TO GT_INFO.
            GT_INFO-INHALT = GT_0072-INHALT.
            _APPEND GT_INFO.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    IF GT_INFO[] IS NOT INITIAL.
      CLEAR: GT_INFO. SORT GT_INFO BY BSTKD VBELN VBELV INHALT.
      DELETE ADJACENT DUPLICATES FROM GT_INFO.

      CLEAR: LV_TABIX.
      LOOP AT GT_INFO.
        LV_TABIX = SY-TABIX.
* First Scanned
        LOOP AT GT_0072 WHERE VBELN  = GT_INFO-VBELV
                        AND   INHALT = GT_INFO-INHALT
                        AND   ZCODE  = 'AR'.

          MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING FTIME.
          EXIT.
        ENDLOOP.
      ENDLOOP.

      CLEAR: GT_0072. SORT GT_0072 BY VBELN INHALT DESCENDING ZNUM.
      CLEAR: LV_TABIX.
      LOOP AT GT_INFO.
        LV_TABIX = SY-TABIX.
* Delivered
        IF GT_INFO-LIFNR = '0000010209'. " FEDEX
          LOOP AT GT_0072 WHERE VBELN  = GT_INFO-VBELV
                          AND   INHALT = GT_INFO-INHALT.
            CASE GT_0072-ZCODE.
              WHEN 'DL'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
              WHEN 'HL'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
            ENDCASE.
          ENDLOOP.
        ELSEIF GT_INFO-LIFNR = '0000010291'. " UPS
          LOOP AT GT_0072 WHERE VBELN  = GT_INFO-VBELV
                          AND   INHALT = GT_INFO-INHALT.
            CASE GT_0072-ZCODE.
              WHEN 'KB'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
              WHEN 'FS'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
              WHEN 'KE'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
              WHEN '9E'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
              WHEN 'KD'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
              WHEN 'KM'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
              WHEN '8Q'.
                GT_INFO-TTIME = GT_0072-ZTIME.
                MODIFY GT_INFO INDEX LV_TABIX TRANSPORTING TTIME. EXIT.
            ENDCASE.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF GT_INFO[] IS NOT INITIAL.
    CALL SCREEN 0700.
  ELSE.
    MESSAGE S000(ZMCSD1) WITH TEXT-E01 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_TRACKING_INFO_NEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_TRACKING_INFO_NEW .

  DATA: LT_ROWS  TYPE LVC_T_ROW,
        LS_ROWS  TYPE LVC_S_ROW,
        LV_COT   TYPE SY-INDEX,
        LV_TABIX TYPE SY-TABIX,
        LV_TIME  LIKE ZSD1T0071-ZTIME.

  DATA: LS_DATA LIKE GT_DISP.

  DATA: BEGIN OF LT_BASE OCCURS 0,
          VBELN	LIKE VBAP-VBELN,      " SO vbeln
          POSNR LIKE VBAP-POSNR,
          VSBED LIKE VBAK-VSBED,      " Shipping Conditions
          LIFNR LIKE VBPA-LIFNR,      " Carrier
          VBELV LIKE LIPS-VBELN,      " DO
        END OF LT_BASE.

  CLEAR: LS_ROWS.
  _CLEAR: LT_ROWS, LT_BASE, GT_INFO.

  CALL METHOD GC_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS.

  IF LINES( LT_ROWS ) EQ 0.
    MESSAGE S000(ZMCSD1) WITH TEXT-E01 DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    SORT LT_ROWS BY INDEX.
    LOOP AT LT_ROWS INTO LS_ROWS.
      CLEAR : LS_DATA.
      READ TABLE GT_DISP INTO LS_DATA INDEX LS_ROWS-INDEX.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING LS_DATA TO LT_BASE.
        COLLECT LT_BASE.CLEAR : LT_BASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CLEAR : GT_TRACK, GT_TRACK[].
  CHECK LT_BASE[] IS NOT INITIAL.
  LOOP AT LT_BASE.
    IF LT_BASE-VSBED = 'LT'.
      IF LT_BASE-LIFNR = '0000080038'.
        CLEAR: GS_VEKP.
        LOOP AT GT_VEKP INTO GS_VEKP WHERE VBELN = LT_BASE-VBELV.
          MOVE-CORRESPONDING GS_VEKP TO GT_TRACK.
          _APPEND GT_TRACK.

          CLEAR : GS_VEKP.
        ENDLOOP.
      ELSE.
        LOOP AT GT_DELIV WHERE VGBEL = LT_BASE-VBELN
                         AND   VGPOS = LT_BASE-POSNR.
          _CLEAR: GT_TRACK.
          GT_TRACK-VBELN   = GT_DELIV-VBELN.
          GT_TRACK-EXIDV2  = GT_DELIV-LIFEX.
          GT_TRACK-INHALT  = GT_DELIV-LIFEX.
          _APPEND GT_TRACK.
        ENDLOOP.
      ENDIF.
    ELSE.
      CLEAR: GS_VEKP.
      CLEAR: GT_TRACK.
      LOOP AT GT_VEKP INTO GS_VEKP WHERE VBELN = LT_BASE-VBELV.
        MOVE-CORRESPONDING GS_VEKP TO GT_TRACK.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = GT_TRACK-EXIDV2
          IMPORTING
            OUTPUT = GT_TRACK-EXIDV2.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = GT_TRACK-INHALT
          IMPORTING
            OUTPUT = GT_TRACK-INHALT.

        _APPEND GT_TRACK.
        CLEAR : GS_VEKP.
      ENDLOOP.
    ENDIF.

  ENDLOOP.
*
  IF GT_TRACK[] IS NOT INITIAL.
    CLEAR: GT_TRACK. SORT GT_TRACK BY VBELN EXIDV2 INHALT.

    CALL SCREEN '0600' STARTING AT 20 20
                         ENDING AT 140 32.
  ENDIF.
*
*  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_sold_to_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_LIFNR
*&      <-- GS_INFO_V_ADDR_NAME
*&      <-- GS_INFO_V_ADDR_1
*&      <-- GS_INFO_V_ADDR_2
*&      <-- GS_INFO_V_ADDR_3
*&      <-- GS_INFO_V_ADDR_4
*&---------------------------------------------------------------------*
FORM GET_SOLD_TO_ADDRESS  USING   PV_ADRNR
                         CHANGING PV_ADDR_NAME
                                  PV_ADDR_1
                                  PV_ADDR_2
                                  PV_ADDR_3
                                  PV_ADDR_4.

  CLEAR : GS_ADDR_V.
  READ TABLE GT_ADDR_V INTO GS_ADDR_V WITH TABLE KEY ADRNR = PV_ADRNR.
  IF SY-SUBRC = 0.
    PV_ADDR_NAME = GS_ADDR_V-NAME1.
    PV_ADDR_1 = GS_ADDR_V-STREET.
    PV_ADDR_2 = GS_ADDR_V-STR_SUPPL1.
    CONCATENATE GS_ADDR_V-STR_SUPPL2 GS_ADDR_V-STR_SUPPL3
                INTO PV_ADDR_3 SEPARATED BY SPACE.

    PV_ADDR_4 = GS_ADDR_V-LANDX.

  ELSE.
    SELECT SINGLE A~ADDRNUMBER AS ADRNR, A~NAME1, A~STREET,      A~STR_SUPPL1,
                  A~COUNTRY,    A~STR_SUPPL2,     A~STR_SUPPL3,  A~CITY1,
                  A~POST_CODE1, A~REGION,         A~TEL_NUMBER,  T~LANDX
                  INTO CORRESPONDING FIELDS OF @GS_ADDR_V
                  FROM ADRC AS A LEFT JOIN T005T AS T
                  ON    A~COUNTRY EQ T~LAND1
                  WHERE A~ADDRNUMBER EQ @PV_ADRNR
                  AND   T~SPRAS      EQ @SY-LANGU.
    IF SY-SUBRC = 0.
      INSERT GS_ADDR_V INTO TABLE GT_ADDR_V.

      PV_ADDR_NAME = GS_ADDR_V-NAME1.
      PV_ADDR_1 = GS_ADDR_V-STREET.
      PV_ADDR_2 = GS_ADDR_V-STR_SUPPL1.
      CONCATENATE GS_ADDR_V-CITY1 ','  INTO PV_ADDR_3.
      CONCATENATE PV_ADDR_3 GS_ADDR_V-POST_CODE1 INTO PV_ADDR_3 SEPARATED BY SPACE.

      PV_ADDR_4 = GS_ADDR_V-LANDX.
    ENDIF.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SHIP_TO_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_WERKS
*&      <-- GS_INFO_V_ADDR_NAME
*&      <-- GS_INFO_V_ADDR_1
*&      <-- GS_INFO_V_ADDR_2
*&      <-- GS_INFO_V_ADDR_3
*&      <-- GS_INFO_V_ADDR_4
*&---------------------------------------------------------------------*
FORM GET_SHIP_TO_ADDRESS  USING    PV_ADRNR
                          CHANGING PV_ADDR_NAME
                                   PV_ADDR_1
                                   PV_ADDR_2
                                   PV_ADDR_3
                                   PV_ADDR_4.

  CLEAR : GS_ADDR_S.
  READ TABLE GT_ADDR_S INTO GS_ADDR_S WITH TABLE KEY ADRNR = PV_ADRNR.
  IF SY-SUBRC = 0.
    PV_ADDR_NAME = GS_ADDR_S-STR_SUPPL1.
    PV_ADDR_1    = GS_ADDR_S-STREET.

    CONCATENATE GS_ADDR_S-NAME1 ',' INTO PV_ADDR_2.
    CONCATENATE PV_ADDR_2 GS_ADDR_S-REGION GS_ADDR_S-POST_CODE1
                INTO PV_ADDR_2
                SEPARATED BY SPACE.

    PV_ADDR_3 = GS_ADDR_S-TEL_NUMBER.
  ELSE.
    SELECT SINGLE A~ADDRNUMBER AS ADRNR      A~NAME1       A~STREET     A~CITY1
                  A~REGION     A~POST_CODE1  A~TEL_NUMBER A~LOCATION
                  A~STR_SUPPL1 A~STR_SUPPL2  A~STR_SUPPL3
                  INTO CORRESPONDING FIELDS OF GS_ADDR_S
                  FROM  ADRC AS A
                  WHERE A~ADDRNUMBER EQ PV_ADRNR.
    IF SY-SUBRC = 0.
      INSERT GS_ADDR_S INTO TABLE GT_ADDR_S.

      PV_ADDR_NAME = GS_ADDR_S-STR_SUPPL1.
      PV_ADDR_1    = GS_ADDR_S-STREET.

      CONCATENATE GS_ADDR_S-STR_SUPPL2 ',' INTO PV_ADDR_2.
*      CONCATENATE GS_ADDR_S-NAME1 ',' INTO PV_ADDR_2.
      CONCATENATE PV_ADDR_2 GS_ADDR_S-REGION GS_ADDR_S-POST_CODE1
                  INTO PV_ADDR_2
                  SEPARATED BY SPACE.

      PV_ADDR_3 = GS_ADDR_S-TEL_NUMBER.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SHIP_TO_ADDRESS_EXCEPTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ADRN2
*&      <-- GS_INFO_S_ADDR_NAME
*&      <-- GS_INFO_S_ADDR_1
*&      <-- GS_INFO_S_ADDR_2
*&      <-- GS_INFO_S_ADDR_3
*&      <-- GS_INFO_S_ADDR_4
*&---------------------------------------------------------------------*
FORM GET_SHIP_TO_ADDRESS_EXCEPTION  USING    PV_ADRN2
                                    CHANGING PV_ADDR_NAME
                                             PV_ADDR_1
                                             PV_ADDR_2
                                             PV_ADDR_3
                                             PV_ADDR_4.

  READ TABLE GT_ADDR_SE INTO GS_ADDR_SE WITH TABLE KEY ADRN2 = PV_ADRN2.
  IF SY-SUBRC = 0.
    IF GS_ADDR_SE-COUNTRY = 'US'.
      PV_ADDR_NAME = GS_ADDR_SE-STR_SUPPL1.
      PV_ADDR_1    = GS_ADDR_SE-STREET.

      CONCATENATE GS_ADDR_SE-NAME1 ',' INTO PV_ADDR_2.
      CONCATENATE PV_ADDR_2 GS_ADDR_SE-REGION GS_ADDR_SE-POST_CODE1
                  INTO PV_ADDR_2
                  SEPARATED BY SPACE.

      PV_ADDR_3 = GS_ADDR_SE-TEL_NUMBER.
    ELSE.
      CONCATENATE GS_ADDR_SE-NAME2 GS_ADDR_SE-NAME1 INTO PV_ADDR_NAME
                                                    SEPARATED BY SPACE.

      PV_ADDR_1 = GS_ADDR_SE-STREET.

      CONCATENATE GS_ADDR_SE-CITY1 ',' INTO PV_ADDR_2.
      CONCATENATE PV_ADDR_2 GS_ADDR_SE-BEZEI GS_ADDR_SE-POST_CODE1
                                             INTO PV_ADDR_2
                                             SEPARATED BY SPACE.

      PV_ADDR_3 = GS_ADDR_SE-LANDX.
      PV_ADDR_4 = GS_ADDR_SE-TEL_NUMBER.
    ENDIF.
  ELSE.
    SELECT SINGLE A~NAME1,       A~NAME2,     A~STREET,
                  A~CITY1,       A~LOCATION,  A~REGION,
                  A~POST_CODE1,  A~TEL_NUMBER,  A~COUNTRY,
                  A~STR_SUPPL1,  A~STR_SUPPL2,  A~STR_SUPPL3,
                  T~BEZEI,       L~LANDX
                  INTO CORRESPONDING FIELDS OF @GS_ADDR_SE
                  FROM  ADRC AS A
                  LEFT JOIN T005U AS T
                  ON    A~COUNTRY EQ T~LAND1
                  AND   A~REGION  EQ T~BLAND
                  LEFT JOIN T005T AS L
                  ON    L~LAND1 EQ T~LAND1
                  WHERE A~ADDRNUMBER EQ @PV_ADRN2
                  AND   T~SPRAS      EQ @SY-LANGU
                  AND   L~SPRAS      EQ @SY-LANGU.
    IF SY-SUBRC = 0.
      GS_ADDR_SE-ADRN2 = PV_ADRN2.
      INSERT GS_ADDR_SE INTO TABLE GT_ADDR_SE.

      IF GS_ADDR_SE-COUNTRY = 'US'.
        PV_ADDR_NAME = GS_ADDR_SE-STR_SUPPL1.
        PV_ADDR_1    = GS_ADDR_SE-STREET.

        CONCATENATE GS_ADDR_SE-NAME1 ',' INTO PV_ADDR_2.
        CONCATENATE PV_ADDR_2 GS_ADDR_SE-REGION GS_ADDR_SE-POST_CODE1
                    INTO PV_ADDR_2
                    SEPARATED BY SPACE.

        PV_ADDR_3 = GS_ADDR_SE-TEL_NUMBER.
      ELSE.
        CONCATENATE GS_ADDR_SE-NAME2 GS_ADDR_SE-NAME1 INTO PV_ADDR_NAME
                                                      SEPARATED BY SPACE.

        PV_ADDR_1 = GS_ADDR_SE-STREET.

        CONCATENATE GS_ADDR_SE-CITY1 ',' INTO PV_ADDR_2.
        CONCATENATE PV_ADDR_2 GS_ADDR_SE-BEZEI GS_ADDR_SE-POST_CODE1
                                               INTO PV_ADDR_2
                                               SEPARATED BY SPACE.

        PV_ADDR_3 = GS_ADDR_SE-LANDX.
        PV_ADDR_4 = GS_ADDR_SE-TEL_NUMBER.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SOLD_TO_SHIP_TO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SORDER
*&      <-- LV_LIFNR
*&      <-- LV_KUNNR
*&---------------------------------------------------------------------*
FORM GET_SOLD_TO_SHIP_TO  USING    PV_SORDER
                          CHANGING PV_LIFNR  PV_SOLDTO_ADRNO
                                   PV_KUNNR  PV_SHIPTO_ADRNO
                                   PV_SHIPVIA.

  DATA : BEGIN OF LS_VBPA,
           VBELN   LIKE VBPA-VBELN,
           LIFNR   LIKE VBPA-LIFNR,
           ADRNR_V LIKE VBPA-ADRNR,
           KUNNR   LIKE VBPA-KUNNR,
           ADRNR_S LIKE VBPA-ADRNR,
           SHIPVIA LIKE ADRC-NAME1,
         END OF LS_VBPA.
  DATA : LT_VBPA LIKE HASHED TABLE OF LS_VBPA WITH UNIQUE KEY VBELN.


  CLEAR : LS_VBPA.
  READ TABLE LT_VBPA INTO LS_VBPA WITH TABLE KEY VBELN = PV_SORDER.
  IF SY-SUBRC = 0.
    PV_LIFNR        = LS_VBPA-LIFNR.
    PV_SOLDTO_ADRNO = LS_VBPA-ADRNR_V.
    PV_KUNNR        = LS_VBPA-KUNNR.
    PV_SHIPTO_ADRNO = LS_VBPA-ADRNR_S.
    PV_SHIPVIA     = LS_VBPA-SHIPVIA.
  ELSE.
    SELECT SINGLE KUNNR AS LIFNR
                  ADRNR AS ADRNR_V
                  INTO CORRESPONDING FIELDS OF LS_VBPA
                  FROM VBPA
                  WHERE VBELN EQ PV_SORDER
                  AND   PARVW EQ 'AG'.

    SELECT SINGLE KUNNR AS KUNNR
                  ADRNR AS ADRNR_S
                  INTO CORRESPONDING FIELDS OF LS_VBPA
                  FROM VBPA
                  WHERE VBELN EQ PV_SORDER
                  AND   PARVW EQ 'WE'.

    SELECT SINGLE A~NAME1 AS SHIPVIA
                  INTO CORRESPONDING FIELDS OF LS_VBPA
                  FROM VBPA AS V INNER JOIN ADRC AS A
                  ON   V~ADRNR EQ A~ADDRNUMBER
                  WHERE V~VBELN EQ PV_SORDER
                  AND   V~PARVW EQ 'SP'.

    LS_VBPA-VBELN = PV_SORDER.
    INSERT LS_VBPA INTO TABLE LT_VBPA.

    PV_LIFNR        = LS_VBPA-LIFNR.
    PV_SOLDTO_ADRNO = LS_VBPA-ADRNR_V.
    PV_KUNNR        = LS_VBPA-KUNNR.
    PV_SHIPTO_ADRNO = LS_VBPA-ADRNR_S.
    PV_SHIPVIA      = LS_VBPA-SHIPVIA.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MATERIAL_DESC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISP_MATNR
*&      <-- GS_DISP_MAKTX
*&---------------------------------------------------------------------*
FORM GET_MATERIAL_DESC  USING    PV_MATNR
                        CHANGING PV_MAKTX.

  DATA : BEGIN OF LS_MARA,
           MATNR LIKE MARA-MATNR,
           MAKTX LIKE MAKT-MAKTX,
         END OF LS_MARA.
  STATICS : LT_MARA LIKE HASHED TABLE OF LS_MARA WITH UNIQUE KEY MATNR.

  CLEAR : LS_MARA.
  READ TABLE LT_MARA INTO LS_MARA WITH TABLE KEY MATNR = PV_MATNR.
  IF SY-SUBRC = 0.
    PV_MAKTX = LS_MARA-MAKTX.
  ELSE.
    SELECT SINGLE M~MATNR T~MAKTX
           INTO CORRESPONDING FIELDS OF LS_MARA
           FROM MARA AS M INNER JOIN MAKT AS T
           ON    M~MATNR EQ T~MATNR
           WHERE M~MATNR EQ PV_MATNR
           AND   T~SPRAS EQ SY-LANGU.
    IF SY-SUBRC = 0.
      SHIFT LS_MARA-MAKTX LEFT DELETING LEADING ' '.
      INSERT LS_MARA INTO TABLE LT_MARA.

      PV_MAKTX = LS_MARA-MAKTX.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_PDF_FILE
*&---------------------------------------------------------------------*
FORM CALL_PDF_FILE .

*-- SMART FORMS.
  DATA: LV_FORMNAME TYPE TDSFNAME,
        LV_TABIX    TYPE SY-TABIX,
        LV_INT      TYPE I,
        LV_BELNR    LIKE BSAK-BELNR,
        LV_INDEX    TYPE SY-INDEX,
        FILE_NAME   TYPE STRING,
        FILE_PATH   TYPE STRING,
        FULL_PATH   TYPE STRING.

  DATA: LS_FORM_NAME  TYPE TDSFNAME,
        LS_FMODULE    TYPE RS38L_FNAM,
        LS_CPARAM     TYPE SSFCTRLOP,
        LS_OUTOPTIONS TYPE SSFCOMPOP.

  DATA: LV_DEVTYPE TYPE RSPOPTYPE.

  IF GV_CHECK IS NOT INITIAL.
    MESSAGE S000 WITH TEXT-E02 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: GS_FM_NAME, GS_OUTPUT_OPTION,
         GS_PRINT_OPTION, GS_JOB_OUTPUT_INFO,
         LV_FORMNAME.

  GS_PRINT_OPTION-NO_DIALOG = 'X'. " ?????????. X = ????????????.
  GS_PRINT_OPTION-PREVIEW   = ' '.     " ????????????. ' ' = ???????????? ?????????.
  GS_PRINT_OPTION-LANGU     = SY-LANGU.
  GS_PRINT_OPTION-GETOTF    = 'X'.  " GET OTF DATA

*  GS_OUTPUT_OPTION-TDDEST =  'ZHP'.      " ????????? ??????.

  GS_OUTPUT_OPTION-TDCOPIES = 1 .      " ?????? ????????????.
  GS_OUTPUT_OPTION-TDNOPRINT = 'X'.


  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      I_LANGUAGE             = '3'
    IMPORTING
      E_DEVTYPE              = LV_DEVTYPE
    EXCEPTIONS
      NO_LANGUAGE            = 1
      LANGUAGE_NOT_INSTALLED = 2
      NO_DEVTYPE_FOUND       = 3
      SYSTEM_ERROR           = 4
      OTHERS                 = 5.

  GS_OUTPUT_OPTION-TDPRINTER     = LV_DEVTYPE.
  GS_OUTPUT_OPTION-TDAUTORITY    = SPACE.
  GS_OUTPUT_OPTION-TDFINAL       = 'X'.

  LV_FORMNAME = 'ZSD_PRINT_BILLING'.

  " Call Smartforms
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LV_FORMNAME
    IMPORTING
      FM_NAME            = GS_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.

    LOOP AT GT_HEAD INTO GS_HEAD.

      CALL FUNCTION GS_FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = GS_PRINT_OPTION
          OUTPUT_OPTIONS     = GS_OUTPUT_OPTION
          USER_SETTING       = 'X'
          GS_HEAD            = GS_HEAD
        IMPORTING
          JOB_OUTPUT_INFO    = GS_JOB_OUTPUT_INFO
        TABLES
          GT_ITEM            = GT_ITEM
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.

      PERFORM DOWNLOAD_PDF_FILE USING GS_HEAD-VBELN.

      CLEAR : GS_HEAD.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_PDF_FILE
*&---------------------------------------------------------------------*
FORM DOWNLOAD_PDF_FILE USING PV_VBELN.

* Internal table to hold the OTF data
  DATA: LT_OTF     TYPE ITCOO OCCURS 0 WITH HEADER LINE,
        LT_OTF_TMP TYPE ITCOO OCCURS 0 WITH HEADER LINE,
        LT_DOC     LIKE DOCS OCCURS 0 WITH HEADER LINE,
        LT_PDF_TAB LIKE TLINE OCCURS 0 WITH HEADER LINE.

  DATA : LV_BIN_FILESIZE TYPE I, " Binary File Size
         LV_FILE_NAME    TYPE STRING,
         LV_FILE_PATH    TYPE STRING,
         LV_FULL_PATH    TYPE STRING.

  LT_OTF[] = GS_JOB_OUTPUT_INFO-OTFDATA.

*Covert the OTF data returned to PDF format
  CALL FUNCTION 'CONVERT_OTF_2_PDF'
    EXPORTING
      USE_OTF_MC_CMD         = 'X'
    IMPORTING
      BIN_FILESIZE           = LV_BIN_FILESIZE
    TABLES
      OTF                    = LT_OTF
      DOCTAB_ARCHIVE         = LT_DOC
      LINES                  = LT_PDF_TAB
    EXCEPTIONS
      ERR_CONV_NOT_POSSIBLE  = 1
      ERR_OTF_MC_NOENDMARKER = 2
      OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DATA : LV_TIMESTAMP TYPE TIMESTAMP,
         LV_STAMP_C   TYPE CHAR15.

  IF GV_FILE_PATH IS INITIAL.

    READ TABLE GT_DISP WITH KEY VBELB = PV_VBELN.
    IF SY-SUBRC = 0.
      PERFORM  ALPHA_OUTPUT  USING  GT_DISP-BSTKD.
      PERFORM  ALPHA_OUTPUT  USING  GT_DISP-VBELN.
      PERFORM  ALPHA_OUTPUT  USING  PV_VBELN.
      CONCATENATE GT_DISP-BSTKD '-' GT_DISP-VBELN '-' PV_VBELN '.pdf'
     INTO LV_FILE_NAME.
    ENDIF.


* To display File SAVE dialog window
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
      EXPORTING
        WINDOW_TITLE         = 'BILLING PDF'
        DEFAULT_EXTENSION    = '.PDF'
        DEFAULT_FILE_NAME    = LV_FILE_NAME
*       FILE_FILTER          = 'PDF File (*.PDF)'
        PROMPT_ON_OVERWRITE  = 'X'
      CHANGING
        FILENAME             = LV_FILE_NAME
        PATH                 = LV_FILE_PATH
        FULLPATH             = LV_FULL_PATH
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        NOT_SUPPORTED_BY_GUI = 3
        OTHERS               = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSEIF SY-SUBRC = 0.
      GV_FILE_PATH = LV_FILE_PATH.
    ENDIF.

* Use the FM GUI_DOWNLOAD to download the generated PDF file onto the
* presentation server
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        BIN_FILESIZE            = LV_BIN_FILESIZE
        FILENAME                = LV_FULL_PATH
        FILETYPE                = 'BIN'
      TABLES
        DATA_TAB                = LT_PDF_TAB
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
        OTHERS                  = 22.

  ELSE.

    CLEAR : LV_FILE_NAME.
    READ TABLE GT_DISP WITH KEY VBELB = PV_VBELN.
    IF SY-SUBRC = 0.
      PERFORM  ALPHA_OUTPUT  USING  GT_DISP-BSTKD.
      PERFORM  ALPHA_OUTPUT  USING  GT_DISP-VBELN.
      PERFORM  ALPHA_OUTPUT  USING  PV_VBELN.
      CONCATENATE GT_DISP-BSTKD '-' GT_DISP-VBELN '-' PV_VBELN '.pdf'
     INTO LV_FILE_NAME.
    ENDIF.

    CLEAR : LV_FULL_PATH.
    CONCATENATE GV_FILE_PATH LV_FILE_NAME INTO LV_FULL_PATH.

* Use the FM GUI_DOWNLOAD to download the generated PDF file onto the
* presentation server
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        BIN_FILESIZE            = LV_BIN_FILESIZE
        FILENAME                = LV_FULL_PATH
        FILETYPE                = 'BIN'
      TABLES
        DATA_TAB                = LT_PDF_TAB
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
        OTHERS                  = 22.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_KUNNR F4_COMMERCIAL_INVOICE
*&---------------------------------------------------------------------*
FORM F4_KUNNR  CHANGING PV_CODE.

  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL,
        LS_RETURN TYPE DDSHRETVAL.
  DATA: LS_DYNPFIELDS TYPE DYNPREAD,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  DATA : BEGIN OF LT_KUNNR OCCURS 0,
           KUNNR LIKE KNA1-KUNNR,
           NAME1 LIKE KNA1-NAME1,
           SORTL LIKE KNA1-SORTL,
           ORT01 LIKE KNA1-ORT01,
           STRAS LIKE KNA1-STRAS,
           LAND1 LIKE KNA1-LAND1,
           PSTLZ LIKE KNA1-PSTLZ,
         END   OF LT_KUNNR.

  CLEAR : LT_KUNNR[], LT_KUNNR, LT_RETURN, LT_RETURN[].
  SELECT KUNNR NAME1 SORTL ORT01 STRAS LAND1 PSTLZ
         INTO CORRESPONDING FIELDS OF TABLE LT_KUNNR
         FROM KNA1
         WHERE LOEVM EQ SPACE.

*-- Pop-up F4.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'KUNNR'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = LT_KUNNR
      RETURN_TAB      = LT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  READ TABLE LT_RETURN INTO LS_RETURN INDEX 1.
  IF SY-SUBRC = 0.
    PV_CODE = LS_RETURN-FIELDVAL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_DATA
*&---------------------------------------------------------------------*
FORM SELECT_DATA .

* ???????????? DATA??? ????????? ?????????.
  _CLEAR: GT_SALES.

  SELECT A~BSTKD,
         A~ZTERM,
         A~KONDA,
         A~PRSDT,
         B~AUART,
         B~VTWEG,
         B~KUNNR,
         B~AUDAT,
         B~ERDAT,
         B~ERZET,
         B~VSBED,
         B~VDATU,
         B~KNUMV,
         C~POSEX,
         C~KDMAT,
         C~VBELN,
         C~POSNR,
         C~MATNR,
         C~BWTAR,
         C~NETWR,
         C~NETPR,
         C~WAERK,
         C~KWMENG,
         C~VRKME,
         C~BRGEW,
         C~NTGEW,
         C~GEWEI,
         C~VOLUM,
         C~VOLEH,
         C~PSTYV,
         C~WERKS,
         C~ABGRU,
         C~ZZSET_INFO,
         D~NAME1,
         E~BEZEI,
         F~BEZEI AS AEZEI,
         I~MAKTX,
         L~VTEXT AS PRODH_TXT,
         K~FKDAT,
         N~WADAT_IST,
         Z~VTEXT AS KONDA_TXT
    INTO CORRESPONDING FIELDS OF TABLE @GT_SALES
    FROM VBKD AS A INNER JOIN VBAK AS B
                      ON A~VBELN = B~VBELN
                   INNER JOIN VBAP AS C
                      ON B~VBELN = C~VBELN
              LEFT OUTER JOIN KNA1 AS D
                      ON B~KUNNR = D~KUNNR
              LEFT OUTER JOIN TVAGT AS E
                      ON C~ABGRU = E~ABGRU
                     AND E~SPRAS = @SY-LANGU
              LEFT OUTER JOIN TVAKT AS F
                      ON B~AUART = F~AUART
                     AND F~SPRAS = @SY-LANGU
                   INNER JOIN MARA AS G
                      ON C~MATNR = G~MATNR
              LEFT OUTER JOIN T179T AS L
                      ON G~PRDHA = L~PRODH
                     AND L~SPRAS = @SY-LANGU
              LEFT OUTER JOIN MAKT AS I
                      ON C~MATNR = I~MATNR
                     AND I~SPRAS = @SY-LANGU
              LEFT OUTER JOIN VBRP AS J
                      ON J~AUBEL = C~VBELN
                     AND J~AUPOS = C~POSNR
              LEFT OUTER JOIN VBRK AS K
                      ON K~VBELN = J~VBELN
                     AND K~SFAKN = @SPACE
                     AND K~FKSTO = @SPACE
              LEFT OUTER JOIN LIPS AS M
                      ON C~VBELN = M~VGBEL
                     AND C~POSNR = M~VGPOS
              LEFT OUTER JOIN LIKP AS N
                      ON M~VBELN = N~VBELN
              LEFT OUTER JOIN T188T AS Z
                      ON A~KONDA = Z~KONDA
                     AND Z~SPRAS = @SY-LANGU
   WHERE A~BSTKD IN @S_BSTKD
     AND A~VBELN IN @S_VBELN
     AND A~POSNR EQ '000000'
     AND B~VKORG EQ @P_VKORG
     AND B~AUDAT IN @S_AUDAT
     AND B~VTWEG IN @S_VTWEG
     AND B~AUART IN @S_AUART
     AND B~KUNNR IN @S_KUNNR
     AND B~ERDAT IN @S_ERDAT
     AND C~MATNR IN @S_MATNR.

  IF S_FKDAT[] IS NOT INITIAL.
    DELETE GT_SALES WHERE FKDAT NOT IN S_FKDAT.
  ENDIF.

  IF S_WADAT[] IS NOT INITIAL.
    DELETE GT_SALES WHERE WADAT_IST NOT IN S_WADAT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_OBLIGATORY
*&---------------------------------------------------------------------*
FORM CHECK_OBLIGATORY .

  IF S_ERDAT[] IS INITIAL AND
     S_WADAT[] IS INITIAL AND
     S_FKDAT[] IS INITIAL.
    MESSAGE S000 WITH 'At least one field is mandatory '
                      '(Order Creation date, GI Date,'
                      ' Invoice Date).'
                 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORIZATION_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORIZATION_CHECK .

  DATA : LS_RETURN LIKE BAPIRETURN1 .

  CALL FUNCTION 'ZBC_AUTHORIZATION_CHECK'
    EXPORTING
      I_VKORG   = P_VKORG
    IMPORTING
      ES_RETURN = LS_RETURN.

  IF LS_RETURN-TYPE = 'E'.
    MESSAGE E000 WITH LS_RETURN-MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SMARTFORMS_DATA
*&---------------------------------------------------------------------*
FORM GET_SMARTFORMS_DATA .

  DATA : BEGIN OF LT_BILLING OCCURS 0,
           VBELN LIKE VBRK-VBELN,
         END OF LT_BILLING.

  DATA : LV_TEXT       LIKE BSIS-SGTXT,
         LV_DMBTR      LIKE BSIS-DMBTR,
         LV_DMBTR2     LIKE BSIS-DMBTR,
         LV_TOT_WO_VAT LIKE BSIS-DMBTR,
         LV_TOT_VAT    LIKE BSIS-DMBTR,
         LV_WAERS      LIKE T001-WAERS,
         LV_DIV        TYPE P DECIMALS 2,
         LV_MOD        TYPE I,
         LV_LINES      TYPE I,
         LV_SEQ        TYPE I,
         LV_MENGE      TYPE CHAR6,
         LV_QTY        LIKE VBRP-FKIMG.

  CALL METHOD GC_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.

  IF GT_ROWS[] IS INITIAL.
    MESSAGE S014.
    EXIT.
  ENDIF.

  CLEAR GV_CHECK.
  _CLEAR LT_BILLING.

  LOOP AT GT_ROWS INTO GS_ROWS.
    READ TABLE GT_DISP INDEX GS_ROWS-INDEX.
    IF SY-SUBRC = 0.
      IF GT_DISP-FKDAT IS INITIAL.
        GV_CHECK =  'X'.
        EXIT.
      ENDIF.
      LT_BILLING-VBELN = GT_DISP-VBELB.
      COLLECT LT_BILLING. CLEAR LT_BILLING.
    ENDIF.
  ENDLOOP.

  CHECK GV_CHECK IS INITIAL.

  _CLEAR : GT_ITEM.
  CLEAR GS_HEAD.
  LOOP AT LT_BILLING.

    GS_HEAD-VBELN = LT_BILLING-VBELN.
    READ TABLE GT_BILLI WITH KEY VBELN = LT_BILLING-VBELN.
    IF SY-SUBRC = 0.
      GS_HEAD-KUNNR_TXT  = GT_BILLI-NAME_ORG1.
      CONCATENATE SY-DATUM(4) '???' SPACE SY-DATUM+4(2) '???'  SPACE SY-DATUM+6(2) '???'
      INTO GS_HEAD-PRINT_DATE.

      GS_HEAD-BILL_DATE = GT_BILLI-FKDAT.
      CONCATENATE GT_BILLI-FKDAT(4) '???' SPACE GT_BILLI-FKDAT+4(2) '???'  SPACE GT_BILLI-FKDAT+6(2) '???'
     INTO GS_HEAD-BILL_DATE.
    ENDIF.
    CLEAR : LV_QTY, LV_DMBTR, LV_TOT_WO_VAT, LV_TOT_VAT.
    LOOP AT GT_DISP WHERE VBELB EQ GS_HEAD-VBELN.

      READ TABLE GT_BILLI WITH KEY VBELN = GS_HEAD-VBELN
                                   POSNR = GT_DISP-POSNB BINARY SEARCH.

      GT_ITEM-VBELN = GS_HEAD-VBELN.
      GT_ITEM-MATNR = GT_BILLI-MATNR.
      GT_ITEM-ARKTX = GT_BILLI-ARKTX.

      LV_QTY = LV_QTY + GT_BILLI-FKIMG.
      LV_TOT_WO_VAT = LV_TOT_WO_VAT + GT_BILLI-NETWR.
      LV_TOT_VAT = LV_TOT_VAT + GT_BILLI-MWSBP.
      LV_DMBTR = LV_DMBTR + GT_BILLI-NETWR + GT_BILLI-MWSBP.

      CLEAR LV_MENGE.
      WRITE GT_BILLI-FKIMG TO LV_MENGE UNIT GT_BILLI-VRKME.
      GT_ITEM-FKIMG = LV_MENGE.

      PERFORM CONV_CURRENCY USING GT_BILLI-KZWI1 'KRW'
                          CHANGING GT_ITEM-KZWI1.
      PERFORM CONV_CURRENCY USING GT_BILLI-NETWR 'KRW'
                          CHANGING GT_ITEM-NETWR.
      PERFORM CONV_CURRENCY USING GT_BILLI-MWSBP 'KRW'
                          CHANGING GT_ITEM-MWSBP.

      CLEAR LV_DMBTR2.
      LV_DMBTR2 = GT_BILLI-MWSBP +  GT_BILLI-NETWR.
      PERFORM CONV_CURRENCY USING LV_DMBTR2 'KRW'
                          CHANGING GT_ITEM-TOT_AMOUNT.

      GT_ITEM-WAERK = GT_BILLI-WAERK.

      APPEND GT_ITEM. CLEAR GT_ITEM.
    ENDLOOP.

    GS_HEAD-TEL_NUMB = GT_DISP-TEL_NUMBER.
    CONCATENATE GT_DISP-CITY1 GT_DISP-STREET INTO GS_HEAD-ADDRESS
    SEPARATED BY SPACE.

    LV_WAERS = 'KRW'.
    CLEAR LV_TEXT.
    CALL FUNCTION 'Z_CHANGE_GET_TEXT_AMOUNT'
      EXPORTING
        I_AMOUNT   = LV_DMBTR
        I_WAERS    = LV_WAERS
      IMPORTING
        E_AMT_TEXT = LV_TEXT.
    GS_HEAD-TOT_KO = '???' && LV_TEXT && '???'."???????????? ??????

    PERFORM CONV_CURRENCY USING LV_DMBTR 'KRW'
                        CHANGING GS_HEAD-TOT_AMOUNT.

    PERFORM CONV_CURRENCY USING LV_TOT_WO_VAT 'KRW'
                        CHANGING GS_HEAD-TOT_WO_VAT.

    PERFORM CONV_CURRENCY USING LV_TOT_VAT 'KRW'
                        CHANGING GS_HEAD-TOT_VAT.

    CLEAR LV_MENGE.
    WRITE LV_QTY TO LV_MENGE UNIT GT_BILLI-VRKME.
    GS_HEAD-TOT_QUANT = LV_MENGE.

    CLEAR : LV_LINES.
    LOOP AT GT_ITEM WHERE VBELN EQ GS_HEAD-VBELN.
      LV_LINES = LV_LINES + 1.
    ENDLOOP.

    CLEAR : LV_MOD, LV_SEQ.
    LV_MOD = LV_LINES MOD 26.

    IF LV_MOD IS INITIAL.
      LV_SEQ = 26 - LV_LINES.
    ELSE.
      LV_SEQ = 26 - LV_MOD.
    ENDIF.

    DO LV_SEQ TIMES.

      CLEAR : GT_ITEM.
      GT_ITEM-VBELN = GS_HEAD-VBELN.
      APPEND GT_ITEM.
    ENDDO.


    APPEND GS_HEAD TO GT_HEAD.
    CLEAR : GS_HEAD.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_SMARTFORMS
*&---------------------------------------------------------------------*
FORM CALL_SMARTFORMS  USING PV_PREVIEW.

*-- SMART FORMS.
  DATA: LV_FORMNAME TYPE TDSFNAME,
        LV_TABIX    TYPE SY-TABIX,
        LV_INT      TYPE I,
        LV_BELNR    LIKE BSAK-BELNR,
        LV_INDEX    TYPE SY-INDEX,
        FILE_NAME   TYPE STRING,
        FILE_PATH   TYPE STRING,
        FULL_PATH   TYPE STRING.

  DATA : LV_LINES TYPE I.

  DATA: L_DEVTYPE TYPE RSPOPTYPE.


  IF GV_CHECK IS NOT INITIAL.
    MESSAGE S000 WITH TEXT-E02 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  CLEAR: GS_FM_NAME, GS_OUTPUT_OPTION,
         GS_PRINT_OPTION, GS_JOB_OUTPUT_INFO,
         LV_FORMNAME.

  GS_PRINT_OPTION-NO_DIALOG = 'X'. " ?????????. X = ????????????.
  GS_PRINT_OPTION-PREVIEW   = PV_PREVIEW.     " ????????????. ' ' = ???????????? ?????????.
  GS_PRINT_OPTION-LANGU     = SY-LANGU.

  GS_OUTPUT_OPTION-TDDEST =  'ZHP'.    " ????????? ??????.
  GS_OUTPUT_OPTION-TDCOPIES = 1 .      " ?????? ????????????.
  GS_OUTPUT_OPTION-TDNOPREV  = SPACE.
  GS_OUTPUT_OPTION-TDNOPRINT = SPACE.
  GS_OUTPUT_OPTION-TDIMMED = 'X'.

  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      I_LANGUAGE             = 'E'
    IMPORTING
      E_DEVTYPE              = L_DEVTYPE
    EXCEPTIONS
      NO_LANGUAGE            = 1
      LANGUAGE_NOT_INSTALLED = 2
      NO_DEVTYPE_FOUND       = 3
      SYSTEM_ERROR           = 4
      OTHERS                 = 5.

  GS_OUTPUT_OPTION-TDPRINTER     = L_DEVTYPE.

  LV_FORMNAME = 'ZSD_PRINT_BILLING'.

  " Call Smartforms
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = LV_FORMNAME
    IMPORTING
      FM_NAME            = GS_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ELSE.

    DESCRIBE TABLE GT_HEAD LINES LV_LINES.

    LOOP AT GT_HEAD INTO GS_HEAD.

      GS_PRINT_OPTION-NO_OPEN  = 'X'.
      GS_PRINT_OPTION-NO_CLOSE = 'X'.

      AT FIRST.
        GS_PRINT_OPTION-NO_OPEN  = ' '.
        GS_PRINT_OPTION-NO_CLOSE = 'X'.
      ENDAT.

      AT LAST.
        GS_PRINT_OPTION-NO_OPEN  = 'X'.
        GS_PRINT_OPTION-NO_CLOSE = ' '.
      ENDAT.

      IF LV_LINES = 1.
        GS_PRINT_OPTION-NO_OPEN  = ' '.
        GS_PRINT_OPTION-NO_CLOSE = ' '.
      ENDIF.

      CALL FUNCTION GS_FM_NAME
        EXPORTING
          CONTROL_PARAMETERS = GS_PRINT_OPTION
          OUTPUT_OPTIONS     = GS_OUTPUT_OPTION
          USER_SETTING       = 'X'
          GS_HEAD            = GS_HEAD
        IMPORTING
          JOB_OUTPUT_INFO    = GS_JOB_OUTPUT_INFO
        TABLES
          GT_ITEM            = GT_ITEM
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.

      CLEAR : GS_HEAD.

    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_CURRENCY
*&---------------------------------------------------------------------*
FORM CONV_CURRENCY  USING    PV_AMOUNT
                             PV_CURR
                    CHANGING PV_DATA.

  DATA : LV_NETPR TYPE CHAR16.

  CLEAR : LV_NETPR, PV_DATA.
  WRITE PV_AMOUNT TO LV_NETPR CURRENCY PV_CURR.
  CONDENSE LV_NETPR.
  PV_DATA = '???' && LV_NETPR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_AMOUNT
*&---------------------------------------------------------------------*
FORM CONV_AMOUNT  USING    PV_KBETR
                           PV_WAERS
                  CHANGING PV_KBETR_C.


  DATA : LV_KBETR     TYPE C LENGTH 25,
         LV_KONWA_CON TYPE C LENGTH 25,
         LV_WAERS     LIKE TCURC-WAERS.

  CLEAR : LV_KBETR, LV_KONWA_CON, LV_WAERS.

  LV_KBETR = PV_KBETR.
  LV_WAERS = PV_WAERS.
  CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
    EXPORTING
      CURRENCY    = LV_WAERS
      IDOC_AMOUNT = LV_KBETR
    IMPORTING
      SAP_AMOUNT  = LV_KONWA_CON.

  CONDENSE LV_KONWA_CON.
  PV_KBETR_C = LV_KONWA_CON.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALPHA_OUTPUT
*&---------------------------------------------------------------------*
FORM ALPHA_OUTPUT  USING  PV_DATA.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = PV_DATA
    IMPORTING
      OUTPUT = PV_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RETURN_NUMBER
*&---------------------------------------------------------------------*
FORM RETURN_NUMBER  USING PV_NUMBER.

  IF PV_NUMBER IS NOT INITIAL.
    PV_NUMBER = PV_NUMBER * -1.
  ENDIF.

ENDFORM.
