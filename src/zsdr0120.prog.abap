*&---------------------------------------------------------------------*
*& Include          ZSDR0120
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CALL_VA02_NEW_ORDERS_ORIGIN_PO
*&---------------------------------------------------------------------*
*   CALL TRANSACTION auf die Transaktion VA02                          *
*   CALL TRANSACTION to transaction VA02                               *
*&---------------------------------------------------------------------*
FORM call_va02_new_orders_origin_po USING rc.
  "|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  "||||                                                             ||||
  "|||| 필요한 데이터를 ABAP MEMORY를 이용해서 USER-EXIT으로 전송   ||||
  "||||   - CONFIG WE20 XXXXXXXXXX LS INBOUND                       ||||
  "||||   - USER-EXIT MV45AFZZ FORM USEREXIT_SAVE_DOCUMENT_PREPARE  ||||
  "||||                                                             ||||
  "|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  DATA: ls_idoc_data     TYPE edidd,
        lv_bdcdata_tabix TYPE sy-tabix.
  DATA: lv_fname TYPE string,      "field name in long text
        lv_value TYPE string.      "field value in long text
  DATA: BEGIN OF lt_head OCCURS 0,
          sdata TYPE text100,
        END OF lt_head,
        BEGIN OF lt_item OCCURS 0,
          sdata TYPE text100,
        END OF lt_item.
  DATA: lv_ekorg TYPE ekorg,       "Purch. org
        lv_kunwe TYPE kunwe,       "ship to party
        lv_kunsx TYPE kunnr,       "original sold-to
        lv_idx   TYPE sy-tabix.    "idx
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// HEAD / ITEM ALL
  "////
  "/////////////////////////////////////////////////////////////////////
  CLEAR: lt_head[], lt_item[].
  LOOP AT idoc_data INTO ls_idoc_data.
    IF     ls_idoc_data-segnam   = 'E1EDKT2'.
      TRANSLATE ls_idoc_data-sdata USING '* '.
      lt_head-sdata = ls_idoc_data-sdata.
      APPEND lt_head.
      CLEAR lt_head.
    ELSEIF ls_idoc_data-segnam   = 'E1EDPT2'.
      TRANSLATE ls_idoc_data-sdata USING '* '.
      lt_item-sdata = ls_idoc_data-sdata.
      APPEND lt_item.
      CLEAR lt_item.
    ELSE.
      CHECK 1 = 1.
    ENDIF.
  ENDLOOP.
  EXPORT lt_head[] TO MEMORY ID 'ZHEAD'.  "<= 헤더텍스트
  EXPORT lt_item[] TO MEMORY ID 'ZITEM'.  "<= 아이템텍스트
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 본사구매조직
  "////
  "/////////////////////////////////////////////////////////////////////
  LOOP AT idoc_data INTO ls_idoc_data WHERE segnam   = 'E1EDK14'
                                        AND sdata(3) = '014'.
    CLEAR: lv_fname, lv_value.
    lv_value = ls_idoc_data-sdata+3.
    lv_ekorg = lv_value.
    EXPORT lv_ekorg TO MEMORY ID 'ZEKORG'.  "<= 구매조직 전송
    EXIT.
  ENDLOOP.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 본사구매조직 MAP 제조판매조직/유통경로
  "////
  "/////////////////////////////////////////////////////////////////////
  IF lv_ekorg IS NOT INITIAL.
    SELECT SINGLE zcm_code3 INTO xvbak-vtweg
      FROM zcommt0021
     WHERE spras     EQ sy-langu
       AND zmodule   EQ 'SD'
       AND zclass    EQ 'SD004'
       AND zcm_code1 EQ lv_ekorg.
  ENDIF.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// Payment Terms
  "////
  "/////////////////////////////////////////////////////////////////////
  SELECT SINGLE
         zterm
    INTO @xvbak-zterm
    FROM knvv
   WHERE kunnr EQ @xvbak-kunnr
     AND vkorg EQ @xvbak-vkorg
     AND vtweg EQ @xvbak-vtweg
     AND spart EQ @xvbak-spart.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// Pricing Date
  "////
  "/////////////////////////////////////////////////////////////////////
  LOOP AT idoc_data INTO ls_idoc_data WHERE segnam   = 'E1EDK02'.
    xvbak-prsdt = ls_idoc_data-sdata+44(8).
  ENDLOOP.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 본사구매조직에 따라 로직 분기 ★
  "////
  "/////////////////////////////////////////////////////////////////////
  IF     lv_ekorg = '10A1'.                       "Amazon
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// 납품처
    "////
    "/////////////////////////////////////////////////////////////////////
    LOOP AT idoc_data INTO ls_idoc_data WHERE segnam   = 'E1EDKT2'
                                          AND sdata(3) = 'A02'.
      TRANSLATE ls_idoc_data-sdata USING '* '.
      CLEAR: lv_fname, lv_value.
      SPLIT ls_idoc_data-sdata AT ':' INTO lv_fname lv_value.
      CONDENSE lv_value NO-GAPS.
      lv_kunwe = lv_value.
      EXPORT lv_kunwe TO MEMORY ID 'ZKUNWE'.  "<= 납품처 전송
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_kunwe
        IMPORTING
          output = lv_kunwe.
      "//////////////////////////////////////////////////////////////////
      "////
      "//// SHIP-TO Party 바꾸기 (XVBADR 바꾸기)
      "////
      "//////////////////////////////////////////////////////////////////
      READ TABLE xvbadr WITH KEY parvw ='WE'.
      IF sy-subrc EQ 0.
        lv_idx = sy-tabix.
        xvbadr-kunnr = lv_kunwe.
        SELECT SINGLE
               name1,
               land1
          INTO (@xvbadr-name1, @xvbadr-land1)
          FROM kna1
         WHERE kunnr EQ @lv_kunwe.
        MODIFY xvbadr INDEX lv_idx.
      ENDIF.
    ENDLOOP.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// 오리지널 판매처
    "////
    "/////////////////////////////////////////////////////////////////////
    LOOP AT idoc_data INTO ls_idoc_data WHERE segnam   = 'E1EDKT2'
                                          AND sdata(3) = 'A01'.
      TRANSLATE ls_idoc_data-sdata USING '* '.
      CLEAR: lv_fname, lv_value.
      SPLIT ls_idoc_data-sdata AT ':' INTO lv_fname lv_value.
      IF xvbak-vtweg EQ '11' OR xvbak-vtweg EQ '21'.
        lv_value = '2000'.
      ENDIF.
      CONDENSE lv_value NO-GAPS.
      lv_kunsx = lv_value.
      EXPORT lv_kunsx TO MEMORY ID 'ZKUNSX'.  "<= 판매처 전송
      EXIT.
    ENDLOOP.
  ELSEIF lv_ekorg = '10A2' OR lv_ekorg = '10A3'.  "DO
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// 납품처, 오리지널 판매처 결정
    "////
    "/////////////////////////////////////////////////////////////////////
*    SELECT SINGLE
*           zcmf01_ch,  "Ship-to
*           zcmf02_ch   "Original Sold-to
*      INTO (@lv_kunwe, @lv_kunsx)
*      FROM zcommt0021
*     WHERE spras     EQ @sy-langu
*       AND zmodule   EQ 'SD'
*       AND zclass    EQ 'SD008'
*       AND zcm_code1 EQ @xvbak-vkorg
*       AND zcm_code2 EQ @lv_ekorg.
*    EXPORT lv_kunwe TO MEMORY ID 'ZKUNWE'.  "<= 납품처 전송
*    EXPORT lv_kunsx TO MEMORY ID 'ZKUNSX'.  "<= 오리지널 판매처 전송
    DATA: lv_dwerks TYPE werks_d,  "Delivery plant
          lv_dlgort TYPE lgort_d.  "Delivery location
    CLEAR: lv_dwerks, lv_dlgort.
    LOOP AT idoc_data INTO ls_idoc_data WHERE segnam   = 'E1EDPT2'.
      TRANSLATE ls_idoc_data-sdata USING '* '.
      CLEAR: lv_fname, lv_value.
      SPLIT ls_idoc_data-sdata AT ':' INTO lv_fname lv_value.
      CONDENSE lv_value NO-GAPS.
      CASE ls_idoc_data-sdata(3).
        WHEN 'K01'.
          lv_dwerks = lv_value.
        WHEN 'K02'.
          lv_dlgort = lv_value.
      ENDCASE.
    ENDLOOP.
    SELECT SINGLE
           zcmf01_ch,  "Ship-to
           zcmf02_ch   "Original Sold-to
      INTO (@lv_kunwe, @lv_kunsx)
      FROM zcommt0021
     WHERE spras     EQ @sy-langu
       AND zmodule   EQ 'SD'
       AND zclass    EQ 'SD007'
       AND zcm_code1 EQ @lv_dwerks
       AND zcm_code2 EQ @lv_dlgort.
    EXPORT lv_kunwe TO MEMORY ID 'ZKUNWE'.  "<= 납품처 전송
    EXPORT lv_kunsx TO MEMORY ID 'ZKUNSX'.  "<= 오리지널 판매처 전송
    "//////////////////////////////////////////////////////////////////
    "////
    "//// SHIP-TO Party 바꾸기 (XVBADR 바꾸기)
    "////
    "//////////////////////////////////////////////////////////////////
    READ TABLE xvbadr WITH KEY parvw ='WE'.
    IF sy-subrc EQ 0.
      lv_idx = sy-tabix.
      xvbadr-kunnr = lv_kunwe.
      SELECT SINGLE
             name1,
             land1
        INTO (@xvbadr-name1, @xvbadr-land1)
        FROM kna1
       WHERE kunnr EQ @lv_kunwe.
      MODIFY xvbadr INDEX lv_idx.
    ENDIF.
  ELSEIF lv_ekorg = '10A4' OR lv_ekorg = '10A5'.  "Consignment DO
*    DATA: lv_dwerks TYPE werks_d,  "Delivery plant
*          lv_dlgort TYPE lgort_d.  "Delivery location
    CLEAR: lv_dwerks, lv_dlgort.
    LOOP AT idoc_data INTO ls_idoc_data WHERE segnam   = 'E1EDPT2'.
      TRANSLATE ls_idoc_data-sdata USING '* '.
      CLEAR: lv_fname, lv_value.
      SPLIT ls_idoc_data-sdata AT ':' INTO lv_fname lv_value.
      CONDENSE lv_value NO-GAPS.
      CASE ls_idoc_data-sdata(3).
        WHEN 'K01'.
          lv_dwerks = lv_value.
        WHEN 'K02'.
          lv_dlgort = lv_value.
      ENDCASE.
    ENDLOOP.
    SELECT SINGLE
           zcmf01_ch,  "Ship-to
           zcmf02_ch   "Original Sold-to
      INTO (@lv_kunwe, @lv_kunsx)
      FROM zcommt0021
     WHERE spras     EQ @sy-langu
       AND zmodule   EQ 'SD'
       AND zclass    EQ 'SD007'
       AND zcm_code1 EQ @lv_dwerks
       AND zcm_code2 EQ @lv_dlgort.
    EXPORT lv_kunwe TO MEMORY ID 'ZKUNWE'.  "<= 납품처 전송
    EXPORT lv_kunsx TO MEMORY ID 'ZKUNSX'.  "<= 오리지널 판매처 전송
*    "/////////////////////////////////////////////////////////////////////
*    "////
*    "//// 구매오더 Header Text의 변경이 없어 E1EDK02 세그먼트가 없는 경우
*    "////
*    "/////////////////////////////////////////////////////////////////////
*    IF lv_kunwe IS INITIAL AND lv_kunsx IS INITIAL.
*      DATA: lv_ebeln TYPE ebeln,
*            lv_vbeln TYPE vbeln,
*            lv_parvw TYPE vbpa-parvw,
*            lv_kunnr TYPE vbpa-kunnr.
*      LOOP AT idoc_data INTO ls_idoc_data WHERE segnam   = 'E1EDK01'.
*        lv_ebeln = ls_idoc_data-sdata+83(10).
*        SELECT SINGLE
*               vbeln
*          INTO @lv_vbeln
*          FROM vbak
*         WHERE bstnk EQ @lv_ebeln.
*        IF sy-subrc EQ 0.
*          SELECT parvw, kunnr
*            INTO (@lv_parvw, @lv_kunnr)
*            FROM vbpa
*           WHERE vbeln EQ @lv_vbeln.
*            IF lv_parvw EQ 'WE'.  "====================// Header Text에 있었던 납품처
*              lv_kunwe = lv_kunnr.
*              EXPORT lv_kunwe TO MEMORY ID 'ZKUNWE'.  "<= 납품처 전송
*              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                EXPORTING
*                  input  = lv_kunwe
*                IMPORTING
*                  output = lv_kunwe.
*              "//////////////////////////////////////////////////////////////////
*              "////
*              "//// SHIP-TO Party 바꾸기 (XVBADR 바꾸기)
*              "////
*              "//////////////////////////////////////////////////////////////////
*              READ TABLE xvbadr WITH KEY parvw ='WE'.
*              IF sy-subrc EQ 0.
*                lv_idx = sy-tabix.
*                xvbadr-kunnr = lv_kunwe.
*                SELECT SINGLE
*                       name1,
*                       land1
*                  INTO (@xvbadr-name1, @xvbadr-land1)
*                  FROM kna1
*                 WHERE kunnr EQ @lv_kunwe.
*                MODIFY xvbadr INDEX lv_idx.
*              ENDIF.
*            ENDIF.
*            IF lv_parvw EQ 'SX'.  "====================// Header Text에 있었던 오리지널 판매처
*              lv_kunsx = lv_kunnr.
*              EXPORT lv_kunsx TO MEMORY ID 'ZKUNSX'.  "<= 판매처 전송
*            ENDIF.
*          ENDSELECT.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
  ELSE.
    "
  ENDIF.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 구매오더 Header Text의 변경이 없어 E1EDK02 세그먼트가 없는 경우
  "////
  "/////////////////////////////////////////////////////////////////////
  IF lv_kunwe IS INITIAL AND lv_kunsx IS INITIAL.
    DATA: lv_ebeln TYPE ebeln,
          lv_vbeln TYPE vbeln,
          lv_parvw TYPE vbpa-parvw,
          lv_kunnr TYPE vbpa-kunnr.
    LOOP AT idoc_data INTO ls_idoc_data WHERE segnam   = 'E1EDK01'.
      lv_ebeln = ls_idoc_data-sdata+83(10).
      SELECT SINGLE
             vbeln
        INTO @lv_vbeln
        FROM vbak
       WHERE bstnk EQ @lv_ebeln.
      IF sy-subrc EQ 0.
        SELECT parvw, kunnr
          INTO (@lv_parvw, @lv_kunnr)
          FROM vbpa
         WHERE vbeln EQ @lv_vbeln.
          IF lv_parvw EQ 'WE'.  "====================// Header Text에 있었던 납품처
            lv_kunwe = lv_kunnr.
            EXPORT lv_kunwe TO MEMORY ID 'ZKUNWE'.  "<= 납품처 전송
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lv_kunwe
              IMPORTING
                output = lv_kunwe.
            "//////////////////////////////////////////////////////////////////
            "////
            "//// SHIP-TO Party 바꾸기 (XVBADR 바꾸기)
            "////
            "//////////////////////////////////////////////////////////////////
            READ TABLE xvbadr WITH KEY parvw ='WE'.
            IF sy-subrc EQ 0.
              lv_idx = sy-tabix.
              xvbadr-kunnr = lv_kunwe.
              SELECT SINGLE
                     name1,
                     land1
                INTO (@xvbadr-name1, @xvbadr-land1)
                FROM kna1
               WHERE kunnr EQ @lv_kunwe.
              MODIFY xvbadr INDEX lv_idx.
            ENDIF.
          ENDIF.
          IF lv_parvw EQ 'SX'.  "====================// Header Text에 있었던 오리지널 판매처
            lv_kunsx = lv_kunnr.
            EXPORT lv_kunsx TO MEMORY ID 'ZKUNSX'.  "<= 판매처 전송
          ENDIF.
        ENDSELECT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 1 Incoterms location 1
  "//// 2 Material
  "//// 3 Batch
  "////
  "/////////////////////////////////////////////////////////////////////
  "----// 1 incoterms location 1 변수
  DATA: lv_inco2_l TYPE inco2_l.     "incoterms location 1
  CLEAR lv_inco2_l.
  SELECT SINGLE
         inco2_l
    INTO lv_inco2_l
    FROM knvv
   WHERE kunnr EQ xvbak-kunnr
     AND vkorg EQ xvbak-vkorg
     AND vtweg EQ xvbak-vtweg
     AND spart EQ xvbak-spart.
  xvbak-inco2_l = lv_inco2_l.
  "----// 3 Batch 변수
  DATA: lv_matnr  TYPE bapibatchkey-material,
        lv_werks  TYPE bapibatchkey-plant,
        lv_batch  TYPE bapibatchkey-batch,
        lt_return TYPE TABLE OF bapiret2 WITH HEADER LINE,
*        lv_index  TYPE sy-tabix,
        lv_cnt    TYPE i,
        lv_tabix  TYPE sy-tabix.
  TYPES: BEGIN OF ty_batch,
           matnr TYPE mara-matnr,
           werks TYPE vbap-werks,
           batch TYPE mcha-charg,
         END OF ty_batch.
  DATA: ls_batch TYPE ty_batch,
        lt_batch TYPE SORTED TABLE OF ty_batch WITH UNIQUE KEY matnr werks batch,
        ls_mcha  TYPE mcha.
  LOOP AT xvbap.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// 1 Incoterms location 1
    "////
    "/////////////////////////////////////////////////////////////////////
    xvbap-inco2_l = lv_inco2_l.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// 2 Material - 자재정보 중 EAN/UPC 정보를 공란으로 - EAN/UPC를 자재코드로 인식해서 에러가 발생할 가능성 제거
    "////
    "/////////////////////////////////////////////////////////////////////
    IF xvbap-matnr = space AND xvbap-kdmat <> space.
      xvbap-matnr = xvbap-kdmat.
    ENDIF.
    xvbap-ean11 = space.
    "/////////////////////////////////////////////////////////////////////
    "////
    "//// 3 BATCH 미리 만들기
    "////
    "/////////////////////////////////////////////////////////////////////
    CLEAR: lv_matnr, lv_werks, lv_batch, lt_return, lt_return[], lv_tabix.
    "----// BATCH 번호 설정 ----------------------------
    CLEAR lt_item.
    lv_tabix = 1 + ( sy-tabix - 1 ) * 4.
    READ TABLE lt_item INDEX lv_tabix.
    CLEAR: lv_fname, lv_value.
    SPLIT lt_item-sdata AT ':' INTO lv_fname lv_value.
    IF sy-subrc NE 0 OR lv_value IS INITIAL.
      CONTINUE.
    ENDIF.
    TRANSLATE lv_value USING '* '.
    CONDENSE lv_value NO-GAPS.
    "---------------------------------------------------
*    lv_index = sy-tabix.
    lv_matnr = xvbap-matnr.
    lv_werks = '2011'.
    lv_batch = lv_value.
    lv_cnt = strlen( lv_value ).  "length check
    IF xvbak-auart = 'ZFTD'.
      IF lv_cnt > 8.
        CONCATENATE lv_value(8) '-P' INTO lv_batch.
      ELSE.
        CONCATENATE lv_value '-P' INTO lv_batch.
      ENDIF.
    ELSEIF vbak-auart = 'ZICW'.
      IF  lv_cnt > 10.
        lv_batch = lv_value(10).
      ENDIF.
    ENDIF.
    TRANSLATE lv_batch TO UPPER CASE.
    SELECT SINGLE
           *
      INTO ls_mcha
      FROM mcha
     WHERE matnr = lv_matnr
       AND werks = lv_werks
       AND charg = lv_batch.
    IF sy-subrc = 0.
      CLEAR ls_batch.
      READ TABLE lt_batch INTO ls_batch WITH KEY matnr = lv_matnr
                                                 werks = lv_werks
                                                 batch = lv_batch BINARY SEARCH. "Dubp checkk.
      IF sy-subrc NE 0.
        ls_batch-batch = lv_batch.
        ls_batch-werks = lv_werks.
        ls_batch-matnr = lv_matnr.
        INSERT ls_batch INTO TABLE lt_batch.
      ENDIF.
    ENDIF.
    CLEAR ls_batch.
    READ TABLE lt_batch INTO ls_batch WITH KEY matnr = lv_matnr
                                               werks = lv_werks
                                               batch = lv_batch BINARY SEARCH. "Dubp checkk.

    IF sy-subrc = 0.
      "
    ELSE.
      CALL FUNCTION 'BAPI_BATCH_CREATE'
        EXPORTING
          material = lv_matnr
          batch    = lv_batch
          plant    = lv_werks
        IMPORTING
          batch    = lv_batch
        TABLES
          return   = lt_return.
    ENDIF.
*    xvbap-charg = lv_batch.
    MODIFY xvbap.
    CLEAR xvbap.
  ENDLOOP.
*  "/////////////////////////////////////////////////////////////////////
*  "////
*  "//// 자재정보 중 EAN/UPC 정보를 공란으로 - EAN/UPC를 자재코드로 인식해서 에러가 발생할 가능성 제거
*  "////
*  "/////////////////////////////////////////////////////////////////////
*  LOOP AT xvbap.
*    IF xvbap-matnr = space AND xvbap-kdmat <> space.
*      xvbap-matnr = xvbap-kdmat.
*    ENDIF.
*    xvbap-ean11 = space.
*    MODIFY xvbap.
*  ENDLOOP.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 납품처 상세 주소 화면 생략
  "////
  "/////////////////////////////////////////////////////////////////////
  CLEAR d_flag_k-psdewe.
  "/////////////////////////////////////////////////////////////////////
  "////
  "//// 표준 IDOC ORDERS ORDC 프로세스는 그대로 유지 ★★★★★
  "////
  "/////////////////////////////////////////////////////////////////////
*- Einstiegsbild einspielen -------------------------------------------*
*- call transaction first dynpro --------------------------------------*
  PERFORM dynpro_einstieg.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Übersicht einzeilig einspielen -------------------------------------*
*- call transaction onee-line entry -----------------------------------*
  PERFORM va02_dynpro_uebersicht2.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Kopf Partnerdaten einspielen ---------------------------------------*
*- call transaction header partner data -------------------------------*
  PERFORM va02_dynpro_kopf_partner.
*  Dynpro nocheinmal senden, da Warnungen Funktionscode zuruecksetzen
*  sent dynpro again because warnings cancelled function code
  IF last_dynpro = dynpro-kpar_sub.
    PERFORM dynpros_ermitteln USING    programm_auftrag_new
                                       bldgr_kopf
                                       panel-kpas
                              CHANGING dynpro-kpar_sub.
    PERFORM dynpro_new USING programm_auftrag
                             dynpro-kpar_sub
                    CHANGING last_dynpro.
  ENDIF.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Kopf Kaufmann einspielen -------------------------------------------*
*- call transaction header business data ------------------------------*
  PERFORM va02_dynpro_kopf_kaufmann.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Kopf Kaufmann Versand einspielen -----------------------------------*
*- call transaction header business shipping data ---------------------*
  PERFORM va02_dynpro_kopf_kaufmann_kde2.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Kopf Kaufmann Faktura einspielen -----------------------------------*
*- call transaction header business invoice  data ---------------------*
  PERFORM va02_dynpro_kopf_kaufmann_kde3.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Kopf Bestelldaten einspielen ---------------------------------------*
*- call transaction header purchase order data ------------------------*
  PERFORM va02_dynpro_kopf_bestell.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Kopf Zusatzdaten einspielen ----------------------------------------*
*- call transaction header additional data ----------------------------*
  PERFORM va02_dynpro_kopf_kgru.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Kopf Konditionen einspielen ----------------------------------------*
*- call transaction header conditions ---------------------------------*
  PERFORM va02_dynpro_kopf_kondition.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Positionsdaten einspielen ------------------------------------------*
*- call transaction item datas ----------------------------------------*
  PERFORM va02_dynpro_position.
*-Zusätzliche Daten für Dynpros vorsehen (Kundenerweiterungen)---------*
*-additional data's for new dynpro fields (customer exit)--------------*
  sy-subrc = 0.
  PERFORM customer_function_dynpro.
*- Verarbeitungsmodus ermitteln ---------------------------------------*
*- determine input method ---------------------------------------------*
  IF input_method = 'X'.
    EXPORT bdcdata TO MEMORY ID 'idoc_test_bdcdata'.
  ELSE.
    IF input_method IS INITIAL.
      input_method = 'N'.
    ENDIF.
    CALL TRANSACTION 'VA02' USING bdcdata
                            MODE  input_method
                            UPDATE 'S'
                            MESSAGES INTO xbdcmsgcoll.

    retcode = sy-subrc.
    PERFORM check_ordchg_posted USING retcode
                               CHANGING rc.
  ENDIF.
ENDFORM.
