function zsd_asn_save.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(E_MSG) TYPE  STRING
*"  TABLES
*"      IT_DATA STRUCTURE  ZMM1T5020
*"----------------------------------------------------------------------
  data: lv_err(1). "Error indicator

  data: lt_5020 like zmm1t5020 occurs 0 with header line,
        lt_del  like zmm1t5020 occurs 0 with header line,
        lt_po   like gs_zmm1t5020  occurs 0 with header line,
        lt_no   like gs_zmm1t5020  occurs 0 with header line.

  data : begin of lt_item occurs 0,
           ponum like zmm1t5020-ponum,        "  CUS PO
           matno like zmm1t5020-matno,        "  SKU
           poitm like zmm1t5020-poitm,
         end of lt_item.

*  DATA: BEGIN OF LT_BL OCCURS 0,
*          BLNUM LIKE ZMM1T5020-BLNUM,
*        END OF LT_BL.

*** 변경 2020.02.29 BY ZEN08
  data : lt_bl like zmm1t5020 occurs 0 with header line.
***---------------------------------------------------------
  "추가 시작.
  data ls_check like zmm1t5020.
  data lv_vbeln like vbkd-bstkd.
  data lv_idx   like sy-tabix.
  data lv_lifnr like zmm1t5020-lifnr.
  data lv_shptocode like zmm1t5020-shptocode.
  data : lv_ebeln_r like ekko-ebeln,
         lv_ponum_c like zmm1t5020-ponum_c.

  data: begin of lt_temp occurs 0,
          bsart type ekko-bsart,
          ebeln like ekpo-ebeln,
          ebelp like ekpo-ebelp,
          werks like ekpo-werks,
          vbeln like vbap-vbeln,
          posnr like vbap-posnr,
          matnr like ekpo-matnr,
          kdmat like ekpo-matnr,
        end of lt_temp.

  data : begin of lt_vbap occurs 0,
           vbeln like vbap-vbeln,
           posnr like vbap-posnr,
           kdmat like vbap-kdmat,
         end of lt_vbap.
  "추가 종료.
***---------------------------------------------------------

  data: lv_ebeln like ekko-ebeln,
        lv_bsart like ekko-bsart,
        lv_poitm like ekpo-ebelp,
        lv_tabix like sy-tabix.


  if it_data[] is initial.
    e_msg = text-009.
    exit.
  endif.

  clear: lv_tabix, e_msg, gs_zmm1t5020.


  loop at it_data.
    perform check_data changing it_data e_msg.

    check e_msg is initial.

    lv_tabix = sy-tabix.
    clear : ls_check.
    clear : lv_ponum_c.
    clear : lt_temp.

    "Customer SKU 공란일 경우 채워넣는다.
    if it_data-matno_c eq ' '.
      it_data-matno_c = it_data-matno.
      modify it_data index lv_tabix transporting matno_c.
    endif.

    lv_ponum_c = it_data-ponum.



* Customer PO 번호가 US PO 가 아니더라도 다시한번 Amazon DI의 주문번호(PO) 인지 확인

      clear: lv_vbeln, ls_check.
      select single bstkd ihrez
      into   ( lv_vbeln, ls_check-uspo )      "HQ PO / US PO
      from vbkd
      where bstkd_e eq it_data-ponum
          and posnr = '000000'.

    if ls_check-uspo ne ' '.

      "라인별 검색용으로 추가.
      clear : lv_ebeln_r.
      select single ebeln
        into lv_ebeln_r
        from ekpo
        where ebeln eq lv_vbeln.

* US Po 번호를 기준으로 US PO번호 , Item 번호와 Amazon DI의 경우 해당 S/O 번호와 S/O Item 번호를 찾음
      select ep~ebeln ep~ebelp ep~werks ep~matnr ek~vbeln ek~vbelp
      into ( lt_temp-ebeln, lt_temp-ebelp, lt_temp-werks, lt_temp-matnr, lt_temp-vbeln, lt_temp-posnr )
      from ekpo as ep left outer join ekkn as ek
        on ep~ebeln eq ek~ebeln
       and ep~ebelp eq ek~ebelp
      where ep~ebeln eq lv_vbeln.

        lt_temp-bsart = ls_check-bsart.
        append lt_temp. clear lt_temp.
      endselect.


* Amazon DI의 S/O에 있는 Customer Material을 찾아냄
      if lv_vbeln ne ' '.
        select vbeln posnr kdmat
        into corresponding fields of table lt_vbap
        from vbap
        where vbeln eq lv_vbeln.
      endif.

* 위에서 찾아낸 Customer Material을 인터널 테이블 LT_temp에 등록함
      loop at lt_temp where vbeln ne ' '.
        clear: lv_idx.
        lv_idx = sy-tabix.

* Amazon DI의 Set상품인 경우 모품목 Item 번호를 지정하여 Customer Material을 찾음
** EX: 모품목은 S/O Item 번호: 000010,  해당 모품목의 자식 품목은 S/O Item 번호: 000011
        if lt_temp-posnr+5(1) ne '0'.
          concatenate lt_temp-posnr+0(5) '0' into lt_temp-posnr.
        endif.

        read table lt_vbap with key vbeln = lt_temp-vbeln posnr = lt_temp-posnr.
        if sy-subrc eq 0.
          lt_temp-kdmat = lt_vbap-kdmat.
          modify lt_temp index lv_idx transporting kdmat.
        endif.
      endloop.

* US PO Item의 SKU 기준으로 ASN에 있는 SKU와 비교하여 PO Item 번호를 찾음
*      READ TABLE lt_temp WITH KEY matnr = it_data-matno.
      read table lt_temp with key ebeln = lv_ebeln_r
                                  matnr = it_data-matno.


      clear it_data-poitm.
      if sy-subrc eq 0.
        it_data-ponum = lt_temp-ebeln.
        it_data-poitm = lt_temp-ebelp.
        it_data-bsart = lt_temp-bsart.
        it_data-uspo  = ls_check-uspo.
        it_data-matno = lt_temp-matnr.
        it_data-whcd  = lt_temp-werks.
        it_data-ponum_c = lv_ponum_c.
      else.
* 찾지 못하는 경우에는 Customer Material 과  ASN에 있는 SKU와 비교하여 PO Item 번호를 찾음
*        READ TABLE lt_temp WITH KEY kdmat = it_data-matno.
        read table lt_temp with key ebeln = lv_ebeln_r
                                    kdmat = it_data-matno.

        if sy-subrc eq 0.
          it_data-ponum = lt_temp-ebeln.
          it_data-poitm = lt_temp-ebelp.
          it_data-bsart = lt_temp-bsart.
          it_data-uspo  = ls_check-uspo.
          it_data-matno = lt_temp-matnr.
          it_data-whcd = lt_temp-werks.
          it_data-werks = lt_temp-werks.
          it_data-ponum_c = lv_ponum_c.
        endif.
      endif.

* PO No, Item, US PO & type, asn sku, Wherehouse 변환
      if it_data-poitm ne ' '.
        modify it_data index lv_tabix transporting ponum poitm bsart uspo matno whcd werks ponum_c.
        move-corresponding it_data to lt_5020.
        append lt_5020.
      endif.

    else.
* 상기 로직을 통해 PO를 찾지 못한경우
      move-corresponding it_data to lt_no.
      lt_no-ponum_c = lv_ponum_c.
      append lt_no.
    endif.
  endloop.


*** uspo 찾지 못한 라인만 별도 진행.
  loop at lt_no.
    move-corresponding lt_no to lt_item.
    append lt_item.
  endloop.

  clear lt_item. sort lt_item by ponum matno.
  delete adjacent duplicates from lt_item comparing all fields.
  clear lv_tabix.


*** uspo 미존재 DATA : POITEM 부여.
  sort lt_item by ponum.
  loop at lt_item.
    lv_tabix = sy-tabix.
    at new ponum.
      clear lv_poitm.
    endat.
* PONUM의 SKU 기준으로 정렬하여 10씩 증가
    add 10 to lv_poitm.
    lt_item-poitm = lv_poitm.

    modify lt_item index lv_tabix transporting poitm.

  endloop.

  clear lt_item. sort lt_item by ponum matno.

  loop at lt_no.
    move-corresponding lt_no to lt_5020.
    lt_5020-ponum_c = lt_no-ponum. " PONUM_C

    read table lt_item with key ponum = lt_no-ponum
                                matno = lt_no-matno.
*                              BINARY SEARCH.
    if sy-subrc = 0.
      lt_5020-poitm = lt_item-poitm.
    endif.
    append lt_5020.
  endloop.


*** uspo 존재 & 미존재 데이터 SAVE용 테이블에 취합 이후 LOGIC
  clear lt_5020. sort lt_5020 by conno blnum ponum matno.
*** Sold to / Ship to / Bill to 설정.
  clear lv_tabix.
  loop at lt_5020.
    lv_tabix = sy-tabix.

** Excel upload 시 통화단위, 수량단위 세팅.
    lt_5020-waers = 'USD'.
    lt_5020-meins = 'EA'.

** SKU Description 공백일 경우 채우기
    if lt_5020-matdc eq space.
      select single maktx
        into lt_5020-matdc
        from makt
       where matnr eq lt_5020-matno
         and spras eq sy-langu.
    endif.

** UPLOAD 필드의 VEND를 LIFNR.
    lt_5020-lifnr = lt_5020-vend.

    clear lv_shptocode.
    lv_shptocode = lt_5020-shptocode.
    lv_shptocode = |{ lv_shptocode alpha = out }|.

*** 1 SOLD TO 설정
    "공백 0 제거 후 SELECT 진행.
    clear lv_lifnr.
    select single partner
      into lv_lifnr
      from but000
      where title_let eq lv_shptocode.

    " 발췌 실패시 본 데이터로 진행
    if sy-subrc ne 0.
      select single partner
        into lv_lifnr
        from but000
        where title_let eq lt_5020-shptocode.
    endif.

    if lv_lifnr ne space.
      "SOLD TO 설정
      select single kunnr
        into lt_5020-soldto
        from knvp
        where kunn2 eq lv_lifnr
          and vkorg eq '9900'
          and vtweg eq '10'
          and spart eq '00'
          and parvw eq 'WE'.

*** 2 SHIP TO 설정
      lt_5020-shipto = lv_lifnr.
    endif.

*** 3 BILL TO 설정
    select single kunn2
      into lt_5020-billto
      from knvp
      where kunnr eq lt_5020-soldto
      and   vkorg eq '9900'
      and   vtweg eq '10'
      and   spart eq '00'
      and   parvw eq 'RE'.

*** 직접 입력한 EXCEL UPLOAD의 OCS ORDER NO, LINE 은 DATE, TIIME으로 한다.
*** 2019.12.09
    lt_5020-cnordno = sy-datum.
    lt_5020-cnordln = sy-uzeit.

*    MODIFY lt_5020 INDEX lv_tabix TRANSPORTING soldto shipto billto.

* 대만, 이탈리아에 한해서 UPLOAD 시 SKU 를 FCODE에 대입한다. 2019.11.18
    if lt_5020-vend = '40020' or
       lt_5020-vend = '40021' or
       lt_5020-vend = '0000040020' or
       lt_5020-vend = '0000040021'.

      lt_5020-fcode = lt_5020-matno.
      lt_5020-zernam = sy-uname. "추가 2019.11.18
      lt_5020-zerdat = sy-datum. "추가 2019.11.18
      lt_5020-nechf  = 'M'.      "추가 2019.11.19 EXCEL UPLOAD FLAG.
      modify lt_5020 index lv_tabix
                     transporting lifnr soldto shipto billto fcode zernam
                                  zerdat nechf cnordno cnordln waers meins matdc.
      move-corresponding lt_5020 to lt_bl.
      append lt_bl.
    else.
      lt_5020-zernam = sy-uname. "추가 2019.11.18
      lt_5020-zerdat = sy-datum. "추가 2019.11.18
      lt_5020-nechf  = 'M'.      "추가 2019.11.19 EXCEL UPLOAD FLAG.
      modify lt_5020 index lv_tabix
                     transporting lifnr soldto shipto billto zernam
                                  zerdat nechf cnordno cnordln waers meins matdc.
      move-corresponding lt_5020 to lt_bl.
      append lt_bl.
    endif.
  endloop.

  clear lt_bl. sort lt_bl by blnum.
  delete adjacent duplicates from lt_bl comparing all fields.


  if lt_bl[] is not initial.
    select *
      into corresponding fields of table lt_del
      from zmm1t5020 for all entries in lt_bl
     where blnum  eq lt_bl-blnum
       and kcoo   eq lt_bl-kcoo
       and dcto   eq lt_bl-dcto
       and matno  eq lt_bl-matno
       and lifnr  eq lt_bl-lifnr
       and soldto eq lt_bl-soldto
       and shipto eq lt_bl-shipto
       and billto eq lt_bl-billto
       and zconfirm eq abap_off.
  endif.

  if lt_del[] is not initial.
    delete zmm1t5020 from table lt_del.
    if sy-subrc = 0.
      insert zmm1t5020 from table lt_5020 accepting duplicate keys.
      if sy-subrc = 0.
        commit work and wait.
*        message i000(zmcsd1) with text-007.
        e_msg = text-007.
      else.
        rollback work.
*        message i000(zmcsd1) with text-008.
        e_msg = text-008.
      endif.
    else.
      rollback work.
*      message i000(zmcsd1) with text-008.
      e_msg = text-008.
    endif.
  else.
    insert zmm1t5020 from table lt_5020 accepting duplicate keys.
    if sy-subrc = 0.
      commit work and wait.
*      message i000(zmcsd1) with text-007.
      e_msg = text-007.
    else.
      rollback work.
*      message i000(zmcsd1) with text-008.
      e_msg = text-008.
    endif.
  endif.




endfunction.
