*&---------------------------------------------------------------------*
*& Include          ZSDR0051_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form CREATE_INVOICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form create_invoice .

  data: lv_year       like inri-toyear,
        lv_number(10).


  gv_api_doc_id = |{ p_budat }{ sy-timlo }|.

  clear : gs_likp.                                                                "Get Delivery info.
  select single a~vstel , a~kunag,  a~grulg, b~ladgr, a~vsbed, a~tragr
    into @gs_likp
    from likp as a
    inner join lips as b
               on a~vbeln = b~vbeln
    where a~vbeln = @p_vbeln.


*E00062 03/01/2021
  " Header Data-----------------------------------------
  gt_head-invno_im = |ZIVN{ p_vbeln }|.
  gt_head-bldat = gt_head-budat = gt_head-onbdt = p_budat.
  gt_head-bukrs = |{ gs_likp-kunag alpha = out }|.
  gt_head-zterm = 'T090'.
  gt_head-waers = 'USD'.
  gt_head-lifnr =  gs_likp-vstel.
  append gt_head.

*    Item Data ------------------------------------
  gs_comwa-vbeln = p_vbeln.
  call function 'RV_ORDER_FLOW_INFORMATION'
    exporting
      comwa         = gs_comwa
    tables
      vbfa_tab      = gt_vbfa_tab
    exceptions
      no_vbfa       = 1
      no_vbuk_found = 2
      others        = 3.

  .
  if gt_vbfa_tab is not initial.

    read table gt_vbfa_tab with key vbelv = p_vbeln vbtyp_n = '8'.     "Shipment Number
    if sy-subrc = 0.
      gv_shipment = gt_vbfa_tab-vbeln.
    else.
      message s499 with text-t02 display like 'E'.
      exit.
    endif.

    read table gt_vbfa_tab with key  vbtyp_n = 'C'.   "Sales Order
    if sy-subrc = 0.
      gv_salesorder = gt_vbfa_tab-vbeln.
    else.
      message s499 with text-t03 display like 'E'.
      exit.
    endif.

    clear: gv_ponum.
    if gv_salesorder is not initial.        "P/O Number
      select single bstkd, bstkd_e, ihrez
        into ( @gv_ponum, @gv_ponum_c, @gv_uspo )
        from vbkd
        where vbeln = @gv_salesorder
           and posnr = '000000'.
    endif.
  else.
    message s499 with text-t04 display like 'E'.
    exit.

  endif.

  loop at gt_vbfa_tab where vbeln = p_vbeln and vbtyp_n = 'J' and posnn ne '000000'.   "Info Deleivery
    gt_delivery-vbeln = gt_vbfa_tab-vbeln.
    gt_delivery-posnr = gt_vbfa_tab-posnv.      ""선행문서  Item line No.
    gt_delivery-lfimg = gt_vbfa_tab-rfmng.

    select single matnr, netpr, kwmeng
      from vbap
      into ( @gt_delivery-matnr, @gt_delivery-netpr, @gt_delivery-kwmeng )
      where vbeln = @gv_salesorder
           and posnr = @gt_delivery-posnr.
    append gt_delivery.
  endloop.






  loop at gt_delivery.
    gt_item-invno_im = gt_head-invno_im.
    gt_item-ebeln = gv_ponum.
    gt_item-ebelp = gt_delivery-posnr.
    gt_item-matnr = gt_delivery-matnr.
    gt_item-menge = gt_delivery-lfimg.
    gt_item-wrbtr = gt_delivery-lfimg * gt_delivery-netpr.
    append gt_item.
  endloop.


  call function 'ZMM_IF_IMPORT_INVOICE'
    exporting
      api_doc_id = gv_api_doc_id
*     CANCEL     =
      xtrade     = 'X'
    tables
      gt_head    = gt_head
      gt_item    = gt_item
      gt_return  = gt_return.

  if gt_return[] is not initial.

    loop at gt_return where zsap_rtype = 'E'.
      message s499 with gt_return-zsap_rmsg display like 'E'.
    endloop.

    if sy-subrc ne 0.
      message s499 with text-t01.
      read table gt_return index 1.
      gv_invno = gt_return-invno_im.
    else.
      exit.
    endif.
*    read table gt_return with key zsap_rtype = 'E'.
*    if sy-subrc = 0 .
*      message s499 with gt_return-zsap_rmsg display like 'E'.
*      exit.
*    else.
*
*    endif.
*  else.
*    message s499 with text-t01 display like 'E'.
*    exit.
  endif.

  wait up to 2 seconds.

* BL Container Header --------------------------------------------
  gt_head_bl-lifnr = gs_likp-vstel.
  gt_head_bl-bldat = gt_head_bl-budat = gt_head_bl-onbdt = p_budat.
  gt_head_bl-frefw = '80000'.
  gt_head_bl-frefw = |{ gt_head_bl-frefw  alpha = in }|.
  gt_head_bl-bukrs = |{ gs_likp-kunag alpha = out }|.

  select single exti1
    into @gt_head_bl-bldoc
   from vttk
    where tknum = @gv_shipment.
  append gt_head_bl.


* BL Container Item --------------------------------------------
  lv_year  = p_budat+0(4).




  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = '01'
      object                  = 'ZIVN'
      quantity                = '1'
      subobject               = lv_year
      toyear                  = lv_year
*     IGNORE_BUFFER           = ' '
    importing
      number                  = lv_number
*     QUANTITY                =
*     RETURNCODE              =
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.



  clear: gv_zseq.
  loop at gt_delivery.
    gv_zseq = gv_zseq + 1.
    gt_item_bl-zseq = gv_zseq.
    gt_item_bl-lifnr = gt_head_bl-lifnr.
    gt_item_bl-bldoc = gt_head_bl-bldoc.
    gt_item_bl-invno_im = gt_return-invno_im.
    gt_item_bl-bldoc = gt_head_bl-bldoc.
    gt_item_bl-conno = gv_shipment.
    gt_item_bl-bstkd = gv_ponum.
    gt_item_bl-menge = gt_delivery-lfimg.
    gt_item_bl-matnr = gt_delivery-matnr.
    gt_item_bl-ebeln = gv_ponum.
    gt_item_bl-ebelp = gt_delivery-posnr.
    gv_master_ivno = gt_item_bl-invno_ex = |ZIUS{ lv_year+2(2) }-{ lv_number+6(4) }|.
    append gt_item_bl.
*    endif.
  endloop.


  call function 'ZMM_IF_BL_CONTAINER'
    exporting
      api_doc_id = gv_api_doc_id
*     CANCEL     =
      xtrade     = 'Y'
      zim_ex     = 'A'
    tables
      gt_head    = gt_head_bl
      gt_item    = gt_item_bl
      gt_return  = gt_return_bl.

  if gt_return_bl[] is not initial.
    loop at gt_return_bl where zsap_rtype = 'E'.
      message s499 with gt_return_bl-zsap_rmsg display like 'E'.
    endloop.
    if sy-subrc ne 0.
*  ASN Data -----------------------------------------------
      perform asn_data_save.
    endif.
*  else.
    message s499 with text-t01.
  endif.



endform.
*&---------------------------------------------------------------------*
*& Form CANCEL_INVOICE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form cancel_invoice .
  data: lv_year       like inri-toyear,
        lv_number(10).

  clear : gs_likp.                                                                "Get Delivery info.
  select single a~vstel , a~kunag,  a~grulg, b~ladgr, a~vsbed, a~tragr
    into @gs_likp
    from likp as a
    inner join lips as b
               on a~vbeln = b~vbeln
    where a~vbeln = @p_vbeln.

  gv_api_doc_id = |{ p_budat }{ sy-timlo }|.
  gv_invno_im  = |ZIVN{ p_vbeln }|.

* S/O Info ---------------------------------------------------
  gs_comwa-vbeln = p_vbeln.
  call function 'RV_ORDER_FLOW_INFORMATION'
    exporting
      comwa         = gs_comwa
    tables
      vbfa_tab      = gt_vbfa_tab
    exceptions
      no_vbfa       = 1
      no_vbuk_found = 2
      others        = 3.

  if gt_vbfa_tab is not initial.

    read table gt_vbfa_tab with key vbelv = p_vbeln vbtyp_n = '8'.     "Shipment Number
    if sy-subrc = 0.
      gv_shipment = gt_vbfa_tab-vbeln.
    else.
      message s499 with text-t02 display like 'E'.
      exit.
    endif.

    read table gt_vbfa_tab with key  vbtyp_n = 'C'.   "Sales Order
    if sy-subrc = 0.
      gv_salesorder = gt_vbfa_tab-vbeln.
    else.
      message s499 with text-t03 display like 'E'.
      exit.
    endif.

    clear: gv_ponum.
    if gv_salesorder is not initial.        "P/O Number
      select single bstkd, ihrez
        into ( @gv_ponum, @gv_uspo )
        from vbkd
        where vbeln = @gv_salesorder
           and posnr = '000000'.
    endif.
  else.
    message s499 with text-t04 display like 'E'.
    exit.
  endif.






  loop at gt_vbfa_tab where vbeln = p_vbeln and vbtyp_n = 'J' and posnn ne '000000'.   "Info Deleivery



*    if gv_invno_im is initial.                                 "개별 Delivery 이므로 한 번 만 조회
*      select single kp~bktxt
*        into @gv_invno_im
*        from rbkp as kp
*        inner join rseg as eg
*                  on kp~belnr = eg~belnr
*                  and kp~gjahr = eg~gjahr
*        where eg~ebeln = @gv_ponum
*            and eg~ebelp = @gt_vbfa_tab-posnv
*            and kp~stblg = @space.
*    endif.

    gt_delivery-vbeln = gt_vbfa_tab-vbeln.
    gt_delivery-posnr = gt_vbfa_tab-posnv.      ""선행문서  Item line No.
    gt_delivery-lfimg = gt_vbfa_tab-rfmng.
    gt_delivery-invno_im = gv_invno_im.


    select single matnr, netpr, kwmeng
      from vbap
      into ( @gt_delivery-matnr, @gt_delivery-netpr, @gt_delivery-kwmeng )
      where vbeln = @gv_salesorder
           and posnr = @gt_delivery-posnr.

    append gt_delivery.
  endloop.

* BL Container Header --------------------------------------------
  gt_head_bl-lifnr = gs_likp-vstel.
  gt_head_bl-bldat = gt_head_bl-budat = gt_head_bl-onbdt = p_budat.
  gt_head_bl-frefw = '80000'.
  gt_head_bl-frefw = |{ gt_head_bl-frefw  alpha = in }|.
  gt_head_bl-bukrs = |{ gs_likp-kunag alpha = out }|.
  select single exti1
    into @gt_head_bl-bldoc
   from vttk
    where tknum = @gv_shipment.
  append gt_head_bl.


  lv_year = p_budat+0(4).


  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = '01'
      object                  = 'ZIVN'
      quantity                = '1'
      subobject               = lv_year
      toyear                  = lv_year
*     IGNORE_BUFFER           = ' '
    importing
      number                  = lv_number
*     QUANTITY                =
*     RETURNCODE              =
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.





* BL Container Item --------------------------------------------
  clear: gv_zseq.
  loop at gt_delivery.
    gv_zseq = gv_zseq + 1.
    gt_item_bl-zseq = gv_zseq.
    gt_item_bl-lifnr = gt_head_bl-lifnr.
    gt_item_bl-bldoc = gt_head_bl-bldoc.
    gt_item_bl-invno_im = gt_delivery-invno_im.
    gt_item_bl-bldoc = gt_head_bl-bldoc.
    gt_item_bl-conno = gv_shipment.
    gt_item_bl-bstkd = gv_ponum.
    gt_item_bl-menge = gt_delivery-lfimg.
    gt_item_bl-matnr = gt_delivery-matnr.
    gt_item_bl-ebeln = gv_ponum.
    gt_item_bl-ebelp = gt_delivery-posnr.
    gv_master_ivno = gt_item_bl-invno_ex = |ZIUS{ lv_year+2(2) }-{ lv_number+6(4) }|.
    append gt_item_bl.
*    endif.
  endloop.


  call function 'ZMM_IF_BL_CONTAINER'
    exporting
      api_doc_id = gv_api_doc_id
      cancel     = 'X'
      xtrade     = 'Y'
      zim_ex     = 'A'
    tables
      gt_head    = gt_head_bl
      gt_item    = gt_item_bl
      gt_return  = gt_return_bl.

  if gt_return_bl[] is not initial.
*    read table gt_return_bl with key zsap_rtype = 'E'.
*    if sy-subrc = 0 .
    loop at gt_return_bl where zsap_rtype = 'E'.
      message s499 with gt_return_bl-zsap_rmsg display like 'E'.
    endloop.
*    else.
    if sy-subrc ne 0.
      message s499 with text-t01.
    else.
      exit.
    endif.
*    endif.
*  else.
*    message S499 with text-t02 display like 'E'.
*    exit.
  endif.
  wait up to 2 seconds.

*E00062 03/01/2021
  " Header Data-----------------------------------------
  gt_head-invno_im = gv_invno_im.
  gt_head-bldat = gt_head-budat = gt_head-onbdt = p_budat.
  gt_head-bukrs = |{ gs_likp-kunag alpha = out }|.
  gt_head-zterm = 'T090'.
  gt_head-waers = 'USD'.
  gt_head-lifnr = gs_likp-vstel.
  append gt_head.

*    Item Data ------------------------------------
  loop at gt_delivery.
    gt_item-invno_im = gt_head-invno_im.
    gt_item-ebeln = gv_ponum.
    gt_item-ebelp = gt_delivery-posnr.
    gt_item-matnr = gt_delivery-matnr.
    gt_item-menge = gt_delivery-lfimg.
    gt_item-wrbtr = gt_delivery-lfimg * gt_delivery-netpr.
    append gt_item.
  endloop.


  call function 'ZMM_IF_IMPORT_INVOICE'
    exporting
      api_doc_id = gv_api_doc_id
      cancel     = 'X'
      xtrade     = 'X'
    tables
      gt_head    = gt_head
      gt_item    = gt_item
      gt_return  = gt_return.

  if gt_return[] is not initial.
*    read table gt_return with key zsap_rtype = 'E'.
*    if sy-subrc = 0 .
    loop at gt_return where zsap_rtype = 'E'.
      message s499 with gt_return-zsap_rmsg display like 'E'.
    endloop.


*    else.
    if sy-subrc ne 0.
      perform asn_data_save.
    else.
      exit.
    endif.

*    endif.
*  else.
*    message s499 with text-t01 display like 'E'.
    exit.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form ENQUEUE_DOCUMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form enqueue_document .
  data: gv_error type string.

  select single bstkd
    into @gv_order
    from vbfa  as a inner join vbkd as b
    on  a~vbelv = b~vbeln
    and a~posnv = b~posnr
    where a~vbtyp_n = 'J'
        and  a~vbeln = @p_vbeln.


  call function 'MM_ENQUEUE_DOCUMENT'
    exporting
      i_ebeln         = gv_order
      i_ebelp         = '00000'
      i_bstyp         = 'F'
      i_mode          = 'E'
      i_scope         = '2'
    exceptions
      document_locked = 1.

  if sy-subrc <> 0.
    call function 'MESSAGE_TEXT_BUILD'
      exporting
        msgid               = sy-msgid
        msgnr               = sy-msgno
        msgv1               = sy-msgv1
        msgv2               = sy-msgv2
        msgv3               = sy-msgv3
        msgv4               = sy-msgv4
      importing
        message_text_output = gv_error.

    message s499 with gv_error display like 'E'.
    leave to list-processing.
    leave list-processing.
  endif.



endform.
*&---------------------------------------------------------------------*
*& Form DEQUEUE_DOCUMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form dequeue_document .



  call function 'DEQUEUE_EMEKKOE'
    exporting
      mode_ekko = 'E'
      mode_ekpo = 'E'
      mandt     = sy-mandt
      ebeln     = gv_order
      ebelp     = '00000'.
*   X_EBELN         = ' '
*   X_EBELP         = ' '
*   _SCOPE          = '3'
*   _SYNCHRON       = ' '
*   _COLLECT        = ' '
  .
endform.
*&---------------------------------------------------------------------*
*& Form ASN_DATA_SAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form asn_data_save .

  gv_mode = cond #( when p_can eq 'X' then 'X'
                                  else space ).

  select single route, dpreg, tdlnr
                into ( @gv_route, @gv_planned, @gv_frefw )
             from vttk
             where tknum = @gv_shipment.



  gv_start = gv_date = p_budat.


  gs_options-transittime = gs_options-forwards_auto = 'X'.


  gv_route_date = gv_route+4(2).

  call function 'RP_CALC_DATE_IN_INTERVAL'
    exporting
      date      = gv_start
      days      = gv_route_date
      months    = '00'
      signum    = '+'
      years     = '00'
    importing
      calc_date = gv_date.


  while 1 = 1.                  "Get Arrival Date



    call function 'SD_SCHEDULING'
      exporting
        if_schedule_direction  = '-'
        if_schedule_shipping   = 'X'
        if_schedule_transport  = 'X'
        if_shipping_point      = gs_likp-vstel
        if_date_today          = gv_init_date
        if_weight_group        = gs_likp-grulg
        if_loading_group       = gs_likp-ladgr
        if_transport_route     = gv_route
        is_options             = gs_options
        if_delivery_date       = gv_date
        if_shipping_conditions = gs_likp-vsbed
        if_transport_group     = gs_likp-tragr
        if_rs_document_sw      = 'X'
        if_webaz               = 0
      importing
        ef_goods_issue_date    = gv_end.

    if gv_end >=  p_budat.
      gv_end = gv_date.
      exit.
    endif.

    call function 'RP_CALC_DATE_IN_INTERVAL'
      exporting
        date      = gv_date
        days      = '01'
        months    = '00'
        signum    = '+'
        years     = '00'
      importing
        calc_date = gv_date.



  endwhile.





  clear: gv_zseq.
  read table gt_head_bl index 1.
  loop at gt_delivery.
    gv_zseq = gv_zseq + 1.
    gv_cna = gt_delivery-netpr * gt_delivery-lfimg.
*    gt_zmm1t5020-kcoo = '2010'.
*    gt_zmm1t5020-lifnr = gt_head_bl-lifnr.
*    gt_zmm1t5020-vend = |{ gt_head_bl-lifnr alpha = in }|.
*    gt_zmm1t5020-blnum = gt_head_bl-bldoc.
*    gt_zmm1t5020-invno = gt_zmm1t5020-koinv = gv_invno.
*    gt_zmm1t5020-poline =  gv_zseq.
*    gt_zmm1t5020-conno = gv_shipment.
*    gt_zmm1t5020-ponum = gv_ponum_c.
*    gt_zmm1t5020-consz = '1'.
*    gt_zmm1t5020-matno = gt_zmm1t5020-fcode = gt_delivery-matnr.
*    gt_zmm1t5020-ordqty = gt_delivery-lfimg.
*    gt_zmm1t5020-quant = gt_delivery-kwmeng.
*    gt_zmm1t5020-cnprice_usd = gt_delivery-netpr.
*    gt_zmm1t5020-cnamount_usd = gv_cna.
*    append gt_zmm1t5020.
*    clear: gt_zmm1t5020.

    gt_asn-conno = gv_shipment.
    gt_asn-blnum = gt_asn-m_blnum = gt_head_bl-bldoc.
    gt_asn-ponum = gv_uspo.
    gt_asn-poitm = gt_delivery-posnr.
    gt_asn-matno = gt_delivery-matnr.
    gt_asn-quant = gt_delivery-kwmeng.
*    gt_asn-fobcn_o = gt_asn-fobcn = gt_delivery-netpr. "HQ 입력
*    gt_asn-netpr_o = gt_asn-netpr = gt_delivery-kwmeng * gt_delivery-netpr. "HQ 입력
    gt_asn-etadt = gv_end.
    gt_asn-etdat = gv_start.
    gt_asn-koinv = gv_master_ivno.
    gt_asn-invno = gv_invno.
*    gt_asn-ordqty = gt_delivery-lfimg.   "HQ 입력
*    gt_asn-plshdt = gv_planned.    "Optional
    gt_asn-frefw = gv_frefw.

    gt_asn-invdt = gt_asn-invdt_k = gt_asn-actsd =
    gt_asn-onbdt = gt_asn-invdt  = p_budat.     "gt_asn-asn_pdate -> HQ 입력

    gt_asn-kunnr = '1000'.
    gt_asn-land1 = 'US'.
    gt_asn-ifind = 'F'.
    gt_asn-consz = '1'.

    select single groes
      from mara
      into @gt_asn-zsize
      where matnr = @gt_delivery-matnr.


*    select single  kunag                   "HQ 입력
*      into  @gt_asn-shptocode
*      from likp
*      where vbeln = @p_vbeln.

*    select single b~street,b~house_num1, b~city1, b~post_code1
*      into ( @gt_asn-shptoadd, @gt_asn-cat01, @gt_asn-cat02, @gt_asn-cat03 )
*            from but020 as a
*      inner join adrc as b
*                on a~addrnumber = b~addrnumber
*           where a~partner = @gt_head_bl-lifnr.

    select single maktx
      from makt
      into @gt_asn-fulldesc
      where matnr = @gt_asn-matno
        and spras = @sy-langu.

*    select single kunnr
*      into @data(gv_soldto)
*      from vbpa
*      where vbeln = @gv_salesorder
*        and posnr = '00000'
*        and parvw = 'SX'.

*    gt_asn-cuscd = gv_soldto.    -> HQ입력

**-----Warehouse Code ---------------------------------   -> HQ 입력
*    select single kunnr
*          from vbpa
*          into @data(gv_shipto)
*     where vbeln = @gv_salesorder
*         and posnr = '00000'
*         and parvw = 'WE'.
*
*    gv_shipto = |{ gv_shipto alpha = out }|.
*    gv_soldto = |{ gv_soldto alpha = out }|.
*
*    select single zcm_code1
*      from zcommt0021
*      into @data(gv_whcd)
*      where zmodule = 'SD'
*          and zclass = 'SD012'
*          and spras = 'E'
*          and zcmf01_ch = @gv_shipto
*          and zcmf02_ch = @gv_soldto.
*
*    if sy-subrc = 0.
*      gt_asn-whcd = gt_asn-werks = gv_whcd.
*    else.
*      select single zcm_code1
*      from zcommt0021
*      into @gv_whcd
*      where zmodule = 'SD'
*          and zclass = 'SD012'
*          and spras = 'E'
*          and zcmf01_ch = @gv_soldto.
*
*      if sy-subrc = 0.
*        gt_asn-whcd = gt_asn-werks = gv_whcd.
*
*      endif.
*    endif.
**----------------------------------------------------
    append gt_asn.
    clear: gt_asn.
  endloop.

  select single zcmf01_ch
      into @gv_nosaps
      from zcommt0021
      where zmodule = 'SD'
          and zclass = 'SD011'
          and zcm_code1 = '2011'
          and spras = 'E'.

*05/10/2021 E)00062
*  if gv_nosaps is not initial.
*    call function 'ZSD_ASN_SAVE'
*      importing
*        e_msg   = gv_msg
*      tables
*        it_data = gt_zmm1t5020[].
*  endif.
*05/10/2021 E)00062

  if sy-subrc = 0.
    message s499 with gv_msg.


    call function 'ZMM_IF_ASN'
      exporting
        api_doc_id = gv_api_doc_id
        mode       = gv_mode
*       nosaps     = 'X'
        nosaps     = gv_nosaps
      tables
        gt_data    = gt_asn[]
        gt_return  = gt_asn_return[].

    loop at gt_asn_return.
      message s499 with gt_asn_return-zsap_rmsg display like gt_asn_return-zsap_rtype.
    endloop.

*    if gt_asn_return[] is not initial.
*      read table gt_asn_return with key zsap_rtype = 'E'.
*      if sy-subrc = 0.
*        message s499 with gt_asn_return-zsap_rmsg display like 'E'.
*      else.
*        message s499 with text-t01.
*      endif.
*    endif.
  endif.
endform.
