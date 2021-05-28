*&---------------------------------------------------------------------*
*& Include          ZSDR0051_TOP
*&---------------------------------------------------------------------*


data: gv_order type ekko-ebeln.

*----Invoice Var.----------------------
data: gv_api_doc_id type psi_doc_id.
data: gt_head       type standard table of zmms0200 with header line,
      gt_item       type standard table of zmms0202 with header line,
      gt_return     type standard table of zmms0201 with header line,
      gs_comwa      like vbco6,
      gt_vbfa_tab   type standard table of vbfas with header line,
      gv_shipment   like vttk-tknum,   "Shipment No.
      gv_salesorder like vbak-vbeln, "Sales Order No.
      gv_ponum      like vbkd-bstkd,   "HQ P/O
      gv_ponum_c    like vbkd-bstkd_e, "Customer P/O NO.
      gv_uspo       like vbkd-ihrez, "US PO
      gv_invno      type icl_invoiceref,
      gv_invno_im   like rbkp-bktxt,    "Invoice
      gv_master_ivno   like rbkp-bktxt,    "Master Invoice Number
      gv_msg        type string,
      gt_asn        like  standard table of zmms0220 with header line,
      gt_asn_return like standard table of  zmms0221 with header line,
      gv_start      like vttk-datbg,
      gv_end        like vttk-daten,
      gv_planned    like vttk-dpreg,
      gv_frefw      like vttk-tdlnr,
      gv_route      like vttk-route,
      gv_actual     like vttk-dareg,
      gv_date       type dats,
      gv_init_date  type dats,
      gv_route_date type n length 2,

      begin of gs_likp,
        vstel like likp-vstel,
        kunag like likp-kunag,
        grulg like likp-grulg,
        ladgr like lips-ladgr,
        vsbed like likp-vsbed,
        tragr like likp-tragr,
      end of gs_likp,

      begin of gt_delivery occurs 0,
        vbeln    like lips-vbeln,
        posnr    like lips-posnr,
        matnr    type matnr,
        lfimg    like lips-lfimg,
        kwmeng   like vbap-kwmeng,
        netpr    like vbap-netpr,
        invno_im like rbkp-bktxt,
      end of gt_delivery.

*----bl containier var..----------------------
data: gt_head_bl   like standard table of  zmms0210 with header line,
      gt_item_bl   like standard table of  zmms0212 with header line,
      gt_return_bl like standard table of zmms0211 with header line,
      gt_zmm1t5020 like standard table of zmm1t5020 with header line,
      gv_zseq      type i,
      gv_cna       type dec015.

*-----ETC----------
data: gv_nosaps(1).
data: gs_options like schedoptions.
data: gv_mode(1).

selection-screen begin of block b1 with frame title text-t01.
parameter : p_vbeln like likp-vbeln.
parameters: p_budat type budat default sy-datlo.
selection-screen end of block b1.

selection-screen begin of block b2.
parameters : p_can as checkbox.
selection-screen end of block b2.
