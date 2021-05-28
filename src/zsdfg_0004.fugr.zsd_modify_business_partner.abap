function ZSD_MODIFY_BUSINESS_PARTNER.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_DATA) TYPE  ZFIS2010
*"  EXPORTING
*"     REFERENCE(EV_BPARTNER) TYPE  BU_PARTNER
*"     REFERENCE(EV_TYPE) TYPE  BAPI_MTYPE
*"     REFERENCE(EV_MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      IT_BANK STRUCTURE  ZFI1S0021 OPTIONAL
*"      IT_PART STRUCTURE  ZFI1S0022 OPTIONAL
*"      IT_INTER STRUCTURE  ZMDS0061 OPTIONAL
*"--------------------------------------------------------------------
*&---------------------------------------------------------------------*
*Module        : FI
*Creator       :  ZEN15
*Create Date   :  2018.09.26
*Description   :  Business Partner  Create / Change
*----------------------------------------------------------------------*
*                     Change History.
*----------------------------------------------------------------------*
*Chg No.       Chg Date         Modifier         Description
*----------------------------------------------------------------------*
*  000         2018/09/26      ZEN15        Initial Version
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Business Partner Rolecategory
*----------------------------------------------------------------------*
* ZZCUST   Customer
* ZZVEND   Vendor
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Business Partner Grouping
*----------------------------------------------------------------------*
* Gen
*1000 Domestic          10000~19999         Internal
*2000 Overseas          20000~29999         Internal
*3000 Person            3000000~399999      Internal
*4000 Intercompany      1000~9999           External
*5000 Employee          5000000~599999      Internal

* Customer
*9000 Onetime           ONETIME00~ONETIME99 External

* Vendor
*6000 Corp. Card        60000~69999         Internal
*7000 LLC & Individual  70000~79999         Internal
*----------------------------------------------------------------------*
* Business Partner number internal
*----------------------------------------------------------------------*
  data: ls_data like zfis2010.   " Business Partner Structure

  data: lv_pguid like but000-partner_guid,
        lv_uuid  type sysuuid_c32.

  data: lv_kunnr like kna1-kunnr, " Customer
        lv_lifnr like lfa1-lifnr. " Vendor
*----------------------------------------------------------------------*
* Parameter initialization
*----------------------------------------------------------------------*
  clear: ev_bpartner, ev_type, ev_message.

*----------------------------------------------------------------------*
* Data check and initial value setting
*----------------------------------------------------------------------*
  perform check_data tables it_bank       " Import Bank
                            it_part       " Import Customer partner
                      using is_data       " Import Bank Business Partner
                   changing ls_data       " Business Partner
                            lv_kunnr      " Customer
                            lv_lifnr      " Vendor
                            ev_type       " Message Type
                            ev_message.   " Message
  check ev_type is initial.

*----------------------------------------------------------------------*
* Business Partner GUID
*----------------------------------------------------------------------*
  select single partner_guid into lv_pguid
     from but000
    where partner = ls_data-bpartner. " Business Partner number

  if sy-subrc = 0.
    lv_uuid = lv_pguid.

  else.
    try.
        call method cl_system_uuid=>if_system_uuid_static~create_uuid_c32
          receiving
            uuid = lv_uuid.
      catch cx_uuid_error.

    endtry.

  endif.

*----------------------------------------------------------------------*
* Business Partner (Create / Change)
*----------------------------------------------------------------------*
  perform modify_business_partner tables it_bank
                                         it_part
                                         it_inter
                                   using ls_data
                                         lv_pguid
                                         lv_uuid
                                         lv_kunnr
                                         lv_lifnr
                                changing ev_bpartner
                                         ev_type
                                         ev_message.

endfunction.
