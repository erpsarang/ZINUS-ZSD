*&---------------------------------------------------------------------*
*& Include          LZSDFG_0004F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_BANK
*&      --> T_PART
*&      --> I_DATA
*&      <-- LS_DATA
*&      <-- LV_KUNNR
*&      <-- LV_LIFNR
*&      <-- E_TYPE
*&      <-- E_MESSAGE
*&---------------------------------------------------------------------*
FORM check_data TABLES pt_bank STRUCTURE zfi1s0021
                       pt_part STRUCTURE zfi1s0022
                 USING ps_itab STRUCTURE zfis2010
              CHANGING ps_data STRUCTURE zfis2010
                       pv_kunnr
                       pv_lifnr
                       pv_type
                       pv_message.

  IF ps_itab IS NOT INITIAL.
    MOVE-CORRESPONDING ps_itab TO ps_data.

    PERFORM conv_exit_alpha_input CHANGING: ps_data-bpartner,     " Business Partner Number
                                            ps_data-akont,        " Reconciliation Account
                                            ps_data-vbund.        " Company ID of trading partner

  ENDIF.

  IF pt_part[] IS NOT INITIAL.
    LOOP AT pt_part.
      PERFORM conv_exit_alpha_input CHANGING pt_part-partner.
      MODIFY pt_part INDEX sy-tabix TRANSPORTING partner.
    ENDLOOP.

  ENDIF.
*----------------------------------------------------------------------*
* Check input value
*----------------------------------------------------------------------*
  PERFORM check_validation TABLES pt_bank
                            USING ps_data
                         CHANGING pv_type
                                  pv_message.

  CHECK pv_type IS INITIAL.
*----------------------------------------------------------------------*
* Check whether Customer and Vendor are created
*----------------------------------------------------------------------*
* Check that the customer does not exist
  SELECT SINGLE kunnr INTO pv_kunnr
     FROM kna1
    WHERE kunnr = ps_data-bpartner.

* Check that the vendor does not exist
  SELECT SINGLE lifnr INTO pv_lifnr
     FROM lfa1
    WHERE lifnr = ps_data-bpartner.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PS_DATA_BPARTNER
*&---------------------------------------------------------------------*
FORM conv_exit_alpha_input CHANGING pv_value.
  CHECK pv_value IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pv_value
    IMPORTING
      output = pv_value.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_BANK
*&      --> PS_DATA
*&      <-- PV_TYPE
*&      <-- PV_MESSAGE
*&---------------------------------------------------------------------*
FORM check_validation TABLES pt_bank STRUCTURE zfi1s0021
                       USING ps_data STRUCTURE zfis2010
                    CHANGING pv_type
                             pv_message.
  DATA: lo_struct TYPE REF TO cl_abap_structdescr.

  DATA: lt_temp  TYPE abap_component_tab,
        lt_field TYPE abap_component_tab,
        ls_field TYPE abap_componentdescr,
        ls_s0880 TYPE zfis2010,
        ls_s0882 TYPE zfis2010.

  RANGES: lr_taxtype FOR tfktaxnumtype_c-taxtype.
  DATA: lt_taxtype LIKE tfktaxnumtype_c OCCURS 0 WITH HEADER LINE.

  DATA: lv_type    TYPE bapi_mtype,
        lv_message TYPE bapi_msg,
        lv_role    TYPE bu_role_screen.

  FIELD-SYMBOLS: <fs_data>  TYPE zfis2010,  " Business Partner Structure
                 <fs_fname> TYPE any,        " FIELD Name
                 <fs_value> TYPE any.        " FIELD Value

  CLEAR: pv_type, pv_message.
*----------------------------------------------------------------------*
* Structure field lookup
*----------------------------------------------------------------------*
  lo_struct ?= cl_abap_structdescr=>describe_by_data( ls_s0880 ).
  lt_field = lo_struct->get_components( ).
  DELETE lt_field WHERE name = space.

  lo_struct ?= cl_abap_structdescr=>describe_by_data( ls_s0882 ).
  lt_temp = lo_struct->get_components( ).
  DELETE lt_temp WHERE name = space.

  APPEND LINES OF lt_temp TO lt_field.

  CLEAR lr_taxtype. REFRESH lr_taxtype.
  lr_taxtype-sign   = 'I'.
  lr_taxtype-option = 'CP'.
  lr_taxtype-low    = ps_data-country && '*'. "국가키
  APPEND lr_taxtype.
  CLEAR  lr_taxtype.

  SELECT taxtype
    INTO CORRESPONDING FIELDS OF TABLE lt_taxtype
    FROM tfktaxnumtype_c
   WHERE taxtype IN lr_taxtype
   ORDER BY taxtype.
*----------------------------------------------------------------------*
* Data check
*----------------------------------------------------------------------*
  ASSIGN ps_data TO <fs_data>.

  LOOP AT lt_field INTO ls_field.
    ASSIGN ls_field-name TO <fs_fname>.                                "FIELD Name
    ASSIGN COMPONENT <fs_fname> OF STRUCTURE <fs_data> TO <fs_value>.  "FIELD Value

    CASE ls_field-name.
      WHEN 'BUKRS'    OR  " Company code
           'GROUPING' OR  " Business Partner Group
           'ROLE'.       " Business Partner Role

        IF <fs_value> IS INITIAL.
          pv_type = 'E'.
          MESSAGE s017(zmcfi1) WITH ls_field-name INTO pv_message. "The (&) field is required.
          RETURN.
        ENDIF.

      WHEN 'TAXNUM1'.  " Business Partner Tax Number 1
        IF ps_data-bpartner IS NOT INITIAL AND <fs_value> IS NOT INITIAL.
          READ TABLE lt_taxtype INDEX 1.
          CLEAR lv_role.
          lv_role = ps_data-role.

          CALL FUNCTION 'ZFI06_TAX_KR2_DUP_CHK'
            EXPORTING
              i_bu_role = lv_role
              i_partner = ps_data-bpartner
              i_taxtype = lt_taxtype-taxtype
              i_stcd2   = ps_data-taxnum1
            IMPORTING
              e_message = pv_message.

          IF pv_message IS NOT INITIAL.
            pv_type = 'E'.
          ENDIF.

        ENDIF.

      WHEN 'TAXNUM2'. " Business Partner Tax Number 2
        IF ps_data-bpartner IS NOT INITIAL AND <fs_value> IS NOT INITIAL.
          READ TABLE lt_taxtype INDEX 2.
          CLEAR lv_role.
          lv_role = ps_data-role.

          CALL FUNCTION 'ZFI06_TAX_KR2_DUP_CHK'
            EXPORTING
              i_bu_role = lv_role
              i_partner = ps_data-bpartner
              i_taxtype = lt_taxtype-taxtype
              i_stcd2   = ps_data-taxnum1
            IMPORTING
              e_message = pv_message.

          IF pv_message IS NOT INITIAL.
            pv_type = 'E'.
          ENDIF.

        ENDIF.

      WHEN 'LZONE' OR 'REGIO'.
        IF ps_data-role EQ TEXT-001.
          IF ps_data-bpartner IS INITIAL AND <fs_value> IS INITIAL.
            pv_type = 'E'.
            MESSAGE s017(zmcfi1) WITH ls_field-name INTO pv_message. "The (&) field is required.
            RETURN.
          ENDIF.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.

*----------------------------------------------------------------------*
* Bank Information Check
*----------------------------------------------------------------------*
* Value must exist in all fields when present
*----------------------------------------------------------------------*
  IF pt_bank[] IS NOT INITIAL.
    LOOP AT pt_bank.
      IF pt_bank-bkvid IS INITIAL OR  " Bank details ID
         pt_bank-banks IS INITIAL OR  " Bank country key
         pt_bank-bankl IS INITIAL OR  " Bank number
         pt_bank-bankn IS INITIAL OR  " Bank account number
         pt_bank-bkref IS INITIAL.    " Reference specifications for bank details
        pv_type    = 'E'.
* "Bank information (T_BANK) should be entered in all field values. (Excluding Account Holder, Bank Control Key)
        pv_message = TEXT-e01.
        RETURN.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_BUSINESS_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_BANK
*&      --> T_PART
*&      --> LS_DATA
*&      --> LV_PGUID
*&      --> LV_UUID
*&      --> LV_KUNNR
*&      --> LV_LIFNR
*&      <-- E_BPARTNER
*&      <-- E_TYPE
*&      <-- E_MESSAGE
*&---------------------------------------------------------------------*
FORM modify_business_partner TABLES pt_bank  STRUCTURE zfi1s0021
                                    pt_part  STRUCTURE zfi1s0022
                                    pt_inter STRUCTURE zmds0061
                              USING ps_data  STRUCTURE zfis2010
                                    pv_pguid
                                    pv_uuid
                                    pv_kunnr
                                    pv_lifnr
                           CHANGING pv_bpartner
                                    pv_type
                                    pv_message.
  DATA: lt_main TYPE cvis_ei_extern_t,  " Composite interface for business partners in CVI
        ls_main TYPE cvis_ei_extern.    " Composite interface for business partners in CVI

  DATA: lt_return TYPE bapiretm,
        ls_return TYPE bapireti,
        ls_msg    TYPE LINE OF bapiretct.

*----------------------------------------------------------------------*
* Data organization
*----------------------------------------------------------------------*
  PERFORM append_bapi_data TABLES pt_bank
                                  pt_part
                                  pt_inter
                            USING ps_data
                                  pv_pguid
                                  pv_uuid
                                  pv_kunnr
                                  pv_lifnr
                                  pv_bpartner
                         CHANGING ls_main.
*----------------------------------------------------------------------*
* 선택적 고객/공급업체 역할 정의 : 처음 생성시에만 세팅
*----------------------------------------------------------------------*
  PERFORM set_create_cv USING ps_data
                              pv_kunnr
                              pv_lifnr
                     CHANGING ls_main.
*----------------------------------------------------------------------*
* 비즈니스 파트너 (생성/변경)
*----------------------------------------------------------------------*
  APPEND ls_main TO lt_main.

  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
    EXPORTING
      i_data   = lt_main
    IMPORTING
      e_return = lt_return.

  LOOP AT lt_return INTO ls_return.
    DELETE ls_return-object_msg WHERE type NE 'E'
                                  AND type NE 'A'.
    IF ls_return-object_msg[] IS NOT INITIAL.
      pv_type = 'E'.
      READ TABLE ls_return-object_msg INDEX 1 INTO ls_msg.
      MESSAGE ID ls_msg-id TYPE 'S' NUMBER ls_msg-number INTO pv_message
         WITH ls_msg-message_v1 ls_msg-message_v2 ls_msg-message_v3 ls_msg-message_v4.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF pv_type = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
* BAPI_TRANSACTION_COMMIT 과 순서 바꾸지 말 것, SY-MSGV1 값이 사라짐
    IF ps_data-bpartner IS INITIAL.
      pv_bpartner = sy-msgv1.
    ELSE.
      pv_bpartner = ps_data-bpartner.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.


* Update international data - Phone, Fax, Email
    IF pt_inter[] IS NOT INITIAL.
      PERFORM update_international_address TABLES pt_inter
                                           USING  pv_bpartner.
    ENDIF.

    pv_type = 'S'.
    IF pv_pguid IS INITIAL.
      MESSAGE s018(zmcfi1) WITH pv_bpartner INTO pv_message.  "Business Partner (&) has been created.
    ELSE.
      MESSAGE s019(zmcfi1) WITH pv_bpartner INTO pv_message.  "Changed Business Partner (&).
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_BAPI_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_BANK
*&      --> PT_PART
*&      --> PS_DATA
*&      --> PV_PGUID
*&      --> PV_UUID
*&      --> PV_KUNNR
*&      --> PV_LIFNR
*&      --> PV_BPARTNER
*&      <-- LS_MAIN
*&---------------------------------------------------------------------*
FORM append_bapi_data TABLES pt_bank  STRUCTURE zfi1s0021
                             pt_part  STRUCTURE zfi1s0022
                             pt_inter STRUCTURE zmds0061
                       USING ps_data  STRUCTURE zfis2010
                             pv_pguid
                             pv_uuid
                             pv_kunnr
                             pv_lifnr
                             pv_bpartner
                    CHANGING ps_main TYPE cvis_ei_extern.
* Partner Data
  PERFORM append_partner_data TABLES pt_bank
                                     pt_inter
                               USING ps_data
                                     pv_pguid
                                     pv_uuid
                                     pv_bpartner
                            CHANGING ps_main.
* Customer Data
  PERFORM append_customer_data TABLES pt_part
                                USING ps_data
                                      pv_kunnr
                                      pv_lifnr
                             CHANGING ps_main.

* Vendor Data
  PERFORM append_vendor_data USING ps_data
                                   pv_kunnr
                                   pv_lifnr
                          CHANGING ps_main.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_PARTNER_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_BANK
*&      --> PS_DATA
*&      --> PV_PGUID
*&      --> PV_UUID
*&      --> PV_BPARTNER
*&      <-- PS_MAIN
*&---------------------------------------------------------------------*
FORM append_partner_data TABLES pt_bank  STRUCTURE zfi1s0021
                                pt_inter STRUCTURE zmds0061
                          USING ps_data  STRUCTURE zfis2010
                                pv_pguid
                                pv_uuid
                                pv_bpartner
                       CHANGING ps_main TYPE cvis_ei_extern.
  DATA: ls_address    TYPE bus_ei_bupa_address,
        ls_phone      TYPE bus_ei_bupa_telephone,
        ls_fax        TYPE bus_ei_bupa_fax,
        ls_smtp       TYPE bus_ei_bupa_smtp,
        ls_taxnumbers TYPE bus_ei_bupa_taxnumber,
        ls_version    TYPE bus_ei_bupa_version,
        ls_roles      TYPE bus_ei_bupa_roles,
        ls_relation   TYPE burs_ei_extern,
        ls_bankdetail TYPE bus_ei_bupa_bankdetail.

  DATA: lt_tel  LIKE bapiadtel  OCCURS 0 WITH HEADER LINE,
        lt_fax  LIKE bapiadfax  OCCURS 0 WITH HEADER LINE,
        lt_smtp LIKE bapiadsmtp OCCURS 0 WITH HEADER LINE.

  DATA: lt_taxtype LIKE tfktaxnumtype_c OCCURS 0 WITH HEADER LINE.

  DATA: ls_inter LIKE zmds0061.

  RANGES: lr_taxtype FOR tfktaxnumtype_c-taxtype.
*----------------------------------------------------------------------*
* PARTNER > HEADER
*----------------------------------------------------------------------*
  ps_main-partner-header-object_task                  = 'M'.              "I:삽입, U:갱신, M:수정, D:삭제, C:현재 상태
  ps_main-partner-header-object_instance-bpartner     = ps_data-bpartner. "비즈니스 파트너 번호
  ps_main-partner-header-object_instance-bpartnerguid = pv_uuid.
*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > COMMON
*----------------------------------------------------------------------*
  ps_main-partner-central_data-common-data-bp_control-category = '2'.               "파트너 범주 (1:개인, 2:조직, 3:그룹)
  ps_main-partner-central_data-common-data-bp_control-grouping = ps_data-grouping.  "비즈니스 파트너 그룹화
  IF ps_data-bpkind IS NOT INITIAL.
    ps_main-partner-central_data-common-data-bp_centraldata-partnertype  = ps_data-bpkind.
    ps_main-partner-central_data-common-datax-bp_centraldata-partnertype = 'X'.
  ENDIF.

  DATA : lv_bpartner LIKE but020-partner.

  CLEAR : lv_bpartner.
  CLEAR : gs_address_info, gt_address_info[].
  IF ps_data-bpartner IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ps_data-bpartner
      IMPORTING
        output = lv_bpartner.

    SELECT a~partner,
           b~nation,     b~name1,      b~name2,
           b~sort1 AS searchterm1,
           b~sort2 AS searchterm2,
           b~street,
           b~house_num1 AS house_no,
           b~city1 AS city,
           b~country,    b~post_code1,
           b~transpzone, b~region,   b~langu, b~tel_number, b~fax_number,
           c~tel_number AS mobile,   d~smtp_addr
           INTO  CORRESPONDING FIELDS OF TABLE @gt_address_info
           FROM  but020 AS a INNER JOIN adrc AS b
           ON    a~addrnumber EQ b~addrnumber
           LEFT  JOIN adr2 AS c
           ON    b~addrnumber EQ c~addrnumber
           AND   c~r3_user    EQ '3'
           LEFT  JOIN adr6 AS d
           ON    b~addrnumber EQ d~addrnumber
           WHERE a~partner   EQ @lv_bpartner
           AND   b~date_from LE @sy-datum
           AND   b~date_to   GT @sy-datum.
    IF sy-subrc = 0.
      READ TABLE gt_address_info INTO DATA(ls_address_info) WITH KEY nation = space.
      IF sy-subrc = 0.
        DELETE gt_address_info INDEX sy-tabix.
      ENDIF.

      SORT gt_address_info BY partner nation.
    ENDIF.
  ENDIF.

* 이름 1
  IF ps_data-name1 IS NOT INITIAL OR
     ( ps_data-bpartner IS NOT INITIAL AND
       ps_data-name1    NE ls_address_info-name1 ).
*    ps_main-partner-central_data-common-data-bp_organization-name1  = ps_data-name1 && ' ' &&  ps_data-name2.
    CONCATENATE ps_data-name1  ps_data-name2 INTO ps_main-partner-central_data-common-data-bp_organization-name1 SEPARATED BY space.
*    ps_main-partner-central_data-common-data-bp_organization-name1  = ps_data-name1 && ' ' &&  ps_data-name2.
    ps_main-partner-central_data-common-datax-bp_organization-name1 = 'X'.
  ENDIF.
* 이름 2
  IF ps_data-name2 IS NOT INITIAL.
*    ps_main-partner-central_data-common-data-bp_organization-name2  = .
*    ps_main-partner-central_data-common-datax-bp_organization-name2 = 'X'.
  ENDIF.
* 검색어 1
*  IF ps_data-searchterm1 IS NOT INITIAL OR
*     ( ps_data-bpartner    IS NOT INITIAL AND
*       ps_data-searchterm1 NE ls_address_info-searchterm1 ).
*    ps_main-partner-central_data-common-data-bp_centraldata-searchterm1  = ps_data-searchterm1.
*    ps_main-partner-central_data-common-datax-bp_centraldata-searchterm1 = 'X'.
*  ENDIF.
** 검색어 2
*  IF ps_data-searchterm2 IS NOT INITIAL OR
*     ( ps_data-bpartner    IS NOT INITIAL AND
*       ps_data-searchterm2 NE ls_address_info-searchterm2 ).
*    ps_main-partner-central_data-common-data-bp_centraldata-searchterm2  = ps_data-searchterm2.
*    ps_main-partner-central_data-common-datax-bp_centraldata-searchterm2 = 'X'.
*  ENDIF.
*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > ADDRESS
*----------------------------------------------------------------------*
* 생성시: M (주소개요탭에만 APPEND 됨)
* 변경시: 5 (주소탭 반영, 주소개요탭에 APPEND)
*       2 (주소탭 반영, 주소개요탭에 UPDATE) - 화면에서 변경시 '2' 와 같음
  IF pv_pguid IS INITIAL.
    ls_address-task = 'M'.
  ELSE.
    ls_address-task = '2'.
  ENDIF.
* 상세 주소
*  IF ps_data-street IS NOT INITIAL OR
*     ( ps_data-bpartner IS NOT INITIAL AND
*       ps_data-street   NE ls_address_info-street ).
*    ls_address-data-postal-data-street  = ps_data-street.
*    ls_address-data-postal-datax-street = 'X'.
*  ENDIF.
** House No.
*  IF ps_data-house_no IS NOT INITIAL OR
*     ( ps_data-bpartner IS NOT INITIAL AND
*       ps_data-house_no   NE ls_address_info-house_no ).
*    ls_address-data-postal-data-house_no  = ps_data-house_no.
*    ls_address-data-postal-datax-house_no = 'X'.
*  ENDIF.
** 우편번호
*  IF ps_data-postl_cod1 IS NOT INITIAL OR
*     ( ps_data-bpartner   IS NOT INITIAL AND
*       ps_data-postl_cod1 NE ls_address_info-post_code1 ).
*    ls_address-data-postal-data-postl_cod1  = ps_data-postl_cod1.
*    ls_address-data-postal-datax-postl_cod1 = 'X'.
*  ENDIF.
** 도시
*  IF ps_data-city IS NOT INITIAL OR
*     ( ps_data-bpartner   IS NOT INITIAL AND
*       ps_data-city NE ls_address_info-city ).
*    ls_address-data-postal-data-city  = ps_data-city.
*    ls_address-data-postal-datax-city = 'X'.
*  ENDIF.
* 국가 키
  IF ps_data-country IS NOT INITIAL OR
     ( ps_data-bpartner IS NOT INITIAL AND
       ps_data-country  NE ls_address_info-country ).
    ls_address-data-postal-data-country  = ps_data-country.
    ls_address-data-postal-datax-country = 'X'.
  ENDIF.
* Transpzone
*  IF ps_data-role = TEXT-001. "CUSTOMER
*    IF  ps_data-bpartner IS NOT INITIAL .                   "수정
*      IF ps_data-lzone    NE ls_address_info-transpzone.
*        ls_address-data-postal-data-transpzone = ps_data-lzone.
*        ls_address-data-postal-datax-transpzone = 'X'.
*      ENDIF.
*    ELSE.                                                         "생성
*      IF ps_data-lzone IS NOT INITIAL .
*        ls_address-data-postal-data-transpzone = ps_data-lzone.
*        ls_address-data-postal-datax-transpzone = 'X'.
*      ENDIF.
*    ENDIF.
*
*  ELSEIF ps_data-role = TEXT-002. "VENDOR
*
*    IF ps_data-bukrs = '1000'.
*      ls_address-data-postal-data-transpzone  = '0000000001'.
*      ls_address-data-postal-datax-transpzone = 'X'.
*    ELSE.
*      IF ps_data-lzone IS NOT INITIAL.
*        ls_address-data-postal-data-transpzone  = ps_data-lzone.
*        ls_address-data-postal-datax-transpzone = 'X'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
** Region
*  IF ps_data-role = TEXT-001.
*    IF  ps_data-bpartner IS NOT INITIAL.                      "수정
*      IF ps_data-regio    NE ls_address_info-region.
*        ls_address-data-postal-data-region  = ps_data-regio.
*        ls_address-data-postal-datax-region = 'X'.
*      ENDIF.
*    ELSE.                                                             "생성
*      IF ps_data-regio IS NOT INITIAL.
*        ls_address-data-postal-data-region  = ps_data-regio.
*        ls_address-data-postal-datax-region = 'X'.
*      ENDIF.
*    ENDIF.
*
*  ELSEIF ps_data-role = TEXT-002.
*
*    IF ps_data-regio IS NOT INITIAL.
*      ls_address-data-postal-data-region  = ps_data-regio.
*      ls_address-data-postal-datax-region = 'X'.
*    ENDIF.
*
*  ENDIF.
*
** 언어 키
*  IF ps_data-langu IS NOT INITIAL OR
*     ( ps_data-bpartner IS NOT INITIAL AND
*       ps_data-langu    NE ls_address_info-langu ).
*    ls_address-data-postal-data-langu  = ps_data-langu.
*    ls_address-data-postal-datax-langu = 'X'.
*  ENDIF.
*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > ADDRESS > ADDRESSES[] > DATA > COMMUNICATION > PHONE
*----------------------------------------------------------------------*
* Address Communication 정보
*  PERFORM get_address TABLES lt_tel
*                             lt_fax
*                             lt_smtp
*                       USING ps_data-bpartner
*                             pv_bpartner.
* I: 삽입, U: 갱신, D: 삭제, 1: 논리 키 삽입, 2: 논리 키 갱신, 4: 논리 키 삭제, S: 표준
* ' ': 유선 전화, 1: 기본 유선 전화, 2: 이동 전화지만 기본 이동 전화가 아님, 3: 기본 이동 전화, X: 비사용 - 더 이상 유효하지 않음
* 전화
*  IF ps_data-telephone IS NOT INITIAL OR
*     ( ps_data-bpartner  IS NOT INITIAL AND
*       ps_data-telephone NE ls_address_info-tel_number ).
*    READ TABLE lt_tel WITH KEY r_3_user = '1' BINARY SEARCH.
*    IF sy-subrc = 0.
*      ls_phone-contact-task = '2'.
*    ELSE.
*      ls_phone-contact-task = 'I'.
*    ENDIF.
*
*    ls_phone-contact-data-telephone   = ps_data-telephone.
*    ls_phone-contact-datax-telephone  = 'X'.
*
*    ls_phone-contact-data-r_3_user    = '1'.
*    ls_phone-contact-datax-r_3_user   = 'X'.
*
*    ls_phone-contact-data-countryiso  = ps_data-country.
*    ls_phone-contact-datax-countryiso = 'X'.
*
*    ls_phone-contact-data-std_no      = 'X'.
*    ls_phone-contact-datax-std_no     = 'X'.
*
*    APPEND ls_phone TO ls_address-data-communication-phone-phone.
*    CLEAR  ls_phone.
*  ENDIF.
* 핸드폰
*  IF ps_data-mobile IS NOT INITIAL OR
*     ( ps_data-bpartner IS NOT INITIAL AND
*       ps_data-mobile   NE ls_address_info-mobile ).
*    READ TABLE lt_tel WITH KEY r_3_user = '3' BINARY SEARCH.
*    IF sy-subrc = 0.
*      ls_phone-contact-task = '2'.
*    ELSE.
*      ls_phone-contact-task = 'I'.
*    ENDIF.
*
*    ls_phone-contact-data-telephone   = ps_data-mobile.
*    ls_phone-contact-datax-telephone  = 'X'.
*
*    ls_phone-contact-data-std_recip   = 'X'.
*    ls_phone-contact-datax-std_recip  = 'X'.
*
*    ls_phone-contact-data-r_3_user    = '3'.
*    ls_phone-contact-datax-r_3_user   = 'X'.
*
*    ls_phone-contact-data-countryiso  = ps_data-country.
*    ls_phone-contact-datax-countryiso = 'X'.
*
*    ls_phone-contact-data-std_no      = 'X'.
*    ls_phone-contact-datax-std_no     = 'X'.
*
*    APPEND ls_phone TO ls_address-data-communication-phone-phone.
*    CLEAR  ls_phone.
*  ENDIF.

*  IF ls_address-data-communication-phone-phone[] IS NOT INITIAL.
*    SORT ls_address-data-communication-phone-phone BY contact ASCENDING.
*  ENDIF.
*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > ADDRESS > ADDRESSES[] > DATA > COMMUNICATION > FAX
*----------------------------------------------------------------------*
* I:삽입, U:갱신, D:삭제, 1:논리 키 삽입, 2:논리 키 갱신, 4:논리 키 삭제, S:표준
* 팩스
*  IF ps_data-fax IS NOT INITIAL OR
*     ( ps_data-bpartner IS NOT INITIAL AND
*       ps_data-fax      NE ls_address_info-fax_number ).
*    IF lt_fax[] IS INITIAL.
*      ls_fax-contact-task = 'I'.
*    ELSE.
*      ls_fax-contact-task = '2'.
*    ENDIF.
*
*    ls_fax-contact-data-fax        = ps_data-fax.
*    ls_fax-contact-datax-fax       = 'X'.
*
*    ls_fax-contact-data-countryiso = ps_data-country.
*    ls_fax-contact-datax-countryiso = 'X'.
*
*    APPEND ls_fax TO ls_address-data-communication-fax-fax.
*  ENDIF.
*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > ADDRESS > ADDRESSES[] > DATA > COMMUNICATION > SMTP
*----------------------------------------------------------------------*
*  i:삽입, u:갱신, d:삭제, 1:논리 키 삽입, 2:논리 키 갱신, 4:논리 키 삭제, s:표준
*  이메일
*   if ps_data-e_mail is not initial or
*      ( ps_data-bpartner is not initial and
*        ps_data-e_mail   ne ls_address_info-smtp_addr ).
*  IF lt_smtp[] IS INITIAL.
*    ls_smtp-contact-task = 'I'.
*  ELSE.
*    ls_smtp-contact-task = '2'.
*  ENDIF.
*
*  ls_smtp-contact-data-e_mail  = ps_data-e_mail.
*  ls_smtp-contact-datax-e_mail = 'X'.
*
*  APPEND ls_smtp TO ls_address-data-communication-smtp-smtp.
*ENDIF.

*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > ADDRESS[] > DATA > COMMUNICATION > VERSION > VERSIONS[] > DATA > ORGANIZATION
*----------------------------------------------------------------------*
*ls_version-task                         = 'M'.                 "I:삽입, U:갱신, M:수정, D:삭제
*ls_version-data-organization-addr_vers  = 'I'.
*ls_version-datax-organization-addr_vers = 'X'.
*
*IF ps_data-name1 IS NOT INITIAL OR
*   ( ps_data-bpartner IS NOT INITIAL AND
*     ps_data-name1    NE ls_address_info-name1 ).
*  ls_version-data-organization-name     = ps_data-name1.       "이름1
*  ls_version-datax-organization-name    = 'X'.
*ENDIF.
*
*IF ps_data-name2 IS NOT INITIAL OR
*   ( ps_data-bpartner IS NOT INITIAL AND
*     ps_data-name2    NE ls_address_info-name2 ).
*  ls_version-data-organization-name     = ps_data-name2.       "이름2
*  ls_version-datax-organization-name    = 'X'.
*ENDIF.
*
*IF ps_data-searchterm1 IS NOT INITIAL OR
*   ( ps_data-bpartner    IS NOT INITIAL AND
*     ps_data-searchterm1 NE ls_address_info-searchterm1 ).
*  ls_version-data-organization-sort1    = ps_data-searchterm1. "검색어1
*  ls_version-datax-organization-sort1   = 'X'.
*ENDIF.
*
*IF ps_data-searchterm2 IS NOT INITIAL OR
*   ( ps_data-bpartner    IS NOT INITIAL AND
*     ps_data-searchterm1 NE ls_address_info-searchterm1 ).
*  ls_version-data-organization-sort1    = ps_data-searchterm2. "검색어2
*  ls_version-datax-organization-sort1   = 'X'.
*ENDIF.
*
*IF ps_data-city IS NOT INITIAL OR
*   ( ps_data-bpartner   IS NOT INITIAL AND
*     ps_data-city NE ls_address_info-city ).
*  ls_version-data-organization-city     = ps_data-city.        "도시
*  ls_version-datax-organization-city    = 'X'.
*ENDIF.
*
*IF ps_data-street IS NOT INITIAL OR
*   ( ps_data-bpartner IS NOT INITIAL AND
*     ps_data-street   NE ls_address_info-street ).
*  ls_version-data-organization-street   = ps_data-street.      "상세주소
*  ls_version-datax-organization-street  = 'X'.
*ENDIF.
*
*IF ps_data-house_no IS NOT INITIAL OR
*   ( ps_data-bpartner IS NOT INITIAL AND
*     ps_data-house_no NE ls_address_info-house_no ).
*  ls_version-data-organization-house_no   = ps_data-house_no.      "상세주소
*  ls_version-datax-organization-house_no  = 'X'.
*ENDIF.
*
*IF pt_inter[] IS INITIAL.
*  APPEND ls_version TO ls_address-data-version-versions.
*  CLEAR  ls_version.
*ELSE.
*
** International version
*  SORT pt_inter BY znation.
*  LOOP AT pt_inter INTO ls_inter.
*
** 전화
*    READ TABLE gt_address_info INTO DATA(ls_nation) WITH KEY nation = ls_inter-znation
*                                                             BINARY SEARCH.
*
*    IF ls_inter-ztelephone IS NOT INITIAL OR
*       ( ps_data-bpartner   IS NOT INITIAL AND
*         ls_inter-ztelephone NE ls_nation-tel_number ).
*      READ TABLE lt_tel WITH KEY r_3_user = '1' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_phone-contact-task = '2'.
*      ELSE.
*        ls_phone-contact-task = 'I'.
*      ENDIF.
*
*      IF ls_phone-contact-task NE '2'.
*        ls_phone-contact-data-telephone   = ls_inter-ztelephone.
*        ls_phone-contact-datax-telephone  = 'X'.
*
*        ls_phone-contact-data-r_3_user    = '1'.
*        ls_phone-contact-datax-r_3_user   = 'X'.
*
*        ls_phone-contact-data-countryiso  = ps_data-country.
*        ls_phone-contact-datax-countryiso = 'X'.
*
*        ls_phone-contact-data-std_no      = 'X'.
*        ls_phone-contact-datax-std_no     = 'X'.
*
*        APPEND ls_phone TO ls_address-data-communication-phone-phone.
*        CLEAR  ls_phone.
*      ENDIF.
*    ENDIF.
*
** 핸드폰
*    IF ls_inter-zmobile IS NOT INITIAL OR
*       ( ps_data-bpartner IS NOT INITIAL AND
*         ls_inter-zmobile NE ls_nation-mobile ).
*      READ TABLE lt_tel WITH KEY r_3_user = '3' BINARY SEARCH.
*      IF sy-subrc = 0.
*        ls_phone-contact-task = '2'.
*      ELSE.
*        ls_phone-contact-task = 'I'.
*      ENDIF.
*
*      IF ls_phone-contact-task NE '2'.
*        ls_phone-contact-data-telephone   = ls_inter-zmobile.
*        ls_phone-contact-datax-telephone  = 'X'.
*
*        ls_phone-contact-data-std_recip   = 'X'.
*        ls_phone-contact-datax-std_recip  = 'X'.
*
*        ls_phone-contact-data-r_3_user    = '3'.
*        ls_phone-contact-datax-r_3_user   = 'X'.
*
*        ls_phone-contact-data-countryiso  = ps_data-country.
*        ls_phone-contact-datax-countryiso = 'X'.
*
*        ls_phone-contact-data-std_no      = 'X'.
*        ls_phone-contact-datax-std_no     = 'X'.
*
*        APPEND ls_phone TO ls_address-data-communication-phone-phone.
*        CLEAR  ls_phone.
*      ENDIF.
*    ENDIF.
*
*    IF ls_inter-zfax IS NOT INITIAL OR
*       ( ps_data-bpartner IS NOT INITIAL AND
*         ls_inter-zfax    NE ls_nation-fax_number ).
*      IF lt_fax[] IS INITIAL.
*        ls_fax-contact-task = 'I'.
*      ELSE.
*        ls_fax-contact-task = '2'.
*      ENDIF.
*
*      ls_fax-contact-data-fax        = ls_inter-zfax.
*      ls_fax-contact-datax-fax       = 'X'.
*
*      ls_fax-contact-data-countryiso  = ps_data-country.
*      ls_fax-contact-datax-countryiso = 'X'.
*
*      APPEND ls_fax TO ls_address-data-communication-fax-fax.
*    ENDIF.
*
*
*    IF ls_inter-ze_mail IS NOT INITIAL OR
*       ( ps_data-bpartner IS NOT INITIAL AND
*         ls_inter-ze_mail NE ls_nation-smtp_addr ).
*      IF lt_smtp[] IS INITIAL.
*        ls_smtp-contact-task = 'I'.
*      ELSE.
*        ls_smtp-contact-task = '2'.
*      ENDIF.
*
*      IF ls_smtp-contact-task NE '2'.
*        ls_smtp-contact-data-e_mail  = ls_inter-ze_mail.
*        ls_smtp-contact-datax-e_mail = 'X'.
*
*        APPEND ls_smtp TO ls_address-data-communication-smtp-smtp.
*      ENDIF.
*    ENDIF.
*
*    ls_version-task                         = 'M'.                 "I:삽입, U:갱신, M:수정, D:삭제
*
*    ls_version-data-organization-addr_vers  = ls_inter-znation.      " I : International
*    ls_version-datax-organization-addr_vers = 'X'.
*
*    IF ls_inter-zname1 IS NOT INITIAL OR
*       ( ps_data-bpartner IS NOT INITIAL AND
*         ls_inter-zname1 NE ls_nation-name1 ).
*      ls_version-data-organization-name     = ls_inter-zname1.       "이름1
*      ls_version-datax-organization-name    = 'X'.
*    ENDIF.
*
*    IF ls_inter-zname2 IS NOT INITIAL OR
*       ( ps_data-bpartner IS NOT INITIAL AND
*         ls_inter-zname2 NE ls_nation-name2 ).
*      ls_version-data-organization-name_2     = ls_inter-zname2.       "이름2
*      ls_version-datax-organization-name_2    = 'X'.
*    ENDIF.
*
*    IF ls_inter-zsearchterm1 IS NOT INITIAL OR
*       ( ps_data-bpartner IS NOT INITIAL AND
*         ls_inter-zsearchterm1  NE ls_nation-searchterm1 ).
*      ls_version-data-organization-sort1    = ls_inter-zsearchterm1. "검색어1
*      ls_version-datax-organization-sort1   = 'X'.
*    ENDIF.
*
*    IF ls_inter-zsearchterm2 IS NOT INITIAL OR
*       ( ps_data-bpartner      IS NOT INITIAL AND
*         ls_inter-zsearchterm2 NE ls_nation-searchterm2 ).
*      ls_version-data-organization-sort2    = ls_inter-zsearchterm2. "검색어2
*      ls_version-datax-organization-sort2   = 'X'.
*    ENDIF.
*
*    IF ls_inter-zcity IS NOT INITIAL OR
*       ( ps_data-bpartner IS NOT INITIAL AND
*         ls_inter-zcity   NE ls_nation-city ).
*      ls_version-data-organization-city     = ls_inter-zcity.        "도시
*      ls_version-datax-organization-city    = 'X'.
*    ENDIF.
*
*    IF ls_inter-zstreet IS NOT INITIAL OR
*       ( ps_data-bpartner IS NOT INITIAL AND
*         ls_inter-zstreet NE ls_nation-street ).
*      ls_version-data-organization-street   = ls_inter-zstreet.      "상세주소
*      ls_version-datax-organization-street  = 'X'.
*    ENDIF.
*
*    IF ls_inter-zhouse_no IS NOT INITIAL OR
*       ( ps_data-bpartner   IS NOT INITIAL AND
*         ls_inter-zhouse_no NE ls_nation-house_no ).
*      ls_version-data-organization-house_no   = ls_inter-zhouse_no.    "상세주소
*      ls_version-datax-organization-house_no  = 'X'.
*    ENDIF.
*
*    APPEND ls_version TO ls_address-data-version-versions.
*
*    CLEAR : ls_inter.
*  ENDLOOP.
*
*ENDIF.
*
APPEND ls_address TO ps_main-partner-central_data-address-addresses.

*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > TAXNUMBER
*----------------------------------------------------------------------*
*IF pv_pguid IS INITIAL.
*  CLEAR: lr_taxtype, lt_taxtype.
*  REFRESH: lr_taxtype, lt_taxtype.
*  lr_taxtype-sign   = 'I'.
*  lr_taxtype-option = 'CP'.
*  lr_taxtype-low    = ps_data-country && '*'. "국가키
*  APPEND lr_taxtype.
*  CLEAR  lr_taxtype.
*
*  SELECT taxtype
*    INTO CORRESPONDING FIELDS OF TABLE lt_taxtype
*    FROM tfktaxnumtype_c
*   WHERE taxtype IN lr_taxtype
*   ORDER BY taxtype.
*
*    IF ps_data-taxnum1 IS NOT INITIAL.
*      READ TABLE lt_taxtype INDEX 1.
*      IF sy-subrc = 0.
*
*        SELECT SINGLE client INTO sy-mandt
*                      FROM  dfkkbptaxnum
*                      WHERE partner = pv_bpartner
*                      AND   taxtype = lt_taxtype-taxtype.
*          IF sy-subrc = 0.
*            ls_taxnumbers-task               = 'U'.                   "I:삽입, U:갱신, D:삭제
*          ELSE.
*            ls_taxnumbers-task               = 'I'.                   "I:삽입, U:갱신, D:삭제
*          ENDIF.
*          ls_taxnumbers-data_key-taxtype   = lt_taxtype-taxtype.    "세금 번호 범주
*          ls_taxnumbers-data_key-taxnumxl  = ps_data-taxnum1.       "비즈니스 파트너 세금 번호
*          ls_taxnumbers-data_key-taxnumber = ps_data-taxnum1.
*          APPEND ls_taxnumbers TO ps_main-partner-central_data-taxnumber-taxnumbers.
*        ENDIF.
*
*      ENDIF.
*
*      IF ps_data-taxnum2 IS NOT INITIAL.
*        READ TABLE lt_taxtype INDEX 2.
*        IF sy-subrc = 0.
*          SELECT SINGLE client INTO sy-mandt
*                        FROM  dfkkbptaxnum
*                        WHERE partner = pv_bpartner
*                        AND   taxtype = lt_taxtype-taxtype.
*            IF sy-subrc = 0.
*              ls_taxnumbers-task               = 'U'.                   "I:삽입, U:갱신, D:삭제
*            ELSE.
*              ls_taxnumbers-task               = 'I'.                   "I:삽입, U:갱신, D:삭제
*            ENDIF.
*            ls_taxnumbers-data_key-taxtype   = lt_taxtype-taxtype.    "세금 번호 범주
*            ls_taxnumbers-data_key-taxnumxl  = ps_data-taxnum2.       "비즈니스 파트너 세금 번호
*            ls_taxnumbers-data_key-taxnumber = ps_data-taxnum2.
*            APPEND ls_taxnumbers TO ps_main-partner-central_data-taxnumber-taxnumbers.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > ROLE
*----------------------------------------------------------------------*
* I:삽입, D:삭제
      ls_roles-task              = 'M'.           "Modify
      ls_roles-currently_valid   = 'X'.
      ls_roles-data_key          = ps_data-role.  "BP 역할

      SELECT SINGLE rolecategory INTO ls_roles-data-rolecategory  "BP 역할 범주
        FROM tb003
        WHERE role = ps_data-role.

        ls_roles-data-valid_from   = sy-datum. "SY-DATLO.
        ls_roles-datax-valid_from  = 'X'.

        ls_roles-data-valid_to     = '99991231'.
        ls_roles-datax-valid_to    = 'X'.

        APPEND ls_roles TO ps_main-partner-central_data-role-roles.
*----------------------------------------------------------------------*
* PARTNER > CENTRAL_DATA > BANKDETAIL
*----------------------------------------------------------------------*
        IF pt_bank[] IS NOT INITIAL.
          LOOP AT pt_bank.
            ls_bankdetail-task                      = 'M'.           "I:삽입, U:갱신, D:삭제, 1:논리 키 삽입, 2:논리 키 갱신, 4:논리 키 삭제
            ls_bankdetail-data_key                  = pt_bank-bkvid. "ID

            ls_bankdetail-data-bank_ctry            = pt_bank-banks. "국가
            ls_bankdetail-data-bank_key             = pt_bank-bankl. "은행키
            ls_bankdetail-data-bank_acct            = pt_bank-bankn. "은행계좌
            ls_bankdetail-data-bank_ref             = pt_bank-bkref. "Reference details
            ls_bankdetail-data-accountholder        = pt_bank-koinh. "계정 보유자 이름
            ls_bankdetail-data-ctrl_key             = pt_bank-bkont. "Bank Control Key
            ls_bankdetail-data-bankdetailvalidfrom  = sy-datlo.      "효력 시작일
            ls_bankdetail-data-bankdetailvalidto    = '99991231'.    "효력 종료일

            ls_bankdetail-datax-bank_ctry           = 'X'.
            ls_bankdetail-datax-bank_key            = 'X'.
            ls_bankdetail-datax-bank_acct           = 'X'.
            ls_bankdetail-datax-bank_ref            = 'X'.
            ls_bankdetail-datax-accountholder       = 'X'.
            ls_bankdetail-datax-ctrl_key            = 'X'.
            ls_bankdetail-datax-bankdetailvalidfrom = 'X'.
            ls_bankdetail-datax-bankdetailvalidto   = 'X'.

            APPEND ls_bankdetail TO ps_main-partner-central_data-bankdetail-bankdetails.
            CLEAR ls_bankdetail.

          ENDLOOP.

        ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TEL
*&      --> LT_FAX
*&      --> LT_SMTP
*&      --> PS_DATA_BPARTNER
*&      --> PV_BPARTNER
*&---------------------------------------------------------------------*
FORM get_address TABLES pt_tel STRUCTURE bapiadtel
                        pt_fax STRUCTURE bapiadfax
                        pt_smtp STRUCTURE bapiadsmtp
                  USING pv_bpartner1
                        pv_bpartner2.
  DATA: ls_addressdata LIKE bapibus1006_address.
  DATA: lv_bpartner LIKE bapibus1006_head-bpartner.

  REFRESH: pt_tel, pt_fax, pt_smtp.

  IF pv_bpartner1 IS NOT INITIAL.
    lv_bpartner = pv_bpartner1.
  ELSEIF pv_bpartner2 IS NOT INITIAL.
    lv_bpartner = pv_bpartner2.
  ENDIF.

  CHECK lv_bpartner IS NOT INITIAL.

  CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
    EXPORTING
      businesspartner = lv_bpartner
    IMPORTING
      addressdata     = ls_addressdata
    TABLES
      bapiadtel       = pt_tel
      bapiadfax       = pt_fax
      bapiadsmtp      = pt_smtp.

  SORT pt_tel BY r_3_user.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_CUSTOMER_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_BANK
*&      --> PS_DATA
*&      --> PV_KUNNR
*&      --> PV_LIFNR
*&      <-- PS_MAIN
*&---------------------------------------------------------------------*
FORM append_customer_data TABLES pt_part  STRUCTURE zfi1s0022
                           USING ps_data  STRUCTURE zfis2010
                                 pv_kunnr
                                 pv_lifnr
                        CHANGING ps_main TYPE cvis_ei_extern.
  DATA: ls_company TYPE cmds_ei_company,
        ls_sales   TYPE cmds_ei_sales.

  CASE ps_data-role.
    WHEN TEXT-001. " Customer
    WHEN OTHERS.
      EXIT.
  ENDCASE.
*----------------------------------------------------------------------*
* CUSTOMER > HEADER
*----------------------------------------------------------------------*
* 생성시: I, 변경시: U, M: 지원되지 않음
  IF pv_kunnr IS INITIAL.
    ps_main-customer-header-object_task = 'I'.
  ELSE.
    ps_main-customer-header-object_task = 'U'.
  ENDIF.
* 비즈니스 파트너 번호
  ps_main-customer-header-object_instance-kunnr = ps_data-bpartner.
*----------------------------------------------------------------------*
* CUSTOMER > CENTRAL_DATA
*----------------------------------------------------------------------*
* 대표 이름
  IF ps_data-j_1kfrepre IS NOT INITIAL.
    ps_main-customer-central_data-central-data-j_1kfrepre  = ps_data-j_1kfrepre.
    ps_main-customer-central_data-central-datax-j_1kfrepre = 'X'.
  ENDIF.
* 종목
  IF ps_data-j_1kftbus IS NOT INITIAL.
    ps_main-customer-central_data-central-data-j_1kftbus  = ps_data-j_1kftbus.
    ps_main-customer-central_data-central-datax-j_1kftbus = 'X'.
  ENDIF.
* 업태
  IF ps_data-j_1kftind IS NOT INITIAL.
    ps_main-customer-central_data-central-data-j_1kftind  = ps_data-j_1kftind.
    ps_main-customer-central_data-central-datax-j_1kftind = 'X'.
  ENDIF.
* 관계사의 회사 ID
  IF ps_data-vbund IS NOT INITIAL.
    ps_main-customer-central_data-central-data-vbund  = ps_data-vbund.
    ps_main-customer-central_data-central-datax-vbund = 'X'.
  ENDIF.
* 공급업체
  IF pv_lifnr IS NOT INITIAL.
    ps_main-customer-central_data-central-data-lifnr  = pv_lifnr.
    ps_main-customer-central_data-central-datax-lifnr = 'X'.
  ENDIF.
*----------------------------------------------------------------------*
* CUSTOMER > COMPANY_DATA
*----------------------------------------------------------------------*
* 조정계정
  IF ps_data-akont IS NOT INITIAL.
    ls_company-data-akont  = ps_data-akont.
    ls_company-datax-akont = 'X'.
  ENDIF.
* 지급조건
  IF ps_data-zterm IS NOT INITIAL.
    ls_company-data-zterm  = ps_data-zterm.
    ls_company-datax-zterm = 'X'.
  ENDIF.
* 지급방법
  IF ps_data-zwels IS NOT INITIAL.
    ls_company-data-zwels  = ps_data-zwels.
    ls_company-datax-zwels = 'X'.
  ENDIF.
* 정렬키
  IF ps_data-zuawa IS NOT INITIAL.
    ls_company-data-zuawa  = ps_data-zuawa.
    ls_company-datax-zuawa = 'X'.
  ENDIF.
* 대변메모에 대한 지급조건키
  IF ps_data-guzte IS NOT INITIAL.
    ls_company-data-guzte  = ps_data-guzte.
    ls_company-datax-guzte = 'X'.
  ENDIF.
** 반제(공급업체 지정)
*  ls_company-data-xverr  = 'X'.  "공급업체 반제
*  ls_company-datax-xverr = 'X'.
* 이전계정번호 (이전 마스터 레코드 번호)
  IF ps_data-altkn IS NOT INITIAL.
    ls_company-data-altkn  = ps_data-altkn.
    ls_company-datax-altkn = 'X'.
  ENDIF.

  IF ls_company IS NOT INITIAL.
    ls_company-task           = 'M'.            "Modify
    ls_company-data_key-bukrs = ps_data-bukrs.  "회사코드

    APPEND ls_company TO ps_main-customer-company_data-company.
  ENDIF.
*----------------------------------------------------------------------*
* CUSTOMER > SALES
*----------------------------------------------------------------------*
  IF ps_data-vkorg IS NOT INITIAL AND
     ps_data-vtweg IS NOT INITIAL AND
     ps_data-spart IS NOT INITIAL.

    IF ps_data-role = TEXT-001. "고객
      ls_sales-task           = 'M'.            "Modify
      ls_sales-data_key-vkorg = ps_data-vkorg.  "영업 조직
      ls_sales-data_key-vtweg = ps_data-vtweg.  "유통 경로
      ls_sales-data_key-spart = ps_data-spart.  "제품군
* 영업소
      IF ps_data-vkbur IS NOT INITIAL.
        ls_sales-data-vkbur  = ps_data-vkbur.
        ls_sales-datax-vkbur = 'X'.
      ENDIF.
* 판매구역
      IF ps_data-bzirk IS NOT INITIAL.
        ls_sales-data-bzirk  = ps_data-bzirk.
        ls_sales-datax-bzirk = 'X'.
      ENDIF.
* 고객그룹
      IF ps_data-kdgrp IS NOT INITIAL.
        ls_sales-data-kdgrp  = ps_data-kdgrp.
        ls_sales-datax-kdgrp = 'X'.
      ENDIF.
* Customer group 1
      IF ps_data-kvgr1 IS NOT INITIAL.
        ls_sales-data-kvgr1 = ps_data-kvgr1.
        ls_sales-datax-kvgr1 = 'X'.
      ENDIF.
* Customer group 2
      IF ps_data-kvgr2 IS NOT INITIAL.
        ls_sales-data-kvgr2 = ps_data-kvgr2.
        ls_sales-datax-kvgr2 = 'X'.
      ENDIF.
* Customer group 3
      IF ps_data-kvgr3 IS NOT INITIAL.
        ls_sales-data-kvgr3 = ps_data-kvgr3.
        ls_sales-datax-kvgr3 = 'X'.
      ENDIF.
* Customer group 5
      IF ps_data-kvgr5 IS NOT INITIAL.
        ls_sales-data-kvgr5 = ps_data-kvgr5.
        ls_sales-datax-kvgr5 = 'X'.
      ENDIF.
* 통화
      IF ps_data-waers_c IS NOT INITIAL.
        ls_sales-data-waers  = ps_data-waers_c.
        ls_sales-datax-waers = 'X'.
      ENDIF.
* 고객 가격 그룹
      IF ps_data-konda IS NOT INITIAL.
        ls_sales-data-konda  = ps_data-konda.
        ls_sales-datax-konda = 'X'.
      ENDIF.
* 환율유형
      IF ps_data-kurst IS NOT INITIAL.
        ls_sales-data-kurst  = ps_data-kurst.
        ls_sales-datax-kurst = 'X'.
      ENDIF.
* 고객가격절차
      IF ps_data-kalks IS NOT INITIAL.
        ls_sales-data-kalks  = ps_data-kalks.
        ls_sales-datax-kalks = 'X'.
      ENDIF.
* 고객 통계 그룹
      IF ps_data-versg IS NOT INITIAL.
        ls_sales-data-versg  = ps_data-versg.
        ls_sales-datax-versg = 'X'.
      ENDIF.
* 오더확률
      IF ps_data-awahr IS NOT INITIAL.
        ls_sales-data-awahr  = ps_data-awahr.
        ls_sales-datax-awahr = 'X'.
      ENDIF.
* 오더조합
      IF ps_data-kzazu IS NOT INITIAL.
        ls_sales-data-kzazu  = ps_data-kzazu.
        ls_sales-datax-kzazu = 'X'.
      ENDIF.
* 납품플랜트
      IF ps_data-vwerk IS NOT INITIAL.
        ls_sales-data-vwerk  = ps_data-vwerk.
        ls_sales-datax-vwerk = 'X'.
      ENDIF.
* 출하조건
      IF ps_data-vsbed IS NOT INITIAL.
        ls_sales-data-vsbed  = ps_data-vsbed.
        ls_sales-datax-vsbed = 'X'.
      ENDIF.
* 최대분할납품
      IF ps_data-antlf IS NOT INITIAL.
        ls_sales-data-antlf  = ps_data-antlf.
        ls_sales-datax-antlf = 'X'.
      ENDIF.
* 계정지정그룹
      IF ps_data-ktgrd IS NOT INITIAL.
        ls_sales-data-ktgrd  = ps_data-ktgrd.
        ls_sales-datax-ktgrd = 'X'.
      ENDIF.
* 지급조건
      IF ps_data-zterm_c IS NOT INITIAL.
        ls_sales-data-zterm  = ps_data-zterm_c.
        ls_sales-datax-zterm = 'X'.
      ENDIF.
* 인도조건1
      IF ps_data-inco1_c IS NOT INITIAL.
        ls_sales-data-inco1  = ps_data-inco1_c.
        ls_sales-datax-inco1 = 'X'.
      ENDIF.
* 인도조건2
      IF ps_data-inco2_c IS NOT INITIAL.
        ls_sales-data-inco2  = ps_data-inco2_c.
        ls_sales-datax-inco2 = 'X'.
      ENDIF.
* 파트너역할
      PERFORM functions_partner_c TABLES pt_part
                                CHANGING ps_data
                                         ls_sales.

      APPEND ls_sales TO ps_main-customer-sales_data-sales.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FUNCTIONS_PARTNER_C
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PS_DATA
*&      <-- LS_SALES
*&---------------------------------------------------------------------*
FORM functions_partner_c TABLES pt_part STRUCTURE zfi1s0022
                       CHANGING ps_data STRUCTURE zfis2010
                                ps_sales TYPE cmds_ei_sales.
  DATA: lt_knvp LIKE knvp OCCURS 0 WITH HEADER LINE.
  DATA: ls_functions TYPE cmds_ei_functions.
*----------------------------------------------------------------------*
* SP(판매처) --> AG : 변경안됨
* BP(청구처) --> RE
* PY(지급인) --> RG
* SH(납품처) --> WE
*----------------------------------------------------------------------*
* 고객마스터 파트너기능
  SELECT kunnr vkorg vtweg spart parvw parza kunn2
     INTO CORRESPONDING FIELDS OF TABLE lt_knvp
     FROM knvp
    WHERE kunnr = ps_data-bpartner
      AND vkorg = ps_data-vkorg
      AND vtweg = ps_data-vtweg
      AND spart = ps_data-spart
    ORDER BY kunnr vkorg vtweg spart parvw.
*----------------------------------------------------------------------*
* SP(판매처) : 생성시에만
*----------------------------------------------------------------------*
    READ TABLE lt_knvp WITH KEY kunnr = ps_data-bpartner
                                vkorg = ps_data-vkorg
                                vtweg = ps_data-vtweg
                                spart = ps_data-spart
                                parvw = 'AG'
                        BINARY SEARCH.
    IF sy-subrc NE 0.
      ls_functions-task           = 'M'.
      ls_functions-data_key-parvw = 'AG'.
      ls_functions-data-partner   = ps_data-bpartner.
      APPEND ls_functions TO ps_sales-functions-functions.

    ENDIF.
*----------------------------------------------------------------------*
* 그외의 파트너 타입 생성
*----------------------------------------------------------------------*
    IF pt_part[] IS NOT INITIAL.
      LOOP AT pt_part.
        IF pt_part-partner IS NOT INITIAL.
          ls_functions-task           = 'M'.
          ls_functions-data_key-parvw = pt_part-parvw. " 'RE'.
          ls_functions-data-partner   = pt_part-partner.

          READ TABLE lt_knvp WITH KEY kunnr = ps_data-bpartner
                                      vkorg = ps_data-vkorg
                                      vtweg = ps_data-vtweg
                                      spart = ps_data-spart
                                      parvw = pt_part-parvw
                             BINARY SEARCH.
          IF sy-subrc = 0.
            ls_functions-data_key-parza = lt_knvp-parza.
          ENDIF.

          APPEND ls_functions TO ps_sales-functions-functions.
          CLEAR  ls_functions.
        ENDIF.

      ENDLOOP.

    ELSE.
      ls_functions-task           = 'M'.
      ls_functions-data_key-parvw = 'RE'.
      ls_functions-data-partner   = ps_data-bpartner.
      APPEND ls_functions TO ps_sales-functions-functions.

      ls_functions-task           = 'M'.
      ls_functions-data_key-parvw = 'WE'.
      ls_functions-data-partner   = ps_data-bpartner.
      APPEND ls_functions TO ps_sales-functions-functions.

      ls_functions-task           = 'M'.
      ls_functions-data_key-parvw = 'RG'.
      ls_functions-data-partner   = ps_data-bpartner.
      APPEND ls_functions TO ps_sales-functions-functions.

    ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_VENDOR_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PS_DATA
*&      --> PV_KUNNR
*&      --> PV_LIFNR
*&      <-- PS_MAIN
*&---------------------------------------------------------------------*
FORM append_vendor_data USING ps_data STRUCTURE zfis2010
                              pv_kunnr
                              pv_lifnr
                     CHANGING ps_main TYPE cvis_ei_extern.

  DATA: ls_company    TYPE vmds_ei_company,
        ls_purchasing TYPE vmds_ei_purchasing.

  DATA: ls_functions  TYPE vmds_ei_functions.

  CASE ps_data-role.
    WHEN TEXT-002.    " Vendor
    WHEN OTHERS.
      EXIT.
  ENDCASE.
*----------------------------------------------------------------------*
* VENDOR > HEADER
*----------------------------------------------------------------------*
* 생성시: I, 변경시: U, M: 지원되지 않음
  IF pv_lifnr IS INITIAL.
    ps_main-vendor-header-object_task = 'I'.
  ELSE.
    ps_main-vendor-header-object_task = 'U'.
  ENDIF.

  ps_main-vendor-header-object_instance-lifnr = ps_data-bpartner. "비즈니스 파트너 번호
*----------------------------------------------------------------------*
* VENDOR > CENTRAL_DATA
*----------------------------------------------------------------------*
* 대표 이름
  IF ps_data-j_1kfrepre IS NOT INITIAL.
    ps_main-vendor-central_data-central-data-j_1kfrepre  = ps_data-j_1kfrepre.
    ps_main-vendor-central_data-central-datax-j_1kfrepre = 'X'.
  ENDIF.
* 종목
  IF ps_data-j_1kftbus IS NOT INITIAL.
    ps_main-vendor-central_data-central-data-j_1kftbus  = ps_data-j_1kftbus.
    ps_main-vendor-central_data-central-datax-j_1kftbus = 'X'.
  ENDIF.
* 업태
  IF ps_data-j_1kftind IS NOT INITIAL.
    ps_main-vendor-central_data-central-data-j_1kftind  = ps_data-j_1kftind.
    ps_main-vendor-central_data-central-datax-j_1kftind = 'X'.
  ENDIF.
* 관계사의 회사 ID
  IF ps_data-vbund IS NOT INITIAL.
    ps_main-vendor-central_data-central-data-vbund  = ps_data-vbund.
    ps_main-vendor-central_data-central-datax-vbund = 'X'.
  ENDIF.
* 고객
  IF pv_kunnr IS NOT INITIAL.
    ps_main-vendor-central_data-central-data-kunnr  = pv_kunnr.
    ps_main-vendor-central_data-central-datax-kunnr = 'X'.
  ENDIF.
*----------------------------------------------------------------------*
* VENDOR > COMPANY_DATA
*----------------------------------------------------------------------*
* 조정계정
  IF ps_data-akont IS NOT INITIAL.
    ls_company-data-akont  = ps_data-akont.
    ls_company-datax-akont = 'X'.
  ENDIF.
* 지급조건
  IF ps_data-zterm IS NOT INITIAL.
    ls_company-data-zterm  = ps_data-zterm.
    ls_company-datax-zterm = 'X'.
  ENDIF.
* 지급방법
  IF ps_data-zwels IS NOT INITIAL.
    ls_company-data-zwels  = ps_data-zwels.
    ls_company-datax-zwels = 'X'.
  ENDIF.
* 정렬키
  IF ps_data-zuawa IS NOT INITIAL.
    ls_company-data-zuawa  = ps_data-zuawa.
    ls_company-datax-zuawa = 'X'.
  ENDIF.
* 이중 송장 점검
  ls_company-data-reprf  = 'X'.
  ls_company-datax-reprf = 'X'.
** 고객에 의한 반제
*  ls_company-data-xverr  = 'X'.
*  ls_company-datax-xverr = 'X'.
* 이전계정번호 (이전 마스터 레코드 번호)
  IF ps_data-altkn IS NOT INITIAL.
    ls_company-data-altkn  = ps_data-altkn.
    ls_company-datax-altkn = 'X'.
  ENDIF.
* 대변메모에 대한 지급조건키
  IF ps_data-guzte IS NOT INITIAL.
    ls_company-data-guzte  = ps_data-guzte.
    ls_company-datax-guzte = 'X'.
  ENDIF.

  IF ls_company IS NOT INITIAL.
    ls_company-task           = 'M'.            "I:생성, U:변경, M:생성/변경
    ls_company-data_key-bukrs = ps_data-bukrs.  "회사코드

    APPEND ls_company TO ps_main-vendor-company_data-company.
  ENDIF.
*----------------------------------------------------------------------*
* VENDOR > PURCHASING
*----------------------------------------------------------------------*
  IF ps_data-ekorg IS NOT INITIAL.
    ls_purchasing-task           = 'M'.            "I:생성, U:변경, M:생성/변경
    ls_purchasing-data_key-ekorg = ps_data-ekorg.  "구매조직
* 오더통화
    IF ps_data-waers_v IS NOT INITIAL.
      ls_purchasing-data-waers  = ps_data-waers_v.
      ls_purchasing-datax-waers = 'X'.
    ENDIF.
* 지급조건
    IF ps_data-zterm_v IS NOT INITIAL.
      ls_purchasing-data-zterm  = ps_data-zterm_v.
      ls_purchasing-datax-zterm = 'X'.
    ENDIF.
* 인도조건1
    IF ps_data-inco1_v IS NOT INITIAL.
      ls_purchasing-data-inco1  = ps_data-inco1_v.
      ls_purchasing-datax-inco1 = 'X'.
    ENDIF.
* 인도조건2
    IF ps_data-inco2_v IS NOT INITIAL.
      ls_purchasing-data-inco2  = ps_data-inco2_v.
      ls_purchasing-datax-inco2 = 'X'.
    ENDIF.
* E-MAIL
    IF ps_data-verkf IS NOT INITIAL.
      ls_purchasing-data-verkf  = ps_data-verkf.
      ls_purchasing-datax-verkf = 'X'.
    ENDIF.
* 전화
    IF ps_data-telf1 IS NOT INITIAL.
      ls_purchasing-data-telf1  = ps_data-telf1.
      ls_purchasing-datax-telf1 = 'X'.
    ENDIF.
* GR기준송장검증
    IF ps_data-webre IS NOT INITIAL.
      ls_purchasing-data-webre  = ps_data-webre.
      ls_purchasing-datax-webre = 'X'.
    ENDIF.
* 자동구매오더
    IF ps_data-kzaut IS NOT INITIAL.
      ls_purchasing-data-kzaut  = ps_data-kzaut.
      ls_purchasing-datax-kzaut = 'X'.
    ENDIF.
*    구매그룹
    IF ps_data-ekgrp IS NOT INITIAL.
      ls_purchasing-data-ekgrp = ps_data-ekgrp.
      ls_purchasing-datax-ekgrp = 'X'.
    ENDIF.
*    계획납품소요시간
    IF ps_data-plifz IS NOT INITIAL.
      ls_purchasing-data-plifz = ps_data-plifz.
      ls_purchasing-datax-plifz = 'X'.
    ENDIF.
*    스키마그룹 업체
    IF ps_data-kalsk IS NOT INITIAL.
      ls_purchasing-data-kalsk = ps_data-kalsk.
      ls_purchasing-datax-kalsk = 'X'.
    ENDIF.
*    확인관리
    IF ps_data-bstae IS NOT INITIAL.
      ls_purchasing-data-bstae = ps_data-bstae.
      ls_purchasing-datax-bstae = 'X'.
    ENDIF.



    ls_functions-task           = 'M'.
    ls_functions-data_key-parvw = 'BA'.
    ls_functions-data-partner   = ps_data-bpartner.
    APPEND ls_functions TO ls_purchasing-functions-functions.

    ls_functions-task           = 'M'.
    ls_functions-data_key-parvw = 'LF'.
    ls_functions-data-partner   = ps_data-bpartner.
    APPEND ls_functions TO ls_purchasing-functions-functions.

    ls_functions-task           = 'M'.
    ls_functions-data_key-parvw = 'RS'.
    ls_functions-data-partner   = ps_data-bpartner.
    APPEND ls_functions TO ls_purchasing-functions-functions.

    APPEND ls_purchasing TO ps_main-vendor-purchasing_data-purchasing.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CREATE_CV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PS_DATA
*&      --> PV_KUNNR
*&      --> PV_LIFNR
*&      <-- LS_MAIN
*&---------------------------------------------------------------------*
FORM set_create_cv USING ps_data STRUCTURE zfis2010
                         pv_kunnr
                         pv_lifnr
                CHANGING ps_main TYPE cvis_ei_extern.

  CASE ps_data-role.
    WHEN 'ZZCUST'.    "Customer
      IF pv_kunnr IS INITIAL.
        ps_main-ensure_create-create_customer = 'X'.
      ENDIF.

    WHEN 'ZZVEND'.    "Vendor
      IF pv_lifnr IS INITIAL.
        ps_main-ensure_create-create_vendor = 'X'.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_INTERNATIONAL_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_BPARTNER
*&---------------------------------------------------------------------*
FORM update_international_address TABLES   pt_inter STRUCTURE zmds0061
                                  USING    pv_bpartner.

  DATA : ls_inter LIKE zmds0061.
  DATA : lv_bpartner  LIKE bapibus1006_head-bpartner.
  DATA : ls_address   LIKE bapibus1006_address.
  DATA : ls_address_x LIKE bapibus1006_address_x.

  DATA : lt_bapiadvers LIKE TABLE OF bapiad3vd  WITH HEADER LINE,
         lt_bapiadtel  LIKE TABLE OF bapiadtel  WITH HEADER LINE,
         lt_bapiadfax  LIKE TABLE OF bapiadfax  WITH HEADER LINE,
         lt_bapiadsmtp LIKE TABLE OF bapiadsmtp WITH HEADER LINE.

  DATA : lt_bapiadversx LIKE TABLE OF bapiad3vdx  WITH HEADER LINE,
         lt_bapiadtelx  LIKE TABLE OF bapiadtelx  WITH HEADER LINE,
         lt_bapiadfaxx  LIKE TABLE OF bapiadfaxx  WITH HEADER LINE,
         lt_bapiadsmtpx LIKE TABLE OF bapiadsmtx  WITH HEADER LINE.

  DATA : lt_return LIKE TABLE OF bapiret2 WITH HEADER LINE .

  CLEAR : lv_bpartner.
  lv_bpartner = pv_bpartner.

  LOOP AT pt_inter INTO ls_inter.

    CLEAR : lt_bapiadvers, lt_bapiadversx,
            lt_bapiadtel,  lt_bapiadtelx,
            lt_bapiadfax,  lt_bapiadfaxx,
            lt_bapiadsmtp, lt_bapiadsmtpx.

    lt_bapiadvers-addr_vers = ls_inter-znation.
    APPEND lt_bapiadvers.

    lt_bapiadvers-addr_vers = 'X'.
    APPEND lt_bapiadversx.

    lt_bapiadtelx-telephone  = 'X'.
    lt_bapiadtelx-r_3_user   = 'X'.
    lt_bapiadtelx-updateflag = 'U'.
    APPEND lt_bapiadtelx.

    lt_bapiadtel-telephone = ls_inter-ztelephone .
    lt_bapiadtel-r_3_user  = ls_inter-zmobile .
    APPEND lt_bapiadtel.

    lt_bapiadfaxx-fax        = 'X'.
    lt_bapiadfaxx-updateflag = 'U'.
    APPEND lt_bapiadfaxx.

    lt_bapiadfax-fax  = ls_inter-zfax .
    APPEND lt_bapiadfax.

    lt_bapiadsmtpx-e_mail  = 'X'.
    lt_bapiadsmtpx-updateflag = 'U'.
    APPEND lt_bapiadsmtpx.

    lt_bapiadsmtp-e_mail  = ls_inter-ze_mail .
    APPEND lt_bapiadsmtp.

    CLEAR : ls_inter.

  ENDLOOP.

  CALL FUNCTION 'BAPI_BUPR_CONTP_ADDR_CHANGE'
    EXPORTING
      businesspartner = lv_bpartner
      contactperson   = lv_bpartner
*     ADDRESSGUID     =
*     ADDRESSDATA     =
*     ADDRESSDATA_X   =
*     DUPLICATE_MESSAGE_TYPE       =
    TABLES
      bapiadvers      = lt_bapiadvers
      bapiadtel       = lt_bapiadtel
      bapiadfax       = lt_bapiadfax
      bapiadsmtp      = lt_bapiadsmtp
      bapiadvers_x    = lt_bapiadversx
      bapiadtel_x     = lt_bapiadtelx
      bapiadfax_x     = lt_bapiadfaxx
      bapiadsmt_x     = lt_bapiadsmtpx
      return          = lt_return.




ENDFORM.
