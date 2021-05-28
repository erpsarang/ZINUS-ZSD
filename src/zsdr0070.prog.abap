*&---------------------------------------------------------------------*
*& Report ZSDR0070
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program ID  : ZSDR0070                                               *
* Title       : Shipment Cost Management                               *
* Author      : Han Sang Yeul                                          *
* Create Date : 2020.12.04                                             *
* T_CODE      : ZSDR0070                                               *
* Description : 기 생성 Shipment에 대한 Shipment Cost를 생성하고 정산  *
*                 - Shipment Cost 등록 (T-code: VI01/VI02)             *
*                 - 송장 입력          (T-code: MIRO)                  *
*                 - 송장 취소          (T-code: MR8M)                  *
* Interface   :                                                        *
*----------------------------------------------------------------------*
* Date         Authors        Description                              *
*----------------------------------------------------------------------*
* 2020.12.04   Han Sang Yeul  Initial Release                          *
*----------------------------------------------------------------------*
REPORT zsdr0070 MESSAGE-ID zmsd NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE zsdr0070_top.
INCLUDE zsdr0070_scr.
INCLUDE zsdr0070_cls.
INCLUDE zsdr0070_o01.
INCLUDE zsdr0070_f01.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT -
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.


*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN F4
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tdlnr-low.
  PERFORM forward_agent_f4 CHANGING s_tdlnr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tdlnr-high.
  PERFORM forward_agent_f4 CHANGING s_tdlnr-high.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_shipment_cost TABLES gt_main.
*----------------------------------------------------------------------*
* END-OF-SLELCTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF gt_main[] IS NOT INITIAL OR gt_main[] IS NOT INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE s004.
  ENDIF.
