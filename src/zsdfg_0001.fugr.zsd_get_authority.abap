FUNCTION ZSD_GET_AUTHORITY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_UNAME) TYPE  SY-UNAME DEFAULT SY-UNAME
*"  TABLES
*"      T_VKORG STRUCTURE  RANGE_VKORG OPTIONAL
*"      T_WERKS STRUCTURE  RANGE_WERKS OPTIONAL
*"--------------------------------------------------------------------

  DATA: BEGIN OF LT_ALLVKORG OCCURS 0,  VKORG TYPE VKORG,  END OF LT_ALLVKORG.
  DATA: BEGIN OF LT_ALLWERKS OCCURS 0,  WERKS TYPE WERKS_D,END OF LT_ALLWERKS.
*----// 조직정보
     SELECT VKORG
     INTO CORRESPONDING FIELDS OF TABLE LT_ALLVKORG
     FROM TVKO.

     SELECT WERKS
    INTO CORRESPONDING FIELDS OF TABLE LT_ALLWERKS
     FROM T001W
     WHERE SPRAS = SY-LANGU.
*----// 체크
  "----// 01_영업조직
  LOOP AT LT_ALLVKORG.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO' FOR USER I_UNAME ID 'VKORG' FIELD LT_ALLVKORG-VKORG.
    IF SY-SUBRC NE 0.
      DELETE LT_ALLVKORG.
    ENDIF.
  ENDLOOP.
  "----// 02_플랜트
  LOOP AT LT_ALLWERKS.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK' FOR USER I_UNAME ID 'WERKS' FIELD LT_ALLWERKS-WERKS.
    IF SY-SUBRC NE 0.
      DELETE LT_ALLWERKS.
    ENDIF.
  ENDLOOP.
*----// 리턴
  SORT: LT_ALLVKORG BY VKORG,
        LT_ALLWERKS BY WERKS.
  LOOP AT LT_ALLVKORG.
    T_VKORG-SIGN = 'I'.
    T_VKORG-OPTION = 'EQ'.
    T_VKORG-LOW = LT_ALLVKORG-VKORG.
    APPEND T_VKORG. CLEAR T_VKORG.
  ENDLOOP.

  LOOP AT LT_ALLWERKS.
    T_WERKS-SIGN = 'I'.
    T_WERKS-OPTION = 'EQ'.
    T_WERKS-LOW = LT_ALLWERKS-WERKS.
    APPEND T_WERKS. CLEAR T_WERKS.
  ENDLOOP.

ENDFUNCTION.