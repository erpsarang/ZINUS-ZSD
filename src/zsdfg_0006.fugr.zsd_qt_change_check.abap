FUNCTION ZSD_QT_CHANGE_CHECK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VBELN) TYPE  VBELN_VA
*"  TABLES
*"      T_RESULT STRUCTURE  ZSDS0100
*"----------------------------------------------------------------------
  DATA : LV_GET_MRP TYPE REF TO CL_PPH_READ_CLASSIC.
  TYPES : BEGIN OF TY_PLAF,
            MATNR    TYPE MATNR,
            WERKS    TYPE WERKS_D,
            DAT00    TYPE DAT00,
            SORT1    TYPE SORT1,
            SORT2    TYPE SORT2,
            DELKZ    TYPE DELKZ,
            VRFKZ    TYPE VRFKZ,
            PLUMI    TYPE PLUMI,
            MNG01    TYPE PPH_DEC_20_3,
            MNG02    TYPE MNG02,
            DAT01    TYPE DAT01,
            DAT02    TYPE DAT02,
            DAT03    TYPE DAT03,
            WEBAZ    TYPE WEBAZ,
            FIX01    TYPE FIX01,
            FIX02    TYPE FIX02,
            BAART    TYPE BAART,
            BESKZ    TYPE BESKZ,
            SOBES    TYPE SOBES,
            UMSKZ    TYPE UMSKZ,
            WRK01    TYPE WERKS_D,
            WRK02    TYPE WERKS_D,
            LGORT    TYPE LGORT_D,
            DELNR    TYPE DELNR,
            RSNUM    TYPE RSNUM,
            SERNR    TYPE SERNR,
            PALTR    TYPE PALTR,
            TECHS    TYPE TECHS,
            REVLV    TYPE REVLV,
            VERID    TYPE VERID,
            STLAN    TYPE STLAN,
            STALT    TYPE STALT,
            STSTA    TYPE STLST,
            KNTTP    TYPE KNTTP,
            KZVBR    TYPE KZVBR,
            SOBKZ    TYPE SOBKZ,
            KDAUF    TYPE KDAUF,
            KDPOS    TYPE KDPOS,
            PSPEL    TYPE PS_POSNR,
            CUOBJ    TYPE CUOBJ,
            AUFNR    TYPE AUFNR,
            VERTO    TYPE SA_VERTO,
            QUNUM    TYPE QUNUM,
            QUPOS    TYPE QUPOS,
            LIFNR    TYPE LIFNR,
            EKORG    TYPE EKORG,
            EBELN    TYPE EBELN,
            EBELP    TYPE EBELP,
            FRTHW    TYPE FRTHW,
            BEDID    TYPE BEDID,
            TRMER    TYPE TRMER,
            RATID    TYPE RATID,
            RATER    TYPE RATER,
            GROID    TYPE GROID,
            GROER    TYPE GROER,
            ARSNR    TYPE ARSNR,
            ARSPS    TYPE ARSPS,
            PRNKZ    TYPE PRNKZ,
            KAPFX    TYPE KAPFX,
            PSTTI    TYPE PSTTI,
            PEDTI    TYPE PEDTI,
            MONKZ    TYPE MONKZ,
            VRPLA    TYPE VRPLA,
            PBDNR    TYPE PBDNR,
            KZBWS    TYPE KZBWS,
            MDMNG    TYPE MDMNG,
            WAMNG    TYPE WAMNG,
            EDGNO    TYPE EDGNO,
            EMATN    TYPE EMATN,
            PRSCH    TYPE PRSCH,
            LVSCH    TYPE MD_LVSCHED,
            DBSKZ    TYPE DBSKZ,
            PLIFZ    TYPE PLIFZ,
            PSTMP    TYPE PSTMP,
            STAEX    TYPE MD_STAEX,
            RESLO    TYPE RESLO,
            SGT_SCAT TYPE SGT_SCAT,
            XT_LBLKZ TYPE LBLKZ,
            XT_EMLIF TYPE EMLIF,
            XT_KONNR TYPE KONNR,
            XT_KTPNR TYPE KTPNR,
            KNTTP_DB TYPE KNTTP,
            KZVBR_DB TYPE KZVBR,
            KZBWS_DB TYPE KZBWS,
          END OF TY_PLAF .
  TYPES: TT_PLAF_OUT TYPE TABLE OF TY_PLAF.
  DATA:  LT_PLAF   TYPE TT_PLAF_OUT.
  DATA:  LT_CONF   TYPE TT_PLAF_OUT.
  DATA:  LT_MATNR  TYPE TT_PLAF_OUT.
  DATA:  LT_SEL    TYPE PPH_MRP_SEL_MAT_PLANT_TAB.
  DATA:  LS_SEL    LIKE LINE OF LT_SEL.
  DATA:  LT_IMDRQX LIKE TABLE OF MDRQ.
  DATA:  LV_DELNR  TYPE DEL12.

  "생산의 confirm된 pegging정보 확인
  SELECT MATNR, WERKS, POSNR
    INTO TABLE @DATA(LT_VBAP)
    FROM VBAP
    WHERE VBELN EQ @I_VBELN.

  LOOP AT LT_VBAP INTO DATA(LS_VBAP).
    MOVE-CORRESPONDING LS_VBAP TO LS_SEL.
    APPEND LS_SEL TO LT_SEL.
    CLEAR : LS_VBAP, LS_SEL.
  ENDLOOP.

  CLEAR LT_PLAF[].
  CREATE OBJECT LV_GET_MRP.
  LV_GET_MRP->GET_MRP_ELEMENTS(
   EXPORTING
     IV_MANDT       = SY-MANDT
     IV_AGG         = ''
     IV_CUTOFF_DATE = '99991231'
     IT_SEL         = LT_SEL
   IMPORTING
     ET_PLAF_OUT    = LT_PLAF ).

  IF LT_PLAF[] IS NOT INITIAL.
    LT_CONF[] = LT_PLAF[].
    DELETE LT_CONF WHERE FIX01 NE 'X'. "CONFIRM되지 않은 데이터 삭제처리

    LT_MATNR[] = LT_PLAF[].
    SORT LT_MATNR BY MATNR.
    DELETE ADJACENT DUPLICATES FROM LT_MATNR COMPARING MATNR. "자재 중복 제거

    CLEAR T_RESULT[].
    LOOP AT LT_MATNR INTO DATA(LS_MATNR).
      LOOP AT LT_CONF INTO DATA(LS_CONF) WHERE MATNR = LS_MATNR-MATNR.
        "DELNR TYPE 변경 CHAR10 -> CHAR12
        CLEAR LV_DELNR.
        MOVE LS_CONF-DELNR TO LV_DELNR.

        "Plan order no로 function 호출
        CLEAR LT_IMDRQX.
        CALL FUNCTION 'MD_PEGGING_NODIALOG'
          EXPORTING
            EDELKZ = LS_CONF-DELKZ
            EDELNR = LV_DELNR
            EMATNR = LS_CONF-MATNR
            EWERKS = LS_CONF-WRK01
            EPLWRK = LS_CONF-WRK02
            EPLAAB = '02'
          TABLES
            IMDRQX = LT_IMDRQX.

        "해당 Quotation no와 delnr이 같은지 check
        READ TABLE LT_IMDRQX INTO DATA(LS_IMDRQX) WITH KEY  DELKZ = 'VB'
                                                            DELNR = I_VBELN.

        "Pegging o , confirm o
        IF SY-SUBRC EQ 0.
          READ TABLE LT_VBAP INTO LS_VBAP WITH KEY MATNR = LS_IMDRQX-MATNR.

          T_RESULT-VBELN    = I_VBELN.
          T_RESULT-POSNR    = LS_VBAP-POSNR.
          T_RESULT-DAT00    = LS_CONF-DAT00.   "Plan Order end Date
          T_RESULT-DELNR    = LV_DELNR.
          T_RESULT-DAT00_QT = LS_IMDRQX-DAT00. "QT avail Mat Date
          APPEND T_RESULT.
          CLEAR : T_RESULT, LS_IMDRQX, LS_VBAP.
          EXIT.
        ENDIF.

        CLEAR LS_CONF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFUNCTION.
