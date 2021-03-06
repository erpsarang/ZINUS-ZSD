*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 2021.01.13 at 17:24:29
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSDV0041........................................*
FORM GET_DATA_ZSDV0041.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZSDT0041 WHERE
(VIM_WHERETAB) .
    CLEAR ZSDV0041 .
ZSDV0041-MANDT =
ZSDT0041-MANDT .
ZSDV0041-ZKUNNR_IC =
ZSDT0041-ZKUNNR_IC .
ZSDV0041-ZKUNNR =
ZSDT0041-ZKUNNR .
ZSDV0041-ZKUNNR_DESC =
ZSDT0041-ZKUNNR_DESC .
ZSDV0041-REMARK =
ZSDT0041-REMARK .
ZSDV0041-ERNAM =
ZSDT0041-ERNAM .
ZSDV0041-ERDAT =
ZSDT0041-ERDAT .
ZSDV0041-ERZET =
ZSDT0041-ERZET .
ZSDV0041-AENAM =
ZSDT0041-AENAM .
ZSDV0041-AEDAT =
ZSDT0041-AEDAT .
ZSDV0041-AEZET =
ZSDT0041-AEZET .
    SELECT SINGLE * FROM ZSDT0040 WHERE
ZKUNNR_IC = ZSDT0041-ZKUNNR_IC .
    IF SY-SUBRC EQ 0.
    ENDIF.
<VIM_TOTAL_STRUC> = ZSDV0041.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZSDV0041 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZSDV0041.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZSDV0041-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZSDT0041 WHERE
  ZKUNNR_IC = ZSDV0041-ZKUNNR_IC AND
  ZKUNNR = ZSDV0041-ZKUNNR .
    IF SY-SUBRC = 0.
    DELETE ZSDT0041 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZSDT0041 WHERE
  ZKUNNR_IC = ZSDV0041-ZKUNNR_IC AND
  ZKUNNR = ZSDV0041-ZKUNNR .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZSDT0041.
    ENDIF.
ZSDT0041-MANDT =
ZSDV0041-MANDT .
ZSDT0041-ZKUNNR_IC =
ZSDV0041-ZKUNNR_IC .
ZSDT0041-ZKUNNR =
ZSDV0041-ZKUNNR .
ZSDT0041-ZKUNNR_DESC =
ZSDV0041-ZKUNNR_DESC .
ZSDT0041-REMARK =
ZSDV0041-REMARK .
ZSDT0041-ERNAM =
ZSDV0041-ERNAM .
ZSDT0041-ERDAT =
ZSDV0041-ERDAT .
ZSDT0041-ERZET =
ZSDV0041-ERZET .
ZSDT0041-AENAM =
ZSDV0041-AENAM .
ZSDT0041-AEDAT =
ZSDV0041-AEDAT .
ZSDT0041-AEZET =
ZSDV0041-AEZET .
    IF SY-SUBRC = 0.
    UPDATE ZSDT0041 ##WARN_OK.
    ELSE.
    INSERT ZSDT0041 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZSDV0041-UPD_FLAG,
STATUS_ZSDV0041-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZSDV0041.
  SELECT SINGLE * FROM ZSDT0041 WHERE
ZKUNNR_IC = ZSDV0041-ZKUNNR_IC AND
ZKUNNR = ZSDV0041-ZKUNNR .
ZSDV0041-MANDT =
ZSDT0041-MANDT .
ZSDV0041-ZKUNNR_IC =
ZSDT0041-ZKUNNR_IC .
ZSDV0041-ZKUNNR =
ZSDT0041-ZKUNNR .
ZSDV0041-ZKUNNR_DESC =
ZSDT0041-ZKUNNR_DESC .
ZSDV0041-REMARK =
ZSDT0041-REMARK .
ZSDV0041-ERNAM =
ZSDT0041-ERNAM .
ZSDV0041-ERDAT =
ZSDT0041-ERDAT .
ZSDV0041-ERZET =
ZSDT0041-ERZET .
ZSDV0041-AENAM =
ZSDT0041-AENAM .
ZSDV0041-AEDAT =
ZSDT0041-AEDAT .
ZSDV0041-AEZET =
ZSDT0041-AEZET .
    SELECT SINGLE * FROM ZSDT0040 WHERE
ZKUNNR_IC = ZSDT0041-ZKUNNR_IC .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZSDV0041 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZSDV0041-ZKUNNR_IC TO
ZSDT0041-ZKUNNR_IC .
MOVE ZSDV0041-ZKUNNR TO
ZSDT0041-ZKUNNR .
MOVE ZSDV0041-MANDT TO
ZSDT0041-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZSDT0041'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZSDT0041 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZSDT0041'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZSDV0041 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZSDT0041-MANDT =
ZSDV0041-MANDT .
ZSDT0041-ZKUNNR_IC =
ZSDV0041-ZKUNNR_IC .
ZSDT0041-ZKUNNR =
ZSDV0041-ZKUNNR .
ZSDT0041-ZKUNNR_DESC =
ZSDV0041-ZKUNNR_DESC .
ZSDT0041-REMARK =
ZSDV0041-REMARK .
ZSDT0041-ERNAM =
ZSDV0041-ERNAM .
ZSDT0041-ERDAT =
ZSDV0041-ERDAT .
ZSDT0041-ERZET =
ZSDV0041-ERZET .
ZSDT0041-AENAM =
ZSDV0041-AENAM .
ZSDT0041-AEDAT =
ZSDV0041-AEDAT .
ZSDT0041-AEZET =
ZSDV0041-AEZET .
    SELECT SINGLE * FROM ZSDT0040 WHERE
ZKUNNR_IC = ZSDT0041-ZKUNNR_IC .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
ENDFORM.
*...processing: ZSDV0042........................................*
FORM GET_DATA_ZSDV0042.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZSDT0042 WHERE
(VIM_WHERETAB) .
    CLEAR ZSDV0042 .
ZSDV0042-MANDT =
ZSDT0042-MANDT .
ZSDV0042-ZKUNNR_IC =
ZSDT0042-ZKUNNR_IC .
ZSDV0042-KUNNR =
ZSDT0042-KUNNR .
ZSDV0042-VTWEG =
ZSDT0042-VTWEG .
ZSDV0042-VKORG =
ZSDT0042-VKORG .
ZSDV0042-ZKUNNR_S =
ZSDT0042-ZKUNNR_S .
ZSDV0042-ZKUNNR_SUB =
ZSDT0042-ZKUNNR_SUB .
ZSDV0042-REMARK =
ZSDT0042-REMARK .
ZSDV0042-ERNAM =
ZSDT0042-ERNAM .
ZSDV0042-ERDAT =
ZSDT0042-ERDAT .
ZSDV0042-ERZET =
ZSDT0042-ERZET .
ZSDV0042-AENAM =
ZSDT0042-AENAM .
ZSDV0042-AEDAT =
ZSDT0042-AEDAT .
ZSDV0042-AEZET =
ZSDT0042-AEZET .
    SELECT SINGLE * FROM ZSDT0040 WHERE
ZKUNNR_IC = ZSDT0042-ZKUNNR_IC .
    IF SY-SUBRC EQ 0.
    ENDIF.
<VIM_TOTAL_STRUC> = ZSDV0042.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZSDV0042 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZSDV0042.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZSDV0042-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZSDT0042 WHERE
  ZKUNNR_IC = ZSDV0042-ZKUNNR_IC AND
  KUNNR = ZSDV0042-KUNNR AND
  VTWEG = ZSDV0042-VTWEG AND
  VKORG = ZSDV0042-VKORG .
    IF SY-SUBRC = 0.
    DELETE ZSDT0042 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZSDT0042 WHERE
  ZKUNNR_IC = ZSDV0042-ZKUNNR_IC AND
  KUNNR = ZSDV0042-KUNNR AND
  VTWEG = ZSDV0042-VTWEG AND
  VKORG = ZSDV0042-VKORG .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZSDT0042.
    ENDIF.
ZSDT0042-MANDT =
ZSDV0042-MANDT .
ZSDT0042-ZKUNNR_IC =
ZSDV0042-ZKUNNR_IC .
ZSDT0042-KUNNR =
ZSDV0042-KUNNR .
ZSDT0042-VTWEG =
ZSDV0042-VTWEG .
ZSDT0042-VKORG =
ZSDV0042-VKORG .
ZSDT0042-ZKUNNR_S =
ZSDV0042-ZKUNNR_S .
ZSDT0042-ZKUNNR_SUB =
ZSDV0042-ZKUNNR_SUB .
ZSDT0042-REMARK =
ZSDV0042-REMARK .
ZSDT0042-ERNAM =
ZSDV0042-ERNAM .
ZSDT0042-ERDAT =
ZSDV0042-ERDAT .
ZSDT0042-ERZET =
ZSDV0042-ERZET .
ZSDT0042-AENAM =
ZSDV0042-AENAM .
ZSDT0042-AEDAT =
ZSDV0042-AEDAT .
ZSDT0042-AEZET =
ZSDV0042-AEZET .
    IF SY-SUBRC = 0.
    UPDATE ZSDT0042 ##WARN_OK.
    ELSE.
    INSERT ZSDT0042 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZSDV0042-UPD_FLAG,
STATUS_ZSDV0042-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZSDV0042.
  SELECT SINGLE * FROM ZSDT0042 WHERE
ZKUNNR_IC = ZSDV0042-ZKUNNR_IC AND
KUNNR = ZSDV0042-KUNNR AND
VTWEG = ZSDV0042-VTWEG AND
VKORG = ZSDV0042-VKORG .
ZSDV0042-MANDT =
ZSDT0042-MANDT .
ZSDV0042-ZKUNNR_IC =
ZSDT0042-ZKUNNR_IC .
ZSDV0042-KUNNR =
ZSDT0042-KUNNR .
ZSDV0042-VTWEG =
ZSDT0042-VTWEG .
ZSDV0042-VKORG =
ZSDT0042-VKORG .
ZSDV0042-ZKUNNR_S =
ZSDT0042-ZKUNNR_S .
ZSDV0042-ZKUNNR_SUB =
ZSDT0042-ZKUNNR_SUB .
ZSDV0042-REMARK =
ZSDT0042-REMARK .
ZSDV0042-ERNAM =
ZSDT0042-ERNAM .
ZSDV0042-ERDAT =
ZSDT0042-ERDAT .
ZSDV0042-ERZET =
ZSDT0042-ERZET .
ZSDV0042-AENAM =
ZSDT0042-AENAM .
ZSDV0042-AEDAT =
ZSDT0042-AEDAT .
ZSDV0042-AEZET =
ZSDT0042-AEZET .
    SELECT SINGLE * FROM ZSDT0040 WHERE
ZKUNNR_IC = ZSDT0042-ZKUNNR_IC .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZSDV0042 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZSDV0042-ZKUNNR_IC TO
ZSDT0042-ZKUNNR_IC .
MOVE ZSDV0042-KUNNR TO
ZSDT0042-KUNNR .
MOVE ZSDV0042-VTWEG TO
ZSDT0042-VTWEG .
MOVE ZSDV0042-VKORG TO
ZSDT0042-VKORG .
MOVE ZSDV0042-MANDT TO
ZSDT0042-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZSDT0042'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZSDT0042 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZSDT0042'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZSDV0042 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZSDT0042-MANDT =
ZSDV0042-MANDT .
ZSDT0042-ZKUNNR_IC =
ZSDV0042-ZKUNNR_IC .
ZSDT0042-KUNNR =
ZSDV0042-KUNNR .
ZSDT0042-VTWEG =
ZSDV0042-VTWEG .
ZSDT0042-VKORG =
ZSDV0042-VKORG .
ZSDT0042-ZKUNNR_S =
ZSDV0042-ZKUNNR_S .
ZSDT0042-ZKUNNR_SUB =
ZSDV0042-ZKUNNR_SUB .
ZSDT0042-REMARK =
ZSDV0042-REMARK .
ZSDT0042-ERNAM =
ZSDV0042-ERNAM .
ZSDT0042-ERDAT =
ZSDV0042-ERDAT .
ZSDT0042-ERZET =
ZSDV0042-ERZET .
ZSDT0042-AENAM =
ZSDV0042-AENAM .
ZSDT0042-AEDAT =
ZSDV0042-AEDAT .
ZSDT0042-AEZET =
ZSDV0042-AEZET .
    SELECT SINGLE * FROM ZSDT0040 WHERE
ZKUNNR_IC = ZSDT0042-ZKUNNR_IC .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
ENDFORM.
*...processing: ZSDV0090........................................*
FORM GET_DATA_ZSDV0090.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZSDT0090 WHERE
(VIM_WHERETAB) .
    CLEAR ZSDV0090 .
ZSDV0090-MANDT =
ZSDT0090-MANDT .
ZSDV0090-ZPRODH_GROUP =
ZSDT0090-ZPRODH_GROUP .
ZSDV0090-PRODH =
ZSDT0090-PRODH .
ZSDV0090-REMARK =
ZSDT0090-REMARK .
ZSDV0090-ERNAM =
ZSDT0090-ERNAM .
ZSDV0090-ERDAT =
ZSDT0090-ERDAT .
ZSDV0090-ERZET =
ZSDT0090-ERZET .
ZSDV0090-AENAM =
ZSDT0090-AENAM .
ZSDV0090-AEDAT =
ZSDT0090-AEDAT .
ZSDV0090-AEZET =
ZSDT0090-AEZET .
    SELECT SINGLE * FROM T179 WHERE
PRODH = ZSDT0090-PRODH .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T179T WHERE
PRODH = T179-PRODH AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZSDV0090-VTEXT =
T179T-VTEXT .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM ZSDT0100 WHERE
ZPRODH_GROUP = ZSDT0090-ZPRODH_GROUP .
    IF SY-SUBRC EQ 0.
    ENDIF.
<VIM_TOTAL_STRUC> = ZSDV0090.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZSDV0090 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZSDV0090.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZSDV0090-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZSDT0090 WHERE
  ZPRODH_GROUP = ZSDV0090-ZPRODH_GROUP AND
  PRODH = ZSDV0090-PRODH .
    IF SY-SUBRC = 0.
    DELETE ZSDT0090 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZSDT0090 WHERE
  ZPRODH_GROUP = ZSDV0090-ZPRODH_GROUP AND
  PRODH = ZSDV0090-PRODH .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZSDT0090.
    ENDIF.
ZSDT0090-MANDT =
ZSDV0090-MANDT .
ZSDT0090-ZPRODH_GROUP =
ZSDV0090-ZPRODH_GROUP .
ZSDT0090-PRODH =
ZSDV0090-PRODH .
ZSDT0090-REMARK =
ZSDV0090-REMARK .
ZSDT0090-ERNAM =
ZSDV0090-ERNAM .
ZSDT0090-ERDAT =
ZSDV0090-ERDAT .
ZSDT0090-ERZET =
ZSDV0090-ERZET .
ZSDT0090-AENAM =
ZSDV0090-AENAM .
ZSDT0090-AEDAT =
ZSDV0090-AEDAT .
ZSDT0090-AEZET =
ZSDV0090-AEZET .
    IF SY-SUBRC = 0.
    UPDATE ZSDT0090 ##WARN_OK.
    ELSE.
    INSERT ZSDT0090 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZSDV0090-UPD_FLAG,
STATUS_ZSDV0090-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZSDV0090.
  SELECT SINGLE * FROM ZSDT0090 WHERE
ZPRODH_GROUP = ZSDV0090-ZPRODH_GROUP AND
PRODH = ZSDV0090-PRODH .
ZSDV0090-MANDT =
ZSDT0090-MANDT .
ZSDV0090-ZPRODH_GROUP =
ZSDT0090-ZPRODH_GROUP .
ZSDV0090-PRODH =
ZSDT0090-PRODH .
ZSDV0090-REMARK =
ZSDT0090-REMARK .
ZSDV0090-ERNAM =
ZSDT0090-ERNAM .
ZSDV0090-ERDAT =
ZSDT0090-ERDAT .
ZSDV0090-ERZET =
ZSDT0090-ERZET .
ZSDV0090-AENAM =
ZSDT0090-AENAM .
ZSDV0090-AEDAT =
ZSDT0090-AEDAT .
ZSDV0090-AEZET =
ZSDT0090-AEZET .
    SELECT SINGLE * FROM T179 WHERE
PRODH = ZSDT0090-PRODH .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T179T WHERE
PRODH = T179-PRODH AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZSDV0090-VTEXT =
T179T-VTEXT .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZSDV0090-VTEXT .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZSDV0090-VTEXT .
    ENDIF.
    SELECT SINGLE * FROM ZSDT0100 WHERE
ZPRODH_GROUP = ZSDT0090-ZPRODH_GROUP .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZSDV0090 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZSDV0090-ZPRODH_GROUP TO
ZSDT0090-ZPRODH_GROUP .
MOVE ZSDV0090-PRODH TO
ZSDT0090-PRODH .
MOVE ZSDV0090-MANDT TO
ZSDT0090-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZSDT0090'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZSDT0090 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZSDT0090'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZSDV0090 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZSDT0090-MANDT =
ZSDV0090-MANDT .
ZSDT0090-ZPRODH_GROUP =
ZSDV0090-ZPRODH_GROUP .
ZSDT0090-PRODH =
ZSDV0090-PRODH .
ZSDT0090-REMARK =
ZSDV0090-REMARK .
ZSDT0090-ERNAM =
ZSDV0090-ERNAM .
ZSDT0090-ERDAT =
ZSDV0090-ERDAT .
ZSDT0090-ERZET =
ZSDV0090-ERZET .
ZSDT0090-AENAM =
ZSDV0090-AENAM .
ZSDT0090-AEDAT =
ZSDV0090-AEDAT .
ZSDT0090-AEZET =
ZSDV0090-AEZET .
    SELECT SINGLE * FROM T179 WHERE
PRODH = ZSDT0090-PRODH .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM T179T WHERE
PRODH = T179-PRODH AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZSDV0090-VTEXT =
T179T-VTEXT .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZSDV0090-VTEXT .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZSDV0090-VTEXT .
    ENDIF.
    SELECT SINGLE * FROM ZSDT0100 WHERE
ZPRODH_GROUP = ZSDT0090-ZPRODH_GROUP .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
ENDFORM.

* base table related FORM-routines.............
INCLUDE LSVIMFTX .
