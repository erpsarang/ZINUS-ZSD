*&---------------------------------------------------------------------*
*& Include          ZSDR0070_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_tplst LIKE vttk-tplst OBLIGATORY,
            p_shtyp LIKE vttk-shtyp OBLIGATORY.
SELECT-OPTIONS: s_tknum FOR vttk-tknum,
                s_tdlnr FOR vttk-tdlnr,
                s_dtabf FOR vttk-dtabf.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-011.
PARAMETERS: p_all   TYPE c RADIOBUTTON GROUP rd1 DEFAULT 'X' USER-COMMAND rad1 MODIF ID gr1.
PARAMETERS: p_open  TYPE c RADIOBUTTON GROUP rd1 MODIF ID gr1.
PARAMETERS: p_prog  TYPE c RADIOBUTTON GROUP rd1 MODIF ID gr1.
PARAMETERS: p_comp  TYPE c RADIOBUTTON GROUP rd1 MODIF ID gr1.
SELECTION-SCREEN : END OF BLOCK b2.
SELECTION-SCREEN : END OF BLOCK b1.
PARAMETERS p_postd LIKE ekbe-budat OBLIGATORY.
