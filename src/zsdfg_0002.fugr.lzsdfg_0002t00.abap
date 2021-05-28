*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2021.01.13 at 17:25:08
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZSDT0030........................................*
DATA:  BEGIN OF STATUS_ZSDT0030                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0030                      .
CONTROLS: TCTRL_ZSDT0030
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZSDT0040........................................*
DATA:  BEGIN OF STATUS_ZSDT0040                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0040                      .
CONTROLS: TCTRL_ZSDT0040
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZSDT0050........................................*
DATA:  BEGIN OF STATUS_ZSDT0050                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0050                      .
CONTROLS: TCTRL_ZSDT0050
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZSDT0060........................................*
DATA:  BEGIN OF STATUS_ZSDT0060                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0060                      .
CONTROLS: TCTRL_ZSDT0060
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZSDT0100........................................*
DATA:  BEGIN OF STATUS_ZSDT0100                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0100                      .
CONTROLS: TCTRL_ZSDT0100
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZSDT0120........................................*
DATA:  BEGIN OF STATUS_ZSDT0120                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0120                      .
CONTROLS: TCTRL_ZSDT0120
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZSDV0041........................................*
TABLES: ZSDV0041, *ZSDV0041. "view work areas
CONTROLS: TCTRL_ZSDV0041
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZSDV0041. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDV0041.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDV0041_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDV0041.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDV0041_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDV0041_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDV0041.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDV0041_TOTAL.

*...processing: ZSDV0042........................................*
TABLES: ZSDV0042, *ZSDV0042. "view work areas
CONTROLS: TCTRL_ZSDV0042
TYPE TABLEVIEW USING SCREEN '0003'.
DATA: BEGIN OF STATUS_ZSDV0042. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDV0042.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDV0042_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDV0042.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDV0042_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDV0042_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDV0042.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDV0042_TOTAL.

*...processing: ZSDV0090........................................*
TABLES: ZSDV0090, *ZSDV0090. "view work areas
CONTROLS: TCTRL_ZSDV0090
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_ZSDV0090. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDV0090.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDV0090_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDV0090.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDV0090_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDV0090_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDV0090.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDV0090_TOTAL.

*.........table declarations:.................................*
TABLES: *ZSDT0030                      .
TABLES: *ZSDT0040                      .
TABLES: *ZSDT0050                      .
TABLES: *ZSDT0060                      .
TABLES: *ZSDT0100                      .
TABLES: *ZSDT0120                      .
TABLES: T179                           .
TABLES: T179T                          .
TABLES: ZSDT0030                       .
TABLES: ZSDT0040                       .
TABLES: ZSDT0041                       .
TABLES: ZSDT0042                       .
TABLES: ZSDT0050                       .
TABLES: ZSDT0060                       .
TABLES: ZSDT0090                       .
TABLES: ZSDT0100                       .
TABLES: ZSDT0120                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
