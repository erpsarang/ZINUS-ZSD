function zsd_post_delivery .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VBELN) TYPE  VBELN_VL
*"     VALUE(I_BUDAT) TYPE  BUDAT
*"     VALUE(I_CANCEL) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_TEST) TYPE  CHAR01 OPTIONAL
*"----------------------------------------------------------------------

  data: lv_date type d,
        lv_time type t.

  replace '&D' with sy-datum into jobname_gr.
  replace '&T' with sy-uzeit  into jobname_gr.
  replace '&H' with syst-host into jobname_gr.
  replace '&X' with i_vbeln into jobname_gr.
  translate jobname_gr to upper case.

  "Batch Open
  perform open_job using  jobname_gr i_test.


  perform submit_post_delivery using i_vbeln i_budat i_cancel.

  "Batch Close
  perform job_close using jobname_gr lv_date lv_time.





endfunction.
