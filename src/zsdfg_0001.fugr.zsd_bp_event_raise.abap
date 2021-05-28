function zsd_bp_event_raise.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_VBELN) TYPE  VBELN_VL
*"     VALUE(I_TEST) TYPE  CHAR01 DEFAULT SPACE
*"----------------------------------------------------------------------

  data: target_date type d,
        target_time type t.
  data: lv_vkorg type vkorg.
  data: lv_zcmf01_ch(30).
  data: lv_duration type i.


  select single vkorg
    into @lv_vkorg
    from likp
    where vbeln = @i_vbeln.

  if sy-subrc ne 0.
    exit.
  else.
    select single zcmf01_ch
      into @lv_zcmf01_ch
      from zcommt0021
      where spras = @sy-langu
        and zmodule = 'SD'
        and zclass = 'SD009'
        and zcm_code1 = @lv_vkorg.


    lv_duration  = lv_zcmf01_ch.
  endif.


  call function 'TSTR_CALC_TIME'
    exporting
*     IV_CALENDARID          =
*     IV_TSTREAMID           =
*     IV_HANDLE              =
      iv_begin_datelocal_req = sy-datum
      iv_begin_timelocal_req = sy-uzeit
*     IV_BEGIN_TIMESTAMP_REQ =
*     IV_BEGIN_TIMEZONE      =
*     IV_END_TIMEZONE        =
*     IV_DURATION            =
      iv_duration_integer    = lv_duration
*     IV_DIRECTION           = '+'
*     IV_SPLIT_ALLOWED       = 'X'
    importing
*     EV_BEGIN_DATELOCAL_ACT =
*     EV_BEGIN_TIMELOCAL_ACT =
*     EV_BEGIN_TIMESTAMP_ACT =
      ev_end_datelocal       = target_date
      ev_end_timelocal       = target_time.
*     EV_END_TIMESTAMP       =
*     ES_SEGMENTS_TOUCHED_FIRST =
*     ES_SEGMENTS_TOUCHED_LAST  =
*     ET_SEGMENTS_TOUCHED    =
*        EXCEPTIONS
*     FATAL_ERROR            = 1
*     TIME_INVALID           = 2
*     TIME_MISSING           = 3
*     TSTREAM_NOT_LOADABLE   = 4
*     TSTREAM_GENERATION_ERROR  = 5
*     PARAMETER_ERROR        = 6
*     UNSPECIFIED_ERROR      = 7
*     OTHERS                 = 8
  replace '&D' with target_date into jobname.
  replace '&T' with target_time  into jobname.
  replace '&H' with syst-host into jobname.
  replace '&X' with i_vbeln into jobname.
  translate jobname to upper case.

  perform open_job using  jobname i_test.
  perform submit_step using target_date i_vbeln lv_vkorg.
  perform job_close using jobname target_date target_time.
endfunction.
