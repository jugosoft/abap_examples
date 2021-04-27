  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING 
      persnr          = pernr-pernr
    TABLES
      in_rgdir        = lt_rgdir
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.

  LOOP AT lt_rgdir INTO DATA(ls_rgdir)
                  WHERE fpbeg >= |{ p_gjahr }0101|
                    AND fpend <= |{ p_gjahr }1231|
                    AND srtza =  lc_srtza
                    AND payty =  ' '.

    CLEAR ls_payresult.

    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        clusterid                    = lc_clusterid
        employeenumber               = pernr-pernr
        sequencenumber               = ls_rgdir-seqnr
        filter_cumulations           = ' '
      CHANGING
        payroll_result               = ls_payresult
      EXCEPTIONS
        illegal_isocode_or_clusterid = 1
        error_generating_import      = 2
        import_mismatch_error        = 3
        subpool_dir_full             = 4
        no_read_authority            = 5
        no_record_found              = 6
        versions_do_not_match        = 7
        error_reading_archive        = 8
        error_reading_relid          = 9
        OTHERS                       = 10.
    IF sy-subrc = 0.
      LOOP AT ls_payresult-inter-rt INTO DATA(ls_payresult_inter_rt) WHERE lgart = lc_lgart.
        ls_odata-tg23 = ls_odata-tg23 + ls_payresult_inter_rt-betrg.
      ENDLOOP.
    ENDIF.