REPORT z_report MESSAGE-ID z_report.
TABLES pernr.

SELECTION-SCREEN BEGIN OF BLOCK bl01.
  PARAMETERS:
    p_monat TYPE monat OBLIGATORY,
    p_gjahr TYPE gjahr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl01.

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF mty_s_alv_data,
        pernr_o     TYPE p_pernr,
        seqnr_o     TYPE hrpayru_seqnr,
        wtmod_o     TYPE hrpayru_wtmod,
        paydr_o     TYPE datum,
        paydd_o     TYPE datum,
        pernr_n     TYPE p_pernr,
        seqnr_n     TYPE hrpayru_seqnr,
        wtmod_n     TYPE hrpayru_wtmod,
        paydr_n     TYPE datum,
        paydd_n     TYPE datum,
        colour_line TYPE char4,
      END OF mty_s_alv_data,
      mty_t_alv_data TYPE TABLE OF mty_s_alv_data WITH DEFAULT KEY.
    CONSTANTS:
      mc_edited_color_line     TYPE char4 VALUE 'C510', " зелёный
      mc_edited_color_line_err TYPE char4 VALUE 'C410'. "не зелёный
    CLASS-DATA:
      mt_alv_data  TYPE mty_t_alv_data.
    CLASS-METHODS:
      start_of_selection,
      get_pernr,
      end_of_selection,
      write_payroll_results
        IMPORTING
          iv_seqnr   TYPE cdseq
          is_results TYPE payru_result,
      submit_results_to_cluster.
  PRIVATE SECTION.
    CLASS-DATA:
      mo_msg_journal   TYPE REF TO z_messages_log,
      mv_paydt_corr    TYPE datum,
      mv_paydd_corr    TYPE datum,
      mv_paydd_default TYPE datum,
      mv_paydt_default TYPE datum.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD start_of_selection.

    CONSTANTS:
      lc_cl78           TYPE char1 VALUE '2',
      lc_ocrsn_default  TYPE z_table-ocrsn VALUE 'Z109',
      lc_datid_excluded TYPE iddat VALUE '01', 
      lc_molga          TYPE molga VALUE '33'.

    IF pnpbukrs IS INITIAL AND pnpwerks IS INITIAL.
      MESSAGE s010.
      LEAVE LIST-PROCESSING.
    ENDIF.

    mo_msg_journal = NEW #( ).
    DATA(lo_hrpayru_im_paydates) = NEW zcl_hrpayru_im_paydates( ).

    SELECT SINGLE value
      FROM z_table_conf
      INTO @DATA(lv_6ndfl_paydt)
      WHERE ( werks IN @pnpbukrs OR werks IN @pnpwerks )
        AND monat =  @p_monat
        AND gjahr =  @p_gjahr
        AND ocrsn =  @lc_ocrsn_default.

    mv_paydt_corr = COND #( WHEN sy-subrc = 0 THEN lv_6ndfl_paydt ).
    mv_paydd_corr = lo_hrpayru_im_paydates->set_paydd( iv_date = lv_6ndfl_paydt iv_cl78 = lc_cl78 ).

    IF ( mv_paydt_corr IS INITIAL AND mv_paydd_corr IS INITIAL ) OR mv_paydd_corr IS INITIAL.
      MESSAGE s014.
      LEAVE LIST-PROCESSING.
    ENDIF.

    SELECT SINGLE *
      FROM t549a
      INTO @DATA(ls_t549a)
      WHERE abkrs IN @pnpabkrs.

    SELECT SINGLE pdate
      FROM t549s
      INTO @DATA(lv_pdate)
      WHERE permo = @ls_t549a-permo
        AND datmo = @ls_t549a-datmo
        AND molga = @lc_molga
        AND pabrj = @p_gjahr
        AND pabrp = @p_monat
        AND datid = @lc_datid_excluded.

    mv_paydt_default = lv_pdate.
    mv_paydd_default = lo_hrpayru_im_paydates->set_paydd( iv_date = lv_pdate iv_cl78 = lc_cl78 ).

  ENDMETHOD.

  METHOD get_pernr.
    CONSTANTS:
      lc_status_excluded TYPE zstatus VALUE '1',
      lc_ocrsn_default   TYPE zocrsn VALUE 'Z109'.

    DATA:
      ls_alv_data TYPE mty_s_alv_data,
      lv_flag     TYPE abap_bool.

    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = pernr-pernr.

    CALL FUNCTION 'HR_INITIALIZE_BUFFER'
      EXPORTING
        pernr = pernr-pernr.

    SELECT SINGLE *
      FROM z_table_pay
      INTO @DATA(ls_potr)
      WHERE pernr = @pernr-pernr
        AND monat = @p_monat
        AND gjahr = @p_gjahr
        AND ocrsn = @lc_ocrsn_default
        AND ndfl  = @space.
    IF ls_potr-status = lc_status_excluded.
      RETURN.
    ENDIF.

    DATA(lcl_payroll) = NEW zcl_payroll( ).
    DATA(lv_inper) = CONV iperi( |{ p_gjahr }{ p_monat }| ).

    lcl_payroll->read_rgdir_all(
      EXPORTING
        pernr = pernr-pernr
      IMPORTING
        rgdir = DATA(lt_rgdir) ).

    LOOP AT lt_rgdir INTO DATA(ls_rgdir) WHERE inper = lv_inper
                                           AND ocrsn IS INITIAL
                                           AND inocr IS INITIAL.

      lcl_payroll->read_payroll_seqnr(
        EXPORTING
          pernr = pernr-pernr
          seqnr = ls_rgdir-seqnr
        IMPORTING
          result = DATA(ls_results) ).

      ls_alv_data-pernr_n = pernr-pernr.
      ls_alv_data-pernr_o = pernr-pernr.

      ls_alv_data-seqnr_n = pernr-pernr.
      ls_alv_data-seqnr_o = pernr-pernr.

      LOOP AT ls_results-nat-rudat ASSIGNING FIELD-SYMBOL(<ls_rudat>).
        ls_alv_data-paydd_o = ls_alv_data-paydd_n = <ls_rudat>-paydd.
        ls_alv_data-paydr_o = ls_alv_data-paydr_n = <ls_rudat>-paydr.
        ls_alv_data-wtmod_o = ls_alv_data-wtmod_n = <ls_rudat>-wtmod.
        IF <ls_rudat>-paydr = mv_paydt_default AND <ls_rudat>-paydd = mv_paydd_default.
          <ls_rudat>-paydr = mv_paydt_corr.
          <ls_rudat>-paydd = mv_paydd_corr.
          lv_flag = abap_true.
          ls_alv_data-paydr_n = mv_paydt_corr.
          ls_alv_data-paydd_n = mv_paydd_corr.
          ls_alv_data-colour_line = mc_edited_color_line.
        ELSEIF <ls_rudat>-paydr = ls_rgdir-fpend AND <ls_rudat>-paydd = mv_paydd_default.
          <ls_rudat>-paydd = mv_paydd_corr.
          lv_flag = abap_true.
          ls_alv_data-paydd_n = mv_paydd_corr.
          ls_alv_data-colour_line = mc_edited_color_line.
        ENDIF.

        APPEND ls_alv_data TO mt_alv_data.

        CLEAR:
          ls_alv_data-paydd_o,
          ls_alv_data-paydd_n,
          ls_alv_data-paydr_o,
          ls_alv_data-paydr_n,
          ls_alv_data-wtmod_o,
          ls_alv_data-wtmod_n,
          ls_alv_data-colour_line.
      ENDLOOP.

      IF lv_flag = abap_true.
        lcl_main=>write_payroll_results( iv_seqnr = ls_rgdir-seqnr is_results = ls_results ).
        CLEAR lv_flag.
      ENDIF.
      CLEAR ls_alv_data.
    ENDLOOP.

    CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = pernr-pernr.
  ENDMETHOD.

  METHOD end_of_selection.
    IF lines( mo_msg_journal->t_return ) > 0.
      mo_msg_journal->show_as_popup( ).
    ELSE.
      CALL SCREEN 0100.
    ENDIF.
  ENDMETHOD.

  METHOD write_payroll_results.
    CONSTANTS lc_cluster_id TYPE relid_pcl2 VALUE 'UR'.
    CALL FUNCTION 'PYXX_WRITE_PAYROLL_RESULT'
      EXPORTING
        clusterid                    = lc_cluster_id
        employeenumber               = pernr-pernr
        sequencenumber               = iv_seqnr
        payroll_result               = is_results
      EXCEPTIONS
        illegal_isocode_or_clusterid = 1
        error_generating_export      = 2
        export_error                 = 3
        subpool_dir_full             = 4
        no_update_authority          = 5
        incomplete_result_imported   = 6
        OTHERS                       = 7.
    IF sy-subrc <> 0.
      IF line_exists( mt_alv_data[ pernr_o = pernr-pernr seqnr_o = iv_seqnr ] ).
        mt_alv_data[ pernr_o = pernr-pernr seqnr_o = iv_seqnr ]-colour_line = mc_edited_color_line_err.
      ENDIF.
      MESSAGE e011 WITH pernr-pernr iv_seqnr INTO z_utils=>dummy.
      mo_msg_journal->add_msg_text( z_utils=>dummy ).
    ENDIF.
  ENDMETHOD.

  METHOD submit_results_to_cluster.
    CONSTANTS lc_answer_positive TYPE char1 VALUE '1'.
    DATA lv_answer TYPE char1.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = TEXT-030
        text_question  = TEXT-031
        text_button_1  = TEXT-032
        text_button_2  = TEXT-033
      IMPORTING
        answer         = lv_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF lv_answer = lc_answer_positive.
      CALL FUNCTION 'HR_FLUSH_BUFFER_UPDATE_PCLX'
        EXPORTING
          test                = space
          client              = sy-mandt
        EXCEPTIONS
          insert_error        = 1
          no_update_authority = 2
          OTHERS              = 3.
      IF sy-subrc = 0.
        MESSAGE s012.
      ELSE.
        MESSAGE e013.
      ENDIF.
    ENDIF.
  ENDMETHOD. 

ENDCLASS.

MODULE status_0100 OUTPUT.

  CONSTANTS:
    lc_update_button  TYPE char6 VALUE 'UPDATE',
    lc_container_name TYPE char8 VALUE 'ALV_0100',
    lc_line_name      TYPE lvc_cifnm VALUE 'COLOUR_LINE'.

  DATA:
    lt_fieldcat TYPE lvc_t_fcat,
    ls_layout   TYPE lvc_s_layo.

  IF line_exists( lcl_main=>mt_alv_data[ colour_line = lcl_main=>mc_edited_color_line ] ).
    SET PF-STATUS 'GUI_STATUS_0100'.
  ELSE.
    SET PF-STATUS 'GUI_STATUS_0100' EXCLUDING lc_update_button.
  ENDIF.

  APPEND:
    VALUE #( fieldname = 'PERNR_O' scrtext_l = TEXT-020 col_pos = 1  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'SEQNR_O' scrtext_l = TEXT-021 col_pos = 2  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'WTMOD_O' scrtext_l = TEXT-022 col_pos = 3  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'PAYDR_O' scrtext_l = TEXT-023 col_pos = 4  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'PAYDD_O' scrtext_l = TEXT-024 col_pos = 5  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'PERNR_N' scrtext_l = TEXT-025 col_pos = 6  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'SEQNR_N' scrtext_l = TEXT-026 col_pos = 7  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'WTMOD_N' scrtext_l = TEXT-027 col_pos = 8  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'PAYDR_N' scrtext_l = TEXT-028 col_pos = 9  outputlen = 10 ) TO lt_fieldcat,
    VALUE #( fieldname = 'PAYDD_N' scrtext_l = TEXT-029 col_pos = 10 outputlen = 10 ) TO lt_fieldcat.

  ls_layout-info_fname = lc_line_name.

  DATA(lo_container) = NEW cl_gui_custom_container( container_name = lc_container_name ).
  NEW cl_gui_alv_grid(
    i_parent = lo_container
  )->set_table_for_first_display(
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = lt_fieldcat
      it_outtab       = lcl_main=>mt_alv_data ).

ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'UPDATE'.
      lcl_main=>submit_results_to_cluster( ).
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

START-OF-SELECTION.
  lcl_main=>start_of_selection( ).

GET pernr.
  lcl_main=>get_pernr( ).

END-OF-SELECTION.
  lcl_main=>end_of_selection( ).