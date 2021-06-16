CLASS lcl_report DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES:

*     ТИП ДЛЯ ЛОГА УЧАСТНИКОВ
      BEGIN OF tys_log_col_participant,
        pernr TYPE persno,
        fio   TYPE zhre_fio,
        plans TYPE char40,
        orgeh TYPE char40,
      END OF tys_log_col_participant,

*    ТИП ДЛЯ ЛОГА КОЛЛЕКТИВНЫХ ПОКАЗАТЕЛЕЙ
      BEGIN OF tys_log_col_stat,
        id        TYPE char8,
        stat_name TYPE char40,
      END OF tys_log_col_stat.

    CLASS-METHODS:
      at_selection_screen_output,

      at_selection_screen,

      end_of_selection,

      start_of_selection,

      get_help_search_a1,

      get_help_search_g1,

      get_search_help_orgeh,

      fill_statg_texts_table,

      check_existance_in_hrhap
        IMPORTING
          iv_pernr TYPE persno
          iv_type  TYPE char2
        EXPORTING
          ev_bool  TYPE abap_bool,

      get_data_for_participant_log
        IMPORTING
          iv_pernr      TYPE persno
          iv_plans      TYPE plans
          iv_orgeh      TYPE orgeh
        EXPORTING
          es_log_record TYPE tys_log_col_participant.

  PRIVATE SECTION.
    CLASS-DATA:
      mt_pernr               TYPE TABLE OF hrpernr,
      mt_log_col_stat        TYPE TABLE OF tys_log_col_stat,
      mt_log_col_participant TYPE TABLE OF tys_log_col_participant,
      mt_statg_texts         TYPE hap_t_dynp_hrsobid,

      mv_ap_start_date       TYPE begda,
      mv_ap_end_date         TYPE endda.
ENDCLASS. " lcl_report definition

CLASS lcl_report IMPLEMENTATION.

*--------------------------------------------------------------------*
* METHOD START_OF_SELECTION
*--------------------------------------------------------------------*
  METHOD at_selection_screen.
    mv_ap_start_date = |{ so_year }{ gc_begda_mmdd }|.
    mv_ap_end_date   = |{ so_year }{ gc_endda_mmdd }|.
  ENDMETHOD. " at_selection_screen

*--------------------------------------------------------------------*
* METHOD START_OF_SELECTION
*--------------------------------------------------------------------*
  METHOD start_of_selection.

*         ДЛЯ ФМ HRHAP_PREPARE_DOCUMENT - ДОБАВЛЕНИЕ НОВОЙ АТТЕСТАЦИИ
    DATA: lt_header_appraiser    TYPE hap_t_header_appraiser, " ADM
          lt_header_appraisee    TYPE hap_t_header_appraisee, " ОЦЕНИВАЕМЫЕ

*         ПО ЭТИМ ИНФО-ТИПАМ ПРОВЕРЯЮТСЯ УСЛОВИЯ ПЕРЕД ДОБАВЛЕНИЕМ АТТЕСТАЦИИ
          lt_p0000               TYPE TABLE OF p0000, " МЕРОПРИЯТИЯ
          lt_p0001               TYPE TABLE OF p0001, " ОРГ ПРИСВОЕНИЕ
          lt_hrp9100             TYPE TABLE OF p9100, " УЧАСТНИК ПРОГРАММЫ УПЦ
          lt_mainpos             TYPE TABLE OF p0001, " СПИСОК ОШД

*         ДЛЯ ФМ HRHAP_PREPARE_DOCUMENT - ДОБАВЛЕНИЕ НОВОЙ АТТЕСТАЦИИ
          ls_mainpos             TYPE p0001,
          ls_header_appraiser    TYPE LINE OF hap_t_header_appraiser,
          ls_header_appraisee    TYPE LINE OF hap_t_header_appraisee,
          ls_log_col_participant TYPE tys_log_col_participant,
          lv_document_name       TYPE hap_appraisal_name,

          lv_record_exists       TYPE abap_bool.

    IF so_year < gc_year_low.
      MESSAGE e005.
      RETURN.
    ENDIF.

    IF so_admng-low IS INITIAL .
      MESSAGE e006.
      RETURN.
    ENDIF.

*   НЕ ОТРАБОТАЕТ СП ПО А1, ЕСЛИ ГР. АДМИНИСТРАТОРОВ НЕ ДОПОЛНЕНА НУЛЯМИ
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = so_admng-low
      IMPORTING
        output = so_admng-low.

    ls_header_appraiser-plan_version = gc_plan_variant.
    ls_header_appraiser-type         = gc_1a.
    ls_header_appraiser-id           = so_admng-low.
    APPEND ls_header_appraiser TO lt_header_appraiser.

    mv_ap_start_date = |{ so_year }{ gc_begda_mmdd }|.
    mv_ap_end_date   = |{ so_year }{ gc_endda_mmdd }|.
    lv_document_name = |{ TEXT-015 }{ so_year }{ TEXT-016 }|.

**********************************************************************
* ГРУППА КОЛЛЕКТИВНЫХ ПОКАЗАТЕЛЕЙ
**********************************************************************
    IF rb_colst = abap_true.
      LOOP AT so_statg ASSIGNING FIELD-SYMBOL(<statg>).

*       ЗАПОЛНЕНИЕ ID У КАТЕГОРИИ
        ls_header_appraisee-plan_version = gc_plan_variant.
        ls_header_appraisee-type = gc_1g.
        ls_header_appraisee-id = <statg>+3(8).
        APPEND ls_header_appraisee TO lt_header_appraisee.

*       ЕСЛИ УЖЕ ЕСТЬ ЗАПЛАНИРОВАННАЯ АТТЕСТАЦИЯ ТИП 1G
        check_existance_in_hrhap(
          EXPORTING
            iv_pernr = CONV #( ls_header_appraisee-id )
            iv_type  = ls_header_appraisee-type
          IMPORTING
            ev_bool  = lv_record_exists ).

        IF lv_record_exists = abap_true.
          CONTINUE.
        ENDIF.

*       ФМ, ДОБАВЛЯЮЩИЙ АТТЕСТАЦИЮ (В t_appraisee -
*       ДОЛЖНА ЛЕЖАТЬ ОДНА ЗАПИСЬ КОЛЛЕКТИВНОГО ПОКАЗАТЕЛЯ)
        CALL FUNCTION 'HRHAP_DOCUMENT_PREPARE'
          EXPORTING
            add_on_appl   = gc_pa
            plan_version  = gc_plan_variant
            template_id   = gc_template_id
            document_name = lv_document_name
            ap_start_date = mv_ap_start_date
            ap_end_date   = mv_ap_end_date
            t_appraiser   = lt_header_appraiser
            t_appraisee   = lt_header_appraisee
            administrator = abap_true
            test          = abap_false
            skip_in_prep  = abap_true
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc <> 0.
          MESSAGE e011 WITH ls_header_appraisee-id.
        ENDIF.

        CLEAR: ls_header_appraisee, lt_header_appraisee.

      ENDLOOP.

**********************************************************************
* УЧАСТНИКИ
**********************************************************************
    ELSEIF rb_partc = abap_true.
      IF so_objid-low IS NOT INITIAL.
*   ПОЛУЧЕНИЕ СПИСКА РАБОТНИКОВ ПО ОРГ.ЕДЕНИЦЕ
        CALL FUNCTION 'HRCM_ORGUNIT_EMPLOYEE_LIST_GET'
          EXPORTING
            plvar              = gc_plan_variant  " hap_s_wizard_doc_prep-plan_version
            otype              = gc_otype_orgeh
            objid              = so_objid-low
            begda              = mv_ap_start_date
            endda              = mv_ap_end_date
          TABLES
            pernr_table        = mt_pernr
          EXCEPTIONS
            path_error         = 1
            root_error         = 2
            no_employees_found = 3
            OTHERS             = 4.
        IF sy-subrc = 3.
          MESSAGE i008.
        ENDIF.
      ENDIF.

*     ТАКЖЕ, ЕСЛИ УКАЗАНЫ ОТДЕЛЬНЫЕ ТН, НЕ ОТНОСЯЩИЕСЯ К УКАЗАННОЙ ОРГ.ЕДИНИЦЕ,
*     ТО ДОБАВЛЯЕМ ИХ. ДУБЛИКАТЫ НЕ БУДУТ ДОБАВЛЕНЫ
      IF lines( so_pernr ) > 0.
        LOOP AT so_pernr INTO DATA(ls_pernr).

          READ TABLE mt_pernr WITH KEY pernr = ls_pernr-low TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            APPEND ls_pernr-low TO mt_pernr.
          ENDIF.

        ENDLOOP.
      ENDIF.

      LOOP AT mt_pernr ASSIGNING FIELD-SYMBOL(<pernr>).

        CLEAR: lt_header_appraisee, ls_header_appraisee.
        CLEAR: lt_p0000, lt_p0001, lt_mainpos.

*       ВЫБИРАЮТСЯ ВСЕ АКТИВНЫЕ СОТРУДНИКИ (STAT2-PA0000=«3»)
        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
            infty           = gc_0000
            pernr           = <pernr>-pernr
            begda           = mv_ap_start_date
            endda           = mv_ap_end_date
          TABLES
            infty_tab       = lt_p0000
          EXCEPTIONS
            infty_not_found = 1
            invalid_input   = 2.

        IF sy-subrc = 0.

          READ TABLE lt_p0000 WITH KEY stat2 = '3' TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

        ENDIF.

        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
            infty           = gc_0001
            pernr           = <pernr>-pernr
            begda           = mv_ap_start_date
            endda           = mv_ap_end_date
          TABLES
            infty_tab       = lt_p0001
          EXCEPTIONS
            infty_not_found = 1
            invalid_input   = 2.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        CALL FUNCTION 'ZHR_GET_PERNR_MAINPOS_NEW'
          EXPORTING
            pernr   = <pernr>-pernr
*           AUTH_DISABLE = 'X'
          TABLES
            p0000   = lt_p0000[]
            p0001   = lt_p0001[]
            mainpos = lt_mainpos[].

        CLEAR ls_mainpos.

*       ШТАТКА НА УКАЗАННЫЙ ПЕРИОД
        ##NEEDED
        LOOP AT lt_mainpos INTO ls_mainpos WHERE begda <= mv_ap_end_date
                                             AND endda >= mv_ap_start_date.
        ENDLOOP.

*       «УЧАСТНИК ПРОГРАММЫ УПЦ» У ШТАТНОЙ ДОЛЖНОСТИ
        CALL FUNCTION 'RH_READ_INFTY_NNNN'
          EXPORTING
            plvar                 = gc_plan_variant
            infty                 = gc_9100
            objid                 = ls_mainpos-plans
            otype                 = gc_otype_plans
            begda                 = mv_ap_start_date
            endda                 = mv_ap_start_date
          TABLES
            innnn                 = lt_hrp9100
          EXCEPTIONS
            nothing_found         = 1
            wrong_condition       = 2
            infotyp_not_supported = 3
            wrong_parameters      = 4
            OTHERS                = 5.

*       ЕСЛИ НЕ УЧАСТНИК - ПЕРЕХОДИМ К СЛЕДУЮЩЕЙ ИТЕРАЦИИ
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

*       ЕСЛИ УЖЕ ЕСТЬ ЗАПЛАНИРОВАННАЯ АТТЕСТАЦИЯ
        check_existance_in_hrhap(
          EXPORTING
            iv_pernr = <pernr>-pernr
            iv_type  = gc_otype_pernr
          IMPORTING
            ev_bool  = lv_record_exists ).

        IF lv_record_exists = abap_true.
          CONTINUE.
        ENDIF.

*       ЗАПОЛНЕНИЕ ДАННЫХ АТТУСТУЕМЫХ
        ls_header_appraisee-plan_version = gc_plan_variant.
        ls_header_appraisee-type = gc_otype_pernr.
        ls_header_appraisee-id = <pernr>-pernr.
        APPEND ls_header_appraisee TO lt_header_appraisee.

*       ДОБАВЛЕНИЕ ЗАПИСИ АТТЕСТАЦИИ
*       (УДАЛЯТЬ АТТЕСТАЦИИ ЧЕРЕЗ ТРАНЗАЦИЮ PHAP_ADMIN)
        CALL FUNCTION 'HRHAP_DOCUMENT_PREPARE'
          EXPORTING
            add_on_appl   = gc_pa
            plan_version  = gc_plan_variant
            template_id   = gc_template_id
            document_name = lv_document_name
            ap_start_date = mv_ap_start_date
            ap_end_date   = mv_ap_end_date
            t_appraiser   = lt_header_appraiser
            t_appraisee   = lt_header_appraisee
            administrator = abap_true
            test          = abap_false
            skip_in_prep  = abap_true
          EXCEPTIONS
            OTHERS        = 1.

        IF sy-subrc = 0.

*         ЗАПОЛНЯЕМ ЗАПИСЬ ЛОГА
          get_data_for_participant_log(
            EXPORTING
              iv_pernr      = <pernr>-pernr
              iv_plans      = ls_mainpos-plans
              iv_orgeh      = ls_mainpos-orgeh
            IMPORTING
              es_log_record = ls_log_col_participant ).

*         ЗАПИСЫВАЕМ ЗАПИСЬ В ЛОГ
          IF ls_log_col_participant IS NOT INITIAL.
            APPEND ls_log_col_participant TO mt_log_col_participant.
          ENDIF.

        ENDIF.
      ENDLOOP. " at mt_pernr
    ENDIF. " rb_colst OR rb_partc
  ENDMETHOD. " start_of_selection

*--------------------------------------------------------------------*
* METHOD END_OF_SELECTION
*--------------------------------------------------------------------*
  METHOD end_of_selection.

    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column  TYPE REF TO cl_salv_column_list,
          lo_table   TYPE REF TO cl_salv_table,

          ls_statg   TYPE tys_log_col_stat,

          lv_col_s   TYPE scrtext_s,
          lv_col_m   TYPE scrtext_m,
          lv_col_l   TYPE scrtext_l,
          lv_msg     TYPE string,

          lc_msg     TYPE REF TO cx_salv_msg.

*   ПРИ КОЛЛЕКТИВНЫХ КРИТЕРИЯХ
    IF rb_colst = abap_true.
      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = lo_table
            CHANGING
              t_table      = mt_log_col_stat ).
        CATCH cx_salv_msg INTO lc_msg .
          lv_msg = lc_msg->get_text( ).
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

      TRY.
          lo_columns = lo_table->get_columns( ).

          lo_column ?= lo_columns->get_column( gc_col_id ).

          lv_col_s = TEXT-008.
          lo_column->set_short_text( lv_col_s ).
          lv_col_m = TEXT-009.
          lo_column->set_medium_text( lv_col_m ).
          lv_col_l = TEXT-010.
          lo_column->set_long_text( lv_col_l ).

          lo_column ?= lo_columns->get_column( gc_col_stat_name ).

          lv_col_s = TEXT-011.
          lo_column->set_short_text( lv_col_s ).
          lv_col_m = TEXT-012.
          lo_column->set_medium_text( lv_col_m ).
          lv_col_l = TEXT-013.
          lo_column->set_long_text( lv_col_l ).
        CATCH cx_salv_not_found.                        "#EC NO_HANDLER
      ENDTRY.

      LOOP AT so_statg ASSIGNING FIELD-SYMBOL(<statg>).

*       ОТСЕКАЕМ IEQ ИЗ SELECT-OPTIONS'А
        ls_statg-id = <statg>+3(8).
        READ TABLE mt_statg_texts WITH KEY sobid = ls_statg-id
          TRANSPORTING stext INTO DATA(ls_statg_texts).
        IF sy-subrc = 0.
          ls_statg-stat_name = ls_statg_texts-stext.
        ENDIF.

        APPEND ls_statg TO mt_log_col_stat.
      ENDLOOP.

*   ФОРМИРОВАНИЕ ЛОГА ПРИ ВЫБРАННОМ ПАРАМЕТРЕ "УЧАСТНИКИ"
    ELSEIF rb_partc = abap_true.
      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = lo_table
            CHANGING
              t_table      = mt_log_col_participant ).
        CATCH cx_salv_msg INTO lc_msg .
          lv_msg = lc_msg->get_text( ).
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.

      TRY.
          lo_columns = lo_table->get_columns( ).
          lo_columns->set_optimize( ).

          lo_column ?= lo_columns->get_column( gc_col_plans ).
          lv_col_s = TEXT-004.
          lo_column->set_short_text( lv_col_s  ).
          lv_col_m = TEXT-004.
          lo_column->set_medium_text( lv_col_m ).
          lv_col_l = TEXT-004.
          lo_column->set_long_text( lv_col_l ).

          lo_column ?= lo_columns->get_column( gc_col_orgeh ).
          lv_col_s = TEXT-005.
          lo_column->set_short_text( lv_col_s ).
          lv_col_m = TEXT-006.
          lo_column->set_medium_text( lv_col_m ).
          lv_col_m = TEXT-007.
          lo_column->set_long_text( lv_col_l ).
        CATCH cx_salv_not_found.                        "#EC NO_HANDLER
      ENDTRY.

    ENDIF.

    lo_table->display( ).

  ENDMETHOD. " end_of_selection

*--------------------------------------------------------------------*
* METHOD GET_HELP_SEARCH_A1
*--------------------------------------------------------------------*
* Средство поиска по аттестующему A1
*--------------------------------------------------------------------*
  METHOD get_help_search_a1.

    IF mv_ap_start_date IS INITIAL.
      MESSAGE i010.
      RETURN.
    ENDIF.

    DATA:
      lt_base_objects     TYPE TABLE OF rhmc2,
      lt_selected         TYPE hap_t_hrsobid,
      lt_selected_objects TYPE objec_t.

    CALL FUNCTION 'RH_OBJID_REQUEST'
      EXPORTING
        plvar           = gc_plan_variant
        otype           = gc_1a
        seark           = '*'
        seark_begda     = mv_ap_start_date
        seark_endda     = mv_ap_end_date
        restrict_fb     = 'HRHAP_0CB_RESTRICT_APPRAISER'
      TABLES
        base_objects    = lt_base_objects
        sel_objects     = lt_selected_objects
        marked_objects  = lt_selected
      EXCEPTIONS
        cancelled       = 1
        wrong_condition = 2
        nothing_found   = 3
        illegal_mode    = 4
        internal_error  = 5
        OTHERS          = 6.

    IF sy-subrc = 0 AND lines( lt_selected_objects ) > 0.
      so_admng-low = lt_selected_objects[ 1 ]-objid.
    ENDIF.

  ENDMETHOD. " get_help_search_a1

*--------------------------------------------------------------------*
* METHOD GET_HELP_SEARCH_G1
*--------------------------------------------------------------------*
* Средство поиска по коллекстивным критерия оценки (G1)
*--------------------------------------------------------------------*
  METHOD get_help_search_g1.

    IF mv_ap_start_date IS INITIAL.
      MESSAGE i010.
      RETURN.
    ENDIF.

    DATA: lt_return_tab  TYPE TABLE OF ddshretval.

*   ЗАПОЛНЯЕМ ТАБЛИЦУ С ТЕКСТАМИ КОЛЛЕКТИВНЫХ КРИТЕРИЕВ
    fill_statg_texts_table( ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'SOBID'
        window_title    = TEXT-014
        value_org       = 'S'
        multiple_choice = abap_true
      TABLES
        value_tab       = mt_statg_texts
        return_tab      = lt_return_tab[]
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE i009.
    ENDIF.

    DATA(lv_flag) = abap_false.

    LOOP AT lt_return_tab ASSIGNING FIELD-SYMBOL(<return_tab>).
      CLEAR so_statg.

      CHECK <return_tab> IS ASSIGNED.

      IF lv_flag = abap_false.
        so_statg-low    = <return_tab>-fieldval.
        so_statg-high   = <return_tab>-fieldval.
        so_statg-option = 'EQ'.
        so_statg-sign   = 'I'.
        APPEND so_statg TO so_statg[].

        lv_flag = abap_true.

      ENDIF.

      IF <return_tab>-fieldval NOT IN so_statg.
        so_statg-low    = <return_tab>-fieldval.
        so_statg-high   = <return_tab>-fieldval.
        so_statg-option = 'EQ'.
        so_statg-sign   = 'I'.
        APPEND so_statg TO so_statg[].
      ENDIF.
    ENDLOOP.

    IF lines( so_statg ) > 0.
      so_statg = so_statg[ 1 ].
    ENDIF.

  ENDMETHOD. " get_help_search_g1

*--------------------------------------------------------------------*
* METHOD GET_SEARCH_HELP_ORGEH
*--------------------------------------------------------------------*
* Средство поиска по орг. единице
*--------------------------------------------------------------------*
  METHOD get_search_help_orgeh.

    IF mv_ap_start_date IS INITIAL.
      MESSAGE i010.
      RETURN.
    ENDIF.

    DATA: lt_selected         TYPE hap_t_hrsobid,
          lt_selected_objects TYPE objec_t.

    CALL FUNCTION 'RH_OBJID_REQUEST'
      EXPORTING
        plvar           = gc_plan_variant
        otype           = gc_otype_orgeh
        seark_begda     = mv_ap_start_date
        seark_endda     = mv_ap_end_date
      TABLES
        sel_objects     = lt_selected_objects
        marked_objects  = lt_selected
      EXCEPTIONS
        cancelled       = 1
        wrong_condition = 2
        nothing_found   = 3
        illegal_mode    = 4
        internal_error  = 5
        OTHERS          = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSEIF lines( lt_selected_objects ) > 0.
      so_objid-low = lt_selected_objects[ 1 ]-objid.
    ENDIF.

  ENDMETHOD. " get_search_help_orgeh

*--------------------------------------------------------------------*
* METHOD AT_SELECTION_SCREEN_OUTPUT
*--------------------------------------------------------------------*
  METHOD at_selection_screen_output.

    CLEAR mt_log_col_participant.
    CLEAR mt_log_col_stat.
    CLEAR: so_statg, so_statg[].

    DATA: lv_vis1 TYPE c VALUE '1',
          lv_vis2 TYPE c VALUE '0'.

    LOOP AT SCREEN.

      CASE 'X'.
        WHEN rb_colst.
          lv_vis1 = '1'.
          lv_vis2 = '0'.
        WHEN rb_partc.
          lv_vis1 = '0'.
          lv_vis2 = '1'.
      ENDCASE.

      CASE screen-group1.
        WHEN 'MI1'.
          screen-input  = lv_vis1.
          screen-output = lv_vis1.
        WHEN 'MI2'.
          screen-input  = lv_vis2.
          screen-output = lv_vis2.

      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD. " at_selection_screen_output

*--------------------------------------------------------------------*
* METHOD FILL_STATG_TEXTS_TABLE
*--------------------------------------------------------------------*
* Заполнение таблицы с текстами коллективных критериев оценки (G1)
*--------------------------------------------------------------------*
  METHOD fill_statg_texts_table.

    DATA: lt_types        TYPE hap_t_type,
          lt_objects_base TYPE hap_t_hrsobid,
          lt_appee        TYPE hap_t_appee,
          lt_objects      TYPE hap_t_hrsobid,
          pt_objects      TYPE hap_t_dynp_hrsobid,

          lw_types        TYPE hap_s_type,
          lw_objects_base TYPE hrsobid,
          lw_appee        TYPE hap_s_appee,

          l_cat_group_id  TYPE hap_cat_group_id,
          l_category_id   TYPE hap_category_id.

*   ДАННЫЕ ДЛЯ ВЫБОРКИ
    lw_objects_base-plvar = gc_plan_variant.
    lw_objects_base-otype = gc_1g.
    lw_objects_base-sobid = so_admng-low.
    APPEND lw_objects_base TO lt_objects_base.

    CALL FUNCTION 'HRHAP_C_IT5020_READ_SINGLE'
      EXPORTING
        plan_version = gc_plan_variant
        element_type = gc_element_type_va
        element_id   = gc_template_id
      IMPORTING
        cat_group_id = l_cat_group_id
        category_id  = l_category_id.

*     СЧИТЫВАЕМ КАТЕГОРИИ АТТЕСТУЕМЫХ
    CALL FUNCTION 'HRHAP_C_CATEGORY_APPEE_READ'
      EXPORTING
        cat_group_id = l_cat_group_id
        category_id  = l_category_id
      IMPORTING
        t_appee      = lt_appee.

*    ЗАПОЛНЕНИЕ ТАБЛИЦЫ ТИПОВ
    LOOP AT lt_appee INTO lw_appee.
      lw_types-type = lw_appee-appee_type_allow.
      APPEND lw_types TO lt_types.
    ENDLOOP.

*   ВЫБИРАЕМ КОЛЛЕКТИВНЫЕ ПОКАЗАТЕЛИ
    CALL FUNCTION 'HRHAP_SELECT_OBJECTS'
      EXPORTING
        flt_val          = gc_zkp
        t_objects_base   = lt_objects_base
        from_date        = mv_ap_start_date
        to_date          = mv_ap_end_date
        t_target_types   = lt_types
      IMPORTING
        t_objects        = lt_objects
      EXCEPTIONS
        not_implemented  = 1
        no_objects_found = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*   ЗАПОЛНЕНИЕ ИМЁН ОБЪЕКТОВ
    DATA: lw_hrsobid TYPE hrsobid,
          lw_dynpro  TYPE hap_s_dynp_hrsobid,
          l_short    TYPE short_d.

    CALL FUNCTION 'RH_TEXT_BUFFER_FILL_EXTERNAL'
      TABLES
        objects = lt_objects.

    LOOP AT lt_objects INTO lw_hrsobid.

      MOVE-CORRESPONDING lw_hrsobid TO lw_dynpro.
      CALL FUNCTION 'RH_GET_OTYPE_TEXT'
        EXPORTING
          otype         = lw_dynpro-otype
        IMPORTING
          otext         = lw_dynpro-otext
        EXCEPTIONS
          invalid_otype = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'RH_READ_OBJECT'
        EXPORTING
          plvar     = lw_dynpro-plvar
          otype     = lw_dynpro-otype
          realo     = lw_dynpro-sobid
        IMPORTING
          short     = l_short
          stext     = lw_dynpro-stext
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      IF sy-subrc <> 0.
        CONTINUE.
      ELSEIF lw_dynpro-stext IS INITIAL.
        lw_dynpro-stext = l_short.
      ENDIF.

      APPEND lw_dynpro TO pt_objects.
    ENDLOOP.

    SORT pt_objects BY plvar otype sobid.
    DELETE ADJACENT DUPLICATES FROM pt_objects
                          COMPARING plvar otype sobid.

    IF sy-subrc = 0.
      MESSAGE i007.
    ENDIF.

    IF lines( mt_statg_texts ) = 0.
      mt_statg_texts = pt_objects.
    ENDIF.

  ENDMETHOD. " fill_statg_texts_table

*--------------------------------------------------------------------*
* METHOD CHECK_EXISTANCE_IN_HRHAP
*--------------------------------------------------------------------*
* Проверка на существоввание аттестации на данного пользователя
* на указанный период
*--------------------------------------------------------------------*
  METHOD check_existance_in_hrhap.

*   ДЛЯ ФМ HRHAP_DOCUMENT_GET_LIST
    DATA: lt_appraisee           TYPE hap_t_hrsobid,
          lt_appraisal_id        TYPE hap_t_appraisal_id,
          lt_templates           TYPE hap_t_hrobject,

          ls_sel_dates           TYPE hap_s_sel_dates,
          ls_sel_with_or_without TYPE hap_s_sel_with_or_without,
          ls_templates           TYPE LINE OF hap_t_hrobject,
          ls_appraisee           TYPE LINE OF hap_t_hrsobid.

    CLEAR: lt_appraisal_id.
    CLEAR: lt_appraisee.

    ev_bool = abap_false.

    ls_sel_with_or_without-sel_display_existing = abap_true.

*   СОБИРАЕМ ДАТЫ ДЛЯ ПРОВЕРКИ СУЩЕСТВОВАНИЯ АТТЕСТАЦИQ НА ЭТИ ДАТЫ
    ls_sel_dates-validity_from_date = mv_ap_start_date.
    ls_sel_dates-validity_to_date   = mv_ap_end_date.

    ls_templates-plvar = gc_plan_variant.
    ls_templates-otype = gc_element_type_va.
    ls_templates-objid = gc_template_id.
    APPEND ls_templates TO lt_templates.

    ls_appraisee-otype = iv_type.
    ls_appraisee-plvar = gc_plan_variant.
    ls_appraisee-sobid = iv_pernr.
    APPEND ls_appraisee TO lt_appraisee.

*   ФМ, ВОЗВРАЩАЮЩИЙ СПИСОК АТТЕСТАЦИЙ
    CALL FUNCTION 'HRHAP_DOCUMENT_GET_LIST'
      EXPORTING
        add_on_application    = gc_pa
        t_templates           = lt_templates
        plan_version          = gc_plan_variant
        t_appraisees          = lt_appraisee
        s_sel_with_or_without = ls_sel_with_or_without
        s_sel_dates           = ls_sel_dates
      IMPORTING
        t_appraisal_id        = lt_appraisal_id
      EXCEPTIONS
        OTHERS                = 1.
    IF sy-subrc <> 0.
      MESSAGE e012 WITH 'HRHAP_DOCUMENT_GET_LIST'.
    ENDIF.

*   ПРИ ИМЕЮЩИХСЯ АТТЕСТАЦИЯХ ЛЮБОГО СТАТУСА НОВАЯ НЕ СОЗДАЁТСЯ
*   (флаг - abap_true)
    IF lines( lt_appraisal_id ) > 0.
      ev_bool = abap_true.
    ENDIF.

  ENDMETHOD. " check_existance_in_hrhap

*--------------------------------------------------------------------*
* METHOD GET_DATA_FOR_PARTICIPANT_LOG
*--------------------------------------------------------------------*
* Заполнение данных для лога (ТН, ФИО, ШД, ОЕ)
*--------------------------------------------------------------------*
  METHOD get_data_for_participant_log.

    IF iv_pernr IS INITIAL.
      RETURN.
    ENDIF.

    es_log_record-pernr = iv_pernr.

    DATA: lt_p0000   TYPE TABLE OF p0000,
          lt_p0001   TYPE TABLE OF p0001,
          lt_p0002   TYPE TABLE OF p0002,
          lt_mainpos TYPE TABLE OF p0001,

          lo_biorgeh TYPE REF TO zhr_objbif_orgeh,
          lo_biplans TYPE REF TO zhr_objbif_plans,
          ##NEEDED
          ls_mainpos TYPE p0001.

    IF iv_plans IS INITIAL OR iv_orgeh IS INITIAL.
      CALL FUNCTION 'ZHR_GET_PERNR_MAINPOS_NEW'
        EXPORTING
          pernr   = iv_pernr
        TABLES
          p0000   = lt_p0000[]
          p0001   = lt_p0001[]
          mainpos = lt_mainpos[].

      CLEAR ls_mainpos.
      ##NEEDED
      LOOP AT lt_mainpos INTO ls_mainpos WHERE begda <= mv_ap_start_date
                                           AND endda >= mv_ap_end_date.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        infty           = gc_0002
        pernr           = iv_pernr
        begda           = mv_ap_start_date
        endda           = mv_ap_end_date
      TABLES
        infty_tab       = lt_p0002
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      READ TABLE lt_p0002 INTO DATA(ls_p0002) WITH KEY pernr = iv_pernr.

      IF sy-subrc = 0.
        es_log_record-fio = |{ ls_p0002-nachn } { ls_p0002-vorna } { ls_p0002-midnm }|.
      ENDIF.

    ENDIF.

    TRY.
*   ДЛЯ ВЫВОДА ЛОГА ПОЛУЧАЕМ НАИМЕНОВАНИЕ ОРГЕДИНИЦЫ И ШТАТКИ
        CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
          EXPORTING
            otype            = gc_otype_orgeh
            objid            = iv_orgeh
          CHANGING
            result           = lo_biorgeh
          EXCEPTIONS
            invalid_otype    = 1
            invalid_casting  = 2
            object_not_found = 3
            OTHERS           = 4.
        IF sy-subrc = 0 AND lo_biorgeh IS BOUND.
          es_log_record-orgeh = lo_biorgeh->get_name( ).
        ENDIF.
      CATCH zcx_objbif_objec.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
          EXPORTING
            plvar            = gc_plan_variant
            otype            = gc_otype_plans
            objid            = iv_plans
          CHANGING
            result           = lo_biplans
          EXCEPTIONS
            invalid_otype    = 1
            invalid_casting  = 2
            object_not_found = 3
            OTHERS           = 4.
        IF sy-subrc = 0 AND lo_biplans IS BOUND.
          es_log_record-plans = lo_biplans->get_name( ).
        ENDIF.
      CATCH zcx_objbif_objec.                           "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD. " get_data_for_participant_log
ENDCLASS. " lcl_report implementation
