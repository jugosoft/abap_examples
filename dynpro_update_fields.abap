form validation_dates_w_out_prompt.
  data:
    lt_fieldvalues         type standard table of dynpread,
    lv_planned_date_local  type datum,
    lv_customer_date_local type datum.

  data ls_fieldvalue like line of lt_fieldvalues.
  ls_fieldvalue-fieldname = 'PLANNED_DATE'.
  append ls_fieldvalue to lt_fieldvalues.
  ls_fieldvalue-fieldname = 'CUSTOMER_DATE'.
  append ls_fieldvalue to lt_fieldvalues.

  call function 'DYNP_VALUES_READ'
    exporting
      dyname              = sy-repid
      dynumb              = sy-dynnr
    tables
      dynpfields          = lt_fieldvalues
   exceptions
     invalid_abapworkarea = 1
     invalid_dynprofield  = 2
     invalid_dynproname   = 3
     invalid_dynpronummer = 4
     invalid_request      = 5
     no_fielddescription  = 6
     invalid_parameter    = 7
     undefind_error       = 8
     double_conversion    = 9
     stepl_not_found      = 10
     others               = 11.

  read table lt_fieldvalues into ls_fieldvalue transporting fieldvalue with key fieldname = 'PLANNED_DATE'.
  lv_planned_date_local = zcl_value=>input( iv_exit = 'DATEX' iv_value = ls_fieldvalue-fieldvalue ).

  read table lt_fieldvalues into ls_fieldvalue transporting fieldvalue with key fieldname = 'CUSTOMER_DATE'.
  lv_customer_date_local = zcl_value=>input( iv_exit = 'DATEX' iv_value = ls_fieldvalue-fieldvalue ).

  if lv_planned_date_local <> planned_date or lv_customer_date_local <> customer_date.
    planned_date  = lv_planned_date_local.
    customer_date = lv_customer_date_local.
    perform time_selector.
  endif.
endform.
