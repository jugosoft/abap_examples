CLASS lcx_helper DEFINITION INHERITING FROM cx_no_check.

ENDCLASS.
 
CLASS lcl_p0027_transform DEFINITION.

  PUBLIC SECTION.
    TYPES BEGIN OF mty_s_costdistribtution.
      TYPES begda TYPE begda.
      TYPES endda TYPE endda.
      INCLUDE TYPE rhcost_distr.
    TYPES END OF mty_s_costdistribtution.

    TYPES mty_t_costdistribution TYPE TABLE OF mty_s_costdistribtution WITH EMPTY KEY.

    METHODS:
      constructor IMPORTING is_p0027 TYPE p0027,
      transform_p0027_to_table RETURNING VALUE(rt_costdistribution) TYPE mty_t_costdistribution.


  PRIVATE SECTION.
    DATA ms_p0027 TYPE p0027.

    METHODS:
      process_p0027_field IMPORTING iv_nn             TYPE num2
                                    iv_name_costass   TYPE char20
                                    iv_name_0027      TYPE char10
                          EXPORTING es_costassignment TYPE mty_s_costdistribtution.
ENDCLASS.

CLASS lcl_p0027_transform IMPLEMENTATION.
  METHOD constructor.
    IF is_p0027 IS INITIAL.
      RAISE EXCEPTION TYPE lcx_helper.
    ENDIF.

    me->ms_p0027 = is_p0027.

  ENDMETHOD.

  METHOD transform_p0027_to_table.
    TYPES:
      BEGIN OF lty_s_match_table,
        field_costass TYPE char20,
        field_p0027   TYPE char10,
      END OF lty_s_match_table,
      lty_t_match_table TYPE TABLE OF lty_s_match_table WITH EMPTY KEY.

    DATA(lt_match_table) = VALUE lty_t_match_table(
      ( field_costass = 'BUKRS'     field_p0027 = 'KBU' )
      ( field_costass = 'GSBER'     field_p0027 = 'KGB' )
      ( field_costass = 'KOSTL'     field_p0027 = 'KST' )
      ( field_costass = 'AUFNR'     field_p0027 = 'AUF' )
      ( field_costass = 'POSNR'     field_p0027 = 'PSP' )
      ( field_costass = 'FINCODE'   field_p0027 = 'FCD' )
      ( field_costass = 'FISTL'     field_p0027 = 'FCT' )
      ( field_costass = 'PROZT'     field_p0027 = 'KPR' )
      ( field_costass = 'FKBER'     field_p0027 = 'FKBER' )
      ( field_costass = 'GRANT'     field_p0027 = 'GRANT' )
      ( field_costass = 'SGMNT'     field_p0027 = 'SGM' )
      ( field_costass = 'BUDGET_PD' field_p0027 = 'BUDGET_PD' )
    ).

    DATA lv_last_line TYPE sy-tabix.

    DATA(lv_number) = VALUE num2( ).

    WHILE lv_number < 25.
      lv_number = lv_number + 1.

      LOOP AT lt_match_table INTO DATA(ls_match).
        me->process_p0027_field(
          EXPORTING
            iv_nn              = lv_number
            iv_name_costass    = ls_match-field_costass
            iv_name_0027       = ls_match-field_p0027
          IMPORTING
            es_costassignment = DATA(ls_costdistribution)
        ).
      ENDLOOP.

      IF NOT ls_costdistribution IS INITIAL.
        lv_last_line = lv_number.
      ELSE.
        CONTINUE.
      ENDIF.

      ls_costdistribution-begda = ms_p0027-begda.
      ls_costdistribution-endda = ms_p0027-endda.

      APPEND ls_costdistribution TO rt_costdistribution.
      CLEAR ls_costdistribution.
    ENDWHILE.

    IF lv_last_line < 25.
      ADD 1 TO lv_last_line.
      DELETE rt_costdistribution FROM lv_last_line TO 25.
    ENDIF.
  ENDMETHOD.

  METHOD process_p0027_field.

    DATA:
      lv_costass   TYPE char20,
      lv_fieldname TYPE char40.

    FIELD-SYMBOLS:
      <ls_p0027_field>          TYPE any,
      <ls_costassignment_field> TYPE any.

    lv_costass = iv_name_costass.

    IF iv_name_costass = 'GRANT'.
      lv_costass = 'GRANT_NBR'.
    ENDIF.

    lv_fieldname = |MS_P0027-{ iv_name_0027 }{ iv_nn }|.
    ASSIGN (lv_fieldname) TO <ls_p0027_field>.
    IF <ls_p0027_field> IS ASSIGNED.
      lv_fieldname = |ES_COSTASSIGNMENT-{ lv_costass }|.
      ASSIGN (lv_fieldname) TO <ls_costassignment_field>.
      IF <ls_costassignment_field> IS ASSIGNED.
        <ls_costassignment_field> = <ls_p0027_field>.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.