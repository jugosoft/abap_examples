class zcl_demo_amdp definition
  public 
  final
  create public .

public section.
interfaces if_amdp_marker_hdb.
types: begin of t_bupa.
       include type snwd_bpa.
       types: end of t_bupa.
              types: tt_bupa type table of t_bupa.
    class-methods:
        get_bupa exporting value(table_bupa) type tt_bupa.
protected section.
private section.
endclass.



class zcl_demo_amdp implementation.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DEMO_AMDP=>GET_BUPA
* +-------------------------------------------------------------------------------------------------+
* | [<---] TABLE_BUPA                     TYPE        TT_BUPA
* +--------------------------------------------------------------------------------------</SIGNATURE>
 method get_bupa by database procedure for hdb language sqlscript using snwd_bpa.
    declare lv_client nvarchar(3);
    select session_context('CLIENT') into lv_client from dummy;
    table_bupa = select * from snwd_bpa where client = lv_client;
 endmethod.
endclass.