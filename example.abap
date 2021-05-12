*&---------------------------------------------------------------------*
*& Report YTESTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yteste.


*  DATA: lt_bapischdl  TYPE STANDARD TABLE OF bapischdl,
*        lt_bapischdlx TYPE STANDARD TABLE OF bapischdlx.
*  DATA: lt_return TYPE bapiret2_tab.
*
*  lt_bapischdl  = VALUE #( ( itm_number = '00020'
*                             sched_line = '0002'
*                             req_qty = 1 ) ) .
*
*  lt_bapischdlx = VALUE #( ( itm_number = '00020'
*                             sched_line = '0002'
*                             updateflag = 'U'
*                             req_qty = abap_true  ) ) .
**
**  lt_bapischdl  = VALUE #( ( itm_number = '00010'
**                             sched_line = '0001'
**                             req_qty = 1 )
**                           ( itm_number = '00020'
**                             sched_line = '0002'
**                             req_qty = 2 ) ) .
**
**  lt_bapischdlx = VALUE #( ( itm_number = '00010'
**                             sched_line = '0001'
**                             updateflag = 'U'
**                             req_qty = abap_true  )
**                           ( itm_number = '00020'
**                             sched_line = '0002'
**                             updateflag = 'U'
**                             req_qty = abap_true  ) ) .
*
*  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*    EXPORTING
*      salesdocument    = CONV bapivbeln-vbeln( '3140000123' )
**     ORDER_HEADER_IN  =
*      order_header_inx = VALUE bapisdh1x( updateflag = 'U' )
**     SIMULATION       =
**     BEHAVE_WHEN_ERROR           = ' '
**     INT_NUMBER_ASSIGNMENT       = ' '
**     LOGIC_SWITCH     =
**     NO_STATUS_BUF_INIT          = ' '
*    TABLES
*      return           = lt_return
**     ORDER_ITEM_IN    =
**     ORDER_ITEM_INX   =
**     PARTNERS         =
**     PARTNERCHANGES   =
**     PARTNERADDRESSES =
**     ORDER_CFGS_REF   =
**     ORDER_CFGS_INST  =
**     ORDER_CFGS_PART_OF          =
**     ORDER_CFGS_VALUE =
**     ORDER_CFGS_BLOB  =
**     ORDER_CFGS_VK    =
**     ORDER_CFGS_REFINST          =
*      schedule_lines   = lt_bapischdl
*      schedule_linesx  = lt_bapischdlx
**     ORDER_TEXT       =
**     ORDER_KEYS       =
**     CONDITIONS_IN    =
**     CONDITIONS_INX   =
**     EXTENSIONIN      =
**     EXTENSIONEX      =
**     NFMETALLITMS     =
*    .
*
*  IF line_exists( lt_return[ type = 'E' ] ).
*    EXIT.
*  ENDIF.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = abap_true.




PARAMETERS: p_vbeln  TYPE vbap-vbeln OBLIGATORY DEFAULT '3140000123', "Order Number
            p_posnr  TYPE vbap-posnr OBLIGATORY, "Order Item
            p_etenr  TYPE vbep-etenr OBLIGATORY, "Schedule Line
            p_reqqty TYPE bapischdl-req_qty OBLIGATORY. " Order Qty

*----


DATA: i_hdr  TYPE bapisdh1,
      i_hdrx TYPE bapisdh1x,
      i_ret  TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
      wa_ret TYPE bapiret2.

DATA: BEGIN OF i_sched OCCURS 10.
        INCLUDE STRUCTURE bapischdl.
DATA: END OF i_sched.

DATA: BEGIN OF i_schedx OCCURS 10.
        INCLUDE STRUCTURE bapischdlx.
DATA: END OF i_schedx.


DATA:
  lt_order_schedules_out TYPE bapisdhedutab .

*----

START-OF-SELECTION.

*" Initialize internal tables.

  REFRESH: i_sched, i_schedx, i_ret.
  CLEAR: i_sched, i_schedx, i_ret.

  DATA(lt_sales_documents) =
    VALUE shp_sales_key_t( ( vbeln = |{ p_vbeln ALPHA = IN }| ) ) .

  "Buscando dados da Ordem de Vendas
  CALL FUNCTION 'BAPISDORDER_GETDETAILEDLIST'
    EXPORTING
      i_bapi_view         = VALUE order_view( sdschedule = abap_on )
*     i_memory_read       = i_memory_read
*     i_with_header_conditions = space
    TABLES
      sales_documents     = lt_sales_documents
*     order_headers_out   = order_headers_out
*     order_items_out     = order_items_out
      order_schedules_out = lt_order_schedules_out
*     order_business_out  = order_business_out
*     order_partners_out  = order_partners_out
*     order_address_out   = order_address_out
*     order_statusheaders_out  = order_statusheaders_out
*     order_statusitems_out    = order_statusitems_out
*     order_conditions_out     = order_conditions_out
*     order_cond_head     = order_cond_head
*     order_cond_item     = order_cond_item
*     order_cond_qty_scale     = order_cond_qty_scale
*     order_cond_val_scale     = order_cond_val_scale
*     order_contracts_out = order_contracts_out
*     order_textheaders_out    = order_textheaders_out
*     order_textlines_out = order_textlines_out
*     order_flows_out     = order_flows_out
*     order_cfgs_curefs_out    = order_cfgs_curefs_out
*     order_cfgs_cucfgs_out    = order_cfgs_cucfgs_out
*     order_cfgs_cuins_out     = order_cfgs_cuins_out
*     order_cfgs_cuprts_out    = order_cfgs_cuprts_out
*     order_cfgs_cuvals_out    = order_cfgs_cuvals_out
*     order_cfgs_cublbs_out    = order_cfgs_cublbs_out
*     order_cfgs_cuvks_out     = order_cfgs_cuvks_out
*     order_billingplans_out   = order_billingplans_out
*     order_billingdates_out   = order_billingdates_out
*     order_creditcards_out    = order_creditcards_out
*     extensionout        = extensionout
    .

*" Fill required ORDER_HEADER_IN data.

  i_hdrx-updateflag = 'U'.

  " Fill required SCHEDULE_LINES data.

  i_sched-itm_number  = p_posnr.
  i_sched-sched_line  = p_etenr.
  i_sched-req_qty     = p_reqqty.
  i_schedx-updateflag = 'U'.

  i_schedx-itm_number = p_posnr.
  i_schedx-sched_line = p_etenr.
  i_schedx-req_qty    = 'X'.

  i_schedx-itm_number = 'X'.
  i_schedx-sched_line = 'X'.

  APPEND i_sched.
  APPEND i_schedx.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = p_vbeln
      order_header_in  = i_hdr
      order_header_inx = i_hdrx
    TABLES
      return           = i_ret
      schedule_lines   = i_sched
      schedule_linesx  = i_schedx.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  LOOP AT i_ret.

    WRITE / i_ret-message.

  ENDLOOP.
