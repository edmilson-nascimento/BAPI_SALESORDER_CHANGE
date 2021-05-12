*&---------------------------------------------------------------------*
*& Report YTESTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yteste.

PARAMETERS:
  p_vbeln  TYPE vbap-vbeln        OBLIGATORY DEFAULT '3140000123', "Order Number
  p_posnr  TYPE vbap-posnr        OBLIGATORY, "Order Item
  p_etenr  TYPE vbep-etenr        OBLIGATORY, "Schedule Line
  p_reqqty TYPE bapischdl-req_qty OBLIGATORY. " Order Qty

DATA:
  lt_order_schedules_out TYPE bapisdhedutab,

  ls_order_header_in     TYPE bapisdh1,
  ls_order_header_inx    TYPE bapisdh1x,
  lt_return              TYPE bapiret2_t,
  ls_return              TYPE bapiret2,
  lt_schedule_lines      TYPE TABLE OF bapischdl,
  lt_schedule_linesx     TYPE TABLE OF bapischdlx.


START-OF-SELECTION.

  " Initialize internal tables.
  CLEAR:
    lt_order_schedules_out,ls_order_header_in, ls_order_header_inx, lt_return,
    ls_return, lt_schedule_lines, lt_schedule_linesx .

  " Fill Sales Order
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

  " Fill required ORDER_HEADER_IN data
  ls_order_header_inx-updateflag = 'U'.

*  " Fill required SCHEDULE_LINES data.
*  i_sched-itm_number  = p_posnr.
*  i_sched-sched_line  = p_etenr.
*  i_sched-req_qty     = p_reqqty.
*  i_schedx-updateflag = 'U'.
*
*  i_schedx-itm_number = p_posnr.
*  i_schedx-sched_line = p_etenr.
*  i_schedx-req_qty    = 'X'.
*
*  i_schedx-itm_number = 'X'.
*  i_schedx-sched_line = 'X'.
*
*  APPEND i_sched.
*  APPEND i_schedx.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = p_vbeln
      order_header_in  = ls_order_header_in
      order_header_inx = ls_order_header_inx
    TABLES
      return           = lt_return
      schedule_lines   = lt_schedule_lines
      schedule_linesx  = lt_schedule_linesx.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

*  LOOP AT lt_return.
*
*    WRITE / lt_return-message.
*
*  ENDLOOP.
