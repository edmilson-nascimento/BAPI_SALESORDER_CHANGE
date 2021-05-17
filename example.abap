*&---------------------------------------------------------------------*
*& Report YTESTE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yteste.

*PARAMETERS:
*  p_vbeln  TYPE vbap-vbeln        OBLIGATORY DEFAULT '3130000524', "Order Number
*  p_posnr  TYPE vbap-posnr        OBLIGATORY, "Order Item
*  p_etenr  TYPE vbep-etenr        OBLIGATORY, "Schedule Line
*  p_reqqty TYPE bapischdl-req_qty OBLIGATORY. " Order Qty
*
*DATA:
*  lt_order_schedules_out TYPE bapisdhedutab,
*
*  ls_order_header_in     TYPE bapisdh1,
*  ls_order_header_inx    TYPE bapisdh1x,
*  lt_return              TYPE bapiret2_t,
*  ls_return              TYPE bapiret2,
*  lt_schedule_lines      TYPE TABLE OF bapischdl,
*  lt_schedule_linesx     TYPE TABLE OF bapischdlx.
*
*
*START-OF-SELECTION.
*
*  " Initialize internal tables.
*  CLEAR:
*    lt_order_schedules_out,ls_order_header_in, ls_order_header_inx, lt_return,
*    ls_return, lt_schedule_lines, lt_schedule_linesx .
*
*  " Fill required ORDER_HEADER_IN data
*  ls_order_header_inx-updateflag = 'U'.
*
*  " Fill required SCHEDULE_LINES data.
*  lt_schedule_lines =
*    VALUE #( ( itm_number  = p_posnr
*               sched_line  = p_etenr
*               req_qty     = p_reqqty ) ) .
*
*  lt_schedule_linesx =
*    VALUE #( ( itm_number = p_posnr
*               sched_line = p_etenr
*               req_qty    = 'X'
*               updateflag = 'U' ) ) .
*
*
*  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*    EXPORTING
*      salesdocument    = CONV vbeln_va( |{ p_vbeln ALPHA = IN }| )
*      order_header_in  = ls_order_header_in
*      order_header_inx = ls_order_header_inx
*    TABLES
*      return           = lt_return
*      schedule_lines   = lt_schedule_lines
*      schedule_linesx  = lt_schedule_linesx.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*
*  LOOP AT lt_return INTO ls_return .
*    WRITE / ls_return-message .
*  ENDLOOP.


*
**Tables:
*TABLES:
*  vbap.
*
**Internal tables:
*DATA:
*  gt_vbap               TYPE STANDARD TABLE OF vbap,
*  gt_vbep               TYPE STANDARD TABLE OF vbep,
*  gt_item_in            TYPE STANDARD TABLE OF bapisditm,
*  gt_item_inx           TYPE STANDARD TABLE OF bapisditmx,
*  gt_schedule_lines     TYPE STANDARD TABLE OF bapischdl,
*  gt_schedule_linesx    TYPE STANDARD TABLE OF bapischdlx,
*  gt_return             TYPE STANDARD TABLE OF bapiret2.
*
**Field Symbols:
*FIELD-SYMBOLS:
*  <fs_vbap>             TYPE vbap,
*  <fs_vbep>             TYPE vbep.
*
**Structures:
*DATA:
*  gst_item_hedx         TYPE bapisdh1x,
*  gst_item_in           TYPE bapisditm,
*  gst_item_inx          TYPE bapisditmx,
*  gst_schedule_lines    TYPE bapischdl,
*  gst_schedule_linesx   TYPE bapischdlx.
*
**Variables:
*DATA:
*  gv_msg                TYPE string,
*  gv_tabix              TYPE sy-tabix.
*
**Constants:
*CONSTANTS:
*  gc_error              TYPE string
*        VALUE ': An error occured, no change done to the sales order.',
*  gc_success            TYPE string
*        VALUE ': Sales order changed successfully.'.
*
*************************************************************************
** SELECTION SCREEN                                                     *
*************************************************************************
*SELECT-OPTIONS:
**  Sales Order Number.
*   s_vbeln FOR vbap-vbeln OBLIGATORY.
*
*PARAMETERS:
*** Reason for Rejection.
**  p_abgru TYPE vbap-abgru OBLIGATORY,
*
** Order Quantity.
*  p_wmeng TYPE vbep-wmeng OBLIGATORY.
*
*
*************************************************************************
** CODE LOGIC                                                           *
*************************************************************************
*
**Select sales order data from table VBAP.
*SELECT *
*  FROM vbap
*  INTO TABLE gt_vbap
*  WHERE vbeln IN s_vbeln.
*
*IF sy-subrc EQ 0.
*
** Rules 'For All Entries'.
** Not necessary as 'VBELN' and 'POSNR' is already primary key, this
**                                                              logic
** just to demonstrate 'For All Entries' rule.
*  SORT gt_vbap BY vbeln posnr.
*  DELETE ADJACENT DUPLICATES FROM gt_vbap COMPARING vbeln posnr.
*
** Retrieving schedule lines entries.
*  SELECT *
*    FROM vbep
*   INTO TABLE gt_vbep
* FOR ALL ENTRIES IN gt_vbap
*   WHERE vbeln EQ gt_vbap-vbeln
*     AND posnr EQ gt_vbap-posnr.
*
*  IF sy-subrc EQ 0.
*
**   Sorting for binary search.
*    SORT gt_vbep BY vbeln posnr.
*
*    LOOP AT gt_vbap ASSIGNING <fs_vbap>.
*
**     ........................
**     BAPI Data for updating the reason for rejection.
**     ........................
**     (Order Header Level)
**     Setting the update flag at order header level to update mode.
*      gst_item_hedx-updateflag = 'U'.
*
**     (Order Item Level)
**     Setting of the material number(MATNR) at order item level.
*      gst_item_in-material = <fs_vbap>-matnr.
*
**     Setting of the item number(POSNR) at order item level.
*      gst_item_in-itm_number  = <fs_vbap>-posnr.
*      gst_item_inx-itm_number = <fs_vbap>-posnr.
*
***     Setting of the reason for rejection(ABGRU) at order item level.
**      gst_item_in-reason_rej  = p_abgru.
**      gst_item_inx-reason_rej = 'X'.
*
**     Setting the update flag at order item level to update mode.
*      gst_item_inx-updateflag = 'U'.
*
**     BAPI items level tables:
*      APPEND:
*        gst_item_in  TO gt_item_in,
*        gst_item_inx TO gt_item_inx.
*
**     ........................
**     BAPI Data for updating the order quantity.
**     ........................
**     Adding the schedule lines items.
*      READ TABLE gt_vbep TRANSPORTING NO FIELDS
*      WITH KEY  vbeln = <fs_vbap>-vbeln
*                posnr = <fs_vbap>-posnr
*                BINARY SEARCH.
*
*      IF sy-subrc EQ 0.
*
*        gv_tabix = sy-tabix.
*
**       Index looping for better performance.
*        LOOP AT gt_vbep ASSIGNING <fs_vbep> FROM gv_tabix.
*
*          IF  <fs_vbep>-vbeln EQ <fs_vbap>-vbeln
*          AND <fs_vbep>-posnr EQ <fs_vbap>-posnr.
*
**           (Schedule Line Level)
**           Setting of the item number(POSNR) at schedule line level.
*            gst_schedule_lines-itm_number  = <fs_vbep>-posnr.
*            gst_schedule_linesx-itm_number = <fs_vbep>-posnr.
*
**           Setting of the schedule line number(ETENR) at schedule
**                                                        line level.
*            gst_schedule_lines-sched_line  = <fs_vbep>-etenr.
*            gst_schedule_linesx-sched_line = <fs_vbep>-etenr.
*
**           Setting the update flag at schedule line level to update
**                                                                mode.
*            gst_schedule_linesx-updateflag = 'U'.
*
**           Setting the new order quantity(WMENG).
*            gst_schedule_lines-req_qty  = p_wmeng.
*            gst_schedule_linesx-req_qty = 'X'.
*
**           BAPI schedule lines level tables:
*            APPEND:
*              gst_schedule_lines  TO gt_schedule_lines,
*              gst_schedule_linesx TO gt_schedule_linesx.
*
*          ELSE.
*
**           Clear index
*            CLEAR gv_tabix.
*
**           Move out of the loop.
*            EXIT.
*
*          ENDIF.
*
**         Clearing of work areas.
*          CLEAR:
*            gst_schedule_lines,
*            gst_schedule_linesx.
*
*        ENDLOOP.
*
*      ENDIF.
*
**     Calling BAPI to update reason for rejection and the
**     schedule line order quantity in the selected sales order.
*      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*        EXPORTING
*          salesdocument    = <fs_vbap>-vbeln
*          order_header_inx = gst_item_hedx
*        TABLES
*          return           = gt_return
*          order_item_in    = gt_item_in
*          order_item_inx   = gt_item_inx
*          schedule_lines   = gt_schedule_lines
*          schedule_linesx  = gt_schedule_linesx.
*
**     Preparing the result message.
*      CONCATENATE <fs_vbap>-vbeln   " Sales Order Number
*                  <fs_vbap>-posnr   " Item Number
*             INTO gv_msg            " Message
*     SEPARATED BY space.            " Space
*
**     Check if at least one error was raised by the BAPI. Loop inside
**     loop is not advise, however, the return table will contains small
**     amount of entries. We can use that for our demo.
*      LOOP AT gt_return TRANSPORTING NO FIELDS
*      WHERE type EQ 'E'
*         OR type EQ 'A'.
*
**       Exit and rollback changes.
*        EXIT.
*
*      ENDLOOP.
*
**     If error found, rollback database changes.
*      IF sy-subrc EQ 0.
*
**       Rollback changes.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
**       Preparing error message.
*        CONCATENATE gv_msg        "Sales Order and Item Number
*                    gc_error      "Error Message
*               INTO gv_msg
*       SEPARATED BY space.
*
**       Output message.
*        WRITE / gv_msg.
*
**     Else, no error found, commit database changes.
*      ELSE.
*
**       Commit changes.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = abap_true.
*
**       Preparing success message.
*        CONCATENATE gv_msg        "Sales Order and Item Number
*                    gc_success    "Success Message
*               INTO gv_msg
*       SEPARATED BY space.
*
**       Output message.
*        WRITE / gv_msg.
*
*      ENDIF.
*
**     Write a line after each sales order.
*      AT END OF vbeln.
*        WRITE: sy-uline.
*      ENDAT.
*
**     Clearing of variables and structures:
*      CLEAR:
**       Variables:
*        gv_msg,
*        gv_tabix,
**       Structures:
*        gst_item_hedx,
*        gst_item_in,
*        gst_item_inx.
*
**     Refreshing internal tables:
*      REFRESH:
*        gt_item_in,
*        gt_item_inx,
*        gt_schedule_lines,
*        gt_schedule_linesx,
*        gt_return.
*
*    ENDLOOP.
*
*  ENDIF.
*
*ENDIF.


data: gc_new_vbeln  type bapivbeln-vbeln,
      it_ret        type table of bapiret2,
      wa_header     type bapisdh1,
      wa_headerx    type bapisdh1x,
      wa_bape_vbap  type bape_vbap,
      wa_bape_vbapx type bape_vbapx,
      it_sched      type table of bapischdl,
      it_schedx     type table of bapischdlx,
      it_extin      type table of bapiparex,
      it_item       type table of bapisditm,
      it_itemx      type table of bapisditmx.

parameters: p_vbeln type vbrk-vbeln obligatory.

start-of-selection.

  call function 'BAPI_SALESDOCUMENT_COPY'
    exporting
      salesdocument    = p_vbeln
      documenttype     = 'ZSMD'
    importing
      salesdocument_ex = gc_new_vbeln
    tables
      return           = it_ret.

  check gc_new_vbeln is not initial.

  call function 'BAPI_TRANSACTION_COMMIT' exporting wait = if_salv_c_bool_sap=>true.

  refresh: it_ret, it_extin, it_sched, it_schedx, it_extin, it_item, it_itemx.

  wa_header-ord_reason = 'ZRD'.
  wa_headerx-updateflag = 'U'.
  wa_headerx-ord_reason = if_salv_c_bool_sap=>true.

  append value: bapischdl( itm_number = 1
                           sched_line = 1
                           req_qty = 200 ) to it_sched,

                bapischdlx( itm_number = 1
                            sched_line = 1
                            updateflag = 'U'
                            req_qty = if_salv_c_bool_sap=>true ) to it_schedx,

                bapisditm( itm_number = 1
                           "hg_lv_item = '000000'
                           "item_categ = 'ZSMD'
                           target_qty = 200 ) to it_item,

                bapisditmx( itm_number = 1
                            updateflag = 'U'
                            "hg_lv_item = if_salv_c_bool_sap=>true
                            "item_categ = if_salv_c_bool_sap=>true
                            target_qty = if_salv_c_bool_sap=>true ) to it_itemx.

  wa_bape_vbap-vbeln = gc_new_vbeln.
  wa_bape_vbap-zzqter  = '5'.
  wa_bape_vbap-zajahr  = sy-datum(4).
  wa_bape_vbap-posnr   = 1.

  data(lw_extin) = value bapiparex( ).

  lw_extin-structure = 'BAPE_VBAP'.
  lw_extin+30(960)   = wa_bape_vbap.
  append lw_extin to it_extin.

  wa_bape_vbapx-vbeln = gc_new_vbeln.
  wa_bape_vbapx-posnr  = 1.
  wa_bape_vbapx-zzqter = if_salv_c_bool_sap=>true.
  wa_bape_vbapx-zajahr = if_salv_c_bool_sap=>true.

  lw_extin-structure = 'BAPE_VBAPX'.
  lw_extin+30(960)   = wa_bape_vbapx.
  append lw_extin to it_extin.

  call function 'BAPI_SALESORDER_CHANGE'
    exporting
      salesdocument    = gc_new_vbeln
      order_header_in  = wa_header
      order_header_inx = wa_headerx
    tables
      return           = it_ret
      order_item_in    = it_item
      order_item_inx   = it_itemx
      schedule_lines   = it_sched
      schedule_linesx  = it_schedx
      extensionin      = it_extin.

  if line_exists( it_ret[ type = 'E' ] ).

  else.
    call function 'BAPI_TRANSACTION_COMMIT' exporting wait = if_salv_c_bool_sap=>true.
  endif.
