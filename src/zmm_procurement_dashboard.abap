REPORT zmm_procurement_dashboard.

************************************************************************
*  Procurement Operations Dashboard                                    *
*  Aggregates KPIs for Purchase Requisitions, Purchase Orders, Goods    *
*  Receipts, Invoice Receipts and remaining open quantities.            *
************************************************************************

TABLES: ekpo.

TYPES: BEGIN OF ty_metric,
         category  TYPE string,
         doc_count TYPE i,
         quantity  TYPE p LENGTH 16 DECIMALS 3,
         unit      TYPE meins,
         amount    TYPE p LENGTH 16 DECIMALS 2,
         currency  TYPE waers,
       END OF ty_metric,
       ty_t_metric TYPE STANDARD TABLE OF ty_metric WITH EMPTY KEY.

TYPES: BEGIN OF ty_pr_item,
         banfn TYPE eban-banfn,
         bnfpo TYPE eban-bnfpo,
         bukrs TYPE eban-bukrs,
         werks TYPE eban-werks,
         erdat TYPE eban-erdat,
       END OF ty_pr_item,
       ty_t_pr TYPE STANDARD TABLE OF ty_pr_item WITH EMPTY KEY.

TYPES: BEGIN OF ty_po_item,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
         werks TYPE ekpo-werks,
         loekz TYPE ekpo-loekz,
         elikz TYPE ekpo-elikz,
         erekz TYPE ekpo-erekz,
         wemng TYPE ekpo-wemng,
         menge TYPE ekpo-menge,
         meins TYPE ekpo-meins,
         bedat TYPE ekko-bedat,
         bukrs TYPE ekko-bukrs,
         ekorg TYPE ekko-ekorg,
         waers TYPE ekko-waers,
         netwr TYPE ekko-netwr,
       END OF ty_po_item,
       ty_t_po TYPE STANDARD TABLE OF ty_po_item WITH EMPTY KEY.

TYPES: BEGIN OF ty_gr_item,
         ebeln TYPE mseg-ebeln,
         ebelp TYPE mseg-ebelp,
         menge TYPE mseg-menge,
         meins TYPE mseg-meins,
         werks TYPE mseg-werks,
         budat TYPE mkpf-budat,
         ekorg TYPE ekko-ekorg,
       END OF ty_gr_item,
       ty_t_gr TYPE STANDARD TABLE OF ty_gr_item WITH EMPTY KEY.

TYPES: BEGIN OF ty_ir_item,
         ebeln TYPE rseg-ebeln,
         ebelp TYPE rseg-ebelp,
         menge TYPE rseg-menge,
         meins TYPE rseg-meins,
         werks TYPE rseg-werks,
         budat TYPE rbkp-budat,
         wrbtr TYPE rseg-wrbtr,
         waers TYPE rbkp-waers,
         ekorg TYPE ekko-ekorg,
       END OF ty_ir_item,
       ty_t_ir TYPE STANDARD TABLE OF ty_ir_item WITH EMPTY KEY.

DATA gt_metrics TYPE ty_t_metric.

PARAMETERS: p_bukrs     TYPE bukrs OBLIGATORY,
            p_date_from TYPE sy-datum,
            p_date_to   TYPE sy-datum DEFAULT sy-datum.

SELECT-OPTIONS: s_ekorg FOR ekko-ekorg,
                s_werks FOR ekpo-werks.

INITIALIZATION.
  p_date_from = sy-datum - 30.

AT SELECTION-SCREEN.
  IF p_date_from > p_date_to.
    MESSAGE 'Start date must be before or equal to end date.' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  PERFORM collect_pr_data CHANGING gt_metrics.
  PERFORM collect_po_data CHANGING gt_metrics.
  PERFORM collect_gr_data CHANGING gt_metrics.
  PERFORM collect_ir_data CHANGING gt_metrics.
  PERFORM display_dashboard USING gt_metrics.

*&---------------------------------------------------------------------*
*&      Form  COLLECT_PR_DATA
*&---------------------------------------------------------------------*
FORM collect_pr_data CHANGING ct_metrics TYPE ty_t_metric.
  DATA lt_pr TYPE ty_t_pr.

  SELECT banfn,
         bnfpo,
         bukrs,
         werks,
         erdat
    FROM eban
    INTO TABLE @lt_pr
    WHERE bukrs = @p_bukrs
      AND erdat BETWEEN @p_date_from AND @p_date_to.

  IF s_werks[] IS NOT INITIAL.
    DELETE lt_pr WHERE werks NOT IN s_werks.
  ENDIF.

  DATA(lv_pr_item_count) = lines( lt_pr ).

  SORT lt_pr BY banfn.

  DATA lv_pr_header_count TYPE i VALUE 0.
  DATA lv_last_banfn TYPE eban-banfn.

  LOOP AT lt_pr INTO DATA(ls_pr).
    IF lv_last_banfn IS INITIAL OR lv_last_banfn <> ls_pr-banfn.
      lv_pr_header_count += 1.
      lv_last_banfn = ls_pr-banfn.
    ENDIF.
  ENDLOOP.

  PERFORM add_metric USING 'Purchase Requisitions'
                           lv_pr_header_count
                           0
                           ''
                           0
                           ''
                           CHANGING ct_metrics.

  PERFORM add_metric USING 'Purchase Requisition Items'
                           lv_pr_item_count
                           0
                           ''
                           0
                           ''
                           CHANGING ct_metrics.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COLLECT_PO_DATA
*&---------------------------------------------------------------------*
FORM collect_po_data CHANGING ct_metrics TYPE ty_t_metric.
  DATA lt_po TYPE ty_t_po.

  SELECT a~ebeln,
         a~ebelp,
         a~werks,
         a~loekz,
         a~elikz,
         a~erekz,
         a~wemng,
         a~menge,
         a~meins,
         b~bedat,
         b~bukrs,
         b~ekorg,
         b~waers,
         b~netwr
    FROM ekpo AS a
    INNER JOIN ekko AS b ON a~ebeln = b~ebeln
    INTO TABLE @lt_po
    WHERE b~bukrs = @p_bukrs
      AND b~bedat BETWEEN @p_date_from AND @p_date_to.

  IF s_ekorg[] IS NOT INITIAL.
    DELETE lt_po WHERE ekorg NOT IN s_ekorg.
  ENDIF.

  IF s_werks[] IS NOT INITIAL.
    DELETE lt_po WHERE werks NOT IN s_werks.
  ENDIF.

  DATA(lv_po_item_count) = lines( lt_po ).

  SORT lt_po BY ebeln.

  DATA lv_po_header_count TYPE i VALUE 0.
  DATA lv_last_ebeln TYPE ekpo-ebeln.
  DATA ls_po TYPE ty_po_item.

  LOOP AT lt_po INTO ls_po.
    IF lv_last_ebeln IS INITIAL OR lv_last_ebeln <> ls_po-ebeln.
      lv_po_header_count += 1.
      lv_last_ebeln = ls_po-ebeln.
    ENDIF.
  ENDLOOP.

  DATA lt_po_header TYPE ty_t_po.
  lt_po_header = lt_po.
  SORT lt_po_header BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lt_po_header COMPARING ebeln.

  DATA(lv_po_value) TYPE p LENGTH 16 DECIMALS 2 VALUE 0.
  DATA(lv_po_currency) TYPE waers.

  LOOP AT lt_po_header INTO DATA(ls_po_header).
    lv_po_value = lv_po_value + CONV #( ls_po_header-netwr ).
    IF lv_po_currency IS INITIAL.
      lv_po_currency = ls_po_header-waers.
    ELSEIF lv_po_currency <> ls_po_header-waers.
      lv_po_currency = 'MULTI'.
    ENDIF.
  ENDLOOP.

  DATA(lv_open_delivery_count) TYPE i VALUE 0.
  DATA(lv_open_delivery_qty) TYPE p LENGTH 16 DECIMALS 3 VALUE 0.
  DATA(lv_open_delivery_uom) TYPE meins.

  LOOP AT lt_po INTO ls_po WHERE loekz = space AND elikz <> 'X' AND menge > wemng.
    lv_open_delivery_count += 1.
    lv_open_delivery_qty += CONV #( ls_po-menge - ls_po-wemng ).
    IF lv_open_delivery_uom IS INITIAL.
      lv_open_delivery_uom = ls_po-meins.
    ELSEIF lv_open_delivery_uom <> ls_po-meins.
      lv_open_delivery_uom = '***'.
    ENDIF.
  ENDLOOP.

  DATA(lv_open_invoice_count) TYPE i VALUE 0.

  LOOP AT lt_po INTO ls_po WHERE loekz = space AND ( erekz IS INITIAL OR erekz <> 'X' ).
    lv_open_invoice_count += 1.
  ENDLOOP.

  PERFORM add_metric USING 'Purchase Orders'
                           lv_po_header_count
                           0
                           ''
                           lv_po_value
                           lv_po_currency
                           CHANGING ct_metrics.

  PERFORM add_metric USING 'Purchase Order Items'
                           lv_po_item_count
                           0
                           ''
                           0
                           ''
                           CHANGING ct_metrics.

  PERFORM add_metric USING 'PO Items Still to Deliver'
                           lv_open_delivery_count
                           lv_open_delivery_qty
                           lv_open_delivery_uom
                           0
                           ''
                           CHANGING ct_metrics.

  PERFORM add_metric USING 'PO Items Still to Invoice'
                           lv_open_invoice_count
                           0
                           ''
                           0
                           ''
                           CHANGING ct_metrics.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COLLECT_GR_DATA
*&---------------------------------------------------------------------*
FORM collect_gr_data CHANGING ct_metrics TYPE ty_t_metric.
  DATA lt_gr TYPE ty_t_gr.

  SELECT m~ebeln,
         m~ebelp,
         m~menge,
         m~meins,
         m~werks,
         k~budat,
         h~ekorg
    FROM mseg AS m
    INNER JOIN mkpf AS k
            ON m~mblnr = k~mblnr
           AND m~mjahr = k~mjahr
    INNER JOIN ekpo AS p
            ON p~ebeln = m~ebeln
           AND p~ebelp = m~ebelp
    INNER JOIN ekko AS h
            ON h~ebeln = p~ebeln
    INTO TABLE @lt_gr
    WHERE h~bukrs = @p_bukrs
      AND k~budat BETWEEN @p_date_from AND @p_date_to
      AND m~bwart IN ( '101', '105', '109' ).

  IF s_werks[] IS NOT INITIAL.
    DELETE lt_gr WHERE werks NOT IN s_werks.
  ENDIF.

  IF s_ekorg[] IS NOT INITIAL.
    DELETE lt_gr WHERE ekorg NOT IN s_ekorg.
  ENDIF.

  DATA(lv_gr_count) = lines( lt_gr ).
  DATA(lv_gr_qty) TYPE p LENGTH 16 DECIMALS 3 VALUE 0.
  DATA(lv_gr_uom) TYPE meins.

  LOOP AT lt_gr INTO DATA(ls_gr).
    lv_gr_qty += CONV #( ls_gr-menge ).
    IF lv_gr_uom IS INITIAL.
      lv_gr_uom = ls_gr-meins.
    ELSEIF lv_gr_uom <> ls_gr-meins.
      lv_gr_uom = '***'.
    ENDIF.
  ENDLOOP.

  PERFORM add_metric USING 'Goods Receipts'
                           lv_gr_count
                           lv_gr_qty
                           lv_gr_uom
                           0
                           ''
                           CHANGING ct_metrics.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COLLECT_IR_DATA
*&---------------------------------------------------------------------*
FORM collect_ir_data CHANGING ct_metrics TYPE ty_t_metric.
  DATA lt_ir TYPE ty_t_ir.

  SELECT r~ebeln,
         r~ebelp,
         r~menge,
         r~meins,
         r~werks,
         k~budat,
         r~wrbtr,
         k~waers,
         h~ekorg
    FROM rseg AS r
    INNER JOIN rbkp AS k
            ON r~belnr = k~belnr
           AND r~gjahr = k~gjahr
    INNER JOIN ekpo AS p
            ON p~ebeln = r~ebeln
           AND p~ebelp = r~ebelp
    INNER JOIN ekko AS h
            ON h~ebeln = p~ebeln
    INTO TABLE @lt_ir
    WHERE k~bukrs = @p_bukrs
      AND k~budat BETWEEN @p_date_from AND @p_date_to.

  IF s_werks[] IS NOT INITIAL.
    DELETE lt_ir WHERE werks NOT IN s_werks.
  ENDIF.

  IF s_ekorg[] IS NOT INITIAL.
    DELETE lt_ir WHERE ekorg NOT IN s_ekorg.
  ENDIF.

  DATA(lv_ir_count) = lines( lt_ir ).
  DATA(lv_ir_amount) TYPE p LENGTH 16 DECIMALS 2 VALUE 0.
  DATA(lv_ir_currency) TYPE waers.

  LOOP AT lt_ir INTO DATA(ls_ir).
    lv_ir_amount += CONV #( ls_ir-wrbtr ).
    IF lv_ir_currency IS INITIAL.
      lv_ir_currency = ls_ir-waers.
    ELSEIF lv_ir_currency <> ls_ir-waers.
      lv_ir_currency = 'MULTI'.
    ENDIF.
  ENDLOOP.

  PERFORM add_metric USING 'Invoice Receipts'
                           lv_ir_count
                           0
                           ''
                           lv_ir_amount
                           lv_ir_currency
                           CHANGING ct_metrics.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ADD_METRIC
*&---------------------------------------------------------------------*
FORM add_metric USING    iv_category TYPE string
                         iv_doc_cnt TYPE i
                         iv_quantity TYPE ty_metric-quantity
                         iv_unit TYPE meins
                         iv_amount TYPE ty_metric-amount
                         iv_currency TYPE waers
                CHANGING ct_metrics TYPE ty_t_metric.
  APPEND VALUE #( category = iv_category
                  doc_count = iv_doc_cnt
                  quantity  = iv_quantity
                  unit      = iv_unit
                  amount    = iv_amount
                  currency  = iv_currency ) TO ct_metrics.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DASHBOARD
*&---------------------------------------------------------------------*
FORM display_dashboard USING it_metrics TYPE ty_t_metric.
  DATA(lo_alv) TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table       = it_metrics ).

      lo_alv->get_columns( )->set_optimize( abap_true ).
      lo_alv->display( ).
    CATCH cx_salv_msg.
      WRITE: / 'Procurement Dashboard Metrics:'.
      ULINE.
      LOOP AT it_metrics ASSIGNING FIELD-SYMBOL(<ls_line>).
        WRITE: / <ls_line>-category,
                 <ls_line>-doc_count,
                 <ls_line>-quantity,
                 <ls_line>-unit,
                 <ls_line>-amount,
                 <ls_line>-currency.
      ENDLOOP.
  ENDTRY.
ENDFORM.
