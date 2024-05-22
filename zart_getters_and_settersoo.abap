**&---------------------------------------------------------------------*
**& Report ZART_GETTERS_AND_SETTERSOO
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT ZART_GETTERS_AND_SETTERSOO.
*
*CLASS lcl_doc_contabil DEFINITION.
*	PUBLIC SECTION.
*	METHODS:
*    constructor IMPORTING iv_belnr TYPE belnr_d
*                          iv_bukrs TYPE bukrs
*                          iv_gjahr TYPE gjahr
*                          iv_blart TYPE blart OPTIONAL,
*
*   apresentar_documento.
*
*  PRIVATE SECTION.
*  DATA:
*   belnr TYPE belnr_d,
*   bukrs TYPE bukrs,
*   gjahr TYPE gjahr,
*   blart TYPE blart.
*ENDCLASS.
*
*START-OF-SELECTION.
*  DATA: lo_documento TYPE REF TO lcl_doc_contabil.
*
*  CREATE OBJECT lo_documento
*   EXPORTING
*     iv_belnr = '1'
*     iv_bukrs = '9999'
*     iv_gjahr = '2019'.
*
*  lo_documento->apresentar_documento( ).
*
*CLASS lcl_doc_contabil IMPLEMENTATION.
*	METHOD constructor.
*    me->belnr = iv_belnr,
*    me->bukrs = iv_bukrs
*	ENDMETHOD.
*ENDCLASS.


**********************************************************************
* DEFINIÇÃO DE CLASSE
**********************************************************************
CLASS lcl_doc_contabil DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_belnr TYPE belnr_d
                            iv_bukrs TYPE bukrs
                            iv_gjahr TYPE gjahr
                            iv_blart TYPE blart OPTIONAL,

      apresentar_documento,

      set_blart IMPORTING iv_blart TYPE blart,
      get_blart RETURNING VALUE(blart) TYPE blart,

      set_belnr IMPORTING iv_belnr TYPE belnr_d,
      get_belnr RETURNING VALUE(return) TYPE belnr_d,

      set_bukrs IMPORTING iv_bukrs TYPE bukrs,
      get_bukrs RETURNING VALUE(return) TYPE bukrs,

      set_gjahr IMPORTING iv_gjahr TYPE gjahr,
      get_gjahr RETURNING VALUE(return) TYPE gjahr.


  PRIVATE SECTION.
    DATA:
      belnr TYPE belnr_d,
      bukrs TYPE bukrs,
      gjahr TYPE gjahr,
      blart TYPE blart.
ENDCLASS.


**********************************************************************
* INICIO DE PROCESSAMENTO
**********************************************************************
START-OF-SELECTION.
  DATA: lo_documento TYPE REF TO lcl_doc_contabil.

  CREATE OBJECT lo_documento
    EXPORTING
      iv_belnr = '1'
      iv_bukrs = '9999'
      iv_gjahr = '2019'.

  lo_documento->set_blart( 'DZ' ).

  lo_documento->apresentar_documento( ).

  lo_documento->set_blart( 'PC' ).

  WRITE:/ lo_documento->get_bukrs( ).

  lo_documento->apresentar_documento( ).


**********************************************************************
* IMPLEMENTAÇÃO DE CLASSE
**********************************************************************
CLASS lcl_doc_contabil IMPLEMENTATION.
  "$. Region Getters e Setters
  METHOD set_blart.
    me->blart = iv_blart.
  ENDMETHOD.
  METHOD get_blart.
    blart = me->blart.
  ENDMETHOD.

  METHOD set_belnr.
    me->belnr = iv_belnr.
  ENDMETHOD.
  METHOD get_belnr.
    return = me->belnr.
  ENDMETHOD.

  METHOD set_gjahr.
    me->gjahr = iv_gjahr.
  ENDMETHOD.
  METHOD get_gjahr.
    return = me->gjahr.
  ENDMETHOD.

  METHOD set_bukrs.
    me->bukrs = iv_bukrs.
  ENDMETHOD.
  METHOD get_bukrs.
    return = me->bukrs.
  ENDMETHOD.
  "$. Endregion Getters e Setters

  METHOD constructor.
    me->belnr = iv_belnr.
    me->bukrs = iv_bukrs.
    me->gjahr = iv_gjahr.
    me->blart = iv_blart.
  ENDMETHOD.

  METHOD apresentar_documento.
    WRITE:/ 'Documento Contábil', me->belnr,
          / 'Exercício', me->gjahr,
          / 'Empresa', me->bukrs,
          / 'Tipo de Documento', me->blart.
  ENDMETHOD.
ENDCLASS.