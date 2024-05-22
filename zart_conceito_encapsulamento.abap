*&---------------------------------------------------------------------*
*& Report ZART_CONCEITO_ENCAPSULAMENTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zart_conceito_encapsulamento.

CLASS lcl_documento_contabil DEFINITION.
  PUBLIC SECTION.
  METHODS:
    constructor IMPORTING iv_belnr  TYPE bkpf-belnr
                          iv_gjahr TYPE bkpf-gjahr
                          iv_bukrs TYPE bkpf-bukrs
                          iv_blart TYPE bkpf-blart
                          iv_bldat TYPE bkpf-bldat,
    descrever_documento,

    efetuar_compensacao RETURNING VALUE(result) TYPE REF TO lcl_documento_contabil.

  PRIVATE SECTION.
    DATA:
      belnr TYPE bkpf-belnr, "Nº DOCUMENTO
      gjahr TYPE bkpf-gjahr, "Exércicio
      bukrs TYPE bkpf-gjahr, "Empresa
      blart TYPE bkpf-blart, "Tipo de Documento
      bldat TYPE bkpf-bldat. "Data do Documento
ENDCLASS.


START-OF-SELECTION.
*  DATA: lo_documento TYPE REF TO lcl_documento_contabil.
*
*  CREATE OBJECT lo_documento
*    EXPORTING
*      iv_belnr = '0000000001'
*      iv_gjahr = '2019'
*      iv_bukrs = '9999'
*      iv_blart = 'DZ' "Partida MEMO
*      iv_bldat = sy-datum.

  DATA(lo_documento) = NEW lcl_documento_contabil( iv_belnr = '0000000001'
                                                   iv_gjahr = '2019'
                                                   iv_bukrs = '9999'
                                                   iv_blart = 'DZ' "Partida MEMO
                                                   iv_bldat = sy-datum ).

*  lo_documento->belnr = '2'.

  lo_documento->descrever_documento( ).



CLASS lcl_documento_contabil IMPLEMENTATION.
  METHOD constructor.
    me->belnr = iv_belnr.
    me->gjahr = iv_gjahr.
    me->bukrs = iv_bukrs.
    me->blart = iv_blart.
    me->bldat = iv_bldat.
  ENDMETHOD.

  METHOD descrever_documento.
    WRITE:/ 'Nº Documento', me->belnr,      /
            'Exercício', me->gjahr,         /
            'Empresa', me->bukrs,           /
            'Tipo de Documento', me->blart, /
            'Data do Documento', me->bldat.
  ENDMETHOD.

  METHOD efetuar_compensacao.
    "Efetua compensação do Documento
  ENDMETHOD.
ENDCLASS.