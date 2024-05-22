*&---------------------------------------------------------------------*
*& Report ZART_HERANCA_METODOS_OO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZART_HERANCA_METODOS_OO.

CLASS lcl_documento DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_doc TYPE char10,

      descrever_documento,

      set_v_doc IMPORTING iv_v_doc      TYPE char10,
      get_v_doc RETURNING VALUE(return) TYPE char10.

  PRIVATE SECTION.
    DATA:
      v_doc TYPE char10.

    METHODS:
      ajustar_nr_doc.
ENDCLASS.

CLASS lcl_documento_contabil DEFINITION INHERITING FROM lcl_documento.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_doc     TYPE char10
                            iv_ano     TYPE gjahr
                            iv_empresa TYPE bukrs,

      descrever_documento REDEFINITION.

  PRIVATE SECTION.
    DATA:
      v_ano     TYPE gjahr,
      v_empresa TYPE bukrs.
ENDCLASS.


"DOCUMENTO MATERIAL
"ANO

START-OF-SELECTION.
  DATA: lo_documento_contabil TYPE REF TO lcl_documento_contabil.

  CREATE OBJECT lo_documento_contabil
    EXPORTING
      iv_doc     = '1'
      iv_ano     = '2019'
      iv_empresa = '9999'.

  lo_documento_contabil->descrever_documento( ).



CLASS lcl_documento IMPLEMENTATION.
  METHOD constructor.
    me->v_doc = iv_doc.
  ENDMETHOD.

  METHOD set_v_doc.
    me->v_doc = iv_v_doc.
  ENDMETHOD.
  METHOD get_v_doc.
    return = me->v_doc.
  ENDMETHOD.

  METHOD descrever_documento.
    me->ajustar_nr_doc( ).

    WRITE:/ 'Nº Documento', me->v_doc.
  ENDMETHOD.

  METHOD ajustar_nr_doc.
    me->v_doc = |{ me->v_doc ALPHA = IN }|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_documento_contabil IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_doc ).

    me->v_ano     = iv_ano.
    me->v_empresa = iv_empresa.
  ENDMETHOD.

  METHOD descrever_documento.
    super->descrever_documento( ).

    WRITE:/ 'Ano', me->v_ano,
          / 'Empresa', me->v_empresa.
  ENDMETHOD.
ENDCLASS.