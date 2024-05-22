*&---------------------------------------------------------------------*
*& Report ZART_GETTERS_AND_SETTERS_TYPES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZART_GETTERS_AND_SETTERS_TYPES.

TABLES: makt.

**********************************************************************
* DEFINIÇÃO DEFERIDA
**********************************************************************
CLASS lcl_material DEFINITION DEFERRED.


**********************************************************************
* DEFINIÇÕES DE CLASSE
**********************************************************************
CLASS lcl_descricao_material DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_material TYPE REF TO lcl_material
                            iv_idioma   TYPE sy-langu,

      set_idioma IMPORTING iv_idioma     TYPE sy-langu,
      get_idioma RETURNING VALUE(return) TYPE sy-langu,

      set_material IMPORTING iv_material   TYPE REF TO lcl_material,
      get_material RETURNING VALUE(return) TYPE REF TO lcl_material,

      set_descricao IMPORTING iv_descricao  TYPE makt-maktx,
      get_descricao RETURNING VALUE(return) TYPE makt-maktx.

  PRIVATE SECTION.
    DATA:
      idioma    TYPE sy-langu,
      material  TYPE REF TO lcl_material,
      descricao TYPE makt-maktx.
ENDCLASS.

CLASS lcl_material DEFINITION.
  PUBLIC SECTION.
    TYPES: t_descricoes TYPE TABLE OF REF TO lcl_descricao_material WITH NON-UNIQUE KEY table_line.

    METHODS:
      constructor IMPORTING iv_material TYPE matnr,

      set_matnr IMPORTING iv_matnr      TYPE matnr,
      get_matnr RETURNING VALUE(return) TYPE matnr,

      adicionar_descricao IMPORTING iv_idioma TYPE sy-langu,

      set_descricoes IMPORTING iv_descricoes TYPE t_descricoes,
      get_descricoes RETURNING VALUE(return) TYPE t_descricoes.

  PRIVATE SECTION.
    DATA:
      matnr      TYPE matnr,
      descricoes TYPE TABLE OF REF TO lcl_descricao_material.
ENDCLASS.

CLASS lcl_programa DEFINITION.
  PUBLIC SECTION.
    TYPES: r_langu TYPE RANGE OF sy-langu.

    CLASS-METHODS:
      _start_of_selection IMPORTING iv_material TYPE matnr
                                    ir_langu    TYPE r_langu.
ENDCLASS.



**********************************************************************
* TELA DE SELEÇÃO
**********************************************************************
PARAMETERS: p_matnr TYPE matnr DEFAULT '100-431'.
SELECT-OPTIONS s_langu FOR makt-spras NO INTERVALS DEFAULT: 'PT' OPTION EQ SIGN I.


**********************************************************************
* INÍCIO DA APLICAÇÃO
**********************************************************************
START-OF-SELECTION.
  lcl_programa=>_start_of_selection( iv_material = p_matnr
                                     ir_langu    = s_langu[] ).



**********************************************************************
* IMPLEMENTAÇÃO DE CLASSE
**********************************************************************
CLASS lcl_programa IMPLEMENTATION.
  METHOD _start_of_selection.

    DATA: lo_material   TYPE REF TO lcl_material,
          lo_descricao  TYPE REF TO lcl_descricao_material,
          lt_descricoes TYPE lcl_material=>t_descricoes,
          ls_langu      LIKE LINE OF ir_langu.

    "Cria o material
    CREATE OBJECT lo_material
      EXPORTING
        iv_material = iv_material.

    "Cria as descrições
    LOOP AT ir_langu INTO ls_langu.
      lo_material->adicionar_descricao( ls_langu-low ).
    ENDLOOP.

    "Escreve o Material em tela
    WRITE:/ 'Material:', lo_material->get_matnr( ).

    "Escreve as descrições em tela
    lt_descricoes = lo_material->get_descricoes( ).
    LOOP AT lt_descricoes INTO lo_descricao.
      WRITE:/ lo_descricao->get_idioma( ), / lo_descricao->get_descricao( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_material IMPLEMENTATION.
  "$. Region Getters and Setters
  METHOD set_matnr.
    me->matnr = iv_matnr.
  ENDMETHOD.
  METHOD get_matnr.
    return = me->matnr.
  ENDMETHOD.

  METHOD set_descricoes.
    me->descricoes = iv_descricoes.
  ENDMETHOD.
  METHOD get_descricoes.
    return = me->descricoes.
  ENDMETHOD.
  "$. Endregion Getters and Setters

  METHOD constructor.
    me->matnr = iv_material.
  ENDMETHOD.

  METHOD adicionar_descricao.
    DATA: lo_descricao TYPE REF TO lcl_descricao_material.

    CREATE OBJECT lo_descricao
      EXPORTING
        iv_idioma   = iv_idioma
        io_material = me. "No construtor da descrição, o mesmo recebe uma referência de material. O ME indica a referência do atual material que está criando essa descrição

    APPEND lo_descricao TO me->descricoes.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_descricao_material IMPLEMENTATION.
  "$. Region Getters and Setters
  METHOD set_idioma.
    me->idioma = iv_idioma.
  ENDMETHOD.
  METHOD get_idioma.
    return = me->idioma.
  ENDMETHOD.

  METHOD set_material.
    me->material = iv_material.
  ENDMETHOD.
  METHOD get_material.
    return = me->material.
  ENDMETHOD.

  METHOD set_descricao.
    me->descricao = iv_descricao.
  ENDMETHOD.
  METHOD get_descricao.
    return = me->descricao.
  ENDMETHOD.
  "$. Endregion Getters and Setters

  METHOD constructor.
    DATA: lv_matnr TYPE matnr.

    me->idioma   = iv_idioma.
    me->material = io_material.

    lv_matnr = me->material->get_matnr( ).

    SELECT SINGLE maktx
      INTO me->descricao
      FROM makt
     WHERE matnr = lv_matnr
       AND spras = me->idioma.
  ENDMETHOD.
ENDCLASS.