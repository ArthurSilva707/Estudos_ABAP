**&---------------------------------------------------------------------*
*& Report ZART_ACESS_METODOS_ATRIB_OO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zart_acess_metodos_atrib_oo.


CLASS lcl_dominio DEFINITION.
  PUBLIC SECTION.
    DATA:
      nome  TYPE dd01l-domname,
      tab_a TYPE TABLE OF dd07v,
      tab_b TYPE TABLE OF dd07v.

    CLASS-METHODS:
      class_constructor,
      _gerar_dominio IMPORTING iv_nome          TYPE dd01l-domname
                     RETURNING VALUE(instancia) TYPE REF TO lcl_dominio.

    METHODS:
      Constructor IMPORTING iv_nome TYPE dd01l-domname,
      resgatar_valores,
      listar_valores.
ENDCLASS.
***************************************************

START-OF-SELECTION.
  DATA:
        lo_dominio TYPE REF TO lcl_dominio.

*  lcl_dominio=>_gerar_dominio.
*  exporting
*    iv_nome = 'XFELD'
*  receiving
*   instancia = lo_dominio_xfeld.

  lo_dominio = lcl_dominio=>_gerar_dominio( 'XFELD' ).
  WRITE:/'O nome do domínio é:', lo_dominio->nome.
  lo_dominio->listar_valores( ).

  lo_dominio = lcl_dominio=>_gerar_dominio( 'BSTAT' ).
  WRITE:/'O nome do domínio é:', lo_dominio->nome.
  lo_dominio->listar_valores( ).

***************************************************

CLASS lcl_dominio IMPLEMENTATION.
  METHOD resgatar_valores.

    CALL FUNCTION 'DD_DOMA_GET'
      EXPORTING
        domain_name    = nome
      TABLES
        dd07v_tab_a    = tab_a
        dd07v_tab_n    = tab_b
      EXCEPTIONS
        illegal_values = 1
        op_failure     = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      "Deu erro, mas não faça nada
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    nome = iv_nome.
  ENDMETHOD.

  METHOD class_constructor.
    WRITE:/'Classe iniciada'.
  ENDMETHOD.

  METHOD _gerar_dominio.

    CREATE OBJECT instancia
      EXPORTING
        iv_nome = iv_nome.
  ENDMETHOD.

  METHOD listar_valores.
    DATA: ls_data_a LIKE LINE OF tab_a.

    LOOP AT tab_a INTO ls_data_a.
      WRITE:/ ls_data_a.
    ENDLOOP.
    ENDMETHOD.
ENDCLASS.

*CLASS lcl_dominio DEFINITION.
*  PUBLIC SECTION.
*    DATA:
*      nome  TYPE dd01l-domname,
*      tab_a TYPE TABLE OF dd07v,
*      tab_b TYPE TABLE OF dd07v.
*
*    CLASS-METHODS:
*      class_constructor,
*      _gerar_dominio IMPORTING iv_nome TYPE dd01l-domname
*                     RETURNING VALUE(instancia) TYPE REF TO lcl_dominio.
*
*    METHODS:
*      constructor IMPORTING iv_nome TYPE dd01l-domname,
*      resgatar_valores,
*      listar_valores.
*
*ENDCLASS.
***********************************************************************
*
*START-OF-SELECTION.
*  DATA: lo_dominio TYPE REF TO lcl_dominio.
*
**  CALL METHOD lcl_dominio=>_gerar_dominio
**    EXPORTING
**      iv_nome = 'XFELD'
**    RECEIVING
**      instancia = lo_dominio_xfeld.
*
*  lo_dominio = lcl_dominio=>_gerar_dominio( 'XFELD' ).
*  WRITE:/ 'O nome do domínio é:', lo_dominio->nome.
*  lo_dominio->listar_valores( ).
*
*
*  lo_dominio = lcl_dominio=>_gerar_dominio( 'BSTAT' ).
*  WRITE:/ 'O nome do domínio é:', lo_dominio->nome.
*  lo_dominio->listar_valores( ).
*
*
***********************************************************************
*CLASS lcl_dominio IMPLEMENTATION.
*
*  METHOD resgatar_valores.
*
*    CALL FUNCTION 'DD_DOMA_GET'
*      EXPORTING
*        domain_name   = nome
*      TABLES
*        dd07v_tab_a   = tab_a
*        dd07v_tab_n   = tab_b
*      EXCEPTIONS
*        illegal_value = 1
*        op_failure    = 2
*        OTHERS        = 3.
*
*    IF sy-subrc <> 0.
*      "Deu erro, mas não faça nada
*    ENDIF.
*
*  ENDMETHOD.
*
*  METHOD constructor.
*    "Atribuição do Nome
*    nome = iv_nome.
*
*    "Chamada de Método
*    resgatar_valores( ).
*  ENDMETHOD.
*
*  METHOD class_constructor.
*    WRITE:/ 'Classe iniciada'.
*  ENDMETHOD.
*
*  METHOD _gerar_dominio.
*
*    CREATE OBJECT instancia
*      EXPORTING
*        iv_nome = iv_nome.
*
**    instancia = NEW lcl_dominio( iv_nome = iv_nome ).
*
*  ENDMETHOD.
*
*  METHOD listar_valores.
*    DATA: ls_data_a LIKE LINE OF tab_a.
*
*    LOOP AT tab_a INTO ls_data_a.
*      WRITE:/ ls_data_a.
*    ENDLOOP.
*  ENDMETHOD.
*ENDCLASS.