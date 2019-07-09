      ******************************************************************
      * Autor.....: Alexandre Trevisani (PROVA COBOL)
      * Data......: Julho/2019
      * Programa..: PROG00 - MENU Sitema Distribuição Cliente X Vendedor
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PMENU.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-MODULO.
           05 FILLER        PIC X(30) VALUE "PROVA TESTE COBOL".
           05 FILLER        PIC X(12) VALUE "MENSAGEM :".
           05 WS-MENSAG     PIC X(20) VALUE SPACES.

       77 WS-OPCAO          PIC 9     VALUE ZEROS.

       77 WS-NUML           PIC 999.
       77 WS-NUMC           PIC 999.
       77 COR-FUNDO         PIC 9 VALUE 1.
       77 COR-FRENTE        PIC 9 VALUE 6.


       77 WS-STATUS         PIC X(30).
       77 WS-MSGERRO        PIC X(80).
       77 WS-FUNCAO         PIC X.
          88 WS-FIM         VALUE 'F'.

       SCREEN SECTION.

       01 SS-CLS.
           05 SS-FILLER.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
               10 LINE WS-NUML COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-CABECALHO.
               10 LINE 01 COLUMN 02 PIC X(70) FROM WS-MODULO
                  HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-STATUS.
               10 LINE WS-NUML COLUMN 2 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
       01 SS-MENU FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 45 VALUE "CADASTROS".
           05 LINE 08 COLUMN 45 VALUE "1   CLIENTES".
           05 LINE 09 COLUMN 45 VALUE "2   VENDEDORES".
           05 LINE 10 COLUMN 45 VALUE "RELATORIOS".
           05 LINE 11 COLUMN 45 VALUE "3   CLIENTES".
           05 LINE 12 COLUMN 45 VALUE "4   VENDEDORES".
           05 LINE 13 COLUMN 45 VALUE "EXECUTAR".
           05 LINE 14 COLUMN 45 VALUE "5   DISTRIBUICAO".
           05 LINE 15 COLUMN 45 VALUE "FINALIZAR".
           05 LINE 16 COLUMN 45 VALUE "9   FIM PROCESSAMENTO".
           05 LINE 18 COLUMN 45 VALUE "OPCAO :  ".
           05 LINE 18 COLUMN 53 USING WS-OPCAO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT WS-NUML FROM LINES
           ACCEPT WS-NUMC FROM COLUMNS
      *    DISPLAY SS-CLS
           .
       DIG-OPCAO.
           PERFORM UNTIL WS-FIM
                   DISPLAY SS-CLS
                   ACCEPT SS-MENU
                   PERFORM ROT-VALIDA
                   DISPLAY SS-CABECALHO
           END-PERFORM

           PERFORM ROT-FIM
            .
       MAIN-PROCEDURE-FIM.
            EXIT.

      *-----------------------------------------------------------------
       ROT-VALIDA.
      *-----------------------------------------------------------------

            EVALUATE WS-OPCAO
            WHEN 1
                 MOVE "CADASTRO CLIENTES"      TO WS-MENSAG
                 DISPLAY SS-CABECALHO
                 CALL "PROG01" END-CALL
            WHEN 2
                 MOVE "CADASTRO VENDEDORES"    TO WS-MENSAG
                 DISPLAY SS-CABECALHO
                 CALL "PROG04" END-CALL
            WHEN 3
                 MOVE "RELATORIO CLIENTES"     TO WS-MENSAG
                 DISPLAY SS-CABECALHO
                 CALL "PROG02" END-CALL
            WHEN 4
                 MOVE "RELATORIO VENDEDORES"   TO WS-MENSAG
                 DISPLAY SS-CABECALHO
                 CALL "PROG05" END-CALL
            WHEN 5
                 MOVE "DISTRIBUICAO"           TO WS-MENSAG
                 DISPLAY SS-CABECALHO
                 CALL "PROG09" END-CALL
            WHEN 9
                 MOVE "FIM PROCESSAMENTO"      TO WS-MENSAG
                 PERFORM ROT-FIM
            WHEN OTHER
                 MOVE "OPCAO INVALIDA"         TO WS-MENSAG
                 DISPLAY SS-CABECALHO
           END-EVALUATE
            .
       ROT-VALIDA-EXIT.
           EXIT.

       ROT-FIM.
           STOP RUN.
