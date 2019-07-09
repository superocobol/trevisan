      ******************************************************************
      * Autor.....: Alexandre Trevisani (PROVA COBOL)
      * Data......: Julho/2019
      * Programa..: PROG01 - Manutenção Cadastro CLIENTES
      * chamadas..: PROG07 - Verifica Integridade CNPJ
      *           : PROG03 - Programa de Importação Clientes
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PROG01.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.

       SELECT CLIENTES ASSIGN TO "C:\COBOL\CLIENTES.ARQ"
              ORGANIZATION         IS INDEXED
              ACCESS MODE          IS RANDOM
              FILE STATUS          IS CLI-STAT
              ALTERNATE RECORD KEY IS CLI-COD
              ALTERNATE RECORD KEY IS CLI-NOME  WITH DUPLICATES
              LOCK MODE IS MANUAL WITH LOCK ON MULTIPLE RECORDS
              RECORD KEY           IS CLI-KEY.

       DATA                DIVISION.
       FILE                SECTION.

       COPY FD_CLIENTES.

      * FD CLIENTES.

       WORKING-STORAGE SECTION.

       01 WS-MODULO.
           05 FILLER        PIC X(30) VALUE
              "PROVA COBOL - CAD. CLIENTES".
           05 FILLER        PIC X(12) VALUE "MENSAGEM :".
           05 WS-MENSAG     PIC X(40) VALUE SPACES.

       77 WS-OPCAO          PIC X(03) VALUE SPACES.
           88 INCLUIR       VALUE IS "INC" "inc".
           88 IMPORTAR      VALUE IS "IMP" "imp".
           88 ALTERAR       VALUE IS "ALT" "alt".
           88 EXCLUIR       VALUE IS "EXC" "exc".
           88 FINALIZAR     VALUE IS "FIM" "fim".

       77 CLI-STAT          PIC 9(02).
           88 FS-OK         VALUE ZEROS.
           88 FS-NAO-EXISTE VALUE 35.
           88 FS-CANCELA    VALUE 99.

       77 WS-ERRO           PIC X.
           88 E-SIM         VALUE "S".
           88 E-NAO         VALUE "N".

       77 WS-NUML           PIC 999.
       77 WS-NUMC           PIC 999.
       77 COR-FUNDO         PIC 9 VALUE 1.
       77 COR-FRENTE        PIC 9 VALUE 6.

       77 WS-STATUS         PIC X(40).
       77 WS-MSGERRO        PIC X(80).

       COPY LK_FILE_STATUS.

       COPY screenio.

       COPY LK_CNPJCPF.

       SCREEN SECTION.
       01 SS-CLS.
           05 SS-FILLER.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
               10 LINE WS-NUML COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-CABECALHO.
               10 LINE 01 COLUMN 02 PIC X(80) FROM WS-MODULO
                  HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-STATUS.
               10 LINE WS-NUML COLUMN 2 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.

       01 SS-FUNCAO.
           05 SS-OPCAO.
               10 LINE 08 COLUMN 12 VALUE
                  "ESCOLHA OPCAO INC/ALT/EXC/IMP/FIM :".
               10 LINE 08 COL PLUS 1 USING WS-OPCAO.

       01 SS-TELA-REGISTRO.
           05 SS-CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 12 VALUE "CNPJ...:".
               10 COLUMN PLUS 2 PIC 9(14) USING CLI-CNPJ
                  BLANK WHEN ZEROS.
           05 SS-ALTER.
               07 LINE 12 COLUMN 12 VALUE "CODIGO.....:".
               07 COLUMN PLUS 2 PIC 9(07) USING CLI-COD
                  BLANK WHEN ZEROS.
              07 SS-NOME FOREGROUND-COLOR 2.
                 10 LINE 14 COLUMN 13 VALUE "NOME.....:".
                 10 COLUMN PLUS 2 PIC X(40) USING CLI-NOME.
              07 SS-LAT FOREGROUND-COLOR 2.
                 10 LINE 16 COLUMN 13 VALUE "LATITUDE.:".
                 10 COLUMN PLUS 2 PIC ZZ9,9999999- USING CLI-LATIT.
              07 SS-LONG FOREGROUND-COLOR 2.
                 10 LINE 18 COLUMN 13 VALUE "LATITUDE.:".
                 10 COLUMN PLUS 2 PIC ZZ9,9999999- USING CLI-LONGI.
              07 SS-VEND FOREGROUND-COLOR 2.
                 10 LINE 20 COLUMN 13 VALUE "VENDEDOR.:".
                 10 COLUMN PLUS 2 PIC 9(03) USING CLI-VEND.

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 6 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.


       PROCEDURE DIVISION.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT  WS-NUML FROM LINES
           ACCEPT  WS-NUMC FROM COLUMNS
           PERFORM ABRIR-ARQUIVO
           DISPLAY SS-CLS
           PERFORM UNTIL FINALIZAR
               MOVE "ESCOLHA A OPCAO" TO WS-MENSAG
               DISPLAY                   SS-CLS
               MOVE SPACES            TO WS-OPCAO
               DISPLAY                   SS-TELA-REGISTRO
               ACCEPT                    SS-FUNCAO

               EVALUATE TRUE
                   WHEN INCLUIR
                       PERFORM INCLUI THRU INCLUI-FIM
                   WHEN ALTERAR
                       PERFORM ALTERA THRU ALTERA-FIM
                   WHEN EXCLUIR
                       PERFORM EXCLUI THRU EXCLUI-FIM
                   WHEN IMPORTAR
                       PERFORM IMPORTA THRU IMPORTA-FIM
                   WHEN FINALIZAR
                       MOVE "FIM PROCESSAMENTO" TO WS-MENSAG
                       DISPLAY SS-CABECALHO
                       ACCEPT  SS-ERRO
                       MOVE SPACES              TO WS-MENSAG
                   WHEN OTHER
                       MOVE "OPCAO INVALIDA"    TO WS-MENSAG
                       DISPLAY SS-CABECALHO
                       ACCEPT  SS-ERRO
                       MOVE SPACES              TO WS-MENSAG
                END-EVALUATE
           END-PERFORM

           CLOSE CLIENTES
           GOBACK
           .

      *-----------------------------------------------------------------
       INCLUI.
      *-----------------------------------------------------------------
           MOVE "INCLUSAO" TO WS-MENSAG
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS
      *     DISPLAY SS-CLS
           DISPLAY SS-TELA-REGISTRO
           MOVE SPACES TO CLIENTES-CLI
           SET E-NAO TO TRUE
           .
       DIG-DADOS.
           ACCEPT SS-TELA-REGISTRO  END-ACCEPT
      *     IF FS-CANCELA
      *        PERFORM FINALIZA
      *     END-IF

           IF CLI-COD = ZEROS
              MOVE "CODIGO INVALIDO" TO WS-MSGERRO
              SET E-SIM TO TRUE
              PERFORM MOSTRA-ERRO
           END-IF

           IF CLI-NOME EQUAL SPACES
               MOVE "FAVOR INFORMAR NOME " TO WS-MSGERRO
               SET E-SIM TO TRUE
               PERFORM MOSTRA-ERRO
           END-IF

           IF CLI-LATIT = ZEROS
              MOVE "FAVOR INFORMAR LATITUDE" TO WS-MSGERRO
              SET E-SIM TO TRUE
              PERFORM MOSTRA-ERRO
           END-IF

           IF CLI-LONGI = ZEROS
              MOVE "FAVOR INFORMAR LONGITUDE" TO WS-MSGERRO
              SET E-SIM TO TRUE
              PERFORM MOSTRA-ERRO
           END-IF

           PERFORM ROT-VALIDAR-CNPJ

           IF E-NAO
              WRITE CLIENTES-CLI
                    INVALID KEY
                      MOVE 'INCLUI'                          TO LK-PAR
                      MOVE 'WRITE'                           TO LK-CMD
                      PERFORM ERRO-FILE-STATUS
                      SET E-SIM TO TRUE
                      MOVE ZEROS TO CLI-KEY
              END-WRITE
           END-IF
           .

       INCLUI-FIM.
           EXIT.
      *-----------------------------------------------------------------
       ALTERA.
      *-----------------------------------------------------------------
           MOVE "ALTERACAO" TO WS-MENSAG.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
      *     DISPLAY SS-CLS.
       ALTERA-LOOP.
           MOVE SPACES TO CLIENTES-CLI.
           DISPLAY SS-TELA-REGISTRO.
           PERFORM LE-CLIENTE THRU LE-CLIENTE-FIM.
           IF FS-CANCELA
               GO TO ALTERA-FIM
           END-IF
           IF FS-OK
               DISPLAY SS-TELA-REGISTRO
               ACCEPT SS-ALTER
               IF COB-CRT-STATUS = COB-SCR-ESC
                   GO ALTERA-LOOP
               END-IF
           ELSE
               GO ALTERA-LOOP
            END-IF
            REWRITE CLIENTES-CLI
                INVALID KEY
                      MOVE 'ALTERA'                          TO LK-PAR
                      MOVE 'REWRITE'                         TO LK-CMD
                      PERFORM ERRO-FILE-STATUS
                      SET E-SIM TO TRUE
                NOT INVALID KEY
                    CONTINUE
            END-REWRITE.
            GO ALTERA-LOOP.
       ALTERA-FIM.
            EXIT.

      *-----------------------------------------------------------------
       EXCLUI.
      *-----------------------------------------------------------------
           MOVE "EXCLUSAO" TO WS-MENSAG.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS

           MOVE SPACES TO CLIENTES-CLI
           DISPLAY SS-TELA-REGISTRO
           PERFORM LE-CLIENTE THRU LE-CLIENTE-FIM
           IF FS-CANCELA
               GO EXCLUI-FIM
           END-IF
           IF NOT FS-OK
               GO EXCLUI
           END-IF
           DISPLAY SS-TELA-REGISTRO.
           MOVE "N" TO WS-ERRO.
           MOVE "CONFIRMA A EXCLUSAO DO CLIENTE (S/N)?" TO WS-MSGERRO.
           ACCEPT SS-ERRO.
           IF NOT E-SIM
               GO EXCLUI-FIM
           END-IF
           DELETE CLIENTES
               INVALID KEY
                      MOVE 'EXCLUIR'                         TO LK-PAR
                      MOVE 'DELETE'                          TO LK-CMD
                      PERFORM ERRO-FILE-STATUS
                      SET E-SIM TO TRUE
           END-DELETE.
       EXCLUI-FIM.
           EXIT.

      *-----------------------------------------------------------------
       LE-CLIENTE.
      *-----------------------------------------------------------------
           ACCEPT SS-CHAVE.
           IF NOT COB-CRT-STATUS = COB-SCR-ESC
               READ CLIENTES
                   INVALID KEY
                       MOVE "CLIENTE NAO ENCONTRADO" TO WS-MSGERRO
                       PERFORM MOSTRA-ERRO
               END-READ
           ELSE
               MOVE 99 to CLI-STAT
           END-IF.
       LE-CLIENTE-FIM.
           EXIT.
      *-----------------------------------------------------------------
       ROT-VALIDAR-CNPJ.
      *-----------------------------------------------------------------

           INITIALIZE PARCPF
           MOVE CLI-CNPJ TO LK-CPF
           MOVE 'J'      TO LK-TPC
           CALL "PROG07" USING PARCPF END-CALL

           IF LK-RCC = 'N'
              MOVE "CNPJ INVALIDO , TENTE NOVAMENTE" TO WS-MSGERRO
              PERFORM MOSTRA-ERRO
           END-IF
           .
       ROT-VALIDAR-CNPJ-FIM.
           EXIT.

      *-----------------------------------------------------------------
       IMPORTA.
      *-----------------------------------------------------------------

           MOVE 'C'      TO LK-TPC
           CALL "PROG03" USING LK-TPC END-CALL
           .
       IMPORTA-FIM.
           EXIT.

      *-----------------------------------------------------------------
       ABRIR-ARQUIVO.
      *-----------------------------------------------------------------
           OPEN I-O CLIENTES
           IF FS-NAO-EXISTE THEN
               OPEN OUTPUT CLIENTES
               CLOSE CLIENTES
               OPEN I-O CLIENTES
           END-IF
           .
       ABRIR-ARQUIVO-FIM.
           EXIT.

      *-----------------------------------------------------------------
       MOSTRA-ERRO.
      *-----------------------------------------------------------------
           DISPLAY SS-ERRO
           ACCEPT  SS-ERRO
           DISPLAY SS-STATUS
           .
       MOSTRA-ERRO-FIM.
           EXIT.

      *-----------------------------------------------------------------
       ERRO-FILE-STATUS.
      *-----------------------------------------------------------------
           MOVE 'PROG01'                    TO LK-PRG
           MOVE 'CLIENTES'                  TO LK-ARQ
           MOVE CLI-STAT                    TO LK-STA
           CALL "PROG10" USING LK-FILE-STATUS END-CALL
           .
       FIM-ERRO.
           EXIT.
