      ******************************************************************
      * Autor.....: Alexandre Trevisani (PROVA COBOL)
      * Data......: Julho/02019
      * Programa..: PROG10 - Tratamento erros de FILE-STATUS
      * Parametros Esperados/Devolvidos
      * LK-PRG PIC X(08). (E) Nome Programa
      * LK-ARQ PIC X(08). (E) Nome do Arquivo
      * LK-CMD PIC X(08). (E) Nome do Comando
      * LK-PAR PIC X(30). (E) Nome Paragrafo
      * LK-STA PIC X(02). (E) File-Status
      * LK-RET PIC X(02). (D) Cod. Retorno :
      *                       '00' = Processo OK
      *                       '10' = Erro FILE-STATUS não Informado
      *                       '20' = Erro nome Programa
      *                       '30' = Erro nome Comando
      *                       '40' = Erro nome Paragrafo
      *                       '50' = Erro no FILE-STATUS Invalido
      *                       '60' = Erro não Especificado
      * LK-MSG PIC X(50). (D) Mensagem de Erro
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PROG10.
       ENVIRONMENT         DIVISION.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.

       77 WS-SPACES        PIC X(80) VALUE '.'.
       01 LLCC.
          05 WS-LIN        PIC 9(02) VALUE 04.
          05 WS-COL        PIC 9(02) VALUE 01.
      *
       LINKAGE             SECTION.

       COPY LK_FILE_STATUS.

      *----------------------------------------------------------------
       PROCEDURE   DIVISION   USING    LK-FILE-STATUS.
      *----------------------------------------------------------------

           MOVE '00'                                         TO LK-RET
           MOVE 'PROCESSAMENTO NORMAL'                       TO LK-MSG

           IF LK-STA EQUAL SPACES OR LK-STA NOT NUMERIC
              MOVE 'FILE-STATUS NÃO INFORMADO              ' TO LK-MSG
              MOVE '10'                                      TO LK-RET
            END-IF

           IF LK-PRG EQUAL SPACES
              MOVE 'NOME PROGRAMA NÃO INFORMADO            ' TO LK-MSG
              MOVE '20'                                      TO LK-RET
            END-IF

           IF LK-CMD EQUAL SPACES      AND
              LK-CMD NOT = 'OPEN'      AND
              LK-CMD NOT = 'CLOSE'     AND
              LK-CMD NOT = 'READ'      AND
              LK-CMD NOT = 'WRITE'     AND
              LK-CMD NOT = 'START'     AND
              LK-CMD NOT = 'REWRITE'   AND
              LK-CMD NOT = 'DELETE'
              MOVE 'COMANDO            ' TO LK-MSG
              MOVE '20'                                      TO LK-RET
            END-IF

           IF LK-PAR EQUAL SPACES
              MOVE 'NOME PARAGRAFO NÃO INFORMADO           ' TO LK-MSG
              MOVE '40'                                      TO LK-RET
           END-IF

           PERFORM VERIFICA-ERRO
           PERFORM MOSTRA-RESUMO
           GOBACK
           .
      *----------------------------------------------------------------
       VERIFICA-ERRO.
      *----------------------------------------------------------------
           EVALUATE LK-STA
               WHEN 00 MOVE 'SUCCESS                       ' TO LK-MSG
               WHEN 02 MOVE 'SUCCESS DUPLICATE             ' TO LK-MSG
               WHEN 04 MOVE 'SUCCESS INCOMPLETE            ' TO LK-MSG
               WHEN 05 MOVE 'SUCCESS OPTIONAL              ' TO LK-MSG
               WHEN 07 MOVE 'SUCCESS NO UNIT               ' TO LK-MSG
               WHEN 10 MOVE 'END OF FILE                   ' TO LK-MSG
               WHEN 14 MOVE 'OUT OF KEY RANGE              ' TO LK-MSG
               WHEN 21 MOVE 'KEY INVALID                   ' TO LK-MSG
               WHEN 22 MOVE 'KEY EXISTS                    ' TO LK-MSG
               WHEN 23 MOVE 'KEY NOT EXISTS                ' TO LK-MSG
               WHEN 30 MOVE 'PERMANENT ERROR               ' TO LK-MSG
               WHEN 31 MOVE 'INCONSISTENT FILENAME         ' TO LK-MSG
               WHEN 34 MOVE 'BOUNDARY VIOLATION            ' TO LK-MSG
               WHEN 35 MOVE 'FILE NOT FOUND                ' TO LK-MSG
               WHEN 37 MOVE 'PERMISSION DENIED             ' TO LK-MSG
               WHEN 38 MOVE 'CLOSED WITH LOCK              ' TO LK-MSG
               WHEN 39 MOVE 'CONFLICT ATTRIBUTE            ' TO LK-MSG
               WHEN 41 MOVE 'ALREADY OPEN                  ' TO LK-MSG
               WHEN 42 MOVE 'NOT OPEN                      ' TO LK-MSG
               WHEN 43 MOVE 'READ NOT DONE                 ' TO LK-MSG
               WHEN 44 MOVE 'RECORD OVERFLOW               ' TO LK-MSG
               WHEN 46 MOVE 'READ ERROR                    ' TO LK-MSG
               WHEN 47 MOVE 'INPUT DENIED                  ' TO LK-MSG
               WHEN 48 MOVE 'OUTPUT DENIED                 ' TO LK-MSG
               WHEN 49 MOVE 'I/O DENIED                    ' TO LK-MSG
               WHEN 51 MOVE 'RECORD LOCKED                 ' TO LK-MSG
               WHEN 52 MOVE 'END-OF-PAGE                   ' TO LK-MSG
               WHEN 57 MOVE 'I/O LINAGE                    ' TO LK-MSG
               WHEN 61 MOVE 'FILE SHARING FAILURE          ' TO LK-MSG
               WHEN 91 MOVE 'FILE NOT AVAILABLE            ' TO LK-MSG
               WHEN OTHER
                       MOVE '50'                             TO LK-RET
                       MOVE 'FILE-STATUS INVALIDO.         ' TO LK-MSG
           END-EVALUATE
           .
       VERIFICA-ERROS-FIM.
           EXIT.

      *----------------------------------------------------------------
       MOSTRA-RESUMO.
      *----------------------------------------------------------------

           DISPLAY '.                                          ' AT 0812
           DISPLAY '                                           ' AT 0840
           DISPLAY '.                                          ' AT 1012
           DISPLAY '                                           ' AT 1040
           DISPLAY '.                                          ' AT 1212
           DISPLAY '                                           ' AT 1240
           DISPLAY '.                                          ' AT 1412
           DISPLAY '                                           ' AT 1440
           DISPLAY '.                                          ' AT 1612
           DISPLAY '                                           ' AT 1640
           DISPLAY '.                                          ' AT 1812
           DISPLAY '                                           ' AT 1840
           DISPLAY '.                                          ' AT 2012
           DISPLAY '                                           ' AT 2040
      *
           DISPLAY '*******************************************' AT 0820
           DISPLAY '* ATENCAO, OCORREU ERRO SISTEMA, AVISE TI *' AT 0920
           DISPLAY '*******************************************' AT 1020
           DISPLAY '* PROGRAMA........: '                        AT 1120
           DISPLAY LK-PRG                                        AT 1140
           DISPLAY '* PARAGRAFO.......: '                        AT 1220
           DISPLAY LK-PAR                                        AT 1240
           DISPLAY '* ARQUIVO.........: '                        AT 1320
           DISPLAY LK-ARQ                                        AT 1340
           DISPLAY '* COMANDO.........: '                        AT 1420
           DISPLAY LK-CMD                                        AT 1440
           DISPLAY '* FILE-STATAUS....: '                        AT 1520
           DISPLAY LK-STA                                        AT 1540
           DISPLAY '*------------------------------------------' AT 1620
           DISPLAY '* MENSAGEM RETORNO: '                        AT 1720
           DISPLAY LK-MSG                                        AT 1740
           DISPLAY '*******************************************' AT 1820
           DISPLAY 'TECLE "ENTER" P RETORNAR'                    AT 2020
           STOP ' '
           .
       MOSTRA-RESUMO-FIM.
           GOBACK.
