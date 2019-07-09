       01  PARCPF.
           03 LK-CPF       PIC 9(15).
           03 LK-RD1       REDEFINES  LK-CPF.
              05 LK-CDC    PIC 9(13).
              05 LK-DG1    PIC 9(01).
              05 LK-DG2    PIC 9(01).
           03 LK-RD2       REDEFINES  LK-CPF.
              05 LK-CR1    PIC 9(01)  OCCURS  13.
              05 LK-DIG    PIC 9(02).
           03 LK-RD3       REDEFINES  LK-CPF.
              05 LK-CR2    PIC 9(01)  OCCURS  14.
              05 FILLER    PIC 9(01).
           03 LK-RD4       REDEFINES  LK-CPF.
              05 LK-CG1    PIC 9(13).
              05 LK-CG2    PIC 9(02).
           03 LK-RD5       REDEFINES  LK-CPF.
              05 LK-CP1    PIC 9(04).
              05 LK-CP2    PIC 9(09).
              05 FILLER    PIC 9(02).
           03 LK-TPC       PIC X(01).
           03 LK-RCC       PIC X(01).
           03 LK-ECC       PIC X(18).
