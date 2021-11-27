       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP011.
      *AUTOR."GUSTAVO GONZAGA DE FARIAS".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQDEP ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODIGO
                      ALTERNATE RECORD KEY IS DENOMINACAO
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.

       DATA DIVISION.
       FILE SECTION.
       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".
       01 REGDEP.
          03 CODIGO                   PIC 9(03).
          03 DENOMINACAO              PIC X(25).
          03 DEPSTATUS                PIC X(01).
          03 TPHIERARC                PIC 9(02).

       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 CONLIN       PIC 9(03) VALUE 001.
       01 CODENTR      PIC 9(03) VALUE ZEROS.
       01 DENOENTR     PIC X(22) VALUE SPACES.
       01 IND             PIC 9(02) VALUE ZEROS.
       01 TXTTHIERARC  PIC X(30) VALUE SPACES. 

       01 TABHIERARC.
           03 TBHIERARC      PIC X(45) OCCURS 9 TIMES.

       01 TABSTATUS.
           03 FILLER PIC X(15) VALUE "ATIVA".
           03 FILLER PIC X(15) VALUE "SUSPENSA".
           03 FILLER PIC X(15) VALUE "DESATIVADA".

       01 TABSTATUSX      REDEFINES TABSTATUS.
           03 TBSTATUS    PIC X(15) OCCURS 3 TIMES.

       01 TXTSTATUS.
           03 TXTSTATUS2 PIC X(14) VALUE SPACES.

       01 DET2.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETCODIGO       PIC 999.
          03 FILLER          PIC X(06) VALUE SPACES.
          03 DETDENOMINACAO  PIC X(22) VALUE SPACES.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETHIERARC      PIC X(30) VALUE SPACES.
          03 FILLER          PIC X(02) VALUE SPACES.
          03 DETDEPSTATUS    PIC X(20) VALUE SPACES.


       SCREEN SECTION.

       01  TELACONSDEP.
           05 BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "-----CONSULTA DE DEPARTAMENTO-----".
           05  LINE 04  COLUMN 01 
               VALUE  "  COD.DEP |    DENOMINACAO      |".
           05  LINE 04  COLUMN 41 
               VALUE  "  NIVEL HIERARC.     |     STATUS".
           05  LINE 22  COLUMN 01 
               VALUE  "  MENSAGEM:".

           05  TCODENTR
               LINE 05  COLUMN 03  PIC 9(03)
               USING  CODENTR.
           05  TDENOENTR
               LINE 05  COLUMN 20  PIC X(22)
               USING  DENOENTR.
        
       PROCEDURE DIVISION.
       INICIO.
           MOVE "01-CONSELHO CONSULTIVO "     TO TBHIERARC(1)
           MOVE "02-CONSELHO FISCAL"          TO TBHIERARC(2)
           MOVE "03-PRESIDENCIA"              TO TBHIERARC(3)
           MOVE "04-DIRETORIA ADMINISTRATIVA" TO TBHIERARC(4)
           MOVE "05-DIRETORIA INDUSTRIAL"     TO TBHIERARC(5)
           MOVE "06-DIRETORIA COMERCIAL"      TO TBHIERARC(6)
           MOVE "07-DIRETORIA MARKETING"      TO TBHIERARC(7)
           MOVE "08-DIRETORIA JURIDICA"       TO TBHIERARC(8)
           MOVE "09-CONSULTORIA"              TO TBHIERARC(9).

       INC-OP1.
           OPEN INPUT ARQDEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO ARQDEP NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQDEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.

       INC-001.
           MOVE SPACES TO DENOENTR 
           MOVE ZEROS TO CODENTR 

           DISPLAY  TELACONSDEP.

       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/DENOMINACAO."
           ACCEPT TCODENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQDEP
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CODENTR TO CODIGO
           START ARQDEP KEY IS NOT LESS CODIGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/CODIGO DEP "
           ACCEPT TDENOENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQDEP
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001B.
           MOVE DENOENTR TO DENOMINACAO
           START ARQDEP KEY IS NOT LESS DENOMINACAO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ ARQDEP NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQDEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       ROT-STATUS.
       
           IF DEPSTATUS = "A"
               MOVE "ATIVA" TO TXTSTATUS
           ELSE
               IF DEPSTATUS = "S"
                  MOVE "SUSPENSA" TO TXTSTATUS
               ELSE
                   IF DEPSTATUS = "D"
                      MOVE "DESATIVADA" TO TXTSTATUS.
                 
       ROT-MONTAR.
           MOVE CODIGO      TO DETCODIGO
           MOVE DENOMINACAO TO DETDENOMINACAO
           MOVE TBHIERARC(TPHIERARC) TO TXTTHIERARC
           MOVE TXTTHIERARC   TO DETHIERARC 
           MOVE TXTSTATUS2    TO DETDEPSTATUS 
           
           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 01) DET2
           ADD 1 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.

       ROT-SOL.
           DISPLAY (22, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELACONSDEP
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.

       NOV-SOL.
           DISPLAY (22, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (22, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.

       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQDEP.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.

       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (22, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (22, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.