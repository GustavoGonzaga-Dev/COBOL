       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR0B.
      *AUTOR. ANGELO LOTIERZO FILHO.
      *******************************************
      * MANUTENCAO DO PROGRAMA 0  *
      *******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 APELIDO     PIC X(12) VALUE "ANGELO".
       01 NOME          PIC X(30) VALUE SPACES.
       01 IDADE          PIC 9(03) VALUE ZEROS.
       01 PESO           PIC 999V9 VALUE ZEROS.
       01 MASCPESO PIC ZZ9,9.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           DISPLAY (02, 25) "CADASTRO DE AMIGOS".
           DISPLAY (06, 10) "APELIDO  :".
           DISPLAY (08, 10) "NOME   :"
           DISPLAY (10, 10) "IDADE   :".
           DISPLAY (12, 10) "PESO    :".
       ROT-APELIDO.
           ACCEPT (06, 20) APELIDO.
           IF APELIDO = SPACES
               GO TO ROT-APELIDO.
           ACCEPT (08, 20) NOME
           ACCEPT (10, 20) IDADE.
           ACCEPT (12, 20) PESO.
           DISPLAY (12, 40) PESO.
           MOVE PESO TO MASCPESO
           DISPLAY (12, 60) MASCPESO
.          STOP RUN.
