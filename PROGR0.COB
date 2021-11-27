       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR0.
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
       01 APELIDO     PIC X(12) VALUE "Japa".
       01 NOME          PIC X(30) VALUE SPACES.
       01 IDADE          PIC 9(03) VALUE ZEROS.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           MOVE "Diego Toshio Takeda Koga" TO NOME
		   DISPLAY (08, 20) APELIDO
		   DISPLAY (10, 20) NOME
		   ACCEPT (12, 20) IDADE
		   DISPLAY(12, 20) "Idade: " IDADE
		   
.          STOP RUN.
