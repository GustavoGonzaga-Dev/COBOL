       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR1.
       AUTHOR. ANGELO LOTIERZO FILHO.
      **************************************
      * MANUTENCAO DO XXXXXXXX  *
      **************************************
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
       01 NOME          PIC X(30) VALUE SPACES.
       01 IDADE          PIC 9(02) VALUE ZEROS.
       01 EMAIL.
            02 EMAIL1   PIC X(10) VALUE SPACES.
            02 EMAIL2   PIC X(20) VALUE SPACES.
       01 DATANASC.
            03 DIA          PIC 99.
            03 MES        PIC 9(02).
            03 ANO.
                05 MIL      PIC 9.
                05 SEC    PIC 9.
                05 DEC    PIC 99.
        01 MASCARA PIC 99.99.9999.
        01 MASCAR1 PIC *9.99.9999.
        01 MASCAR2 PIC Z9.99.9999.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           MOVE "ANGELO LOTIERZO FILHO" TO NOME
           MOVE 64 TO IDADE.
          
       LABEL1.
           DISPLAY (13, 10) "NOME :  " NOME
           DISPLAY (15, 10) "IDADE : " IDADE
           DISPLAY (17, 10) "EMAIL : " 
           ACCEPT (17, 18) EMAIL.
           DISPLAY (19, 10)  EMAIL1 "        " EMAIL2
           DISPLAY (20, 10) "DATA : " 
           ACCEPT (20, 18) DATANASC.
           DISPLAY (21, 18) DIA "/" MES "/" ANO
 
           MOVE DATANASC TO MASCAR1 MASCARA MASCAR2
           DISPLAY (22, 18) MASCARA   "       " MASCAR1 "      " MASCAR2.
.          STOP RUN.
