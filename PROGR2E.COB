       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR2D.
      *AUTHOR. ANGELO LOTIERZO FILHO.
      **************************************
      * MANUTENCAO DO CADASTRO DE AMIGOS   *
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
       01 APELIDO       PIC X(12) VALUE SPACES.
       01 NOME          PIC X(30) VALUE SPACES.
       01 EMAIL         PIC X(30) VALUE SPACES.
       01 SEXO          PIC X(01) VALUE SPACES.
       01 GENERO        PIC X(01) VALUE SPACES.
       01 TPAMIGO       PIC 9(01) VALUE ZEROS.
       01 TXTTPAMIGO    PIC X(10) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.
       01 TEXSEXO       PIC X(12) VALUE SPACES.

       01 TABAMIGO.
          03 TBAMIGO    PIC X(10) OCCURS 9 TIMES.

       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "THETEROSEXUAL".
          03 FILLER     PIC X(15) VALUE "HHOMESEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISSESUXUAL".
          03 FILLER     PIC X(15) VALUE "PPANSEXUAL".
          03 FILLER     PIC X(15) VALUE "AA           ".
          03 FILLER     PIC X(15) VALUE "BB           ".
          03 FILLER     PIC X(15) VALUE "CC           ".
          03 FILLER     PIC X(15) VALUE "DD            ".
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 9 TIMES.
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES. 

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            ÚÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 01  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 02  COLUMN 01 
               VALUE  "                            ³ CADASTRO D".
           05  LINE 02  COLUMN 41 
               VALUE  "E AMIGOS  ³".
           05  LINE 03  COLUMN 01 
               VALUE  "                            ÀÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 03  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÙ".
           05  LINE 04  COLUMN 01 
               VALUE  "  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 05  COLUMN 01 
               VALUE  "  ³ APELIDO :               ³      NOME".
           05  LINE 05  COLUMN 41 
               VALUE  ":".
           05  LINE 06  COLUMN 01 
               VALUE  "  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
           05  LINE 08  COLUMN 01 
               VALUE  "    EMAIL  :".
           05  LINE 11  COLUMN 01 
               VALUE  "    SEXO   :                       GENER".
           05  LINE 11  COLUMN 41 
               VALUE  "O :".
           05  LINE 14  COLUMN 01 
               VALUE  "    TIPO AMIGO :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TAPELIDO
               LINE 05  COLUMN 15  PIC X(12)
               USING  APELIDO
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 43  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TEMAIL
               LINE 08  COLUMN 14  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TSEXO
               LINE 11  COLUMN 14  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTEXSEXO
               LINE 11  COLUMN 16  PIC X(15)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 11  COLUMN 45  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO2
               LINE 11  COLUMN 47  PIC X(14)
               USING  TXTGENERO2
               HIGHLIGHT.
           05  TTPAMIGO
               LINE 14  COLUMN 18  PIC 9
               USING  TPAMIGO
               HIGHLIGHT.
           05  TTXTTPAMIGO
               LINE 14  COLUMN 20  PIC X(10)
               USING  TXTTPAMIGO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "FACULDADE" TO TBAMIGO(1)
           MOVE "IGREJA   " TO TBAMIGO(2)
           MOVE "BAIRRO"    TO TBAMIGO(3)
           MOVE "COLEGIO"   TO TBAMIGO(4)
           MOVE "BALADA"    TO TBAMIGO(5)
           MOVE "FUTEBOL"   TO TBAMIGO(6)
           MOVE "VIAGEM"    TO TBAMIGO(7)
           MOVE "EX"        TO TBAMIGO(8)
           MOVE "NAMOR.EX"  TO TBAMIGO(9).
      *
       R1.
           DISPLAY TELA2.
           ACCEPT TAPELIDO
           ACCEPT TNOME
           ACCEPT TEMAIL.
       R5.
           ACCEPT TSEXO
           IF SEXO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO TEXSEXO
           ELSE
              IF SEXO = "F"
                 MOVE "FEMENINO" TO TEXSEXO
              ELSE
                 DISPLAY (23, 14) "DIGITE M=MASCULINO   F=FEMENINO  "
                 STOP "  DIGITE ENTER PARA CONTINUAR"
                 GO TO R5.
           DISPLAY TTEXSEXO.
       R6.
           MOVE 1 TO IND
           ACCEPT TGENERO.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 DISPLAY (23, 14) "TIPO DE GENERO INCORRETO"
                 STOP "DIGITE ENTER PARA CONTINUAR"
                 GO TO R6
           ELSE
               DISPLAY TTXTGENERO2.

       R7.
           ACCEPT TTPAMIGO.
           IF TPAMIGO = 0 
                 DISPLAY (23, 14) "DIGITE APENAS DE 1 A 9 "
                 STOP "  DIGITE ENTER PARA CONTINUAR"
                 GO TO R7.
           MOVE TBAMIGO(TPAMIGO) TO TXTTPAMIGO
           DISPLAY TTXTTPAMIGO.
           STOP RUN.