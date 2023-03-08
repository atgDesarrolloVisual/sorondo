       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMIFAC.                                              JP260308
       AUTHOR.     LAURA.                                               LG280308
       DATE-WRITTEN. 29/08/08.                                          LG290808

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. RM-COBOL.
       OBJECT-COMPUTER. RM-COBOL.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           COPY "IMPREACT.SD".                                          
                                                                        MD040106
           SELECT IMPRESOR  ASSIGN TO PRINT  SYN-LO
                            FILE STATUS  IS ST-XX.

       DATA DIVISION.
       
       FILE SECTION.
       
       COPY "IMPREACT.FDS".                                             
           
       FD IMPRESOR 
           LABEL RECORDS ARE OMITTED.  
       01 LINEA                PIC X(136).                              LG130608

       WORKING-STORAGE SECTION.
       copy "leo.cpy"
       copy "leo MODIFICADO.cpy"

       COPY "COLOR.VAR".                                                MD040106
       COPY "FECHA.VAR".
       COPY "PARVENT.VAR".
       COPY "ST-ACE.VAR".
       COPY "EMIFAC.VAR".                                               MD040106
       COPY "SETEOIMP.WRK".                                             MD040106

       01  DECIMALES           PIC 9.
       01  TOTAL-CANT          PIC S9(009)V9(005) COMP-3.
       
       01  MD                  PIC 99.
       01  MD2                 PIC 99.
       
       01  AUX                 PIC 9.
                                                                        LG270308
      ***************** VARIABLE PARA SIMULAR VIAS************          LG270308
       01  AUX-VIA             PIC 9 VALUE 1.                           LG270308
       01  CANT-VIA            PIC 9 VALUE 3.                           LG270308
      ********************************************************          LG270308
                                                                        LG270308
       01  SUBTOTAL            PIC S9(011)V99     COMP-3.
       01  TOTAL               PIC S9(011)V99     COMP-3.
       
       01  TEXTO               PIC X(60) VALUE 
           "VALORES EN MONEDA NACIONAL SOLO A EFECTOS CONTABLES T.C.".

       01  PRIM-VEZ            PIC 9         VALUE 0.
       01  YY                  PIC S9(005)   COMP-1.                    MD040106
       01  I                   PIC S9(005)   COMP-1.
       01  JJ                  PIC S9(005)   COMP-1.

       01 LINEA-IMPRESA        PIC X(160).                              LG260308
    
      * 1a linea
      
       01  LINE-RUC-FA       REDEFINES LINEA-IMPRESA.                   LG290808
           02  FILLER          PIC X(20).                               LG201008
           02  M-RUC-FA        PIC XXBXXXXXXBXXXX.                      LG290808
       01  LINE-CF           REDEFINES LINEA-IMPRESA.                   LG290808
           02  FILLER          PIC X(46).                               LG201008
           02  M-CF            PIC X.                                   LG290808
       01 LINEA-FECHA-FA     REDEFINES LINEA-IMPRESA.                   
           03 FILLER           PIC X(60).                               LG201008
           03  M-DDD           PIC 9(02).                               LG160908
           03  FILLER          PIC X(04).                               LG160908
           03  M-MMM           PIC 9(02).                               LG160908
           03  FILLER          PIC X(03).                               LG160908
           03  M-AAA           PIC 9999.                                LG160908
       01 LINEA-NOMCLI-FA    REDEFINES LINEA-IMPRESA.                   
           03 FILLER           PIC X(015).                              LG160908
           03 M-NOMCLI-FA      PIC X(030).                              LG280308
       01 LINEA-DIREC-FA     REDEFINES LINEA-IMPRESA.                   
           03 FILLER           PIC X(015).                              LG160908
           03 M-DIREC-FA       PIC X(030).                              
           
      * LINEAS DE DETALLE
                                                                        
       01 LINEA-DESC-FA      REDEFINES LINEA-IMPRESA.                   
           03 FILLER           PIC X(010).                              LG160908
           03 M-DESC-FA        PIC X(034).                              
                                                                        
           
       01 LINEA-NETO-SIV     REDEFINES LINEA-IMPRESA.                   
           02  FILLER          PIC X(63).                               LG201008
           02  M-NETO-SIV      PIC -----.--9,99  BLANK WHEN ZERO.       LG040908
                                                                        
                                                                        LG160908
       01 LINEA-SUBTOTAL     REDEFINES LINEA-IMPRESA.                   LG010908
           02 FILLER           PIC X(063).                              LG201008
           02 M-SUBTOTAL       PIC -----.--9,99.                        
                                                                        LG270308
       01  LINE-PIVA         REDEFINES LINEA-IMPRESA.                   LG160908
           03 FILLER          PIC X(55).                                LG201008
           03 M-PIVA          PIC Z9  BLANK WHEN ZERO.                  LG160908
           03 M-POR           PIC X.                                    LG160908
                                                                        LG160908
       01  LINE-MONE-FA       REDEFINES LINEA-IMPRESA.                  LG160908
           03 FILLER          PIC X(060).                               LG201008
           03 M-MONE-FA       PIC XXX.                                  LG160908
                                                                        LG160908
       01  LINE-TOTIVA     REDEFINES LINEA-IMPRESA.                     LG160908
           03 FILLER          PIC X(063).                               LG201008
           03 M-TOTIVA        PIC -----.--9,99.                         LG010908
                                                                        LG270308
       01  LINE-TOTAL       REDEFINES LINEA-IMPRESA.                    
           02  FILLER          PIC X(063).                              LG201008
           02  M-TOTAL         PIC -----.--9,99  BLANK WHEN ZERO.       LG010908
                                                                        LG300408
       01  TABLA-SYN.                                                   LG010908
           02  CANT-SYN        PIC 99 VALUE 01.                         LG270308
           02  SYN-IMPRE       VALUE "IMPRESORA".
               04  IMPRESORA   PIC X.
               04  FILLER      PIC X(029).

       01  SYN-ERROR           PIC 9  VALUE 0.
       01  EX-PROG             PIC 9(002) VALUE 99.
       01  ST-XAUX             PIC X(004).
       
       LINKAGE SECTION.
       
       01  FUNCION             PIC  9.

       COPY "PARAMFAC.REG".
       COPY "MONEDAS.REG".
       COPY "PARAMDOC.CPY".                                             MD040106
       COPY "FACTURAS.VAR".
       COPY "CLIENTES.REG".
       COPY "FORMAPAG.REG".
       COPY "PARSYS.REG".
       COPY "VIRTUAL.REG".
       COPY "SELE-CA.VAR".
       COPY "VENDEDOR.REG".                                             MD110808
       COPY "LOCAL.REG".                                                MD110808
       COPY "IMPRES.REG".                                               
       COPY "NUMERADO.REG".                                             
       
       PROCEDURE DIVISION USING FUNCION
                                REG-PARAMFAC
                                REG-MONEDAS
                                REG-PDOCCPY                             MD040106
                                REG-FACTURAS
                                REG-CLIENTES
                                REG-FORMAPAG
                                REG-VENDEDOR
                                REG-LOCALES
                                REG-PARSYS
                                REG-VIRTUAL
                                ERROR-CA
                                REG-IMPRES                              MD040106
                                REG-NUMERADO.                           MD040106

       DECLARATIVES.                                                    
       ERROR-IMPRESOR  SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON IMPRESOR.
       END DECLARATIVES.

       APERTURA SECTION.       
       ABRE-ARCHIVOS.                                                   
      * Proceso de facturacion 
           MOVE "00"      TO ST-XX-CA                                   MD040106
           MOVE DIRE-IMPR TO SYN-LO                                     MD040106
                                                                        MD040106
           IF PRIM-VEZ     = 0  AND                                     MD040106
              FUNCION  NOT = 2  AND                                     MD040106
              CIMP-PDC NOT = 0                                          MD040106
               PERFORM PRIMER-LLAMADO THRU EXIT-P-LLAMA                 MD040106
           END-IF                                                       MD040106
                                                                        MD040106
           IF FUNCION = 1 AND CIMP-PDC NOT = 0                          MD040106
               PERFORM TESTEO-IMP                                       MD040106
               PERFORM PONGO-VENT                                       MD040106
               PERFORM IMPRE-LINEA-1 THRU EXIT-IMPRE                    
                           VARYING YY FROM 1 BY 1                       MD040106
                           UNTIL YY > CIMP-PDC                          JP260308
               PERFORM SACO-VENT                                        MD040106
               CLOSE IMPRESOR                                           MD040106
           END-IF.                                                      LG270308
                                                                        MD040106
       EXIT-PGM.                                                        
           EXIT PROGRAM.                                                
           
       SETEO-IMPRE.
      * 50 LINEAS
           MOVE SPACES TO  LINEA                                        LG270308
           IF TIPO-IMPR = 1                                             MD040106
               SET 6-LPI            TO TRUE                             MD040106
               MOVE RM-LINE-SPACING TO LINEA                            MD040106
           ELSE                                                         
               MOVE H"1B4324" TO LINEA                                  JP030608
           END-IF                                                       MD040106
           WRITE LINEA AFTER 0 LINES
           MOVE SPACES TO LINEA                                         LG270308
           
           PERFORM SETEO-12.                                            LG160908
                                                                        LG280308
           
           
       COPY "FECHA.RUT".    
       COPY "EMIFAC.RUT".                                               MD040106
       COPY "SETEOIMP.RUT".
           
      ******************************************************************
      *                                                                 
      *        RUTINAS DE ENTRADA/SALIDA Y AUDITORIA DE ERRORES         
      *                                                                 
                                                                        
       PRIMER-LLAMADO.
           CALL "ASIGNA" USING TABLA-SYN SYN-ERROR.
           IF SYN-ERROR NOT = 0 
               MOVE "ASI"       TO MOTIVO-CA
               MOVE "EMIFAC"    TO NOMBRE-AR-CA                         JP260308
               MOVE ST-XX       TO ST-XX-CA
               GO TO EXIT-PGM
           END-IF.
           
           MOVE 1 TO PRIM-VEZ.
           PERFORM TESTEO-IMP THRU EXIT-TIMP.                           MD040106
       EXIT-P-LLAMA.
           EXIT.  

       IMPRI-PRUEBA.
           MOVE SPACES TO LINEA-IMPRESA                                 LG270308
           PERFORM IMPRIME-LINEA 13 TIMES.                              LG201008
           MOVE XX       TO M-RUC-FA                                    LG010908
                            M-CF                                        LG010908
           MOVE NUEVES        TO M-DDD                                  LG160908
           MOVE NUEVES        TO M-MMM                                  LG160908
           MOVE NUEVES        TO M-AAA                                  LG160908
           PERFORM IMPRIME-LINEA 4 TIMES.                               LG201008
           MOVE XX       TO M-NOMCLI-FA                                 LG160908
           PERFORM IMPRIME-LINEA 2 TIMES                                LG201008
           MOVE XX       TO M-DIREC-FA                                  
           PERFORM IMPRIME-LINEA 5 TIMES.                               LG201008
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 16                   LG201008
               MOVE XX     TO M-DESC-FA                                 
               MOVE NUEVES TO M-NETO-SIV                                
               PERFORM IMPRIME-LINEA
           END-PERFORM
           
           MOVE XX         TO M-MONE-FA                                 
           MOVE NUEVES     TO M-SUBTOTAL                                LG010908
           PERFORM IMPRIME-LINEA 2 TIMES.                               
           MOVE NUEVES     TO M-PIVA                                    LG160908
           MOVE "%"        TO M-POR                                     LG160908
           MOVE XX         TO M-MONE-FA                                 LG160908
           MOVE NUEVES     TO M-TOTIVA                                  LG160908
           PERFORM IMPRIME-LINEA 2 TIMES.                               
           MOVE XX         TO M-MONE-FA                                 LG160908
           MOVE NUEVES     TO M-TOTAL                                   LG010908
           PERFORM IMPRIME-LINEA                                        LG160908
           WRITE LINEA FROM SPACES BEFORE PAGE.                         LG280308
                                                                        LG280308
       EXIT-PRU.                                                        LG280308
           EXIT.

       IMPRE-LINEA-1.                                                   MD040106
           PERFORM SETEO-IMPRE.                                         LG270308
                                                                        LG280308
       LOOP-IMPRIME-LINEA-1.                                            LG280308
      *     IF YY NOT = 1                                               JP030608
      *         MOVE SPACES TO LINEA-IMPRESA                            JP030608
      *         PERFORM IMPRIME-LINEA 3 TIMES                           JP030608
      *     END-IF.                                                     JP030608
           MOVE SPACES TO LINEA-IMPRESA                                 
           PERFORM IMPRIME-LINEA 13 TIMES.                              LG201008
           IF RUC-FA NOT = SPACES                                       LG010908
               MOVE RUC-FA    TO M-RUC-FA                               LG010908
           ELSE                                                         LG010908
               MOVE ALL "-"   TO M-RUC-FA                               LG010908
               MOVE "X"       TO M-CF                                   LG010908
           END-IF                                                       LG010908
           MOVE FECHA-FA      TO FECHA-AMD                              LG160908
           PERFORM AMD-TO-DMA                                           LG160908
           MOVE DDD        TO M-DDD                                     LG160908
           MOVE MMM        TO M-MMM                                     LG160908
           MOVE AAA        TO M-AAA                                     LG160908
           PERFORM IMPRIME-LINEA 4 TIMES.                               LG201008
           IF RSOC-FA NOT = SPACES                                      LG010908
               MOVE RSOC-FA   TO M-NOMCLI-FA                            LG010908
           ELSE                                                         LG010908
               MOVE NOMCLI-FA TO M-NOMCLI-FA                            LG010908
           END-IF                                                       LG010908
           PERFORM IMPRIME-LINEA 2 TIMES                                LG201008
           MOVE DIREC-FA      TO M-DIREC-FA                             
           PERFORM IMPRIME-LINEA 5 TIMES.                               LG201008
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 16                   LG201008
               IF I <= CLIN-FA
                   MOVE DESC-FA (I) TO M-DESC-FA                        
                   IF EMVAL-PDC(YY) NOT = "N"                           MD040106
                       MOVE NETO-SIV(I)           TO M-NETO-SIV         
                   END-IF                                               MD040106
               END-IF
               PERFORM IMPRIME-LINEA
           END-PERFORM
           
           IF EMVAL-PDC(YY) NOT = "N"                                   
               MOVE MONE-FA       TO M-MONE-FA                          LG160908
               MOVE SUMA-NETO-SIV TO M-SUBTOTAL                         LG010908
           END-IF                                                       LG280308
           PERFORM IMPRIME-LINEA 2 TIMES.                               LG010908
           IF EMVAL-PDC(YY) NOT = "N"                                   
               IF PIVA-FA(2) NOT = 0                                    LG160908
                   MOVE PIVA-FA(2)     TO M-PIVA                        LG160908
                   MOVE "%"        TO M-POR                             LG160908
               ELSE                                                     LG160908
                   IF PIVA-FA(1) NOT = 0                                LG160908
                     MOVE PIVA-FA(2)     TO M-PIVA                      LG160908
                     MOVE "%"            TO M-POR                       LG160908
                   END-IF                                               LG160908
               END-IF                                                   LG160908
               MOVE MONE-FA       TO M-MONE-FA                          LG160908
               MOVE TOTIVA               TO M-TOTIVA                    LG160908
           END-IF                                                       MD040106
           PERFORM IMPRIME-LINEA 2 TIMES.                               
           IF EMVAL-PDC(YY) NOT = "N"                                   
               MOVE MONE-FA       TO M-MONE-FA                          LG160908
               MOVE TOTAL-A-PAGAR TO M-TOTAL                            LG010908
           END-IF                                                       
           PERFORM IMPRIME-LINEA                                        LG160908
           IF TIPO-IMPR NOT = 1 OR YY NOT = CIMP-PDC                    
               WRITE LINEA FROM SPACES BEFORE PAGE                      MD040106
           END-IF.                                                      MD040106
       EXIT-IMPRE.
           EXIT.

