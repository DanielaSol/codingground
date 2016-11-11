        IDENTIFICATION DIVISION.
        PROGRAM-ID. TP.
        
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.
        
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
          
            SELECT NOV-TIMES1     ASSIGN TO DISK
                               	  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS NOV-TIMES1-ESTADO.
        
            SELECT NOV-TIMES2     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS NOV-TIMES2-ESTADO.
        
            SELECT NOV-TIMES3     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS NOV-TIMES3-ESTADO.
                                   
            SELECT PROFESORES     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS PROF-ESTADO.

            SELECT SUCURSALES     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS SUCURSALES-ESTADO.

            SELECT TIPOS_CLASE    ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS TIPOS_CLASE-ESTADO.
        
            SELECT MAE-TIMES ASSIGN TO PRINTER "Times.dat".
            SELECT LISTADO ASSIGN TO PRINTER "Listado.dat".
        
        DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

        FILE SECTION.
        
        FD LISTADO     LABEL RECORD OMITTED.
        01 LINEA-LISTADO PIC X(87).
        
        FD MAE-TIMES     LABEL RECORD OMITTED.
        01 REG-MAE-TIMES.
            03 MAE-TIMES-NUMERO       PIC X(5).
            03 MAE-TIMES-FECHA.
                05 MAE-TIMES-ANIO     PIC 9(4).
                05 MAE-TIMES-MES      PIC 9(2).
                05 MAE-TIMES-DIA      PIC 9(2).
            03 MAE-TIMES-SUCURSAL     PIC X(3).
            03 MAE-TIMES-TIPO-CLASE   PIC X(4).
            03 MAE-TIMES-HORAS        PIC 9(2)V99.

             
        FD NOV-TIMES1     LABEL RECORD IS STANDARD
                         VALUE OF FILE-ID IS "NovTimes1.dat".
        01 REG-NOV-TIMES1.
            03 NOV-TIMES1-NUMERO       PIC X(5).
            03 NOV-TIMES1-FECHA.
                05 NOV-TIMES1-ANIO     PIC 9(4).
                05 NOV-TIMES1-MES      PIC 9(2).
                05 NOV-TIMES1-DIA      PIC 9(2).
            03 NOV-TIMES1-SUCURSAL     PIC X(3).
            03 NOV-TIMES1-TIPO-CLASE   PIC X(4).
            03 NOV-TIMES1-HORAS        PIC 9(2)V99.
                
        FD NOV-TIMES2     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "NovTimes2.dat".
        01 REG-NOV-TIMES2.
            03 NOV-TIMES2-NUMERO      PIC X(5).
            03 NOV-TIMES2-FECHA.
                05 NOV-TIMES2-ANIO    PIC 9(4).
                05 NOV-TIMES2-MES     PIC 9(2).
                05 NOV-TIMES2-DIA     PIC 9(2).
            03 NOV-TIMES2-SUCURSAL    PIC X(3).
            03 NOV-TIMES2-TIPO-CLASE  PIC X(4).
            03 NOV-TIMES2-HORAS       PIC 9(2)V99.
                
        FD NOV-TIMES3     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "NovTimes3.dat".
        01 REG-NOV-TIMES3.
            03 NOV-TIMES3-NUMERO      PIC X(5).
            03 NOV-TIMES3-FECHA.
                05 NOV-TIMES3-ANIO    PIC 9(4).
                05 NOV-TIMES3-MES     PIC 9(2).
                05 NOV-TIMES3-DIA     PIC 9(2).
            03 NOV-TIMES3-SUCURSAL    PIC X(3).
            03 NOV-TIMES3-TIPO-CLASE   PIC X(4).
            03 NOV-TIMES3-HORAS        PIC 9(2)V99.    
       


        FD PROFESORES     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "Profesores.dat".
        01 REG-PROFESORES.
            03 PROF-NUMERO            PIC X(5).
            03 PROF-DNI               PIC 9(8).
            03 PROF-NOMBRE            PIC X(25).
            03 PROF-DIRE              PIC X(20).
            03 PROF-TEL               PIC X(20).


        FD SUCURSALES     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "Sucursales.dat".
        01 REG-SUCURSALES.
            03 SUC-SUCURSAL       PIC X(3).
            03 SUC-RAZON          PIC X(25).
            03 SUC-DIRE           PIC X(20).
            03 SUC-TEL            PIC X(20).
            03 SUC-CUIT           PIC 9(11).

     
        FD TIPOS_CLASE    LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "TiposClase.dat".
        01 REG-TIPOS_CLASE.
           03 TIP-TIP_CLASE       PIC X(4).
           03 TIP-DESC            PIC X(20).
           03 TIP-TARIFA          PIC 9(5)V99.  


        WORKING-STORAGE SECTION.        
        77 NOV-TIMES1-ESTADO PIC XX.
        77 NOV-TIMES2-ESTADO PIC XX.
        77 NOV-TIMES3-ESTADO PIC XX.
        77 PROF-ESTADO PIC XX.
        77 SUCURSALES-ESTADO PIC XX.        
        77 TIPOS_CLASE-ESTADO PIC XX.      
        77 EOF-NOVTIMES1 PIC XX VALUE "NO".
            88 EOF-NOV-TIMES1 VALUE "SI".
        77 EOF-NOVTIMES2 PIC XX VALUE "NO".
            88 EOF-NOV-TIMES2 VALUE "SI".
        77 EOF-NOVTIMES3 PIC XX VALUE "NO".
            88 EOF-NOV-TIMES3 VALUE "SI".    
        77 EOF-PROF PIC XX VALUE "NO".
            88 EOF-PROFESORES VALUE "SI".    
        77 EOF-MAE-TIMES PIC XX VALUE "NO".
            88 EOF-MAE-TIMES VALUE "SI".
        77 EOF-SUC PIC XX VALUE "NO".
            88 EOF-SUCURSALES VALUE "NO".
        77 EOF-CLASES PIC XX VALUE "NO".
            88 EOF-TIPOS_CLASE VALUE "SI".
            
            
        01 CLAVE-NOV-TIMES1.
            03 CLAVE-NOV-TIMES1-NUMERO   PIC X(5).
            03 CLAVE-NOV-TIMES1-FECHA    PIC 9(8).
            03 CLAVE-NOV-TIMES1-SUCURSAL PIC X(3).
        01 CLAVE-NOV-TIMES2.
            03 CLAVE-NOV-TIMES2-NUMERO   PIC X(5).
            03 CLAVE-NOV-TIMES2-FECHA    PIC 9(8).
            03 CLAVE-NOV-TIMES2-SUCURSAL PIC X(3).
        01 CLAVE-NOV-TIMES3.
            03 CLAVE-NOV-TIMES3-NUMERO   PIC X(5).
            03 CLAVE-NOV-TIMES3-FECHA    PIC 9(8).
            03 CLAVE-NOV-TIMES3-SUCURSAL PIC X(3).
        01 MENOR-CLAVE.
            03 MENOR-CLAVE-NUMERO        PIC X(5).
            03 MENOR-CLAVE-FECHA         PIC 9(8).
            03 MENOR-CLAVE-SUCURSAL      PIC X(3).

        01 LINEA-A-ESCRIBIR PIC 9(2) VALUE 1.
        01 HORAS-TOTALES PIC 9(4)V99.
        01 HORAS-PROFESOR PIC 9(2)V99.
        01 HORAS-FECHA PIC 9(3)V99.
        01 PROFESOR-ANTERIOR PIC X(5) VALUE '     '.
        01 FECHA-ANTERIOR PIC 9(8) VALUE 00000000.
        01 IMPORTE PIC 9(7)V99 VALUE 0.
        01 IMPORTE-FECHA PIC 9(8)V99 VALUE 0.
        01 IMPORTE-PROFESOR PIC 9(9)V99 VALUE 0. 
        01 IMPORTE-TOTAL PIC 9(10)V99 VALUE 0.
        01 AUX-TARIFA PIC 9(5)V99.
        01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR     PIC X(04).
               10 WS-CURRENT-MONTH    PIC X(02).
               10 WS-CURRENT-DAY     PIC X(02).
           05  WS-CURRENT-TIME.
               10 WS-CURRENT-HOUR     PIC  9(2).
               10  WS-CURRENT-MINUTE  PIC  9(2).
               10  WS-CURRENT-SECOND  PIC  9(2).
               10  WS-CURRENT-MS      PIC  9(2).
               10  WS-GMT-SIGN        PIC X(01).
               10  WS-GMT-TIME        PIC X(04).


       01 SUBINDICE PIC 9(2) VALUE 1.
       01 TABLA-SUCURSALES.
           02 TAB-SUCURSALES OCCURS 100 TIMES INDEXED BY SUC-INDICE.
               03 TAB-SUC-SUCURSAL PIC X(3).
               03 TAB-SUC-RAZON PIC X(25).
               03 TAB-SUC-DIRE PIC X(20).
               03 TAB-SUC-TEL PIC X(20).
               03 TAB-SUC-CUIT PIC 9(11).
       01 TABLA-TIPOS-CLASE.
           02 TAB-TIPOS-CLASE OCCURS 50 TIMES INDEXED BY TIP-INDICE.
               03 TAB-TIP-TIP-CLASE PIC X(4).
               03 TAB-TIP-DESC PIC X(20).
               03 TAB-TIP-TARIFA PIC 9(5)V99.
    
       01 ENCABEZADO.
          03 FILLER PIC X VALUE SPACES.
          03 DIA PIC 99.
          03 FILLER PIC X VALUE '/'.
          03 MES PIC 99.
          03 FILLER PIC X VALUE '/'.
          03 ANIO PIC 9(4).
          03 FILLER PIC X(61) VALUE SPACES.
          03 FILLER PIC X(5) VALUE 'HOJA '.
          03 HOJA PIC 9(3) VALUE 1.
       01 LINEA-EN-BLANCO.
          03 FILLER PIC X(80) VALUE SPACES.
       01 MOSTRAR-DATOS-PROFESOR.
          03 FILLER PIC X(1) VALUE SPACES.
          03 FILLER PIC X(10) VALUE 'PROFESOR: '.
          03 MOSTRAR-NUMERO-PROFESOR PIC X(5).
          03 FILLER PIC X(6) VALUE SPACES.
          03 FILLER PIC X(8) VALUE 'NOMBRE: '.
          03 MOSTRAR-NOMBRE-PROFESOR PIC X(25).
          03 FILLER PIC X(25) VALUE SPACES.
       01 ENCABEZADO-TABLA.
          03 FILLER PIC X(25) VALUE ' FECHA         SUCURSAL  '.
          03 FILLER PIC X(31) VALUE '  TIPO DE CLASE          TARIFA'.
          03 FILLER PIC X(24) VALUE '       HORAS     IMPORTE'.
       01 LINEA-HORIZONTAL.
          03 FILLER PIC X(80) VALUE ALL "_". 
       01 DATOS-TABLA.
          03 MOSTRAR-FECHA.
             05 MOSTRAR-DIA PIC 99.
             05 FILLER PIC X VALUE '/'.
             05 MOSTRAR-MES PIC 99.
             05 FILLER PIC X VALUE '/'.
             05 MOSTRAR-ANIO PIC 9(4).        
          03 FILLER PIC X(5) VALUE SPACES.
          03 MOSTRAR-SUCURSAL PIC X(3).
          03 FILLER PIC X(10) VALUE SPACES.
          03 MOSTRAR-TIPO-DE-CLASE PIC X(20).
          03 FILLER PIC X(1) VALUE SPACES.
          03 MOSTRAR-TARIFA PIC ZZZZ9,99.
          03 FILLER PIC X(6) VALUE SPACES.
          03 MOSTRAR-HORAS PIC Z9,99.
          03 FILLER PIC X(1) VALUE SPACES.
          03 MOSTRAR-IMPORTE PIC ZZZZZZ9,99.
       01 LINEA-TOTAL-FECHA.
          03 FILLER PIC X(18) VALUE 'TOTALES POR FECHA:'.
          03 FILLER PIC X(44) VALUE SPACES.
          03 MOSTRAR-TOTAL-HORAS-FECHA PIC ZZ9,99.
          03 FILLER PIC X(1) VALUE SPACES.
          03 MOTRAR-TOTAL-IMPORTE-FECHA PIC ZZZZZZZ9,99.
       01 LINEA-TOTAL-PROFESOR.
           03 FILLER PIC X(21) VALUE 'TOTALES POR PROFESOR:'.
           03 FILLER PIC X(38) VALUE SPACES.
           03 MOSTRAR-TOTAL-HORAS-PROFESOR PIC ZZZ9,99.
           03 FILLER PIC X(3) VALUE ' '.
           03 MOSTRAR-TOTAL-IMPORTE-PROFESOR PIC ZZZZZZZ9,99.
       01 LINEA-TOTAL-GRAL.
           03 FILLER PIC X(14) VALUE 'TOTAL GENERAL:'.
           03 FILLER PIC X(53) VALUE SPACES.
           03 MOSTRAR-TOTAL-GENERAL PIC ZZZZZZZ9,99.           

        PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      * COMIENZO.
        
        PERFORM 0100-INICIO.
        PERFORM 0200-LEER-NOV-TIMES1. 
        PERFORM 0300-LEER-NOV-TIMES2. 
        PERFORM 0400-LEER-NOV-TIMES3.
        PERFORM 0500-LEER-PROFESORES.
        PERFORM 0600-LEER-SUCURSALES.
        PERFORM 0700-LEER-TIPOS_CLASE.
        PERFORM 0800-CARGAR-TABLAS.
        MOVE 0 TO HORAS-TOTALES.
        PERFORM 1900-MOSTRAR-ENCABEZADO.
        PERFORM 1100-PROCESAR-ARCHIVOS UNTIL EOF-NOV-TIMES1
        AND EOF-NOV-TIMES2 AND EOF-NOV-TIMES3.
        MOVE  IMPORTE-TOTAL TO MOSTRAR-TOTAL-GENERAL.
        WRITE LINEA-LISTADO FROM LINEA-TOTAL-GRAL.
        PERFORM 1800-FIN.        
        STOP RUN.
        
      *----------    PERFORM INICIO      -------------------------*
      *-----------------------------------------------------------*
        0100-INICIO.           
            OPEN INPUT NOV-TIMES1.
            OPEN INPUT NOV-TIMES2.
            OPEN INPUT NOV-TIMES3.
            OPEN INPUT PROFESORES.
            OPEN INPUT SUCURSALES.
            OPEN INPUT TIPOS_CLASE.
            OPEN OUTPUT MAE-TIMES.
            OPEN OUTPUT LISTADO.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0200-LEER-NOV-TIMES1.
         READ NOV-TIMES1
            AT END MOVE "SI" TO EOF-NOVTIMES1.
         MOVE NOV-TIMES1-NUMERO TO CLAVE-NOV-TIMES1-NUMERO.
         MOVE NOV-TIMES1-FECHA TO CLAVE-NOV-TIMES1-FECHA.
         MOVE NOV-TIMES1-SUCURSAL TO CLAVE-NOV-TIMES1-SUCURSAL.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0300-LEER-NOV-TIMES2.
         READ NOV-TIMES2
            AT END MOVE "SI" TO EOF-NOVTIMES2.
         MOVE NOV-TIMES2-NUMERO TO CLAVE-NOV-TIMES2-NUMERO.
         MOVE NOV-TIMES2-FECHA TO CLAVE-NOV-TIMES2-FECHA.
         MOVE NOV-TIMES2-SUCURSAL TO CLAVE-NOV-TIMES2-SUCURSAL.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0400-LEER-NOV-TIMES3.
         READ NOV-TIMES3
            AT END MOVE "SI" TO EOF-NOVTIMES3.      
         MOVE NOV-TIMES3-NUMERO TO CLAVE-NOV-TIMES3-NUMERO.
         MOVE NOV-TIMES3-FECHA TO CLAVE-NOV-TIMES3-FECHA.
         MOVE NOV-TIMES3-SUCURSAL TO CLAVE-NOV-TIMES3-SUCURSAL.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0500-LEER-PROFESORES.
         READ PROFESORES AT END MOVE "SI" TO EOF-PROF.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------* 
        0600-LEER-SUCURSALES.
         READ SUCURSALES AT END MOVE "SI" TO EOF-SUC.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0700-LEER-TIPOS_CLASE.
         READ TIPOS_CLASE AT END MOVE "SI" TO EOF-CLASES.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0800-CARGAR-TABLAS.
         PERFORM 0900-CARGAR-TIPOS_CLASE UNTIL EOF-TIPOS_CLASE.
         MOVE 1 TO SUBINDICE.
         PERFORM 1000-CARGAR-SUCURSALES UNTIL EOF-SUCURSALES.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        0900-CARGAR-TIPOS_CLASE.       
         MOVE TIP-TIP_CLASE TO TAB-TIP-TIP-CLASE(SUBINDICE).
         MOVE TIP-DESC TO TAB-TIP-DESC(SUBINDICE).
         MOVE TIP-TARIFA TO TAB-TIP-TARIFA(SUBINDICE).
         ADD 1 TO SUBINDICE.
         PERFORM 0700-LEER-TIPOS_CLASE.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        1000-CARGAR-SUCURSALES.
         MOVE SUC-SUCURSAL TO TAB-SUC-SUCURSAL(SUBINDICE).
         MOVE SUC-RAZON TO TAB-SUC-RAZON(SUBINDICE).
         MOVE SUC-DIRE TO TAB-SUC-DIRE(SUBINDICE).
         MOVE SUC-TEL TO TAB-SUC-TEL(SUBINDICE).
         MOVE SUC-CUIT TO TAB-SUC-CUIT(SUBINDICE).
         ADD 1 TO SUBINDICE.
         PERFORM 0600-LEER-SUCURSALES.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        1100-PROCESAR-ARCHIVOS.
         PERFORM 1200-DETERMINAR-MENOR. 
         MOVE 0 TO HORAS-PROFESOR.
         MOVE 0 TO IMPORTE-PROFESOR.
         MOVE MENOR-CLAVE-NUMERO TO PROFESOR-ANTERIOR.
         MOVE PROFESOR-ANTERIOR TO MOSTRAR-NUMERO-PROFESOR.
         MOVE PROF-NOMBRE TO MOSTRAR-NOMBRE-PROFESOR.
         WRITE LINEA-LISTADO FROM MOSTRAR-DATOS-PROFESOR.
         WRITE LINEA-LISTADO FROM LINEA-EN-BLANCO.
         WRITE LINEA-LISTADO FROM ENCABEZADO-TABLA.
         WRITE LINEA-LISTADO FROM LINEA-HORIZONTAL.
         ADD 4 TO LINEA-A-ESCRIBIR.         
         PERFORM 1300-PROCESAR-PROFESOR UNTIL (EOF-NOV-TIMES1
             AND EOF-NOV-TIMES2 AND EOF-NOV-TIMES3) OR
             (PROFESOR-ANTERIOR NOT EQUAL MENOR-CLAVE-NUMERO).         
         PERFORM 0500-LEER-PROFESORES.
         ADD HORAS-PROFESOR TO HORAS-TOTALES.        
         ADD IMPORTE-PROFESOR TO IMPORTE-TOTAL.
         MOVE HORAS-PROFESOR TO MOSTRAR-TOTAL-HORAS-PROFESOR.
         MOVE IMPORTE-PROFESOR TO MOSTRAR-TOTAL-IMPORTE-PROFESOR.
         WRITE LINEA-LISTADO FROM LINEA-TOTAL-PROFESOR.
         ADD 1 TO LINEA-A-ESCRIBIR.
         PERFORM 2000-SALTO-DE-PAGINA.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        1200-DETERMINAR-MENOR.
         MOVE CLAVE-NOV-TIMES1 TO MENOR-CLAVE.
         IF CLAVE-NOV-TIMES2 < MENOR-CLAVE THEN
             MOVE CLAVE-NOV-TIMES2 TO MENOR-CLAVE.
         IF CLAVE-NOV-TIMES3 < MENOR-CLAVE THEN
             MOVE CLAVE-NOV-TIMES3 TO MENOR-CLAVE.
      
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
         1300-PROCESAR-PROFESOR.
          MOVE MENOR-CLAVE-FECHA TO FECHA-ANTERIOR.
          MOVE 0 TO HORAS-FECHA.         
          MOVE 0 TO IMPORTE-FECHA.
          PERFORM 1400-PROCESAR-FECHA UNTIL (EOF-NOV-TIMES1 AND
               EOF-NOV-TIMES2 AND EOF-NOV-TIMES3) OR
               (FECHA-ANTERIOR NOT EQUAL MENOR-CLAVE-FECHA) OR 
               (PROFESOR-ANTERIOR NOT EQUAL MENOR-CLAVE-NUMERO).   
          WRITE LINEA-LISTADO FROM LINEA-HORIZONTAL.
          ADD 1 TO LINEA-A-ESCRIBIR.
          ADD HORAS-FECHA TO HORAS-PROFESOR. 
          ADD IMPORTE-FECHA TO IMPORTE-PROFESOR.
          MOVE HORAS-FECHA TO MOSTRAR-TOTAL-HORAS-FECHA.
          MOVE IMPORTE-FECHA TO MOTRAR-TOTAL-IMPORTE-FECHA.
          WRITE LINEA-LISTADO FROM LINEA-TOTAL-FECHA.
          WRITE LINEA-LISTADO FROM LINEA-EN-BLANCO.
          ADD 2 TO LINEA-A-ESCRIBIR.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
         1400-PROCESAR-FECHA.
          PERFORM 1500-PROCESAR-NOV-TIMES1 UNTIL (MENOR-CLAVE 
              NOT EQUAL CLAVE-NOV-TIMES1) OR EOF-NOV-TIMES1.
          PERFORM 1600-PROCESAR-NOV-TIMES2 UNTIL (MENOR-CLAVE
              NOT EQUAL CLAVE-NOV-TIMES2) OR EOF-NOV-TIMES2.
          PERFORM 1700-PROCESAR-NOV-TIMES3 UNTIL (MENOR-CLAVE
              NOT EQUAL CLAVE-NOV-TIMES3) OR EOF-NOV-TIMES3.
          IF EOF-NOV-TIMES1 THEN
              MOVE 9999999999999999 TO CLAVE-NOV-TIMES1.
          IF EOF-NOV-TIMES2 THEN
              MOVE 9999999999999999 TO CLAVE-NOV-TIMES2.
          IF EOF-NOV-TIMES3 THEN
              MOVE 9999999999999999 TO CLAVE-NOV-TIMES3.
          PERFORM 1200-DETERMINAR-MENOR. 

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
         1500-PROCESAR-NOV-TIMES1.
          ADD NOV-TIMES1-HORAS TO HORAS-FECHA. 
          MOVE NOV-TIMES1-DIA TO MOSTRAR-DIA.
          MOVE NOV-TIMES1-MES TO MOSTRAR-MES.
          MOVE NOV-TIMES1-ANIO TO MOSTRAR-ANIO.
          MOVE NOV-TIMES1-SUCURSAL TO MOSTRAR-SUCURSAL.
          MOVE NOV-TIMES1-TIPO-CLASE TO MOSTRAR-TIPO-DE-CLASE.
          MOVE 1 TO TIP-INDICE.
          SEARCH TAB-TIPOS-CLASE
          AT END DISPLAY 'TIPO DE CLASE NO ENCONTRADA'
          WHEN TAB-TIP-TIP-CLASE(TIP-INDICE) EQUAL NOV-TIMES1-TIPO-CLASE
          MOVE TAB-TIP-TARIFA(TIP-INDICE) TO AUX-TARIFA
          END-SEARCH
          MOVE AUX-TARIFA TO MOSTRAR-TARIFA.
          MOVE NOV-TIMES1-HORAS TO MOSTRAR-HORAS.
          COMPUTE IMPORTE = AUX-TARIFA*NOV-TIMES1-HORAS. 
          MOVE IMPORTE TO MOSTRAR-IMPORTE.
          WRITE LINEA-LISTADO FROM DATOS-TABLA.
          ADD 1 TO LINEA-A-ESCRIBIR.
          ADD IMPORTE TO IMPORTE-FECHA.    
          WRITE REG-MAE-TIMES FROM REG-NOV-TIMES1.           
          PERFORM 0200-LEER-NOV-TIMES1.
      
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
         1600-PROCESAR-NOV-TIMES2.
          ADD NOV-TIMES2-HORAS TO HORAS-FECHA.
          MOVE NOV-TIMES2-DIA TO MOSTRAR-DIA.
          MOVE NOV-TIMES2-MES TO MOSTRAR-MES.
          MOVE NOV-TIMES2-ANIO TO MOSTRAR-ANIO.
          MOVE NOV-TIMES2-SUCURSAL TO MOSTRAR-SUCURSAL.
          MOVE NOV-TIMES2-TIPO-CLASE TO MOSTRAR-TIPO-DE-CLASE.          
          MOVE NOV-TIMES2-HORAS TO MOSTRAR-HORAS.
          MOVE 1 TO TIP-INDICE.
          SEARCH TAB-TIPOS-CLASE
          AT END DISPLAY 'TIPO DE CLASE NO ENCONTRADA'
          WHEN TAB-TIP-TIP-CLASE(TIP-INDICE) EQUAL NOV-TIMES2-TIPO-CLASE
          MOVE TAB-TIP-TARIFA(TIP-INDICE) TO AUX-TARIFA
          END-SEARCH
          MOVE AUX-TARIFA TO MOSTRAR-TARIFA.
          COMPUTE IMPORTE = AUX-TARIFA*NOV-TIMES2-HORAS.    
          MOVE IMPORTE TO MOSTRAR-IMPORTE.
          WRITE LINEA-LISTADO FROM DATOS-TABLA.
          ADD 1 TO LINEA-A-ESCRIBIR.
          ADD IMPORTE TO IMPORTE-FECHA.
          WRITE REG-MAE-TIMES FROM REG-NOV-TIMES2.
          PERFORM 0300-LEER-NOV-TIMES2.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
         1700-PROCESAR-NOV-TIMES3.
          ADD NOV-TIMES3-HORAS TO HORAS-FECHA.
          MOVE NOV-TIMES3-DIA TO MOSTRAR-DIA.
          MOVE NOV-TIMES3-MES TO MOSTRAR-MES.
          MOVE NOV-TIMES3-ANIO TO MOSTRAR-ANIO.
          MOVE NOV-TIMES3-SUCURSAL TO MOSTRAR-SUCURSAL.
          MOVE NOV-TIMES3-TIPO-CLASE TO MOSTRAR-TIPO-DE-CLASE.         
          MOVE NOV-TIMES3-HORAS TO MOSTRAR-HORAS.
          MOVE 1 TO TIP-INDICE.
          SEARCH TAB-TIPOS-CLASE
          AT END DISPLAY 'TIPO DE CLASE NO ENCONTRADA'
          WHEN TAB-TIP-TIP-CLASE(TIP-INDICE) EQUAL NOV-TIMES3-TIPO-CLASE
          MOVE TAB-TIP-TARIFA(TIP-INDICE) TO AUX-TARIFA
          END-SEARCH
          MOVE AUX-TARIFA TO MOSTRAR-TARIFA.
          COMPUTE IMPORTE = AUX-TARIFA*NOV-TIMES3-HORAS.
          MOVE IMPORTE TO MOSTRAR-IMPORTE.
          WRITE LINEA-LISTADO FROM DATOS-TABLA.
          ADD 1 TO LINEA-A-ESCRIBIR.
          ADD IMPORTE TO IMPORTE-FECHA.
          WRITE REG-MAE-TIMES FROM REG-NOV-TIMES3.
          PERFORM 0400-LEER-NOV-TIMES3.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
         1800-FIN.
            CLOSE NOV-TIMES1.
            CLOSE NOV-TIMES2.
            CLOSE NOV-TIMES3.
            CLOSE PROFESORES.
            CLOSE SUCURSALES.
            CLOSE TIPOS_CLASE.
            CLOSE MAE-TIMES.
            CLOSE LISTADO.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        1900-MOSTRAR-ENCABEZADO.         
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR TO ANIO.
           MOVE WS-CURRENT-MONTH TO MES.
           MOVE WS-CURRENT-DAY TO DIA.
           MOVE 1 TO LINEA-A-ESCRIBIR.
           WRITE LINEA-LISTADO FROM LINEA-HORIZONTAL.
           WRITE LINEA-LISTADO FROM ENCABEZADO.
           WRITE LINEA-LISTADO FROM LINEA-EN-BLANCO.
           ADD 3 TO LINEA-A-ESCRIBIR.           

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
        2000-SALTO-DE-PAGINA.
           PERFORM UNTIL LINEA-A-ESCRIBIR EQUAL 61
              WRITE LINEA-LISTADO FROM LINEA-EN-BLANCO
               ADD 1 TO LINEA-A-ESCRIBIR
           END-PERFORM.
           ADD 1 TO HOJA.
           MOVE 0 TO LINEA-A-ESCRIBIR.
           PERFORM 1900-MOSTRAR-ENCABEZADO.

