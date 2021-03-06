/* SIMA - MATRICULA ACADEMICA

MODULO      : UTILIDADES
SUBMODULO...: LIMITE DE CUPOS               MODULO No. 101

**************************************************************************
* TITULO..: LIMITE DE CUPOS                                              *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: JUL 30/96 MAR A
       Bucaramanga, Colombia	    INICIO: 05:00 PM   JUL 30/96 MAR

REQUERIMIENTOS:

1- Para ejecutar la funci�n tener presente la sintaxis

OBJETIVOS:

1- Permite consultar los cupos disponibles para cada nivel

2- Permite definir los cupos dispobiles para cada nivel

3- Retorna NIL


SINTAXIS:

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Matri_793(aParam1,aParam2,aParam3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aParam1                              // Parametros Generales
       aParam2                              // Parametros Generales
       aParam3                              // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "ARC-MATR.PRG"       // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL lHayErr := .F.                 // .T. Hay Error
     *�Variables generales

       LOCAL       i := 0                   // Contador
       LOCAL aCupDis[18]                    // N�mero de Cupos disponibles
       LOCAL nColIni := 0                   // Columna Inicial
       LOCAL nNroFil := 0                   // N�mero de la fila
       LOCAL aColJar := {}                  // N�meros de Columna de Jardin
       LOCAL aColPri := {}                  // N�meros de Columna de Primaria
       LOCAL aColBto := {}                  // N�meros de Columna de Bachillerato
       LOCAL cCabeza := ''                  // Encabezado de columna
       LOCAL GetList := {}                  // Variable del sistema
     *�Variables espec�ficas 
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DECLARACION PARAMETROS GENERALES
       LOCAL lShared := .T.                 // .T. Sistema Compartido
       LOCAL nModCry := 0                   // Modo de Protecci�n
       LOCAL cCodSui := ''                  // C�digo del Sistema
       LOCAL cNomSis := ''                  // Nombre del Sistema
     *�Detalles del Sistema

       LOCAL cEmpPal := ''                  // Nombre de la Empresa principal
       LOCAL cNitEmp := ''                  // Nit de la Empresa
       LOCAL cNomEmp := ''                  // Nombre de la Empresa
       LOCAL cNomSec := ''                  // Nombre de la Empresa Secundario
       LOCAL cCodEmp := ''                  // C�digo de la Empresa
     *�Detalles de la Empresa

       LOCAL cNomUsr := ''                  // Nombre del Usuario
       LOCAL cAnoUsr := ''                  // A�o del usuario
       LOCAL cAnoSis := ''                  // A�o del sistema
       LOCAL cPatSis := ''                  // Path del sistema
     *�Detalles del Usuario

       LOCAL nFilPal := 0                   // Fila Inferior Men� principal
       LOCAL nFilInf := 0                   // Fila Inferior del SubMen�
       LOCAL nColInf := 0                   // Columna Inferior del SubMen�
     *�Detalles Tecnicos

       LOCAL PathW01 := ''                  // Sitio del Sistema No.01
       LOCAL PathW02 := ''                  // Sitio del Sistema No.02
       LOCAL PathW03 := ''                  // Sitio del Sistema No.03
       LOCAL PathW04 := ''                  // Sitio del Sistema No.04
       LOCAL PathW05 := ''                  // Sitio del Sistema No.05
       LOCAL PathW06 := ''                  // Sitio del Sistema No.06
       LOCAL PathW07 := ''                  // Sitio del Sistema No.07
       LOCAL PathW08 := ''                  // Sitio del Sistema No.08
       LOCAL PathW09 := ''                  // Sitio del Sistema No.09
       LOCAL PathW10 := ''                  // Sitio del Sistema No.10
     *�Sitios del Sistema

       LOCAL PathUno := ''                  // Path de Integraci�n Uno
       LOCAL PathDos := ''                  // Path de Integraci�n Dos
       LOCAL PathTre := ''                  // Path de Integraci�n Tres
       LOCAL PathCua := ''                  // Path de Integraci�n Cuatro
     *�Path de Integraci�n

       LOCAL cMaeAlu := ''                  // Maestros habilitados
       LOCAL cMaeAct := ''                  // Maestro Activo
       LOCAL cJorTxt := ''                  // Jornada escogida
     *�Detalles Acad�micos

       LOCAL aParams := {}                  // Parametros Generales
*>>>>FIN DECLARACION PARAMETROS GENERALES

*>>>>LECTURA PARAMETROS GENERALES
       aParams := aParams(aParam1,aParam2,aParam3)
       IF  !lParam0101(aParams,;
		       @lShared,@nModCry,@cCodSui,@cNomSis,;
		       @cEmpPal,@cNitEmp,@cNomEmp,@cNomSec,@cCodEmp,;
		       @cNomUsr,@cAnoUsr,@cAnoSis,@cPatSis,;
		       @nFilPal,@nFilInf,@nColInf,;
		       @PathW01,@PathW02,@PathW03,@PathW04,@PathW05,;
		       @PathW06,@PathW07,@PathW08,@PathW09,@PathW10,;
		       @PathUno,@PathDos,@PathTre,@PathCua,;
		       @cMaeAlu,@cMaeAct,@cJorTxt)
	  CloseAll()
	  RETURN NIL
       ENDIF
       CloseAll()
*>>>>FIN LECTURA PARAMETROS GENERALES

*>>>>SELECCION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,cPatSis+'\'+;
		       fMtrAno+cAnoUsr+ExtFile,'ANO',NIL,lShared)
	  cError('ABRIENDO ARCHIVOS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECCION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       DO CASE
       CASE ANO->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DE LA MATRICULA PARA EL A�O')
       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       CuposDispi(lShared,cNomEmp,cNomSis,nFilPal,cMaeAlu)
       CLOSE ALL
       RETURN NIL
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS



***************************************************************************
*----------------------------FIN DEL PROGRAMA ----------------------------*
*--------------- DECLARACION DE FUNCIONES PARA EL PROGRAMA ---------------*
***************************************************************************

/*************************************************************************
* TITULO DE LA FUNCION..: LIMITE DE CUPOS                                *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: JUL 30/96 MAR A
       Bucaramanga, Colombia	    INICIO: 05:00 PM   JUL 30/96 MAR

OBJETIVOS:

1- Permite consultar los cupos disponibles para cada nivel

2- Permite definir los cupos dispobiles para cada nivel

3- Retorna NIL

SINTAXIS:


*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION CuposDispi(lShared,cNomEmp,cNomSis,nFilPal,cMaeAlu)

*>>>>PARAMETROS DE LA FUNCION
/*     lShared                              // .T. Archivos Compartidos
       cNomEmp                              // Nombre de la empresa
       cNomSis                              // Nombre del Sistema
       nFilPal                              // Fila principal
       cMaeAlu                              // Maestros habilitados */
*>>>>FIN PARAMETROS DE LA FUNCION

*>>>>DECLARACION DE VARIABLES
       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL       i := 0                   // Contador
       LOCAL nTipCur := 0                   // Tipo de Cursor
       LOCAL aCupDis[18]                    // N�mero de Cupos disponibles
       LOCAL nFilIni := 0                   // Fila Inicial
       LOCAL nNroFil := 0                   // N�mero de la fila
       LOCAL aColJar := {}                  // N�meros de Columna de Jardin
       LOCAL aColPri := {}                  // N�meros de Columna de Primaria
       LOCAL aColBto := {}                  // N�meros de Columna de Bachillerato
       LOCAL cCabeza := ''                  // Encabezado de columna
       LOCAL GetList := {}                  // Variable del sistema
     *�Variables espec�ficas 
*>>>>FIN DECLARACION DE VARIABLES

*>>>>PARAMETROS POR DEFECTO
       nTipCur := SETCURSOR()
       SETCURSOR(1)
       cSavPan := SAVESCREEN(0,0,24,79)
*>>>>FIN PARAMETROS POR DEFECTO

*>>>>IMPRESION DE LA LINEA DE ESTADO
       LineaEstado('LICENCIA DE USO A FAVOR DE: '+cNomEmp,cNomSis)
*>>>>FIN IMPRESION DE LA LINEA DE ESTADO

*>>>>DEFINICION DEL LIMITE CUPOS PARA JARDIN
       nFilIni := nMarco(nFilPal+1,'DISPONIBILIDAD DE CUPOS')
       IF 'JAR' $ cMaeAlu

*---------IMPRESION DE LOS ENCABEZADOS DEL NIVEL
	    nNroFil := nFilIni
	    cCabeza := cRegPrint({'MATERNALES  ',;
				  'CAMINADORES ',;
				  'PARVULOS    ',;
				  'PREJARDIN   ',;
				  'JARDINA     '},;   // 'JARDINB     '},;
				 {12,12,12,12,12,12},@aColJar)
	    @ nNroFil++,  01 SAY 'CUPOS PARA JARDIN'
	    @ nNroFil++,01  SAY cCabeza
*---------FIN IMPRESION DE LOS ENCABEZADOS DEL NIVEL

*---------INICIALIZACION DE LOS CUPOS
	    FOR i = 13 TO 17
	      aCupDis[i] := SUBS(ANO->cNroCupAno,i*4-3,4)
	      @ nNroFil,aColJar[i-12] SAY aCupDis[i] PICT '9999' COLOR("I")
	    ENDFOR
*---------FIN INICIALIZACION DE LOS CUPOS

       ENDIF
*>>>>FIN DEFINICION DEL LIMITE CUPOS PARA JARDIN

*>>>>DEFINICION DEL LIMITE CUPOS PARA PRIMARIA
       IF 'PRI' $ cMaeAlu

*---------IMPRESION DE LOS ENCABEZADOS DEL NIVEL
	    cCabeza := cRegPrint({'JARDINB ',;
				  'PRIMERO ',;
				  'SEGUNDO ',;
				  'TERCERO ',;
				  'CUARTO  ',;
				  'QUINTO  '},;
				 {08,08,08,08,08,08},@aColPri)

	    nNroFil := IF('JAR' $ cMaeAlu,nNroFil+2,nFilIni)
	    @ nNroFil++,  01 SAY 'CUPOS PARA PRIMARIA'
	    @ nNroFil++,01  SAY cCabeza
*---------FIN IMPRESION DE LOS ENCABEZADOS DEL NIVEL

*---------INICIALIZACION DE LOS CUPOS
	    FOR i = 1 TO 6
	      aCupDis[i] := SUBS(ANO->cNroCupAno,i*4-3,4)
	      @ nNroFil,aColPri[i] SAY aCupDis[i] PICT '9999' COLOR("I")
	    ENDFOR
*---------FIN INICIALIZACION DE LOS CUPOS

       ENDIF
*>>>>FIN DEFINICION DEL LIMITE CUPOS PARA PRIMARIA

*>>>>DEFINICION DEL LIMITE CUPOS PARA BACHILLERATO
       IF 'BTO' $ cMaeAlu

*---------IMPRESION DE LOS ENCABEZADOS DEL NIVEL
	    nNroFil := IF('PRI' $ cMaeAlu,nNroFil+2,nFilIni)
	    cCabeza := cRegPrint({'SEXTO   ',;
				  'SEPTIMO ',;
				  'OCTAVO  ',;
				  'NOVENO  ',;
				  'DECIMO  ',;
				  'UNDECIMO'},;
				 {08,08,08,08,08,08},@aColBto)
	    @ nNroFil++,01 SAY 'CUPOS PARA BACHILLERATO'
	    @ nNroFil++,01 SAY cCabeza
*---------FIN IMPRESION DE LOS ENCABEZADOS DEL NIVEL

*---------INICIALIZACION DE LOS CUPOS
	    FOR i = 7 TO 12
	      aCupDis[i] := SUBS(ANO->cNroCupAno,i*4-3,4)
	      @ nNroFil,aColBto[i-6] SAY aCupDis[i] PICT '9999' COLOR("I")
	    ENDFOR
*---------FIN INICIALIZACION DE LOS CUPOS

       ENDIF
*>>>>FIN DEFINICION DEL LIMITE CUPOS PARA BACHILLERATO

*>>>>LECTURA DE LOS CUPOS
       IF lPregunta('DESEA MODIFICAR EL LIMITE DE CUPOS? No Si')

**********LECTURA Y GRABACION DE CUPOS DE JARDIN
	    IF 'JAR' $ cMaeAlu

*--------------LECTURA DE CUPOS
		 nNroFil := nFilIni+2
		 FOR i = 13 TO 17
		   @ nNroFil,aColJar[i-12] GET aCupDis[i] PICT '9999'
		 ENDFOR
		 READ
*--------------FIN LECTURA DE CUPOS

*--------------GRABACION DE CUPOS
		 IF ANO->(lRegLock(lShared,.F.))

		    FOR i := 13 TO 17
			REPL ANO->cNroCupAno WITH STUFF(ANO->cNroCupAno,;
							i*4-3,4,aCupDis[i])
			ANO->(DBCOMMIT())
		    ENDFOR

		 ELSE
		    cError('NO SE GRABAN LOS CUPOS PARA JARDIN')
		 ENDIF
		 IF lShared
		    ANO->(DBUNLOCK())
		 ENDIF
*--------------FIN GRABACION DE CUPOS

	    ENDIF
**********FIN CUPOS PARA JARDIN

**********LECTURA Y GRABACION DE CUPOS DE PRIMARIA
	    IF 'PRI' $ cMaeAlu

*--------------LECTURA DE CUPOS
		 nNroFil := IF('JAR' $ cMaeAlu,nNroFil+4,nFilIni+2)
		 FOR i = 1 TO 6
		   @ nNroFil,aColPri[i] GET aCupDis[i] PICT '9999'
		 ENDFOR
		 READ
*--------------FIN LECTURA DE CUPOS

*--------------GRABACION DE CUPOS
		 IF ANO->(lRegLock(lShared,.F.))

		    FOR i := 1 TO 6
			REPL ANO->cNroCupAno WITH STUFF(ANO->cNroCupAno,;
							i*4-3,4,aCupDis[i])
			ANO->(DBCOMMIT())
		    ENDFOR

		 ELSE
		    cError('NO SE GRABAN LOS CUPOS PARA JARDIN')
		 ENDIF
		 IF lShared
		    ANO->(DBUNLOCK())
		 ENDIF
*--------------FIN GRABACION DE CUPOS

	    ENDIF
**********FIN LECTURA Y GRABACION DE CUPOS DE PRIMARIA

**********LECTURA Y GRABACION DE CUPOS DE BACHILLERATO
	    IF 'BTO' $ cMaeAlu

*--------------LECTURA DE CUPOS
		 nNroFil := IF('PRI' $ cMaeAlu,nNroFil+4,nFilIni+2)
		 FOR i = 7 TO 12
		   @ nNroFil,aColBto[i-6] GET aCupDis[i] PICT '9999'
		 ENDFOR
		 READ
*--------------FIN LECTURA DE CUPOS

*--------------GRABACION DE CUPOS
		 IF ANO->(lRegLock(lShared,.F.))

		    FOR i := 7 TO 12
			REPL ANO->cNroCupAno WITH STUFF(ANO->cNroCupAno,;
							i*4-3,4,aCupDis[i])
			ANO->(DBCOMMIT())
		    ENDFOR

		 ELSE
		    cError('NO SE GRABAN LOS CUPOS PARA JARDIN')
		 ENDIF
		 IF lShared
		    ANO->(DBUNLOCK())
		 ENDIF
*--------------FIN GRABACION DE CUPOS

	    ENDIF
	    cPausa()
**********FIN LECTURA Y GRABACION DE CUPOS DE BACHILLERATO

       ENDIF
       RESTSCREEN(0,0,24,79,cSavPan)
       SETCURSOR(nTipCur)

       RETURN NIL
*>>>>FIN LECTURA DE LOS CUPOS




