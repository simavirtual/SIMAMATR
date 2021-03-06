/* SIMA - MATRICULA ACADEMICA

MODULO      : CITACION
SUBMODULO...: CITAS                  MODULO No. 305

**************************************************************************
* NOMBRE DEL PROGRAMA..: MATRI305.PRG                                    *
* TITULO DEL PROGRAMA..: IMPRESION DEL RECIBO DE LA CITA                 *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: JUL 29/96 LUN A
       Colombia, Bucaramanga        INICIO: 02:33 PM   JUL 29/96 LUN


REQUERIMIENTOS:

1- Para ejecutar la funci�n tener presente la sintaxis

OBJETIVOS:

1- Imprime los recibos de las citas

2- Retorna NIL

SINTAXIS:


*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Matri_305(aParam1,aParam2,aParam3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aParam1                              // Parametros Generales
       aParam2                              // Parametros Generales
       aParam3                              // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "EPSONFX.PRG"               // Impresora EPSON FX
       #INCLUDE "ARC-MATR.PRG"       // Archivos del Sistema
       #DEFINE  TAMPAGE CHR(27)+'C'+CHR(22) // P�gina de 22 l�neas

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL lHayErr := .F.                 // .T. Hay Error
       LOCAL cIntUno := ''                  // Path de integraci�n uno
       LOCAL cIntDos := ''                  // Path de integraci�n dos
       LOCAL cIntTre := ''                  // Path de integraci�n tres
       LOCAL cIntCua := ''                  // Path de integraci�n cuatro
     *�Variables generales

       LOCAL       i := 0                   // Contador
       LOCAL nNroNiv := 0                   // N�mero Nivel
       LOCAL cNivIni := 0                   // Nivel Inicial
       LOCAL cNivFin := 0                   // Nivel Final
       LOCAL aDetCan := {}                  // Detalles del Candidato
       LOCAL lHayCit := .F.                 // .T. Hay citas para imprimir
     *�Variables espec�ficas 

       LOCAL cNombreTes := ''               // Nombre del Estudiante
     *�Varibles de Campo
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
       IF !lUseDbf(.T.,PathSis+'\'+fSimMtr,'MTR',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathSis+'\'+FilePrn,'PRN',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+;
		       fMtrAno+cAnoUsr+ExtFile,'ANO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+;
		       FileAdm+cAnoUsr+ExtFile,'ADM',NIL,lShared)
	  cError('ABRIENDO ARCHIVOS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECCION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       DO CASE
       CASE MTR->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DEL SISTEMA')

       CASE PRN->(RECCOUNT()) == 0
	    cError('NO EXISTEN IMPRESIORAS GRABADAS')

       CASE ANO->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DE LA MATRICULA PARA EL A�O')

       CASE ADM->(RECCOUNT()) == 0
	    cError('No Existen Inscripciones')

       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>LOCALIZACION DE LA IMPRESORA
       IF !lLocCodigo('nCodigoPrn','PRN',MTR->nCodigoPrn)
	  cError('NO EXISTE LA IMPRESORA QUE ESTA HABILITADA')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN LOCALIZACION DE LA IMPRESORA

*>>>>ANALISIS DE LA ACTIVACION DE LAS ENTREVISTAS
       IF !ANO->lHayEntAno
	  cError('NO ESTA HABILITADA LA OPCION PARA ENTREVISTAS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN ANALISIS DE LA ACTIVACION DE LAS ENTREVISTAS

*>>>>LECTURA DE LOS NIVELES A IMPRIMIR
       IF !lLeeNivels(nFilInf+1,nColInf,@cNivIni,@cNivFin,NIL,cMaeAlu)
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       nNroNiv := VAL(cNivIni)
*>>>>FIN LECTURA DE LOS NIVELES A IMPRIMIR

*>>>>SELECION DEL TIPO DE IMPRESION
       IF lShared
	  NNETSETQ('COLA')
	  NNETCAPBEG(1)
	  IF !lPregunta('DESEA IMPRIMIR EN RED? Si No')
	     NNETCAPEND(1)
	  ENDIF
       ENDIF
*>>>>FIN SELECION DEL TIPO DE IMPRESION

*>>>>ACTIVACION DE LA IMPRESORA
       IF !Printer_on(MTR->lPrnArcMtr)
	   CLOSE ALL
	   RETURN NIL
       ENDIF
       @ PROW(),00 SAY INICIALIZA+PROPOR_OF+CONDEN_ON+DRAFT_ON+TAMPAGE
*>>>>FIN ACTIVACION DE LA IMPRESORA

*>>>>IMPRESION DE LAS CITAS
       DO WHILE nNroNiv <= VAL(cNivFin)

**********FILTRACION DEL MAESTRO
	    SET FILTER TO VAL(SUBS(ADM->cCodigoGru,1,2)) == nNroNiv
	    GO TOP
	    IF EOF()
	       cError('PARA '+cNomNiv(nNroNiv)+' NO HAY INSCRITOS')
	    ENDIF
**********FIN FILTRACION DEL MAESTRO

**********IMPRESION DE LA LINEA DE ESTADO
	    SET DEVICE TO SCREEN
	    LineaEstado('IMPRIMIENDO NIVEL: '+STR(nNroNiv,2)+'�'+;
			'HASTA NIVEL: '+cNivFin+;
			'�POR FAVOR ESPERE ...',cNomSis)
	    SET DEVICE TO PRINT
**********FIN IMPRESION DE LA LINEA DE ESTADO

**********IMPRESION DEL CONTENIDO DEL INFORME
	    SELECT ADM
	    GO TOP
	    DO WHILE .NOT. ADM->(EOF())

*--------------ANALISIS DE DECISION
		 DO CASE
		 CASE ADM->lRetiroAdm  // ESTA RETIRADO
		      ADM->(DBSKIP())
		      LOOP

		 CASE !ADM->lEntrevAdm // NO FUE CITADO
		      ADM->(DBSKIP())
		      LOOP

		 CASE ADM->lSiCupoEst // YA SE LE ASIGNO EL CUPO
		      ADM->(DBSKIP())
		      LOOP

		 CASE ADM->lEntrevAdm         .AND.; // YA FUE ENTREVISTADO
		      !EMPTY(ADM->dFecNacEst) .AND.;
		      !EMPTY(ADM->cLugNacEst)

		      ADM->(DBSKIP())
		      LOOP

		 ENDCASE
		 cNombreTes := SUBS(RTRIM(ADM->cNombreEst)+' '+;
					  ADM->cApelliEst+SPACE(32),1,32)
		 lHayCit := .T.
*--------------FIN ANALISIS DE DECISION

*--------------ACUMULACION DE RECIBOS
		 ++i
		 IF i <= 3
		    AADD(aDetCan,{ADM->cNumeroAdm,;
				  cNombreTes,;
				  ADM->dFecEntAdm,;
				  ADM->cHorEntAdm})
		 ENDIF
*--------------FIN ACUMULACION DE RECIBOS

*--------------IMPRESION DE LA CITA
		 IF i == 3
		    Cita(cNomEmp,cNomNiv(nNroNiv),aDetCan,cJorTxt)
		    EJECT
		    i := 0
		    aDetCan := {}
		 ENDIF
*--------------FIN IMPRESION DE LA CITA

	       SELECT ADM
	       SKIP

	    ENDDO
**********FIN IMPRESION DEL CONTENIDO DEL INFORME

	  nNroNiv++

       ENDDO
*>>>>FIN IMPRESION DE LAS CITAS

*>>>>IMPRESION DE LA CITA
       IF !EMPTY(aDetCan)
	  Cita(cNomEmp,cNomNiv(nNroNiv-1),aDetCan,cJorTxt)
	  EJECT
       ENDIF
       SET FILTER TO
       CLOSE ALL
       SET DEVICE TO SCREEN
*>>>>FIN IMPRESION DE LA CITA

*>>>>ANALISIS SI HAY CITAS
       IF !lHayCit
	  cError('NO HAY CITAS POR IMPRIMIR PARA EL NIVEL '+;
	  STR(nNroNiv,2))
       ENDIF
       RETURN NIL
*>>>>FIN IMPRESION DE LA CITA

/*************************************************************************
* TITULO DE LA FUNCION..: IMPRESION DE RECIBO DE PAGO                    *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: JUL 29/96 LUN A
       Colombia, Bucaramanga        INICIO: 02:40 PM   JUL 29/96 LUN

OBJETIVOS:

1- Imprime el recibo de la cita de la entrevista

2- Retorna Nil

SINTAXIS:

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Cita(cNomEmp,cNomNiv,aDetCan,cJorTxt)

*>>>>PARAMETROS DE LA FUNCION
/*     cNomEmp                              // Nombre de la empresa
       cNomNiv                              // Nombre del nivel
       aDetCan                              // Detalles de los candidatos
       cJorTxt                              // Jornada Escogida */
*>>>>FIN PARAMETROS DE LA FUNCION

*>>>>DECLARACION DE VARIABLES
       LOCAL nNroRec := 0                   // N�mero de Recibo
       LOCAL cTxtPrn := ''                  // Texto de impresi�n
       LOCAL dFecEnt := CTOD('00/00/00')    // Fecha de Entrevista
       LOCAL cFecEnt := ''                  // Fecha de Entrevista
       LOCAL nColUno := 0                   // Primera Columna
       LOCAL nColDos := 0                   // Segunda Columna
       LOCAL nColTre := 0                   // Tercera Columna
       LOCAL cLinea1 := ''                  // L�nea de impresi�n No 1
       LOCAL cLinea2 := ''                  // L�nea de impresi�n No 2
       LOCAL cLinea3 := ''                  // L�nea de impresi�n No 3
*>>>>FIN DECLARACION DE VARIABLES

*>>>>IMPRESION DEL NOMBRE DE LA EMPRESA
       nNroRec := LEN(aDetCan)
       nColUno := 02
       nColDos := 49
       nColTre := 96

       IF nNroRec >= 1
	   @ PROW()+00,nColUno SAY cNomEmp
       ENDIF

       IF nNroRec >= 2
	   @ PROW()+00,nColDos SAY cNomEmp
       ENDIF

       IF nNroRec == 3
	   @ PROW()+00,nColTre SAY cNomEmp
       ENDIF
*>>>>FIN IMPRESION DEL NOMBRE DE LA EMPRESA

*>>>>IMPRESION DEL TITULO
       cTxtPrn := 'CITACION A ENTREVISTA '
       IF nNroRec >= 1
	   @ PROW()+02,nColUno SAY cTxtPrn
       ENDIF

       IF nNroRec >= 2
	   @ PROW()+00,nColDos SAY cTxtPrn
       ENDIF

       IF nNroRec == 3
	   @ PROW()+00,nColTre SAY cTxtPrn
       ENDIF
*>>>>FIN IMPRESION DEL TITULO

*>>>>IMPRESION DEL CURSO
       cTxtPrn := 'PARA EL GRADO '
       IF nNroRec >= 1
	   @ PROW()+01,nColUno SAY cTxtPrn+cNomNiv+'. JORNADA: '+cJorTxt
       ENDIF

       IF nNroRec >= 2
	   @ PROW()+00,nColDos SAY cTxtPrn+cNomNiv+'. JORNADA: '+cJorTxt
       ENDIF

       IF nNroRec >= 3
	   @ PROW()+00,nColTre SAY cTxtPrn+cNomNiv+'. JORNADA: '+cJorTxt
       ENDIF
*>>>>FIN IMPRESION DEL CURSO

*>>>>IMPRESION DEL CODIGO DE INSCRIPCION
       cTxtPrn := 'INSCRIPCION No. '

       IF nNroRec >= 1
	   @ PROW()+02,nColUno SAY cTxtPrn+aDetCan[1,1]
       ENDIF

       IF nNroRec >= 2
	   @ PROW()+00,nColDos SAY cTxtPrn+aDetCan[2,1]
       ENDIF
*>>>>FIN IMPRESION DEL CODIGO DE INSCRIPCION

*>>>>IMPRESION DEL NOMBRE DEL ESTUDIANTE
       IF nNroRec == 3
	   @ PROW()+00,nColTre SAY cTxtPrn+aDetCan[3,1]
       ENDIF

       cTxtPrn := 'NOMBRE: '

       IF nNroRec >= 1
	   @ PROW()+01,nColUno SAY cTxtPrn+aDetCan[1,2]
       ENDIF

       IF nNroRec >= 2
	   @ PROW()+00,nColDos SAY cTxtPrn+aDetCan[2,2]
       ENDIF

       IF nNroRec == 3
	   @ PROW()+00,nColTre SAY cTxtPrn+aDetCan[3,2]
       ENDIF
*>>>>FIN IMPRESION DEL NOMBRE DEL ESTUDIANTE

*>>>>IMPRESION DE LA FECHA DE LA CITA
       cTxtPrn := 'FECHA: '
       IF nNroRec >= 1
	   dFecEnt := aDetCan[1,3]
	   cFecEnt := cFecha(dFecEnt)+' '+cDia(DOW(dFecEnt))+' '
	   @ PROW()+02,nColUno SAY cTxtPrn+cFecEnt
       ENDIF

       IF nNroRec >= 2
	  dFecEnt := aDetCan[2,3]
	  cFecEnt := cFecha(dFecEnt)+' '+cDia(DOW(dFecEnt))+' '
	  @ PROW()+00,nColDos SAY cTxtPrn+cFecEnt
       ENDIF

       IF nNroRec == 3
	   dFecEnt := aDetCan[3,3]
	   cFecEnt := cFecha(dFecEnt)+' '+cDia(DOW(dFecEnt))+' '
	   @ PROW()+00,nColTre SAY cTxtPrn+cFecEnt
       ENDIF
*>>>>FIN IMPRESION DE LA FECHA DE LA CITA

*>>>>IMPRESION DE LA HORA DE LA CITA
       IF nNroRec >= 1
	   @ PROW()+00,nColUno+25 SAY cHoraSys(aDetCan[1,4])
       ENDIF

       IF nNroRec >= 2
	   @ PROW()+00,nColDos+25 SAY cHoraSys(aDetCan[2,4])
       ENDIF

       IF nNroRec == 3
	   @ PROW()+00,nColTre+25 SAY cHoraSys(aDetCan[3,4])
       ENDIF
*>>>>FIN IMPRESION DE LA HORA DE LA CITA

*>>>>IMPRESION DE LA LINEA DE OBSERVACION No.1
       cLinea1 := 'Debe presentarse puntualmente con los dos '
       IF nNroRec >= 1
	   @ PROW()+02,nColUno SAY cLinea1
       ENDIF
       IF nNroRec >= 2
	   @ PROW()+00,nColDos SAY cLinea1
       ENDIF
       IF nNroRec == 3
	   @ PROW()+00,nColTre SAY cLinea1
       ENDIF
*>>>>FIN IMPRESION DE LA LINEA DE OBSERVACION No.1

*>>>>IMPRESION DE LA LINEA DE OBSERVACION No.2
       cLinea2 := 'PADRES DE FAMILIA en la oficina de '
       IF nNroRec >= 1
	   @ PROW()+01,nColUno SAY cLinea2
       ENDIF
       IF nNroRec >= 2
	   @ PROW()+00,nColDos SAY cLinea2
       ENDIF
       IF nNroRec == 3
	   @ PROW()+00,nColTre SAY cLinea2
       ENDIF
*>>>>FIN IMPRESION DE LA LINEA DE OBSERVACION No.2

*>>>>IMPRESION DE LA LINEA DE OBSERVACION No.3
       cLinea3 := 'Bienestar Estudiantil del Colegio.'
       IF nNroRec >= 1
	   @ PROW()+01,nColUno SAY cLinea3
       ENDIF
       IF nNroRec>= 2
	   @ PROW()+00,nColDos SAY cLinea3
       ENDIF
       IF nNroRec == 3
	   @ PROW()+00,nColTre SAY cLinea3
       ENDIF
       RETURN NIL
*>>>>FIN IMPRESION DE LA LINEA DE OBSERVACION No.3
