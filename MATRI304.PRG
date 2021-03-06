/* SIMA - MATRICULA ACADEMICA

MODULO      : CITACION
SUBMODULO...: FECHA DE CITA          MODULO No. 304

**************************************************************************
* NOMBRE DEL PROGRAMA..: MATRI304.PRG                                    *
* TITULO DEL PROGRAMA..: GRABACION DE FECHAS DE CITAS A ENTREVISTAS      *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: ENE 04/96 JUE A
       Colombia, Bucaramanga        INICIO: 10:00 AM   ENE 04/96 JUE


REQUERIMIENTOS:

1- Para ejecutar la funci�n tener presente la sintaxis

OBJETIVOS:

1- Permite asignar la fecha y hora de la entrevista

2- Retorna NIL

SINTAXIS:


*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Matri_304(aParam1,aParam2,aParam3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aParam1                              // Parametros Generales
       aParam2                              // Parametros Generales
       aParam3                              // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "EPSONFX.PRG"
       #INCLUDE "inkey.ch"                  // Declaraci�n de teclas
       #INCLUDE "ARC-MATR.PRG"      // Archivos del Sistema
       #DEFINE  TAMPAGE CHR(27)+'C'+CHR(22) // P�gina de 22 l�neas

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL lHayErr    := .F.              // .T. Hay Error

       LOCAL nNroNiv := 0                   // N�mero Nivel
       LOCAL cNivIni := 0                   // Nivel Inicial
       LOCAL cNivFin := 0                   // Nivel Final

       LOCAL dFecIni := ''                  // Fecha de inicio
       LOCAL cHorIni := ''                  // Hora de Inicial
       LOCAL cHorFin := ''                  // Hora Final
       LOCAL nMinEnt := ''                  // Duraci�n entrevista
       LOCAL nEntrev := 0                   // Entrevistadores
       LOCAL lHayCit := .F.                 // .T. Hay citas para imprimir
       LOCAL nNroFil := 0                   // N�mero de la Fila
       LOCAL nNroEnt := 0                   // N�mero de Entrevistas 
       LOCAL cHorPar := ''                  // Hora Parcial
       LOCAL lMisHor := .F.                 // .T. Misma Hora
       LOCAL lMisFec := .F.                 // .T. Misma Fecha

       LOCAL cNumeroTad := ''               // N�mero de la entrevista
       LOCAL cNombreTes := ''               // Nombre del Estudiante
       LOCAL dFecEntTad := CTOD("00/00/00") // Fecha de la entrevista
       LOCAL cHorEntTad := ''               // Hora de la entrevista

       LOCAL GetList := {}                  // Variable del sistema
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

*>>>>ANALISIS DE LA ACTIVACION DE LOS EXAMENES
       IF !ANO->lHayEntAno
	  cError('NO ESTA HABILITADA LA OPCION PARA ENTREVISTAS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN ANALISIS DE LA ACTIVACION DE LOS EXAMENES

*>>>>LECTURA DE LOS NIVELES A IMPRIMIR
       IF !lLeeNivels(nFilInf+1,nColInf,@cNivIni,@cNivFin,NIL,cMaeAlu)
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       nNroNiv := VAL(cNivIni)
*>>>>FIN LECTURA DE LOS NIVELES A IMPRIMIR

*>>>>LECTURA DE LOS PARAMETROS PARA LAS CITAS
       dFecIni := DATE()
       cHorPar := SPACE(08)
       cHorIni := SPACE(08)
       cHorFin := SPACE(08)
       nMinEnt := 0
       nEntrev := 0

       @ nFilInf+3,nColInf SAY "HORA PARCIAL      :";
			   GET cHorPar PICT "99:99:99" ;
			   VALID TIMEVALID(ALLTRIM(cHorPar))

       @ nFilInf+4,nColInf SAY 'FECHA INICIO      :';
			   GET dFecIni

       @ nFilInf+5,nColInf SAY "HORA INICIAL      :";
			   GET cHorIni PICT "99:99:99" ;
			   VALID TIMEVALID(ALLTRIM(cHorIni))

       @ nFilInf+6,nColInf SAY "HORA FINAL        :";
			   GET cHorFin PICT "99:99:99" ;
			   VALID TIMEVALID(ALLTRIM(cHorFin))

       @ nFilInf+7,nColInf SAY "MINUTOS POR ALUMNO:";
			   GET nMinEnt PICT "99"

       @ nFilInf+8,nColInf SAY "ENTREVISTADORES   :";
			   GET nEntrev PICT "99"
       READ

       cHorFin := SECTOTIME(TIMETOSEC(cHorFin)+nMinEnt*60)
       cHorEntTad := IF(cHorPar == '00:00:00',cHorIni,cHorPar)
       dFecEntTad := dFecIni
       nNroEnt := 1
       lMisHor := .F.
       lMisFec := .F.
*>>>>FIN LECTURA PARAMETROS PARA LAS CITAS

*>>>>ANALISIS PARA LA SALIDA DEL PROGRAMA
       IF nEntrev == 0
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN ANALISIS PARA LA SALIDA DEL PROGRAMA

*>>>>GRABACION POR NIVELES
       DO WHILE nNroNiv <= VAL(cNivFin)

**********FILTRACION DEL MAESTRO
	    SET FILTER TO VAL(SUBS(ADM->cCodigoGru,1,2)) == nNroNiv
	    GO TOP
	    IF EOF()
*	       cError('PARA '+cNomNiv(nNroNiv)+' NO HAY INSCRITOS')
	    ENDIF
**********FIN FILTRACION DEL MAESTRO

**********GRABACION DE LOS REGISTROS
	     nNroFil := nMarco(nFilPal+1,'ASIGNANDO FECHA Y HORA '+;
			       'DE ENTREVISTA PARA '+cNomNiv(nNroNiv))
	     DO WHILE .NOT. ADM->(EOF())

*---------------ANALISIS DE DECISION
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

		  CASE ADM->lEntrevAdm        .AND.; // YA FUE ENTREVISTADO
		       !EMPTY(ADM->dFecNacEst) .AND.;
		       !EMPTY(ADM->cLugNacEst)

		       ADM->(DBSKIP())
		       LOOP

		  ENDCASE
		  cNombreTes := SUBS(RTRIM(ADM->cApelliEst)+' '+;
					   ADM->cNombreEst+SPACE(35),1,35)
		  lHayCit := .T.
*---------------FIN ANALISIS DE DECISION

*---------------GRABACION DEL REGISTRO
		  IF ADM->(lRegLock(lShared,.F.))
		     REPL ADM->dFecEntAdm WITH dFecEntTad
		     REPL ADM->cHorEntAdm WITH cHorEntTad
		  ELSE
		      cError('NO SE GRABA EL REGISTRO')
		  ENDIF
		  ADM->(DBCOMMIT())
		  IF lShared
		     ADM->(DBUNLOCK())
		  ENDIF
*---------------FIN GRABACION DEL REGISTRO

*---------------VISUALIZACION DEL ESTUDIANTE GRABADO
		  nNroFil++
		  @ nNroFil,01 SAY cNombreTes+'... Fecha: '+;
				   cFecha(dFecEntTad)+' '+;
				   cDia(DOW(dFecEntTad))+;
				   ' Hora: '+SUBS(cHoraSys(cHorEntTad),6,8)

		  IF nNroFil == 21
		     nNroFil := nMarco(nFilPal+1,'ASIGNANDO FECHA Y HORA '+;
				'DE ENTREVISTA PARA '+cNomNiv(nNroNiv))
		  ENDIF
*---------------FIN VISUALIZACION DEL ESTUDIANTE GRABADO

*---------------CALCULO DE LA FECHA Y HORA
		  nNroEnt++
		  IF nNroEnt > nEntrev
		     nNroEnt := 1
		     cHorEntTad := SECTOTIME(TIMETOSEC(cHorEntTad)+nMinEnt*60)
		     IF cHorEntTad == cHorFin
			cHorEntTad := cHorIni
			dFecEntTad++
			DO CASE
			CASE DOW(dFecEntTad) == 7
			     dFecEntTad := dFecEntTad + 2
			CASE DOW(dFecEntTad) == 1
			     dFecEntTad++
			ENDCASE
		     ENDIF
		  ENDIF
*---------------FIN CALCULO DE LA FECHA Y HORA

		ADM->(DBSKIP())

	    ENDDO
**********FIN GRABACION DE LOS REGISTROS

**********INCREMENTO DEL NIVEL
	  DO WHILE .T.
	     nNroNiv++
	     IF nNroNiv <= 11
		IF lValCodNiv(STR(nNroNiv,2),cMaeAlu,.F.)
		   EXIT
		ELSE
		   LOOP
		ENDIF
	     ENDIF
	     EXIT
	  ENDDO
**********FIN INCREMENTO DEL NIVEL

       ENDDO
*>>>>FIN GRABACION POR NIVELES

*>>>>ANALISIS SI HAY CITAS
       IF !lHayCit
	  cError('NO HAY CITAS PENDIENTES POR ASIGNAR PARA EL NIVEL '+;
		 STR(nNroNiv-1,2))
       ENDIF
       SET FILTER TO
       CLOSE ALL
       RETURN NIL
*>>>>FIN IMPRESION DE LA CITA
