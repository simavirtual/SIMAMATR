/* SIMA - MATRICULA ACADEMICA

MODULO      : CITACION
SUBMODULO...: TODOS                  MODULO No. 312

**************************************************************************
* NOMBRE DEL PROGRAMA..: MATRI312.PRG                                    *
* TITULO DEL PROGRAMA..: CITACION A ENTREVISTAS PARA TODOS               *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: OCT 31/96 JUE A
       Colombia, Bucaramanga        INICIO: 12:00 PM   OCT 31/96 JUE

REQUERIMIENTOS:

1- Para ejecutar la funci�n tener presente la sintaxis

OBJETIVOS:

1- Permite citar a entrevistas a todos los estudiantes que
   aproboron la Evaluaci�n

2- Retorna NIL

SINTAXIS:


*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Matri_312(aParam1,aParam2,aParam3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aParam1                              // Parametros Generales
       aParam2                              // Parametros Generales
       aParam3                              // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "ARC-MATR.PRG"       // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL lHayErr := .F.                 // .T. Hay Error
       LOCAL cIntUno := ''                  // Path de integraci�n uno
       LOCAL cIntDos := ''                  // Path de integraci�n dos
       LOCAL cIntTre := ''                  // Path de integraci�n tres
       LOCAL cIntCua := ''                  // Path de integraci�n cuatro
     *�Variables generales

       LOCAL nNroNiv := 0                   // N�mero Nivel
       LOCAL cNivIni := 0                   // Nivel Inicial
       LOCAL cNivFin := 0                   // Nivel Final
       LOCAL nCopias := 0                   // N�meros de copias a imprimir
       LOCAL nNroCop := 1                   // N�mero de copia
       LOCAL nNroFil := 0                   // N�mero de la Fila
     *�Variables espec�ficas 

       LOCAL lEntrevTad := .F.              // .T. Entrevista
       LOCAL cNombreTes := ''               // Nombre del Estudiante
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

*>>>>LECTURA DE LOS NIVELES A IMPRIMIR
       IF !lLeeNivels(nFilInf+1,nColInf,@cNivIni,@cNivFin,NIL,cMaeAlu)
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       nNroNiv := VAL(cNivIni)
*>>>>FIN LECTURA DE LOS NIVELES A IMPRIMIR

*>>>>ANALISIS DE DECISION
       lEntrevTad := .F.
       IF lPregunta('DESEA CITAR A ENTREVISTA '+;
		     'A TODOS LOS ESTUDIANTES? Si No')
	  lEntrevTad := .T.
       ENDIF
*>>>>FIN ANALISIS DE DECISION

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

*>>>>IMPRESION DEL INFORME
       DO WHILE nNroNiv <= VAL(cNivFin)

**********FILTRACION DEL MAESTRO
	    SET FILTER TO VAL(SUBS(ADM->cCodigoGru,1,2)) == nNroNiv
	    GO TOP
	    IF EOF()
	       cError('PARA '+cNomNiv(nNroNiv)+' NO HAY INSCRITOS')
	       nNroCop := nCopias+1
	    ENDIF
**********FIN FILTRACION DEL MAESTRO

**********IMPRESION DE LA LINEA DE ESTADO
	    LineaEstado('CITANDO AL NIVEL: '+;
			STR(nNroNiv,2)+'�'+;
			'HASTA NIVEL: '+cNivFin+;
			'�POR FAVOR ESPERE ...',cNomSis)
**********FIN IMPRESION DE LA LINEA DE ESTADO

**********CITACION A LA ENTREVISTA
	    nNroFil := nMarco(nFilPal+1,'CITANDO AL NIVEL '+;
					cNomNiv(nNroNiv))
	    SELECT ADM
	    GO TOP
	    DO WHILE .NOT. EOF()

*--------------VALIDACION DEL CODIGO
		 DO CASE
		 CASE ADM->lRetiroAdm  // ESTA RETIRADO
		      ADM->(DBSKIP())
		      LOOP

		 CASE ADM->nDefExaAdm == 0 .AND.; // NO SE LE HA REGISTRADO LA EVALUACION
		      ANO->lHayExaAno

		      ADM->(DBSKIP())
		      LOOP

		 CASE ADM->nDefExaAdm # 0 .AND.; // NO APROBO EL EXAMEN
		      ANO->lHayExaAno     .AND.;
		      !ADM->lExamenAdm

		      ADM->(DBSKIP())
		      LOOP

		 ENDCASE
		 cNombreTes := SUBS(RTRIM(ADM->cApelliEst)+' '+;
					  ADM->cNombreEst+SPACE(35),1,35)
*--------------FIN VALIDACION DEL CODIGO

*---------------GRABACION DEL REGISTRO
		  IF ADM->(lRegLock(lShared,.F.))
		     REPL ADM->lEntrevAdm WITH lEntrevTad
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
		  @ nNroFil,01 SAY cNombreTes+IF(lEntrevTad,'... CITADO','')

		  IF nNroFil == 21
		     nNroFil := nMarco(nFilPal+1,'CITANDO AL NIVEL '+;
						 cNomNiv(nNroNiv))
		  ENDIF
*---------------FIN VISUALIZACION DEL ESTUDIANTE GRABADO

		ADM->(DBSKIP())

	    ENDDO
**********FIN CITACION A LA ENTREVISTA

	  nNroNiv++

       ENDDO
       SET FILTER TO
       CLOSE ALL
       RETURN NIL
*>>>>FIN IMPRESION DEL INFORME