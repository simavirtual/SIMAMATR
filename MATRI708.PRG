/*SISTEMA DE MATRICULA ACADEMICA

MODULO      : MODULO PRINCIPAL
SUBMODULO...: UTILIDADES                               MODULO No. 708

**************************************************************************
* TITULO..: CONFIGURACION DEL SISTEMA                                    *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: NOV 03/98 MAR A
       Colombia, Bucaramanga        INICIO: 11:00 AM   NOV 03/98 MAR

REQUERIMIENTOS:

1- Para ejecutar la funci�n tener presente la sintaxis

OBJETIVOS:

1- Visualiza un men� de utilidades varios

2- Ejecuta las diferentes funciones


SINTAXIS:

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Matri_708(aP1,aP2,aP3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aP1			      // Parametros Generales
       aP2			      // Parametros Generales
       aP3			      // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "ARC-MATR.PRG"      // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL cSavLin := ''                  // Salvar Linea
       LOCAL lHayErr    := .F.              // .T. Hay Error

       LOCAL lIntUno := .T.                 // Primera Integraci�n
       LOCAL lIntDos := .T.                 // Segunda Integraci�n
       LOCAL lIntTre := .F.                 // Tercera Integraci�n
       LOCAL lIntCua := .F.                 // Cuarta Integraci�n

       LOCAL aArcNtx := {}                  // Archivo de Indices
       LOCAL aArchvo := {}                  // Archivos para Uso
     *�Variables archivos

       LOCAL lPrnArc := .F.                 // .T. Imprimir a Archivo
       LOCAL nCodPrn := 0                   // C�digo de Impresi�n
       LOCAL cOpcPrn := ''                  // Opciones de Impresi�n.
       LOCAL cUsrIso := ''                  // Usuario del Iso
       LOCAL nCodXsl := 0                   // C�digo Xsl informes
       LOCAL cPiePag := ''                  // Pie de p�gina informes
       LOCAL aMezIso := {}                  // Campos a Mesclar
     *�Variables informes

       LOCAL nPrueba := 0                   // Habilitar pruebas
       LOCAL lModReg := .F.                 // .T. Modificar el Registro
       LOCAL lDelReg := .F.                 // .T. Borrar Registros
       LOCAL lInsReg := .F.                 // .T. Insertar Registro
       LOCAL lHaySql := .F.                 // .T. Exportar a Sql
       LOCAL bInsReg := NIL                 // Block Insertar registros
     *�Variables registros

       LOCAL nLenOpc := 0                   // Longitud de las opciones
       LOCAL nNroCol := 0                   // N�mero de Columna
       LOCAL nNroOpc := 1                   // Numero de la opcion
       LOCAL aMenus  := {}                  // Vector de declaracion de men�
       LOCAL aAyuda  := {}                  // Vector de ayudas para el men�
     *�Variables de menu

       LOCAL GetList := {}                  // Variable del Sistema
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DECLARACION PARAMETROS GENERALES
       LOCAL lShared := xPrm(aP1,'lShared') // .T. Sistema Compartido
       LOCAL nModCry := xPrm(aP1,'nModCry') // Modo de Protecci�n
       LOCAL cCodSui := xPrm(aP1,'cCodSui') // C�digo del Sistema
       LOCAL cNomSis := xPrm(aP1,'cNomSis') // Nombre del Sistema
     *�Detalles del Sistema

       LOCAL cEmpPal := xPrm(aP1,'cEmpPal') // Nombre de la Empresa principal
       LOCAL cNitEmp := xPrm(aP1,'cNitEmp') // Nit de la Empresa
       LOCAL cNomEmp := xPrm(aP1,'cNomEmp') // Nombre de la Empresa
       LOCAL cNomSec := xPrm(aP1,'cNomSec') // Nombre de la Empresa Secundario
       LOCAL cCodEmp := xPrm(aP1,'cCodEmp') // C�digo de la Empresa
     *�Detalles de la Empresa

       LOCAL cNomUsr := xPrm(aP1,'cNomUsr') // Nombre del Usuario
       LOCAL cAnoUsr := xPrm(aP1,'cAnoUsr') // A�o del usuario
       LOCAL cAnoSis := xPrm(aP1,'cAnoSis') // A�o del sistema
       LOCAL cPatSis := xPrm(aP1,'cPatSis') // Path del sistema
     *�Detalles del Usuario

       LOCAL PathW01 := xPrm(aP1,'PathW01') // Sitio del Sistema No.01
       LOCAL PathW02 := xPrm(aP1,'PathW02') // Sitio del Sistema No.02
       LOCAL PathW03 := xPrm(aP1,'PathW03') // Sitio del Sistema No.03
       LOCAL PathW04 := xPrm(aP1,'PathW04') // Sitio del Sistema No.04
       LOCAL PathW05 := xPrm(aP1,'PathW05') // Sitio del Sistema No.05
       LOCAL PathW06 := xPrm(aP1,'PathW06') // Sitio del Sistema No.06
       LOCAL PathW07 := xPrm(aP1,'PathW07') // Sitio del Sistema No.07
       LOCAL PathW08 := xPrm(aP1,'PathW08') // Sitio del Sistema No.08
       LOCAL PathW09 := xPrm(aP1,'PathW09') // Sitio del Sistema No.09
       LOCAL PathW10 := xPrm(aP1,'PathW10') // Sitio del Sistema No.10
     *�Sitios del Sistema

       LOCAL PathUno := xPrm(aP1,'PathUno') // Path de Integraci�n Uno
       LOCAL PathDos := xPrm(aP1,'PathDos') // Path de Integraci�n Dos
       LOCAL PathTre := xPrm(aP1,'PathTre') // Path de Integraci�n Tres
       LOCAL PathCua := xPrm(aP1,'PathCua') // Path de Integraci�n Cuatro
     *�Path de Integraci�n

       LOCAL nFilPal := xPrm(aP1,'nFilPal') // Fila Inferior Men� principal
       LOCAL nFilInf := xPrm(aP1,'nFilInf') // Fila Inferior del SubMen�
       LOCAL nColInf := xPrm(aP1,'nColInf') // Columna Inferior del SubMen�
     *�Detalles Tecnicos

       LOCAL cMaeAlu := xPrm(aP1,'cMaeAlu') // Maestros habilitados
       LOCAL cMaeAct := xPrm(aP1,'cMaeAct') // Maestro Activo
       LOCAL cJorTxt := xPrm(aP1,'cJorTxt') // Jornada escogida
     *�Detalles Acad�micos

       CloseAll()
*>>>>FIN DECLARACION PARAMETROS GENERALES

*>>>>AREAS DE TRABAJO
       aUseDbf := {}
       AADD(aUseDbf,{.T.,PathSis+'\'+FSimMtr,'MTR',NIL,lShared,nModCry})
*>>>>FIN AREAS DE TRABAJO

*>>>>SELECCION DE LAS AREAS DE TRABAJO
       IF !lUseDbfs(aUseDbf)
	  cError('ABRIENDO ARCHIVOS')
	  CloseAll(aUseDbf)
	  RETURN NIL
       ENDIF
*>>>>FIN SELECCION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       DO CASE
       CASE MTR->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DEL SISTEMA')

       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CloseAll(aUseDbf)
	  RETURN NIL
       ENDIF
       nPrueba := MTR->nPruebaMtr
       lPrnArc := MTR->lPrnArcMtr
       nCodPrn := MTR->nCodigoPrn
       cOpcPrn := MTR->cOpcPrnMtr
       cUsrIso := MTR->cUsrIsoMtr
       nCodXsl := MTR->nCodXslMtr
       cPiePag := MTR->cPiePagMtr
       CloseDbf('MTR')
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>DECLARCION Y EJECUCION DEL MENU
       cSavPan := SAVESCREEN(0,0,24,79)

       AADD(aMenus,'1<CONFIGURACION GENERAL>')
       AADD(aMenus,'2<CONFIGURACION SISTEMA>')
       AADD(aMenus,'3<CONFIGURACION A�O    >')
       AADD(aMenus,'4<CONFIGURACION COLOR  >')
       AADD(aMenus,'5<CONFIGURACION PRINTER>')
       AADD(aMenus,'6<INFORMES DEL SISTEMA >')
       AADD(aMenus,'7<INFORMES CONTROLADOS >')
       AADD(aMenus,'8<CONFI PRINTER RECIBOS>')

       AADD(aAyuda,'Consulta la Configuraci�n General del Sistema')
       AADD(aAyuda,'Consulta la Configuraci�n del Sistema')
       AADD(aAyuda,'Consulta la Configuraci�n para el A�o')
       AADD(aAyuda,'Consulta la Configuraci�n del Color')
       AADD(aAyuda,'Consulta la Configuraci�n de las Impresoras')
       AADD(aAyuda,'Consulta los informes del sistema')
       AADD(aAyuda,'Consulta los informes controlados')
       AADD(aAyuda,'Consulta la Configuraci�n de la Impresi�n de Recibos')
*>>>>FIN DECLARCION Y EJECUCION DEL MENU

*>>>>VALIDACION DEL LIMITE DE LA ULTIMA COLUMNA
       nLenOpc := LEN(aMenus[1])
      *Calculo de la Longitud mayor

       nNroCol := nColInf
       IF nColInf + nLenOpc + 2 > 78
	  nNroCol := 78-nLenOpc-3
       ENDIF
*>>>>FIN VALIDACION DEL LIMITE DE LA ULTIMA COLUMNA

*>>>>ANALISIS DE OPCION ESCOGIDA
       nNroOpc := 1
       DO WHILE nNroOpc # 0

	  nNroOpc := nMenu(aMenus,aAyuda,nFilInf-1,nNroCol,NIL,NIL,nNroOpc,.F.)

	  DO CASE
	  CASE nNroOpc == 0
	       EXIT

	  CASE nNroOpc == 1
	       ManSimCon(lShared,NIL,cNomSis,cEmpPal,cNitEmp,cNomEmp,;
			 NIL,NIL,nFilPal,nFilInf,nColInf,cNomUsr,;
			 cAnoUsr,cPatSis,PathUno,PathDos,PathTre,;
			 PathCua,cMaeAlu,cMaeAct,cJorTxt,NIL,.F.,.T.)
	      *Mantenimiento Configuraci�n del Sistema

	  CASE nNroOpc == 2
	       MantenMtr(lShared,cNomSis,cEmpPal,cNitEmp,cNomEmp,;
			 nFilInf,nColInf,nFilPal,cNomUsr,cAnoUsr,;
			 cPatSis,lIntUno,lIntDos,lIntTre,lIntCua,;
			 cMaeAlu,cJorTxt,cMaeAct)
	      *Mantenimiento Configuraci�n General

	  CASE nNroOpc == 3
	       MantenAno(lShared,cNomSis,cEmpPal,cNitEmp,cNomEmp,;
			 nFilInf,nColInf,nFilPal,cNomUsr,cAnoUsr,;
			 cPatSis,lIntUno,lIntDos,lIntTre,lIntCua,;
			 cMaeAlu,cJorTxt,cMaeAct)
	      *Mantenimiento Configuraci�n para el A�o

	  CASE nNroOpc == 4
	       MantenClr(lShared,nModCry,PathSis,FileClr)
	      *Configuraci�n del Color

	  CASE nNroOpc == 5
	       MantenPrn(lShared,nModCry,cNomSis,cNomEmp,;
			 cNomUsr,cAnoUsr,PathSis,FilePrn)
	      *Configuraci�n de la Impresora

	  CASE nNroOpc == 6

***************AREAS DE TRABAJO
		 aArchvo := {}
		 AADD(aArchvo,{PathSis+'\'+FilePrn,NIL,'PRN'})
		 AADD(aArchvo,{PathSis+'\'+FileInf,NIL,'INF'})
		 AADD(aArchvo,{PathSis+'\'+FileIso,NIL,'ISO'})
***************FIN AREAS DE TRABAJO

***************DETALLES DEL INFORME
		 aP3 := {}
		 AADD(aP3,{'cPieTre',cPiePag})
		 AADD(aP3,{'cUsrIso',cUsrIso})
		 AADD(aP3,{'nCodXsl',nCodXsl})
***************FIN DETALLES DEL INFORME

***************MANTENIMIENTO DEL ARCHIVO
		  lModReg := .F.
		  lDelReg := .F.
		  lInsReg := .F.
		  lHaySql := .F.
		  bInsReg := {||MtrInfSys(lShared,cNomUsr,cCodSui)}

		  MantenInf(aP1,aP2,aP3,aArchvo,cOpcPrn,;
			    nCodPrn,lModReg,lDelReg,;
			    lInsReg,lHaySql,bInsReg)
***************FIN MANTENIMIENTO DEL ARCHIVO

	  CASE nNroOpc == 7

***************AREAS DE TRABAJO
		 aArchvo := {}
		 AADD(aArchvo,{PathSis+'\'+FilePrn,NIL,'PRN'})
		 AADD(aArchvo,{PathSis+'\'+FileIso,NIL,'ISO'})
		 AADD(aArchvo,{PathSis+'\'+FileInf,NIL,'INF'})
***************FIN AREAS DE TRABAJO

***************DETALLES DEL INFORME
		 aP3 := {}
		 AADD(aP3,{'cPieTre',cPiePag})
		 AADD(aP3,{'cUsrIso',cUsrIso})
		 AADD(aP3,{'nCodXsl',nCodXsl})
***************FIN DETALLES DEL INFORME

***************MANTENIMIENTO DEL ARCHIVO
		  lModReg := .F.
		  lDelReg := .F.
		  lInsReg := .F.
		  lHaySql := .F.
		  bInsReg := NIL

		  MantenIso(aP1,aP2,aP3,;
			    aArchvo,cOpcPrn,nCodPrn,lModReg,;
			    lDelReg,lInsReg,lHaySql,bInsReg)
***************FIN MANTENIMIENTO DEL ARCHIVO


	  CASE nNroOpc == 8

***************AREAS DE TRABAJO
		 aArchvo := {}
		 AADD(aArchvo,{PathSis+'\'+FilePrn,NIL,'PRN'})
		 AADD(aArchvo,{PathSis+'\'+FileMzp,NIL,'MZP'})
		 AADD(aArchvo,{PathSis+'\'+FileInf,NIL,'INF'})
***************FIN AREAS DE TRABAJO

***************MANTENIMIENTO DEL ARCHIVO
		  lModReg := .T.
		  lDelReg := .F.
		  lInsReg := .F.
		  lHaySql := .F.

		 MantenMzp(lShared,nModCry,cNomSis,cCodEmp,cNitEmp,cEmpPal,;
			   cNomEmp,cNomSec,cNomUsr,cAnoUsr,aArchvo,lPrnArc,;
			   cOpcPrn,nCodPrn,lModReg,lDelReg,lInsReg,lHaySql)
***************FIN MANTENIMIENTO DEL ARCHIVO

	  ENDCASE

       ENDDO
       CloseAll()
       RETURN NIL
*>>>>FIN ANALISIS DE OPCION ESCOGIDA