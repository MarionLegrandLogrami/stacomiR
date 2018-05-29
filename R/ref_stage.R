# Nom fichier :        ref_stage   (classe)

#' Class "ref_stage"
#' 
#' Representation of a fish phase
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_stage", data="data.frame")}.  \describe{
#' \item{list("data")}{Object of class \code{"data.frame"} ~ The phases
#' available in the database}\item{:}{Object of class \code{"data.frame"} ~ The
#' phases available in the database} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
#' @family referential objects
setClass(Class="ref_stage",representation=representation(data="data.frame") )

#' Loading method for ref_stage referential objects
#' @param object An object of class \link{ref_stage-class}
#' @return An S4 object of class ref_stage
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_stage")
#'  charge(object)
#' }
setMethod("charge",signature=signature("ref_stage"),definition=function(object) {
	  req=new("RequeteODBC") 
	  req@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  req@sql="SELECT std_code, std_libelle FROM ref.tr_stadedeveloppement_std ORDER BY std_code ;"
	  req<-stacomirtools::connect(req)  # appel de la methode connect de l'object requeteODBC
	  object@data<-req@query
	  return(object)
	})
#' Loading method for ref_stage referential objects searching only those stages existing for a DC and a Taxon
#' @param object An object of class \link{ref_stage-class}
#' @param dc_selectionne The selected counting device
#' @param taxa_selectionne The selected species

#' @return An S4 object of class \link{ref_stage-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selectionne=6
#'	taxa_selectionne=2038
#'  object=new("ref_stage")
#'  charge_with_filter(object,dc_selectionne,taxa_selectionne)
#' }
setMethod("charge_with_filter",signature=signature("ref_stage"),definition=function(object,dc_selectionne,taxa_selectionne) {
	  requete=new("RequeteODBCwhere")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@select=paste("SELECT DISTINCT ON (std_code) std_code, std_libelle", 
		  " FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
		  " JOIN ref.tr_stadedeveloppement_std on lot_std_code=std_code",sep="")
	  requete@where=paste("where dis_identifiant in ",vector_to_listsql(dc_selectionne),sep="")
	  requete@and=paste("and lot_tax_code in ",vector_to_listsql(taxa_selectionne),sep="")
	  requete@order_by="ORDER BY std_code"  
	  requete<-stacomirtools::connect(requete)  # appel de la methode connect de l'object requeteODBC
	  object@data<-requete@query
	  if (nrow(object@data)==0) funout(gettext("No data for this counting device and this taxa\n",domain="R-stacomiR"),arret=TRUE)
	  return(object)
	})
#' Choice method for ref_stage referential objects
#' @param object An object of class \link{ref_stage-class}
#' @param objectreport An object report which includes the \link{ref_stage-class}, default NULL
#' @param is.enabled Sets if the frame is enabled at launch, defaut TRUE
#' @note the method tests if the load is called from within a "report" object, and loads par, parqual, or parquan objects accordingly
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_stage")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' report_migPar=new(report_migPar)
#' objectreport=report_taille # for another test
#' choice(object,objectreport=report_migPar)
#' }
#' @keywords internal
setMethod("choice",signature=signature("ref_stage"),definition=function(object,objectreport=NULL,is.enabled=TRUE) {
	  if (nrow(object@data) > 0){
		hstd=function(h,...){
		  stage=svalue(choice)
		  object@data<-object@data[std_libelle%in%stage ,]
		  assign("ref_stage",object,envir_stacomi)
		  funout(gettext("Stage selected\n",domain="R-stacomiR"))
		  if (!is.null(objectreport)) {
			# par defaut la methode ne charge pas de maniere interactive  (par exemple en ne premnant que les stage des taxa du dc par la methode charge_with_filter
			# elle est alors affichee des le debut par la methode choice e laquelle on ne passe pas d'objectreport en parametre 
			#il y a bien un object par dans l'object report             
			if (class(try(objectreport@par,silent=TRUE))!="try-error") {
			  objectreport@par<<-charge_with_filter(object=objectreport@par,
				  dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne,
				  taxa_selectionne=get("ref_taxa",envir_stacomi)@data$tax_code,
				  stage_selectionne=get("ref_stage",envir_stacomi)@data$std_code)
			  
			  if (exists("frame_par",envir_stacomi)){
				delete(group,get("frame_par",envir_stacomi))
			  } 
			  choice(objectreport@par,is.enabled=TRUE)
			} 
			#il y a bien un object parqual dans l'object report						
			if (class(try(objectreport@parqual,silent=TRUE))!="try-error") {
			  objectreport@parqual<<-charge_with_filter(object=objectreport@parqual,
				  dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne,
				  taxa_selectionne=get("ref_taxa",envir_stacomi)@data$tax_code,
				  stage_selectionne=get("ref_stage",envir_stacomi)@data$std_code)
			  
			  if (exists("frame_parqual",envir_stacomi)) {
				delete(group,get("frame_parqual",envir_stacomi))
			  }
			  choice(objectreport@parqual,label=gettext("Qualitative feature",domain="R-stacomiR"),
				  nomassign="ref_parqual",
				  frameassign="frame_parqual",is.enabled=TRUE)
			}
#il y a bien un object parquan dans l'object report
			if (class(try(objectreport@parquan,silent=TRUE))!="try-error") {
			  objectreport@parquan<<-charge_with_filter(object=objectreport@parquan,
				  dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne,
				  taxa_selectionne=get("ref_taxa",envir_stacomi)@data$tax_code,
				  stage_selectionne=get("ref_stage",envir_stacomi)@data$std_code)
			  if (class(objectreport)=="report_taille" )
			  {
				if (nrow(objectreport@parquan@data)>0) {
				  objectreport@parquan@data=objectreport@parquan@data[objectreport@parquan@data$par_code=="1786"|  #taille
						  objectreport@parquan@data$par_code=="1785"|     # taille fourche
						  is.na(objectreport@parquan@data$par_code)|objectreport@parquan@data$par_code=="C001",]     # aucune
				}
			  }
			  if (exists("frame_parquan",envir_stacomi)){
				delete(group,get("frame_parquan",envir_stacomi))
				
			  } 
			  choice(objectreport@parquan,label=gettext("Quantitative feature",domain="R-stacomiR"),
				  nomassign="ref_parquan",
				  frameassign="frame_parquan",is.enabled=TRUE)
			}
			
		  }
		}
		group<-get("group",envir=envir_stacomi)
		frame_std<-gframe(gettext("Stage selection",domain="R-stacomiR"))
		assign("frame_std",frame_std,envir_stacomi)
		add(group,frame_std)
		std_libelle=fun_char_spe(object@data$std_libelle)
		choice=gcombobox(std_libelle,container=frame_std,handler=hstd)
		enabled(frame_std)<-is.enabled
		gbutton("OK", container=frame_std,handler=hstd)
	  } else funout(gettext("Stop internal error : load data to make a choice\n",domain="R-stacomiR"),arret=TRUE)
	})

#' Multiple Choice method for ref_stage referential objects
#' 
#' @param object An object of class \link{ref_stage-class}
#' @param objectreport An object report which includes the \link{ref_stage-class}, default NULL
#' @param is.enabled Sets if the frame is enabled at launch, defaut TRUE

#' @note the method tests if the load is called from within a "report" object, and loads par, parqual, or parquan objects accordingly
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_stage")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' report_migPar=new(report_migPar)
#' objectreport=report_taille # for other test
#' choicemult(object,objectreport=report_migPar)	
#' }
#' @keywords internal
setMethod("choicemult",signature=signature("ref_stage"),definition=function(object,objectreport=NULL,is.enabled=TRUE) {
	  
	  if (nrow(object@data) > 0){
		hstd=function(h,...){
		  stage=tbdeststd[,][tbdeststd[,]!=""]
		  if (length(stage)>0){
			object@data<-object@data[std_libelle%in%stage ,]
			assign("ref_stage",object,envir_stacomi)
			funout(gettext("Stage selected\n",domain="R-stacomiR"))
			if (!is.null(objectreport)) {
			  objectreport@stage<-object
			  assign(get("objectreport",envir=envir_stacomi),objectreport,envir=envir_stacomi)
			  
			  # suppresses all tab larger than current tab
			  stagetab<-svalue(notebook)
			  if (length(notebook)>stagetab){
				for (i in length(notebook):(stagetab+1)){
				  svalue(notebook) <- i							
				  dispose(notebook) ## dispose current tab
				}}
			  # if the class is loaded from with a reportObject
			  # and there is no parquan inside
			  # then par is loaded, instead parquan and parqual are loaded
			  # as now report_mig_char inherits from report_sample_char and thus possesses a
			  # ref_par class
			  if (class(try(objectreport@par,silent=TRUE))!="try-error"&
				  class(try(objectreport@parqual,silent=TRUE))=="try-error") {
				objectreport@par<-charge_with_filter(object=objectreport@par,
					dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne,
					taxa_selectionne=get("ref_taxa",envir_stacomi)@data$tax_code,
					stage_selectionne=get("ref_stage",envir_stacomi)@data$std_code)
				choicemult(objectreport@par,nomassign="ref_par",
					objectreport,
					label=gettext("Parm.",domain="R-stacomiR"))
			  } 
			  # there is an object parqual with the report Object						
			  if (class(try(objectreport@parqual,silent=TRUE))!="try-error") {
				objectreport@parqual<-charge_with_filter(object=objectreport@parqual,
					dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne,
					taxa_selectionne=get("ref_taxa",envir_stacomi)@data$tax_code,
					stage_selectionne=get("ref_stage",envir_stacomi)@data$std_code)
				choicemult(objectreport@parqual,
					objectreport,
					nomassign="ref_parqual",
					label=gettext("Qualit. parm.",domain="R-stacomiR")
				)
			  }
			  # there is an object parquan with the report Object	
			  if (class(try(objectreport@parquan,silent=TRUE))!="try-error") {
				objectreport@parquan<-charge_with_filter(object=objectreport@parquan,
					dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne,
					taxa_selectionne=get("ref_taxa",envir_stacomi)@data$tax_code,
					stage_selectionne=get("ref_stage",envir_stacomi)@data$std_code)
				if (class(objectreport)=="report_taille" )
				{
				  if (nrow(objectreport@parquan@data)>0) {
					objectreport@parquan@data=objectreport@parquan@data[objectreport@parquan@data$par_code=="1786"|  #taille
							objectreport@parquan@data$par_code=="1785"|     # taille fourche
							is.na(objectreport@parquan@data$par_code)|objectreport@parquan@data$par_code=="C001",]     # aucune
				  }
				}
				choicemult(objectreport@parquan,
					objectreport,
					nomassign="ref_parquan",
					label=gettext("Quant. parm.",domain="R-stacomiR"))
			  }
			  svalue(notebook)<-stagetab+1
			}
		  } else {
			funout(gettext("No Stage selected\n",domain="R-stacomiR"))
		  }
		}
		# below the widget structure [=> within (=> type
		# group(ggroup)[notebook(notebook)[groupstd(ggroup&tab)[[framestdsource(gframe)[tbsourcestd(gtable)],framestddest(gframe)[tbdeststd(gtable)]],OKbutton]]
		
		if (!exists("notebook",envir=envir_stacomi)) {
		  group<-get("group",envir_stacomi)
		  notebook <- gnotebook(container=group)
		} else {
		  notebook<-get("notebook",envir=envir_stacomi)	
		}
		std_libelle=fun_char_spe(object@data$std_libelle)
		groupstd<-ggroup() 
		assign("goupstd",groupstd,envir=envir_stacomi)
		add(notebook,groupstd,label="stage")
		framestdsource<-gframe(gettext("Stage selection",domain="R-stacomiR"),container=groupstd)
		tbsourcestd  = gtable(std_libelle,container=framestdsource,expand = TRUE, fill = TRUE)
		size(tbsourcestd)<-c(160,300) 
		framestddest<-gframe(gettext("drop here",domain="R-stacomiR"),container=groupstd)
		# need for a fixed size data.frame otherwise errors when adding new lines
		xx<-data.frame(choice=rep("",8))
		xx$choice<-as.character(xx$choice)
		tbdeststd=gtable(xx,container=framestddest,expand = TRUE, fill = TRUE)
		size(tbdeststd)<-c(160,300)
		adddropsource(tbsourcestd)
		adddroptarget(tbdeststd)				
		adddropmotion(tbdeststd,handler=function(h,...) {
			  valeurs<-tbdeststd[,]
			  valeurs<-valeurs[valeurs!=""]
			  if (!svalue(tbsourcestd)%in%valeurs){
				tbdeststd[length(valeurs)+1,1]<-svalue(tbsourcestd)
			  }
			})
		addHandlerDoubleclick(tbsourcestd,handler=function(h,...) {
			  valeurs<-tbdeststd[,]
			  valeurs<-valeurs[valeurs!=""]
			  if (!svalue(tbsourcestd)%in%valeurs){
				tbdeststd[length(valeurs)+1,1]<-svalue(h$obj)
			  }
			})
		adddropsource(tbdeststd)
		adddroptarget(tbsourcestd)
		removestd<-function(){
		  valeurs<-tbdeststd[,]
		  valeurs<-valeurs[valeurs!=""]
		  valeurs<-valeurs[-match(svalue(tbdeststd),valeurs)]
		  tbdeststd[,]<-c(valeurs,rep("",8-length(valeurs)))
		}
		adddropmotion(tbsourcestd,handler=function(h,...) {
			  removestd()
			})
		addHandlerDoubleclick(tbdeststd,handler=function(h,...) {
			  removestd()
			})
		gbutton("OK", container = groupstd, handler = hstd)
	  } else {
		funout(gettext("Error : no stages in the database (the query returns 0 entry)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	})

#' choice_c method for ref_stage
#' 
#' the choice_c method is intendedto have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. The values passed to the choice_c method
#' for stage is the code.  Any numeric value will be discarded
#' @param object An object of class \link{ref_stage-class}
#' @param stage the vector of stages chosen
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#'object=new("ref_taxa")
#'object<-charge(object)
#'objectreport=new("report_mig_mult")
#' choice_c(object=object,objectreport=objectreport,"Anguilla anguilla")
#' }

setMethod("choice_c",signature=signature("ref_stage"),definition=function(object,stage,silent=FALSE) {
	  if (is.null(stage)) {
		funout(gettext("No value for argument stage\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  libellemanquants<-stage[!stage%in%object@data$std_code]
	  if (length(libellemanquants)>0&!silent) funout(gettextf("No data for this counting device and this taxa\n %s",stringr::str_c(libellemanquants,collapse=", "),domain="R-stacomiR"))
	  object@data<-object@data[object@data$std_code%in%stage,]					
	  if (nrow(object@data)==0 )	{
		funout(gettext("Stop there is no line in the taxa table (problem with the ODBC link ?)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  assign("ref_stage",object,envir=envir_stacomi)
	  return(object)
	})
