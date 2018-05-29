#' Class "ref_taxa"
#' 
#' Loading and selection of fish species. This class is a referential class, and it is 
#' integrated into refreport objects.
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_taxa", ...)}.   
#' @slot data A \code{"data.frame"} of species available in the database
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family referential objects
setClass(Class="ref_taxa",representation= representation(data="data.frame" ))


#' Loading method for ref_taxa referential objects
#' 
#' @return An S4 object of class ref_taxa
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{ref_taxa-class}
#' @examples \dontrun{
#'  object=new("ref_taxa")
#'  charge(object)}
setMethod("charge",signature=signature("ref_taxa"),definition=function(object) {
	  req=new("RequeteODBC") 
	  req@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  req@sql="SELECT tax_code, tax_nom_latin, tax_nom_commun, tax_ntx_code, tax_tax_code FROM ref.tr_taxon_tax  ORDER BY tax_rang ASC ;"
	  req<-stacomirtools::connect(req)  # appel de la methode connect de l'object requeteODBC
	  object@data<-req@query
	  return(object)
	})

#' Loading method for ref_taxa referential objects searching only taxa existing for a DC
#' @param object An object of class \link{ref_taxa-class}
#' @param dc_selectionne A counting device selected, only taxa attached to this dc are selected
#' @return An S4 object of class ref_taxa
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @examples \dontrun{
#'  dc_selectionne=6
#'  object=new("ref_taxa")
#'  charge_with_filter(object,dc_selectionne=dc_selectionne)}
setMethod("charge_with_filter",signature=signature("ref_taxa"),definition=function(object,dc_selectionne) {
	  requete=new("RequeteODBCwhere")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@select=paste("SELECT DISTINCT ON (tax_rang) tax_code, tax_nom_latin, tax_nom_commun, tax_ntx_code, tax_tax_code", 
		  " FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
		  " JOIN ref.tr_taxon_tax on lot_tax_code=tax_code",sep="")
	  requete@where=paste("where dis_identifiant in",vector_to_listsql(dc_selectionne))
	  requete@order_by="ORDER BY tax_rang ASC"  
	  requete<-stacomirtools::connect(requete)  
	  object@data<-requete@query
	  return(object)
	})


#' Choice method for reftaxa referential objects with only one taxa selected
#' @param object An object of class \link{ref_taxa-class}
#' @param objectreport An object report which includes the \link{ref_taxa-class}, default NULL
#' @param is.enabled Sets if the frame is enabled at launch, defaut TRUE
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
#' @examples  \dontrun{
#'  object=new("ref_taxa")
#' win=gwindow()
#' group<-ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' report_mig<-new(report_mig)
#' choice(object,objectreport=report_mig)
#' }
setMethod("choice",signature=signature("ref_taxa"),
	definition=function(
		object,
		objectreport=NULL,
		is.enabled=TRUE) {
	  if (nrow(object@data) > 0){
		htax=function(h,...){
		  taxa=svalue(choice)
		  object@data<-object@data[tax_libelle%in%taxa ,]
		  assign("ref_taxa",object,envir_stacomi)
		  funout(gettext("Taxon selected\n",domain="R-stacomiR"))
		  if (!is.null(objectreport)) {
			objectreport@stage<<-charge_with_filter(object=objectreport@stage,dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne,taxa_selectionne=get("ref_taxa",envir_stacomi)@data$tax_code)
			if (exists("frame_std",envir_stacomi)){
			  delete(group,get("frame_std",envir_stacomi))
			}
			if (exists("frame_par",envir_stacomi)) {
			  delete(group,get("frame_par",envir_stacomi))
			}
			if (exists("frame_parquan",envir_stacomi)) {
			  delete(group,get("frame_parquan",envir_stacomi))
			}
			if (exists("frame_parqual",envir_stacomi)) {
			  delete(group,get("frame_parqual",envir_stacomi))
			}
			choice(objectreport@stage,objectreport,is.enabled=TRUE)						
		  }
		}
		group<-get("group",envir=envir_stacomi)
		frame_tax<-gframe(gettext("Taxon selection",domain="R-stacomiR"))
		# assignment in envir_stacomi to get it back and delete it when previous
		# object (dc) is selected
		assign("frame_tax",frame_tax,envir_stacomi)
		add(group,frame_tax)
		tax_libelle=fun_char_spe(object@data$tax_nom_latin)
		choice=gdroplist(tax_libelle,container=frame_tax,handler=htax)
		enabled(frame_tax)<-is.enabled
		gbutton("OK", container=frame_tax,handler=htax)
	  } else funout(gettext("Stop there is no line in the taxa table (problem with the ODBC link ?)\n",domain="R-stacomiR"),arret=TRUE)
	})

#' Multiple Choice method for reftaxa referential objects, the graphical interface is built to allow
#' for multiple choices. See load for method in the command line.
#' @param object An object of class \link{ref_taxa-class}
#' @param objectreport An object report which includes the \link{ref_taxa-class}, default NULL
#' @param is.enabled Sets if the frame is enabled at launch, defaut TRUE
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{ 
#'  object=new("ref_taxa")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' report_mig=new(report_mig)
#' choicemult(object,objectreport=report_mig)
#' }
setMethod("choicemult",signature=signature("ref_taxa"),definition=function(object,objectreport=NULL,is.enabled=TRUE) {
	  if (nrow(object@data) > 0){
		htax=function(h,...){
		  taxa=tbdesttaxa[,][tbdesttaxa[,]!=""]
		  if (length(taxa)>0){
			object@data<-object@data[tax_libelle%in%taxa ,]
			assign("ref_taxa",object,envir_stacomi)
			funout(gettext("Taxa selected\n",domain="R-stacomiR"))
			if (!is.null(objectreport)) {
			  objectreport@taxa<-object
			  objectreport@stage<-charge_with_filter(object=objectreport@stage,
				  dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne,
				  taxa_selectionne=get("ref_taxa",envir_stacomi)@data$tax_code
			  )
			  assign(get("objectreport",envir=envir_stacomi),objectreport,envir=envir_stacomi)
			  # suppresses all tab larger than 3 (taxa)
			  # suppresses all tab larger than (dc)
			  currenttab<-svalue(notebook)
			  if (length(notebook)>currenttab){
				for (i in length(notebook):(currenttab+1)){
				  svalue(notebook) <- i							
				  dispose(notebook) ## dispose current tab
				}}
			  choicemult(objectreport@stage,objectreport,is.enabled=TRUE)						
			}
			# changing tab of notebook to next tab
			if (svalue(notebook)<length(notebook)){
			  svalue(notebook)<-svalue(notebook)+1	
			}
		  } else {
			funout(gettext("No taxa selected\n",domain="R-stacomiR"))					
		  }				
		}
		# below the widget structure [=> within (=> type
		# group(ggroup)[notebook(notebook)[grouptaxa(ggroup&tab)[[frametaxaource(gframe)[tbsourcetaxa(gtable)],frametaxadest(gframe)[tbdtaxadest(gtable)]],OKbutton]]
		if (!exists("notebook",envir_stacomi)){ 
		  group<-get("group",envir_stacomi)
		  notebook <- gnotebook(container=group)} 
		else {
		  notebook<-get("notebook",envir=envir_stacomi)
		}			
		tax_libelle=fun_char_spe(object@data$tax_nom_latin)
		grouptaxa<-ggroup() 
		assign("grouptaxa",grouptaxa,envir_stacomi)
		add(notebook,grouptaxa,label="taxa")
		frametaxaource<-gframe(gettext("Taxon selection",domain="R-stacomiR"),container=grouptaxa)
		tbsourcetaxa  = gtable(tax_libelle,container=frametaxaource,expand = TRUE, fill = TRUE)
		size(tbsourcetaxa)<-c(160,300) # les dimensions sont testees a la main 
		# pour s'ajuster aux dimensions du notebook (largeur 400)
		frametaxadest<-gframe(gettext("drop here",domain="R-stacomiR"),container=grouptaxa)
		# need for a fixed size data.frame otherwise errors when adding new lines
		xx<-data.frame(choice=rep("",8))
		xx$choice<-as.character(xx$choice)
		tbdesttaxa=gtable(xx,container=frametaxadest,expand = TRUE, fill = TRUE)
		size(tbdesttaxa)<-c(160,300)
		adddropsource(tbsourcetaxa)
		adddroptarget(tbdesttaxa)				
		adddropmotion(tbdesttaxa,handler=function(h,...) {
			  valeurs<-tbdesttaxa[,]
			  valeurs<-valeurs[valeurs!=""]
			  if (!svalue(tbsourcetaxa)%in%valeurs){
				tbdesttaxa[length(valeurs)+1,1]<-svalue(tbsourcetaxa)
			  }
			})
		addHandlerDoubleclick(tbsourcetaxa,handler=function(h,...) {
			  valeurs<-tbdesttaxa[,]
			  valeurs<-valeurs[valeurs!=""]
			  if (!svalue(tbsourcetaxa)%in%valeurs){
				tbdesttaxa[length(valeurs)+1,1]<-svalue(h$obj)
			  }
			})
		adddropsource(tbdesttaxa)
		adddroptarget(tbsourcetaxa)
		removetaxa<-function(){
		  valeurs<-tbdesttaxa[,]
		  valeurs<-valeurs[valeurs!=""]
		  valeurs<-valeurs[-match(svalue(tbdesttaxa),valeurs)]
		  tbdesttaxa[,]<-c(valeurs,rep("",8-length(valeurs)))
		}
		adddropmotion(tbsourcetaxa,handler=function(h,...) {
			  removetaxa()
			})
		addHandlerDoubleclick(tbdesttaxa,handler=function(h,...) {
			  removetaxa()
			})
		gbutton("OK", container = grouptaxa, handler = htax)
	  } else {
		funout(gettext("Stop there is no line in the taxa table (problem with the ODBC link ?)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	})


#' choice_c method for ref_taxa
#' 
#' the choice_cc method is intendedto have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. The values passed to the choice_c method
#' for taxa can be either numeric (2038 = Anguilla anguilla) or character.  
#' @param object An object from the class ref_taxa
#' @param taxa The vector of taxa, can be either code (numeric) or latin name
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#' object=new("ref_taxa")
#' object<-charge(object)
#' objectreport=new("report_mig_mult")
#' choice_c(object=object,objectreport=objectreport,"Anguilla anguilla")
#' }
setMethod("choice_c",signature=signature("ref_taxa"),definition=function(object,taxa) {
	  if (is.null(taxa)) {
		funout(gettext("No value for argument taxa\n",domain="R-stacomiR"),arret=TRUE)
	  } else	if (class(taxa)=="character"){	
		libellemanquants<-taxa[!taxa%in%object@data$tax_nom_latin]
		if (length(libellemanquants)>0) warning(gettextf("Taxa not present :\n %s",stringr::str_c(libellemanquants,collapse=", "),domain="R-stacomiR"))
		object@data<-object@data[object@data$tax_nom_latin%in%taxa,]
	  } else if (class(taxa)=="numeric"){
		codemanquants<-taxa[!taxa%in%object@data$tax_code]
		if (length(codemanquants)>0) warning(gettextf("Taxa not present :\n %s",stringr::str_c(codemanquants,collapse=", "),domain="R-stacomiR"))
		object@data<-object@data[object@data$tax_code%in%taxa,]
	  }
	  if (nrow(object@data)==0 )	{
		funout(gettext("Stop there is no line in the taxa table (problem with the ODBC link ?)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  assign("ref_taxa",object,envir=envir_stacomi)
	  return(object)
	})
