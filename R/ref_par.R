# Nom fichier :        refPar  (classe)
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :   22/03/2009 21:14:14

#TODO  selection de plusieurs caracteristiques
#' Class "ref_par"
#' 
#' Class enabling to load the list of parameters and select one of them
#' @include create_generic.R
#' @slot data A data.frame
#' @slot par_selected A character vector corresponding to par_code
#' @section Objects from the Class: Objects can be created by calls of the form
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
#' @slot data="data.frame" the list of parameters
#' @family referential objects
setClass(Class="ref_par",representation= representation(data="data.frame",par_selected="character"))


setValidity("ref_par",method=function(object){
	  if (length(object@par_selected)!=0){		
		if (nrow(object@data)>0) {
		  concord<-object@par_selected%in%object@data$par_code					
		  if (any(!concord)){
			return(paste("No data for par",object@par_selected[!concord]))
			
		  } else {
			return(TRUE)
		  }
		} else {
		  return("You tried to set a value for par_selected without initializing the data slot")
		}
	  }  else return(TRUE)
	  
	}   
)
#' Loading method for Repar referential objects
#' @param object An object of class \link{ref_par-class}
#' @return An S4 object of class ref_par
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_par")
#' charge(object)
#' }
setMethod("charge",signature=signature("ref_par"),definition=function(object) {
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@sql= paste("SELECT par_code, par_nom  from ref.tg_parametre_par")
	  requete<-stacomirtools::connect(requete)
	  funout(gettext("Loading parameters query completed\n",domain="R-stacomiR"))
	  object@data<-requete@query
	  return(object)
	})
#' Loading method for Repar referential objects searching only those parameters existing for a DC, a Taxa, and a stage
#' @param object An object of class \link{ref_par-class}
#' @param dc_selectionne A counting device selected for the report 
#' @param taxa_selectionne The taxa selected for the report
#' @param stage_selectionne The stage selected for the report
#' @return An S4 object of class ref_par
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_par")
#' charge_with_filter(object,dc_selectionne=6,taxa_selectionne=2038,stage_selectionne=c("AGJ","CIV")
#' }
setMethod("charge_with_filter",signature=signature("ref_par"),definition=function(object,dc_selectionne,
		taxa_selectionne,
		stage_selectionne) {
	  requete=new("RequeteODBCwhere")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@select=paste("SELECT DISTINCT ON (par_code) par_code, par_nom", 
		  " FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
		  " JOIN ref.tg_parametre_par on par_code=car_par_code",sep="")
	  requete@where=paste("where dis_identifiant in ",vector_to_listsql(dc_selectionne))
	  requete@and=paste("and lot_tax_code in",vector_to_listsql(taxa_selectionne),
		  " and lot_std_code in ",vector_to_listsql(stage_selectionne),sep="")
	  requete@order_by="ORDER BY par_code"  
	  requete<-stacomirtools::connect(requete)  # appel de la methode connect de l'object requeteODBC
	  object@data<-requete@query
	  if (nrow(object@data)==0) funout(gettext("No data for selected device, taxa and stage\n",domain="R-stacomiR"),arret=TRUE)
	  return(object)
	})
#' Choice method for ref_par referential objects
#' @param object An object of class \link{ref_par-class}
#' @param label The label that will be displayed in the message frame or as output text
#' @param nomassign The assignment name in envir_stacomi
#' @param frameassign The name of the frame used for assignment in envir_stacomi
#' @param is.enabled Default TRUE.
#' @note the choice method assigns an object of class ref_par named ref_par in the environment envir_stacomi
#' @note this method choice is also on daughter classes ref_parquan, hence the parameters, however it was redefined in ref_parqual
#' @note to load the possible values of qualitative parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#'  object=new("ref_par")
#'  win=gwindow()
#'  group=ggroup(container=win,horizontal=FALSE)
#'  object<-charge(object)
#'  choice(object)
#' }
#' @keywords internal
setMethod("choice",signature=signature("ref_par"),definition=function(object,
		label="Choix d'une caracteristique de lot",
		nomassign="ref_par",
		frameassign="frame_par",
		is.enabled=TRUE) {
	  if (nrow(object@data) > 0){
		hcar=function(h,...){
		  carchoisi=svalue(choice)
		  object@par_selected<-object@data[car_libelle%in%carchoisi ,"par_code"]
		  #object@data<-object@data[car_libelle%in%carchoisi ,"par_code"]
		  assign(nomassign,object,envir_stacomi)
		  funout(gettext("Feature has been selected\n",domain="R-stacomiR"))
		}
		#frame_par<<-gframe(label)
		assign(frameassign,gframe(label,horizontal=FALSE),envir= envir_stacomi)
		# pour pouvoir la supprimer ensuite
		group<-get("group",envir=envir_stacomi)
		add(group,get(eval(frameassign),envir= envir_stacomi))
		car_libelle=fun_char_spe(object@data$par_nom)
		car_libelle[nchar(car_libelle)>30]<-paste(substr(car_libelle[nchar(car_libelle)>30],1,30),".",sep="")
		choice=gdroplist(items=car_libelle,container=get(eval(frameassign),envir= envir_stacomi),handler=hcar)
		gbutton("OK", container=get(eval(frameassign),envir= envir_stacomi),handler=hcar)
	  } else funout(gettext("Internal error : unable to load any feature to make the choice\n",domain="R-stacomiR"),arret=TRUE)
	})


#' Command line interface to select a parameter
#' 
#' the choice_c method is intendedto have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. 
#' If an objectreport is passed as a parameter, the method will do a charge_with_filter to select only the taxa present in the counting devices
#' @param object an object of class  \link{ref_par-class}
#' @param par A character vector of par
#' @param silent Default FALSE but not used there
#' @return An object of class \link{ref_par-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("choice_c",signature=signature("ref_par"),definition=function(object,par,silent=FALSE) {
	  if (class(par)=="numeric") {
		par<-as.character(par) 
	  }
	  if (any(is.na(par))) stop ("NA values par")			
	  object@par_selected<-par				
	  if (nrow(object@data)==0){
		stop ("Internal error : tried to set a value for par_selected without initializing the data slot")
	  }
	  #validObject(object,test=FALSE) 
	  #here I don't want to generate an error if parm is not present
	  #so I'm not using the validObject which will throw and error
	  concord<-object@par_selected%in%object@data$par_code	
	  
	  if (any(!concord)){
		warning(paste(gettextf("No data for par %s",object@par_selected[!concord],domain="R-stacomiR")))
	  }
	  
	  assign("ref_par",object,envir=envir_stacomi)
	  return(object)
	})


#' Multiple Choice method for ref_par referential objects, internal use
#' 
#' @param object An object of class \link{ref_par-class}
#' @param objectreport An object report which includes the \link{ref_par-class}, default NULL
#' @param label The name for the frame
#' @param nomassign The name by which the widget will be assigned in envir_stacomi 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_par")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' report_mig_char=new("report_mig_char")
#' objectreport=report_taille # for other test
#' choicemult(object,objectreport=report_mig_char)	
#' }
#' @keywords internal
setMethod("choicemult",signature=signature("ref_par"),definition=function(object,
		objectreport=NULL,
		label=gettext("Sample characteristic",domain="R-stacomiR"),
		nomassign="ref_par") {
	  
	  if (nrow(object@data) > 0){
		hpar=function(h,...){
		  parm=tbdestpar[,][tbdestpar[,]!=""]
		  object@par_selected<-object@data[car_libelle%in%parm,"par_code"]
		  assign(nomassign,object,envir_stacomi)
		  funout(gettext("Parameter selected\n",domain="R-stacomiR"))
		  if (!is.null(objectreport)) {
			# the method can be used for parquan or par
			# so I test whether the object contains a class parquan
			if (class(try(objectreport@parquan,silent=TRUE))!="try-error") {
			  objectreport@parquan<-object
			  assign(get("objectreport",envir=envir_stacomi),objectreport,envir=envir_stacomi)
			} else {
			  objectreport@parm<-object
			  assign(get("objectreport",envir=envir_stacomi),objectreport,envir=envir_stacomi)
			}
			# suppresses all tab larger than current tab
			partab<-svalue(notebook)
			if (svalue(notebook)<length(notebook)){
			  svalue(notebook)<-partab+1	
			}
		  }
		}
		# below the widget structure [=> within (=> type
		# group(ggroup)[notebook(notebook)[groupstd(ggroup&tab)[[framestdsource(gframe)[tbsourcestd(gtable)],framestddest(gframe)[tbdeststd(gtable)]],OKbutton]]
		if (!exists("notebook",envir_stacomi)){ 
		  group<-get("group",envir_stacomi)
		  notebook <- gnotebook(container=group)} 
		else {
		  notebook<-get("notebook",envir=envir_stacomi)
		}
		car_libelle=fun_char_spe(object@data$par_nom)
		car_libelle[nchar(car_libelle)>30]<-paste(substr(car_libelle[nchar(car_libelle)>30],1,30),".",sep="")
		grouppar<-ggroup() 
		assign("gouppar",grouppar,envir=envir_stacomi)
		add(notebook,grouppar,label=label)
		frameparsource<-gframe(gettext("Select here",domain="R-stacomiR"),container=grouppar)
		tbsourcepar  = gtable(car_libelle,container=frameparsource,expand = TRUE, fill = TRUE)
		size(tbsourcepar)<-c(160,300) 
		framepardest<-gframe(gettext("drop here",domain="R-stacomiR"),container=grouppar)
		# need for a fixed size data.frame otherwise errors when adding new lines
		xx<-data.frame(choice=rep("",8))
		xx$choice<-as.character(xx$choice)
		tbdestpar=gtable(xx,container=framepardest,expand = TRUE, fill = TRUE)
		size(tbdestpar)<-c(160,300)
		adddropsource(tbsourcepar)
		adddroptarget(tbdestpar)				
		adddropmotion(tbdestpar,handler=function(h,...) {
			  valeurs<-tbdestpar[,]
			  valeurs<-valeurs[valeurs!=""]
			  if (!svalue(tbsourcepar)%in%valeurs){
				tbdestpar[length(valeurs)+1,1]<-svalue(tbsourcepar)
			  }
			})
		addHandlerDoubleclick(tbsourcepar,handler=function(h,...) {
			  valeurs<-tbdestpar[,]
			  valeurs<-valeurs[valeurs!=""]
			  if (!svalue(tbsourcepar)%in%valeurs){
				tbdestpar[length(valeurs)+1,1]<-svalue(h$obj)
			  }
			})
		adddropsource(tbdestpar)
		adddroptarget(tbsourcepar)
		removepar<-function(){
		  valeurs<-tbdestpar[,]
		  valeurs<-valeurs[valeurs!=""]
		  valeurs<-valeurs[-match(svalue(tbdestpar),valeurs)]
		  tbdestpar[,]<-c(valeurs,rep("",8-length(valeurs)))
		}
		adddropmotion(tbsourcepar,handler=function(h,...) {
			  removepar()
			})
		addHandlerDoubleclick(tbdestpar,handler=function(h,...) {
			  removepar()
			})
		gbutton("OK", container = grouppar, handler = hpar)
	  } else {
		funout(gettext("Error : no parameters in the database (the query returns 0 entry)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	})
