#' Class "ref_parqual"
#' 
#' Class enabling to load the list of qualitative parameters and to select one
#' of them. It inherits from 'ref_par', uses its 'choice' method
#' @author cedric.briand"at"eptb-vilaine.fr
#' @slot valqual="data.frame" the list of qualitative parameters
#' @include ref_par.R
#' @family referential objects
setClass(Class="ref_parqual",representation= representation(valqual="data.frame"),contains="ref_par")

#' Loading method for Reparqual referential objects
#' @param object An object of class \link{ref_parqual-class}
#' @return An S4 object of class ref_parqual
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_parqual")
#'  charge(object)
#' }
setMethod("charge",signature=signature("ref_parqual"),definition=function(object) {
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@sql= "select * from ref.tg_parametre_par
		  INNER JOIN ref.tr_parametrequalitatif_qal ON tr_parametrequalitatif_qal.qal_par_code::text = tg_parametre_par.par_code::text"
	  requete<-stacomirtools::connect(requete)
	  #funout(gettext("The query to load parameters is done \n",domain="R-stacomiR"))
	  object@data<-requete@query
	  return(object)
	})

#' Loading method for Reparqual referential objects searching only those parameters existing for a DC, a Taxon, and a stage
#' @param object An object of class \link{ref_parqual-class}
#' @param dc_selectionne The dc set in the report object
#' @param taxa_selectionne The taxa set in the report object
#' @param stage_selectionne The stage set in the report object
#' @return An S4 object of class \link{ref_parqual-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selectionne=6
#'	taxa_selectionne=2038
#'  stage_selectionne="AGJ"
#'  object=new("ref_parqual")
#'  charge_with_filter(object,dc_selectionne,taxa_selectionne,stage_selectionne)
#' }
setMethod("charge_with_filter",signature=signature("ref_parqual"),definition=function(object,dc_selectionne,taxa_selectionne,stage_selectionne) {
	  requete=new("RequeteODBCwhere")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@select=paste("SELECT DISTINCT ON (par_code) par_code, par_nom", 
		  " FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
		  " JOIN ref.tg_parametre_par on par_code=car_par_code",
		  " JOIN ref.tr_parametrequalitatif_qal ON tr_parametrequalitatif_qal.qal_par_code::text = tg_parametre_par.par_code::text",sep="")
	  requete@where=paste("where dis_identifiant in ",vector_to_listsql(dc_selectionne))
	  requete@and=paste("and lot_tax_code in ",vector_to_listsql(taxa_selectionne)," and lot_std_code in ",vector_to_listsql(stage_selectionne),sep="")
	  requete@order_by="ORDER BY par_code"  
	  requete<-stacomirtools::connect(requete)
	  object@data<-requete@query
	  return(object)
	})

#'  Loads an additional dataset
#' this method is loaded to obtain the possible values of a qualitative parameter. Here data only contains one line
#' @param object An object of class \link{ref_parqual-class}
#' @return An S4 object of class ref_parqual with the valqual slot filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selectionne=6
#'	taxa_selectionne=2038
#'  stage_selectionne="AGJ"
#'  object=new("ref_parqual")
#'  object<-charge(object)
#'  charge_complement(object)
#' }		
setMethod("charge_complement",signature=signature("ref_parqual"),definition=function(object) {
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@sql= paste("select * from ref.tr_valeurparametrequalitatif_val",
		  " WHERE val_qal_code in ", vector_to_listsql(object@par_selected),
		  " ORDER BY val_rang",sep="")
	  requete<-stacomirtools::connect(requete)
	  #funout(gettext("The query to load parameters is done \n",domain="R-stacomiR"))
	  object@valqual<-requete@query
	  return(object)
	})


#' Choice method for ref_parqual referential objects, internal use
#' @note the choice method assigns an object of class ref_parqual named ref_parqual in the environment envir_stacomi
#' this method rewrites the method from ref_par, as it integrates a request of the possible values of qualitative parameters, hence the parameters,however it was redefined in ref_parqual
#' to load the possible values of qualitative parameters
#' @param object An object of class \link{ref_parqual-class}
#' @param label The label to set in the frame
#' @param nomassign The name used when assigning the object ref_parqual to the \code{envir_stacomi} environment
#' @param frameassign The name of the gframe assigned to the user environment
#' @param is.enabled Is the frame enabled at launch.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#'  object=new("ref_parqual")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' choice(object)
#' }
#' @keywords internal
setMethod("choice",signature=signature("ref_parqual"),definition=function(object,
		label=gettext("Choice of a sample characteristic",domain="R-stacomiR"),
		nomassign="ref_parqual",
		frameassign="frame_par",
		is.enabled=TRUE) {
	  if (nrow(object@data) > 0){
		hcar=function(h,...){
		  carchoisi=svalue(choice)
		  object@data<-object@data[car_libelle%in%carchoisi ,]
		  object<-charge_complement(object)
		  assign(nomassign,object,envir_stacomi)
		  funout(gettext("Features have been selected\n",domain="R-stacomiR"))
		}
		group<-get("group",envir=envir_stacomi)
		assign(frameassign,gframe(label),envir= envir_stacomi)
		add(group,get(eval(frameassign),envir= envir_stacomi))
		car_libelle=fun_char_spe(object@data$par_nom)
		choice=gdroplist(items=car_libelle,container=get(eval(frameassign),envir= envir_stacomi),handler=hcar)
		gbutton("OK", container=get(eval(frameassign),envir= envir_stacomi),handler=hcar)
	  } else stop(gettext("Internal error : unable to load any feature to make the choice\n",domain="R-stacomiR"),arret=TRUE)
	})


#' Multiple Choice method for ref_parqual referential objects internal use
#' @note this methods rewrites that of the ref_par as it integrates a call to chargecomplement to load
#' the list of possible values for a qualitative parameter
#' @param object An object of class \link{ref_parqual-class}
#' @param objectreport An object report which includes the \link{ref_parqual-class}, default NULL
#' @param nomassign The name used when assigning the object ref_parqual to the \code{envir_stacomi} environment
#' @param label The name of the frame
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
setMethod("choicemult",signature=signature("ref_parqual"),definition=function(object,
		objectreport=NULL,
		nomassign="ref_parqual",
		label=gettext("Qualitative",domain="R-stacomiR")) {			
	  if (nrow(object@data) > 0){
		hpar=function(h,...){
		  parm=tbdestpar[,][tbdestpar[,]!=""]
		  if (length(parm)>0){
			object@par_selected<-object@data[car_libelle%in%parm ,"par_code"]
			# below the line that changes from the ref_par
			object<-charge_complement(object)
			assign(nomassign,object,envir_stacomi)
			funout(gettext("Parameter selected\n",domain="R-stacomiR"))
		  } else {
			funout(gettext("No Parameter selected\n",domain="R-stacomiR"))	
		  }
		  if (!is.null(objectreport)) {
			objectreport@parqual<-object
			assign(get("objectreport",envir=envir_stacomi),objectreport,envir=envir_stacomi)
			# suppresses all tab larger than current tab
			qualtab<-svalue(notebook)
			if (svalue(notebook)<length(notebook)){
			  svalue(notebook)<-qualtab+1	
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
		add(notebook,grouppar,label=gettext("Qualitative",domain="R-stacomiR"))
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
		funout(gettext("Error : no qualitative parameters in the database (the query returns 0 entry)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	})
