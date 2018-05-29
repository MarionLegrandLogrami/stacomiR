# Nom fichier :        ref_env   (classe)
# Date de creation :   02/01/2009 15:02:40

#' Class "ref_env"
#' 
#' Enables to load measure stations and to select one of them
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_env", ...)}. 
#' @slot dataframe Data concerning the
#' measure station
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
setClass (Class="ref_env", 
	representation=representation(data="data.frame"),
	prototype=prototype(data=data.frame())
)

#' Loading method for ref_env referential object
#' @return An S4 object of class ref_env
#' @param object An object of class \link{ref_env-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_env")
#'  charge(object)
#' }
setMethod("charge",
	signature=signature("ref_env"),     
	definition=function(object) 
	{
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@sql= paste("SELECT stm_identifiant, stm_libelle, stm_sta_code, stm_par_code, stm_description",
		  " FROM ",get("sch",envir=envir_stacomi),"tj_stationmesure_stm", 
		  " ORDER BY stm_identifiant;",sep="")
	  requete@silent = TRUE;
	  requete<-stacomirtools::connect(requete)    
	  object@data<-requete@query
	  return(object)
	}
)
#' Choice method for ref_env referential object
#' @param object An object of class \link{ref_env-class}
#' @param is.enabled A boolean parameter, if TRUE the frame is enabled when first displayed
#' @param title The title of the frame, defaut to "Monitoring stations selection" 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
#' @examples  
#' \dontrun{
#' object=new("ref_env")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object)
#' choice(object)
#' }
setMethod("choice",signature=signature("ref_env"),definition=function(object,
		is.enabled=TRUE,
		title=gettext("Monitoring stations selection",domain="R-stacomiR")) {
	  if (nrow(object@data) > 0){
		hSTM=function(h,...){
		  stationMesure=svalue(choice,index=TRUE)
		  if(length(stationMesure)==0)
		  {
			funout(gettext("Select at least one value\n",domain="R-stacomiR"),arret=TRUE)
		  }
		  else
		  {
			object@data<-object@data[stationMesure,]
			assign("ref_env",object,envir_stacomi)
			funout(gettext("The monitoring stations have been selected\n",domain="R-stacomiR"))
		  }
		}
		group<-get("group",envir=envir_stacomi)
		frame_stationMesure<<-gexpandgroup(title)
		add(group,frame_stationMesure)
		stm_libelle=fun_char_spe(object@data$stm_libelle)
		choice=gcheckboxgroup(stm_libelle,container=frame_stationMesure)
		enabled(frame_stationMesure)<-is.enabled
		gbutton("OK", container=frame_stationMesure,handler=hSTM)
	  } 
	  else funout(gettext("Stop : no data for selected monitoring station (problem with the ODBC link ?)\n",domain="R-stacomiR"),arret=TRUE)
	})



#' Command line interface to select a monitoring  station
#' 
#' the choice_c method is intendedto have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line. 
#' @param object an object of class ref_env
#' @param stationMesure a character vector of the monitoring station code (corresponds to stm_libelle in the tj_stationmesure_stm table)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("choice_c",signature=signature("ref_env"),definition=function(object,stationMesure) {
	  if (class(stationMesure)!="character") {		
		stop("the stationmesure should be of class character")
	  } 
	  if(length(stationMesure)==0){
		stop("Select at least one value\n")
	  }
	  if (any(is.na(stationMesure))){
		stop("NA values for stationmesure")
	  } 
	  # I can use the stm_libelle as there is a unique constraint in the table
	  libellemanquants<-stationMesure[!stationMesure%in%object@data$stm_libelle]
	  if (length(libellemanquants)>0) warning(gettextf("stationmesure code not present :\n %s",stringr::str_c(libellemanquants,collapse=", "),domain="R-stacomiR"))
	  object@data<-object@data[object@data$stm_libelle%in%stationMesure,]		
	  assign("ref_env",object,envir_stacomi)
	  return(object)
	})