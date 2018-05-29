# Nom fichier :        PasdeTemps (classe)
# Projet :             controle migrateur
# Organisme :          IAV
# Auteur :             Cedric Briand
# Contact :            cedric.briand"at"eptb-vilaine.fr
# Date de creation :  31/03/2008 17:21:25
# Compatibilite :
# Etat :               developpement
# Description          calcul et affichage des pas de temps (classe object)
#**********************************************************************
#*

################################################################
# Declarations de classe
################################################################


#' Class "ref_timestep_daily"
#' 
#' Representation of a ref_timestep object with a step length equal to one day.
#' It receives an inheritance from ref_timestep
#' 
#' validity_ref_timestep_daily
#' @include ref_timestep.R
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_timestep_daily",
#' dateDebut="POSIXt",step_duration=numeric(),nb_step=numeric(),nocurrent_step=integer())}.
#' \describe{ \item{list("dateDebut")}{Object of class \code{"POSIXt"} Starting
#' date }\item{:}{Object of class \code{"POSIXt"} Starting date }
#' \item{list("step_duration")}{Object of class \code{"numeric"} Step length
#' }\item{:}{Object of class \code{"numeric"} Step length }
#' \item{list("nb_step")}{Object of class \code{"numeric"} Number of steps
#' }\item{:}{Object of class \code{"numeric"} Number of steps }
#' \item{list("nocurrent_step")}{Object of class \code{"integer"} Number of the
#' current step }\item{:}{Object of class \code{"integer"} Number of the
#' current step } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{ref_timestep}}
#' @keywords classes
setClass(Class="ref_timestep_daily",contains="ref_timestep",
	prototype=(step_duration=86400) 
)



setValidity(Class="ref_timestep_daily",function(object)
	{
	  retValue<-NULL
	  rep1 = validity_ref_timestep(object)
	  if (!is.logical(rep1)) retValue<-rep1
	  rep2 = (object@step_duration==86400)
	  if (!rep2) retValue=paste(retValue,gettext("Time step duration should be daily",domain="R-stacomiR"))
	  rep3 = length(get_year(object))==1
	  if (!rep3) retValue=paste(retValue,gettext("Time step can't include more than one year",domain="R-stacomiR"))
	  return(ifelse( rep1 & rep2 & rep3 ,TRUE,retValue)   )
	})	
# pour test #object=new("ref_timestep_daily")

#' Method to select timesteps from the graphical interface
#' @param object An object of class \link{ref_timestep_daily-class}
#' @keywords internal
setMethod("choice",signature=signature("ref_timestep_daily"),definition=function(object) {
	  if (length(Lesref_timestep$Labelref_timestep) > 0){
		hwinpa=function(h,...){
		  pas=svalue(choicepas)
		  nb_step=as.numeric(svalue(choicenb_step)) 
		  object@nb_step<-nb_step
		  object@step_duration<-as.numeric(Lesref_timestep$Valeurref_timestep[Lesref_timestep$Labelref_timestep%in%pas])
		  object=set_starting_date(object,as.POSIXlt(svalue(datedeb)))
		  svalue(datedefin)<-as.Date(end_date(object))
		  assign("timestep",object,envir_stacomi)
		  funout(gettext("Time steps loaded\n",domain="R-stacomiR"))
		  #dispose(winpa)
		}
		group<-get("group",envir=envir_stacomi)
		winpa=gframe(gettext("Time steps choice (1 year duration)",domain="R-stacomiR"),container=group,horizontal=FALSE)
		pg<-glayout(container=winpa)
		pg[1,1]<-glabel(gettext("Start date",domain="R-stacomiR"))
		datedeb<-gedit(as.Date(getdateDebut(object)),handler=hwinpa,width=10)
		pg[2,1]<-datedeb
		pg[3,1]<-glabel(gettext("Time step",domain="R-stacomiR"))
		pas_libelle=fun_char_spe(Lesref_timestep$Labelref_timestep)
		choicepas=gdroplist(pas_libelle,selected = 8,handler=hwinpa)
		pg[4,1]<-choicepas 
		enabled(choicepas)=FALSE
		pg[3,2]<-glabel(gettext("Number of days",domain="R-stacomiR"))
		choicenb_step=gedit("365",coerce.with=as.numeric,handler=hwinpa,width=5)
		pg[4,2]<-choicenb_step
		pg[1,2]<-glabel(gettext("End date",domain="R-stacomiR"),container=pg)
		datedefin<-gedit("...",width=10) # heigth=30
		enabled(datedefin)<-FALSE
		pg[2,2]<-datedefin			
		pg[3,4:4]<-	gbutton("OK", handler=hwinpa,icon="execute")
	  } else stop("internal error length(Lesref_timestep$Labelref_timestep) == 0")
	})



#' choice_c method for class ref_timestep_daily
#' 
#' the choice_c method is intendedto have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line.  
#' @param object An object of class \link{ref_timestep_daily-class}
#' @param datedebut A character (format \code{"15/01/1996"} or \code{"1996-01-15"} or \code{"15-01-1996"}), or POSIXct object
#' @param datefin A character 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#'  object=new("ref_dc")
#'  object<-charge(object)
#'  choice_c(object=object,datedebut="2012-01-01",datefin="2013-01-01")
#' }
setMethod("choice_c",signature=signature("ref_timestep_daily"),definition=function(object,datedebut,datefin) {
	  if (class(datedebut)=="character") {
		if (grepl("/",datedebut)){
		  datedebut=strptime(datedebut, format="%d/%m/%Y")
		  if (is.na(datedebut)){
			datedebut=strptime(datedebut, format="%d/%m/%y")				
		  }
		} else if (grepl("-",datedebut)){
		  datedebut=strptime(datedebut, format="%Y-%m-%d")
		  if (is.na(datedebut)){
			datedebut=strptime(datedebut, format="%d-%m-%Y")				
		  }
		}
		if (is.na(datedebut)){
		  stop ("datedebut not parsed to datetime try format like '01/01/2017'")
		}
	  }
	  
	  # the datedebut can have a POSIXct format
	  if (class(datefin)=="character") {
		if (grepl("/",datefin)){
		  datefin=strptime(datefin, format="%d/%m/%Y")
		  if (is.na(datefin)){
			datefin=strptime(datefin, format="%d/%m/%y")				
		  }
		} else if (grepl("-",datefin)){
		  datefin=strptime(datefin, format="%Y-%m-%d")
		  if (is.na(datefin)){
			datefin=strptime(datefin, format="%d-%m-%Y")				
		  }
		}
		if (is.na(datefin)){
		  stop ("datefin not parsed to datetime try format like '01/01/2017'")
		}	
	  }
	  object@dateDebut<-as.POSIXlt(datedebut)
	  object@nb_step=as.numeric(difftime(datefin,datedebut,units="days")) # to fit with end_date(object)
	  validObject(object) 		
	  assign("timestep",object,envir_stacomi)
	  return(object)
	})
