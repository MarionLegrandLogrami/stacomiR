UNE_SECONDE     = as.difftime(c("0:0:1")) ;
UNE_MINUTE      = 60 * UNE_SECONDE ;
DIX_MINUTES     = 10 * UNE_MINUTE ;
QUINZE_MINUTES  = 15 * UNE_MINUTE ;
TRENTE_MINUTES  = 30 * UNE_MINUTE ;
UNE_HEURE       = 60 * UNE_MINUTE ;
DOUZE_HEURES    = 12 * UNE_HEURE ;
UN_JOUR         = 24 * UNE_HEURE ;
UNE_SEMAINE     = 7 * UN_JOUR ;
DEUX_SEMAINES   = 2 * UNE_SEMAINE ;
UN_MOIS         = 30 * UN_JOUR ;
TROIS_MOIS      = 91 * UN_JOUR ;
SIX_MOIS        = 182 * UN_JOUR ;
UN_AN           = 365 * UN_JOUR ;

Valeurref_timestep=c(UNE_SECONDE,UNE_MINUTE,DIX_MINUTES,QUINZE_MINUTES,TRENTE_MINUTES,UNE_HEURE,DOUZE_HEURES,
	UN_JOUR,UNE_SEMAINE,DEUX_SEMAINES,UN_MOIS,TROIS_MOIS,SIX_MOIS,UN_AN)
Labelref_timestep=c(
	"1 sec",
	"1 min",
	"10 min" ,
	"15 min" ,
	"30 min",
	"1 h"   ,
	"12 h"  ,
	"1 jour"   ,
	"1 sem" ,
	"2 sem"  ,
	"1 mois" ,
	"3 mois" ,
	"6 mois" ,
	"1 an"   )
Lesref_timestep=data.frame("Valeurref_timestep"=Valeurref_timestep)
Lesref_timestep[,"Labelref_timestep"]=Labelref_timestep
rownames(Lesref_timestep)=
	c("UNE_SECONDE","UNE_MINUTE","DIX_MINUTES","QUINZE_MINUTES","TRENTE_MINUTES","UNE_HEURE","DOUZE_HEURES",
		"UN_JOUR","UNE_SEMAINE","DEUX_SEMAINES","UN_MOIS","TROIS_MOIS","SIX_MOIS","UN_AN")
rm(UNE_SECONDE,UNE_MINUTE,DIX_MINUTES,QUINZE_MINUTES,TRENTE_MINUTES,UNE_HEURE,DOUZE_HEURES,
	UN_JOUR,UNE_SEMAINE,DEUX_SEMAINES,UN_MOIS,TROIS_MOIS,SIX_MOIS,UN_AN,Labelref_timestep)


validity_ref_timestep=function(object)
{
  retValue=NULL
  rep1= class(object@dateDebut)[1]=="POSIXlt"
  if (!rep1) retValue="object@dateDebut is not of class POSIXlt"  
  rep2=length(object@step_duration)==1
  if (!rep2) retValue=paste(retValue,"length(object@step_duration) !=1") 
  rep3=length(object@nb_step)==1
  if (!rep3) retValue=paste(retValue,"length(object@nb_step) !=1") 
  rep4=length(object@nocurrent_step)==1
  if (!rep4) retValue=paste(retValue,"length(object@nocurrent_step) !=1")
  return(ifelse(rep1 & rep2 & rep3 & rep4,TRUE,retValue))
}

#' Class "ref_timestep"
#' 
#' Describes a time step
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_timestep",
#' dateDebut="POSIXt",step_duration=numeric(),nb_step=numeric(),nocurrent_step=integer())}.
#' \describe{ 
#' \item{list("dateDebut")}{Object of class \code{"POSIXt"} Starting
#' date }
#' \item{:}{Object of class \code{"POSIXt"} Starting date }
#' \item{list("step_duration")}{Object of class \code{"numeric"} Step length
#' }\item{:}{Object of class \code{"numeric"} Step length }
#' \item{list("nb_step")}{Object of class \code{"numeric"} Number of steps
#' }\item{:}{Object of class \code{"numeric"} Number of steps }
#' \item{list("nocurrent_step")}{Object of class \code{"integer"} Number of the
#' current step }\item{:}{Object of class \code{"integer"} Number of the
#' current step } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{ref_timestep_daily}}
#' @concept report Object
setClass(Class="ref_timestep",representation=
		representation(dateDebut="POSIXlt",step_duration="numeric",nb_step="numeric",nocurrent_step="integer"),
	validity=validity_ref_timestep,
	prototype=prototype(dateDebut=as.POSIXlt(Hmisc::truncPOSIXt(Sys.time(),"year")),
		step_duration=as.numeric(86400),
		nb_step=as.numeric(1),
		nocurrent_step=as.integer(0) ) )
# timestep= new("ref_timestep")


validity_ref_timestepChar=function(object)
{
  rep1= class(object@dateDebut)[1]=="POSIXlt"
  rep2=length(object@step_duration)==1
  rep3=length(object@nb_step)==1
  rep4=length(object@nocurrent_step)==1
  rep5= object@step_duration%in%Lesref_timestep[,"Labelref_timestep"]
  return(ifelse(rep1 & rep2 & rep3 & rep4 & rep5,TRUE,c(1:5)[!c(rep1, rep2, rep3, rep4,rep5)]))
}
#' Class "ref_timestepChar"
#' 
#' Character to represent a ref_timestep
#' 
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_timestepChar", \dots{})}
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{ref_timestep}}
#' @keywords classes
#' @examples
#' 
#' showClass("ref_timestepChar")
#' 
setClass(Class="ref_timestepChar",representation=
		representation(dateDebut="POSIXlt",step_duration="character",nb_step="numeric",nocurrent_step="integer"),
	validity=validity_ref_timestepChar,
	prototype=prototype(dateDebut=as.POSIXlt(strptime("2008-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S"),tz="GMT"),
		step_duration=as.character("1 jour"),
		nb_step=as.numeric(1),
		nocurrent_step=as.integer(0) ))

setAs("ref_timestepChar","ref_timestep",   # from to
	function(from,to){
	  index=Lesref_timestep[,"Labelref_timestep"]%in%from@step_duration
	  newstep_duration=Lesref_timestep[index,"Valeurref_timestep"]
	  new("ref_timestep",dateDebut=from@dateDebut,
		  step_duration=newstep_duration,
		  nb_step=from@nb_step,
		  nocurrent_step=from@nocurrent_step)})
# timestep=as(timestepChar,"ref_timestep")

#' Generic method to get current time step
#' @param object An object
#' @param ... Additional parameters passed to the method
setGeneric("getnocurrent_step",def=function(object,...) standardGeneric("getnocurrent_step"))
#' Gets the current time step of an object of class \link{ref_timestep-class}
#' @param object An object of class \link{ref_timestep-class}
#' @return the current time step of the object
#' @keywords internal
setMethod("getnocurrent_step",signature=signature("ref_timestep"),definition=function(object) object@nocurrent_step)

#' Generic method for getting the final date
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("end_date",def=function(object,...) standardGeneric("end_date"))
#' Gets the final horodate for an object of class \link{ref_timestep-class}
#' @param object An object of class \link{ref_timestep-class}
#' @return end_date, The final date corresponding to nb_step*time duration + initial date
#' @export
#' @keywords internal
setMethod("end_date",signature=signature("ref_timestep"),definition=function(object){
	  end_date=object@dateDebut+ object@step_duration*(object@nb_step)
	  # pour les pb de changement d'heure
	  
	  return(end_date)
	})

#' Generic method for getting the beginning date for current time step
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("currentstart_date",def=function(object,...) standardGeneric("currentstart_date"))
#' Gets the starting date of a time step for an object of class \link{ref_timestep-class}
#' @param object An object of class \link{ref_timestep-class}
#' @return current_start_date, The starting date for the current timestep
#' @keywords internal
setMethod("currentstart_date",signature=signature("ref_timestep"),definition=function(object){
	  current_start_date=object@dateDebut+ object@step_duration*object@nocurrent_step
	  # bug cht heure
	  if (object@step_duration==86400) {
		current_start_date=Hmisc::roundPOSIXt(current_start_date,"days")
	  }			
	  return(current_start_date)
	})

#' Generic method for getting the ending date for current time step
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("current_end_date",def=function(object,...) standardGeneric("current_end_date"))
#' Gets the ending date of a time step for an object of class \link{ref_timestep-class}
#' @param object An object of class \link{ref_timestep-class}
#' @return Currentend_date, The ending date for the current timestep
setMethod("current_end_date",signature=signature("ref_timestep"),definition=function(object){
	  the_current_end_date=object@dateDebut+ object@step_duration*(object@nocurrent_step+as.integer(1))
	  if (object@step_duration==86400) {
		the_current_end_date=Hmisc::roundPOSIXt(the_current_end_date,"days")
	  }
	  return(the_current_end_date)
	})


#' Generic method the get starting date
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("getdateDebut",def=function(object,...) standardGeneric("getdateDebut"))
#' Returns the starting date as character
#' @param object An object of class \link{ref_timestep-class}
#' @param ... Additional parameters passed to the method
#' @keywords internal
setMethod("getdateDebut",signature=signature("ref_timestep"),definition=function(object){
	  return ( strftime(as.POSIXlt(object@dateDebut),format="%Y-%m-%d %H:%M:%S") )
	})


#' Generic method to set the starting date
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("set_starting_date",def=function(object,...) standardGeneric("set_starting_date"))
#' Sets starting date from a character
#' 
#' 
#' @param object An object of class \link{ref_timestep-class}
#' @param string Character string of type"\%Y-\%m-\%d \%H:\%M:\%S" or "\%Y-\%m-\%d".
#' this allows to use either horodate or date
#' @return An object of class \link{ref_timestep-class}
#' @keywords internal
setMethod("set_starting_date",signature=signature("ref_timestep"),definition=function(object,string){
	  object@dateDebut=if (!is.na(strptime(string,format="%Y-%m-%d %H:%M:%S"))) strptime(string,format="%Y-%m-%d %H:%M:%S") else
                  strptime(string,format="%Y-%m-%d") 
	  return(object) 
	})

#' Generic method to get the string value of time step
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("get_step_label",def=function(object,...) standardGeneric("get_step_label"))



#' Gets the string value of time step
#' 
#' @param object An object of class \link{ref_timestep-class}
#' @return A string corresponding to the value of current time step
#' @keywords internal
setMethod("get_step_label",signature=signature("ref_timestep"),definition=function(object){
	  ret=paste(Lesref_timestep$Labelref_timestep)
	  return (ret )
	})

#' Generic method to get the years 
#' @param object An object
#' @param ... Additional parameters passed to the method
#' @keywords internal
setGeneric("get_year",def=function(object,...) standardGeneric("get_year"))

#' Gets the year or a vector of years corresponding to the timestep ("ref_timestep") object
#' @param object An object of class \link{ref_timestep-class}
#' @keywords internal
setMethod("get_year",signature=signature("ref_timestep"),definition=function(object){
	  dateFin=end_date(object)
	  dateDebut=object@dateDebut
	  seq=seq.POSIXt(from=dateDebut,to=dateFin,by="day")
	  seq=seq[-length(seq)]
	  annees=unique(strftime(seq,"%Y"))
  	  return (as.numeric(annees))
	})

#' Method to select timesteps from the graphical interface
#' @param object An object of class \link{ref_timestep-class}
#' @keywords internal
setMethod("choice",signature=signature("ref_timestep"),definition=function(object) {
	  if (length(Lesref_timestep$Labelref_timestep) > 0){
		hwinpa=function(h,...){
		  pas=svalue(choicepas)
		  nb_step=as.numeric(svalue(choicenb_step)) 
		  object@nb_step<-nb_step
		  object@step_duration<-as.numeric(Lesref_timestep$Valeurref_timestep[Lesref_timestep$Labelref_timestep%in%pas])
		  object=set_starting_date(object,svalue(datedeb))
		  assign("timestep",object,envir_stacomi)					
		}
		hchoicepas=function(h,...){
		  pas=svalue(choicepas)
		  nb_step=as.numeric(svalue(choicenb_step))
		  object@step_duration<-as.numeric(Lesref_timestep$Valeurref_timestep[Lesref_timestep$Labelref_timestep%in%pas])
		  object@nb_step<-nb_step 
		  object=set_starting_date(object,svalue(datedeb))
		  add(datedefin,strftime(as.POSIXlt(end_date(object)),format="%Y-%m-%d %H:%M:%S"),
			  font.attr=c(foreground.colors="red") )
		  hwinpa(h)
		}
		group<-get("group",envir=envir_stacomi)
		winpa=gframe(gettext("Time steps choice",domain="R-stacomiR"),container=group,horizontal=FALSE)
		pg<-ggroup(horizontal=FALSE,container=winpa)
		glabel(gettext("Starting date",domain="R-stacomiR"),container=pg)
		datedeb<-gedit(getdateDebut(object),
			container=pg,handler=hchoicepas,width=15)
		datedebut2=as.character(strftime(object@dateDebut,"%Y-%m-%d"))
		datedeb2<-gcalendar(datedebut2,container=pg,handler=function(h,...){
			  svalue(datedeb)<-as.character(strftime(
					  strptime(svalue(datedeb2),"%Y-%m-%d"),
					  "%Y-%m-%d %H:%M:%S"))
			  hchoicepas(h)				
			} )
		glabel(gettext("Time steps choice",domain="R-stacomiR"),container=winpa)
		pas_libelle=fun_char_spe(Lesref_timestep$Labelref_timestep)
		choicepas=gdroplist(pas_libelle,selected = 8,container=winpa,handler=hchoicepas) 
		glabel(gettext("Number of time step choice",domain="R-stacomiR"),container=winpa)
		choicenb_step=gedit("365",container=winpa,coerce.with=as.numeric,handler=hchoicepas,width=15)
		datedefin<-gtext(gettext("End date",domain="R-stacomiR"),height=50,container=winpa) # Date de fin
		gbutton("OK", container=winpa,handler=hwinpa,icon="execute")
	  } else funout(gettext("Internal error : no entry in time steps table\n",domain="R-stacomiR"), arret=TRUE)
	})


#' Graphical interface for multiple choice method for PasdeTemps (used in report_mig_mult)
#' @param object An object of class \link{ref_timestep-class}
#' @note this method differs from choice as it is called within a notebook,
#' it does not allow for multiple choice to be made
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
setMethod("choicemult",signature=signature("ref_timestep"),definition=function(object) {
	  if (length(Lesref_timestep$Labelref_timestep) > 0){
		hwinpa=function(h,...){
		  pas=svalue(choicepas)
		  nb_step=as.numeric(svalue(choicenb_step)) 
		  object@nb_step<<-nb_step
		  object@step_duration<<-as.numeric(Lesref_timestep$Valeurref_timestep[Lesref_timestep$Labelref_timestep%in%pas])
		  object=set_starting_date(object,svalue(datedeb))						
		  assign("timestep",object,envir_stacomi)
		  funout(gettext("Timesteps loaded\n",domain="R-stacomiR"))
		  # charge le deuxieme onglet du notebook
		  svalue(notebook)<-2
		}
		hchoicepas=function(h,...){
		  #browser()
		  pas=svalue(choicepas)
		  nb_step=as.numeric(svalue(choicenb_step))
		  object@step_duration<-as.numeric(Lesref_timestep$Valeurref_timestep[Lesref_timestep$Labelref_timestep%in%pas])
		  object@nb_step<-nb_step 
		  object=set_starting_date(object,svalue(datedeb))  
		  add(datedefin,strftime(as.POSIXlt(end_date(object)),format="%Y-%m-%d %H:%M:%S"),
			  font.attr=c(foreground.colors="red") )
		  hwinpa(h)
		}
		hchoicedatedebut=function(h,...){
		  # TODO to develop
		}
		notebook<-get("notebook",envir=envir_stacomi)
		groupdate<-ggroup(container=notebook, label="periode")   ## "add" called by constructor this is a tab of the notebook
		assign("groupdate",groupdate,envir=envir_stacomi)
		winpa=gframe(gettext("Time steps choice",domain="R-stacomiR"),container=groupdate,horizontal=FALSE)
		pg<-ggroup(horizontal=FALSE,container=winpa)
		glabel(gettext("Starting date",domain="R-stacomiR"),container=pg)
		datedeb<-gedit(getdateDebut(object),container=pg,handler=hchoicepas,width=15)
		datedebut2=as.character(strftime(object@dateDebut,"%Y-%m-%d"))
		datedeb2<-gcalendar(datedebut2,container=pg,handler=function(h,...){
			  svalue(datedeb)<-as.character(strftime(
					  strptime(svalue(datedeb2),"%Y-%m-%d"),
					  "%Y-%m-%d %H:%M:%S"))
			  hchoicepas(h)				
			} )
		glabel(gettext("Time steps choice",domain="R-stacomiR"),container=winpa)
		pas_libelle=fun_char_spe(Lesref_timestep$Labelref_timestep)
		choicepas=gdroplist(pas_libelle,selected = 8,container=winpa,handler=hchoicepas) 
		glabel(gettext("Number of time steps choice",domain="R-stacomiR"),container=winpa)
		choicenb_step=gedit("365",container=winpa,coerce.with=as.numeric,handler=hchoicepas,width=15)
		datedefin<-gtext(gettext("Ending date",domain="R-stacomiR"),height=50,container=winpa) # Date de fin)
		gbutton("OK", container=winpa,handler=hwinpa,icon="execute")
	  } else funout(gettext("Internal error : no entry in time steps table\n",domain="R-stacomiR"), arret=TRUE)
	})



