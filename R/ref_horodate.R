#' Validity check for ref_horodate
#' 
#' @param object A ref_horodate object
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
validity_ref_horodate=function(object)
{
  rep1= class(object@horodate)[2]=="POSIXt"
  
  return(ifelse(rep1,TRUE,FALSE))
}


#' Class ref_horodate
#' 
#' choice of date with method to show current and previous year
#' 
#' 
#' @slot horodate a "POSIXt"
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_horodate", \dots{})}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family referential objects
setClass(Class="ref_horodate",representation=
		representation(horodate="POSIXt"),
	validity=validity_ref_horodate,
	prototype=prototype(horodate=Hmisc::roundPOSIXt(Sys.time(),"years")))
# date= new("Horodate")
#retourne la date en format character
setGeneric("getref_horodate",def=function(object,...) standardGeneric("getref_horodate"))
setMethod("getref_horodate",signature=signature("ref_horodate"),definition=function(object){
	  return ( strftime(as.POSIXlt(object@horodate),format="%Y-%m-%d %H:%M:%S") )
	})

#Fixe la date de debut e partir d'un champ charactere de type "%Y-%m-%d %H:%M:%S"
setGeneric("setref_horodate",def=function(object,...) standardGeneric("setref_horodate"))




#' Method to set the Horodate
#' @param object An object of class \link{ref_horodate-class}
#' @param string A string representing an horodate in the format "\%Y-\%m-\%d \%H:\%M:\%S"
#' @return An Object of class "ref_horodate" 
#' @author cedric.briand
#' @keywords internal
setMethod("setref_horodate",signature=signature("ref_horodate"),definition=function(object,string){
	  object@horodate=strptime(string,format="%Y-%m-%d %H:%M:%S")
	  return(object) 
	})
# retourne l'annee d'avant l'annee en cours






#' Graphical interface
#' @param object An object of class \link{ref_horodate-class}
#' @param label Label for the gframe
#' @param nomassign The name assigned in environment envir_stacomi
#' @param funoutlabel, text displayed by the interface
#' @param decal Default 0, number of years to shift forward or backward 
#' @keywords internal
#' @return Selects the date in the graphical interface, and assigns an object of class POSIXt with name nomassign in envir_stacomi
setMethod("choice",signature=signature("ref_horodate"),definition=function(object,
		label="date",
		nomassign="horodate",
		funoutlabel="nous avons le choix dans la date\n",
		decal=0) {
	  hwinhor=function(h,...){
		object=setref_horodate(object,svalue(horodate))
		assign(nomassign,object@horodate,envir_stacomi)
		funout(funoutlabel)
		#print(object)
		#dispose(winpa)
	  }
	  if (decal!=0){
		# Returns the first horodate of a year shifted by decal
		# @param horodate The horodate to shift (class POSIXt)
		# @param decal number of year to shift
		# @return A POSIXt
		shiftyear<-function(horodate,decal){
		  anneeprec=as.numeric(strftime(horodate,"%Y"))+decal
		  return(strptime(paste(anneeprec,"-01-01",sep=""),format="%Y-%m-%d"))
		}
		object@horodate<-shiftyear(object@horodate,decal)
	  }
	  group<-get("group",envir=envir_stacomi)
	  winhor=gframe(label,container=group,horizontal=FALSE)
	  pg<-ggroup(horizontal=FALSE,container=winhor)
	  horodate<-gedit(getref_horodate(object),container=pg,handler=hwinhor,width=20)
	  horodate2=as.character(strftime(object@horodate,"%Y-%m-%d"))
	  gbutton("OK", container=winhor,handler=hwinhor,icon="execute")
	})



#' Command line
#' @param object An object of class \link{ref_horodate-class}
#' @param nomassign The name assigned in environment envir_stacomi
#' @param funoutlabel, text displayed by the interface
#' @param silent Default FALSE, should messages be displayed
#' @param horodate The horodate to set, formats "\%d/\%m/\%Y \%H:\%M:\%s", "\%d/\%m/\%y \%H:\%M:\%s", "\%Y-\%m-\%d  \%H:\%M:\%s" formats
#' can also be passed with the date set to the minute \%d/\%m/\%Y \%H:\%M or the day  \%d/\%m/\%Y
#' \dots are accepted. The choice_c method assigns and
#' @return An object of class \link{ref_horodate-class} with slot \emph{horodate} set,
#'  and assigns an object of class POSIXt with name nomassign in envir_stacomi
setMethod("choice_c",signature=signature("ref_horodate"),definition=function(object,
		nomassign="horodate",
		funoutlabel="nous avons le choix dans la date\n",
		#decal=0,
		horodate,
		silent=FALSE
	) {
	  # horodate="2013-01-01"
	  # parse the horohorodate
	  if (length(horodate)>1) stop("horodate should be a vector of length 1")
	  if (is.null(horodate)) stop("horodate should not be null")
	  if (class(horodate)=="character") {
		if (grepl("/",horodate)){
		  .horodate=strptime(horodate, format="%d/%m/%Y %H:%M:%s")
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%d/%m/%y %H:%M:%s")				
		  }
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%d/%m/%y %H:%M")				
		  }
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%d/%m/%Y %H:%M")				
		  }
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%d/%m/%y")				
		  }
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%d/%m/%Y")				
		  }
		} else if (grepl("-",horodate)){
		  .horodate=strptime(horodate, format="%Y-%m-%d  %H:%M:%s")
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%d-%m-%Y  %H:%M:%s")				
		  }
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%Y-%m-%d  %H:%M")				
		  }
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%d-%m-%Y  %H:%M")				
		  }
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%Y-%m-%d")				
		  }
		  if (is.na(.horodate)){
			.horodate=strptime(horodate, format="%d-%m-%Y")				
		  }
		} else {
		  stop("Formatting problem, the character vector you are trying to pass as horodate could not
				  be parsed. Check example or documentation")
		}
		
	  } else if (class(horodate)=="Date"){
		.horodate<-as.POSIXlt(horodate)
	  } else if (class(horodate)[2]=="POSIXt"){
		.horodate=horodate
	  }
	  if (is.na(.horodate)) stop("Formatting problem, the character vector you are trying to pass as horodate could not
				be parsed. Check example or documentation")
	  object@horodate=.horodate	
	  validObject(object)				
	  assign(nomassign,object@horodate,envir_stacomi)
	  if (!silent) funout(funoutlabel)	
	  return(object)
	})

#' Multiple Choice method for ref_horodate referential objects, to put together with notebook widgets
#' @param object An object of class \link{ref_horodate-class}
#' @param label the name to write in the frame
#' @param nomassign the name with which the frame will be assigned to envir_stacomi
#' @param funoutlabel the sentence to write when the choice has been made
#' @param decal Default year will be current year, use -1 to set the default value in the interface to the year before
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
setMethod("choicemult",signature=signature("ref_horodate"),definition=function(object,
		label="date",
		nomassign="horodate",
		funoutlabel="nous avons le choix dans la date\n",
		decal=0
	) {
	  hhoro=function(h,...){
		object=setref_horodate(object,svalue(horodate))
		assign(nomassign,object@horodate,envir_stacomi)
		funout(gettext("Horodate selected\n",domain="R-stacomiR"))				
		# changing tab of notebook to next tab
		if (svalue(notebook)<length(notebook)){
		  svalue(notebook)<-svalue(notebook)+1	
		}
	  }
	  if (decal!=0){
		# Returns the first horodate of a year shifted by decal
		# @param horodate The horodate to shift (class POSIXt)
		# @param decal number of year to shift
		# @return A POSIXt
		shiftyear<-function(horodate,decal){
		  anneeprec=as.numeric(strftime(horodate,"%Y"))+decal
		  return(strptime(paste(anneeprec,"-01-01",sep=""),format="%Y-%m-%d"))
		}
		object@horodate<-shiftyear(object@horodate,decal)
	  }
	  
	  if (!exists("notebook",envir_stacomi)){ 
		group<-get("group",envir_stacomi)
		notebook <- gnotebook(container=group)} 
	  else {
		notebook<-get("notebook",envir=envir_stacomi)
	  }
	  grouphorodate<-ggroup(container=notebook, label=label,horizontal=FALSE) 
	  horodate<-gedit(getref_horodate(object),container=grouphorodate,handler=hhoro,width=20)			
	  gbutton("OK", container=grouphorodate,handler=hhoro,icon="execute")			
	})