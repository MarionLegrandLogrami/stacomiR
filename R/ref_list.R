# Nom fichier :        ref_list   (classe)
# Description          Classe permettant charger un choice dans une liste utilisee par un objectreport
#' Class "ref_list"
#' 
#' Enables to load a "ref_choice" object fom a list given by a "report" object
#' 
#' 
#' @param liste choice="character" A vector of character to choose within a droplist
#' @param label="character" the title of the box
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_list", listechoice, label)}.  \describe{
#' \item{list("listechoice")}{Object of class \code{"character"}}\item{:}{Object
#' of class \code{"character"}} \item{list("label")}{Object of class
#' \code{"character"}}\item{:}{Object of class \code{"character"}} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords internal
#' @family referential objects
setClass(Class="ref_list",representation= representation(listechoice="character",selectedvalue="character",label="character"))


#' Loading method for refliste referential objects
#' @return An object of class ref_list
#' @param object An object of class \link{ref_list-class}
#' @param listechoice A character vector setting the possible values in which the user can select
#' @param label A label for refliste
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_list")
#' charge(object)
#' }
setMethod("charge",signature=signature("ref_list"),definition=function(object,listechoice,label) {
	  object@listechoice=listechoice
	  object@label=label
	  return(object)
	})
#' Choice method for ref_list referential objects
#' @param object An object of class \link{ref_list-class}
#' @param is.enabled A boolean indicating whether the frame is enabled when first displayed, default to TRUE
#' @note the choice method assigns an object of class refList named ref_list in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#'  object=new("ref_list")
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object<-charge(object,vecteur=c("choice1","choice2"),label="please choose")
#' choice(object)
#' }
#' @keywords internal
setMethod("choice",signature=signature("ref_list"),definition=function(object,is.enabled=TRUE) {
	  hlist=function(h,...){
		valeurchoisie=svalue(choice)
		object@selectedvalue<-object@listechoice[list_libelle%in%valeurchoisie]
		assign("refliste",object,envir_stacomi)
		funout(paste(object@label,"\n"))
	  }
	  group<-get("group",envir=envir_stacomi)
	  frame_list<<-gframe(object@label)
	  add(group,frame_list)
	  list_libelle=fun_char_spe(object@listechoice)
	  choice=gdroplist(items=list_libelle,container=frame_list,handler=hlist)
	  enabled(frame_list)<-is.enabled
	  gbutton("OK", container=frame_list,handler=hlist)
	})


#' Choice_c method for ref_list referential objects
#' @param object An object of class \link{ref_list-class}
#' @param selectedvalue the value selected in the combo
#' @note the choice method assigns an object of class refList named ref_list in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#' object=new("ref_list")
#' object<-charge(object,vecteur=c("1","2"),label="please choose")
#' object<-choice_c(object)
#' }
setMethod("choice_c",signature=signature("ref_list"),definition=function(object,selectedvalue) {
	  if (length(selectedvalue)>1) stop("valeurchoisie should be a vector of length 1")
	  if (class (selectedvalue)=="numeric") selectedvalue<-as.character(selectedvalue)
	  # the charge method must be performed before
	  
	  if ( !selectedvalue %in% object@listechoice ) {
		stop(stringr::str_c("The selected valeur,",selectedvalue," not in the list of possible values :",
				stringr::str_c(object@listechoice,collapse=",")))
	  } else {
		object@selectedvalue<-selectedvalue
	  }
	  return(object)
	})
