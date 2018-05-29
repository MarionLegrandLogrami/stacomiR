#' Class "ref_choice"
#' 
#' ref_choice referential class allows to choose within several values with
#' radiobuttons
#' 
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_choice", listechoice=character() ,label=character()
#' ,selected=integer())}.
#' @slot listechoice A character vector giving possible choices
#' @slot label A character, title of the box giving the possible choices
#' @slot selected An \code{Integer}  the initial selected value (as an index), first=1 used in gradio
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family referential objects
setClass(Class="ref_choice",representation= representation(listechoice="ANY",
		label="character",
		selected="integer",
		selectedvalue="ANY"),
	prototype=list(
		selectedvalue=vector()))

#' Loading method for Rechoice referential objects
#' 
#' @family referential objects
#' @return An S4 object of class ref_choice
#' @param object An object of class ref_choice
#' @param vecteur A vector of name, see example code.
#' @param label Labels for the choices
#' @param selected An integer indicating which object is selected at launch
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#' object=new("ref_choice")
#' charge(object,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
#' }
setMethod("charge",signature=signature("ref_choice"),definition=function(object,vecteur,label,selected) {
	  object@listechoice=vecteur
	  object@label=label
	  object@selected=selected
	  object
	  return(object)
	})
#' Choice method for Rechoice referential objects
#' 
#' Used by the graphical interface.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{ref_choice-class}
#' @examples 
#' \dontrun{
#'  object=new("ref_choice")
#'  object<-charge(object,vecteur=c("oui","non"),label="essai",selected=as.integer(1))
#' win=gwindow(title="test ref_choice")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object) 
#' dispose(win)}
#' @keywords internal
setMethod("choice",signature=signature("ref_choice"),definition=function(object) {
	  hlist=function(h,...){
		valeurchoisie=svalue(choice)
		object@listechoice<-valeurchoisie
		assign("refchoice",object,envir_stacomi)
		funout(paste(object@label,"\n"))
	  }
	  group<-get("group",envir=envir_stacomi)
	  frame_choice<-gframe(object@label)
	  assign("frame_choice",frame_choice,envir=envir_stacomi)
	  ##=>selection de plusieurs caracteristiques
	  add(group,frame_choice)
	  list_libelle=fun_char_spe(object@listechoice)
	  choice=gradio(items=list_libelle,selected=object@selected,horizontal=TRUE,container=frame_choice,handler=hlist)
	  gbutton("OK", container=frame_choice,handler=hlist)
	})

#' Choice_c method for refchoix referential objects
#' @param object An object of class \link{ref_list-class}
#' @param selectedvalue the value selected in the combo
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#' \dontrun{
#' object=new("ref_list")
#' object<-charge(object,vecteur=c("1","2"),label="please choose")
#' object<-choice_c(object)
#' }
setMethod("choice_c",signature=signature("ref_choice"),definition=function(object,selectedvalue) {
	  
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

#' Multiple Choice method for ref_choice referential objects, to put together with notebook widgets
#' @param object An object of class \link{ref_choice-class}
#' @param selected_value the value selected in the combo
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
setMethod("choicemult",signature=signature("ref_choice"),definition=function(object,
		selected_value
	) {
	  hlist=function(h,...){
		valeurchoisie=svalue(choice)
		object@selectedvalue<-valeurchoisie
		assign("refchoice",object,envir_stacomi)
		funout(gettext("choice made\n",domain="R-stacomiR"))
		if (svalue(notebook)<length(notebook)){
		  svalue(notebook)<-svalue(notebook)+1	
		}
	  }
	  group<-get("group",envir=envir_stacomi)
	  if (!exists("notebook",envir=envir_stacomi)){
		notebook <- gnotebook(container=group)
	  } else {
		notebook<-get("notebook",envir=envir_stacomi)
	  }
	  groupchoice<-ggroup(container=notebook, 
		  label=gettext("options",domain="R-stacomiR"),
		  horizontal=FALSE) 
	  glabel(object@label,container=groupchoice)
	  list_libelle=fun_char_spe(object@listechoice)
	  choice=gradio(items=list_libelle,
		  selected=object@selected,
		  horizontal=FALSE,
		  container=groupchoice,
		  handler=hlist)		
	  gbutton("OK", container=groupchoice,handler=hlist)
	})