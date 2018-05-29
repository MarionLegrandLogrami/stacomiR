#' ref_checkbox referencial class 
#' 
#' referential class allows to choose for several parms with checkbox
#' @slot title A "character", the title of the box giving the possible choices
#' @slot labels The logical parameters choice
#' @slot checked A boolean vector
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_checkbox", ...)}.
#' @family referential objects
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setClass(Class="ref_checkbox",representation= representation(title="character",labels="character",checked="logical"),
	prototype=prototype(title="liste de choice",labels="choice",checked=FALSE))

#' Loading method for ref_checkbox referential objects
#' @param object An object of class \link{ref_checkbox-class}
#' @param title Title of the frame
#' @param labels Labels for checked
#' @param checked Vector of boolean indicating if ref_checkbox are checked
#' @return An object of class \link{ref_checkbox-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_checkbox")
#' charge(object,title="essai",labels=c("par1","par2","par3"),checked=c(TRUE,TRUE,TRUE))
#' }
setMethod("charge",signature=signature("ref_checkbox"),definition=function(object,title,labels,checked) {
	  if (length(labels) != length(checked)) stop ("les longeur de 'labels' et 'checked' sont differentes")
	  object@title=title
	  object@labels=labels
	  object@checked=checked
	  return(object)
	})
#' Choice method for ref_checkbox referential objects
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{ref_checkbox-class}
#' @examples 
#' \dontrun{
#' object=new("ref_checkbox")
#' object<- charge(object,title="essai",labels=c("par1","par2","par3"),checked=c(TRUE,TRUE,TRUE))
#' win=gwindow(title="test ref_checkbox")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object) 
#' dispose(win)
#' }
#' @keywords internal
setMethod("choice",signature=signature("ref_checkbox"),definition=function(object) {
	  hlist=function(h,...){
		i=h$action
		if (exists("ref_checkbox",envir_stacomi)) {
		  object<-get("ref_checkbox",envir_stacomi)
		}
		object@checked[i]<-svalue(the_choice[[i]])
		assign("ref_checkbox",object,envir_stacomi)
		funout(paste("choice",object@labels[i],"\n"))
	  }
	  group<-get("group",envir=envir_stacomi)
	  frame_check<-gframe(object@title)	
	  assign("frame_check",frame_check,envir=envir_stacomi)
	  ##=>selection de plusieurs caracteristiques
	  add(group,frame_check)
	  the_choice=list()
	  for(i in 1: length(object@labels)){
		the_choice[[i]]=gcheckbox(text=object@labels[i], action=i,checked = object@checked[i],container=frame_check,handler=hlist)
	  }
	})

