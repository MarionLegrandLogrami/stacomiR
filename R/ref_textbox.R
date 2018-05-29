#' ref_textbox referencial class 
#' 
#' allows to a put a value within a glabel
#' @author cedric.briand"at"eptb-vilaine.fr
#' @slot title="character" the title of the box giving the possible choices
#' @slot labels the logical parameters choice
#' @slot checked a vector
setClass(Class="ref_textbox",representation= representation(title="character",label="character"))

#' Loading method for ReTextBox referential objects
#' @param object An object of class \link{ref_textbox-class}
#' @param title A title for the frame
#' @param label A label for the TextBox
#' @return An S4 object of class ref_textbox
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_textbox")
#' charge(object,title="un titre",label="20")
#' }
setMethod("charge",signature=signature("ref_textbox"),definition=function(object,title,label) {
	  object@title=title
	  object@label=label
	  return(object)
	})


#' Choice method for ReTextBox referential objects
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{ref_textbox-class}
#' @param nomassign The name with which the object will be assigned in envir_stacomi
#' @keywords internal
#' @examples 
#' \dontrun{
#' object=new("ref_textbox")
#' object<- charge(object,title="le titre",label="20")
#' win=gwindow(title="test ref_textbox")
#' group=ggroup(container=win,horizontal=FALSE)
#' choice(object) 
#' dispose(win)
#' }
setMethod("choice",signature=signature("ref_textbox"),definition=function(object,nomassign="ref_textbox") {
	  hlist=function(h,...){
		object@label<-svalue(choice)
		assign(nomassign,object,envir_stacomi)
		funout(paste("choice",object@label,"\n"))
	  }
	  group<-get("group",envir=envir_stacomi)
	  frame_text<-gframe(object@title)	
	  assign("frame_text",frame_text,envir_stacomi)
	  add(group,frame_text)
	  choice=glabel(text=object@label,container=frame_text,handler=hlist,editable=TRUE)
	  addhandlerchanged(choice,handler=hlist)
	})

#' Choice_c method for ReTextBox referential objects
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @param object An object of class \link{ref_textbox-class}
#' @param value The value to set
#' @param nomassign  The name with which the object will be assigned in envir_stacomi
setMethod("choice_c",signature=signature("ref_textbox"),definition=function(object,value,nomassign="ref_textbox") {
	  object@label<-value
	  assign(nomassign,object,envir_stacomi)
	  return(object)
	})
