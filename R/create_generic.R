
#' Generic method for choice (using the gwidget graphical interface)
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @keywords internal
setGeneric("choice",def=function(object,...) standardGeneric("choice"))
#' Generic method for multiple choice (using the gwidget notebook for multiple choice)
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @keywords internal
setGeneric("choicemult",def=function(object,...) standardGeneric("choicemult"))
#' Generic method for manual choice using the command line
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @keywords internal
setGeneric("choice_c",def=function(object,...) standardGeneric("choice_c"))
#' Generic method to load referentials
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
setGeneric("charge",def=function(object,...) standardGeneric("charge"))
#' Generic method to load referentials, with filters from the parent object in the database
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
#' @keywords internal
setGeneric("charge_with_filter",def=function(object,...) standardGeneric("charge_with_filter"))
#setGeneric("connect",def=function(object,...) standardGeneric("connect")) # package stacomirtools
#setGeneric("plot",def=function(x,y,...) standardGeneric("plot"))
#' Generic for prediction
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
setGeneric("model",def=function(object,...) standardGeneric("model"))
#' Generic method to load additional data
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
setGeneric("charge_complement",def=function(object,...) standardGeneric("charge_complement"))
#' Generic method for calculations
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
setGeneric("calcule",def=function(object,...) standardGeneric("calcule"))
#' Generic method to delete entires from the database
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
setGeneric("supprime",def=function(object,...) standardGeneric("supprime"))
#' Generic method write_database
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
setGeneric("write_database",def=function(object,...) standardGeneric("write_database"))
#' Generic method getvalue
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
setGeneric("getvalue",def=function(object,...) standardGeneric("getvalue"))
#' Generic method to forward outputs
#' @param object Object
#' @param ... Additional parms
#' @author cedric.briand
setGeneric("createmessage",def=function(object,...) standardGeneric("createmessage"))
#' Generic method to transform quantitative par into a qualitative one
#' @param object Object
#' @param ... Additional parms 
#' @author cedric.briand
setGeneric("setasqualitative",def=function(object,...) standardGeneric("setasqualitative"))


#' Environment where most objects from the package are stored and then loaded
#' by the charge method
#' 
#' envir_stacomi \code{envir_stacomi <- new.env(parent = baseenv())} is the
#' environment where most object created by the interface are stored
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
"envir_stacomi"
