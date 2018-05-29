# Nom fichier :        ref_periode.R

#' Class "ref_period" referential class
#' 
#' ref_period referential class to choose a period
#' 
#' @note pgval are used by seq.POSIXt
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_period", ...)}.
#' @keywords classes
#' @slot data="data.frame" providing correspondance between period and their English names
#' @family referential objects
setClass(Class="ref_period",representation=
              representation(
                      data="data.frame"          
              ),
	prototype=prototype(
        data=data.frame("id"=c("jour","semaine","quinzaine","mois"),
                      "pgval"=c("day","week","2 week","month"))
    )
)
#' Returns the POSIXt value of a given name
#' @param object An object of class \link{ref_period-class}
#' @param id one of "jour", "semaine", "quinzaine", "mois"
#' @return "a character to be used in seq.POSIXt
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  getvalue(new("ref_period"),"quinzaine")
#' }
#' @export
setMethod("getvalue",signature=signature("ref_period"), definition=function(object,id)
      {return(as.character(object@data[object@data$id==id,"pgval"]))
      } ) 