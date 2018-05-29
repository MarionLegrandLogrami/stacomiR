# Nom fichier :        ref_df   (classe)

#' Class "ref_df"
#' 
#' Representation of a fishway, contains description data of all fishways from
#' the database along with the selected fishways (df) (integer)
#' Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_df", df_selectionne=integer(), ouvrage=integer(),
#' data=data.frame())}.  
#' 

#' @param df_selectionne Object of class \code{"integer"} The identifier of the fishway
#' @param ouvrage Object of class \code{"integer"} The attached dam
#' @param data Object of class \code{"data.frame"} Data concerning the fishway
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family referential objects
setClass(Class="ref_df",representation=
		representation(df_selectionne="integer",ouvrage="integer",data="data.frame") )

setValidity("ref_df",method=function(object){
	  if (length(object@df_selectionne)!=0){		
		if (nrow(object@data)>0) {
		  concord<-object@df_selectionne%in%object@data$df					
		  if (any(!concord)){
			return(paste("No data for DF",object@df_selectionne[!concord]))
			
		  } else {
			return(TRUE)
		  }
		} else {
		  return("You tried to set a value for df_selectionne without initializing the data slot")
		}
	  }  else return(TRUE)
	  
	}   
)
#' Loading method for DF referential objects
#' @param object An object of class \link{ref_df-class}
#' @return An object of class ref_df
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_df")
#' charge(object)
#' }
setMethod("charge",signature=signature("ref_df"),definition=function(object) {
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@sql = paste("select dis_identifiant as DF,",
		  " dis_date_creation,",
		  " dis_date_suppression,",
		  " dis_commentaires,",
		  " dif_ouv_identifiant,",
		  " ouv_libelle,",
		  " dif_code as DF_code,",
		  " dif_localisation,",
		  " dif_orientation,",
		  " tdf_libelle as type_DF",
		  " from ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_dispositiffranchissement_dif ON dif_dis_identifiant=dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"tj_dfesttype_dft ON dif_dis_identifiant=dft_df_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_ouvrage_ouv on dif_ouv_identifiant=ouv_identifiant",   
		  " JOIN ref.tr_typedf_tdf ON tdf_code=dft_tdf_code",
		  " ORDER BY dis_identifiant;",sep="")
	  requete<-stacomirtools::connect(requete) 
	  object@data<-requete@query
	  return(object)
	})

#' Graphical method to choose a fishway through the interface
#' @param object An object of class \link{ref_df-class}
#' @note the choice method assigns an object of class ref_df in the environment envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples  
#'  \dontrun{ 
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("ref_df")
#' object<-charge(object)
#' choice(object)
#' }
#' @keywords internal
setMethod("choice",signature=signature("ref_df"),definition=function(object) {
	  if (nrow(object@data) > 0){
		h_report_df=function(h,...){
		  object@df_selectionne=svalue(choice)
		  object@ouvrage= object@data$dif_ouv_identifiant[object@data$df%in%object@df_selectionne]
		  #cat("passe par la")
		  assign("ref_df",object,envir_stacomi)
		  funout(gettext("Fishway selected\n",domain="R-stacomiR"))
		  #dispose(winst)
		} 
		# Handler d'affichage du tableau
		h_report_dfi=function(h,...){
		  w=gwindow(gettext("Fishways data",domain="R-stacomiR"),width=400)
		  wg=ggroup(horizontal=FALSE,container=w)
		  tab=gtable(object@data[,c(1,6,7)],chosencol=1,multiple=FALSE,expand=TRUE, container=wg)
		  bg<-ggroup(container=wg)
		  addSpring(bg)
		  gbutton(gettext("close",domain="R-stacomiR"), container=bg, handler = function(h,...) dispose(w))
		}
		group<-get("group",envir=envir_stacomi)
		frameDF=gframe(gettext("Fishway choice",domain="R-stacomiR"),container=group)
		DF_identifiant=object@data$df
		choice=gdroplist(DF_identifiant,container=frameDF,handler=h_report_df)
		gbutton(gettext("Table",domain="R-stacomiR"), container=frameDF,handler=h_report_dfi)
		gbutton("OK", container=frameDF,handler=h_report_df)
	  } else {
		funout(gettext("No fishway in the database (the query returns 0 entry)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	})


#' Command line interface to choose a fishway
#' 
#' the choice_c method is intendedto have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line.  The parameters for dF are transformed to integer as the ref_df only 
#' takes integer in the df slots. 
#' DF are third in hierarchy in the stacomi database Station>ouvrage>DF>DC>operation. This class is only used in the
#' report_df class.
#' @param object an object of class \link{ref_df-class}
#' @param df a character vector of df chosen
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("ref_df")
#' object<-charge(object)
#' objectreport=new("report_mig_mult")
#' choice_c(object=object,objectreport=objectreport,dc=1)
#' }
setMethod("choice_c",signature=signature("ref_df"),definition=function(object,df) {
	  #object<-ref_df
	  if (class(df)=="numeric") {
		df<-as.integer(df) 
	  } else if (class(df)=="character"){
		options(warn = -1)
		df=as.integer(as.numeric(df))
		options(warn = 0)
	  }
	  if (any(is.na(df))) stop ("NA values df")			
	  object@df_selectionne<-df
	  object@ouvrage= object@data$dif_ouv_identifiant[object@data$df%in%object@df_selectionne]
	  validObject(object) 		
# the method validObject verifies that the df is in the data slot of ref_df			
	  
	  assign("ref_df",object,envir=envir_stacomi)
	  return(object)
	})