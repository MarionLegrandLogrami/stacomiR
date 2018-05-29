#' Class "ref_parquan"
#' 
#' Class enabling to load the list of quantitative parameters and to select one
#' of them. It inherits from 'ref_par', uses its 'choice' method
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
#' @family referential objects
#' @include ref_par.R
setClass(Class="ref_parquan",contains="ref_par")

#' Loading method for Reparquan referential objects
#' @param object An object of class \link{ref_parquan-class}
#' @return An S4 object of class ref_parquan
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  object=new("ref_parquan")
#'  charge(object)
#' }
setMethod("charge",signature=signature("ref_parquan"),definition=function(object) {
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@sql= "SELECT * FROM ref.tg_parametre_par
		  INNER JOIN ref.tr_parametrequantitatif_qan ON qan_par_code=par_code"
	  requete<-stacomirtools::connect(requete)
	  #funout(gettext("The query to load parameters is done \n",domain="R-stacomiR"))
	  object@data<-requete@query
	  return(object)
	})


#' Loading method for Reparquan referential objects searching only those parameters existing for a DC, a Taxon, and a stage
#' @param object An object of class \link{ref_parquan-class}
#' @param dc_selectionne The dc set in the report object
#' @param taxa_selectionne The taxa set in the report object
#' @param stage_selectionne The stage set in the report object
#' @return An S4 object of class ref_parqualn
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{
#'  dc_selectionne=6
#'	taxa_selectionne=2038
#'  stage_selectionne="AGJ"
#'  object=new("ref_parquan")
#'  charge_with_filter(object,dc_selectionne,taxa_selectionne,stage_selectionne)
#' }		
setMethod("charge_with_filter",signature=signature("ref_parquan"),definition=function(object,dc_selectionne,taxa_selectionne,stage_selectionne) {
	  requete=new("RequeteODBCwhere")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@select=paste("SELECT DISTINCT ON (par_code) par_code, par_nom", 
		  " FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic on dis_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_operation_ope on ope_dic_identifiant=dic_dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot on lot_ope_identifiant=ope_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"tj_caracteristiquelot_car on car_lot_identifiant=lot_identifiant",
		  " JOIN ref.tg_parametre_par on par_code=car_par_code",
		  " JOIN ref.tr_parametrequantitatif_qan ON qan_par_code=par_code",sep="")
	  requete@where=paste("where dis_identifiant in ",vector_to_listsql(dc_selectionne))
	  requete@and=paste("and lot_tax_code in ",vector_to_listsql(taxa_selectionne)," and lot_std_code in ",vector_to_listsql(stage_selectionne),"",sep="")
	  requete@order_by="ORDER BY par_code"  
	  requete<-stacomirtools::connect(requete) 
	  object@data<-requete@query
	  return(object)
	})


