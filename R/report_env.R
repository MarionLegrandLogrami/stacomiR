#' class report_env simple output of one or several environmental
#' conditions...
#' 
#' Annual overview of environmental conditions. This class enables to draw some plot, but will mostly used to build
#' joined graphs crossing the information from \link{report_mig_mult-class} and \link{report_mig_env-class}
#' 
#' @include ref_horodate.R 
#' @include ref_env.R
#' @include create_generic.R
#' @include utilities.R
#' @slot horodatedebut \link{ref_horodate-class}
#' @slot horodatefin \link{ref_horodate-class}
#' @slot stationMesure \link{ref_env-class}
#' @slot data \code{data.frame}
#' @author cedric.briand"at"eptb-vilaine.fr
#' @family report Objects
#' @keywords classes
#' @aliases report_env
#' @keywords classes
#' @example inst/examples/report_env-example.R
#' @export 
setClass(Class="report_env",
	representation=representation(			
		stationMesure="ref_env",
		horodatedebut="ref_horodate",
		horodatefin="ref_horodate",
		data="data.frame"
	),
	prototype=prototype(
		horodatedebut=new("ref_horodate"),
		horodatefin=new("ref_horodate"),
		stationMesure=new("ref_env"),
		data=data.frame())
)


#' connect method for report_env class
#' @param object An object of class \link{report_env-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @return an object of report_env class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases connect.report_env
#' @export
setMethod("connect",signature=signature("report_env"),definition=function(object,silent=FALSE) {
	  #object<-r_env
	  requete=new("RequeteODBCwheredate")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@datedebut=strptime(object@horodatedebut@horodate,format="%Y-%m-%d")
	  requete@datefin=strptime(object@horodatefin@horodate,format="%Y-%m-%d")
	  requete@colonnedebut="env_date_debut"
	  requete@colonnefin="env_date_fin"
	  requete@select=paste("SELECT", 
		  " env_date_debut,",
		  " env_date_fin,",
		  " env_methode_obtention,",
		  " val_libelle as env_val_identifiant,",
		  " env_valeur_quantitatif,",
		  " env_stm_identifiant",
		  " FROM ",get("sch",envir=envir_stacomi),"tj_conditionenvironnementale_env",
		  " LEFT JOIN ref.tr_valeurparametrequalitatif_val on env_val_identifiant=val_identifiant",sep="")
	  requete@order_by<-"ORDER BY env_stm_identifiant, env_date_debut"			
	  tmp<-vector_to_listsql(object@stationMesure@data$stm_identifiant)
	  requete@and=paste(" AND env_stm_identifiant IN ",tmp )			
	  requete<-stacomirtools::connect(requete)			
	  object@data<-stacomirtools::killfactor(requete@query)
	  if (!silent) funout(gettext("Environmental conditions loading query completed\n",domain="R-stacomiR"))
	  return(object)
	}
)
#' command line interface for report_env class
#' @param object An object of class \link{report_env-class}
#' @param stationMesure A character, the code of the monitoring station, which records environmental parameters \link{choice_c,ref_env-method}
#' @param datedebut The starting date as a character, formats like \code{\%Y-\%m-\%d} or \code{\%d-\%m-\%Y} can be used as input
#' @param datefin The finishing date of the report, for this class this will be used to calculate the number of daily steps.
#' @param silent Boolean default FALSE, if TRUE information messages not displayed.
#' @return An object of class \link{report_env-class}
#' The choice_c method fills in the data slot for ref_env and  and then 
#' uses the choice_c methods of these object to select the data.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#'  @aliases choice_c.report_env
#' @export
setMethod("choice_c",signature=signature("report_env"),definition=function(object,stationMesure,datedebut,datefin,silent=FALSE){
	  # code for debug using r_mig example
	  #stationmesure=c("temp_gabion","coef_maree");datedebut="2008-01-01";datefin="2008-12-31";silent=FALSE
	  r_env<-object
	  r_env@stationMesure=charge(r_env@stationMesure)
	  # loads and verifies the stationmesure (selects the relevant lines in the table
	  r_env@stationMesure<-choice_c(object=r_env@stationMesure,stationMesure)
	  r_env@horodatedebut<-choice_c(object=r_env@horodatedebut,
		  nomassign="report_env_date_debut",
		  funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
		  horodate=datedebut, 
		  silent=silent)
	  r_env@horodatefin<-choice_c(r_env@horodatefin,
		  nomassign="report_env_date_fin",
		  funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
		  horodate=datefin,
		  silent=silent)
	  return(r_env)
	})
#' charge method for report_env class
#' @param object An object of class \link{report_env-class}
#' @param silent Default FALSE, if TRUE the program should no display messages
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @aliases charge.report_env
#' @keywords internal
setMethod("charge",signature=signature("report_env"),definition=function(object,silent) {
	  
	  if (exists("ref_env",envir_stacomi)) {
		object@stationMesure<-get("ref_env",envir_stacomi)
	  } else {
		funout(gettext("You need to choose a monitoring station, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
	  }     
	  
	  if (exists("report_env_date_debut",envir_stacomi)) {
		object@horodatedebut@horodate<-get("report_env_date_debut",envir_stacomi)
	  } else {
		funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  
	  if (exists("report_env_date_fin",envir_stacomi))  {
		object@horodatefin@horodate<-get("report_env_date_fin",envir_stacomi)
	  }else {
		funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)
	  }      		
	  return(object)
	})


#' h_report_envgraph Internal method
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
h_report_envgraph = function(h,...) 
{	
  report_env<-get("report_env",envir=envir_stacomi)
  report_env=charge(report_env)
  report_env=connect(report_env)
  plot(report_env)
}	
#' Plot method for report_env
#' @param x An object of class \link{report_env-class}
#' @param silent Stops displaying the messages
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.report_env
#' @export
setMethod("plot", signature(x = "report_env", y = "missing"), definition=function(x,silent=FALSE){ 
	  # le dataframe contenant le res de la requete
	  r_env<-x
	  dat<-r_env@data	
	  if(length(unique(dat$env_stm_identifiant))!=0){
		# le layout pour l'affichage des graphiques
		vplayout <- function(x, y) { grid::viewport(layout.pos.row = x, layout.pos.col = y)   }
		grid::grid.newpage()
		grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(unique(dat$env_stm_identifiant)),1,just="center")))
		lesGraphes=list()
		if(length(unique(dat$env_stm_identifiant))!= nrow(r_env@stationMesure@data))
		{
		  funout(gettext("Some monitoring stations lack associated values (no environmental data)\n",domain="R-stacomiR"))
		}
		
		# for all stationmesure selected
		for (i in 1:length(unique(dat$env_stm_identifiant)))
		{
		  # the identifier of the current station
		  stmidentifiant <- unique(dat$env_stm_identifiant)[i]
		  
		  # the line of report_env@stationMesure currently processed in the loop
		  stm <- r_env@stationMesure@data[r_env@stationMesure@data$stm_identifiant==stmidentifiant,]
		  
		  # all measures for the selected station
		  nameColonne <- as.character(stm$stm_libelle)
		  datstm <- stacomirtools::chnames(dat,"env_valeur_quantitatif", nameColonne) 
		  datstm <- datstm[datstm$env_stm_identifiant==stmidentifiant,]
		  
		  # creating the plot
		  g<-ggplot(datstm,aes_string(x="env_date_debut",y=nameColonne))  
		  g<-g+geom_line(aes_string(colour=nameColonne))+scale_y_continuous(stm$stm_libelle)+
			  scale_x_datetime(name="date")
		  
		  # printing plot on screen
		  print(g, vp=vplayout(i,1))
		} 
	  } else {
		funout(gettext("No environmental conditions values for selected monitoring stations (report_env.R)\n",domain="R-stacomiR"),arret=TRUE)
	  }	
	  
	})   
