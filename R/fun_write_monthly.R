#' This writes monthly data in t_reportmensuel_mens table
#' 
#' @note This function is launched by fun_Ecritreport_daily, the resum 
#' dataset is created by the \link{funstat} function
#' 
#' 
#' @param report_mig an object of class \code{\linkS4class{report_mig}}
#' @param resum data frame with summary per month
#' @param silent Suppresses messages
#' @export
fun_write_monthly<-function(report_mig,resum,silent){
  # voir essai_table_reportmensuel.sql pour le format du tableau
  # below not the most elegant way to do it but efficient
  
  t_reportmigrationmensuel_bme=stacomirtools::killfactor(
	  cbind(report_mig@dc@dc_selectionne,
		  report_mig@taxa@data$tax_code,
		  report_mig@stage@data$std_code,
		  unique(strftime(as.POSIXlt(report_mig@time.sequence),"%Y")), # une valeur
		  rep(rownames(resum),(ncol(resum)-2)), # nb of month except columns report and label
		  stack(resum,select=c(2:(ncol(resum)-1))),# stack re-ordonne les tab de donnees !  
		  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
		  substr(toupper(get("sch",envir=envir_stacomi)),1,nchar(toupper(get("sch",envir=envir_stacomi)))-1)
	  )
  )
  
  # la requete pour la suppression
  
  
  
  # ecriture dans la base...
  
  for (i in 1:nrow(t_reportmigrationmensuel_bme)) {
	requete=new("RequeteODBC")
	requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	requete@sql=paste("INSERT INTO ",get("sch",envir=envir_stacomi),"t_report_migMensuel_bme (",			
		"bme_dis_identifiant,bme_tax_code,bme_std_code,bme_annee,bme_labelquantite,bme_valeur,bme_mois,bme_horodateexport,bme_org_code)",
		" VALUES ('",paste(t_reportmigrationmensuel_bme[i,],collapse="','"),"');",sep="")
	invisible(utils::capture.output(stacomirtools::connect(requete)))
  } # end for
  if (!silent) funout(gettext("Writing monthly summary in the database\n",domain="R-stacomiR"))
} # end function

