#' function to create daily statistics
#' @param tableau A table
#' @param time.sequence Time sequence from report_mig and report_mig_mult
#' @param taxa A taxa
#' @param stage A stage
#' @param DC A counting device
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funstat_daily=function(tableau,time.sequence,taxa,stage,DC){
  mois=months(time.sequence)
  moislab=unique(mois)
  annee=unique(strftime(as.POSIXlt(time.sequence),"%Y"))
  somme=tapply(tableau$Effectif_total, mois, sum, na.rm=TRUE) # sums
  moyennes_journalieres=tapply(tableau$Effectif_total, mois, mean, na.rm=TRUE) # means
  ecarts_types=tapply(tableau$Effectif_total, mois, stats::sd, na.rm=TRUE) # std. deviations
  nombre=as.integer(tapply(tableau$Effectif_total, mois, function(x) sum(!is.na(x)))) # counts
  resum=rbind(nombre,somme,moyennes_journalieres,ecarts_types)
  
  if (taxa=="Anguilla anguilla"& stage=="civelle") 
  {
	poids_depuis_effectif=tapply(tableau$poids_depuis_effectif, mois,  sum, na.rm=TRUE)
	poids_mesure=tapply(tableau$Poids_total, mois,  sum, na.rm=TRUE)
	Poids_total=poids_depuis_effectif+ poids_mesure
	resum=rbind(nombre,somme,moyennes_journalieres,ecarts_types,poids_depuis_effectif,poids_mesure,Poids_total)
  }
  
  resum=resum[,moislab]
  resum=data.frame(resum)
  resum["somme","report"]=round(sum(tableau$Effectif_total, na.rm=TRUE),2)
  resum["moyennes_journalieres","report"]=round(mean(tableau$Effectif_total, na.rm=TRUE),2)
  resum["ecarts_types","report"]=round(stats::sd(tableau$Effectif_total, na.rm=TRUE),2)
  if (taxa=="Anguilla anguilla"& stage=="civelle") 
  {
	resum["poids_depuis_effectif","report"]=round(sum(tableau$poids_depuis_effectif, na.rm=TRUE),2)
	resum["poids_mesure","report"]=round(sum(tableau$Poids_total, na.rm=TRUE),2)
	resum["Poids_total","report"]=round(sum(Poids_total, na.rm=TRUE),2)
  }
  resum=cbind("label"=paste("DC",DC,taxa,stage,annee,sep="_"),resum)
  funout(paste(DC,taxa,stage,annee,"\n"))
  
  print( resum["somme",])
  return(resum)
}