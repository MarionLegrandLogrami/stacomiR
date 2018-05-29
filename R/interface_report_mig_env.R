#' interface for report_migConditionEnv class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_mig_env = function()
{
  quitte()
  r_mig_env=new("report_mig_env")
  assign("r_mig_env",r_mig_env,envir=envir_stacomi)	
  funout(gettext("Loading of the lists for taxa, stages, counting devices and monitoring stations\n",domain="R-stacomiR"))
  r_mig_env@report_env@stationMesure=charge(r_mig_env@report_env@stationMesure)
  #(destroys everything in envir_stacomi except stuff required at to level)
  objectreport="report_mig_mult"
  # the following name is created by the interface
  # as I can't get the name from within the function (deparse(substitute(objectreport)) does not return
  # "report_mig_mult" see ref_dc choice_c method)
  # so this will allow to assign "report_mig_mult" in envir_stacomi while using other class
  # like ref_dc
  assign("objectreport",objectreport,envir=envir_stacomi)
  r_mig_env@report_mig_mult=new("report_mig_mult")
  assign("report_mig_mult",r_mig_env@report_mig_mult,envir = envir_stacomi)
  report_dc=new("report_dc")
  assign("report_dc",report_dc,envir = envir_stacomi)
  report_df=new("report_df")
  assign("report_df",report_df,envir = envir_stacomi)
  report_ope=new("report_ope")
  assign("report_ope",report_ope, envir=envir_stacomi)
  report_mig=new("report_mig")
  assign("report_mig",report_mig,envir = envir_stacomi)
  
  
  r_mig_env@report_mig_mult@taxa=charge(r_mig_env@report_mig_mult@taxa)
  r_mig_env@report_mig_mult@stage=charge(r_mig_env@report_mig_mult@stage)
  r_mig_env@report_mig_mult@dc=charge(r_mig_env@report_mig_mult@dc)
  group = ggroup(horizontal=TRUE)   # doit toujours s'appeller group
  assign("group",group,envir = envir_stacomi)  
  choice(r_mig_env@report_env@stationMesure)
  notebook <- gnotebook(container=group)	
  assign("notebook",notebook,envir=envir_stacomi)
  size(notebook)<-c(400,300)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  add(ggroupboutons,group)
  
  choicemult(r_mig_env@report_mig_mult@timestep)
  choicemult(r_mig_env@report_mig_mult@dc,objectreport=r_mig_env@report_mig_mult,is.enabled=TRUE)
  svalue(notebook)<-1
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  
  toolbarlist = list(
	  Calc=gWidgets::gaction(handler = hbmmCEcalc,	
		  icon = "new",
		  label="calcul",
		  tooltip=gettext("Calculation of environnemental conditions by time step",domain="R-stacomiR")),
	  Graph=gWidgets::gaction(handler = hbmmCEgraph,
		  icon = "graph",
		  label="graph",
		  tooltip=gettext("Balance graphic",domain="R-stacomiR")),
	  annuler=gWidgets::gaction(handler= quitte,
		  icon = "close",
		  label="quitter"))
  assign("toolbarlist",toolbarlist,envir=envir_stacomi)
  enabled(toolbarlist[["Graph"]])<-FALSE
  gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
  assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)	
  gWidgets::addSpring(group)
  return(invisible(NULL))
}