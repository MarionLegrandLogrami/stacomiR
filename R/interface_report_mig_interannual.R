#' interface for report_mig_interannual class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_mig_interannual = function()
{
  quitte() # vidange de l'interface
  report_mig_interannual=new("report_mig_interannual")
  assign("report_mig_interannual",report_mig_interannual,envir=envir_stacomi)
  funout(gettext("Loading of the existing daily summaries\n",domain="R-stacomiR"))
  report_mig_interannual@start_year=charge(report_mig_interannual@start_year)
  report_mig_interannual@end_year=charge(report_mig_interannual@end_year)
  report_mig_interannual@dc=charge(report_mig_interannual@dc)
  report_mig_interannual@taxa=charge(report_mig_interannual@taxa)
  report_mig_interannual@stage=charge(report_mig_interannual@stage)
  
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  assign("group",group,envir = envir_stacomi)  
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  add(ggroupboutons,group)
  
  # pour preselectionner une date on lui fournit l'indice de la date dans le ref_year. indice = 11 pour 2005
  
  choice(report_mig_interannual@start_year,
	  nomassign="start_year",
	  funoutlabel=gettext("The year of beginning has been chosen\n",domain="R-stacomiR"),
	  titleFrame=gettext("Beginning year",domain="R-stacomiR"),
	  preselect=which(report_mig_interannual@start_year@data==min(report_mig_interannual@start_year@data)))
  choice(report_mig_interannual@end_year,
	  nomassign="end_year",
	  funoutlabel=gettext("The last year has been chosen\n",domain="R-stacomiR"),
	  titleFrame=gettext("Ending year",domain="R-stacomiR"),
	  preselect=which(report_mig_interannual@start_year@data==max(report_mig_interannual@end_year@data)))
  choice(report_mig_interannual@dc,objectreport=report_mig_interannual,is.enabled=TRUE)
  
  # dans l'ordre 
  # dans le handler, modifier le contenu de l'object fils si il existe
  # supprimer les widgets fils si ils existent (appel de la methode delete)
  # appeller la methode choice pour l'affichage du fils si il existe
  ### premiere toobar
  
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  
  toolbarlist1 = list(
	  aGraph=gWidgets::gaction(label="all",
		  icon="lines",
		  handler=hgraph_report_mig_interannual,
		  tooltip=gettext("Migration of all the years in the same graphic",domain="R-stacomiR")),
	  aGraph7=gWidgets::gaction(label="cum15",
		  icon="curve",
		  handler=hgraph_report_mig_interannual7,
		  tooltip=gettext("Migration of all the years in the same graphic, cumulated",domain="R-stacomiR")),
	  aGraph3=gWidgets::gaction(label="step",
		  icon="graph2",
		  handler=hgraph_report_mig_interannual3,
		  tooltip=gettext("cumulated migrations %",domain="R-stacomiR")),
	  aSummary=gWidgets::gaction(handler=hsummaryreport_migInterannuelle,
		  icon="dataframe", 
		  label="stat",
		  tooltip="summary"),  
	  aQuit=gWidgets::gaction(label="fermer",
		  icon="close", 
		  handler=quitte,
		  tooltip=gettext("Exit",domain="R-stacomiR"))
  )
  toolbarlist2=list(
	  aGraph2=gWidgets::gaction(label=gettext("day",domain="R-stacomiR"),
		  icon="hist",handler=hgraph_report_mig_interannual2,
		  tooltip=gettext("Daily migration",domain="R-stacomiR")),
	  aGraph4=gWidgets::gaction(label=gettext("week",domain="R-stacomiR"),
		  icon="hist",handler=hgraph_report_mig_interannual4,
		  action="semaine",
		  tooltip=gettext("weekly migration",domain="R-stacomiR")),
	  aGraph5=gWidgets::gaction(label=gettext("fortnight",domain="R-stacomiR"),
		  icon="hist",
		  handler=hgraph_report_mig_interannual4,
		  action="quinzaine",
		  tooltip=gettext("Fortnight Migration",domain="R-stacomiR")),
	  aGraph6=gWidgets::gaction(label=gettext("month",domain="R-stacomiR"),
		  icon="hist",handler=hgraph_report_mig_interannual4,
		  action="mois",
		  tooltip=gettext("Monthly migration",domain="R-stacomiR"))
  )
  toolbarlist3=list(
	  aGraph1=gWidgets::gaction(label=gettext("week",domain="R-stacomiR"),
		  icon="gWidgetsRGtk2-points",handler=hgraph_report_mig_interannual5,
		  action="semaine",
		  tooltip=gettext("weekly migration",domain="R-stacomiR")),
	  aGraph2=gWidgets::gaction(label=gettext("fortnight",domain="R-stacomiR"),
		  icon="gWidgetsRGtk2-points",
		  handler=hgraph_report_mig_interannual5,
		  action="quinzaine",
		  tooltip=gettext("Fortnight migration",domain="R-stacomiR")),
	  aGraph3=gWidgets::gaction(label=gettext("month",domain="R-stacomiR"),
		  icon="gWidgetsRGtk2-points",handler=hgraph_report_mig_interannual5,
		  action="mois",
		  tooltip=gettext("Monthly migration",domain="R-stacomiR"))
  )
  add(ggroupboutonsbas, gtoolbar(toolbarlist1))
  add(ggroupboutonsbas, gtoolbar(toolbarlist2)) 
  add(ggroupboutonsbas, gtoolbar(toolbarlist3))
  gWidgets::addSpring(group)
  dev.new() 
}