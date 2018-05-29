#' Interface for class conditionEnv
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_env = function()
{
  quitte()
  report_env=new("report_env")
  funout(gettext("Loading of the monitoring stations\n",domain="R-stacomiR"))
  report_env@stationMesure=charge(report_env@stationMesure)
  assign("report_env",report_env,envir=envir_stacomi)
  
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  
  assign("group",group,envir = envir_stacomi)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  gWidgets::add(ggroupboutons,group)
  choice(report_env@horodatedebut,label=gettext("Begginning",domain="R-stacomiR"),
	  nomassign="report_env_date_debut",
	  funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
	  decal=-2)
  choice(report_env@horodatefin,
	  label=gettext("End",domain="R-stacomiR"),
	  nomassign="report_env_date_fin",
	  funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
	  decal=-1)
  choice(report_env@stationMesure)
  
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)
  
  toolbarlist = list(
	  Graph=gWidgets::gaction(handler=h_report_envgraph , icon = "graph",label="graph",tooltip=gettext("Summary graphic",domain="R-stacomiR")),
	  annuler=gWidgets::gaction(handler= quitte,icon = "close",label=gettext("Exit",domain="R-stacomiR"),domain="R-stacomiR"))
  gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
  gWidgets::addSpring(group)
  dev.new()
}