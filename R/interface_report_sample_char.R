#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_sample_char = function()
{  
  quitte()
  report_sample_char=new("report_sample_char")
  assign("report_sample_char",report_sample_char,envir = envir_stacomi)
  
  funout(gettext("Loading of the view vue_ope_lot, and choice of the counting device and of the time steps\n",domain="R-stacomiR"))
  report_sample_char@dc=charge(report_sample_char@dc)
  report_sample_char@taxa=charge(report_sample_char@taxa)
  report_sample_char@stage=charge(report_sample_char@stage)
  report_sample_char@par=charge(report_sample_char@par)    
  
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  
  assign("group",group,envir = envir_stacomi)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  gWidgets::add(ggroupboutons,group)
  gl=glabel(text=gettext("Batch summary",domain="R-stacomiR"),container=group)
  # dans l'ordre 
  # dans le handler, modifier le contenu de l'object fils si il existe
  # supprimer les widgets fils si ils existent (appel de la methode delete)
  # appeller la methode choice pour l'affichage du fils si il existe
  
  
  choice(report_sample_char@horodatedebut,label=gettext("Fist timestamp",domain="R-stacomiR"),
	  nomassign="report_sample_char_date_debut",
	  funoutlabel=gettext("The beginning date has been chosen\n",domain="R-stacomiR"),
	  decal=-2)
  choice(report_sample_char@horodatefin,label=gettext("Last timestamp",domain="R-stacomiR"),
	  nomassign="report_sample_char_date_fin",
	  funoutlabel=gettext("The ending date has been chosen",domain="R-stacomiR"),
	  decal=-1)
  
  choice(report_sample_char@dc,objectreport=report_sample_char,is.enabled=TRUE)
  # Les methodes choice nextes sont passees en cascade e l'interieur des methodes choice
  #choice(report_sample_char@taxa,is.enabled=FALSE)
  #choice(report_sample_char@stage,is.enabled=FALSE)
  #choice(report_sample_char@par,is.enabled=FALSE)
  #toolbarlist$Calc$handler = connect(report_dc)
  #toolbarlist$Calc$icon = "dataframe"
  #getStockIcons(toolkit=guiToolkit())
  
  aPoint=gWidgets::gaction(label=gettext("dotplot",domain="R-stacomiR"),
	  icon="gWidgetsRGtk2-cloud",
	  handler=funpointreport_sample_char,
	  tooltip=gettext("dotplot",domain="R-stacomiR"))
  aDensity=gWidgets::gaction(label=gettext("density",domain="R-stacomiR"),
	  icon="gWidgetsRGtk2-density",
	  handler=fundensityreport_sample_char,
	  tooltip=gettext("density",domain="R-stacomiR"))
  a_boxplot=gWidgets::gaction(label=gettext("boxplot",domain="R-stacomiR"),
	  icon="gWidgetsRGtk2-boxplot",
	  handler=funboxplotreport_sample_char,
	  tooltip=gettext("boxplot",domain="R-stacomiR"))
  a_table=gWidgets::gaction(label=gettext("table",domain="R-stacomiR"),
	  icon="dataframe",
	  handler=funtablereport_sample_char,
	  tooltip=gettext("dataframe",domain="R-stacomiR"))
  aQuit=gWidgets::gaction(label=gettext("Exit",domain="R-stacomiR"),
	  icon="close",
	  handler=quitte,tooltip="Exit")
  
  toolbarlist <- list(    
	  plot=aPoint,
	  density=aDensity, 
	  boxplot= a_boxplot,
	  table=a_table,
	  Quit = aQuit)
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
  assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)	
  gWidgets::addSpring(group)
}
