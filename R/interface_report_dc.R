#' interface for report_dc class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_dc = function()
{
  quitte()
  report_dc=new("report_dc")
  assign("report_dc",report_dc,envir=envir_stacomi)
  
  funout(gettext("Loading of the list for fishways and choice of the time step\n",domain="R-stacomiR"))
  report_dc@dc=charge(report_dc@dc)
  
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  
  assign("group",group,envir = envir_stacomi)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  gWidgets::add(ggroupboutons,group)
  choice(report_dc@dc)
  choice(report_dc@horodatedebut,
	  label=gettext("Start",domain="R-stacomiR"),
	  nomassign="report_dc_date_debut",
	  funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
	  decal=-2)
  choice(report_dc@horodatefin,
	  label=gettext("End",domain="R-stacomiR"),
	  nomassign="report_dc_date_fin",
	  funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
	  decal=-1)
  
  #toolbarlist$Calc$handler = connect(report_dc)
  #toolbarlist$Calc$icon = "dataframe"
  #getStockIcons(toolkit=guiToolkit())
  
  bar_chart=gWidgets::gaction(label="barchart",
	  icon="barplot",
	  handler=funbarchartDC,
	  tooltip=gettext("Monthly graph",domain="R-stacomiR"))
  bar_chart1=gWidgets::gaction(label="barchart_fct",
	  icon="barplot",
	  handler=funbarchart1DC,
	  tooltip=gettext("Another monthly graph",domain="R-stacomiR"))	
  a_box=gWidgets::gaction(label=gettext("box"),
	  icon="graph2",
	  handler=funboxDC,
	  tooltip=gettext("Boxplot",domain="R-stacomiR"))
  a_table=gWidgets::gaction(label=gettext("table",domain="R-stacomiR"),
	  icon="dataframe",
	  handler=funtableDC,
	  tooltip=gettext("Table",domain="R-stacomiR"))
  aQuit=gWidgets::gaction(label="Quitter",
	  icon="close", 
	  handler=quitte,
	  tooltip=gettext("Exit",domain="R-stacomiR"))
  aOut=gWidgets::gaction(label=gettext("code"),
	  handler=houtDC, 
	  icon="gtk-info",
	  tooltip=gettext("code",domain="R-stacomiR"))    
  toolbarlist <- list(
	  barchart=bar_chart, 
	  barchart1=bar_chart1,
	  box= a_box,
	  table=a_table,
	  out=aOut,
	  Quit = aQuit)    
  add(group, gmenu(toolbarlist))
  gWidgets::addSpring(group)
}