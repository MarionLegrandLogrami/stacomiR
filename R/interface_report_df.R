#' interface for report_df class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_df = function()
{
  quitte()
  
  report_df=new("report_df")
  assign("report_df",report_df,envir=envir_stacomi)    
  funout(gettext("Loading of the list for fishways and choice of the time step\n",domain="R-stacomiR"))
  report_df@df=charge(report_df@df)    
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  assign("group",group,envir=envir_stacomi)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  gWidgets::add(ggroupboutons,group)
  
  choice(report_df@df)
  # here decale =-1 or -2 will make the report for the year preceeding the current date
  choice(report_df@horodatedebut,
	  label=gettext("start",domain="R-stacomiR"),
	  nomassign="report_df_date_debut",
	  funoutlabel=gettext("The beginning date has been chosen\n",domain="R-stacomiR"),
	  decal=-2)
  choice(report_df@horodatefin,
	  label=gettext("End",domain="R-stacomiR"),
	  nomassign="report_df_date_fin",
	  funoutlabel=gettext("The ending date has been chosen\n",domain="R-stacomiR"),
	  decal=-1)
  
  bar_chart=gWidgets::gaction(label="barchart_typefct",icon="barplot",handler=funbarchartDF,tooltip=gettext("Monthly graph",domain="R-stacomiR"))
  bar_chart1=gWidgets::gaction(label="barchart_fct",icon="barplot",handler=funbarchart1DF,tooltip=gettext("Monthly graph",domain="R-stacomiR"))
  a_box=gWidgets::gaction(label="box",icon="graph2",handler=funboxDF,tooltip=gettext("Boxplot",domain="R-stacomiR"))
  aChart=gWidgets::gaction(label="chart",icon="graph",handler=funchartDF,tooltip=gettext("Calendar",domain="R-stacomiR"))
  a_table=gWidgets::gaction(label="table",icon="dataframe",handler=funtableDF,tooltip=gettext("Table",domain="R-stacomiR"))
  aOut=gWidgets::gaction(label="code",handler=houtDF, icon="gtk-info", tooltip=gettext("Code",domain="R-stacomiR"))
  aQuit=gWidgets::gaction(label="Close",icon="close", handler=quitte,tooltip=gettext("Exit",domain="R-stacomiR"))
  
  toolbarlist <- list(
	  barchart=bar_chart, 
	  barchart1=bar_chart1,
	  box= a_box,
	  chart=aChart,
	  table=a_table,
	  out=aOut,
	  Quit = aQuit)
  
  add(group, gmenu(toolbarlist))
  gWidgets::addSpring(group)
}