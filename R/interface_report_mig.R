#' Interface for report_mig class, internal use, this function is called
#' by a handler in the main graphical interface
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_mig=function(){ 
  # the quitte() method removes everything assigned in envir_stacomi by report classes 
  quitte()
  # classes used along with report_mig by functions fungraph and fungraphcivelle
  # they are loaded there to be used later by methods "load" and "connect" of report_mig
  # so that connections to the database are not used later in examples (they are surrounded
  # by dontrun{}
  report_dc=new("report_dc")
  assign("report_dc",report_dc,envir = envir_stacomi)
  report_df=new("report_df")
  assign("report_df",report_df,envir = envir_stacomi)
  report_ope=new("report_ope")
  assign("report_ope",report_ope, envir=envir_stacomi)
  report_mig=new("report_mig")
  assign("report_mig",report_mig,envir = envir_stacomi)
  # see report_mig_mult for explaination
  # this is used internally by ref_dc
  objectreport="report_mig"
  assign("objectreport",objectreport,envir=envir_stacomi)
  funout(gettext("Loading of the lists for taxa, stages and counting devices\n",domain="R-stacomiR"))
  report_mig@taxa=charge(report_mig@taxa)
  report_mig@stage=charge(report_mig@stage)
  report_mig@dc=charge(report_mig@dc)  
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  assign("group",group,envir = envir_stacomi)  
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  add(ggroupboutons,group)
  choice(report_mig@timestep)
  choice(object=report_mig@dc,objectreport=report_mig,is.enabled=TRUE)	
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  toolbarlist = list(
	  Calc=gWidgets::gaction(handler=h_report_migcalc, action=report_mig, icon="new", label=gettext("calculate",domain="R-stacomiR"), tooltip=gettext("Calculation of numbers by time step",domain="R-stacomiR")),
	  Graph=gWidgets::gaction(handler=h_report_miggraph, icon="graph", label=gettext("graph",domain="R-stacomiR"), tooltip=gettext("Balance graphic",domain="R-stacomiR")),
	  Graph2=gWidgets::gaction(handler=h_report_miggraph2, icon="graph2", label=gettext("grcum",domain="R-stacomiR"), tooltip=gettext("Cumulative graphic",domain="R-stacomiR")),
	  Stat=gWidgets::gaction(handler=hTablereport_mig, icon="dataframe", label=gettext("stat",domain="R-stacomiR"), tooltip=gettext("Balance sheet in .csv",domain="R-stacomiR")),
	  write=gWidgets::gaction(handler=h_report_migwrite, icon="gtk-harddisk", label=gettext("write",domain="R-stacomiR"), tooltip=gettext("Writing daily summary in the database",domain="R-stacomiR")),
	  Out=gWidgets::gaction(handler=houtreport_mig, icon="gtk-info", label=gettext("code",domain="R-stacomiR"), tooltip=gettext("Code",domain="R-stacomiR")),
	  annuler=gWidgets::gaction(handler= quitte,icon = "close",label=gettext("quit",domain="R-stacomiR"))
  )    
  gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
  gWidgets::addSpring(group)
  dev.new()
}