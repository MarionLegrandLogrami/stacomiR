#' graphical interface for report_mig_mult class
#' 
#' launches the graphical interface after the main gwidget dialog has been launched
#' This function is called from a handler in the main graphical interface
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_mig_mult=function(){
  quitte() # quitte must be the first in interface methods 
  #(destroys everything in envir_stacomi except stuff required at to level)
  objectreport="report_mig_mult"
  # the following name is created by the interface
  # as I can't get the name from within the function (deparse(substitute(objectreport)) does not return
  # "report_mig_mult" see ref_dc choice_c method)
  # so this will allow to assign "report_mig_mult" in envir_stacomi while using other class
  # like ref_dc
  assign("objectreport",objectreport,envir=envir_stacomi)
  report_mig_mult=new("report_mig_mult")
  assign("report_mig_mult",report_mig_mult,envir = envir_stacomi)
  report_dc=new("report_dc")
  assign("report_dc",report_dc,envir = envir_stacomi)
  report_df=new("report_df")
  assign("report_df",report_df,envir = envir_stacomi)
  report_ope=new("report_ope")
  assign("report_ope",report_ope, envir=envir_stacomi)
  report_mig=new("report_mig")
  assign("report_mig",report_mig,envir = envir_stacomi)
  report_mig_mult@taxa=charge(report_mig_mult@taxa)
  report_mig_mult@stage=charge(report_mig_mult@stage)
  report_mig_mult@dc=charge(report_mig_mult@dc)   	
  group = ggroup(horizontal=TRUE)   # doit toujours s'appeller group
  assign("group",group,envir = envir_stacomi)  
  notebook <- gnotebook(container=group)	
  assign("notebook",notebook,envir=envir_stacomi)
  size(notebook)<-c(400,400)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  add(ggroupboutons,group)
  choicemult(report_mig_mult@timestep)
  choicemult(report_mig_mult@dc,objectreport=report_mig_mult,is.enabled=TRUE)
  svalue(notebook)<-1
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  toolbarlist = list(
	  Calc=gWidgets::gaction(handler=h_report_mig_multcalc,  icon="new", 
		  label="calcul", 
		  tooltip=gettext("Calculation of numbers by time step",domain="R-stacomiR")),
	  Graph=gWidgets::gaction(handler=h_report_mig_mult_graph,
		  icon="graph", 
		  label="graph",
		  tooltip=gettext("Balance graphic",domain="R-stacomiR")),
	  Graph2=gWidgets::gaction(handler=h_report_mig_multgraph2, 
		  icon="graph2", 
		  label="grcum",
		  tooltip=gettext("Cumulative graphic",domain="R-stacomiR")),
	  Graph3=gWidgets::gaction(handler=h_report_mig_multgraph3, 
		  icon="gWidgetsRGtk2-barplot",
		  label=gettext("gr(all)",domain="R-stacomiR"), 
		  tooltip=gettext("Balance graphic",domain="R-stacomiR")),
	  Stat=gWidgets::gaction(handler=hTablereport_mig_mult, 
		  icon="dataframe", label="stat", 
		  tooltip=gettext("Balance sheet in .csv",domain="R-stacomiR")),    
	  Out=gWidgets::gaction(handler=houtreport_mig_mult,
		  icon="gtk-info",
		  label="code", 
		  tooltip=gettext("Code",domain="R-stacomiR")),
	  annuler=gWidgets::gaction(handler= quitte,
		  icon = "close",
		  label="quitter")
  )    
  gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
  gWidgets::addSpring(group)
}

