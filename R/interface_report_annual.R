#' interface for report_annual class 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_annual = function()
{
  quitte() # vidange de l'interface
  report_annual=new("report_annual")
  assign("report_annual",report_annual,envir=envir_stacomi)
  objectreport="report_annual"
  # the following name is created by the interface
  # as I can't get the name from within the function (deparse(substitute(objectreport)) does not return
  # "report_mig_mult" see ref_dc choice_c method)
  # so this will allow to assign "report_mig_mult" in envir_stacomi while using other class
  # like ref_dc
  assign("objectreport",objectreport,envir=envir_stacomi)
  report_annual@dc=charge(report_annual@dc)
  report_annual@taxa=charge(report_annual@taxa)
  report_annual@stage=charge(report_annual@stage)
  report_annual@anneedebut=charge(report_annual@anneedebut,objectreport="report_annual")
  report_annual@anneefin=charge(report_annual@anneefin,objectreport="report_annual")
  
  
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  assign("group",group,envir = envir_stacomi)  
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  add(ggroupboutons,group)
  notebook <- gnotebook(container=group)	
  assign("notebook",notebook,envir=envir_stacomi)
  size(notebook)<-c(400,300)
  # pour preselectionner une date on lui fournit l'indice de la date dans le ref_year. indice = 11 pour 2005
  
  choice(report_annual@anneedebut,
	  nomassign="anneedebut",
	  funoutlabel=gettext("The year of beginning has been chosen\n",domain="R-stacomiR"),
	  titleFrame=gettext("First year",domain="R-stacomiR"),
	  preselect=which(report_annual@anneedebut@data==min(report_annual@anneedebut@data)))
  choice(report_annual@anneefin,
	  nomassign="anneefin",
	  funoutlabel=gettext("The last year has been chosen\n",domain="R-stacomiR"),
	  titleFrame=gettext("Last year",domain="R-stacomiR"),
	  preselect=which(report_annual@anneefin@data==max(report_annual@anneefin@data)))
  choicemult(report_annual@dc,objectreport=report_annual,is.enabled=TRUE)
  svalue(notebook)<-1
  
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  assign("ggroupboutonsbas",ggroupboutonsbas,envir=envir_stacomi)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  
  toolbarlist = list(
	  aGraph=gWidgets::gaction(label="barplot",icon="barplot",handler=hbarplotreport_annual,tooltip=gettext("Migration of all the years in the same graphic",domain="R-stacomiR")),
	  aGraph2=gWidgets::gaction(label="plot",icon="plot",handler=hplotreport_annual,tooltip=gettext("Migration of all the years in the same graphic",domain="R-stacomiR")),
	  a_table=gWidgets::gaction(handler=hxtablereport_annual, icon="dataframe", label="xtable", tooltip="xtable"),  
	  aQuit=gWidgets::gaction(label="fermer",icon="close", handler=quitte,tooltip=gettext("Exit",domain="R-stacomiR"))
  )
  
  add(ggroupboutonsbas, gtoolbar(toolbarlist))
  gWidgets::addSpring(group)
  
  
}