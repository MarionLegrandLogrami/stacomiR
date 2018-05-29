#' interface for report_migPar class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_mig_char = function()
{
  quitte()
  
  r_mig_char=new("report_mig_char")
  assign("r_mig_char",r_mig_char,envir=envir_stacomi)
  
  report_dc=new("report_dc") 
  assign("report_dc",report_dc,envir=envir_stacomi)
  
  objectreport="r_mig_char"
  # the following name is created by the interface
  # as I can't get the name from within the function (deparse(substitute(objectreport)) does not return
  # "report_mig_mult" see ref_dc choice_c method)
  # so this will allow to assign "report_mig_mult" in envir_stacomi while using other class
  # like ref_dc
  assign("objectreport",objectreport,envir=envir_stacomi)
  funout(gettext("Loading of the lists for taxa, stages, counting devices, qualitative and quantitative parameters\n",domain="R-stacomiR"))
  r_mig_char@taxa=charge(r_mig_char@taxa)
  r_mig_char@stage=charge(r_mig_char@stage)
  r_mig_char@dc=charge(r_mig_char@dc)
  r_mig_char@parquan=charge(r_mig_char@parquan)
  r_mig_char@parqual=charge(r_mig_char@parqual)
  # below, the first element must be the element where samples are accepted (currently with)
  # this is how it will be evaluated in the connect method, as I can't base myself on the value
  # which will change with language
  r_mig_char@echantillon=charge(r_mig_char@echantillon,
	  vecteur=gettext("with","without",domain="R-stacomiR"),
	  label=gettext("Choice of batch type, inclusion of samples ?",domain="R-stacomiR"), 
	  selected=as.integer(1))
  #######################
  # Interface Graphique 
  ##########################
  group <- gWidgets::ggroup(horizontal=TRUE)   # doit toujours s'appeller group	
  assign("group",group,envir = envir_stacomi)
  notebook <- gnotebook(container=group)	
  assign("notebook",notebook,envir=envir_stacomi)
  size(notebook)<-c(400,600)
  
  
  choicemult(r_mig_char@horodatedebut,nomassign="bmC_date_debut",label=gettext("from",domain="R-stacomiR"),decal=-1)
  choicemult(r_mig_char@horodatefin,,nomassign="bmC_date_fin",label=gettext("to",domain="R-stacomiR"),decal=0)
  choicemult(r_mig_char@echantillon)
  choicemult(r_mig_char@dc,objectreport=r_mig_char,is.enabled=TRUE)
  svalue(notebook)<-1	
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  gWidgets::add(ggroupboutons,group)
  # ggroupboutons is attached to the original frame
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)
  toolbarlist = list(
	  Calc=gWidgets::gaction(handler = hbmCcalc,
		  icon = "new",
		  label=gettext("calculation"),
		  tooltip=gettext("calculation",domain="R-stacomiR")),
	  Graph=gWidgets::gaction(handler = hbmCplotquan,
		  icon = "graph",
		  label="gr quan",
		  tooltip=gettext("Plot for qualitative parm",domain="R-stacomiR")),
	  Graph2=gWidgets::gaction(handler = hbmCplotqual,
		  icon = "graph2",
		  label="gr qual",
		  tooltip=gettext("plot for quantitative parm",domain="R-stacomiR")),
	  Graph3=gWidgets::gaction(handler = hbmCplotcrossed,
		  icon = "graph2",
		  label="gr crossed",
		  tooltip=gettext("Crossed graph for qualitative and quantitative parameter",domain="R-stacomiR")),
	  Stat =gWidgets::gaction(handler= hbmCstat,
		  icon = "matrix",
		  label="stat",
		  tooltip=gettext("Summary",domain="R-stacomiR")),
	  annuler=gWidgets::gaction(handler= quitte,
		  icon = "close",
		  label=gettext("Exit",domain="R-stacomiR")))
  gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
  gWidgets::addSpring(group)
  assign("r_mig_char",r_mig_char,envir=envir_stacomi)
}