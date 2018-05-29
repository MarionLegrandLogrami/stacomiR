#' Interface for report_species class
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_species=function(){
  bilesp=new("report_species")
  assign("bilesp",bilesp,envir = envir_stacomi)
  funout(gettext("Summary of encountered species for the counting device\n",domain="R-stacomiR"))
  bilesp@dc=charge(bilesp@dc)   
  bilesp@split=charge(object=bilesp@split,
	  listechoice=c("none","week","month","year"),
	  label=gettext("Choice of cutting",domain="R-stacomiR"))
  bilesp@anneedebut=charge(bilesp@anneedebut,objectreport="report_species")
  bilesp@anneefin=charge(bilesp@anneefin,objectreport="report_species")
  quitte()
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  assign("group",group,envir = envir_stacomi)  
  gl=glabel(text=gettext("Species summary",domain="R-stacomiR"),container=group)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  add(ggroupboutons,group)
  choice(bilesp@anneedebut,
	  nomassign="anneedebut",
	  funoutlabel=gettext("The year of beginning has been chosen\n",domain="R-stacomiR"),
	  titleFrame=gettext("First year",domain="R-stacomiR"),
	  preselect=which(bilesp@anneedebut@data==min(bilesp@anneedebut@data)))
  choice(bilesp@anneefin,
	  nomassign="anneefin",
	  funoutlabel=gettext("The last year has been chosen\n",domain="R-stacomiR"),
	  titleFrame=gettext("Last year",domain="R-stacomiR"),
	  preselect=which(bilesp@anneefin@data==max(bilesp@anneefin@data)))
  
  choice(bilesp@dc,objectreport=bilesp,is.enabled=TRUE)
  choice(bilesp@split)	
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  toolbarlist = list(
	  Calc=gWidgets::gaction(handler=hbilespcalc,  icon="new", label="calcul", tooltip=gettext("Loading",domain="R-stacomiR")),
	  Graph=gWidgets::gaction(label="pie",tooltip=gettext("Pie chart graphic",domain="R-stacomiR"),icon="bubbles",handler=hplotbilesp,action="pie"),
	  Graph2=gWidgets::gaction(handler=hplotbilesp, icon="barplot", label="histo", tooltip=gettext("barplot",domain="R-stacomiR"),action="barplot"),
	  Stat=gWidgets::gaction(handler=hsummarybilesp, icon="dataframe", label="summary", tooltip=gettext("Summary tables in .csv and XML",domain="R-stacomiR")),    
	  annuler=gWidgets::gaction(handler= quitte,icon = "close",label=gettext("exit",domain="R-stacomiR"))
  ) 
  add(ggroupboutonsbas, gtoolbar(toolbarlist))
  gWidgets::addSpring(group)
}