#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_sea_age = function()
{  
  quitte() # vidange de l'interface
  r_seaa=new("report_sea_age")
  assign("r_seaa",r_seaa,envir = envir_stacomi)
  
  funout(gettext("Loading view vue_ope_lot, and choice of counting device and time steps\n",domain="R-stacomiR"))
  r_seaa@dc=charge(r_seaa@dc)
  r_seaa@taxa=charge(r_seaa@taxa)
  r_seaa@stage=charge(r_seaa@stage)
  r_seaa@par=charge(r_seaa@par)    
  
  group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
  
  assign("group",group,envir = envir_stacomi)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  gWidgets::add(ggroupboutons,group)
  gl=glabel(text="report age de mer",container=group)
  # dans l'ordre 
  # dans le handler, modifier le contenu de l'object fils si il existe
  # supprimer les widgets fils si ils existent (appel de la methode delete)
  # appeller la methode choice pour l'affichage du fils si il existe
  
  
  choice(r_seaa@horodatedebut,label=gettext("Start",domain="R-stacomiR"),
	  nomassign="r_seaa_date_debut",
	  funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
	  decal=-2)
  choice(r_seaa@horodatefin,label=gettext("End",domain="R-stacomiR"),
	  nomassign="r_seaa_date_fin",
	  funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
	  decal=-1)	
  r_seaa@dc<-choice(r_seaa@dc,objectreport=NULL,is.enabled=TRUE)
  r_seaa@limit1hm<-charge(r_seaa@limit1hm,title="Limit s1 for 1sw (L(1sw)<=s1), click to edit",label="0")
  r_seaa@limit2hm<-charge(r_seaa@limit2hm,title="Limit s2 for 2sw (s1<L(2sw)<=s2) & L(3sw)>s2, click to edit",label="0")
#  the choice method for ref_dc will stop there and the other slots are filled with choicec
  # we only want silver eels in this report, and parameters length, eye diameter, pectoral length, contrast...
  
  choice(r_seaa@limit1hm,nomassign="limit1hm")
  choice(r_seaa@limit2hm,nomassign="limit2hm")
  choice_c(r_seaa@taxa,2220)
  choice_c(r_seaa@stage,c('5','11','BEC','BER','IND'))
  choice_c(r_seaa@par,c('1786','1785','C001','A124'))
  aplot1=gWidgets::gaction(label="plot-1",
	  icon="gWidgetsRGtk2-cloud",
	  handler=funplotreport_sea_age,
	  action="1",
	  tooltip="1")
  aplot2=gWidgets::gaction(label="plot-2",
	  icon="gWidgetsRGtk2-cloud",
	  handler=funplotreport_sea_age,
	  action="2",
	  tooltip="2")
  asummary=gWidgets::gaction(label="Summary",icon="dataframe",handler=funtablereport_sea_age,tooltip="Summary")
  aquit=gWidgets::gaction(label=gettext("Exit",domain="R-stacomiR"),icon="close", handler=quitte,tooltip="Exit")
  toolbarlist <- list(    
	  plot1= aplot1,
	  plot2= aplot2,
	  summary= asummary,
	  quit = aquit)
  ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
  gWidgets::add(ggroupboutons,ggroupboutonsbas)
  gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
  assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)	
  gWidgets::addSpring(group)
}
