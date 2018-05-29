#' interface for report_ge_weight class 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_report_ge_weight = function()
{
  quitte()  # erase the interface
  r_gew=new("report_ge_weight")
  assign("report_ge_weight",r_gew,envir = envir_stacomi)
  r_gew@dc=charge(r_gew@dc)
  r_gew@anneedebut=charge(r_gew@anneedebut)
  r_gew@anneefin=charge(r_gew@anneefin)
  r_gew@liste=charge(object=r_gew@liste,listechoice=c("=1",">1",gettext("all",domain="R-stacomiR")),label=gettext("choice of number in sample (one, several, both)",domain="R-stacomiR"))
  # choice of number type
  group <- gWidgets::ggroup(horizontal=FALSE)   # must always be named group
  assign("group",group,envir = envir_stacomi)
  ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
  gWidgets::add(ggroupboutons,group)
  gl=glabel(text=gettext("Mean weight report",domain="R-stacomiR"),container=group)
  
  ### first toobar    
  gWidgets::addSpring(group)
  choice(r_gew@liste)
  choice(r_gew@dc,objectreport=NULL,is.enabled=TRUE)
  choice(r_gew@anneedebut,
	  nomassign="ref_yearDebut",
	  titleFrame=gettext("Beginning year",domain="R-stacomiR")) #annee debut
  choice(r_gew@anneefin,
	  nomassign="ref_yearFin",
	  titleFrame=gettext("Ending year",domain="R-stacomiR"))#annee fin
  aCalc=gWidgets::gaction(label=gettext("load",domain="R-stacomiR"),
	  icon="lines",
	  handler=hcalc,
	  tooltip=gettext("load",domain="R-stacomiR")) 
  a_table=gWidgets::gaction(label="table",
	  icon="dataframe",
	  handler=funtablereport_ge_weight,
	  tooltip=gettext("table",domain="R-stacomiR"))
  aQuit=gWidgets::gaction(
	  label=gettext("exit",domain="R-stacomiR"),
	  icon="close", 
	  handler=quitte,
	  tooltip=gettext("exit",domain="R-stacomiR"))
  toolbarlist <- list(barchart=aCalc,table=a_table,Quit = aQuit)
  add(group, gmenu(toolbarlist))
  
  ### second toobar    
  aGra=gWidgets::gaction(label=gettext("Gra",domain="R-stacomiR"),icon="lines",handler=hplot,action="1",
	  tooltip=gettext("plot.type='1', plot of mean weight of glass eel against the mean date of operation",domain="R-stacomiR"))
  aCoe=gWidgets::gaction(label=gettext("Coe",domain="R-stacomiR"),icon="newplot",handler=hplot,action="2",
	  tooltip=gettext("plot.type=2, standard plot of current coefficent",domain="R-stacomiR"))
  aSize=gWidgets::gaction(label=gettext("Leng",domain="R-stacomiR"),icon="gWidgetsRGtk2-bubbles",handler=hplot,action="3",
	  tooltip=gettext("plot.type=3, same as 1 but size of the bubble according to number",domain="R-stacomiR"))         
  aReg=gWidgets::gaction(label=gettext("seasonal",domain="R-stacomiR"),icon="function",handler=hreg,action="seasonal",
	  tooltip=gettext("model.type='seasonal', sine wave curve for a cyclic variation fitted with nls",domain="R-stacomiR"))        
  aReg1=gWidgets::gaction(label=gettext("seasonal1",domain="R-stacomiR"),icon="function1",handler=hreg,action="seasonal1",
	  tooltip=gettext("model.type='seasonal1', long term variation along with seasonal variation fitted with gam",domain="R-stacomiR"))        
  aReg2=gWidgets::gaction(label=gettext("seasonal2",domain="R-stacomiR"),icon="function",handler=hreg,action="seasonal2",
	  tooltip=gettext("model.type='seasonal2', long term variation + seasonal component fitted with sine curve",domain="R-stacomiR"))
  aExp=gWidgets::gaction(label=gettext("export",domain="R-stacomiR"),icon="gtk-harddisk",handler=hexp)    
  toolbarlistgraph <- gmenu(list(gra=aGra,coe=aCoe,size=aSize))
  assign("toolbarlistgraph",toolbarlistgraph,envir_stacomi)
  toolbarlistgraph1<-gmenu(list(reg=aReg,reg1=aReg1,reg2=aReg2,exp=aExp))
  assign("toolbarlistgraph1",toolbarlistgraph1,envir_stacomi)
  add(group,toolbarlistgraph)
  add(group,toolbarlistgraph1)  
}