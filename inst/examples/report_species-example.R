require(stacomiR)
stacomi(gr_interface=FALSE,
	login_window=FALSE,
	database_expected=FALSE)
\dontrun{
  bilesp<-new("report_species")
  # split is one of "none", "year", "week", "month
  bilesp<-choice_c(bilesp,
	  dc=c(5,6,12),
	  split="year", 
	  anneedebut="2008",
	  anneefin="2012",
	  silent=FALSE)	
  #bilesp<-charge(bilesp) this is used by the graphical interface
  bilesp<-connect(bilesp)
  bilesp<-calcule(bilesp)
  plot(bilesp,plot.type="pie",silent=FALSE)
  plot(bilesp,plot.type="barplot",silent=FALSE)
  bilesp<-choice_c(bilesp,
	  dc=c(5,6,12),
	  split="month",
	  anneedebut="2015",
	  anneefin="2016",
	  silent=FALSE)
  bilesp<-charge(bilesp)
  bilesp<-connect(bilesp)
  plot(bilesp,plot.type="pie",silent=FALSE)
  plot(bilesp,plot.type="barplot",silent=FALSE)
  #length(unique(bilesp@calcdata$taxa_stage)) # 15
  # here creating a vector of length 15 with nice blending colours
  color<-c(mycolorrampblue(5),
	  mycolorrampyellow(5),
	  mycolorrampred(5))
  plot(bilesp,plot.type="barplot",color=color,silent=TRUE)
  summary(bilesp)
}	

