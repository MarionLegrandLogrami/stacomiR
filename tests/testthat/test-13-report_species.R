context("report_species")


test_that("test creating an instance of report_species",{
	  
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,
		  login_window=FALSE,
		  database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  bilesp<-new("report_species")
	  # split is one of "none", "year", "week", "month
	  
	  bilesp<-choice_c(bilesp,
		  dc=c(5,6,12),
		  split="year", 
		  anneedebut="2008",
		  anneefin="2012",
		  silent=TRUE)	
	  
	  bilesp<-charge(bilesp,silent=TRUE)
	  bilesp<-connect(bilesp,silent=TRUE)
	  expect_s4_class(bilesp,
		  "report_species")
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("test calcule method report_species",{
	  
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,
		  login_window=FALSE,
		  database_expected=TRUE)
  # overriding user schema
  baseODBC<-get("baseODBC",envir=envir_stacomi)
  baseODBC[c(2,3)]<-rep("iav",2)
  assign("baseODBC",baseODBC,envir_stacomi)
  sch<-get("sch",envir=envir_stacomi) # "iav."
  assign("sch","iav.",envir_stacomi)
	  bilesp<-new("report_species")
	  # split is one of "none", "year", "week", "month
	  
	  bilesp<-choice_c(bilesp,
		  dc=c(5,6,12),
		  split="year", 
		  anneedebut="2008",
		  anneefin="2012",
		  silent=TRUE)				
	  bilesp<-charge(bilesp,silent=TRUE)
	  bilesp<-connect(bilesp,silent=TRUE)
	  bilesp<-calcule(bilesp,silent=TRUE)
	  expect_gt(nrow(bilesp@calcdata),0,"No data in calcdata after running calculations")
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("test that plot method works",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,
		  login_window=FALSE,
		  database_expected=FALSE)
	  bilesp<-new("report_species")
	  # split is one of "none", "year", "week", "month
	  
	  bilesp<-choice_c(bilesp,
		  dc=c(5,6,12),
		  split="year", 
		  anneedebut="2008",
		  anneefin="2012",
		  silent=TRUE)				
	  bilesp<-charge(bilesp,silent=TRUE)
	  bilesp<-connect(bilesp,silent=TRUE)
	  bilesp<-calcule(bilesp,silent=TRUE)
	  plot(bilesp,plot.type="pie",silent=TRUE)
	  plot(bilesp,plot.type="barplot",silent=TRUE)
	  mycolorrampblue<-grDevices::colorRampPalette(c("#395B74","#010F19"))
	  mycolorrampyellow<-grDevices::colorRampPalette(c("#B59C53","#271D00"))
	  mycolorrampred<-grDevices::colorRampPalette(c("#B56F53","#270B00"))
	  #length(unique(bilesp@calcdata$taxa_stage)) # 15
	  # here creating a vector of length 15 with nice blending colours
	  color<-c(mycolorrampblue(5),
		  mycolorrampyellow(5),
		  mycolorrampred(5))
	  plot(bilesp,plot.type="barplot",color=color,silent=TRUE)
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	  })

test_that("test that summary method works",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,
		  login_window=FALSE,
		  database_expected=FALSE)
	  bilesp<-new("report_species")
	  # split is one of "none", "year", "week", "month
	  
	  bilesp<-choice_c(bilesp,
		  dc=c(5,6,12),
		  split="year", 
		  anneedebut="2008",
		  anneefin="2012",
		  silent=TRUE)				
	  bilesp<-charge(bilesp,silent=TRUE)
	  bilesp<-connect(bilesp,silent=TRUE)
	  bilesp<-calcule(bilesp,silent=TRUE)
	  summary(bilesp,silent=TRUE)
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

