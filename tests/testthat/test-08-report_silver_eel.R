context("report_silver_eel")


test_that("test creating an instance of report_silver_eel with data loaded (fd80 schema required)",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
        r_silver<-new("report_silver_eel")
        baseODBC<-get("baseODBC",envir=envir_stacomi)
        baseODBC[c(2,3)]<-rep("fd80",2)
        assign("baseODBC",baseODBC,envir_stacomi)
        sch<-get("sch",envir=envir_stacomi)
        assign("sch","fd80.",envir_stacomi)
        r_silver<-choice_c(r_silver,
	            dc=c(2,6),			
	            horodatedebut="2010-09-01",
	            horodatefin="2016-10-04",
	            silent=TRUE)
        r_silver<-connect(r_silver,silent=TRUE)
	  # warnings No data for par 1786No data for par 1785
	  r_silver<-connect(r_silver,silent=TRUE)
      r_silver<-suppressWarnings(calcule(r_silver,silent=TRUE))
      plot(r_silver, plot.type=1)
      plot(r_silver, plot.type=2)
      plot(r_silver, plot.type=3)      
      plot(r_silver, plot.type=4)
# print a summary statistic, and save the output in a list for later use
      stats<-summary(r_silver,silent=TRUE)
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})






