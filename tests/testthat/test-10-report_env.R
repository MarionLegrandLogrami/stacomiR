context("report_env")


test_that("test creating an instance of report_env",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  r_env<-new("report_env")
	  r_env<-choice_c(r_env,
		  stationMesure=c("temp_gabion","coef_maree"),
		  datedebut="2008-01-01",
		  datefin="2008-12-31",
		  silent=TRUE)	
	  r_env<-connect(r_env,silent=TRUE)
	  expect_true(nrow(r_env@data)>0,"The is a problem when loading data in the data slot" )
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("test plot method",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  r_env<-new("report_env")
	  r_env<-choice_c(r_env,
		  stationMesure=c("temp_gabion","coef_maree"),
		  datedebut="2008-01-01",
		  datefin="2008-12-31",
		  silent=TRUE)	
	  r_env<-connect(r_env,silent=TRUE)
	  r_env<-plot(r_env)
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


