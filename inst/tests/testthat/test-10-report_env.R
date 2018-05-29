context("report_env")


test_that("test creating an instance of report_env",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
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
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
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


