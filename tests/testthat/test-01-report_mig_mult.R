context("report_mig_mult")
test_that("Test an instance of report_mig_mult",{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  report_mig_mult<-new("report_mig_mult")
	  #options(warn = -1)
	  report_mig_mult<-suppressWarnings(choice_c(report_mig_mult,
			  dc=c(6,7),
			  taxa=c("Anguilla anguilla","Salmo salar"),
			  stage=c("AGG","AGJ","CIV"),
			  datedebut="2012-01-01",
			  datefin="2012-12-31",
			  silent=TRUE))
	  #options(warn = 0)
	  expect_s4_class(report_mig_mult,
		  "report_mig_mult")
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

## This test check that the code above works with numeric and a different formating for date
test_that("Test another instance of report_mig_mult",{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  report_mig_mult<-new("report_mig_mult")
	  #options(warn = -1)
	  report_mig_mult<-suppressWarnings(choice_c(report_mig_mult,
			  dc=c(6,7),
			  taxa=c(2038,2220),
			  stage=c("AGG","AGJ","CIV"),
			  datedebut="2012-01-01",
			  datefin="31/12/2012",
			  silent=TRUE))
	  #options(warn = 0)
	  expect_s4_class(report_mig_mult,
		  "report_mig_mult")
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})
test_that("Tests one instance with error (dc does not exist)",
	{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  report_mig_mult<-new("report_mig_mult")
	  options(warn = -1)
	  expect_error(choice_c(report_mig_mult,
			  dc=c(6,7000),
			  taxa=c("Anguilla anguilla","Salmo salar"),
			  stage=c("AGG","AGJ","CIV"),
			  datedebut="2012-01-01",
			  datefin="31/12/2012",
			  silent=TRUE))
	  options(warn = 0)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test charge method for report_mig_mult",
	{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  report_mig_mult<-new("report_mig_mult")
	  options(warn = -1)
	  report_mig_mult<-choice_c(report_mig_mult,
		  dc=c(6,7),
		  taxa=c(2038),
		  stage=c("AGG","AGJ","CIV"),
		  datedebut="2012-01-01",
		  datefin="31/12/2012",
		  silent=TRUE)
	  options(warn = 0)
	  report_mig_mult<-charge(report_mig_mult,silent=TRUE)
      expect_is(get("report_df",envir=envir_stacomi),"report_df")
	  expect_is(get("report_dc",envir=envir_stacomi),"report_dc")
	  expect_is(get("report_ope",envir=envir_stacomi),"report_ope")
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test connect method for report_mig_mult",
	{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  report_mig_mult<-new("report_mig_mult")
	  options(warn = -1)
	  report_mig_mult<-choice_c(report_mig_mult,
		  dc=c(6,7),
		  taxa=c(2038),
		  stage=c("AGG","AGJ","CIV"),
		  datedebut="2012-01-01",
		  datefin="31/12/2012",
		  silent=TRUE)
	  options(warn = 0)
	  report_mig_mult<-charge(report_mig_mult,silent=TRUE)
	  report_mig_mult<-connect(report_mig_mult,silent=TRUE)
	  expect_gt(nrow(report_mig_mult@data),0)
	  report_ope<-get("report_ope",envir=envir_stacomi)
	  expect_gt(nrow(report_ope@data),0)
	  report_df<-get("report_df",envir=envir_stacomi)
	  expect_gt(nrow(report_df@data),0)
	  report_dc<-get("report_dc",envir=envir_stacomi)
	  expect_gt(nrow(report_dc@data),0)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

#test_that("Test example 01_report_mig_mult",
#	{
#	  #skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
#	  # check if built with examples (Rtools install --example
#	  example_path<-file.path(.libPaths()[1],"stacomiR","R-ex","report_mig_mult-class.R")
#	  test<-file.access(example_path,0)
#	  if (test[1]!=0) warnings("Package example dir not created ?") else
#		options(warn = -1)
#	  source(example_path)
#	  options(warn = 0)
#	  expect_output(summary(r_mig_mult,silent=FALSE))      
#      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
#	})


