context("report_sample_char")

test_that("Test that view lot_ope_car exists",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  req<-new("RequeteODBC")
	  req@baseODBC<-get("baseODBC", envir=envir_stacomi)		
	  sch=get("sch",envir=envir_stacomi)
	  req@sql=paste("select * from ",sch," vue_lot_ope_car limit 10")
	  req<-stacomirtools::connect(req)
	  result<-req@query	
	  expect_true(nrow(result)>0)
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test an instance of report_sample_char loaded with choice_c",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  
	  r_sample_char<-new("report_sample_char")
	  #options(warn = -1)
	  r_sample_char<-suppressWarnings(choice_c(r_sample_char,
			  dc=c(6),
			  taxa=c("Anguilla anguilla"),
			  stage=c("AGJ"),
			  par=c(1785,1786,1787,"C001"),
			  horodatedebut="2013-01-01",
			  horodatefin="2013-12-31",
			  silent=TRUE))
	  # three warning produced, none shown due to silent=TRUE
	  #options(warn = 0)
	  expect_s4_class(r_sample_char,
		  "report_sample_char")
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


test_that("Test methods in report_sample_char",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  
	  r_sample_char<-new("report_sample_char")
	  #options(warn = 2)
	  r_sample_char<-suppressWarnings(choice_c(r_sample_char,
			  dc=c(5,6),
			  taxa=c("Anguilla anguilla"),
			  stage=c("AGJ","CIV"),
			  par=c(1785,1786,1787,"C001"),
			  horodatedebut="2013-01-01",
			  horodatefin="2013-12-31",
			  silent=TRUE))
	  # two warning produced: No data for par 1785 No data for par 1787
	  #options(warn = 0)
	  r_sample_char<-connect(r_sample_char,silent=TRUE)
	  expect_true(nrow(r_sample_char@data)>0,label="No data for r_sample_char")
	  r_sample_char<-calcule(r_sample_char,silent=TRUE)
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	  
	})

test_that("Test charge method for report_sample_char",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  
	  r_sample_char<-new("report_sample_char")
	  #options(warn = 2)
	  r_sample_char<-suppressWarnings(choice_c(r_sample_char,
			  dc=c(5,6),
			  taxa=c("Anguilla anguilla"),
			  stage=c("AGJ","CIV"),
			  par=c(1785,1786,1787,"C001"),
			  horodatedebut="2013-01-01",
			  horodatefin="2013-12-31",
			  silent=TRUE))
	  # two warning produced: No data for par 1785 No data for par 1787
	  #options(warn = 0)
	  r_sample_char<-connect(r_sample_char,silent=TRUE)
	  r_sample_char<-charge(r_sample_char)
	  r_sample_char<-calcule(r_sample_char,silent=TRUE)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test example reportcarlot-example",
	{
	  # check if built with examples (Rtools install --example)
	  # the file is generate it examples but later loaded to examples from the class using @example
	  # be sure you have built Roxygen documentation before running
	  example_path<-file.path(.libPaths(),"stacomiR","R-ex","report_sample_char-class.R")
	  test<-file.access(example_path,0)
	  if (test[1]!=0) warnings("Package example dir not created ?") else
		source(example_path)
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})



