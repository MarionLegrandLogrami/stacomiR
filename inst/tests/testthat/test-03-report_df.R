context("report_df")

test_that("Test an instance of report_df",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  
	  r_df<-new("report_df")
	  r_df<-choice_c(r_df,
		  2,
		  horodatedebut="2013-01-01",
		  horodatefin="2013-12-31",
		  silent=TRUE)
	  expect_gt(nrow(r_df@df@data),0,
		  label="There should be data loaded by the choice_c method in the data slot of 
              the ref_df slot,nrow(r_df@df@data)")				
	  expect_s4_class(r_df,
		  "report_df")
	  expect_error(BfDF<-choice_c(r_df,
			  2,
			  horodatedebut="2013 01 011",
			  horodatefin="2013-12-31",
			  silent=TRUE))			
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("report_df charge method works",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  
	  r_df<-new("report_df")
	  r_df<-choice_c(r_df,
		  2,
		  horodatedebut="2013-01-01",
		  horodatefin="2013-12-31",
		  silent=TRUE)
	  r_df<-charge(r_df,silent=TRUE)
	  r_df<-connect(r_df,silent=TRUE)
	  expect_equal(nrow(r_df@data),5)		
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)    
	})


test_that("report_df plot method works",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
	  data(r_df)
	  r_df<-r_df
	  plot(r_df,plot.type="1",silent=TRUE)
	  plot(r_df,plot.type="2",silent=TRUE,main="An example title")
	  plot(r_df,plot.type="3",silent=TRUE,main="An example title")	
	  plot(r_df,plot.type="4",silent=TRUE,main="An example title")	
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


test_that("report_df summary method works",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
	  data(r_df)
	  r_df<-r_df
	  expect_output(summary(r_df,silent=FALSE),"summary")
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


test_that("report_df print method works",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
	  data(r_df)
	  r_df<-r_df
	  expect_output(print(r_df))
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})