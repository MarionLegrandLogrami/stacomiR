context("report_sea_age")


test_that("test creating an instance of report_sea_age with data loaded (logrami required)",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  r_seaa<-new("report_sea_age")
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("logrami",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi)
	  assign("sch","logrami.",envir_stacomi)
	  r_seaa<-suppressWarnings(choice_c(r_seaa,
			  dc=c(107,108,101),			
			  horodatedebut="2012-01-01",
			  horodatefin="2012-12-31",
			  limit1hm=675,
			  limit2hm=875,
			  silent=TRUE
		  ))
	  # warnings No data for par 1786No data for par 1785
	  r_seaa<-connect(r_seaa,silent=TRUE)
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("test that loading bad limits fails",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
	  # overriding user schema to point to iav
	  r_seaa<-new("report_sea_age")
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("logrami",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi)
	  assign("sch","logrami.",envir_stacomi)
	  expect_error(
		  r_seaa<-choice_c(r_seaa,
			  dc=c(107,108,101),			
			  horodatedebut="2012-01-01",
			  horodatefin="2012-12-31",
			  limit1hm=675,
			  limit2hm="strawberry",
			  silent=FALSE
		  ))		
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})





