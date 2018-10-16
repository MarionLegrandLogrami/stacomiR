context("ref_horodate")
test_that("Test that the parsing of many kind of dates works",
	{
	  ref_horodate<-new("ref_horodate")
	  # regular expression to test string "1] nous avons le choix dans la date\n"
	  # default string returned by the method
	  expect_that(ref_horodate<-choice_c(ref_horodate,	
			  horodate="01/01/2013 00:00:00"),prints_text("^\\[1\\].+date.+"))
	  expect_that(ref_horodate<-choice_c(ref_horodate,	
			  horodate="01/01/2013 00:00"),prints_text("^\\[1\\].+date.+"))
	  expect_that(ref_horodate<-choice_c(ref_horodate,	
			  horodate="01-01-2013 00:00"),prints_text("^\\[1\\].+date.+"))		
	  expect_that(ref_horodate<-choice_c(ref_horodate,	
			  horodate="2013-01-01 00:00"),prints_text("^\\[1\\].+date.+"))	
	  expect_that(ref_horodate<-choice_c(ref_horodate,	
			  horodate="01-01-2013"),prints_text("^\\[1\\].+date.+"))				
	  expect_error(ref_horodate<-choice_c(ref_horodate,	
			  horodate="2013/01/01 00:00:00"))
	})



test_that("Test that the parsing of wrong character formats gets an error",
	{
	  ref_horodate<-new("ref_horodate")
	  options(warn = -1)
	  expect_error(ref_horodate<-choice_c(ref_horodate,	
			  horodate="2013 01 01"))	
	  options(warn = 1)
	  
	})

context("ref_df")

test_that("Test that ref_df choice_c method loads character, numeric, but not rubbish",
	{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
	  ref_df<-new("ref_df")
	  ref_df<-charge(ref_df)
	  expect_silent(ref_df<-choice_c(ref_df,	2))		
	  expect_silent(ref_df<-choice_c(ref_df,	"2"))			
	  expect_error(ref_df<-suppressWarnings(choice_c(ref_df,	"semoule")))
	})
