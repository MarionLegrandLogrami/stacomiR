#install.packages("testthat",dependencies=c("Depends", "Imports"))
#install.packages("relax")
library(testthat)
library(stacomiR)

#getUsername <- function(){
#  name <- Sys.info()[["user"]]
#  return(name)
#}
#if(getUsername() == 'cedric.briand')
#{
#  setwd("C:/workspace/stacomir/pkg/stacomir/")
#}
#if(getUsername() == 'marion.legrand')
#{
#  setwd("C:/Users/logrami/workspace/stacomir/pkg/stacomir/")
#}
#
#
## to launch all
#
#
#if(getUsername() == 'cedric.briand')
#{
#  test_dir("C:/workspace/stacomir/pkg/stacomir/tests/testthat")
#}
#if(getUsername() == 'marion.legrand')
#{
#  test_dir("C:/Users/logrami/workspace/stacomir/pkg/stacomir/tests/testthat")
#}

test_check("stacomiR")
#devtools::test()

#test_file(stringr::str_c(getwd(),"/tests/testthat/test-00-stacomir.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-00-zrefclasses.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-01-report_mig_mult.R"))
## warning we don't need to be worried about
##Quoted identifiers should have class SQL, use DBI::SQL() if the caller performs the quoting.
## this comes from incompatibility between  RSQLite 1.1-1 and sqldf
## we don't really use RSQLite and this is only a warning not a problem
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-02-report_mig.R"))
## if errors check existence of dbname test and grants to test on dbname test
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-03-report_df.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-04-report_dc.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-05-report_sample_char.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-06-report_mig_interannual.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-07-report_sea_age.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-08-report_silver_eel.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-09-report_annual.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-10-report_env.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-11-report_mig_env.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-12-report_mig_char.R"))
#test_file(stringr::str_c(getwd(),"/tests/testthat/test-13-report_species.R"))
