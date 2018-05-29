context("report_mig_interannual")


test_that("Test an instance of report_mig_interannual loaded with choice_c",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  r_mig_interannual<-new("report_mig_interannual")
	  # the following will load data for size, 
	  # parameters 1786 (total size) C001 (size at video control)
	  # dc 5 and 6 are fishways located on the Arzal dam
	  # two stages are selected
	  r_mig_interannual<-choice_c(r_mig_interannual,
		  dc=6,
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ"),
		  anneedebut=1996,
		  anneefin=2015,
		  silent=TRUE)
	  r_mig_interannual<-connect(r_mig_interannual,silent=TRUE)	
	  # three warning produced, none shown due to silent=TRUE
	  options(warn = 0)
	  expect_s4_class(r_mig_interannual,"report_mig_interannual")
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


test_that("Test method summary in report_mig_interannual",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  r_mig_interannual<-new("report_mig_interannual")
	  # the following will load data for size, 
	  # parameters 1786 (total size) C001 (size at video control)
	  # dc 5 and 6 are fishways located on the Arzal dam
	  # two stages are selected
	  r_mig_interannual<-choice_c(r_mig_interannual,
		  dc=6,
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ"),
		  anneedebut="1996",
		  anneefin=2015,
		  silent=TRUE)
	  r_mig_interannual<-connect(r_mig_interannual,silent=TRUE)	
	  summary(object=r_mig_interannual,silent=TRUE)
	  # two warning produced
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	  
	})

test_that("Test example report_mig_interannual-example",
	{
	  # check if built with examples (Rtools install --example)
	  # the file is generate it examples but later loaded to examples from the class using @example
	  # be sure you have built Roxygen documentation before running
	  example_path<-file.path(.libPaths(),"stacomiR","R-ex","report_mig_interannual-class.R")
	  test<-file.access(example_path,0)
	  if (test[1]!=0) warnings("Package example dir not created ?") else
		suppressWarnings(source(example_path))
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test that loading two taxa will fail",
	{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  r_mig_interannual<-new("report_mig_interannual")
	  # the following will load data for size, 
	  # parameters 1786 (total size) C001 (size at video control)
	  # dc 5 and 6 are fishways located on the Arzal dam
	  # two stages are selected
	  r_mig_interannual<-suppressWarnings(choice_c(r_mig_interannual,
			  dc=5,
			  taxa=c("Anguilla anguilla","Petromyzon marinus"),
			  stage=c("AGJ"),
			  anneedebut="1996",
			  anneefin=2015,
			  silent=TRUE))
	  expect_error(charge(r_mig_interannual))
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	  
	})


test_that("Test that report_mig_interannual loads missing data with correct warning",
	{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("logrami",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi)
	  assign("sch","logrami.",envir_stacomi)
	  
	  bmi_cha<-new("report_mig_interannual") #châtelrault
	  bmi_cha<-suppressWarnings(choice_c(bmi_cha,
			  dc=c(21),
			  taxa=c("Salmo salar"),
			  stage=c("5"),
			  anneedebut="2004",
			  anneefin="2006",
			  silent=TRUE))
	  
	  bmi_cha<-charge(bmi_cha,silent=TRUE)
# deleting all data to ensure everything is loaded
	  supprime(bmi_cha)
	  bmi_cha<-connect(bmi_cha,silent=TRUE)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test that different sums are the same, for  report_mig_interannual, report_mig_mult",
	{
	  stacomi(gr_interface=FALSE,
		  login_window=FALSE,
		  database_expected=TRUE)	
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  # this chunk is not launched from examples but loads the r_mig dataset if connection works	
	  r_mig_interannual<-new("report_mig_interannual")
	  # the following will load data for size, 
	  # parameters 1786 (total size) C001 (size at video control)
	  # dc 5 and 6 are fishways located on the Arzal dam
	  # two stages are selected
	  r_mig_interannual<-choice_c(r_mig_interannual,
		  dc=6,
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ"),
		  anneedebut=1997,
		  anneefin=1997,
		  silent=TRUE)
	  r_mig_interannual<-connect(r_mig_interannual,silent=TRUE)	
	  bmM<-as(r_mig_interannual,"report_mig_mult")
	  # we still need to load the associated classes properly
	  # so we need to launch the choice method.
	  bmM<-choice_c(bmM, 
		  dc=bmM@dc@dc_selectionne,
		  taxa=bmM@taxa@data$tax_code, 
		  stage=bmM@stage@data$std_code, 
		  datedebut=as.character(bmM@timestep@dateDebut),
		  datefin=as.character(as.POSIXlt(end_date(bmM@timestep))),
		  silent=TRUE)
	  bmM<-charge(bmM,silent=TRUE)
	  bmM<-connect(bmM,silent=TRUE)
	  bmM<-calcule(bmM,silent=TRUE)
	  
	  expect_equal(				
		  round(sum(bmM@calcdata[["dc_6"]][["data"]]$Effectif_total)),
		  round(sum(r_mig_interannual@data$bjo_valeur[r_mig_interannual@data$bjo_labelquantite=="Effectif_total"]))
	  )
	  ######################
	  # Test for report_annual
	  #####################
	  r_ann=as(r_mig_interannual,"report_annual")
	  r_ann<-connect(r_ann,silent=TRUE)
	  # we test that the report_annual has the same number as
	  # report_mig
	  expect_equal(
		  round(sum(bmM@calcdata[["dc_6"]][["data"]]$Effectif_total)),
		  round(r_ann@data$effectif[1]),
		  label="The sum of number in the report_mig are different to the
			  number in the report_annual class"
	  )		
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})
    
    test_that("Test bmi step plot",{
	            require(stacomiR)
	            stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	            # overriding user schema to point to iav
	            baseODBC<-get("baseODBC",envir=envir_stacomi)
	            baseODBC[c(2,3)]<-rep("iav",2)
	            assign("baseODBC",baseODBC,envir_stacomi)
	            sch<-get("sch",envir=envir_stacomi) # "iav."
	            assign("sch","iav.",envir_stacomi)
	            r_mig_interannual<-new("report_mig_interannual")
	            # the following will load data for size, 
	            # parameters 1786 (total size) C001 (size at video control)
	            # dc 5 and 6 are fishways located on the Arzal dam
	            # two stages are selected
	            r_mig_interannual<-choice_c(r_mig_interannual,
		                dc=6,
		                taxa=c("Anguilla anguilla"),
		                stage=c("AGJ"),
		                anneedebut=1996,
		                anneefin=2015,
		                silent=TRUE)
	            r_mig_interannual<-connect(r_mig_interannual,silent=TRUE)	
	            plot(r_mig_interannual,plot.type="step",silent=TRUE)
                rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	        })
              
