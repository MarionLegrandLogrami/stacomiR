context("report_mig")
test_that("Test an instance of report_mig",{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)			
	  report_mig<-new("report_mig")
	  options(warn = -1)
	  report_mig<-choice_c(report_mig,
		  dc=c(6),
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ"),
		  datedebut="2013-01-01",
		  datefin="2013-12-31")
	  options(warn = 0)
	  expect_s4_class(report_mig,
		  "report_mig")
	   rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


test_that("Test an instance of report_mig, check that operations accross two years are split correcly",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  
	  report_mig<-new("report_mig")
	  options(warn = -1)
	  report_mig<-choice_c(report_mig,
		  dc=c(6),
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ"),
		  datedebut="1997-01-01",
		  datefin="1997-12-31")
	  options(warn = 0)
	  report_mig<-charge(report_mig,silent=TRUE)
	  report_mig<-connect(report_mig,silent=TRUE)
	  report_mig<-calcule(report_mig,silent=TRUE)
	  # before doing the split per year the sum was 8617
	  # now it is less, only one third of the 7 eel belong to 1997
	  # the rest are in 1998
	  expect_equal(round(sum(report_mig@calcdata[["dc_6"]][["data"]]$Effectif_total)),
		  8614)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test another instance of report_mig, check that operations accross two years are split correcly",{
	  skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","iav.",envir_stacomi)
	  
	  report_mig<-new("report_mig")
	  options(warn = -1)
	  report_mig<-choice_c(report_mig,
		  dc=c(6),
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ"),
		  datedebut="2015-01-01",
		  datefin="2015-12-31")
	  options(warn = 0)
	  report_mig<-charge(report_mig,silent=TRUE)
	  report_mig<-connect(report_mig,silent=TRUE)
	  report_mig<-calcule(report_mig,silent=TRUE)
	  # before doing the split per year the sum was 8617
	  # now it is less, only one third of the 7 eel belong to 1997
	  # the rest are in 1998
	  expect_equal(round(sum(report_mig@calcdata[["dc_6"]][["data"]]$Effectif_total)),
		  26454)
	   rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test connect method",{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
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
	  r_mig=new("report_mig")
	  r_mig=choice_c(r_mig,
		  dc=5,
		  taxa=c("Liza ramada"),
		  stage=c("IND"),
		  datedebut="2015-01-01",
		  datefin="2015-12-31")
	  r_mig<-charge(r_mig,silent=TRUE)
	  r_mig<-connect(r_mig,silent=TRUE)
	  
	  expect_length(r_mig@data,11)
	   rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test example 02_report_mig",
	{
	  # check if built with examples (Rtools install --example)
	  # the file is generate it examples but later loaded to examples from the class using @example
	  # be sure you have built Roxygen documentation before running
	  example_path<-file.path(.libPaths(),"stacomiR","R-ex","report_mig-class.R")
	  test<-file.access(example_path,0)
	  if (test[1]!=0) warnings("Package example dir not created ?") else
		source(example_path)
	})


test_that("Summary method works",
	{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
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
	  data("r_mig")
	  r_mig<-calcule(r_mig,silent=TRUE)
	  summary(r_mig,silent=TRUE)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test writing an example to the database",
	{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
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
	  data("r_mig")
	  r_mig<-calcule(r_mig,silent=TRUE)
	  write_database(object=r_mig,silent=TRUE)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test that different sums are the same, for report_mig, report_mig_interannual, report_annual",
	{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
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
	  data("r_mig")
	  r_mig<-calcule(r_mig,silent=TRUE)
	  expect_equal(
		  sum(r_mig@calcdata$dc_5$data$Effectif_total),
		  sum(r_mig@data[r_mig@data$ope_dic_identifiant==5,"value"]))
	  write_database(object=r_mig,silent=TRUE)
	  # using setAs to transform the report_mig into report_mig_interannual
	  bili=as(r_mig,"report_mig_interannual")			
	  bila=as(bili,"report_annual")
	  bila<-connect(bila,silent=TRUE)
	  # we test that the report_annual has the same number as
	  # report_mig
	  expect_equal(
		  sum(r_mig@calcdata$dc_5$data$Effectif_total),
		  bila@data$effectif,
		  label="The sum of number in the report_mig are different to the
			  number in the report_annual class"
	  )
	  
	  bili<-connect(bili,check=TRUE,silent=TRUE)
	  expect_equal(
		  sum(r_mig@calcdata$dc_5$data$Effectif_total),	
		  sum(bili@data$bjo_valeur[bili@data$bjo_labelquantite=="Effectif_total"]),			
		  label="The sum of number in the report_mig are different to the
			  number in the report_mig_interannual")
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


test_that("print method works",
	{
	  stacomi(gr_interface=FALSE,
		  login_window=FALSE,
		  database_expected=FALSE)	
	  # overriding user schema
	  data("r_mig")
	  expect_output(print(r_mig), "report_mig=choice_c",info = NULL)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})



test_that("test example for fd80",
	{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
	  stacomi(gr_interface=FALSE,
		  login_window=FALSE,
		  database_expected=TRUE)	
	  # overriding user schema
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("fd80",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi) # "iav."
	  assign("sch","fd80.",envir_stacomi)
	  # this chunk is not launched from examples but loads the r_mig dataset if connection works	
	  bM_EclusierVaux=new("report_mig")
	  bM_EclusierVaux=choice_c(bM_EclusierVaux,
		  dc=6,
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGG"),
		  datedebut="2016-01-01",
		  datefin="2016-12-31")
	  bM_EclusierVaux<-charge(bM_EclusierVaux,silent=TRUE)
	  bM_EclusierVaux<-connect(bM_EclusierVaux,silent=TRUE)
	  bM_EclusierVaux<-calcule(bM_EclusierVaux,silent=TRUE)
	  plot(bM_EclusierVaux,silent=TRUE)
	  summary(bM_EclusierVaux,silent=TRUE)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


test_that("test example with glass eel",
	{
      skip_if_not(stacomi_installed(),"skipping as the program is not installed on this computer")
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
	  bM_Arzal_civ=new("report_mig")
	  bM_Arzal_civ=choice_c(bM_Arzal_civ,
		  dc=6,
		  taxa=c("Anguilla anguilla"),
		  stage=c("CIV"),
		  datedebut="2003-01-01",
		  datefin="2003-12-31")
	  bM_Arzal_civ<-charge(bM_Arzal_civ,silent=TRUE)
	  bM_Arzal_civ<-connect(bM_Arzal_civ,silent=TRUE)
	  bM_Arzal_civ<-calcule(bM_Arzal_civ,silent=TRUE)
	  plot(bM_Arzal_civ,silent=TRUE)
	  # some additional arguments passed to plot via ...
	  plot(bM_Arzal_civ,silent=TRUE,bty="n")
	  summary(bM_Arzal_civ,silent=TRUE)
	  rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})
# here require setting a connection to logrami server under the name BD_CONTMIG_NAT_SERVEUR
#test_that("test connexion to logrami server",
#		{
#			require(stacomiR)
#			stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
#			baseODBC<-get("baseODBC",envir=envir_stacomi)
#			baseODBC[1]<- "BD_CONTMIG_SERVEUR"
#			baseODBC[c(2,3)]<-rep('logrami',2)
#			assign("baseODBC",baseODBC,envir_stacomi)
#			sch<-get("sch",envir=envir_stacomi)
#			assign("sch",paste('logrami',".", sep=""),envir_stacomi)		
#			sqldf.options<-get("sqldf.options",envir=envir_stacomi)
#			getpassword<-function(){  
#				require(tcltk);  
#				wnd<-tktoplevel();tclVar("")->passVar;  
#				#Label  
#				tkgrid(tklabel(wnd,text="Enter password:"));  
#				#Password box  
#				tkgrid(tkentry(wnd,textvariable=passVar,show="*")->passBox);  
#				#Hitting return will also submit password  
#				tkbind(passBox,"<Return>",function() tkdestroy(wnd));  
#				#OK button  
#				tkgrid(tkbutton(wnd,text="OK",command=function() tkdestroy(wnd)));  
#				#Wait for user to click OK  
#				tkwait.window(wnd);  
#				password<-tclvalue(passVar);  
#				return(password);  
#			}  
#			sqldf.options["sqldf.RPostgreSQL.host"]<-getpassword()
#			sqldf.options["sqldf.RPostgreSQL.port"]<-5432
#			assign("sqldf.options",sqldf.options,envir_stacomi)
#			report_mig=new('report_mig')
#			report_mig=choice_c(report_mig,
#					dc=23,
#					taxa=c("Petromyzon marinus"),
#					stage=c(5),
#					datedebut="2015-01-01",
#					datefin="2015-12-31")
#			report_mig<-charge(report_mig, silent=TRUE)
#			report_mig=connect(report_mig, silent=TRUE)
#			report_mig=calcule(report_mig, silent=TRUE)
#		
#			r_mig_interannual<-new("report_mig_interannual")
#			r_mig_interannual<-choice_c(r_mig_interannual,
#					dc=c(101,107),
#					taxa=c("Silurus glanis"),
#					stage=c(5),
#					anneedebut="2014",
#					anneefin="2016",
#					silent=TRUE)
#			r_mig_interannual<-connect(r_mig_interannual, silent=FALSE)			
#		})