context("report_annual")


test_that("Test an instance of report_annual loaded with choice_c",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi)
	  assign("sch","iav.",envir_stacomi)
	  r_ann<-new("report_annual")
	  r_ann<-choice_c(r_ann,
		  dc=c(5,6,12),
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ","AGG"),
		  anneedebut="1996",
		  anneefin="2015",
		  silent=TRUE)
	  r_ann<-connect(r_ann,silent=TRUE)	
	  expect_s4_class(r_ann,"report_annual")
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})


test_that("Test methods in report_annual",{
	  require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  # overriding user schema to point to iav
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi)
	  assign("sch","iav.",envir_stacomi)
	  r_ann<-new("report_annual")
	  r_ann<-choice_c(r_ann,
		  dc=c(5,6,12),
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ","AGG"),
		  anneedebut="1996",
		  anneefin="2015",
		  silent=TRUE)			
	  r_ann<-connect(r_ann,silent=TRUE)	
	  dev.new()
	  plot(r_ann,silent=TRUE)
	  dev.new()
	  barplot(r_ann)			
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("Test example report_mig_annual-example",
	{
	  # check if built with examples (Rtools install --example)
	  # the file is generate it examples but later loaded to examples from the class using @example
	  # be sure you have built Roxygen documentation before running
	  example_path<-file.path(.libPaths(),"stacomiR","R-ex","report_annual-class.R")
	  test<-file.access(example_path,0)
	  if (test[1]!=0) warnings("Package example dir not created ?") else
		suppressWarnings(source(example_path))
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	  
	})

test_that("Complement to example",
    {
      require(stacomiR)
	  stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=TRUE)
	  data(r_ann)
      xtr_ann<-stacomiR::xtable(r_ann,
	      dc_name=c("Passe bassins","Piege anguille RG","Piege anguille RD"),
	      tax_name="Anguille",
	      std_name=c("Arg.","Jaun."))   
	  path=file.path(path.expand(get("datawd",envir=envir_stacomi)),
		  paste(paste(r_ann@dc@dc_selectionne,collapse="+"),"_",
			  paste(r_ann@taxa@data$tax_code,collapse="+"),"_",
			  paste(r_ann@stage@data$std_code,collapse="+"),"_",
			  r_ann@anneedebut@annee_selectionnee,":",
			  r_ann@anneefin@annee_selectionnee,".html",sep=""),fsep ="/")
      
# here you can add an argument file=path
	  expect_output(print(xtr_ann,type="html"))
	  
# the following uses the "addtorow" argument which creates nice column headings,
# format.args creates a thousand separator
# again this will need to be saved in a file using the file argument
	  expect_output(print(xtr_ann,
			  add.to.row=get("addtorow",envir_stacomi),
			  include.rownames = TRUE,
			  include.colnames = FALSE,
			  format.args = list(big.mark = " ", decimal.mark = ",")
	      ))
# barplot transforms the data, further arguments can be passed as to barplot
	  dev.new()
	  barplot(r_ann)
	  dev.new()
	  barplot(r_ann,
		  args.legend=list(x="topleft",bty = "n"),
		  col=c("#CA003E","#1A9266","#E10168","#005327","#FF9194"))
	  
# An example with custom arguments for legend.text (overriding plot defauts)
	  data(r_ann_adour)
	  if (requireNamespace("RColorBrewer", quietly = TRUE)){
		lesdc<-r_ann_adour@dc@data$dc_code[r_ann_adour@dc@data$dc%in%r_ann_adour@dc@dc_selectionne]
		barplot(r_ann_adour,
			legend.text=lesdc,
			args.legend=list(x="topleft",bty = "n"),
			col=RColorBrewer::brewer.pal(9,"Spectral"),
			beside=TRUE)
	  }
	  dev.new()
	  plot(r_ann_adour,silent=TRUE)
	  graphics.off()
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
    })

test_that("test xtable method for report_annual",{
	  require(stacomiR)
# launching stacomi without selecting the scheme or interface
	  stacomi(gr_interface=FALSE,
		  login_window=FALSE,
		  database_expected=FALSE)
	  baseODBC<-get("baseODBC",envir=envir_stacomi)
	  baseODBC[c(2,3)]<-rep("iav",2)
	  assign("baseODBC",baseODBC,envir_stacomi)
	  sch<-get("sch",envir=envir_stacomi)
	  assign("sch","iav.",envir_stacomi)
	  r_ann<-new("report_annual")
	  r_ann<-choice_c(r_ann,
		  dc=c(5,6,12),
		  taxa=c("Anguilla anguilla"),
		  stage=c("AGJ","AGG"),
		  anneedebut="1996",
		  anneefin="2015",
		  silent=TRUE)
	  r_ann<-connect(r_ann)	
	  data(r_ann)
      # if xtable is loaded to namespace, the function is masked ?
	  xtr_mig_annual<-stacomiR::xtable(r_ann,
		  dc_name=c("Passe bassins","Piege anguille RG","Piege anguille RD"),
		  tax_name="Anguille",
		  std_name=c("Arg.","Jaun."))
	  expect_equal(class(xtr_mig_annual)[1],"xtable","report_annual should have an xtable method")
	  xtr_mig_annual<-stacomiR::xtable(r_ann,
		  dc_name=c("Passe bassins","Piege anguille RG","Piege anguille RD"),
		  tax_name="Anguille",
		  std_name=c("Arg.","Jaun."))
	  expect_equal(class(xtr_mig_annual)[1],"xtable","report_annual should have an xtable method")
	  path=file.path(path.expand(get("datawd",envir=envir_stacomi)),
		  paste(paste(r_ann@dc@dc_selectionne,collapse="+"),"_",
			  paste(r_ann@taxa@data$tax_code,collapse="+"),"_",
			  paste(r_ann@stage@data$std_code,collapse="+"),"_",
			  r_ann@anneedebut@annee_selectionnee,":",
			  r_ann@anneefin@annee_selectionnee,".html",sep=""),fsep ="/")
	  
	  expect_output(print(xtr_mig_annual,type="html"))			
	  expect_output(print(xtr_mig_annual,
			  add.to.row=get("addtorow",envir_stacomi),
			  include.rownames = TRUE,
			  include.colnames = FALSE,
			  format.args = list(big.mark = " ", decimal.mark = ",")))
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	})

test_that("test plot methods for report_annual",{
	  data(r_ann_adour)
	  dev.new()
	  if (requireNamespace("RColorBrewer", quietly = TRUE)){
		lesdc<-r_ann_adour@dc@data$dc_code[r_ann_adour@dc@data$dc%in%r_ann_adour@dc@dc_selectionne]
		barplot(r_ann_adour,
			legend.text=lesdc,
			args.legend=list(x="topleft",bty = "n"),
			col=RColorBrewer::brewer.pal(9,"Spectral"),
			beside=TRUE)
	  }
	  dev.new()
	  plot(r_ann_adour,silent=TRUE)
	  graphics.off()		
      rm(list=ls(envir=envir_stacomi),envir=envir_stacomi)
	  
	})
