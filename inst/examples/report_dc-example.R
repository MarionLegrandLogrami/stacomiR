require(stacomiR)
stacomi(gr_interface=FALSE,
	login_window=FALSE,
	database_expected=FALSE)
###########################################################
## An example that will work only if the database is present 
## and the program installed and comprises the schema iav
###########################################################"
\dontrun{
  r_dc=new("report_dc")
  r_dc<-choice_c(r_dc,
	  5,
	  horodatedebut="2000-01-01",
	  horodatefin="2015-12-31",
	  silent=TRUE)
  Sys.setenv(TZ='GMT')
  # This dataset format is GMT. If this option is not set
  # the dataset is tranformed from timestamp to date
  r_dc<-connect(r_dc)
  # this dataset has been loaded by the previous lines
  ###########################################################	
# Without connexion to the database (use dataset r_dc)
  ##########################################################
  data("r_dc")
  plot(r_dc,plot.type="1")
  plot(r_dc,plot.type="2")
  plot(r_dc,plot.type="3",main="trial title")
  plot(r_dc,plot.type="4",main="trial title")
# the following will write in the datawd folder
   summary(r_dc)
}
##




