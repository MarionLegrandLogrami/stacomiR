require(stacomiR)
stacomi(gr_interface=FALSE,
	login_window=FALSE,
	database_expected=FALSE)
\dontrun{
  r_env<-new("report_env")
  r_env<-choice_c(r_env,
	  stationMesure=c("temp_gabion","coef_maree"),
	  datedebut="2008-01-01",
	  datefin="2008-12-31",
	  silent=FALSE)	
  r_env<-connect(r_env)
  
}	

data("r_env")
plot(r_env,silent=TRUE)
