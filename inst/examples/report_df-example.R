require(stacomiR)
stacomi(gr_interface=FALSE,
	login_window=FALSE,
	database_expected=FALSE)
## An example that will work with the database installed only
\dontrun{
  r_df=new("report_df")
  r_df<-choice_c(r_df,
	  1,
	  horodatedebut="2015-01-01",
	  horodatefin="2015-12-31",
	  silent=TRUE)
  Sys.setenv(TZ='GMT')
  # the times at Arzal are recorded continuously
  # they are converted to date when a time appears while the hour is changing
  # hence the following
  r_df<-connect(r_df)
}

data("r_df")
plot(r_df,plot.type="4")
# the following examples work but take a while to compute
\dontrun{
  plot(r_df,plot.type="1")
  plot(r_df,plot.type="2",main="A nice title")
  plot(r_df,plot.type="3",main="A nice title")	
}





