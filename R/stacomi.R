#' handler function used by the main interface
#' 
#' 
#' @param h A handler
#' @param ... Other parameters
#' @keywords internal
h_report_df=function(h,...){
  funout(gettext("Calculation of the operating fishway\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_df(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_dc=function(h,...){
  funout(gettext("Calculation of the operating counting device\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_dc(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_ope=function(h,...){
  # TODO a developper
  funout(text=gettext("Summary of the operations of a device ... to do\n",domain="R-stacomiR"),wash=TRUE)
}
#' handler function used by the main interface 
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_dfdc=function(h,...){
  # TODO develop this
  funout(gettext("Summary between the operating fishway and the counting device ... to do\n",domain="R-stacomiR"),wash=TRUE)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_mig=function(h,...){
  funout(gettext("Migration summary (for a species and a stage)\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_mig(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_mig_mult=function(h,...){
  funout(gettext("For help, contact Cedric Briand - 0033 29 99 08 844 - cedric.briand@eptb-vilaine.fr, or mail stacomi@googlegroups.com.\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_mig_mult(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_mig_interannual=function(h,...){
  funout(gettext("Summary of interannual migration\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_mig_interannual(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_mig_env=function(h,...){
  funout(gettext("Summary of migration environnemental conditions\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_mig_env(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_mig_char=function(h,...){
  funout(gettext("Summary of migration with parameters\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_mig_char(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_env=function(h,...){
  funout(gettext("Summary of the environnemental conditions\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_env(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_sample_char=function(h,...){
  funout(gettext("Summary of samples characteristics (size, weight...) by calling the view vue_lot_ope\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_sample_char(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_size=function(h,...){
  funout(gettext("Lengths summary, not implemented \n",domain="R-stacomiR"),wash=TRUE)
  #eval(interface_report_size(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_ge_weight=function(h,...){
  eval(interface_report_ge_weight(),envir = envir_stacomi)
  funout(gettext("Summary of average weight for the calculation of the relation between length and number.\n",domain="R-stacomiR"),wash=TRUE) 
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_pigment_stages=function(h,...){
  funout(gettext("Not adapted yet to version 0.5.2",domain="R-stacomiR"))
  #funout(gettext("Calculation of the pigmentary stages\n",domain="R-stacomiR"),wash=TRUE)
  #eval(interface_report_stage_pigm(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_mig_annual=function(h,...){
  eval(interface_report_annual(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_silver=function(h,...){
  eval(interface_report_silver_eel(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_species=function(h,...){
  funout(gettext("Species summary of the counting device\n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_species(),envir = envir_stacomi)
}
#' handler function used by the main interface
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
h_report_sea_age=function(h,...){
  funout(gettext("Age calculation from size limit \n",domain="R-stacomiR"),wash=TRUE)
  eval(interface_report_sea_age(),envir = envir_stacomi)
}


#' Internal function, tests the connection and if it works loads the stacomi interface
#' @note \code{gr_interface} is copied by stacomi into envir_stacomi. Same for \code{database_expected}
#' 
#' @param h A handler
#' @param ... Other arguments
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
load_stacomi=function(h,...){
  baseODBC<-get("baseODBC",envir=envir_stacomi)
  # assigned when passing through stacomi
  gr_interface<-get("gr_interface",envir_stacomi) # logical true or false
  database_expected<-get("database_expected",envir_stacomi) # logical true or false
  login_window<-get("login_window",envir_stacomi) # logical true or false
  usrname<-get("usrname",envir_stacomi)
  usrpwd<-get("usrpwd",envir_stacomi)
  # test de la connection
  if (login_window & gr_interface&database_expected){	
	baseODBC[2]<-svalue(usrname)
	baseODBC[3]<-svalue(usrpwd)
	assign("sch",paste(baseODBC[2],".",sep=""),envir=envir_stacomi)
	assign("baseODBC",baseODBC,envir=envir_stacomi)
	logw<-get("logw",envir_stacomi)
	dispose(logw)
  } else {
	# nothing sch and baseODBC have been assigned from default value in stacomi()	
  }
  
  # we dispose loginwindow
  
  
  if (database_expected){
	con=new("ConnectionODBC")
	con@baseODBC=get("baseODBC",envir=envir_stacomi)
	e=expression(con<-connect(con))
	con=tryCatch(eval(e),error=gettext("Error when using the method connect of the ConnectionODBC class") )
	test<-con@etat=="Connection in progress"|con@etat=="Connexion en cours"
	odbcCloseAll()
  }
  # if the test is OK launches the stacomi interface
  # function handler called by gmessage
  hgmessage=function(h,...){
	stacomi(gr_interface=TRUE)
	# if there is an error, re-launches and asks for a password
  }
  #############################
  # second test to check that the database is working well
  ############################
  if (database_expected){
	if (test) { # il existe un lien ODBC mais qui pointe peut etre ailleurs
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@sql="select count(*) from ref.tr_taxon_tax"
	  requete<-stacomirtools::connect(requete)
	  if (nrow(requete@query)==0){
		# the odbc link does not work and might be pointing to a wrong schema
		# this time the argument login_window will be ignored
		gWidgets::gmessage(message=paste(gettext("Problem during the test, the ODBC link works but doesn't point to the database 0.5, check the ODBC link",domain="R-stacomiR"),
				"\n",
				gettext("ODBC link",domain="R-stacomiR"),
				" :",
				baseODBC[1],
				"\n",
				gettext("User",domain="R-stacomiR"),
				" :",
				baseODBC[2],
				"\n",
				gettext("Password",domain="R-stacomiR"),
				" :",
				baseODBC[3]),						
			title=gettext("Error title of the frame",domain="R-stacomiR"),
			icon = "error",
			handler=hgmessage)		
	  } else {
		assign("baseODBC",baseODBC,envir=envir_stacomi)
		if (gr_interface){
		  interface_graphique()
		}
	  }# end else nrow(>0)
	} else {
	  # the test has failed and the user will be prompted to another login window
	  # this time the argument loginwindow will be ignored
	  gWidgets::gmessage(message=paste(gettext("Problem when testing the ODBC connection",domain="R-stacomiR"),
			  "\n",
			  gettext("ODBC link",domain="R-stacomiR"),
			  " :",
			  baseODBC[1],
			  "\n",
			  gettext("User",domain="R-stacomiR"),
			  " :",
			  baseODBC[2],
			  "\n",
			  gettext("Password",domain="R-stacomiR"),
			  " :",
			  baseODBC[3]),						
		  title=gettext("Error title of the frame",domain="R-stacomiR"),
		  icon = "error",
		  handler=hgmessage)
	} # end else test (else == the test didn't pass, we have to change the name and password	
  } else {
	# here : database_expected=FALSE
	# we don't want to check the connection at all...
	if (gr_interface){
	  interface_graphique()
	}
  }
}






#' Function that loads the loginwindow, tests connection, and then destroys the
#' window
#' 
#' @note The defaut behaviour of the program is to run through the following elements
#'  \itemize{
#'      \item{login window}{ The program opens a login window to prompt the user to give his usernames and passwords.
#' 			default values will proposed from "C:/program files/stacomi/calcmig.csv" and if this file does not exists, 
#' 			from \code{file.path(.libPaths(),"stacomiR","config","calcmig.csv")} as a default. If \code{login_window=FALSE}
#' 			the program will skip the login window and use calcmig values for user (\code{uid}) and password(\code{pwd}) as a default.}
#'      \item{tests for connection}{ Test for the existence of a calcmig.csv file, and then the existence of the file
#' 			\code{usr.tr_taxon_tax} where usr is the username extracted from calcmig. These tests are only done if 
#' 			\code{database_expected=TRUE}. If the test don't pass, then the user is prompted for a "login window" even if argument
#' 			\code{login_window} was set to \code{FALSE} at launch.}
#'       \item{graphical interface}{ When either, previous tests have been run successfully, or the value for
#'          \code{database_expected=FALSE} the program will launch. If \code{graphical_interface} is \code{TRUE}, the program will use
#'          a graphical interface \code{\link{interface_graphique}} to build the graphical interface, otherwise the program is expected to run
#' 			through the command line.}
#'  }
#' When \code{database_expected=FALSE} a connection to the database is not expected. Therefore test are run by calling examples object stored in Rdata.
#' To change the language use Sys.setenv(LANG = "fr") or Sys.setenv(LANG = "en")
#' @param gr_interface Boolean, if \code{TRUE} the program will launch the graphical interface
#' @param login_window Boolean, if \code{TRUE} a login window will be displayed asking the user to specify
#' user name.
#' @param database_expected Boolean, if \code{TRUE} pre launch tests will be run to test the connection validity
#' @usage stacomi(gr_interface=TRUE,login_window=TRUE,database_expected=TRUE)
#' @import stringr
#' @import RColorBrewer
#' @import gWidgets
#' @import gWidgetsRGtk2
#' @import RGtk2
#' @import ggplot2
#' @import RPostgreSQL
#' @import methods
#' @import stacomirtools
#' @import RODBC
#' @import RGtk2
#' @import sqldf
#' @import xtable
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr rename
#' @importFrom dplyr do
#' @importFrom dplyr filter
#' @importFrom dplyr mutate 
#' @importFrom dplyr min_rank 
#' @importFrom dplyr first
#' @importFrom dplyr ungroup
#' @importFrom dplyr desc
#' @importFrom intervals Intervals
#' @importFrom intervals closed<-
#' @importFrom intervals interval_overlap
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom grid grid.newpage
#' @importFrom grid grid.layout
#' @importFrom utils read.csv
#' @importFrom utils stack
#' @importFrom utils globalVariables
#' @importFrom utils select.list write.table data
#' @importFrom stats ftable
#' @importFrom stats xtabs
#' @importFrom stats AIC
#' @importFrom grDevices dev.new
#' @importFrom grDevices gray.colors
#' @importFrom stats sd
#' @importFrom stats complete.cases
#' @importFrom reshape2 dcast
#' @importFrom reshape2 melt
#' @importFrom lattice barchart trellis.par.get trellis.par.set simpleKey
#' @importFrom grid gpar
#' @importFrom graphics layout matplot mtext points polygon segments par axis text legend rect axis.Date abline arrows hist
#' @importFrom stats as.formula coef na.fail nls pbeta predict sd coefficients
#' @importFrom grDevices gray rainbow adjustcolor
#' @importFrom lubridate round_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate %m+%
#' @importFrom lubridate isoweek
#' @importFrom lubridate years
#' @importFrom Hmisc wtd.quantile 
#' @importFrom Hmisc capitalize 
#' @importFrom mgcv gam
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' 
#'  require(stacomiR)
#' #launch stacomi with the graphical interface
#'  \dontrun{ 	
#' 	stacomi()
#' }
#'  # launch stacomi but do not prompt for password
#'  \dontrun{	
#' 	stacomi(login_window=FALSE)
#' }  
#' #launch stacomi without connection to the database
#' stacomi(gr_interface=FALSE,login_window=FALSE,database_expected=FALSE)
#' 
#' @export
stacomi=function(gr_interface=TRUE,login_window=TRUE,database_expected=TRUE){
  # first loading of connection and odbc info using chargexml()
  # trois variables passees a interface graphique via envir_stacomi :
  #envir_stacomi <- new.env(parent = asNamespace("stacomiR"))
  assign("gr_interface",gr_interface,envir=envir_stacomi)	
  assign("database_expected",database_expected,envir=envir_stacomi)
  assign("login_window",login_window,envir=envir_stacomi)
  mylinks=chargecsv(database_expected)
  baseODBC=mylinks[["baseODBC"]]
  datawd=mylinks[["datawd"]]
  lang=mylinks[["lang"]] # deprecated this option is set at the higher level in R	
  sqldf.options=mylinks[["sqldf.options"]]	
  # values assigned in the envir_stacomi
  assign("datawd",datawd,envir=envir_stacomi)
  assign("sqldf.options",sqldf.options,envir=envir_stacomi)
  # default the usrname and usrpwd come from baseODBC
  
  
  # the following values may be overridden later in load_stacomi()
  assign("baseODBC",baseODBC,envir=envir_stacomi)
  assign("sch",paste(baseODBC[2],".",sep=""),envir=envir_stacomi)
  
  #libraries()
  options(sqldf.RPostgreSQL.user = sqldf.options["sqldf.uid"], 
	  sqldf.RPostgreSQL.password =sqldf.options["sqldf.pwd"],
	  sqldf.RPostgreSQL.dbname = sqldf.options["sqldf.dbname"],
	  sqldf.RPostgreSQL.host = sqldf.options["sqldf.host"],#  1.100.1.6
	  sqldf.RPostgreSQL.port = sqldf.options["sqldf.port"])
  # loginWindow, will call the load_stacomi handler
  # user login
  if (gr_interface&login_window&database_expected){
	logw <- gWidgets::gwindow(gettext("Connection",domain="R-stacomiR"), 
		name="log",
		parent=c(0,0),
		width=100,height=100)
	assign("logw",logw,envir=envir_stacomi)
	logly=gWidgets::glayout(container=logw)
	usrname<- gWidgets::gedit(text = baseODBC[2], 
		width = 10, 
		container = logly)
	assign("usrname",usrname,envir_stacomi)
	usrpwd<- gWidgets::gedit(text = baseODBC[3], 
		width = 10, 
		container = logly)
	assign("usrpwd",usrpwd,envir_stacomi)
	but=gWidgets::gbutton(text =  gettext("Login",domain="R-stacomiR"),
		border=TRUE, 
		handler = load_stacomi, 
		container = logly)
	logly[1,1]<-gettext("User",domain="R-stacomiR")
	logly[2,1]<-gettext("Password",domain="R-stacomiR")
	logly[1,2]<-usrname
	logly[2,2]<-usrpwd
	logly[3,2]<-but
  } else {
	assign("usrname",baseODBC[2],envir_stacomi)
	assign("usrpwd",baseODBC[3],envir_stacomi)
	load_stacomi(h=NULL)
  }
  invisible(NULL)
}




#' Program launch, this function launches the GwidgetRgtk graphical
#' interface to stacomi. To be able to run, some widgets (win, grouptotal, group...) 
#' are assigned in the user environment \code{.GlobalEnv}. 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
interface_graphique=function(){
  if (exists("ggroupboutonsbas"))  rm(list=c("ggroupboutonsbas"),envir=envir_stacomi) 
  if (exists("group"))  rm(list=c("group"),envir=envir_stacomi)
  if (!file.exists(path.expand(get("datawd",envir=envir_stacomi)))) {
	dir.create(path.expand(get("datawd",envir=envir_stacomi)))
  }
  
  col.sortie=rep(c("pink","purple","red","orange","green","blue","cyan","magenta"),20)  # couleurs pour le texte
  # beware these must be standard colors usable by gWidgets
  assign("col.sortie",col.sortie,envir_stacomi)
  nbligne=0
  assign("nbligne",nbligne,envir_stacomi)
  
  win <- gWidgets::gwindow(gettext("Migratory treatment",domain="R-stacomiR"), name="main",parent=c(0,0),width=400,height=100)
  assign("win",win,envir=envir_stacomi)
  
  ## Menubar is defined by a list
  menubarlist = list()
  
  menubarlist[[gettext("Station")]][[gettext("Fishway",domain="R-stacomiR")]]$handler =h_report_df
  menubarlist[[gettext("Station")]][[gettext("Fishway",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-rarrow"
  menubarlist[[gettext("Station")]][[gettext("Counting Device",domain="R-stacomiR")]]$handler =h_report_dc
  menubarlist[[gettext("Station")]][[gettext("Counting Device",domain="R-stacomiR")]]$icon = "gWidgetsRGtk2-plot"
  menubarlist[[gettext("Station")]][[gettext("Operation (TODO)",domain="R-stacomiR")]]$handler=h_report_ope
  menubarlist[[gettext("Station")]][[gettext("Operation (TODO)",domain="R-stacomiR")]]$icon="gtk-cancel"#"gtk-go-forward"
  menubarlist[[gettext("Station")]][[gettext("Fishway without counting device (TODO)",domain="R-stacomiR")]]$handler=h_report_dfdc
  menubarlist[[gettext("Station")]][[gettext("Fishway without counting device (TODO)",domain="R-stacomiR")]]$icon="gtk-cancel"
  
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Migration",domain="R-stacomiR")]]$handler=h_report_mig
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Migration",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-curve"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Multiple migrations",domain="R-stacomiR")]]$handler=h_report_mig_mult
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Multiple migrations",domain="R-stacomiR")]]$icon="gtk-dnd-multiple"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Yearly",domain="R-stacomiR")]]$handler=h_report_mig_annual
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Yearly",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-barplot"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Inter annual migration",domain="R-stacomiR")]]$handler=h_report_mig_interannual
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Inter annual migration",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-hist"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Sample characteristics",domain="R-stacomiR")]]$handler=h_report_sample_char
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Sample characteristics",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-newplot"#"gWidgetsRGtk2-logical"
  
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Environnemental conditions",domain="R-stacomiR")]]$handler=h_report_env
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Environnemental conditions",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-curve"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Migration. ~Environnemental conditions",domain="R-stacomiR")]]$handler=h_report_mig_env
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Migration. ~Environnemental conditions",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-plot1"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Migration / quant. param. / qual. param.",domain="R-stacomiR")]]$handler=h_report_mig_char
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Migration / quant. param. / qual. param.",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-curve"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Species",domain="R-stacomiR")]]$handler=h_report_species	
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Species",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-polar"#"gWidgetsRGtk2-boxplot"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Average weight glass eel",domain="R-stacomiR")]]$handler=h_report_ge_weight
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Average weight glass eel",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-cloud"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Lenghts",domain="R-stacomiR")]]$handler=h_report_size
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Lenghts",domain="R-stacomiR")]]$icon="gtk-cancel"#"gWidgetsRGtk2-scatterplot3d"#"gWidgetsRGtk2-boxplot"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Pigmentary stages",domain="R-stacomiR")]]$handler=h_report_pigment_stages
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Pigmentary stages",domain="R-stacomiR")]]$icon="gtk-cancel"#"gWidgetsRGtk2-contour"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Silver eel",domain="R-stacomiR")]]$handler=h_report_silver
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Silver eel",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-bubbles"
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Treatment size - age salmonids",domain="R-stacomiR")]]$handler=h_report_sea_age
  menubarlist[[gettext("Summary",domain="R-stacomiR")]][[gettext("Treatment size - age salmonids",domain="R-stacomiR")]]$icon="gWidgetsRGtk2-hist"
  
  gWidgets::add(win, gmenu(menubarlist))
  ggrouptotal<- gWidgets::ggroup(horizontal=FALSE)        
  # gsortie is above the window
  assign("ggrouptotal",ggrouptotal,envir=envir_stacomi) 
  
  gWidgets::add(win,ggrouptotal)
  
  gSortie=gWidgets::gtext(gettext("Output of the program\n",domain="R-stacomiR"),width =400 , height = 100,
	  font.attr=list(style="italic", col="blue",family="monospace",sizes="medium"))
  assign("gSortie",gSortie,envir=envir_stacomi) 
  
  gWidgets::add(ggrouptotal,  gSortie,  expand=FALSE)
# un groupe en dessous mais cette fois horizontal
  ggrouptotal1<- gWidgets::ggroup(horizontal=TRUE) 
  assign("ggrouptotal1",ggrouptotal1,envir=envir_stacomi) 
  
  gWidgets::add(ggrouptotal,ggrouptotal1,expand=FALSE)
  
# De nouveau un groupe vertical de boutons qui sera pousse a gauche quand le graphique sera insere
  ggroupboutons=gWidgets::ggroup(horizontal=FALSE)
  assign("ggroupboutons",ggroupboutons,envir=envir_stacomi)
  
  gWidgets::add(ggrouptotal1,ggroupboutons,expand=FALSE)
}
# Variables used in aes arguments generate a note as being assigned to .GlobalEnv, either use aes_string,
# or listing them below removes the warning in Rcheck. 
utils::globalVariables(c("quinzaine", "mois","val_quant","time.sequence","Effectifs",
		"..density..","Cumsum","Date","Effectif","Effectif_total",
		"annee","car_val_identifiant","car_valeur_quantitatif","coef","date_format",
		"debut_pas","effectif","effectif_CALCULE","effectif_EXPERT","effectif_MESURE","effectif_PONCTUEL",
		"effectif_total","report_df","quantite_CALCULE",
		"quantite_EXPERT","quantite_MESURE","quantite_PONCTUEL","libelle","null","type",
		'val_libelle','lot_effectif','lot_identifiant','ope_dic_identifiant','ope_identifiant','dev_code',
		'dev_libelle','ope_date_fin','report_stage_pigm','ope_date_debut','p','g','poids_moyen',
		'taxa_stage','jour',"valeur","mintab","maxtab","moyenne","jour","total_annuel",
		"taxa_stage","time.sequence","sum","variable","duree","Hdeb","Hfin","per_tar_code",
		"per_etat_fonctionnement","std_libelle","sumduree","dc","stage","taxa","stage","ouv",
		"Q0","Q100","Q5","Q50","Q95","age","bjo_annee","bjo_labelquantite","bjo_valeur","doy",
		"pred_weight","pred_weight_lwr","pred_weight_upr","total","w","year","sta","tableauCEst","stm_libelle",
		"env_valeur_quantitatif","env_val_identifiant","DC","color"))

# variable used by dplyr
utils::globalVariables(c("n0","newid","xmin","xmax"))

# dataset used in the package
utils::globalVariables(c("coef_durif"))
# Assignation in global environment for the use of gWidget interface (there is no way arround this)
#utils::globalVariables(c("win","group","nbligne","ggrouptotal","ggrouptotal1","gSortie",
#				"col.sortie","ggroupboutons","ggroupboutonsbas","groupdate","groupdc",
#				"frame_annee","frame_check","frame_choice","frame_par","frame_parqual","frame_parquan",
#				"frame_std","frame_tax","frame_annee","frame_check","frame_choice","ref_year",
#				"logw","report_stage_pigm","usrname","usrpwd","notebook","values","ind","progress_bar","progres"))
# Progressbar
#utils::globalVariables(c("progres"))
# environment
#utils::globalVariables(c("envir_stacomi"))
# reoxygenize fails if data are not loaded
#setwd("F:/workspace/stacomir/branch0.5/stacomir")



## THESE LINES MUST BE UNCOMMENTED IN ORDER TO MAKE THE DOCUMENT METHOD FROM DEVTOOL WORK
#calcmig<-
#		structure(list(lienODBC = structure(1L, .Label = "bd_contmig_nat", class = "factor"), 
#						uid = structure(1L, .Label = "iav", class = "factor"), pwd = structure(1L, .Label = "iav", class = "factor"), 
#						sqldf.uid = structure(1L, .Label = "test", class = "factor"), 
#						sqldf.pwd = structure(1L, .Label = "test", class = "factor"), 
#						sqldf.dbname = structure(1L, .Label = "test", class = "factor"), 
#						sqldf.host = structure(1L, .Label = "localhost", class = "factor"), 
#						sqldf.port = 5432L, pgwd = structure(1L, .Label = "F:/workspace/stacomir/pkg/stacomir/R/", class = "factor"), 
#						datawd = structure(1L, .Label = "~/CalcmigData", class = "factor"), 
#						lang = structure(1L, .Label = "French", class = "factor")), .Names = c("lienODBC", 
#						"uid", "pwd", "sqldf.uid", "sqldf.pwd", "sqldf.dbname", "sqldf.host", 
#						"sqldf.port", "pgwd", "datawd", "lang"), class = "data.frame", row.names = c(NA, 
#						-1L))
#' Working environment for stacomiR created when launching stacomi()
#' 
#' This is where the graphical interface stores its objects
#' try \code{ls(envir=envir_stacomi)}
#' @keywords environment
#' @export
envir_stacomi <- new.env(parent = asNamespace("stacomiR"))
#calcmig<-data.frame()