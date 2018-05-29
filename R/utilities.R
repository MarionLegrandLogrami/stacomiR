#' function used to clean the objects within the group and the graphs and
#' also elements remaining in the envir_stacomi environment
#' 
#' 
#' @param \dots additional arguments passed to the function
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
quitte=function(...){
  # all gWidgets object are assigned in .GlobalEnv, the other are in envir_stacomi
  # note R3.4.0 calling below introduces a bug (cannot destroy external pointer)
  # so we call dispose on any object which will destroy the top-level window
  # and we call it again
#	if (exists("ggroupboutonsbas",envir=.GlobalEnv)) 
#		# delete for something added with the add method
#		delete(ggroupboutons,ggroupboutonsbas)
#		
#
#	if (exists("notebook",envir=envir_stacomi))
#		dispose(notebook)
#	
#
#	if (exists("group",envir=.GlobalEnv)) {
#		delete(ggroupboutons,group) 
#		rm(group,envir= .GlobalEnv)
#	}
  if (exists("win",envir=envir_stacomi)){
	win<-get("win",envir_stacomi)
	dispose(win)
  }
  if (exists("envir_stacomi")){
	miettes <- ls(envir=envir_stacomi)
	if (length(miettes)> 0 ) {
	  miettes=miettes[!miettes%in%c("datawd","sch","baseODBC","usrname","usrpwd","database_expected","gr_interface","login_window","sqldf.options")]
	  rm(list=miettes,envir=envir_stacomi)
	}      
  }
#	if (length(ls(pattern="frame",envir=envir_stacomi))!=0) {
#		rm(list=ls(pattern="frame",envir=envir_stacomi),envir=envir_stacomi)
#	}
  if (exists("g")) rm(g)
  interface_graphique()
}

#' function used for some lattice graphs with dates 
#' @param vectordate date or POSIXt 
#' @return vectordate (without class)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
graphdate<-function(vectordate){
  vectordate <- as.POSIXct(vectordate)
  attributes(vectordate) <- NULL
  unclass(vectordate)
  return(vectordate)
}







#' function used to remove special non utf8 character which cause the gtk
#' interface to crash
#' 
#' 
#' @param text a text string which might contain no utf8 characters
#' @return text
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
fun_char_spe<-function(text){
  text <- gsub("\u00e9","e",text) 
  text <- gsub("\u00e8","e",text) 
  text <- gsub("\u00ea","e",text) 
  text <- gsub("\u00e0","a",text) 
  return(text)}






#' this function uses gfile, edits a text with info and changing colors
#' 
#' 
#' @param text The text to display both in the gtk interface and in the R
#' console
#' @param arret Should this cause the program to stop ?
#' @param wash Should the console be cleared after displaying the message
#' @param ... Additional parameters passed to print
#' @return nblignes Assigned in envir_stacomi
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
#' @export
# internal= funout is exported to ease debug during tests but not showns to users
funout<-function(text,arret=FALSE,wash=FALSE,...){
  if (exists("gSortie",envir=envir_stacomi)) {
	gSortie<-get("gSortie",envir=envir_stacomi)
	nbligne<-get("nbligne",envir=envir_stacomi)
	col.sortie<-get("col.sortie",envir_stacomi)
	if (isExtant(gSortie)){
	  if (wash) dispose(gSortie)
	  
	  nbligne=nbligne+1
	  text<-fun_char_spe(text)
	  add(gSortie,
		  text,
		  do.newline=FALSE,font.attr=list(
			  style="italic", 
			  col=col.sortie[nbligne],
			  family="monospace",
			  sizes="medium"),
		  where="beginning")
	  if (nbligne==20) nbligne <- 1
	  assign("nbligne",nbligne,envir=envir_stacomi)
	} else {
	  # gSortie exists but has not been removed
	  rm("gSortie",envir=envir_stacomi)
	}
  } 
  # this is printed anyway
  if(arret) stop(text) else print(text,quote=FALSE,...)
}






#' chargecsv loads the informations stored in c:/program
#' files/stacomi/calcmig.csv file
#' 
#' be sure to configure your odbc link to the
#' database, the name is the name of the first column of the calcmig.csv file. 
#' 	\code{uid}, \code{pwd} are identifier and password to connect to the database, they should
#' correspond to your own schema in the database. \code{pgwd} is the path to the R
#' source if you plan not to use the compiler but run manually using inst/config/stacomi_manual_launch.R for development.\cr
#' \code{datawd}, is the
#' directory where you want to place the outputs, mostly tables, from the
#' program, default to ~//CalcmigData 
#' \code{lang}, is either one of French, English or Spanish (deprecated)
#' 	other fields correspond to sqldf options.
#' @param database expected Are the program (stacomi directory) and database expected to be installed,
#' this argument is necessary to pass tests on system where stacomi is not installed (e.g. R-forge)
#' @note A version of the calcmig.csv is packaged in the config directory of the stacomiR library.
#' 
#' @return a list with the datawd place and the baseODBC vector
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
#' @keywords internal
chargecsv=function(database_expected){ 
  #library(XML)  # chargement du package XML
  options(guiToolkit = "RGtk2")
  # use of stringr package 
  # by default the csv file is in C:/Program Files/stacomi/ and we don't want to change that
  # note this will only be tested once the program is packages otherwise the path is inst/config/calcmig.csv
  
  
  if (database_expected) {
	filecsv<-"C:/Program Files/stacomi/calcmig.csv"
	test<-file.access(filecsv,0)==0
	if (test) {
	  calcmig<-utils::read.csv(filecsv,header=TRUE,sep=";")
	  # then we test using the file from the package in the config folder
	} else {
	  # the access to csv file failed despite database_expected=true
	  # if the file does not open, we switch to the file located within the package
	  cat("C:/program files/stacomi/calcmig.csv does not exist, switching to defaut package file")
	  data("calcmig",envir=environment())				
	}
  } else {
	# no access to the database is expected, we are using the file in the data directory of the package
	data("calcmig",envir=environment())
	test<-FALSE
  }
  tableau_config = t(calcmig) # renvoit une liste
  datawd=tableau_config["datawd",]
  lang=tableau_config["lang",]
#pgwd=tableau_config["pgwd",]
  baseODBC=c(tableau_config["lienODBC",],tableau_config["uid",],tableau_config["pwd",])
  sqldf.options=c(tableau_config["sqldf.uid",],tableau_config["sqldf.pwd",],tableau_config["sqldf.dbname",],tableau_config["sqldf.host",],tableau_config["sqldf.port",])
  return(list("datawd"=datawd,"baseODBC"=baseODBC,"lang"=lang,"sqldf.options"=sqldf.options))
}






#' Transforms a vector into a string called within an sql command e.g.
#' c('A','B','C') => in ('A','B','C')
#' 
#' Transforms a vector into a string called within an sql command e.g. c(A,B,C)
#' => in ('A','B','C')
#' 
#' 
#' @param vect a character vector
#' @return listsql a list of value
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
vector_to_listsql<-function(vect)
{
  if (is.null(vect)) stop("The vector passed to vector_to_listsql should not be null")
  if (any(is.na(vect))) stop("The vector passed to vector_to_listsql should not be NA")
  if (length(vect)==0) stop("The vector passed to vector_to_listsql should not be of lenght zero")
  if (length(vect)==1) 
  {
	listsql=paste("(","'",vect,"'",")",sep="")
  }
  
  if (length(vect)>2)
  {
	listsql=paste("(","'",vect[1],"'",",", sep="")
	for(j in 2:(length(vect)-1)){
	  listsql=paste(listsql,"'",vect[j],"'",",",sep="")
	}
	listsql=paste(listsql,"'",vect[length(vect)],"'",")", sep="")
  } 
  else if  (length(vect)==2)
  {
	listsql=paste("(","'",vect[1],"'",",", sep="")
	listsql=paste(listsql,"'",vect[length(vect)],"'",")", sep="") 
  }
  
  return(listsql)
} 


#' Progress bar using a gtkdialog, the progress bar is assigned in envir_stacomi
#' This progress bar has a button to close.
#' @note The name of the progress bar is \code{progres}, it will be assigned in envir_stacomi,
#' it contains a progress bar widget named progress bar, also assigned in envir_stacomi See example for use.
#' @param title The title of the bar
#' @param progress_text The text to display for progression
#' @param width Width of the progress bar
#' @param height Height of the progress bar
#' @param pulse Do you want the widget to pulse
#' @return nothing 
#' 
#' @author cedric.briand
#' @examples 
#' \dontrun{
#' progress_bar(title="Trial",progress_text="progress text")
#' fraction_progressed=seq(0,1,length.out=50)
#'  progress_bar<-get("progress_bar",envir_stacomi)
#' for(i in fraction_progressed){ 
#'      Sys.sleep(0.1)
#'    progress_bar$setText(sprintf("%d%% progression",round(100*i)))
#'     progress_bar$setFraction(i)
#' }
#'dispose(progres)
#' }
#' @export
progress_bar<-function(title,progress_text,width=400,height=50,pulse=TRUE){
  .dialog <- RGtk2::gtkDialog(title=title, NULL, NULL,
	  "gtk-close", RGtk2::GtkResponseType["none"],
	  show = FALSE)
  assign("progres",.dialog,envir=envir_stacomi)
  ## Ensure that the dialog box is destroyed when the user responds.
  RGtk2::gSignalConnect(.dialog, "response", RGtk2::gtkWidgetDestroy)	
  progress_bar <- RGtk2::gtkProgressBar()
  assign("progress_bar",progress_bar,envir_stacomi)
  RGtk2::gtkWidgetSetSizeRequest(progress_bar,width=width,height=height)
  .dialog[["vbox"]]$add(progress_bar)
  progress_bar$setText(progress_text)
  if (pulse) RGtk2::gtkProgressBarPulse(progress_bar)
  .dialog$showAll()
  return(invisible(NULL))
}



#' Create a dataframe suitable for charts per 24h and day
#' 
#' This functions takes a data frame with a column with starting time and another with ending time
#' If the period extends over midnight, it will be split into new lines, starting and ending at midnight
#' 
#' @param data The dataframe
#' @param horodatedebut The beginning time
#' @param horodatefin The ending time
#' @return A data frame with four new columns, Hmin (hour min), Hmax (hmax), xmin (day) and xmax (next day),
#' and new rows
#' @author cedric.briand
#' @examples
#' datatemp<-structure(list(per_dis_identifiant = c(1L, 1L, 1L), 
#' per_date_debut = structure(c(1420056600, 
#'	1420071000, 1420081200), class = c("POSIXct", "POSIXt"), tzone = ""), 
#'	per_date_fin = structure(c(1420071000, 1420081200, 1421000000
#'	), class = c("POSIXct", "POSIXt"), tzone = ""), per_commentaires = c("fonct calcul", 
#'	"fonct calcul", "fonct calcul"), per_etat_fonctionnement = c(1L, 
#'	0L, 0L), per_tar_code = 1:3, libelle = c("Fonc normal", "Arr ponctuel", 
#'	"Arr maint")), .Names = c("per_dis_identifiant", "per_date_debut", 
#'	"per_date_fin", "per_commentaires", "per_etat_fonctionnement", 
#'	"per_tar_code", "libelle"), row.names = c(NA, 3L), class = "data.frame")
#'newdf<-split_per_day(data=datatemp,horodatedebut="per_date_debut",
#' horodatefin="per_date_fin")
#' @export
split_per_day<-function(data,horodatedebut,horodatefin){
  if(!horodatedebut%in%colnames(data)) stop("horodatedebut not in column names for data")
  if(!horodatefin%in%colnames(data)) stop("horodatefin not column names for data")	
  data$Hdeb<-as.numeric(strftime(data[,horodatedebut],"%H"))+as.numeric(strftime(data[,horodatedebut],"%M"))/60
  data$Hfin<-as.numeric(strftime(data[,horodatefin],"%H"))+round(as.numeric(strftime(data[,horodatefin],"%M"))/60,2)
  data$xmin<-lubridate::floor_date(data[,horodatedebut],unit="day") # pour les graphiques en rectangle
  data$xmax<-data$xmin+lubridate::days(1)
  # number of times we pass to midnigth
  # round is for when we switch hour
  data$n0<-round(difftime(floor_date(data[,horodatefin],unit="day"),floor_date(data[,horodatedebut],unit="day"),units="days"))
  # rows that will be duplicated
  data$id=sequence(nrow(data))
  data<-data[rep(sequence(nrow(data)),data$n0+1),]
  data$newid<-sequence(nrow(data))
  # within a group where dates overlap between two days
  #the first will and all lines except the last be set 24 for Hfin
  data1<-data%>%filter(n0>0)%>%group_by(id)%>%filter(min_rank(desc(newid)) !=1)%>%mutate("Hfin"=24)
  #replacing rows in data
  data[match(data1$newid,data$newid),]<-data1
  # all except the first will be set 0 to Hdeb
  data2<-data%>%filter(n0>0)%>%group_by(id)%>%filter(min_rank(newid) !=1)%>%mutate("Hdeb"=0)
  #replacing rows in data
  data[match(data2$newid,data$newid),]<-data2
  # now get the sequence of days righly set by adding the number of days to xmin and xmax
  data3<-data%>%filter(n0>0)%>%group_by(id)%>%mutate(xmin=xmin+ as.difftime(rank(newid)-1, units="days"),
	  xmax=xmax+as.difftime(rank(newid)-1, units="days"))
  data[match(data3$newid,data$newid),]<-data3
  data<-as.data.frame(data)	
  return(data)
}

#' This function extracts temporal characteristics from a dataframe
#' 
#' 
#' @param data a data frame containing a Date or POSIXt column
#' @param nom_coldt the name of the column containing date or POSIXt entry to
#' be processed
#' @param annee logical do you want a column describing year to be added to the
#' dataframe
#' @param mois logical, add column with month
#' @param quinzaine logical, add column with 15 days
#' @param semaine logical, add column with weeks
#' @param semaine_std logical, add column with standard weeks (using isoweek from lubridate)
#' @param jour_an logical, add column with day of year
#' @param jour_mois logical, add column with day of month
#' @param heure logical, add column with hour
#' @return data The dataframe with date column filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
fun_date_extraction=function(data, # tableau de donnees e importer
	nom_coldt, # nom de la colonne
	annee=TRUE,
	mois=TRUE,
	quinzaine=FALSE,
	semaine=TRUE,
	semaine_std=FALSE,
	jour_an=FALSE,
	jour_mois=TRUE,
	heure=FALSE                           
){
  if (annee) data$annee <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%Y"))                        
  if (mois) data$mois <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%m"))
  # %b Abbreviated month name in the current locale. (Also matches full name on input.)
  if (quinzaine) {
	data$quinzaine=ceiling(as.numeric(strftime(as.POSIXlt(data[,nom_coldt]),
				format="%W"))/2)
	data$quinzaine <- as.character(data$quinzaine)
	data$quinzaine[as.numeric(data$quinzaine)<10] <- paste("0", data$quinzaine[as.numeric(data$quinzaine)<10],sep="")
	data$quinzaine <- as.factor(data$quinzaine)
  }
  if (semaine) data$semaine <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%W"))
  #%W : Week of the year as decimal number (00e53) using Monday as the first day of week (and typically with the first Monday of the year as day 1 of week 1). The UK convention
  if (jour_an) data$jour_365 <- strftime(as.POSIXlt(data[,nom_coldt]), format="%j")                          
  if (jour_mois) data$jour_mois <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%d"))  
  # %d :  Day of the month as decimal number (01e31).
  if (heure) data$jour_mois <- as.factor(strftime(as.POSIXlt(data[,nom_coldt]), format="%H"))  
  #%H     Hours as decimal number (00e23).    
  if (semaine_std) data$semaine_std=lubridate::isoweek(as.POSIXlt(data[,nom_coldt]))
  return(data)
}    


#' Builds a table with colors to merge with a dataframe for later
#' use in ggplot. An initial check will be done
#' on the name of the color vector. A data frame is built. It contains a column color which is a factor.
#' The factor order match the order of the vector (not the alphabetical order of the colors).

#' 
#' @param color Either null (default) or a named vector of colors, the
#' names should correspond to the values of vec 
#' @param  vec The vector to match the color with, if a named vector
#' or color is supplied the names should match
#' @param palette, the name of the RColorBrewer palette, defaults to "Set2", ignored for other
#' color gradient functions and if a named vector of colors is provided
#' @param color_function, the name of the function used to brew the colors, one for 
#' "brewer.pal", "gray.colors", default to "brewer.pal, this argument is ignored if a
#' named vector of color is passed.
#' @return A dataframe with two columns, the vector (name) and the color (color) as a reordered factor
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
colortable<-function(color=NULL, vec, palette="Set2", color_function="brewer.pal"){
  if (is.null(color)) {
	if (color_function == "brewer.pal") {
	  color <- RColorBrewer::brewer.pal(length(vec),name=palette)[1:length(vec)]
	} else if (color_function == "gray.colors"){
	  color=grDevices::gray.colors(length(vec))
	}
	names(color)<-vec
  } else if (length(color) != length(vec)){
	funout(gettextf("The color argument should have length %s", length(vec)), arret=TRUE)
  }
  if (!all(names(color)%in%vec)) {
	stop (gettextf("The following name(s) %s do not match vector name: %s",
			names(color)[!names(color)%in%vec],
			paste(vec, collapse=", ")))
  }
  # creating a data frame to pass to merge later (to get the color in the data frame)
  cs <- data.frame(name=names(color), color=color)
  # problem with different order (set by color name) implying different order
  # in the graph (ie by color not by car_val_identifiant
  bonordre <- match(cs$color, levels(cs$color))
  cs$color  <-  factor(cs$color, levels(cs$color)[bonordre])
  return(cs)
}


#' Retrieves the dbname from a connection using "baseODBC"
#' 
#' When running a connection using ODBC, the connection string does not contain
#' the name of the database. Pointing to the database is done while setting the ODBC
#' connection, but the program has no "way" to know what the name of the database is.
#' Since we are using sqldf to connect to the base, we need to know the database name, fortunately
#' it is stored in the ODBC connection string. This method gets this name, from
#' a "trial" connection to one of the table of the database.
#' @return A string with the name of the database
getdbname<-function(){
  req <- new("RequeteODBC")
  req@baseODBC <- get("baseODBC",envir=envir_stacomi)
  req@sql <- str_c("select * from ", 
	  get("sch",envir=envir_stacomi), 
	  "t_bilanmigrationjournalier_bjo limit 1")
  res <- connect(req)
  odbc <- res@connection
  aa <- strsplit(attributes(odbc)$connection.string, ";")
  dbname <- gsub("DATABASE=","",aa[[1]][2])
  return(dbname)
}
