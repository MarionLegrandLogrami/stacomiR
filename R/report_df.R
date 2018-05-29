#' Report on fishway operation
#' 
#' Fishways (DF) are of various nature, from very simple eel ladders fed by water discharged from the river,
#' to more complex fishways with levels adjusted by the opening of various gates and regulators. 
#' The objective of this class is to provide an assessment of the working status of a fishway throughout the year.
#' A number of fishes ascending a fishway has meaning only if we know that the fishway is operational, and that the counting 
#' operated on the fishway has remained operational.
#' In the database the operation of the fishway (DF) and counting device (DC) is agregated in one table (t_periodefonctdispositif_per).
#' The column  per_etat_fonctionnement indicates whether the fishway is operational (with a boolean) and the column per_tar_code indicates
#' the status of either the fishway or DC. In the database four types of operation are set,  "1"=normal operation,
#' "2"=Device stopped in normal operation (ie lift ascending, high tide...),
#' "3"="Stopped for maintenance or other problem",
#' "4"="Works but not fully operational, ie flow problem, flood, clogged with debris...",
#' "5"="Not known")
#' 
#' @include ref_df.R
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("report_df")}.
#' @slot data A data frame 
#' @slot df An object of class \code{ref_df-class}
#' @slot horodatedebut An object of class \code{ref_horodate-class}
#' @slot horodatefin An object of class \code{ref_horodate-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family report Objects
#' @keywords classes
#' @example inst/examples/report_df-example.R
#' @aliases report_df
#' @export 
setClass(Class="report_df",
	representation= representation(data="data.frame",
		df="ref_df",
		horodatedebut="ref_horodate",
		horodatefin="ref_horodate"
	),
	prototype=prototype(data=data.frame(),df=new("ref_df"),
		horodatedebut=new("ref_horodate"),
		horodatefin=new("ref_horodate")				
	)
)


#' connect method for report_df
#' 
#' @param object An object of class \link{report_df-class}
#' loads the working periods and type of arrest or disfunction of the DF
#' @param silent Boolean, TRUE removes messages.
#' @return  An object of class \code{report_df}
#' @aliases connect.report_df
#' @author cedric.briand
setMethod("connect",signature=signature("report_df"),definition=function(object,silent=FALSE) {
#  construit une requete ODBCwheredate
	  req<-new("RequeteODBCwheredate")
	  req@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  req@select= paste("SELECT",
		  " per_dis_identifiant,",
		  " per_date_debut,",
		  " per_date_fin,",
		  " per_commentaires,",
		  " per_etat_fonctionnement,",
		  " per_tar_code,",
		  " tar_libelle AS libelle",
		  " FROM  ",get("sch",envir=envir_stacomi),"t_periodefonctdispositif_per per",
		  " INNER JOIN ref.tr_typearretdisp_tar tar ON tar.tar_code=per.per_tar_code",sep="")
	  req@colonnedebut="per_date_debut"
	  req@colonnefin="per_date_fin"
	  req@order_by="ORDER BY per_date_debut"
	  req@datedebut<-object@horodatedebut@horodate
	  req@datefin<-object@horodatefin@horodate
	  req@and=paste("AND per_dis_identifiant in",vector_to_listsql(object@df@df_selectionne))
#req@where=#defini dans la methode ODBCwheredate
	  req<-stacomirtools::connect(req) # appel de la methode connect de l'object ODBCWHEREDATE
	  object@data<-req@query
	  if (!silent) funout(gettext("Time steps of the fishway loaded\n",domain="R-stacomiR"))
	  return(object)
	})


#' charge method for report_df
#' 
#' 
#' used by the graphical interface to retrieve the objects of referential classes
#' assigned to envir_stacomi
#' @param object An object of class \link{report_df-class}
#' @param silent Keeps program silent
#' @return  An object of class \link{report_df-class}
#' @aliases charge.report_df
#' @keywords internal
setMethod("charge",signature=signature("report_df"),definition=function(object,silent=FALSE) {
	  # object<-BfDF
	  if (exists("ref_df",envir=envir_stacomi)) {
		object@df<-get("ref_df",envir=envir_stacomi)
	  } else {
		funout(gettext("You need to choose a crossing device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)
	  }     
	  
	  if (exists("report_df_date_debut",envir=envir_stacomi)) {
		object@horodatedebut@horodate<-get("report_df_date_debut",envir=envir_stacomi)
	  } else {
		funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  
	  if (exists("report_df_date_fin",envir=envir_stacomi)) {
		object@horodatefin@horodate<-get("report_df_date_fin",envir=envir_stacomi)
	  } else {
		funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)
	  }			
	  assign("report_df",object,envir=envir_stacomi)  
	  return(object)
	})

#' command line interface for report_df class
#' 
#' The choice_c method fills in the data slot for ref_df, and then 
#' uses the choice_c methods of these object to "select" the data.
#' @param object An object of class \link{ref_df-class}
#' @param df The df to set
#' @param horodatedebut A POSIXt or Date or character to fix the date of beginning of the report
#' @param horodatefin A POSIXt or Date or character to fix the last date of the report
#' @param silent Should program be silent or display messages
#' @return An object of class \link{ref_df-class} with slots filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases choice_c.report_df
#' @export
setMethod("choice_c",signature=signature("report_df"),definition=function(object,df,horodatedebut,horodatefin,silent=FALSE){
	  # report_df<-r_df;df=2;horodatedebut="2013-01-01";horodatefin="2013-12-31";silent=TRUE
	  report_df<-object
	  assign("report_df",report_df,envir=envir_stacomi)    
	  if (!silent) funout(gettext("Loading of the list for fishways and choice of the time step\n",domain="R-stacomiR"))
	  report_df@df<-charge(report_df@df)    
	  report_df@df<-choice_c(report_df@df,df)
	  # assigns the parameter (horodatedebut) of the method to the object using choice_c method for ref_dc
	  report_df@horodatedebut<-choice_c(object=report_df@horodatedebut,
		  nomassign="report_df_date_debut",
		  funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
		  horodate=horodatedebut, silent=silent)
	  report_df@horodatefin<-choice_c(report_df@horodatefin,
		  nomassign="report_df_date_fin",
		  funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
		  horodate=horodatefin,silent=silent)
	  assign("report_df",report_df,envir=envir_stacomi)  
	  return(report_df)
	})

#' Different plots for report_df
#' 
#' \itemize{
#'      \item{plot.type=1}{A barplot of the operation time per month}
#' 		\item{plot.type=2}{Barchat giving the time per type of operation }
#' 		\item{plot.type=2}{Rectangle plots drawn along a line}
#'      \item{plot.type=4}{Plots per day drawn over the period to show the operation of a df, days in x, hours in y} 
#' 	}	
#' 
#' @note The program cuts periods which overlap between two month. The splitting of different periods into month is 
#' assigned to the \code{envir_stacomi} environment
#' @param x An object of class \link{report_df-class}
#' @param y From the formals but missing
#' @param plot.type One of \code{barchart},\code{box}. Defaut to \code{barchart} showing a summary of the df operation per month, can also be \code{box}, 
#' a plot with adjacent rectangles.
#' @param silent Stops displaying the messages.
#' @param main The title of the graph, if NULL a default title will be plotted with the number of the DF
#' @return Nothing but prints the different plots
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.report_df
#' @export
setMethod("plot",signature(x = "report_df", y = "ANY"),definition=function(x, y,plot.type=1,silent=FALSE,main=NULL){ 
	  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	  #           PLOT OF TYPE BARCHART (plot.type=1 (true/false) or plot.type=2)
	  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	  #report_df<-r_df; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="1"
	  report_df<-x
	  plot.type<-as.character(plot.type)# to pass also characters
	  if (!plot.type%in%c("1","2","3","4")) stop('plot.type must be 1,2,3 or 4')
	  if (nrow(report_df@data)==0) 
		funout(gettext("No data for this fishway\n",domain="R-stacomiR"),arret=TRUE)
	  if (plot.type=="1"|plot.type=="2"){
		t_periodefonctdispositif_per=report_df@data # on recupere le data.frame   
		tempsdebut<-t_periodefonctdispositif_per$per_date_debut
		tempsfin<-t_periodefonctdispositif_per$per_date_fin
		tempsdebut[tempsdebut<report_df@horodatedebut@horodate]<-report_df@horodatedebut@horodate
		tempsfin[tempsfin>report_df@horodatefin@horodate]<-report_df@horodatefin@horodate
		t_periodefonctdispositif_per=cbind(t_periodefonctdispositif_per,tempsdebut,tempsfin)
		seqmois=seq(from=tempsdebut[1],to=tempsfin[nrow(t_periodefonctdispositif_per)],by="month",tz = "GMT")
		seqmois=as.POSIXlt(round_date(seqmois,unit="month"))
		# adding one month at the end to get a complete coverage of the final month
		seqmois<-c(seqmois,
			seqmois[length(seqmois)]%m+%months(1))
		
		#seqmois<-c(seqmois,seqmois[length(seqmois)]+months(1))
		t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per[1,]
		############################
		#progress bar
		###########################
		progress_bar(
			title=gettext("computing ...",domain="R-stacomiR"),
			progress_text=gettext("Progress %",domain="R-stacomiR"))
		progress_bar<-get("progress_bar",envir=envir_stacomi)
		z=0 # compteur tableau t_periodefonctdispositif_per_mois
		for(j in 1:nrow(t_periodefonctdispositif_per)){
		  #cat( j 
		  progress_bar$setFraction(j/nrow(t_periodefonctdispositif_per)) 
		  progress_bar$setText(sprintf("%d%% progression",round(100*j/nrow(t_periodefonctdispositif_per))))
		  #RGtk2::gtkMainIterationDo(FALSE)
		  if (j>1) t_periodefonctdispositif_per_mois=rbind(t_periodefonctdispositif_per_mois, t_periodefonctdispositif_per[j,])
		  lemoisnext=seqmois[seqmois>tempsdebut[j]][1] # le premier mois superieur a tempsdebut
		  while (tempsfin[j]>lemoisnext){    # on est a cheval sur deux periodes    
			
			#if (z>0) stop("erreur")
			z=z+1
			t_periodefonctdispositif_per_mois=rbind(t_periodefonctdispositif_per_mois, t_periodefonctdispositif_per[j,])
			t_periodefonctdispositif_per_mois[j+z,"tempsdebut"]=as.POSIXct(lemoisnext)
			t_periodefonctdispositif_per_mois[j+z-1,"tempsfin"]=as.POSIXct(lemoisnext)
			lemoisnext=seqmois[match(as.character(lemoisnext),as.character(seqmois))+1] # on decale de 1 mois avant de rerentrer dans la boucle
			#if (is.na(lemoisnext) ) break
		  }  
		  #if (is.na(lemoisnext)) break
		}
		t_periodefonctdispositif_per_mois$sumduree<-as.numeric(difftime(t_periodefonctdispositif_per_mois$tempsfin, t_periodefonctdispositif_per_mois$tempsdebut,units = "hours"))
		t_periodefonctdispositif_per_mois$mois1= strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%b")
		t_periodefonctdispositif_per_mois$mois=strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%m")
		t_periodefonctdispositif_per_mois$annee=strftime(as.POSIXlt(t_periodefonctdispositif_per_mois$tempsdebut),"%Y")
		progress_bar$setText("All done.")
		progress_bar$setFraction(1) 
		if (is.null(main)) main<-gettextf("Fishway operation %s",report_df@df@df_selectionne)
		# graphic
		t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$per_tar_code, decreasing = TRUE),]
		
		g<- ggplot(t_periodefonctdispositif_per_mois,
				aes(x=mois,y=sumduree,fill=libelle))+
			facet_grid(annee~.)+
			ylab(gettext("duration",domain="R-stacomiR"))+
			xlab(gettext("month",domain="R-stacomiR"))+
			ggtitle(main)+
			geom_bar(stat='identity')+
			scale_fill_manual(gettext("operation"),
				values = c("#E41A1C","#E6AB02", "#9E0142","#1B9E77","#999999"))
		
		t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$per_etat_fonctionnement),]
		t_periodefonctdispositif_per_mois$per_etat_fonctionnement=as.factor(	t_periodefonctdispositif_per_mois$per_etat_fonctionnement)
		
		g1<- ggplot(t_periodefonctdispositif_per_mois,aes(x=mois,y=sumduree))+facet_grid(annee~.)+
			ylab(gettext("duration",domain="R-stacomiR"))+
			xlab(gettext("month",domain="R-stacomiR"))+						
			ggtitle(main)+
			geom_bar(stat='identity',aes(fill=per_etat_fonctionnement))+
			scale_fill_manual(gettext("operation",domain="R-stacomiR"),values = c("#E41A1C","#4DAF4A")) 
		
		if (plot.type=="1")
		  print(g)
		if (plot.type=="2")
		  print(g1)		
		assign("periodeDF",t_periodefonctdispositif_per_mois,envir_stacomi)
		if (!silent) funout(gettext("Writing the table into envir_stacomi environment : write periodeDF=get('periodeDF',envir_stacomi)\n",domain="R-stacomiR"))
		# the progress bar has been assigned in envir_stacomi, we destroy it
		gtkWidgetDestroy(get("progress_bar",envir=envir_stacomi))
		#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		#           PLOT OF TYPE BOX (plot.type=3)
		#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	  } else if (plot.type=="3"){
		#report_df<-r_df; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="3"
		if (!silent) funout(gettext("No data for this fishway\n")	)
		t_periodefonctdispositif_per=report_df@data
		graphdate<-function(vectordate){
		  vectordate<-as.POSIXct(vectordate)
		  attributes(vectordate)<-NULL
		  unclass(vectordate)
		  return(vectordate)
		}
		time.sequence=seq.POSIXt(from=report_df@horodatedebut@horodate,to=report_df@horodatefin@horodate,by="day")
		debut=graphdate(time.sequence[1])
		fin=graphdate(time.sequence[length(time.sequence)])
		mypalette<-RColorBrewer::brewer.pal(12,"Paired")
		#display.brewer.all()
		mypalette1<-c("#1B9E77","#AE017E","orange", RColorBrewer::brewer.pal(12,"Paired"))
		# creation d'un graphique vide
		if (is.null(main)) main<-""
		plot(graphdate(time.sequence),
			seq(0,1,length.out=length(time.sequence)),
			xlim=c(debut,fin), 
			type= "n", 
			xlab="",
			xaxt="n",
			yaxt="n", 
			ylab=gettext("Fishway",domain="R-stacomiR"),
			main=main,
			#bty="n",
			cex=0.8)
		r <- round(range(time.sequence), "day")
		graphics::axis(1, at=graphdate(seq(r[1], r[2], by="weeks")),labels=strftime(as.POSIXlt(seq(r[1], r[2], by="weeks")),format="%d-%b"))
		if (dim(t_periodefonctdispositif_per)[1]==0 ) {
		  rect(      xleft=debut, 
			  ybottom=0.6,
			  xright=fin,
			  ytop=0.9, 
			  col = mypalette[4],
			  border = NA, 
			  lwd = 1)                     
		  rect(      xleft=debut, 
			  ybottom=0.1,
			  xright=fin,
			  ytop=0.4, 
			  col = mypalette[1],
			  border = NA, 
			  lwd = 1)
		  legend(  x= "bottom",
			  legend= gettext("Func.","Stop","Normal func.",domain="R-stacomiR"),
			  pch=c(16,16),
			  col=c(mypalette[4],mypalette[6],mypalette[1]),
			  #horiz=TRUE,
			  ncol=5,
			  bty="n")
		} else {
		  
		  if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==1)>0){ 
			rect(   xleft =graphdate(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==1]), 
				ybottom=0.6,
				xright=graphdate(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement==1]),
				ytop=0.9, 
				col = mypalette[4],
				border = NA, 
				lwd = 1) }
		  if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==0)>0)                           { 
			rect(   xleft =graphdate(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==0]), 
				ybottom=0.6,
				xright=graphdate(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement==0]),
				ytop=0.9, 
				col = mypalette[6],
				border = NA, 
				lwd = 1) }
		  listeperiode<-
			  fun_table_per_dis(typeperiode=t_periodefonctdispositif_per$per_tar_code,
				  tempsdebut= t_periodefonctdispositif_per$per_date_debut,
				  tempsfin=t_periodefonctdispositif_per$per_date_fin,
				  libelle=t_periodefonctdispositif_per$libelle,
				  date=FALSE)
		  nomperiode<-vector()
		  
		  for (j in 1 : length(listeperiode)){
			nomperiode[j]<-substr(listeperiode[[j]]$nom,1,17)   
			rect(   xleft=graphdate(listeperiode[[j]]$debut), 
				ybottom=0.1,
				xright=graphdate(listeperiode[[j]]$fin),
				ytop=0.4, 
				col = mypalette1[j],
				border = NA, 
				lwd = 1)        
		  }
		  legend  (x= debut,
			  y=0.6,
			  legend= gettext("Func.","Stop",domain="R-stacomiR"),
			  pch=c(15,15),
			  col=c(mypalette[4],mypalette[6]),
			  bty="n",
			  horiz=TRUE,
			  text.width=(fin-debut)/6 ,
			  cex=0.8
		  )                                               
		  legend  (x= debut,
			  y=0.1,
			  legend= c(nomperiode),
			  pch=c(15,15),
			  col=c(mypalette1[1:length(listeperiode)]),
			  bty="n",
			  horiz=TRUE,
			  text.width=(fin-debut)/8,
			  cex=0.7
		  )
		  text(x=debut,y=0.95, label=gettext("Fishway operation",domain="R-stacomiR"),font=4,pos=4)
		  text(x=debut,y=0.45, label=gettext("Shutdowns types for this fishway",domain="R-stacomiR"), font=4,pos=4)
		}
		#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		#           PLOT OF TYPE BOX (plot.type=4)
		#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	  } else if (plot.type=="4"){
		if (is.null(main)) main<-gettextf("Fishway operation %s",report_df@df@df_selectionne)
		
		#report_df<-r_df; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="4"
		t_periodefonctdispositif_per=report_df@data
		tpp<-split_per_day(t_periodefonctdispositif_per,horodatedebut="per_date_debut",horodatefin="per_date_fin")
		
		g<-ggplot(tpp)+
			geom_rect(aes(xmin=xmin,xmax=xmax,ymin=Hdeb,ymax=Hfin,col=factor(per_tar_code),fill=factor(per_tar_code)),alpha=0.5)+
			scale_fill_manual("type",values=c("1"="#40CA2C","2"="#C8B22D","3"="#AB3B26","4"="#B46BED","5"="#B8B8B8"),
				labels = gettext("Normal oper.","Operational stop","Stop","Dysfunct.","Unknown",domain="R-stacomiR"))+
			scale_colour_manual("type",values=c("1"="#40CA2C","2"="#C8B22D","3"="#AB3B26","4"="#B46BED","5"="#B8B8B8"),
				labels = gettext("Normal oper.","Operational stop","Stop","Dysfunct.","Unknown",domain="R-stacomiR"))+		
			ylab("Heure")+theme(
				plot.background = element_rect(fill ="black"),
				panel.background = element_rect(fill="black"),
				legend.background=element_rect(fill="black"),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				text=element_text(colour="white"),								
				line = element_line(colour = "grey50"),
				legend.key=element_rect(fill="black",colour="black"),
				axis.text=element_text(colour="white")
			)
		
		print(g)
		
	  }
	  return(invisible(NULL))
	})


#' Handler for barchart for report_df class from the graphical interface
#' 
#' @note The program cuts periods which overlap between two month
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
funbarchartDF = function(h,...) {
  report_df<-get("report_df",envir=envir_stacomi)  
  report_df=charge(report_df)	
  report_df<-connect(report_df)
  if( nrow(report_df@data)==0 ) {
	funout(gettext("No data for this fishway\n",domain="R-stacomiR"), arret=TRUE)
  }		
  plot(report_df,plot.type=1,silent=FALSE)
}   


#' Handler for barchart for report_df class from the graphical interface
#' 
#' @note The program cuts periods which overlap between two month
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
funbarchart1DF = function(h,...) {
  report_df<-get("report_df",envir=envir_stacomi)  
  report_df=charge(report_df)	
  report_df<-connect(report_df)
  if( nrow(report_df@data)==0 ) {
	funout(gettext("No data for this fishway\n",domain="R-stacomiR"), arret=TRUE)
  }		
  plot(report_df,plot.type=2,silent=FALSE)
}   
#' Internal use, rectangles to describe the DF work for report_df class, 
#' graphical interface handler
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
funboxDF = function(h,...) {
  report_df<-get("report_df",envir=envir_stacomi) 
  report_df=charge(report_df)
  report_df<-connect(report_df)
  
  if( nrow(report_df@data)==0 ) {
	funout(gettext("No data for this fishway\n",domain="R-stacomiR"), arret=TRUE)
  }
  plot(report_df,plot.type=3,silent=FALSE)
  
}   

#' Handler function to plot calendar like graph, internal use
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
funchartDF = function(h,...) {
  report_df<-get("report_df",envir=envir_stacomi) 
  report_df=charge(report_df)
  report_df<-connect(report_df)
  
  if( nrow(report_df@data)==0 ) {
	funout(gettext("No data for this fishway\n",domain="R-stacomiR"), arret=TRUE)
  }
  plot(report_df,plot.type=4,silent=FALSE)
  
}   

#' Table output for report_df class
#' @param h handler
#' @param ... additional parameters
#' @keywords internal
funtableDF = function(h,...) {
  report_df<-get("report_df",envir=envir_stacomi) 
  report_df=charge(report_df)
  report_df<-connect(report_df)
  
  if( nrow(report_df@data)==0 ) {
	funout(gettext("No data for this fishway\n",domain="R-stacomiR"), arret=TRUE)
  }
  summary(report_df)
}

#' handler to print the command line
#' @param h a handler
#' @param ... Additional parameters
#' @keywords internal
houtDF = function(h,...) {
  report_df<-get("report_df",envir=envir_stacomi) 
  report_df<-charge(report_df)
  report_df<-connect(report_df)
  #the charge method will check that all objects necessary to build the formula
  # are in envir_stacomi
  print(report_df)
  
}   

#' Method to print the command line of the object
#' @param x An object of class report_df
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @aliases print.report_df
#' @export
setMethod("print",signature=signature("report_df"),definition=function(x,...){ 
	  
	  sortie1<-"report_df=new('report_df')\n"
	  sortie2<-stringr::str_c("report_df=choice_c(report_df,",
		  "df=",x@df@df_selectionne,",",
		  "horodatedebut=",shQuote(as.character(x@horodatedebut@horodate)),",",
		  "horodatefin=",shQuote(as.character(x@horodatefin@horodate)),")")
	  # removing backslashes
	  funout(stringr::str_c(sortie1,sortie2),...)
	  return(invisible(NULL))
	})


#' summary for report_df, write csv and html output, and prints summary statistics
#' @param object An object of class \code{\link{report_df-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters (not used there)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases summary.report_df
#' @export
setMethod("summary",signature=signature(object="report_df"),definition=function(object,silent=FALSE,...){
	  #report_df<-r_df;
	  report_df<-object
	  t_periodefonctdispositif_per=report_df@data # on recupere le data.frame
	  t_periodefonctdispositif_per$per_date_debut=as.character(t_periodefonctdispositif_per$per_date_debut)
	  t_periodefonctdispositif_per$per_date_fin=as.character(t_periodefonctdispositif_per$per_date_fin)
	  #gdf(t_periodefonctdispositif_per, container=TRUE)
	  annee=paste(unique(strftime(as.POSIXlt(t_periodefonctdispositif_per$per_date_debut),"%Y")),collapse="+")
	  path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("t_periodefonctdispositif_per_DF_",report_df@df@df_selectionne,"_",annee,".csv",sep=""),fsep ="\\")
	  write.table(t_periodefonctdispositif_per,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
	  if(!silent) funout(gettextf("Writing of %s \n",path1))
	  path1html=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("t_periodefonctdispositif_per_DF_",report_df@df@df_selectionne,"_",annee,".html",sep=""),fsep ="\\")
	  if(!silent) funout(gettextf("Writing of %s this might take a while, please be patient ...\n",path1html))
	  funhtml(t_periodefonctdispositif_per,
		  caption=paste("t_periodefonctdispositif_per_DF_",report_df@df@df_selectionne,"_",annee,sep=""),
		  top=TRUE,
		  outfile=path1html,
		  clipboard=FALSE,
		  append=FALSE,
		  digits=2
	  )
	  t_periodefonctdispositif_per=report_df@data
	  print(gettextf("summary statistics for DF=%s",report_df@df@df_selectionne))
	  print(gettextf("df_code=%s",report_df@df@data[report_df@df@data$df==report_df@df@df_selectionne,"df_code"]))
	  duree<-difftime(t_periodefonctdispositif_per$per_date_fin,t_periodefonctdispositif_per$per_date_debut,units="day")
	  sommes<-tapply(duree,t_periodefonctdispositif_per$per_tar_code,sum)
	  perc<-round(100*sommes/as.numeric(sum(duree)))
	  sommes<-round(sommes,2)
	  funout(gettext("Duration in days (operation type):",domain="R-stacomiR"))
	  funout(paste(gettext("Normal oper.","Operational stop","Stop","Dysfunct.","Unknown",domain="R-stacomiR"),
			  " :",
			  sommes,"(",perc,"%)",sep=""))
	  sommes<-tapply(duree,t_periodefonctdispositif_per$per_etat_fonctionnement,sum)
	  perc<-round(100*sommes/as.numeric(sum(duree)))
	  sommes<-round(sommes,2)
	  funout(gettext("Duration in days (operation):",domain="R-stacomiR"))
	  funout(paste(rev(gettext("Func.","Stop",domain="R-stacomiR")),
			  " :",
			  sommes,"(",perc,"%)",sep=""))
	  
	})		
