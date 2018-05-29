#' Class "report_dc" report du fonctionnement du dispositif de
#' comptage
#' 
#' The counting device is not always working. It may me stopped either
#' following a monitoring protocol, or due to malfunction of the device, this
#' class allows to draw graphics allowing an overview of the device operation
#' @slot data A data frame 
#' @slot dc An object of class \code{ref_dc-class}
#' @slot horodatedebut An object of class \code{ref_horodate-class}
#' @slot horodatefin An object of class \code{ref_horodate-class}
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("report_dc", ...)}.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @example inst/examples/report_dc-example.R
#' @family report Objects
#' @keywords classes
#' @aliases report_dc
#' @export 
setClass(Class="report_dc",
	representation= representation(data="data.frame",
		dc="ref_dc",
		horodatedebut="ref_horodate",
		horodatefin="ref_horodate"), 
	prototype=prototype(data=data.frame(),
		dc=new("ref_dc"),
		horodatedebut=new("ref_horodate"),
		horodatefin=new("ref_horodate"))
)




#' connect method for report_dc
#' 
#' loads the working periods and type of arrest or disfunction of the DC
#' @param object An object of class \link{report_dc-class}
#' @param silent boolean, default FALSE, if TRUE messages are not displayed
#' @return  An object of class \link{report_dc-class}
#' @aliases connect.report_dc
#' @author cedric.briand
setMethod("connect",signature=signature("report_dc"),definition=function(object,silent=FALSE) {
	  #object<-report_dc
	  req<-new("RequeteODBCwheredate")
	  req@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  req@select= sql<-paste("SELECT",
		  " per_dis_identifiant,",
		  " per_date_debut,",
		  " per_date_fin,",
		  " per_commentaires,",
		  " per_etat_fonctionnement,",
		  " per_tar_code,",
		  " tar_libelle AS libelle",
		  " FROM  ",get("sch",envir=envir_stacomi),"t_periodefonctdispositif_per per",
		  " INNER JOIN ref.tr_typearretdisp_tar tar ON tar.tar_code=per.per_tar_code",sep="")
	  req@colonnedebut<-"per_date_debut"
	  req@colonnefin<-"per_date_fin"
	  req@datedebut<-object@horodatedebut@horodate
	  req@datefin<-object@horodatefin@horodate
	  req@order_by<-"ORDER BY per_date_debut"
	  req@and<-paste("AND per_dis_identifiant in ",vector_to_listsql(object@dc@dc_selectionne))
#req@where=#defini dans la methode ODBCwheredate
	  req<-stacomirtools::connect(req) # appel de la methode connect de l'object ODBCWHEREDATE
	  object@data<-req@query
	  if (!silent) funout(gettext("Time steps loaded fot this counting device\n",domain="R-stacomiR"))
	  return(object)
	})

#' charge method for report_dc
#' 
#' used by the graphical interface to retrieve the objects of referential classes
#' assigned to envir_stacomi
#' @param object An object of class \link{report_dc-class}
#' @param silent boolean, default FALSE, if TRUE messages are not displayed.
#' @aliases charge.report_dc
#' @return  An object of class \link{report_dc-class}
#' @keywords internal
setMethod("charge",signature=signature("report_dc"),definition=function(object,silent=FALSE) {
	  if (exists("ref_dc",envir_stacomi)) {
		object@dc<-get("ref_dc",envir_stacomi)
	  } else {
		funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)				}     
	  
	  if (exists("report_dc_date_debut",envir_stacomi)) {
		object@horodatedebut@horodate<-get("report_dc_date_debut",envir_stacomi)
	  } else {
		funout(gettext("You need to choose the starting date\n",domain="R-stacomiR"),arret=TRUE)	
	  }
	  
	  if (exists("report_dc_date_fin",envir_stacomi)) {
		object@horodatefin@horodate<-get("report_dc_date_fin",envir_stacomi)
	  } else {
		funout(gettext("You need to choose the ending date\n",domain="R-stacomiR"),arret=TRUE)	
	  }			
	  return(object)
	})


#' command line interface for report_dc class
#' 
#' The choice_c method fills in the data slot for ref_dc, and then 
#' uses the choice_c methods of these object to "select" the data.
#' @param object An object of class \link{ref_dc-class}
#' @param dc The dc to set
#' @param horodatedebut A POSIXt or Date or character to fix the date of beginning of the report
#' @param horodatefin A POSIXt or Date or character to fix the last date of the report
#' @param silent Should program be silent or display messages
#' @aliases choice_c.report_dc
#' @return An object of class \link{ref_dc-class} with slots filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @export
setMethod("choice_c",signature=signature("report_dc"),definition=function(object,dc,horodatedebut,horodatefin,silent=FALSE){
	  # report_dc<-r_dc;dc=5;horodatedebut="2000-01-01";horodatefin="2015-12-31";silent=TRUE
	  report_dc<-object
	  assign("report_dc",report_dc,envir=envir_stacomi)    
	  if (!silent) funout(gettext("Loading of the list for fishways and choice of the time step\n",domain="R-stacomiR"))
	  report_dc@dc<-charge(report_dc@dc)    
	  report_dc@dc<-choice_c(report_dc@dc,dc)
	  # assigns the parameter (horodatedebut) of the method to the object using choice_c method for ref_dc
	  report_dc@horodatedebut<-choice_c(object=report_dc@horodatedebut,
		  nomassign="report_dc_date_debut",
		  funoutlabel=gettext("Beginning date has been chosen\n",domain="R-stacomiR"),
		  horodate=horodatedebut, silent=silent)
	  report_dc@horodatefin<-choice_c(report_dc@horodatefin,
		  nomassign="report_dc_date_fin",
		  funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
		  horodate=horodatefin,silent=silent)
	  assign("report_dc",report_dc,envir=envir_stacomi)  
	  return(report_dc)
	})

#' Different plots for report_dc
#' 
#' \itemize{
#'      \item{plot.type=1}{A barplot of the operation time per month}
#' 		\item{plot.type=2}{Barchat giving the time per type of operation }
#' 		\item{plot.type=2}{Rectangle plots drawn along a line}
#'      \item{plot.type=4}{Plots per day drawn over the period to show the operation of a df, days in x, hours in y} 
#' 	}	
#' 
#' @note The program cuts periods which overlap between two month. 
#' The splitting of different periods into month is 
#' assigned to the \code{envir_stacomi} environment
#' @param x An object of class \link{report_dc-class}
#' @param y From the formals but missing
#' @param plot.type One of \code{barchart},\code{box}. Defaut to \code{barchart} showing
#'  a summary of the df operation per month, can also be \code{box}, 
#' a plot with adjacent rectangles.
#' @param silent Stops displaying the messages.
#' @param main The title of the graph, if NULL a default title will be plotted 
#' with the number of the DF
#' @return Nothing but prints the different plots
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.report_dc 
#' @export
setMethod("plot",signature(x = "report_dc", y = "ANY"), definition=
		function(x, 
			y,
			plot.type=1,
			silent=FALSE,
			main=NULL){ 
	  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	  #           PLOT OF TYPE BARCHART (plot.type=1 (true/false) or plot.type=2)
	  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	  #report_dc<-r_dc; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="1"
	  report_dc<-x
	  plot.type<-as.character(plot.type)# to pass also characters
	  if (!plot.type%in%c("1","2","3","4")) stop('plot.type must be 1,2,3 or 4')
	  if (nrow(report_dc@data)==0) 
		funout(gettext("No data for this counting device\n",domain="R-stacomiR"),arret=TRUE)
	  if (plot.type=="1"|plot.type=="2"){
		t_periodefonctdispositif_per=report_dc@data # on recupere le data.frame   
			tempsdebut<-t_periodefonctdispositif_per$per_date_debut
		tempsfin<-t_periodefonctdispositif_per$per_date_fin
		tempsdebut[tempsdebut<report_dc@horodatedebut@horodate]<-report_dc@horodatedebut@horodate
		tempsfin[tempsfin>report_dc@horodatefin@horodate]<-report_dc@horodatefin@horodate
		t_periodefonctdispositif_per=cbind(t_periodefonctdispositif_per,tempsdebut,tempsfin)
		seqmois=seq(from=tempsdebut[1],to=tempsfin[nrow(t_periodefonctdispositif_per)],by="month",tz = "GMT")
		seqmois=as.POSIXlt(round_date(seqmois,unit="month"))
		# adding one month at the end to get a complete coverage of the final month
		seqmois<-c(seqmois,
			seqmois[length(seqmois)]%m+%months(1))
		t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per[1,]
		############################
		#progress bar
		###########################
		progress_bar(
			title=gettext("time in hours",domain="R-stacomiR"),
			progress_text=gettext("Working of the counting device",domain="R-stacomiR"))
		progress_bar<-get("progress_bar",envir=envir_stacomi)
		# this function assigns
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
		if (is.null(main)) main<-gettextf("Operation of the counting device %s",report_dc@dc@dc_selectionne)
		# graphic
		#modification of the order
		
		t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$per_etat_fonctionnement, decreasing = TRUE),]
		g<- ggplot(t_periodefonctdispositif_per_mois,
				aes(x=mois,y=sumduree,fill=libelle))+
			facet_grid(annee~.)+
			ggtitle(main)+
			ylab(gettext("duration",domain="R-stacomiR"))+
			xlab(gettext("month",domain="R-stacomiR"))+						
			geom_bar(stat='identity')+
			scale_fill_manual(gettext("type_oper.",domain="R-stacomiR"),values = c("#FF6700","#EE1874", "#9E0142","#76BEBE","#999999"))+
			theme(
				plot.background = element_rect(fill ="white"),
				panel.background = element_rect(fill="white"),
				legend.background=element_rect(fill="white"),
				strip.background = element_rect(colour = "pink", fill = "brown"),
				strip.text = element_text(colour = "white"),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				text=element_text(colour="navyblue"),								
				line = element_line(colour = "black"),
				legend.key=element_rect(fill="white",colour="black"),
				axis.text=element_text(colour="black")
			)
		
		t_periodefonctdispositif_per_mois=t_periodefonctdispositif_per_mois[order(t_periodefonctdispositif_per_mois$per_etat_fonctionnement),]
		t_periodefonctdispositif_per_mois$per_etat_fonctionnement=as.factor(t_periodefonctdispositif_per_mois$per_etat_fonctionnement)
		g1<- ggplot(t_periodefonctdispositif_per_mois,aes(x=mois,y=sumduree))+
			facet_grid(annee~.)+
			ggtitle(main)+
			ylab(gettext("duration",domain="R-stacomiR"))+
			xlab(gettext("month",domain="R-stacomiR"))+									
			geom_bar(stat='identity',aes(fill=per_etat_fonctionnement))+
			scale_fill_manual(gettext("operation",domain="R-stacomiR"),values = c("#0F313A","#CEB99A"))+
			theme(
				plot.background = element_rect(fill ="white"),
				panel.background = element_rect(fill="white"),
				legend.background=element_rect(fill="white"),
				strip.background = element_rect(colour = "#C07C44", fill = "#A07C68"),
				strip.text = element_text(colour = "#41DADE"),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				text=element_text(colour="#482E21"),								
				line = element_line(colour = "black"),
				legend.key=element_rect(fill="white",colour="black"),
				axis.text=element_text(colour="black")
			)
		
		if (plot.type=="1")
		  print(g)
		if (plot.type=="2")
		  print(g1)		
		assign("periodeDC",t_periodefonctdispositif_per_mois,envir_stacomi)
		if (!silent) funout(gettext("Writing the table into envir_stacomi environment : write periodeDC=get('periodeDC',envir_stacomi)\n",domain="R-stacomiR"))
		# the progress bar has been assigned in envir_stacomi, we destroy it
		gtkWidgetDestroy(get("progress_bar",envir=envir_stacomi))
		#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		#           PLOT OF TYPE BOX (plot.type=3)
		#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	  } else if (plot.type=="3"){
		#report_dc<-r_dc; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="3"
		t_periodefonctdispositif_per=report_dc@data
		graphdate<-function(vectordate){
		  vectordate<-as.POSIXct(vectordate)
		  attributes(vectordate)<-NULL
		  unclass(vectordate)
		  return(vectordate)
		}
		time.sequence=seq.POSIXt(from=report_dc@horodatedebut@horodate,to=report_dc@horodatefin@horodate,by="day")
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
			ylab=gettext("Counting device",domain="R-stacomiR"),
			main=main,
			#bty="n",
			cex=0.8)
		r <- round(range(time.sequence), "day")
		graphics::axis(1, at=graphdate(seq(r[1], r[2], by="month")),labels=strftime(as.POSIXlt(seq(r[1], r[2], by="month")),format="%d-%b"))
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
				lwd = 1) 
		  }
		  if (sum(t_periodefonctdispositif_per$per_etat_fonctionnement==0)>0)   {                        
			rect(   xleft =graphdate(t_periodefonctdispositif_per$per_date_debut[t_periodefonctdispositif_per$per_etat_fonctionnement==0]), 
				ybottom=0.6,
				xright=graphdate(t_periodefonctdispositif_per$per_date_fin[t_periodefonctdispositif_per$per_etat_fonctionnement==0]),
				ytop=0.9, 
				col = mypalette[6],
				border = NA, 
				lwd = 1) 
		  }
		}
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
			legend= gettext("Normal oper.","Operational stop","Stop","Dysfunct.","Unknown",domain="R-stacomiR"),
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
		graphics::text(x=debut,y=0.95, label=gettext("Operation of the counting device",domain="R-stacomiR"), font=4, pos=4) 
		graphics::text(x=debut,y=0.45, label=gettext("Shutdowns types for this counting device",domain="R-stacomiR"), font=4,pos=4)
		
		#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
		#           PLOT OF TYPE BOX (plot.type=4)
		#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
	  } else if (plot.type=="4"){
		if (is.null(main)) main<-gettext("Working of the counting device",report_dc@dc@dc_selectionne)
		
		#report_dc<-r_dc; require(RGtk2); require(lubridate);require(ggplot2);main=NULL;silent=FALSE;plot.type="4"
		t_periodefonctdispositif_per=report_dc@data
		tpp<-split_per_day(t_periodefonctdispositif_per,horodatedebut="per_date_debut",horodatefin="per_date_fin")
		
		g<-ggplot(tpp)+
			geom_rect(aes(xmin=xmin,xmax=xmax,ymin=Hdeb,ymax=Hfin,fill=factor(per_tar_code)),alpha=0.8)+
			scale_fill_manual("type",values=c("1"="#377F07","2"="#DCE032","3"="#C42306","4"="#AAEDF6","5"="#191917"),
				labels = gettext("Normal oper.","Operational stop","Stop","Dysfunct.","Unknown",domain="R-stacomiR"))+
			#scale_colour_manual("type",values=c("1"="#40CA2C","2"="#C8B22D","3"="#AB3B26","4"="#B46BED","5"="#B8B8B8"),
			#		labels = gettext("Normal oper.","Operational stop","Stop","Dysfunct.","Unknown")+		)
			ggtitle(main)+			
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

#' Function to create a barchart (lattice) corresponding to the periods
#' @param h a handler
#' @param ... Additional parameters
#' @return assigns the data frame \code{periodeDC} allowing to build the lattice graph in the environment envir_stacomi
#' @keywords internal
#' @author cedric.briand
funbarchartDC = function(h,...) {
  report_dc<-get("report_dc",envir=envir_stacomi)  
  report_dc=charge(report_dc)
  report_dc=connect(report_dc)
  if( nrow(report_dc@data)==0 ) {
	funout(gettext("No data for this counting device\n"), arret=TRUE)
  }
  plot(report_dc,plot.type=1,silent=FALSE)
}



#' Handler for barchart for report_df class from the graphical interface
#' 
#' @note The program cuts periods which overlap between two month
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
funbarchart1DC = function(h,...) {
  report_dc<-get("report_dc",envir=envir_stacomi)  
  report_dc=charge(report_dc)	
  report_dc<-connect(report_dc)
  if( nrow(report_df@data)==0 ) {
	funout(gettext("No Shutdowns types for this pass\n"), arret=TRUE)
  }		
  plot(report_dc,plot.type=2,silent=FALSE)
}   

#' function used for some lattice graph 
#' 
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
funboxDC = function(h,...) {  
  report_dc<-get("report_dc",envir=envir_stacomi)  
  report_dc=charge(report_dc)
  report_dc=connect(report_dc)
  
  if( nrow(report_dc@data)==0 ) {
	funout(gettext("No data for this counting device\n"), arret=TRUE)
  }  
  plot(report_dc,plot.type=3)
}

#' Handler function to plot calendar like graph, internal use
#' @param h handler
#' @param ... additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
funchartDC = function(h,...) {
  report_dc<-get("report_dc",envir=envir_stacomi) 
  report_dc=charge(report_dc)
  report_dc<-connect(report_dc)
  
  if( nrow(report_dc@data)==0 ) {
	funout(gettext("No data for this counting device\n"), arret=TRUE)
  }
  plot(report_dc,plot.type=4,silent=FALSE)
  
}   

#' handler to print the command line
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
houtDC = function(h,...) {
  report_dc<-get("report_dc",envir=envir_stacomi) 
  report_dc<-charge(report_dc)
  report_dc<-connect(report_dc)
  #the charge method will check that all objects necessary to build the formula
  # are in envir_stacomi
  print(report_dc)
  
} 

#' Method to print the command line of the object
#' @param x An object of class report_dc
#' @param ... Additional parameters passed to print
#' @return NULL
#' @author cedric.briand
#' @aliases print.report_dc
#' @export
setMethod("print",signature=signature("report_dc"),definition=function(x,...){ 
	  
	  sortie1<-"report_dc=new('report_dc')\n"
	  sortie2<-stringr::str_c("report_dc=choice_c(report_dc,",
		  "dc=",x@dc@dc_selectionne,",",
		  "horodatedebut=",shQuote(as.character(x@horodatedebut@horodate)),",",
		  "horodatefin=",shQuote(as.character(x@horodatefin@horodate)),")")
	  # removing backslashes
	  funout(stringr::str_c(sortie1,sortie2),...)
	  return(invisible(NULL))
	})

#' FuntableDC create a table output for report_dc class
#' @param h Handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
funtableDC = function(h,...) {
  report_dc<-get("report_dc",envir=envir_stacomi) 
  report_dc=charge(report_dc)
  report_dc=connect(report_dc)
  
  if( nrow(report_dc@data)==0 ) {
	funout(gettext("No data for this counting device\n"), arret=TRUE)
  }
  summary(report_dc)
}

#' summary for report_dc, write csv and html output, and prints summary statistics
#' @param object An object of class \code{\link{report_dc-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @param ... Additional parameters (not used there)
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases summary.report_dc
#' @export
setMethod("summary",signature=signature(object="report_dc"),definition=function(object,silent=FALSE,...){
	  #report_dc<-r_dc
	  report_dc<-object
	  t_periodefonctdispositif_per<-report_dc@data # on recupere le data.frame
	  t_periodefonctdispositif_per$per_date_debut<-as.character(t_periodefonctdispositif_per$per_date_debut)
	  t_periodefonctdispositif_per$per_date_fin<-as.character(t_periodefonctdispositif_per$per_date_fin)
	  annee=paste(unique(strftime(as.POSIXlt(t_periodefonctdispositif_per$per_date_debut),"%Y")),collapse="+")
	  path1=file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("t_periodefonctdispositif_per_DC_",report_dc@dc@dc_selectionne,"_",annee,".csv",sep=""),fsep ="\\")
	  write.table(t_periodefonctdispositif_per,file=path1,row.names=FALSE,col.names=TRUE,sep=";")
	  funout(gettext("Writing of %s \n",path1,domain="R-stacomiR"))
	  path1html<-file.path(path.expand(get("datawd",envir=envir_stacomi)),paste("t_periodefonctdispositif_per_DC_",report_dc@dc@dc_selectionne,"_",annee,".html",sep=""),fsep ="\\")
	  funout(gettextf("Writing of %s this might take a while, please be patient ...\n",path1html))
	  funhtml(t_periodefonctdispositif_per,
		  caption=gettextf("t_periodefonctdispositif_per_DC_%s_%s",report_dc@dc@dc_selectionne,annee),
		  top=TRUE,
		  outfile=path1html,
		  clipboard=FALSE,
		  append=FALSE,
		  digits=2
	  )
	  print(gettextf("summary statistics for CD=%s",report_dc@dc@dc_selectionne),domain="R-stacomiR")
	  print(gettextf("dc_code=%s",report_dc@dc@data[report_dc@dc@data$dc==report_dc@dc@dc_selectionne,"dc_code"],domain="R-stacomiR"))
	  duree<-difftime(t_periodefonctdispositif_per$per_date_fin,t_periodefonctdispositif_per$per_date_debut,units="day")
	  sommes<-tapply(duree,t_periodefonctdispositif_per$per_tar_code,sum)
	  perc<-round(100*sommes/as.numeric(sum(duree)))
	  sommes<-round(sommes,2)
	  funout(gettext("Duration in days (operation type):",domain="R-stacomiR"))
	  funout(paste(gettext("Normal oper.","Operational stop","Stop","Dysfunct.","Unknown",gettext("Func.","Stop","Normal func.",domain="R-stacomiR")),
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
