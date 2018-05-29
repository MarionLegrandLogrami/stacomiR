#' Counts of number per taxa/stages
#' 
#' This class is used to make the assessment of all species, and their number. It is intended
#' as a simple way to check what fishes are present (taxa + development stage). Unlike the report_annual,
#' it is not restricted to chosen taxa or stages but gives counts for all species present. The taxa is reported unless 
#' a taxa has several case, in which case the different stages for the taxa will be reported
#' Using the split arguments
#' the calc method of the class will count numbers, subsamples are not accounted for in the Overview.
#' The split argument currently takes values year or month. The class is intended to be used over long periods
#' e.g years. The plot method writes either an histogram or a pie chart of number per
#' year/week/month.
#' @slot dc an object of class \link{ref_dc-class} 
#' @slot anneedebut Object of class \code{\link{ref_year-class}}
#' @slot anneefin Object of class \code{\link{ref_year-class}}
#' @slot data \code{data.frame}
#' @slot calcdata \code{data.frame} with data processed by the calc method
#' @slot split Object of class \code{\link{ref_list-class}} ref_list referential class choose within a list
#' @include ref_dc.R
#' @include ref_list.R
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @family report Objects
#' @aliases report_species 
#' @example inst/examples/report_species-example.R
#' @keywords classes
#' @export 
setClass(Class="report_species",
	representation=
		representation(dc="ref_dc",
			anneedebut="ref_year",
			anneefin="ref_year",
			data="data.frame",
			calcdata="data.frame",
			split="ref_list"),
	prototype=prototype(dc=new("ref_dc"),
		anneedebut=new("ref_year"),
		anneefin=new("ref_year"),
		data=data.frame(),
		calcdata=data.frame(),
		split=new("ref_list")
	)
)


#' connect method for report_species
#' @param object An object of class report_species
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class report_species with data slot filled
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases connect.report_species
#' @export
setMethod("connect",signature=signature("report_species"),definition=function(object,silent=FALSE) {
	  bilesp<-object 
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  anneedebut=	bilesp@anneedebut@annee_selectionnee
	  anneefin=bilesp@anneefin@annee_selectionnee			
	  requete@sql= paste("SELECT lot_identifiant, ope_date_debut, ope_date_fin,",
		  " lot_effectif, lot_tax_code, lot_std_code, tax_nom_latin, std_libelle,",
		  " date_part('year', ope_date_debut) as annee,",
		  " date_part('month',ope_date_debut) as mois,",
		  " date_part('week',ope_date_debut) as semaine",
		  " FROM ",get("sch",envir=envir_stacomi),"t_operation_ope",
		  " INNER JOIN ",get("sch",envir=envir_stacomi),"t_lot_lot ON ope_identifiant=lot_ope_identifiant",
		  " INNER JOIN ref.tr_taxon_tax on tax_code=lot_tax_code",
		  " INNER JOIN ref.tr_stadedeveloppement_std on std_code=lot_std_code",
		  " WHERE extract(year from ope_date_debut)>=",anneedebut,
		  " AND extract(year from ope_date_debut)<=", anneefin, 
		  " AND ope_dic_identifiant in",
		  vector_to_listsql(bilesp@dc@dc_selectionne),
		  " AND lot_lot_identifiant IS NULL",
		  " AND lot_effectif IS NOT NULL",
		  sep="")
	  requete<-stacomirtools::connect(requete)	
	  if (requete@etat!="success") funout(gettext("Query failed for the view vue_ope_lot_car \n",domain="R-stacomiR"),arret=TRUE)
	  bilesp@data<-requete@query
	  if (!silent) funout(gettext("data loaded from the database for report_species"))
	  assign("bilesp",bilesp,envir=envir_stacomi)
	  return(bilesp)
	})


#' command line interface for \link{report_species-class}
#' @param object An object of class \link{report_species-class}
#' @param dc A numeric or integer, the code of the dc, coerced to integer,see \link{choice_c,ref_dc-method}
#' @param anneedebut The starting the first year, passed as character or integer
#' @param anneefin the finishing year
#' @param split one of c("none","week","month","year")
#' @param silent Boolean, if TRUE, information messages are not displayed
#' @return An object of class \link{report_species-class}
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases choice_c.report_species
#' @export
setMethod("choice_c",signature=signature("report_species"),definition=function(object,
		dc,
		split="none",		
		anneedebut,
		anneefin,
		silent=FALSE){
	  # code for debug using example
	  #dc=c(5,6);anneedebut="1996";anneefin="2016";split="none";silent=TRUE
	  bilesp<-object
	  bilesp@dc=charge(bilesp@dc)
	  # loads and verifies the dc
	  # this will set dc_selectionne slot
	  bilesp@dc<-choice_c(object=bilesp@dc,dc)
	  # only taxa present in the report_mig are used
	  bilesp@split=charge(object=bilesp@split,listechoice=c("none","week","month","year"),label=gettext("choice of number in sample (one, several,all)",domain="R-stacomiR"))# choix de la categorie d'effectif)
	  bilesp@split<-choice_c(bilesp@split,selectedvalue=split)
	  # by default choice_c returns reflist but usefull to mimic gr.interface
	  assign("refliste",bilesp@split,envir_stacomi)
	  bilesp@anneedebut<-charge(object=bilesp@anneedebut,
		  objectreport="report_species")
	  bilesp@anneedebut<-choice_c(object=bilesp@anneedebut,
		  nomassign="start_year",
		  annee=anneedebut, 
		  silent=silent)
	  bilesp@anneefin@data<-bilesp@anneedebut@data
	  bilesp@anneefin<-choice_c(object=bilesp@anneefin,
		  nomassign="end_year",
		  annee=anneefin, 
		  silent=silent)
	  assign("bilesp",bilesp,envir=envir_stacomi)
	  return(bilesp)
	})

#' handler for calculation 
#' 
#' internal use
#' @param h a handler
#' @param ... Additional parameters
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
hbilespcalc=function(h,...){
  if (exists("bilesp",envir_stacomi)) {
	bilesp<-get("bilesp",envir_stacomi)
  } else {      
	funout(gettext("No data named bilesp in envir_stacomi",domain="R-stacomiR"),arret=TRUE)
  }	
  bilesp<-charge(bilesp)
  bilesp<-connect(bilesp)
  bilesp<-calcule(bilesp)
}


#' charge method for report_species
#' 
#' Verifies the content of objects when the graphical interface is used, it is not necessary
#' to call the charge method if the choice_c method has been used
#' @param object An object of class \link{report_species-class}
#' @param silent Stops displaying the messages. 
#' @return report_species with slots filled by user choice
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases charge.report_species
#' @export
#' @keywords internal
setMethod("charge",signature=signature("report_species"),definition=function(object, silent=FALSE){
	  if (!silent) funout(gettext("Checking objects and launching query\n",domain="R-stacomiR"))
	  bilesp<-object
	  if (exists("ref_dc",envir_stacomi)) {
		bilesp@dc<-get("ref_dc",envir_stacomi)
	  } else {
		funout(gettext("You need to choose a counting device, clic on validate\n",domain="R-stacomiR"),arret=TRUE)	
	  }
	  if (exists("start_year",envir_stacomi)) {
		bilesp@anneedebut<-get("start_year",envir_stacomi)
	  } else {
		funout(gettext("You need to choose the starting year\n",domain="R-stacomiR"),arret=TRUE)
	  }  	
	  if (exists("end_year",envir_stacomi)) {
		bilesp@anneefin<-get("end_year",envir_stacomi)
	  } else {
		funout(gettext("You need to choose the ending year\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  
	  if (exists("refliste",envir_stacomi)) {      
		bilesp@split<-get("refliste",envir_stacomi)      
	  } else {      
		funout(gettext("You need to choose a size class\n",domain="R-stacomiR"), arret=TRUE)             
	  } 
	  assign("bilesp",bilesp,envir_stacomi)
	  if (!silent) funout(gettext("A report_species object was stored into envir_stacomi environment : write bilesp=get('bilesp',envir_stacomi)",domain="R-stacomiR"))
	  return(bilesp)
	})


#' handler for plot internal use
#' @param h Handler
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
hplotbilesp = function(h) {
  bilesp<-get("bilesp",envir_stacomi)
  plot(bilesp,plot.type=h$action)
}

#' calcule method for report_species
#' 
#' ' the number will be split according to the split argument passed to the class, e.g.
#' per year or month or week. Data from different DC will be grouped. Counts are given per taxa,
#' unless there are several stages, in which case the counts correspond to taxa + stage.
#' @param object An object of class \code{\link{report_species-class}}
#' @param silent Boolean, if TRUE, information messages are not displayed, only warnings and errors
#' @return  with calcdata slot filled.
#' @aliases calcule.report_species
#' @export
setMethod("calcule",signature=signature("report_species"),definition=function(object,silent=FALSE){ 			
	  bilesp<-object
	  DC=as.numeric(bilesp@dc@dc_selectionne)	
	  # update of refliste which does not need calcul button pushed
	  tableEspeces=bilesp@data
	  if (nrow(tableEspeces)==0) funout(gettext("No fish in the database for this period\n",domain="R-stacomiR"),arret=TRUE)
	  tableEspeces$taxa_stage=paste(tableEspeces$tax_nom_latin,tableEspeces$std_libelle,sep="_")
	  # only keeping taxa stage for species with several stages
	  nbstage=tapply(tableEspeces$tax_nom_latin,tableEspeces$taxa_stage,function(X)(length(unique(X))))
	  # we currently have taxa+stage, below this is replaces with taxa unless there are more than one stage per species
	  if (length(nbstage[nbstage>1])>0){
		les_multiples=names(nbstage[nbstage>1])
		tableEspeces[!tableEspeces$taxa_stage%in%les_multiples,"taxa_stage"]<-tableEspeces$tax_nom_latin[!tableEspeces$taxa_stage%in%les_multiples]
	  } else tableEspeces$taxa_stage<-tableEspeces$tax_nom_latin
	  if (min(tableEspeces$lot_effectif)<0) {
		if (!silent) funout(gettext("Some negative counts are transformed into positive ones\n",domain="R-stacomiR"))
		tableEspeces$lot_effectif=abs(tableEspeces$lot_effectif)
	  }
	  sumEspeces=switch(bilesp@split@selectedvalue,
		  "year"=as.data.frame(xtabs(lot_effectif~taxa_stage+annee,data=tableEspeces)),
		  "month"=as.data.frame(xtabs(lot_effectif~taxa_stage+mois,data=tableEspeces)),
		  "week"=as.data.frame(xtabs(lot_effectif~taxa_stage+semaine,data=tableEspeces)),
		  "none"=as.data.frame(xtabs(lot_effectif~taxa_stage,data=tableEspeces)))
	  colnames(sumEspeces)[colnames(sumEspeces)=="Freq"]<-"Effectif" # pas forcement le m nb de colonnes
	  if (bilesp@split@selectedvalue!="none"){			
		colnames(sumEspeces)[2]<-bilesp@split@selectedvalue
	  }
	  bilesp@calcdata<-sumEspeces			
	  assign("bilesp",bilesp,envir_stacomi)			
	  return(bilesp)
	})

#' Plot method for report_species
#' 
#' @param x An object of class \link{report_species-class}
#' @param plot.type Default pie
#' #' \itemize{
#' 		\item{plot.type="pie": A pie}' 	
#' 		\item{plot.type="barchart" : A barchart}
#' }
#' @param color Default NULL, a vector of colors of length corresponding to the number of taxa-stage
#' different values, use unique(bilesp@calcdata$taxa_stage) to get that number. The color applies to both
#' pie and barchart plots
#' @param silent Stops displaying the messages.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases plot.reportreport_species
#' @export
setMethod("plot",signature(x = "report_species", y = "missing"),definition=function(x, 
		plot.type="pie",
		color=NULL,
		silent=FALSE)
	{
	  bilesp<-x
	  if (nrow(bilesp@calcdata)==0) stop("No data in the calcdata slot, did you forget to run calculations ?")
	  nb=length(unique(bilesp@calcdata$taxa_stage))
	  g<-ggplot(bilesp@calcdata)
	  g<-g+geom_col(aes(x="",y=Effectif,fill=taxa_stage)) + 
		  ggtitle(paste("report Especes, DC",
				  str_c(bilesp@dc@dc_selectionne,collapse="+"),
				  bilesp@anneedebut@annee_selectionnee,"=>",
				  bilesp@anneefin@annee_selectionnee))+
		  xlab("")+
		  ylab(gettext("Number",domain="R-stacomiR"))
	  #theme(axis.line.x=element_line("none"))+theme(axis.title.x= element_text("none"))
	  if (bilesp@split@selectedvalue!="none"){
		facet<-switch(bilesp@split@selectedvalue,
			"year"=as.formula(~year),
			"month"=as.formula(~month),
			"week"=as.formula(~week))
		g<-g+facet_wrap(facet,scales="fixed")
	  }
	  if (is.null(color)){
		if (nb<=8) {
		  g<-g+scale_fill_brewer(palette="Accent",name=gettext("Taxa-stage",domain="R-stacomiR"))   
		} else if (nb<=12){
		  p<-g+scale_fill_brewer(palette="Set3",name=gettext("Taxa-stage",domain="R-stacomiR"))   
		}else{
		  g<-g+scale_fill_manual(values=grDevices::rainbow(nb),name=gettext("Taxa-stage",domain="R-stacomiR"))
		}
	  } else { #color is not null
		if (length(color)!=nb) stop(gettextf("The vector of color should be of length %s",domain="R-stacomiR",nb))
		g<-g+scale_fill_manual(values=color,gettext("Taxa-stage",domain="R-stacomiR"))
	  }
	  if (plot.type=="barplot"){				
		print(g)
		assign("g",g,envir=envir_stacomi)
	  } else if(plot.type=="pie"){
		g<-g+ coord_polar(theta="y",start=pi) +xlab('') +ylab('')
		print(g) 
		assign("g",g,envir=envir_stacomi)
	  } else {
		funout(gettext("plot.type should be one of barplot or pie",domain="R-stacomiR"),arret=TRUE)
	  }	
	  if (! silent) funout(gettext("the object g has been assigned to envir_stacomi",domain="R-stacomiR"))
	  
	  return(invisible(NULL))		
	}
)


#' handler for summary report_species, internal use
#' @param h a handler
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords internal
hsummarybilesp=function(h) {
  if (exists("bilesp",envir_stacomi)) {
	bilesp<-get("bilesp",envir_stacomi)
  } else {      
	funout(gettext("No data named bilesp in envir_stacomi",domain="R-stacomiR"),arret=TRUE)
  }
}

#' summary for report_species 
#'  generate csv and html output in the user data directory
#' @param object An object of class \code{\link{report_species-class}}
#' @param silent Should the program stay silent or display messages, default FALSE
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @aliases summary.report_species
#' @export
setMethod("summary",signature=signature(object="report_species"),definition=function(object,silent=FALSE){
	  bilesp<-object
	  if (nrow(bilesp@calcdata)==0) stop("No data in the calcdata slot, did you forget to run calculations ?")				
	  loc<-str_c(str_c(bilesp@dc@dc_selectionne,collapse="+"),
		  bilesp@anneedebut@annee_selectionnee,
		  bilesp@anneefin@annee_selectionnee,sep="_")
	  
	  path=file.path(normalizePath(path.expand(get("datawd",envir=envir_stacomi))),
		  paste("tableEspece",loc,".csv",sep=""),fsep ="\\")
	  write.table(bilesp@calcdata,path,row.names=TRUE,col.names=TRUE,sep=";",append=FALSE)
	  if (!silent){
		funout(gettextf("writing of %s \n",path))
		funout(gettextf("attention, negative numbers were transformed into positive numbers"))
	  }
	})

