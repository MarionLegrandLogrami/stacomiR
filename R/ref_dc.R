#' Class "ref_dc"
#' 
#' Description of a control device.
#' 
#' @include create_generic.R
#' @slot dc_selectionne Object of class \code{"integer"}, The selected device
#' @slot ouvrage Object of class \code{"integer"}, the attached dam
#' @slot station Object of class \code{"character"}, the attached migration monitoring station, this is necessary to join the
#' table of escapements calculated at the station level.
#' @slot data Object of class \code{"data.frame"} data pertaining to the control device
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_dc", dc_selectionne=integer(), ouvrage=integer(),
#' data=data.frame())}.  
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
#' @family referential objects
setClass(Class="ref_dc",representation=
		representation(dc_selectionne="integer",ouvrage="integer",station="character",data="data.frame") ,
	prototype=prototype(dc_selectionne=integer(),ouvrage=integer(),station=character(),data=data.frame())
)



setValidity("ref_dc",method=function(object){
	  if (length(object@dc_selectionne)!=0){		
		if (nrow(object@data)>0) {
		  concord<-object@dc_selectionne%in%object@data$dc					
		  if (any(!concord)){
			return(paste("No data for DC",object@dc_selectionne[!concord]))
			
		  } else {
			return(TRUE)
		  }
		} else {
		  return("You tried to set a value for dc_selectionne without initializing the data slot")
		}
	  }  else return(TRUE)
	  
	}   
)


#' Method to load the counting devices of the control station
#' @param object An object of class \link{ref_dc-class}
#' @return Object of class ref_dc
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
setMethod("charge",signature=signature("ref_dc"),definition=function(object) {
	  requete=new("RequeteODBC")
	  requete@baseODBC<-get("baseODBC",envir=envir_stacomi)
	  requete@sql=  paste("select dis_identifiant as DC,",
		  " dis_date_creation,",
		  " dis_date_suppression,",
		  " dif_dis_identifiant as DF,",
		  " dis_commentaires,",
		  " dif_ouv_identifiant,",
		  " ouv_libelle,",
		  " dif_code as DF_code,",
		  " dic_code as DC_code,",
		  " dif_localisation,",
		  " dif_orientation,",
		  " tdf_libelle as type_DF,",
		  " tdc_libelle as type_DC,",
		  "sta_code",
		  " FROM ",get("sch",envir=envir_stacomi),"tg_dispositif_dis",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_dispositifcomptage_dic ON dic_dis_identifiant =dis_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_dispositiffranchissement_dif ON dif_dis_identifiant=dic_dif_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"tj_dfesttype_dft ON dif_dis_identifiant=dft_df_identifiant",
		  " JOIN ",get("sch",envir=envir_stacomi),"t_ouvrage_ouv on dif_ouv_identifiant=ouv_identifiant", 
		  " JOIN ",get("sch",envir=envir_stacomi),"t_station_sta on ouv_sta_code=sta_code", 
		  " JOIN ref.tr_typedf_tdf ON tdf_code=dft_tdf_code",
		  " JOIN ref.tr_typedc_tdc ON dic_tdc_code=tdc_code",
		  " WHERE  dft_rang=1",
		  " ORDER BY dis_identifiant;",sep="")
	  requete<-stacomirtools::connect(requete) 
	  #funout(gettext("The query to load counting devices is done \n",domain="R-stacomiR"))
	  object@data<-requete@query
	  return(object)
	})



#' Graphical method to choose a fishway through the interface
#' 
#' @note   The choice method has for arguments a report (report) object
#'  (e.g) is called from a report report(e.g report_sample_char).
#'   By default,  the value of the objectreport is null.
#'   When it is not   the method calls daughter widgets (e.g. the dc widget will call species) 
#' and fills it with the method \link{charge_with_filter,ref_taxa-method} 
#' @param object An object of class \link{ref_dc-class}
#' @param objectreport the objectreport from which this frame was called
#' @param is.enabled a boolean indicating whether the current frame will be enabled. Selecting a "parent"
#' frame may cause some frame to be disabled. When created the frame is enabled.
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples \dontrun{ 
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("ref_dc")
#' object<-charge(object)
#' objectreport=new("report_mig")
#' choice(object=object,objectreport=objectreport)
#'}
#' @keywords internal
setMethod("choice",signature=signature("ref_dc"),definition=function(object,objectreport=NULL,is.enabled=TRUE) {
	  if (nrow(object@data) > 0){
		h_report_dc=function(h,...){
		  object@dc_selectionne<-svalue(choice)
		  object@ouvrage= object@data$dif_ouv_identifiant[object@data$dc%in%object@dc_selectionne]
		  object@station=object@data$sta_code[object@data$dc%in%object@dc_selectionne]
		  assign("ref_dc",object,envir_stacomi)
		  funout(gettext("Counting device selected\n",domain="R-stacomiR"))
		  # si il existe un object fils; supprimer
		  # referentiel fils, celui charge par la methode charge_with_filter
		  # ici comme on fait appel e un autre object il faut appeller le conteneur qui contient l'object
		  if (!is.null(objectreport)) {
			# ci dessous pas d'appel de charge_with_filter pour les report_species (tous les taxa)
			# pas non plus d'appel pour les report_silver_eel dont les slots taxa, stage ,et par sont fixes
			if("ref_taxa"%in%as.character(getSlots(class(objectreport)))&class(objectreport)!="report_silver_eel"&class(objectreport)!="report_sea_age"){
			  objectreport@taxa<<-charge_with_filter(object=objectreport@taxa,dc_selectionne=get("ref_dc",object,envir_stacomi)@dc_selectionne)
			  if (exists("frame_tax",envir=envir_stacomi)) {
				frame_tax<-get("frame_tax",envir=envir_stacomi)
				delete(group,frame_tax)
			  }
			  if (exists("frame_std",envir=envir_stacomi)) {
				frame_std<-get("frame_std",envir=envir_stacomi)
				delete(group,frame_std)
			  }
			  if (exists("frame_par",envir=envir_stacomi)) {
				frame_par<-get("frame_par",envir=envir_stacomi)
				delete(group,frame_par)
			  }
			  if (exists("frame_parquan",envir=envir_stacomi)) {
				frame_parquan<-get("frame_parquan",envir=envir_stacomi)
				delete(group,frame_parquan)
			  }
			  if (exists("frame_parqual",envir=envir_stacomi)) {
				frame_parqual<-get("frame_parqual",envir=envir_stacomi)
				delete(group,frame_parqual)
			  }
			  choice(objectreport@taxa,objectreport,is.enabled=TRUE)
			  funout(gettext("Select taxa for this counting device (for all periods)\n",domain="R-stacomiR"))
			}
		  }
		  #dispose(winst)
		} 
		# Handler d'affichage du tableau
		h_report_dci=function(h,...){
		  w=gwindow(gettext("Counting devices data",domain="R-stacomiR",width=400))
		  wg=ggroup(horizontal=FALSE,container=w)
		  tab=gtable(object@data[,c(1,4,7,8,11,12)],chosencol=1,multiple=FALSE,expand=TRUE, container=wg)
		  bg<-ggroup(container=wg)
		  addSpring(bg)
		  gbutton(gettext("close",domain="R-stacomiR"), container=bg, handler = function(h,...) dispose(w))
		}
		group<-get("group",envir=envir_stacomi)
		frame_DC<-gframe(gettext("Counting devices choice",domain="R-stacomiR"))
		assign("frame_DC",frame_DC,envir_stacomi)
		add(group,frame_DC)
		DC_identifiant=object@data$dc
		choice=gdroplist(DC_identifiant,container=frame_DC,handler=h_report_dc)
		gbutton(gettext("Table",domain="R-stacomiR"), container=frame_DC,handler=h_report_dci) 
		enabled(frame_DC)<-is.enabled
		gbutton("OK", container=frame_DC,handler=h_report_dc)
	  } else {
		funout(gettext("Error : no counting device in the database (the query returns 0 entry)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  return(object)
	})
# pour test #choice(object)



#' choicemult, selection method for ref_dc allowing to select several DC
#' 
#' @note   The choice method has for arguments a report (report) object
#'  (e.g) is called from a report report(e.g report_sample_char).
#'   By default,  the value of the objectreport is null.
#'   When it is not   the method calls daughter widgets (e.g. the dc widget will call species) 
#' and fills it with the method \link{charge_with_filter,ref_taxa-method}
#' @param object An object of class ref_dc
#' @param objectreport A report object
#' @param is.enabled A boolean indicating if the widget can be seleted at launch
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples 
#' \dontrun{ 
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("ref_dc")
#' object<-charge(object)
#' objectreport=new("report_mig_mult")
#' choicemult(object=object,objectreport=objectreport)
#'}
setMethod("choicemult",signature=signature("ref_dc"),definition=function(object,objectreport=NULL,is.enabled=TRUE) {
	  
	  if (nrow(object@data) > 0){
		h_report_dc=function(h,...){
		  #browser()
		  dc_selectionne<-tbdestdc[,][tbdestdc[,]!=""]
		  object@dc_selectionne<-as.integer(dc_selectionne)
		  if (length(dc_selectionne)>0){
			object@ouvrage= object@data$dif_ouv_identifiant[object@data$dc%in%object@dc_selectionne]
			object@station= as.character(object@data$sta_code[object@data$dc%in%object@dc_selectionne])
			assign("ref_dc",object,envir_stacomi)
			funout(gettext("Counting device selected\n",domain="R-stacomiR"))
			# si il existe un object fils; supprimer
			# referentiel fils, celui charge par la methode charge_with_filter
			# ici comme on fait appel e un autre object il faut appeller le conteneur qui contient l'object
			if (!is.null(objectreport)) {
			  # ci dessous pas d'appel de charge_with_filter pour les report_species (tous les taxa)
			  if("ref_taxa"%in%as.character(getSlots(class(objectreport)))){
				
				
				objectreport@dc<-object
				objectreport@taxa<-charge_with_filter(object=objectreport@taxa,dc_selectionne=get("ref_dc",envir_stacomi)@dc_selectionne)
				# the name was created by the interface
				# as I can't get the name from within the function (deparse(substitute(objectreport does not return
				# "report_mig_mult"
				assign(get("objectreport",envir=envir_stacomi),objectreport,envir=envir_stacomi)
				# suppresses all tab larger than (dc)
				
				currenttab<-svalue(notebook)
				if (length(notebook)>currenttab){
				  for (i in length(notebook):(currenttab+1)){
					svalue(notebook) <- i							
					dispose(notebook) ## dispose current tab
				  }}
				choicemult(objectreport@taxa,objectreport,is.enabled=TRUE)
				#funout(gettext("Select taxa for this counting device (for all periods)\n",domain="R-stacomiR"))
			  }
			}
			# changing tab of notebook to next tab
			if (svalue(notebook)<length(notebook)){
			  svalue(notebook)<-svalue(notebook)+1	
			}
			#dispose(winst)
		  } else {
			funout(gettext("Counting device not selected\n",domain="R-stacomiR"))
			
		  }
		} 
		# Handler d'affichage du tableau
		# below the widget structure [=> within (=> type
		# group(ggroup)[nb(notebook)[groupdc(ggroup&tab)[[frameDCsource(gframe)[tbsourcedc(gtable)],frameDCdest(gframe)[tbdcdest(gtable)]],OKbutton]]
		notebook<-get("notebook",envir=envir_stacomi)
		DC=object@data[,c("dc","dis_commentaires","type_dc")]
		groupdc<-ggroup(container=notebook, label="dc")   ## "add" called by constructor this is a tab of the notebook
		assign("groupdc",groupdc,envir=envir_stacomi)
		frameDCsource<-gframe(gettext("Counting devices choice",domain="R-stacomiR"),container=groupdc)
		size(frameDCsource)<-c(250,300)
		tbsourcedc  = gtable(DC,container=frameDCsource,expand = TRUE, fill = TRUE)
		
		frameDCdest<-gframe(gettext("drop here",domain="R-stacomiR"),container=groupdc)
		size(frameDCdest)<-c(60,300)
		#addSpring(groupdc)
		# need for a fixed size data.frame otherwise errors when adding new lines
		xx<-data.frame(choice=rep("",12))
		xx$choice<-as.character(xx$choice)
		tbdestdc=gtable(xx,container=frameDCdest,expand=TRUE, fill=TRUE)
		adddropsource(tbsourcedc)
		adddroptarget(tbdestdc)				
		adddropmotion(tbdestdc,handler=function(h,...) {
			  valeurs<-tbdestdc[,]
			  valeurs<-valeurs[valeurs!=""]
			  if (!svalue(tbsourcedc)%in%valeurs){
				tbdestdc[length(valeurs)+1,1]<-svalue(tbsourcedc)
			  }
			})
		addHandlerDoubleclick(tbsourcedc,handler=function(h,...) {
			  valeurs<-tbdestdc[,]
			  valeurs<-valeurs[valeurs!=""]
			  if (!svalue(tbsourcedc)%in%valeurs){
				tbdestdc[length(valeurs)+1,1]<-svalue(h$obj)
			  }
			})
		adddropsource(tbdestdc)
		adddroptarget(tbsourcedc)
		removedc<-function(){
		  valeurs<-tbdestdc[,]
		  valeurs<-valeurs[valeurs!=""]
		  valeurs<-valeurs[-match(svalue(tbdestdc),valeurs)]
		  tbdestdc[,]<-c(valeurs,rep("",8-length(valeurs)))
		}
		adddropmotion(tbsourcedc,handler=function(h,...) {
			  removedc()
			})
		addHandlerDoubleclick(tbdestdc,handler=function(h,...) {
			  removedc()
			})
		gbutton("OK", container = groupdc, handler = h_report_dc)
	  } else {
		funout(gettext("Error : no counting device in the database (the query returns 0 entry)\n",domain="R-stacomiR"),arret=TRUE)
	  }
	  #return(object)
	})


#' Command line interface to select a counting device
#' 
#' the choice_c method is intendedto have the same behaviour as choice (which creates a
#' widget in the graphical interface) but from the command line.  The parameters for dc are transformed to integer as the ref_dc only 
#' takes integer in the dc slots. The method also loads the stations and ouvrages (dams) associated with the counting device (dc).
#' The values passed to the choice_c method are then checked with the setValidty method.
#' Finally, if an objectreport is passed as a parameter, the method will do a charge_with_filter to select only the taxa present in the counting devices
#' @param object an object of class ref_dc
#' @param dc a character vector of dc chosen
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @examples
#' \dontrun{
#' win=gwindow()
#' group=ggroup(container=win,horizontal=FALSE)
#' object=new("ref_dc")
#' object<-charge(object)
#' objectreport=new("report_mig_mult")
#' choice_c(object=object,objectreport=objectreport,dc=1)
#' }
setMethod("choice_c",signature=signature("ref_dc"),definition=function(object,dc) {
	  if (class(dc)=="numeric") {
		dc<-as.integer(dc) 
	  }else if	(class(dc)=="character"){
		dc=as.integer(as.numeric(dc))
	  }
	  if (any(is.na(dc))) stop ("NA values dc")
	  
	  
	  object@dc_selectionne<-dc
	  validObject(object) 		
# the method validObject verifies that the dc is in the data slot of ref_dc			
	  
	  object@station<- as.character(object@data$sta_code[ object@data$dc%in% object@dc_selectionne])
	  object@ouvrage<-object@data$dif_ouv_identifiant[object@data$dc%in%object@dc_selectionne]
	  assign("ref_dc",object,envir=envir_stacomi)
	  return(object)
	})