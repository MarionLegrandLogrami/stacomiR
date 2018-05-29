#' function to call and load the libraries used in stacomi
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
libraries=function() {
necessary = c( 'RODBC','ggplot2','gWidgets','gWidgetsRGtk2',
		'lattice','RColorBrewer','xtable','scales','reshape2','grid','stringr','intervals','sqldf','RPostgreSQL')  # 'tcltk2','XML', 'Hmisc''svMisc''proto''R2HTML'
if(!all(necessary %in% installed.packages()[, 'Package']))
	install.packages(necessary[!necessary %in% installed.packages()[, 'Package']], dependencies = TRUE)
#if (!'XML'%in%installed.packages()[, 'Package']) install.packages("XML", repos = "http://www.omegahat.org/R")
#require('tcltk2')
require('ggplot2')
require('gWidgets') # already called by depends
require('RODBC')
##require('Hmisc')
options(guiToolkit = "RGtk2")
#require('lattice')
#require('RColorBrewer')
require('stacomirtools')
#require('RODBC')
##require('R2HTML')
##require('proto') 
#require('xtable')
##require('Hmisc')
##if(require('XML')) library('XML') 
##require('svMisc')   
#require('stringr')
#require('grid')
#require('reshape2')
#require('scales')
#require('intervals')
##require('Rcmdr') not done there as is causes the interface to load
#require('sqldf')
#require('RPostgreSQL')

}
#require('sma')# function is.odd




