
rm(list=ls(all=TRUE))
envir_stacomi <- new.env(parent = emptyenv())
assign("envir_stacomi",envir_stacomi,.GlobalEnv)

## lancement du programme proprement dit
#  if (exists("group")) {rm(group)}
#  if (exists("graphs")) {rm(graphs) }
#ci dessous en lancement manuel il est necessaire d'indiquer le chemin du repertoire de travail
# avant toute chose
#require(XML)
options(guiToolkit = "RGtk2")
filecsv="C:/Program Files/stacomi/calcmig.csv"
doc<-read.csv(filecsv,header=TRUE,sep=";")
tableau_config = t(doc) 

les_utilisateurs <- tableau_config[1]
#datawd=tableau_config["datawd",]
#assign("datawd",datawd,envir=envir_stacomi)
pgwd=tableau_config["pgwd",]
baseODBC=c(tableau_config["lienODBC",],tableau_config["uid",],tableau_config["pwd",])
setwd(pgwd)
# pour voir apparaitre toutes les requetes dans R
# assign("showmerequest",1,envir=envir_stacomi)
source ("C:/workspace/stacomir/pkg/stacomir/inst/config/libraries.R")



#source ("C:/Users/logrami/workspace/stacomir/pkg/stacomir/inst/config/libraries.R")


libraries()

source("utilities.R") # contient  funout (pour ecrire dans la console) et filechoose
source("fun_table_per_dis.R")  
#source("vector_to_listsql.R")
source("funstat_daily.R") 
source("fun_write_monthly.R")
                      

#listes de connection a la base de donnee (programmation S4)
source("create_generic.R") 
#cree les fonctions generiques et l'environnement envir_stacomi
source("ref_df.R")
source("ref_dc.R")
source("ref_taxa.R")
source("ref_stage.R")
source("ref_timestep.R")
source("ref_timestep_daily.R")
source("ref_par.R")
source("ref_parquan.R")
source("ref_parqual.R")
source("ref_year.R")
source("ref_coe.R") # coeff de conversion poids effectif
source("ref_list.R") #liste de donnees pour un choice
source("ref_choice.R")
source("ref_textbox.R")
source("ref_checkbox.R")
source("ref_env.R")
source("ref_period.R")
source("ref_horodate.R")
source("report_dc.R")
source("report_df.R")
source("report_ope.R")
source("report_mig.R")
source("report_mig_mult.R")
source("report_env.R")
source("report_mig_env.R")
source("report_sample_char.R")
require(xtable)
source("report_mig_char.R")
source("report_mig_interannual.R")
source("report_mig_char.R")
source("report_annual.R")
source("report_silver_eel.R")
source("report_ge_weight.R")
source("report_species.R")
source("report_sea_age.R")
source("setAs.R")

# functions
source("fun_report_mig_char.R")
source("fungraph_glasseel.R")
source("fungraph.R")
source("funstat.R")
source("funtable.R")
source("interface_report_mig_interannual.R")
source("interface_report_sample_char.R")
source("interface_report_ge_weight.R")
source("interface_report_env.R")
source("interface_report_mig.R")
source("interface_report_mig_env.R")
source("interface_report_mig_char.R")
source("interface_report_dc.R")
source("interface_report_df.R")
source("interface_report_mig_mult.R")
source("interface_report_silver_eel.R")
source("interface_report_annual.R")
source("interface_report_sea_age.R")
source("interface_report_species.R")
source("stacomi.R")
# interface_report_species dans report_species
setwd("C:/workspace/stacomir/pkg/stacomir")
stacomi(gr_interface=TRUE,login_window=TRUE,database_expected=TRUE)


