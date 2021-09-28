
# initialize_Matisse --------------------------------------------------------------------------------------------------------------------------------------
#' @title  initialize_Matisse
#' @description This is the first function to launch Matisse. Please note that it is not best-practice compliant, as it loads the required libraries
#' directly instead of through proper library management. This requires some work that hasn't yet been scheduled.
#'
#' @return TRUE
#'
#' @examples
#' initialize_Matisse()
initialize_Matisse <- function(){
  library(tidyverse)
  library(readxl)
  library(car)
  library(FinancialMath)
  library(plyr)
  library(pracma)
  library(reshape2)
  library(FactoMineR)
  library(nnet)
  library(reldist)
  library(matrixStats)
  library(quadprog)
  return(TRUE)
}


# initialize_MatisseParams --------------------------------------------------------------------------------------------------------------------------------
#' initialize_MatisseParams
#'
#' @param MatisseParams$scenario The MatisseParams$scenario from 3ME : values AMS, AME
#' @param MatisseParams$horizon The year for the end of the simulation. Standard values : 2025, 2030, 2035
#' @param MatisseParams$redistribution The type of retrocession of the carbon tax.
#' @param classement The way to select households to have a technical change for houses (Optimiste, Median, Pessimiste and other values)
#' @param class_force_bascule When doing technical change, this forces all new houses of class X or better to migrate out of fossil energies
#' @param year_new_bascule The year after which all new buildings are out of fossil energies
#' @param bascule_min_jump The minimum number of DPE class changes that will result in being completely out of fossil energies
#' @param classement_veh The way to select households to switch to electric vehicule (Optimiste, Median, Pessimiste)
#' @param dom_effic_source A dataframe with 2 columns, which for each energy source forces an efficiency parameter
#' @param classement_bascule The classification for bascule priorisation
#' @param alignement_3ME A boolean indicating whether to align emissions on 3ME data
#' @param align_class_bascule The classes for which you allow bascule forcing to reach 3ME targets
#' @param align_yearnew_bascule A list of 2 arguments (fio and gaz) the gives the year for
#' @param align_jump_bascule The minimum jump in DPE due to renovation  after which you excludes fossiles to reach 3ME targets
#'
#' @return MatisseParams global list that contains all the params
#' @export
#'
#' @examples
#' initialize_MatisseParams("AMS")
#' initialize_MatisseParams()
initialize_MatisseParams <-
  function(scenario = "AMS",
           horizon = 2035,
           iter = 0,
           redistribution = "forfait",
           vec_dec = c(),
           vec_tuu = c(),
           classement = "Optimiste",
           class_force_bascule = c(),
           year_new_bascule = 2100,
           bascule_min_jump = 7,
           classement_veh = "Optimiste",
           veh_effic_VT = 0,
           dom_effic_source = c(),
           classement_bascule = "Optimiste",
           alignement_3ME = TRUE,
           align_class_bascule = "A",
           align_yearnew_bascule = list(Fuel = 2021, Gaz = 2021),
           align_jump_bascule = 2,
           save_intermed_file = FALSE) {


    MatisseCIRED:::initialize_Matisse()

    #Variable globale (<<-) qui définit les paramètre utilisés par Matisse
    #Peut ensuite être modifiée pour forcer d'autres valeurs
    MatisseParams <<- list(
      scenario = scenario,
      horizon = horizon,
      redistribution = redistribution,
      vec_dec = vec_dec,
      vec_tuu = vec_tuu,
      classement = classement,
      class_force_bascule = class_force_bascule,
      iter = iter,
      year_new_bascule = year_new_bascule,
      bascule_min_jump = bascule_min_jump,
      dom_effic_source = dom_effic_source,
      classement_veh = classement_veh,
      veh_effic_VT = veh_effic_VT,
      alignement_3ME = alignement_3ME,
      classement_bascule = classement_bascule,
      align_class_bascule = align_class_bascule,
      align_yearnew_bascule = align_yearnew_bascule,
      align_jump_bascule = align_jump_bascule,
      save_intermed_file = save_intermed_file
    )
    if (is.null(MatisseParams$dom_effic_source)) {
      MatisseParams[["dom_effic_source"]] <<- data.frame(
        sources = c("Elec", "Gaz", "Fuel", "GPL", "Urbain", "Solides") ,
        eff_gain = c(0, 0, 0, 0, 0, 0)
      )
    }

  }






# initialize_MatisseFiles ---------------------------------------------------------------------------------------------------------------------------------
#' @title initialize_MatisseFiles
#'
#' @return Returns a global variable named MatisseFiles, which contains all the files links to be used Matisse
#' @export
#'
#' @examples
#' initialize_MatisseFiles()
initialize_MatisseFiles <- function(){

  MatisseCIRED:::initialize_Matisse()

  #Variable globale (<<-) qui définit les fichiers utilisés par Matisse
  #Peut ensuite être modifiée pour forcer l'utilisation d'autres fichiers
  MatisseFiles <<- list(
    #Fixed files
    ## External source
    "elast_xl"                 = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/Econometrie_demande/elasticite_demande_finale.xlsx", sep=""),
    "depmen_bdf_csv"           = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDF_2010/depmen.csv", sep=""),
    "menage_calibr_2010_rd"    = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDFE_delauretis/menage_calibr_2010.RData", sep=""),
    "phebus_csv"               = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/PHEBUS/Clode_dpe_energie_decideur_revenu.csv", sep=""),
    "menage_bdf_xl"            = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDF_2010/menage.xlsx", sep=""),
    "indiv_bdf_xl"             = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDF_2010/individu.xlsx", sep=""),
    "c05_bdf_csv"              = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDF_2010/c05.csv", sep=""),
    "typovuln_xl"              = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/Econometrie_demande/datacp_typovuln_bdf_2011.xlsx", sep=""),
    "appmen_depact_2010_rd"    = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDFE_delauretis/appmen_depensesactiv_2010.RData", sep=""),
    "auto_bdf_xl"              = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDF_2010/AUTOMOBILE.xlsx", sep=""),
    "def_rev_xl"               = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/Nomenclature_CIRED_ADEME/Definition_revenus.xlsx", sep=""),
    "nom_coicop_3me_xl"        = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/Nomenclature_CIRED_ADEME/Nomenclature_coicop_threeme.xlsx", sep=""),
    "prix_class_csv"           = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDFE_delauretis/Prix_energie_par_classe.csv", sep=""),
    "appmen_intensites_2010_rd"= paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/BDFE_delauretis/appmen_intensites_2010.RData", sep=""),
    "coeff_ems_2010_rd"        = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/Data_interne/coeff_ems_2010.RData", sep=""),
    "ventes_vp_xl"             = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/ThreeME/Ventes_VP.xlsx", sep=""),
    "forcage_km_xl"            = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/ThreeME/forcage_vkm_teletravail_voirie.xlsx", sep=""),
    "insee_proj_men_xl"        = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/Donnees_brutes/INSEE/INSEE - projection men.xlsx", sep=""),
    "insee_proj_pop_xl"        = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/Donnees_brutes/INSEE/INSEE - projection pop.xlsx", sep=""),
    "appariement_bdf_entd_rd"  = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/Data_interne/appariement_bdf_entd.Rdata", sep =""),

    ##IMACLIM
    "sortie_3me_xl"                     = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/IMACLIM/Sorties Three-ME.xlsx", sep="") ,
    "output_macro_code_iter_0_ssrec_xl" = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/IMACLIM/Output_macro_code_iter_0_ssrec.xlsx", sep=""),
    "output_macro_code_iter_0_xl"       = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/IMACLIM/Output_macro_code_iter_0.xlsx", sep=""),
    "EMS_xl"                            = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/IMACLIM/EMS.xlsx", sep=""),
    "coeff_dep_ems_csv"                 = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/IMACLIM/coeff_dep_ems.csv", sep=""),
    "output_macro_code_xl"              = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/IMACLIM/Output_macro_code.xlsx", sep=""),
    "Mask5_xl"                          = paste(gsub("\\\\", "/", Sys.getenv("MATISSE")), "/Input/IMACLIM/Mask5.xlsx", sep=""),


    ##Output Initial format
    "dpe_stock_2010_rd"       = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/Initial format/dpe_stock_2010.RData", sep=""),
    "menage_forme_A1_rd"      = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/Initial format/menage_forme_A1.RData", sep=""),
    "menage_forme_A2_rd"      = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/Initial format/menage_forme_A2.RData", sep=""),
    "menage_forme_A3_rd"      = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/Initial format/menage_forme_A3.RData", sep=""),
    "menage_forme_A4_rd"      = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/Initial format/menage_forme_A4.RData", sep=""),
    "menage_forme_A_rd"       = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/Initial format/menage_forme.RData", sep=""),

    #Menage_ steps
    "menage_echelle_B1_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_B1.RData", sep=""),
    "menage_echelle_B2_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_B2.RData", sep=""),
    "menage_echelle_B_rd"                = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_B.RData", sep=""),
    "menage_echelle_C1_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_C1.RData", sep=""),
    "menage_echelle_C2_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_C2.RData", sep=""),
    "menage_echelle_C_rd"                = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_C.RData", sep=""),
    "menage_echelle_D1_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_D1.RData", sep=""),
    "menage_echelle_D2_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_D2.RData", sep=""),
    "menage_echelle_D3_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_D3.RData", sep=""),
    "menage_echelle_D4_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_D4.RData", sep=""),
    "menage_echelle_D5_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_D5.RData", sep=""),
    "menage_echelle_D_rd"                = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_D.RData", sep=""),
    "menage_contraintes_E1_rd"           = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_contraintes_E1.RData", sep=""),
    "menage_echelle_E2_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_E2.RData", sep=""),
    "menage_echelle_E_rd"                = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_E.RData", sep=""),
    "menage_echelle_F1_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_F1.RData", sep=""),
    "menage_echelle_F2_rd"               = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_F2.RData", sep=""),
    "menage_echelle_F_rd"                = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_F.RData", sep=""),
    "menage_echelle_final_csv"          = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/menage_echelle_final.csv", sep=""),

    #Outputs
    "IMACLIM_rd"                         = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/IMACLIM.RData", sep=""),
    "Threeme_rd"                         = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/ThreeME.RData", sep=""),
    "FC_2010_horizon_rd"                 = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/FC_2010_horizon.RData", sep=""),
    "EMS_scen_rd"                        = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/EMS.RData", sep=""),
    "IMACLIM_3ME_scen_horiz_xl"          = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/IMACLIM_3ME.xlsx", sep=""),
    "agreg_best_rd"                      = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/agreg_best.RData", sep=""),
    "agreg_final_csv"                    = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/agreg_final.csv", sep=""),
    "bascule_details_csv"                = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/bascule_details.csv", sep=""),
    "params_save_csv"                    = paste(gsub("\\\\","/", Sys.getenv("MATISSE")), "/Output/params.csv", sep="")
  )
}





