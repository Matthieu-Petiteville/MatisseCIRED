

# Standard values definition ------------------------------------------------------------------------------------------------------------------------------

#' @title get_sources
#' @description Loads a standard vector of strings containing the different sources of energy taken into account by Matisse
#'
#' @return An array of string
#' @export
#'
#' @examples
#' get_sources()
get_sources <- function() {
  sources <- c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")
  return(sources)
}


#' @title get_list_dep
#' @description Loads a standard vector of 18 strings that contains the default posts of economic behavior for the households in Matisse
#'
#' @return An array of strings
#' @export
#'
#' @examples
#' get_list_dep()
get_list_dep <- function() {
  list_dep = c(
    "agriculture",
    "dep_Elec",
    "dep_Gaz",
    "dep_GPL",
    "dep_Fuel",
    "dep_Urbain",
    "dep_Solides",
    "BTP",
    "prod_veh",
    "carb_lubr",
    "transp_rail_air",
    "transp_routes_eau",
    "loisirs_com",
    "autres_services",
    "autres",
    "loyers",
    "veh_occasion",
    "Hors_budget"
  )
  return(list_dep)
}


#' @title get_list_source_usage
#' @description Gets all the adequat 'source + usage' combinaisons. In particular, only keeps the electric usage for
#' ElecSpe, Clim and Ecl, which are only electrical
#'
#' @return An array of strings
#' @export
#'
#' @examples
#' get_list_source_usage()
get_list_source_usage <- function(){

  #Defaults
  usages_all <- c("ECS", "chauff", "Cuisson")
  usages_elec_only <- c("clim", "ecl", "ElecSpe")
  sources <- get_sources()

  #Adding usages
  list_source_usage <- c()
  for (usage in usages_all) {
    list_source_usage <- c(list_source_usage, paste(sources, usage, sep = "_"))
  }
  list_source_usage <- c(list_source_usage, paste("Elec", usages_elec_only, sep = "_"))

  return(list_source_usage)
}

#' @title get_list_usages
#' @description Loads a standard vector of the list of possible usages
#'
#' @return An array of strings
#' @export
#'
#' @examples
#' get_list_usages()
get_list_usages <- function(){
  usages <- c("ECS", "chauff", "clim", "Cuisson", "ecl", "ElecSpe")
  return(usages)
}



# Extract default values from different files -------------------------------------------------------------------------------------------------------------

#' @title get_elast
#' @description Load the elasticites from the file located in MatisseFiles$elast_xl
#'
#' Elasticities are typically coming from an econometrics work. Format of the source file is important be careful to respect it
#'
#' @return A tibble extract of the Excel file
#' @export
#'
#' @examples
#' get_elast()
get_elast <- function() {
  Elast <- suppressMessages(read_excel(MatisseFiles$elast_xl))
  Elast <-
    Elast %>%
    gather(key = CODADEME_typ , value = elast,-c(1, 2)) %>%
    separate(CODADEME_typ ,
             into = c("CODADEME", "typ_elast") ,
             sep = "_")
  colnames(Elast) <-
    c("Decile" , "Typo" , "CODADEME" , "typ_elast" , "elast")
  return(Elast)

}


#' @title get_TCO
#' @description Loads the TCO from the file located in MatisseFiles$IMACLIM_3ME_scen_horiz_xl, range C103
#'
#' @return A single value, the TCO value for the IMACLIM simulation
#' @export
#'
#' @examples
#' get_TCO()
get_TCO <- function(year_TCO){

  S <- switch (MatisseParams$scenario,
               "AMS" = "scen AMS",
               "AME" = "scen AME",
               "ssTCO" = "scen AMS ss TCO",
               "ssRES" = "scen AMS ss residentiel",
               "ssVE" = "scen AMS ss VE")
  suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl, sheet = S)))
  ThreeME <-
    scen %>%
    select(-Var) %>%
    gather(key = year, value = value, -c(1)) %>%
    filter(year == year_TCO, Def == "TCO")

  return(ThreeME$value[1])
}



# Advanced Matisse functions that do different extractions and calculations -------------------------------------------------------------------------------

#' @title get_DPE_stock_year
#' @description Loads from the file MatisseFiles$sortie_3me_xl, the 3ME standard file, in sheet AMS, the values for the stock of houses for each DPE class
#'
#' @param DPE_year The year for the 3ME simulation
#'
#' @return A dataframe contain the class of DPE and the stock for the DPE_year simulation of 3ME
#' @export
#'
#' @examples
#' get_DPE_stock_year(2020)
get_DPE_stock_year <- function(DPE_year){

  #Stock de DPE à l'année DPE_year
  suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl , sheet="scen AMS")))
  ThreeME <-
    scen %>%
    select(-Def) %>%
    gather(key=year , value=value , -c(1)) %>%
    filter(year==DPE_year)

  #Données DPE 3ME
  dpe_stock <-
    ThreeME %>%
    filter(Var %in% c("BUIL_H01_CA_2" , "BUIL_H01_CB_2" , "BUIL_H01_CC_2" , "BUIL_H01_CD_2" , "BUIL_H01_CE_2" , "BUIL_H01_CF_2" , "BUIL_H01_CG_2"))
  dpe_stock <- as.data.frame(dpe_stock)

  #Extraire nom de la classe de DPE
  dpe_stock <-
    dpe_stock %>%
    mutate(DPE= str_replace_all(Var, pattern="BUIL_H01_C",replacement="")) %>%
    mutate(DPE= str_replace_all(DPE, patter="_2",replacement=""))

  return(dpe_stock)
}



#' @title get_energie_dom_surf
#' @description A function that calculates the energy consumption for each household for its home with different values :
#' total energy consumption, consumption per square-meter.
#'
#' It read from the file MatisseFiles$prix_class_csv the data for the price for each energy for each household type
#'
#' @param menage The default format of menage data in Matisse
#' @param FC The growth factor, used in every Matisse code
#' @param getFulldf A boolean, default F, that states if you just want to returns the new columns or the full df
#'
#' @return A data frame containing either just the new calculated column of energy consumption or the full menage argument including the new columns
#' @export
#'
#' @examples
#' get_energie_dom_surf(menage, FC, F)
get_energie_dom_surf <- function(menage, FC, getFulldf = F){

# Data -------------------------------------------------------------
  list_source_usage <- get_list_source_usage()
  prix_classe <- read.csv2(MatisseFiles$prix_class_csv, header = TRUE, sep = ";",dec = ".", fill = TRUE)
  prix_classe <- arrange(prix_classe, quintile, typmen_corr, MI)


# Mise à l'échelle prix par classe  --------------------------------
  # Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025.
  prix_classe_horizon <- prix_classe
  prix_classe_horizon$prix_elec <- prix_classe$prix_elec * as.numeric(FC$A02)
  prix_classe_horizon[c("prix_fuel", "prix_gpl")] <- prix_classe[c("prix_fuel", "prix_gpl")] * as.numeric(FC$A04)
  prix_classe_horizon[c("prix_bois", "prix_chaleur", "prix_gaz")] <- prix_classe[c("prix_bois", "prix_chaleur", "prix_gaz")] * as.numeric(FC$A03)

# Classer ménages par classe ----------------------------------------------
  # Matrice des prix de chaque énergie pour chaque classe
  prix_classe_mat <- data.matrix(prix_classe_horizon[ ,c("prix_elec", "prix_gaz", "prix_fuel", "prix_gpl", "prix_bois", "prix_chaleur")], rownames.force = NA)

  # Attribution d'un numéro de classe de ménage à chaque ligne de prix_classe (de 1 à 60)
  menage$classe_men <-  as.factor(with(menage, as.integer(interaction(MI_corr, typmen_corr, quintileuc))))

  # Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
  dummies_classe_men <- model.matrix(~ classe_men,
                                     data = menage,
                                     contrasts.arg = list(
                                       classe_men = contrasts(
                                         menage$classe_men,
                                         contrasts = F)))

  #Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
  dummies_classe_men <- dummies_classe_men[,-1]

# Prix énergie par ménage -------------------------------------------------
  # Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
  prix_menages_horizon <- as.data.frame(dummies_classe_men %*% prix_classe_mat)
  prix_menages_horizon_bis <- as.data.frame(prix_menages_horizon)

  # Rajout colonne "ident_men" pour la fusion avec menage
  prix_menages_horizon <- cbind(menage$ident_men, prix_menages_horizon_bis)
  # renommer les colonnes
  colnames(prix_menages_horizon)<-c("ident_men",
                                    "prix_Elec",
                                    "prix_Gaz",
                                    "prix_Fuel",
                                    "prix_GPL",
                                    "prix_Solides",
                                    "prix_Urbain")

  # Rajout des prix et pondération de chaque ménage dans menage
  menage <- menage %>% left_join(prix_menages_horizon,by="ident_men")

# Volumes énergie/ménage ------------------------------------------------
  # Pour convertir les dépenses en volumes, division par le prix moyen à l'horizon de chaque source d'énergie.
  menage$vol_Elec <- menage$dep_Elec / menage$prix_Elec
  menage$vol_Gaz <- menage$dep_Gaz / menage$prix_Gaz
  menage$vol_GPL <- menage$dep_GPL / menage$prix_GPL
  menage$vol_Fuel <- menage$dep_Fuel / menage$prix_Fuel
  menage$vol_Solides <- menage$dep_Solides / menage$prix_Solides
  menage$vol_Urbain <- menage$dep_Urbain / menage$prix_Urbain
  menage$vol_tot <- menage %>% select(starts_with("vol_")) %>% rowSums()

  #Calcul des énergies surfaciques
  menage <- menage %>% mutate(energie_tot_surf = ifelse(surfhab_d > 0, vol_tot / surfhab_d, 0))

# Energie domestique ------------------------------------------------------
  #Calcul des volumes calculés pour chaque source d'énergie : Colonne source_usage / prix_source
  dep_source_usage <- menage[c("ident_men", list_source_usage)]
  sources <- get_sources()
  for(source_i in sources){
    col_idx <- colnames(dep_source_usage %>% select(contains(source_i)))
    for(col_name in col_idx){
      dep_source_usage[col_name] <- dep_source_usage[col_name] / menage[,paste("prix_",source_i,sep = "")]
    }
  }

  # Energie par usage (chauffage, ECS, clim) et somme sous ener_dom puis calcul de ener_dom_surf l'énergie domestique surfacique
  menage$ener_chauff <- dep_source_usage %>% select(contains("_chauff")) %>% rowSums()
  menage$ener_ECS <- dep_source_usage %>% select(contains("_ECS")) %>% rowSums()
  menage$ener_clim <- dep_source_usage %>% select(contains("_clim")) %>% rowSums()
  menage$ener_dom<- menage$ener_chauff + menage$ener_ECS + menage$ener_clim
  menage <-  menage %>% mutate(ener_dom_surf = ifelse(surfhab_d > 0, ener_dom / surfhab_d, 0))

# Exporter données ener dom  ----------------------------------------------
  if(getFulldf){
    menage_ener_dom <- menage
  }else{
    menage_ener_dom <- menage %>% select(ident_men,ener_dom_surf,ener_dom,energie_tot_surf)
  }
  return(menage_ener_dom)

}

#' @title get_emissions
#' @description Calculates the emissions for the the menage data. Uses the file MatisseFiles$coeff_dep_ems_csv as coefficient of emission
#' between the initial year and the calculated year. Uses coeff_ems_2010_rd as the init year emissions.
#'
#' @param menage The default format of menage data in Matisse
#' @param FC The growth factor, used in every Matisse code
#'
#' @return An array the size of nrow(menage) that contains all the domestic emissions of the households
#' @export
#'
#' @examples
#' get_emissions(menage, FC)
get_emissions <- function(menage, FC){

  TCO <- get_TCO(MatisseParams$horizon)

  #Données
  coeff_dep_ems <- suppressMessages(read_csv(MatisseFiles$coeff_dep_ems_csv))
  load(MatisseFiles$coeff_ems_2010_rd)

  # Coefficient croissance des émissions
  FC_coeff <-
    coeff_dep_ems %>%
    filter(year == MatisseParams$horizon) %>%
    filter(scenario == MatisseParams$scenario) %>%
    select(Variable, FC_coeff_emission) %>%
    spread(key = Variable, value = FC_coeff_emission)

  #Calcul des émissions rapportées à l'année initiale
  menage <-
    menage %>%
    mutate(CL_2010 = 0) %>%
    mutate(Oil_2010 = (Fuel_chauff + Fuel_ECS + GPL_chauff + GPL_ECS) / FC$A07) %>%
    mutate(Gaz_2010 = (Gaz_chauff + Gaz_ECS + Urbain_ECS + Urbain_chauff + Solides_chauff + Solides_ECS) / FC$A03)

  #Calcul de la taxe carbone payée par type d'énergie : émissions rapportées à 2010 * coeff * FC * TCO
  menage <-
    menage %>%
    mutate(TCO_CL  = CL_2010  * coeff_ems_2010$coeff_CL_2010  * FC_coeff$C21 * TCO) %>%
    mutate(TCO_Oil = Oil_2010 * coeff_ems_2010$coeff_Oil_2010 * FC_coeff$C22 * TCO) %>%
    mutate(TCO_Gaz = Gaz_2010 * coeff_ems_2010$coeff_Gaz_2010 * FC_coeff$C24 * TCO)

  #Calcul des émissions à partir de la taxe carbone
  menage <-
    menage %>%
    mutate(ems_CL = TCO_CL / TCO,
           ems_Oil = TCO_Oil / TCO,
           ems_Gaz = TCO_Gaz / TCO) %>%
    mutate(ems_tot_chauff_ecs = ems_CL + ems_Oil + ems_Gaz)


  return(menage$ems_tot_chauff_ecs)
}



#' @title get_gain_energie
#' @description Calculates a matrix of energetic gains for a DPE change, according to the conso_moy_dep vector of consumptions
#'
#' @param conso_moy_dep An array of consumption with names being letters from A to G
#'
#' @return A tibble containing the change in percentage in the consumption linked to the change of DPE, depending on the initial and final DPE
#' @export
#'
#' @examples
#' conso <- 1:7 * 10
#' names(conso) <- LETTERS[1:7]
#' get_gain_energie(conso)
get_gain_energie <- function(conso_moy_dep){
  # Matrice des gains énergétique des passages de DPE Mat_gain_ener
  Mat_gain_ener <- data.frame("DPE_before" = sort(rep(LETTERS[1:7], 7)), "DPE_after" = rep(LETTERS[1:7], 7))
  Mat_gain_ener$value_after <- sapply(Mat_gain_ener$DPE_after, function(x) as.numeric(conso_moy_dep[x]))
  Mat_gain_ener$value_before <- sapply(Mat_gain_ener$DPE_before, function(x) as.numeric(conso_moy_dep[x]))
  Mat_gain_ener$value <- (Mat_gain_ener$value_after - Mat_gain_ener$value_before) / Mat_gain_ener$value_before
  Mat_gain_ener <- Mat_gain_ener %>% select(-c(value_after, value_before))

  return(Mat_gain_ener)
}



#' @title ventilate_solde
#' @description Ventilate the argument solde across the different spending posts. Based on the standard menage data.frame for Matisse,
#' solde an array of the solde to be distributed for each menage, FC the growth factor.
#'
#' @param menage Standard menage data.frame for Matisse
#' @param solde An array containing the value to be distributed for each household
#' @param FC The growth factor to be used for each spending post
#' @param step_ventil The type of ventilation : VE or REHAB
#'
#' @return Returns a modified menage data.frame
#' @export
#'
#' @examples
#' ventilate_solde(menage, solde, FC, "VE")
ventilate_solde <- function(menage, solde, FC, step_ventil){

  # Data -----------------------------------------------------------------
  menage_ventil <- menage %>% left_join(solde, by = "ident_men")
  list_dep_14 <- c("agriculture",
                   "dep_Elec",
                   "dep_Gaz",
                   "dep_autres_energies",
                   "BTP",
                   "prod_veh",
                   "carb_lubr",
                   "transp_rail_air",
                   "transp_routes_eau",
                   "loisirs_com",
                   "autres_services",
                   "autres",
                   "loyers",
                   "veh_occasion")
  list_dep <- get_list_dep()

  # Ventilation -------------------------------------------------------------
  menage_ventil <- menage_ventil %>% mutate(dep_autres_energies = dep_Fuel + dep_GPL + dep_Solides + dep_Urbain)
  menage_ventil$Rcons_bis <- rowSums(menage_ventil[list_dep_14])
  menage_ventil <- replace_na(menage_ventil, list(Rcons_bis = 0))

  # Pour chaque secteur, calcul de la part de conso vs le reste, valeurs NA passées à 0
  # Calcul de l'IPStone : produit des (FCi ^ share_i)
  list_A <- paste("A", formatC(1:length(list_dep_14), width = 2, flag = 0), sep="")
  menage_ventil$IP_stone <- 1
  for (i in 1:length(list_A)) {
    #Part conso
    col_name <- paste("share", list_A[i], sep = "_")
    menage_ventil[, col_name] <- menage_ventil[, list_dep_14[i]] / menage_ventil$Rcons_bis
    menage_ventil <- replace_na(menage_ventil, replace = array2list(col_name))
    #IPStone
    menage_ventil$IP_stone <- menage_ventil$IP_stone * as.numeric(FC[1,list_A[i]]) ** as.numeric(unlist(menage_ventil[, col_name]))
  }
  menage_ventil$RDB_reel <-  menage_ventil$RDB / menage_ventil$IP_stone
  menage_ventil <- replace_na(menage_ventil, list(RDB_reel = 0))
  # si le RDB_reel est >0 ou ==0, alors on considère le ratio solde/Rcons_bis
  # qui est le meilleur proxy du comportement de consommation du ménage
  # -menage_ventil$solde/menage_ventil$Rcons_bis
  # L'IP stone ne rentre pas dans ce dernier calcul, diviserait à la fois le solde et le Rcons


  iter <- TRUE #Pour la première itération
  nb_iter_RDB <- 0
  list_dep_autres_ener=c("dep_GPL", "dep_Fuel", "dep_Urbain", "dep_Solides")
  while(iter & nb_iter_RDB < 60){
    sauv_menage <- menage_ventil
    nb_iter_RDB <- nb_iter_RDB + 1

    # On ne reventile pas sur les biens incluant énergie sauf étape 3.4
    # On ne reventile jamais sur les biens ener, effet rebond pris en compte dans les chiffres de Gael
    # qu'on traduit en 3.1, 3.2 et 3.3, 3.4 n'est qu'un effet qu'on applique expst
    if(step_ventil=="REHAB"){
      ### BIENS HORS ENERGIE
      # on repart des dépenses de ménage (base input) pour ne pas empiler les itérations
      for(i in c(1, 5:14)){
        elast_rev <- paste("elast_rev_", list_A[i], sep = "")
        RDB_divisor <- menage_ventil$IP_stone * menage_ventil$RDB_reel
        l_idx <- which(menage_ventil$RDB_reel <= 0 | abs(menage_ventil$solde) > menage_ventil$RDB)
        RDB_divisor[l_idx] <- menage_ventil$Rcons_bis[l_idx]
        menage_ventil[list_dep_14[i]] <- menage[list_dep_14[i]] * (1 + menage_ventil[elast_rev] * -menage_ventil$solde / RDB_divisor)
      }
      # NB : le Hors budget est exclus de la reventilation, nous ne connaissons pas le comportement des agents vis-à-vis cet agrégat qui regroupe des dépenses exceptionnelles et/ou importantes

    }
    if(step_ventil=="VE"){
      ### Biens hors carburant
      # A02
      menage_ventil$dep_Elec <-
        menage$dep_Elec *
        (1 + menage_ventil$elast_rev_A02 *
           ifelse(abs(menage_ventil$solde) > menage_ventil$RDB,
                  -menage_ventil$solde / menage_ventil$Rcons_bis,
                  -menage_ventil$solde / menage_ventil$IP_stone / menage_ventil$RDB_reel))

      # A03
      menage_ventil$dep_Gaz <-
        menage$dep_Gaz *
        (1 + menage_ventil$elast_rev_A03 *
           ifelse(abs(menage_ventil$solde) > menage_ventil$RDB,
                  -menage_ventil$solde / menage_ventil$Rcons_bis,
                  -menage_ventil$solde / menage_ventil$IP_stone / menage_ventil$RDB_reel))

      # A04
      menage_ventil[list_dep_autres_ener] <-
        menage[list_dep_autres_ener] *
        (1 + menage_ventil$elast_rev_A04 *
           ifelse(abs(menage_ventil$solde) > menage_ventil$RDB,
                  -menage_ventil$solde / menage_ventil$Rcons_bis,
                  -menage_ventil$solde / menage_ventil$IP_stone / menage_ventil$RDB_reel))

      for (i in c(1, 5:6, 8:14)) {
        #bien 7 exclus
        elast_rev <- paste("elast_rev_", list_A[i], sep = "")
        menage_ventil[list_dep_14[i]] <-
          menage[list_dep_14[i]] *
          (1 + menage_ventil[elast_rev] *
             ifelse(abs(menage_ventil$solde) > menage_ventil$RDB,
                    -menage_ventil$solde / menage_ventil$Rcons_bis,
                    -menage_ventil$solde / menage_ventil$IP_stone / menage_ventil$RDB_reel))
      }
    }

    #On annule les dépenses NA sur tous les types de dépenses
    menage_ventil <- replace_na(menage_ventil, replace = array2list(list_dep))

    #Depenses énergie
    menage_ventil <-
      menage_ventil %>%
      mutate(dep_autres_energies = dep_Fuel + dep_GPL + dep_Solides + dep_Urbain)
    menage_ventil$Rcons_bis <- rowSums(menage_ventil[,list_dep_14])
    menage_ventil <- replace_na(menage_ventil, replace = list(Rcons_bis = 0))

    #Calcul des part de dépense par secteur de list_dep_14, nettoyage des NA, calcul de l'IPStone
    menage_ventil$IP_stone <- 1
    for (i in 1:14){
      col_name <- paste("share", list_A[i], sep = "_")
      menage_ventil[, col_name] <- menage_ventil[list_dep_14[i]] / menage_ventil$Rcons_bis
      menage_ventil <- replace_na(menage_ventil, replace = array2list(col_name))

      menage_ventil$IP_stone <- menage_ventil$IP_stone * as.numeric(FC[1,list_A[i]]) ** as.numeric(unlist(menage_ventil[, col_name]))
    }
    menage_ventil$RDB_reel <-  menage_ventil$RDB / menage_ventil$IP_stone
    tol <- abs((menage_ventil$RDB_reel - sauv_menage$RDB_reel) / sauv_menage$RDB_reel)

    if (max(tol, na.rm = T) > 10 ^ -6) {
      iter = TRUE
    } else {
      iter = FALSE
    }
  }

  menage_ventil <- menage_ventil %>% select(colnames(menage))
  return(menage_ventil)
}


#' @title get_3ME_vehicule_stats
#' @description Extracts from the MatisseFiles$Threeme_rd file, for the set of params currently used, the different values needed by
#' Matisse for vehicule simulation
#'
#' @param year_veh The year to extract the data at
#'
#' @return A list of various values
#' @export
#'
#' @examples
#' get_3ME_vehicule_stats(2020)
get_3ME_vehicule_stats <- function(year_veh){

  #Permet d'extraire en list les données sur les véhicules selon 3ME
  load(MatisseFiles$Threeme_rd)

  # Km parcourus --------------------------------------------------------------------------------------------------------------------------------------------
  # Somme de tous les Km-véhicules parcourus par classe de performance et motorisation (e.g. KM_AUTO_H01_CE_22_2 (classe F, fioul)
  KM_AUTO_TOT <- as.numeric(ThreeME %>%
                              filter(year == year_veh) %>%
                              filter(str_detect(Var, 'KM_AUTO_H01_C')) %>%
                              summarise(sum(value))) * 1000

  # Stock de véhicules particuliers en milliers
  AUTO_H01_2 <- as.numeric((ThreeME %>%
                              filter(Var == "AUTO_H01_2") %>%
                              filter(year == year_veh) %>%
                              select(value))[1, ]) * 1000

  #vkm moyen
  KM_AUTO_2010 <- KM_AUTO_TOT / AUTO_H01_2


  # Bonus malus nets ----------------------------------------------------------------------------------------------------------------------------------------
  #Bonus-malus net pour les Véhicules thermiques
  BM_net <- 0
  TOT_VTH_nv <- 0
  for (x in LETTERS[1:7]){
    # Volumes de ventes de veh thermiques de classe X
    Vol <- as.numeric(ThreeME %>%
                        filter(year == year_veh) %>%
                        filter(Var == paste("NEWAUTO_TH_H01_C", x, "_2", sep = "")) %>%
                        select(value)) * 1000
    TOT_VTH_nv <- TOT_VTH_nv + Vol
    # Volume de Bonus-Malus pour les véhicules th de classe X
    BM <- as.numeric(ThreeME %>%
                       filter(year == year_veh) %>%
                       filter(Var == paste("BONUS_TH_H01_C", x, "_2", sep = "")) %>%
                       select(value)) * 1000
    BM_net <- BM_net + Vol * BM
  }
  BM_avg <- BM_net / TOT_VTH_nv

  TOT_VTH_euros <-as.numeric(ThreeME %>%
                               filter(Var=="PNEWAUTO_TH_H01_2*NEWAUTO_TH_H01_2") %>%
                               filter(year==year_veh) %>%
                               select(value)) * 10^6 # en Millions €

  # Bonus malus VE ------------------------------------------------------------------------------------------------------------------------------------------
  Bonus_VE <- as.numeric(ThreeME %>%
                           filter(year == year_veh) %>%
                           filter(Var == "BONUS_ELEC_H01_2") %>%
                           select(value)) * 1000


  # Données Prix ----------------------------------------------------------------------------------------------------------------------------------------------------
  # Coût d'un VE en électricité par kilomètre : coût total / km parcourus
  cout_VE_tot <-as.numeric(ThreeME %>%
                             filter(Var == "EXP_AUTO_H01_CA_23_2*PEXP_23_H01_2") %>%
                             filter(year == year_veh) %>%
                             select(value)) * 10^6 # en M€

  vkm_VE_tot<-as.numeric(ThreeME %>%
                           filter(Var == "KM_AUTO_H01_CA_23_2") %>%
                           filter(year == year_veh) %>%
                           select(value)) *1000
  cout_VE_km<-cout_VE_tot/vkm_VE_tot

  #Taux d'intérêt
  R_I_AUTO_H01_CG_2 <- as.numeric(ThreeME %>%
                                    filter(Var == "R_I_AUTO_H01_CG_2") %>%
                                    filter(year == year_veh) %>%
                                    select(value))

  # Durée emprunt
  R_RMBS_AUTO_ELEC_H01_CA <- as.numeric(ThreeME %>%
                                          filter(Var == "R_RMBS_AUTO_ELEC_H01_CA") %>%
                                          filter(year == year_veh) %>%
                                          select(value))

  #Ventil VP : la ventilation des ventes entre véhicules particuliers et professionnels
  ventes_VP <- read_excel(path=MatisseFiles$ventes_vp_xl)
  ventil_VP <- as.numeric(ventes_VP %>% filter(Year == year_veh) %>% select(Particuliers))

  ## Prix unitaire d'un VE
  # Données ThreeME de stock (en milliers)
  TOT_VE_nv <-as.numeric((ThreeME %>%
                            filter(Var == "NEWAUTO_ELEC_H01_2") %>%
                            filter(year == year_veh) %>%
                            select(value))[1,] * 1000 * ventil_VP) # en milliers

  TOT_VE_euros <- as.numeric((ThreeME %>%
                                filter(Var == "PAUTO_ELEC_H01_2*NEWAUTO_ELEC_H01_2") %>%
                                filter(year == year_veh) %>%
                                select(value))[1, ] * ventil_VP) * 10 ^ 6 # en Millions €
  P_VE <- TOT_VE_euros / TOT_VE_nv

  ## Prix unitaire d'un Veh_thermique
  # Données ThreeME de stock (en milliers)
  TOT_VTH_nv <- as.numeric((ThreeME %>%
                              filter(Var == "NEWAUTO_TH_H01_2") %>%
                              filter(year == year_veh) %>%
                              select(value))[1, ]) * 1000 * ventil_VP #en Milliers

  TOT_VTH_euros <- as.numeric(ThreeME %>%
                                filter(Var == "PNEWAUTO_TH_H01_2*NEWAUTO_TH_H01_2") %>%
                                filter(year == year_veh) %>%
                                select(value)) * 10 ^ 6 * ventil_VP # en Millions €
  P_VTH <- TOT_VTH_euros/TOT_VTH_nv

  #Bonus-malus relatif au prix d'un VT : Malus égal à 0.7% du prix d'achat
  BM_rel <- BM_avg / P_VTH


  # Return --------------------------------------------------------------------------------------------------------------------------------------------------
  data_veh <- list(bonus_malus_net = BM_net,
                   bonus_malus_average = BM_avg,
                   bonus_malus_rel = BM_rel,
                   bonus_ve = Bonus_VE,
                   total_veh_th_nv = TOT_VTH_nv,
                   total_veh_th_nv_euros = TOT_VTH_euros,
                   total_veh_elec_nv = TOT_VE_nv,
                   prix_veh_th = P_VTH,
                   prix_veh_elec = P_VE,
                   km_parcourus = KM_AUTO_TOT,
                   total_vehicules = AUTO_H01_2,
                   km_parcourus_average = KM_AUTO_2010,
                   cout_VE_km = cout_VE_km,
                   taux_interet = R_I_AUTO_H01_CG_2,
                   duree_emprunt = R_RMBS_AUTO_ELEC_H01_CA)
  return(data_veh)

}


#' @title calcul_interet_principal
#' @description Calculates the interest and principal discounted values for a loan of loan euros, over a certain amount of years
#' Based on the function amort.period -> check it to see the logic behind
#'
#' @param loan The loaned amount
#' @param n The number of payments
#' @param year_purchase The year of purchase
#' @param horizon The year of our horizon
#' @param i The nominal interest rate convertible ic times per year
#' @param pf The payment frequency- number of payments per year
#'
#' @return A tibble containing the interest and principal values
#' @export
#'
#' @examples
#' calcul_interet_principal(10000,10,2010,2015,0.01)
calcul_interet_principal <- function(loan, n, year_purchase, horizon, i, pf = 1) {
  if (loan == 0) {
    return(c(0, 0))
  }
  A <- amort.period(Loan = loan, n = n, i = i, t = horizon - year_purchase + 1)
  B <- as.data.frame(A)
  C <- cbind(rownames(B), B)
  dim(C)
  colnames(C) <- c("Var", "Value")
  D <- C %>% spread(key = Var, value = Value)
  D
  D$Balance
  repayment <- D %>% select(`Int Paid`, `Princ Paid`)
  colnames(repayment) <- c("Int", "Princ")
  return(repayment)
}


#' @title select_bascule
#'
#' @param sub_men_ech
#' @param Sum_horizon_Mat
#' @param scenario_classement_bascule
#' @param redistribution
#'
#' @return A list of data with the selection of bascules
#' @export
#'
#' @examples
select_bascule <- function(sub_men_ech, Sum_horizon_Mat, scenario_classement_bascule, redistribution, ener){

  ener_ind <- which(Sum_horizon_Mat$type == ener)
  dep_ener <- paste("dep_e_", ener, sep = "")
  bascule_ener <- paste("bascule_", ener, sep = "")


  #Variable de classement pour bascule
  sub_men_ech <-
    sub_men_ech %>%
    dplyr::mutate(kWh_rank_opt = row_number(dep_surf_to_rank)) %>%
    dplyr::mutate(kWh_rank_pess = max(kWh_rank_opt, na.rm = T) - kWh_rank_opt +
                    1) %>%
    dplyr::mutate(kWh_rank_med = kWh_rank_pess - kWh_rank_opt) %>%
    mutate_when(kWh_rank_med <= 0, list(kWh_rank_med = -kWh_rank_med + 1))

  if (str_detect(scenario_classement_bascule, "Pessimiste")) {
    sub_men_ech <- sub_men_ech %>% mutate(kWh_rank = kWh_rank_pess)
  }
  if (str_detect(scenario_classement_bascule, "Optimiste")) {
    sub_men_ech <- sub_men_ech %>% mutate(kWh_rank = kWh_rank_opt)
  }
  if (scenario_classement_bascule == "Median") {
    sub_men_ech <- sub_men_ech %>% mutate(kWh_rank = kWh_rank_med)
  }

  cpt = 1
  nb_basc <- 0
  #On calibre sur les consommations de fioul ou gaz
  while(Sum_horizon_Mat$left_to_bascule[ener_ind] > 0 && cpt <= max(sub_men_ech$kWh_rank)){
    men_line <- which(sub_men_ech$kWh_rank == cpt)
    if(sub_men_ech[men_line,dep_ener] > 0 && sub_men_ech[men_line,bascule_ener] == 0){
      sub_men_ech[men_line, bascule_ener] <- 1
      removed_ener <- (sub_men_ech[men_line, dep_ener] / 10^6)[1,1]
      Sum_horizon_Mat$left_to_bascule[ener_ind] = Sum_horizon_Mat$left_to_bascule[ener_ind] - removed_ener
      nb_basc <- nb_basc + 1
    }
    cpt = cpt + 1
  }

  return(list(sub_men_ech = sub_men_ech,
              Sum_horizon_Mat = Sum_horizon_Mat,
              nb_basc = nb_basc))

}


#' @title get_summary_dep_table
#' @description Generates a table that summarises the emissions between menage_echelle and the 3ME data
#'
#' @param menage_echelle
#'
#' @return A Sum_horizon_mat
#' @export
#'
#' @examples
get_summary_dep_table <- function(menage_echelle){

  load(MatisseFiles$menage_forme_A_rd)
  load(MatisseFiles$FC_2010_horizon_rd)
  #3ME
  S <- switch (MatisseParams$scenario,
               "AMS" = "scen AMS",
               "AME" = "scen AME",
               "ssTCO" = "scen AMS ss TCO",
               "ssRES" = "scen AMS ss residentiel",
               "ssVE" = "scen AMS ss VE")
  suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl, sheet = S)))
  ThreeME <-
    scen %>%
    select(-Def) %>%
    gather(key = year, value = value, -c(1))
  ThreeMe_year <- ThreeME %>% filter(year %in% c(2010, MatisseParams$horizon))
  Ext_3ME <- ThreeMe_year[grep("^EXP_BUIL_H01_C._22.*$", ThreeMe_year$Var),] %>% mutate(type = "fio") %>% select(-Var)
  Ext_3ME <- rbind(Ext_3ME, ThreeMe_year[grep("^EXP_BUIL_H01_C._23.*$", ThreeMe_year$Var),] %>% mutate(type = "ele") %>% select(-Var))
  Ext_3ME <- rbind(Ext_3ME, ThreeMe_year[grep("^EXP_BUIL_H01_C._24.*$", ThreeMe_year$Var),] %>% mutate(type = "gaz") %>% select(-Var))

  Sum_3ME <- Ext_3ME %>%
    mutate(type_year = paste(type,year,sep="_")) %>%
    group_by(type_year) %>%
    dplyr::summarise(sum_Me = sum(value)) %>%
    mutate(type = substr(type_year,1,3),  year = substr(type_year,5,9)) %>%
    select(-type_year)
  Sum_3ME$type <- gsub("ele", "Elec", Sum_3ME$type)
  Sum_3ME$type <- gsub("fio", "Fuel", Sum_3ME$type)
  Sum_3ME$type <- gsub("gaz", "Gaz", Sum_3ME$type)

  #Menage 2010 puis calcul des valeurs cible par type d'énergie à l'horizon

  Sum_Mat <- data.frame(sum_Me = sum((menage_forme$dep_Fuel + menage_forme$dep_GPL) * menage_forme$pondmen) / 10^6,
                        type = "Fuel",
                        year = 2010)
  Sum_Mat <- rbind(Sum_Mat,
                   data.frame(sum_Me = sum(menage_forme$dep_Elec * menage_forme$pondmen) / 10^6,
                              type = "Elec",
                              year = 2010))
  Sum_Mat <- rbind(Sum_Mat,
                   data.frame(sum_Me = sum((menage_forme$dep_Gaz + menage_forme$dep_Urbain + menage_forme$dep_Solides) * menage_forme$pondmen) / 10^6,
                              type = "Gaz",
                              year = 2010))

  #Sum horizon
  Sum_horizon_Mat <- data.frame(sum_Me = sum((menage_echelle$dep_Fuel + menage_echelle$dep_GPL) * menage_echelle$pondmen)/  FC$A07 / 10^6,
                                type = "Fuel",
                                year = MatisseParams$horizon)
  Sum_horizon_Mat <- rbind(Sum_horizon_Mat,
                           data.frame(sum_Me = sum(menage_echelle$dep_Elec * menage_echelle$pondmen)/ FC$A02 / 10^6,
                                      type = "Elec",
                                      year = MatisseParams$horizon))
  Sum_horizon_Mat <- rbind(Sum_horizon_Mat,
                           data.frame(sum_Me = sum((menage_echelle$dep_Gaz + menage_echelle$dep_Urbain + menage_echelle$dep_Solides) * menage_echelle$pondmen)/ FC$A03 / 10^6,
                                      type = "Gaz",
                                      year = MatisseParams$horizon))

  for(ener in Sum_Mat$type){
    sub_3ME <- Sum_3ME[which(Sum_3ME$type == ener),]
    sub_Mat <- Sum_Mat[which(Sum_Mat$type == ener),]
    Sum_Mat <- rbind(Sum_Mat,
                     data.frame(sum_Me = sub_Mat[1,"sum_Me"] * sub_3ME[2,"sum_Me"]/sub_3ME[1,"sum_Me"],
                                type = ener ,
                                year = MatisseParams$horizon))
  }

  Sum_horizon_Mat <- Sum_horizon_Mat %>%
    left_join(Sum_Mat %>%
                filter(year == MatisseParams$horizon) %>%
                mutate(target_sum_Me = sum_Me) %>%
                select(target_sum_Me, type), by = "type")
  Sum_horizon_Mat <- Sum_horizon_Mat %>%
    mutate(diff_Me = sum_Me - target_sum_Me) %>%
    mutate(left_to_bascule = diff_Me)

  return(Sum_horizon_Mat)
}



