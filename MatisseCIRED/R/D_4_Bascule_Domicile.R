# Changement de mode de chauffage et ECS
# Bascule du chauffage et de l'ECS des ménages de classe A en 100% électrique.


#' @title bascule_domicile
#' @description Function that handles the switch of housing category
#'
#' @param menage A menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_D4_rd
#'
#' @return A menage dataframe
#'
#' @examples
#' bascule_domicile(menage, FALSE)
#'
bascule_domicile <- function(menage,  save_intermed_file = F){


# Data ----------------------------------------------------------------------------------------------------------------------------------------------------

  #Importer taux de croissance des prix et des revenus
  load(MatisseFiles$FC_2010_horizon_rd)
  # Import des prix d'énergie par classe de ménage : en €/MWh
  prix_classe <-
    read.csv2(MatisseFiles$prix_class_csv, header = TRUE, sep = ";", dec = ".", fill = TRUE)

  menage_echelle <- menage
  list_source_usage <- get_list_source_usage()
  list_dep <- get_list_dep()
  usages <- get_list_usages()
  sources <- get_sources()
  dep_sources <- paste("dep", sources, sep = "_")
  dep_sources_verif <- paste(dep_sources, "verif", sep = "_")

  #ExtractDomEfficiency gradient from 3ME
  S <- switch (
    MatisseParams$scenario,
    "AMS" = "scen AMS",
    "AME" = "scen AME",
    "ssTCO" = "scen AMS ss TCO",
    "ssRES" = "scen AMS ss residentiel",
    "ssVE" = "scen AMS ss VE"
  )
  suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl, sheet = S)))
  ThreeME <-
    scen %>%
    select(-Def) %>%
    gather(key = year, value = value, -c(1)) %>%
    filter(year %in% c(2010, MatisseParams$horizon))
  ThreeME <- ThreeME[grep("^ENER_BUIL_H01_C._2.11630/BUIL_H01_C._2$", ThreeME$Var), ]

  dom_eff_df <- data.frame(classe = LETTERS[1:7])
  dom_eff_df$Code <- paste("ENER_BUIL_H01_C", dom_eff_df$classe, "_2*11630/BUIL_H01_C", dom_eff_df$classe, "_2", sep = "")
  dom_eff_df <- dom_eff_df %>%
    left_join(ThreeME %>% filter(year == 2010), by = c("Code" = "Var")) %>%
    mutate(Value_init = value) %>%
    select(-year, -value)
  dom_eff_df <- dom_eff_df %>%
    left_join(ThreeME %>% filter(year == MatisseParams$horizon), by = c("Code" = "Var")) %>%
    mutate(Value_hor = value) %>%
    select(-year, -value)
  dom_eff_df <- dom_eff_df %>%
    mutate(Dom_eff = Value_hor/ Value_init - 1)

  # Préparation des données prix énergie par classe ----------------------------------------
  # prix energie par classe de ménage : dans l'ordre : par quintile, type de ménage et type de logement (individuel ou collectif)
  prix_classe <- arrange(prix_classe, quintile, typmen_corr, MI)

  # Mise àl'écheclle des prix d'énergie par classe
  # Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025.
  prix_classe_horizon <- prix_classe
  # A02
  prix_classe_horizon$prix_elec <- prix_classe$prix_elec * FC$A02
  # A03
  prix_classe_horizon[c("prix_gaz", "prix_bois", "prix_chaleur")] <- prix_classe[c("prix_gaz", "prix_bois", "prix_chaleur")] * FC$A03
  # A04
  prix_classe_horizon[c("prix_fuel", "prix_gpl")] <- prix_classe[c("prix_fuel", "prix_gpl")] * FC$A04
  # Matrice des prix de chaque énergie pour chaque classe
  prix_classe_mat <-
    data.matrix(prix_classe_horizon[, c("prix_elec",
                                        "prix_gaz",
                                        "prix_fuel",
                                        "prix_gpl",
                                        "prix_bois",
                                        "prix_chaleur")], rownames.force = NA)


  # Classement des ménages par classe -----------------------------------------------------------------------------------------------------------------------
  # Attribution d'un numéro de classe de ménage à chaque ligne de appmen (de 1 à 60)
  menage_echelle$classe_men <- as.factor(with(menage_echelle, as.integer(interaction(MI_corr, typmen_corr, quintileuc))))
  # Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
  # Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
  dummies_classe_men <- model.matrix(~ classe_men,
                                     data = menage_echelle,
                                     contrasts.arg = list(
                                       classe_men = contrasts(
                                         menage_echelle$classe_men,
                                         contrasts = F)))
  dummies_classe_men <- dummies_classe_men[, -1]


  # Prix énergie par ménage ---------------------------------------------------------------------------------------------------------------------------------
  # Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
  prix_menages_horizon <- as.data.frame(dummies_classe_men %*% prix_classe_mat)
  prix_menages_horizon_bis <- as.data.frame(prix_menages_horizon)

  # Rajout colonne "ident_men" pour la fusion avec det_ener
  prix_menages_horizon <- cbind(menage_echelle$ident_men, prix_menages_horizon_bis)
  # renommer les colonnes
  colnames(prix_menages_horizon)<-c("ident_men",
                                    "prix_Elec",
                                    "prix_Gaz",
                                    "prix_Fuel",
                                    "prix_GPL",
                                    "prix_Solides",
                                    "prix_Urbain")

  # Rajout des prix et pondération de chaque ménage dans dep_ener
  menage_echelle <- menage_echelle %>% left_join(prix_menages_horizon,by="ident_men")
  menage_echelle <- replace_na(menage_echelle, array2list(list_source_usage))


  # Selection menages --------------------------------------------------------
  menage_echelle <-
    menage_echelle %>%
    mutate(DPE_jump = - match(DPE_horizon,LETTERS) + match(DPE_pred,LETTERS))
  menage_echelle$bascule <- 0
  # Bascule des ménages : on force la bascule pour les ménages de classe d'arrivée 'class_force_bascule',
  # qui achètent après year_new_bascule ou qui réhabilitent avec un saut minimum
  l_idx <- which(menage_echelle$classe_arr %in% MatisseParams$class_force_bascule |
                   menage_echelle$year_neuf > MatisseParams$year_new_bascule |
                   (menage_echelle$year_rehab > 0 & menage_echelle$DPE_jump > MatisseParams$bascule_min_jump))
  menage_echelle$bascule[l_idx] <- 1


  # Bascule -----------------------------------------------------------------
  #On bascule tous les usages hormis ceux de l'électricité
  #On calcule d'abord les volumes pour chaque source_usage à basculer, en divisant la consommation par
  list_source_usage_bascule <- list_source_usage[-grep("Elec", list_source_usage)]
  sources_bascules <- sources[-grep("Elec", sources)]
  for (source_usage in list_source_usage_bascule){
    source_basc <- strsplit(source_usage,"_")[[1]][1]
    menage_echelle[,paste(source_usage, "vol", sep="_")] <- menage_echelle[source_usage] / menage_echelle[, paste("prix_",source_basc,sep = "")]
  }

  #Bascule des différents usages vers l'électricité
  menage_echelle$vol_basc_elec_ECS <- rowSums(menage_echelle %>% select(ends_with("ECS_vol")))
  menage_echelle$vol_basc_elec_chauff <- rowSums(menage_echelle %>% select(ends_with("chauff_vol")))
  menage_echelle$vol_basc_elec_cuisson <- rowSums(menage_echelle %>% select(ends_with("Cuisson_vol")))

  #Calcul des coûts en élec des bascules
  menage_echelle <-
    menage_echelle %>%
    mutate(dep_basc_elec_ECS = vol_basc_elec_ECS * prix_Elec) %>%
    mutate(dep_basc_elec_chauff = vol_basc_elec_chauff * prix_Elec) %>%
    mutate(dep_basc_elec_cuisson = vol_basc_elec_cuisson * prix_Elec)
  menage_echelle$dep_non_elec <- rowSums(menage_echelle %>% select(all_of(list_source_usage_bascule)))

  #Calcul des soldes
  solde <-
    menage_echelle %>%
    mutate(solde = dep_basc_elec_ECS + dep_basc_elec_chauff + dep_basc_elec_cuisson - dep_non_elec) %>%
    #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en gaz,fuel, solides,etc
    select(ident_men, solde, bascule)
  solde$solde[which(solde$bascule == 0)] <- 0

  #Bascule des dépenses hors des énergies fossiles
  menage_echelle <-
    menage_echelle %>%
    mutate_when(bascule == 1,
                list(Gaz_ECS = 0,
                     GPL_ECS = 0,
                     Fuel_ECS = 0,
                     Solides_ECS = 0,
                     Urbain_ECS = 0,
                     Gaz_chauff = 0,
                     GPL_chauff = 0,
                     Fuel_chauff = 0,
                     Solides_chauff = 0,
                     Urbain_chauff = 0,
                     Gaz_Cuisson = 0,
                     GPL_Cuisson = 0,
                     Solides_Cuisson = 0,
                     Urbain_Cuisson = 0,
                     Fuel_Cuisson = 0,
                     Elec_chauff = Elec_chauff + dep_basc_elec_chauff,
                     Elec_ECS = Elec_ECS + dep_basc_elec_ECS,
                     Elec_Cuisson = Elec_Cuisson + dep_basc_elec_cuisson,
                     dep_Gaz = dep_Gaz - Gaz_ECS - Gaz_chauff - Gaz_Cuisson,
                     dep_GPL = dep_GPL - GPL_ECS - GPL_chauff - GPL_Cuisson,
                     dep_Fuel = dep_Fuel - Fuel_ECS - Fuel_chauff - Fuel_Cuisson,
                     dep_Solides = dep_Solides - Solides_ECS - Solides_chauff - Solides_Cuisson,
                     dep_Urbain = dep_Urbain - Urbain_ECS - Urbain_chauff - Urbain_Cuisson,
                     dep_Elec = dep_Elec + dep_basc_elec_chauff + dep_basc_elec_ECS + dep_basc_elec_cuisson))

  #Ajustement efficacité domicile
  dom_eff_DPE_vec <- dom_eff_df$Dom_eff[match(menage_echelle$DPE_horizon , dom_eff_df$classe)]
  #Pour chaque source_usage, on applique le gain d'efficacité dans le param dom_effic_source
  for(source_usage in list_source_usage){
    source_it <- strsplit(source_usage,"_")[[1]][1]
    dom_eff_coeff_source <- MatisseParams$dom_effic_source$eff_gain[which(MatisseParams$dom_effic_source$sources == source_it)]
    dom_eff_compo_vec <- (1 + dom_eff_DPE_vec) * (1 + dom_eff_coeff_source)
    if(source_usage %in% names(menage_echelle)){
      solde$solde <-  solde$solde + menage_echelle[,source_usage] * (dom_eff_compo_vec - 1)
      menage_echelle[,source_usage] <- menage_echelle[,source_usage] * dom_eff_compo_vec
    }
  }
  #On applique l'efficacité aux dépenses totales par source
  for(source_it in sources){
    menage_echelle[paste("dep",source_it,sep="_")] <- menage_echelle[paste("dep",source_it,sep="_")] * dom_eff_compo_vec
  }

  # Reventilation -----------------------------------------------------------
  solde <- solde %>% select(-bascule)
  menage_echelle <- menage_echelle %>% select(-paste("prix_",sources,sep = ""))
  menage_echelle <- ventilate_solde(menage_echelle, solde, FC , step = "REHAB")
  menage_ener_dom <- get_energie_dom_surf(menage_echelle, FC, F)
  menage_echelle <- menage_echelle %>%
    select(-ener_dom_surf, -ener_dom, -energie_tot_surf) %>%
    left_join(menage_ener_dom, by = "ident_men") %>%
    select(colnames(menage))

  for(i in 1:length(sources)){
    menage_echelle[ ,dep_sources_verif[i]] <- rowSums(menage_echelle %>%
                                                        select(all_of(list_source_usage)) %>%
                                                        select(starts_with(all_of(sources[i]))))
  }

  # Succes ------------------------------------------------------------------
  print("Step D / 4_bascule_domicile : SUCCESS")
  if(save_intermed_file){save(menage_echelle, file=MatisseFiles$menage_echelle_D4_rd)}
  return(menage_echelle)

}









