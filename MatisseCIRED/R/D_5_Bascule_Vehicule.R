
#' @title bascule_efficacite_vehicule
#' @description Function that handles the change of vehicules
#'
#' @param menage A menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_D5_rd
#'
#' @return A menage dataframe
#'
#' @examples
#' bascule_efficacite_vehicule(menage, FALSE)
#'
bascule_efficacite_vehicule <- function(menage, save_intermed_file = F){


  # Data ----------------------------------------------------------------------------------------------------------------------------------------------------
  #Données standard
  load(MatisseFiles$FC_2010_horizon_rd)
  library(plyr)
  sources <- get_sources()
  dep_sources <- paste("dep", sources, sep = "_")
  dep_sources_verif <- paste(dep_sources, "verif", sep = "_")
  list_source_usage <- get_list_source_usage()
  list_dep <- get_list_dep()

  #Données menag
  menage_echelle  <- menage

  ## Données bdf
  auto <- as.data.frame(suppressWarnings(read_excel(MatisseFiles$auto_bdf_xl)), stringsAsFactors = F)
  # Kilométrage effectué par le ménage tous véhicules confondus : km_auto est une donnée hebdomadaire
  # on corrige les données abberante de 99998 ou 99999 km en une semaine pour une véhicule
  auto <-
    auto %>%
    mutate(km_auto = ifelse(km_auto >= 99998, NA, km_auto))
  auto_elec <-
    auto %>%
    select(ident_men, carbu) %>%
    filter(carbu == 4) %>% # carbu 4 = electrique
    filter(ident_men %in% menage_echelle$ident_men) %>%
    left_join(menage_echelle %>% select(ident_men, pondmen), by = "ident_men")
  load(MatisseFiles$appariement_bdf_entd_rd)
  appariement_bdf_entd <-
    appariement_bdf_entd %>%
    dplyr::rename(percent_pkm_eligible=percent_W_mean_eligible)

  #Données véhicules 3ME
  load(MatisseFiles$Threeme_rd)
  data_veh_2010 <- get_3ME_vehicule_stats(2010)
  data_veh_horizon <- get_3ME_vehicule_stats(MatisseParams$horizon)


  # Calculs des potentiels d'électrification des véhicules --------------------------------------------------------------
  # On classe les ménage pour favoriser le remplacement des véhicules par de l'électrique en fonction de
  # MatisseParams$classement_veh (idem classement bascule domicile)

  # Pour chaque cellule de menage_echelle on attribue la valeur de percent_pkm_eligible de la cellule équivalente dans ENTD.
  menage_echelle$percent_pkm_eligible <- NA
  for (i in 1:nrow(appariement_bdf_entd)) {
    classe <- appariement_bdf_entd[i,]
    if (is.na(classe$typmen5)) {
      #Si classe$typmen5 est NA, la cellule est définie par tuu X quintile
      l_idx <- which(menage_echelle$quintileuc == classe$niveau_vie_quintile &
            menage_echelle$tuu == classe$tuu)
    }else{
      #Sinon définition par tuu x decile x typmen5
      l_idx <- which(menage_echelle$quintileuc == classe$niveau_vie_quintile &
                    menage_echelle$tuu == classe$tuu &
                    menage_echelle$typmen5 == classe$typmen5)
    }
    menage_echelle$percent_pkm_eligible[l_idx] <- classe$percent_pkm_eligible
  }

  # Le potentiel de transition au VE est calculé en absolu du point de vue aggrégé
  # comme la potentielle diminution de la consommation de carburant (directement
  # proportionnelle aux émissions) des trajets éligibles aux VE du ménage
  potent_idx <- which(!is.na(menage_echelle$nbvehic) &
                      !is.na(menage_echelle$percent_pkm_eligible) &
                      !is.na(menage_echelle$carb_lubr) &
                      menage_echelle$carb_lubr > 0 &
                      menage_echelle$nbvehic > 0)
  menage_echelle$potentiel_VE <- NA
  menage_echelle$potentiel_VE[potent_idx] <- menage_echelle$carb_lubr[potent_idx] *
                                        menage_echelle$percent_pkm_eligible[potent_idx]


  # Classement des ménages ----------------------------------------------------------------------------------------------------------------------------------------------
  # Classement des ménages possédant une voiture ou plus et des dépenses en carburant strictement positives.
  # on n'exclut pas les ménages achetant une auto en 2010 (prod_veh>0) pour la sélection 2010-(n-1). En revanche, nouveau classement à l'horizon n uniquement sur les ménages avec prod_veh>0 (exclus ceux qui ont déjà un VE pour respecter les trajectoires ADEME)
  # On prend les premiers rangs en premier

  #Init
  menage_echelle[,c("VE_rank_opt", "VE_rank_pess", "VE_rank_med", "VE_rank_rich", "VE_rank_poor")] <- NA

  #Optimiste : les plus gros potentiels sont favorisés
  menage_echelle$VE_rank_opt <- row_number(-menage_echelle$potentiel_VE)

  #Pessimiste : les plus petits potentiels sont favorisés
  menage_echelle$VE_rank_pess[potent_idx] <- max(menage_echelle$VE_rank_opt, na.rm = T) -
                                            menage_echelle$VE_rank_opt[potent_idx] + 1

  #Median
  menage_echelle$VE_rank_med <- menage_echelle$VE_rank_pess - menage_echelle$VE_rank_opt
  l_idx <- which(menage_echelle$VE_rank_med <= 0)
  menage_echelle$VE_rank_med[l_idx] <- -1 * menage_echelle$VE_rank_med[l_idx] + 1

  #Rich : favorise les ménages les plus haut en RDB/UC
  menage_echelle$VE_rank_rich[potent_idx] <- row_number(- menage_echelle$RDB[potent_idx] /
                                                          menage_echelle$coeffuc[potent_idx])

  #Poor : favorise les ménages les plus bas en RDB/UC
  menage_echelle$VE_rank_poor[potent_idx] <- max(menage_echelle$VE_rank_rich, na.rm = T) -
                                                menage_echelle$VE_rank_rich[potent_idx] + 1

  #Selection de la variable à conserver pour le classement
  if(str_detect(MatisseParams$classement_veh,"Optimal_ener")){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_opt)
  }
  if(str_detect(MatisseParams$classement_veh,"Pess_ener")){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_opt)
  }
  if(str_detect(MatisseParams$classement_veh,"Med_ener")){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_opt)
  }
  if(str_detect(MatisseParams$classement_veh,"Optimal_co2")){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_opt)
  }
  if(str_detect(MatisseParams$classement_veh,"Pessimiste")){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_pess)
  }
  if(str_detect(MatisseParams$classement_veh,"Optimiste")){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_opt)
  }
  if(MatisseParams$classement_veh=="Median"){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_med)
  }
  if(MatisseParams$classement_veh=="Rich"){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_rich)
  }
  if(MatisseParams$classement_veh=="Poor"){
    menage_echelle <- menage_echelle %>% mutate(VE_rank = VE_rank_poor)
  }


  # Recvoi_d ----------------------------------------------------------------
  #Pour tous les ménages classés, on indique le montant actuel du remboursement de l'emprunt au cours des 12 derniers mois pour le dernier véhicule acheté par le ménage
  #pour chaque ménage on sélectionne le véhicule le plus récent, on indique si oui ou non le ménage a un remboursement en cours
  # RECVOI Année d'achat du véhicule
  # MCREVOI_D Montant définitif remboursé du crédit pour le véhicule au cours des 12 derniers mois
  # KM_AUTO Km parcourus pendant la semaine de tenue des carnets

  # Renvoie l'identifiant, l'année d'achat et le montant du crédit remboursé sur l'année pour le véhicule le plus récent de chaque ménage
  max_recvoi <-
    plyr::ddply(auto %>% select(ident_men, recvoi, mcrevoi_d), .(ident_men), function(x)
      x[which.max(x$recvoi), ])
  km_auto_sum <-
    plyr::ddply(auto %>% select(ident_men, km_auto), .(ident_men), function(x)
      sum(x$km_auto) * 52)
  colnames(km_auto_sum) <- c("ident_men", "km_auto")

  menage_echelle <- menage_echelle %>% left_join(max_recvoi %>% select(ident_men, mcrevoi_d), by = "ident_men")
  menage_echelle <- replace_na(menage_echelle, replace = list(mcrevoi_d = 0))
  menage_echelle <- menage_echelle %>% left_join(km_auto_sum, by = "ident_men")
  # Estimation à partir des données moyennes pour les ménages où km_auto est manquant
  l_idx <- which(is.na(menage_echelle$km_auto))
  menage_echelle$km_auto[l_idx] <-  data_veh_2010$km_parcourus_average * menage_echelle$nbvehic[l_idx]


  # Achat VE 2010 -------------------------------------------------------------------------------------------------------------------------------------------
  # Les VE déjà achetés dans BDF sont comptés dans les achats de 2010
  menage_echelle$VE <- FALSE
  menage_echelle$new_VE <- FALSE
  menage_echelle$year_VE <- 0
  menage_echelle$year_VE_sauv <- ""
  VE_idx <- which(menage_echelle$ident_men %in% auto_elec$ident_men)
  menage_echelle$VE[VE_idx] <- TRUE
  menage_echelle$year_VE[VE_idx] <- 2010
  menage_echelle$VE_rank[VE_idx] <- 0
  menage_echelle$year_VE_sauv[VE_idx] <- "2010"
  #On exclut les ménages non solvables
  menage_echelle$VE_rank[which(menage_echelle$solv > 0.328)] <- 0


  # Bascule VE 2011 à l'horizon -----------------------------------------------------------------------------------------------------------------------------

  IM <- c()
  #Classement par lequel on va commencer
  # on s'assure que le classement i est bien présent dans la base et qu'on n'a pas dépassé le maximum
  i <- 1
  while (!i %in% menage_echelle$VE_rank &
         i < max(menage_echelle$VE_rank, na.rm = T)) {i = i + 1}

  menage_echelle[,c("solde_carb", "solde_elec", "solde_dette", "solde_veh",
                    "solde_malus", "solde_rev_capital", "solde_int", "solde_princ")] <- 0


  menages_insolvables_suppr=c()



  for (Y in 2011:MatisseParams$horizon){
    sauv_menage_echelle_annee_precedente<-menage_echelle
    menage_echelle$new_VE <- FALSE
    data_veh_inter <- get_3ME_vehicule_stats(Y)

    #Ajustement pour aligner les chiffres de ventes de véhicules totales 3ME - BDD, uniquement pour l'horizon
    coeff_adj_nb_veh_tot <- as.numeric(menage_echelle %>%
                                         filter(prod_veh > 5 * 10 ^ (3) & carb_lubr > 0) %>%
                                         summarize(sum(pondmen)))
    TOT_VE_nv_adj <-  coeff_adj_nb_veh_tot * data_veh_inter$total_veh_elec_nv /
                      (data_veh_inter$total_veh_elec_nv + data_veh_inter$total_veh_th_nv)
    TOT_VTH_nv_adj <- coeff_adj_nb_veh_tot * data_veh_inter$total_veh_th_nv  /
                      (data_veh_inter$total_veh_elec_nv + data_veh_inter$total_veh_th_nv)

    #Ajustement pour tenir compte de la croissance de la population entre 2010 et horizon
    POP_y <- as.numeric(ThreeME %>%
                        filter(Var == "POP_TOT") %>%
                        filter(year == Y) %>%
                        select(value))
    POP_init <- as.numeric(ThreeME %>%
                           filter(Var == "POP_TOT") %>%
                           filter(year == 2010) %>%
                           select(value))
    if (Y == MatisseParams$horizon) {
      TOT_VE_nv_correct <- TOT_VE_nv_adj
    } else{
      TOT_VE_nv_correct <-  data_veh_inter$total_veh_elec_nv / POP_y * POP_init
    }

    while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T) ){i=i+1}

      ## RENOUVELLEMENT
      #on renouvelle tout d'abord le stock de VE achetés 13 ans plus tôt, à partir de 2023, qui est la 14e année après l'achat d'un VE en 2010
      # (stock suffisant, vérif dans Divers/2020-07-13 Verif renouvellement stock VE.R)
      if(Y>2022){
        l_idx <- which(menage_echelle$year_VE == Y - 13)
        menage_echelle$year_VE[l_idx] <- Y
        menage_echelle$new_VE[l_idx] <- TRUE
      }

      # Somme des achats de VE par renouvellement ou stock préalable (si Y=2010)
      sum_ve_nv <- menage_echelle %>% filter(year_VE == Y) %>% summarise(sum(pondmen))
      if(is.na(sum_ve_nv)){sum_ve_nv <- 0}

      # La trajectoire de ThreeME prévoit de plus en plus de ventes de VE, le renouvellement de la flotte ne saurait entièrement occuper les ventes de l'année Y
      # while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T) ){i=i+1}
      # Condition : pas encore assez de ventes de VE et encore des ménages éligibles

      ## SELECTION
      while (sum_ve_nv < TOT_VE_nv_correct & i < max(menage_echelle$VE_rank, na.rm = T)){

        ###
        # Cas 1
        ####
        # DESCRIPTION :
        # Pour les ménages qu'on sélectionne entre 6 et 13 ans
        # => emprunt remboursé par hypothèse
        # ils doivent avoir fini de rembourser leur éventuel crédit
        # ou reconstitué leur épargne, on enlève donc les ménages où crevoi==1
        # si horizon 2035 => les ménages achetant entre 2010 et 2016
        # ne doivent pas avoir d'emprunt en cours
        ####
        if (Y > MatisseParams$horizon - 13 & Y <= MatisseParams$horizon - 6 || Y > MatisseParams$horizon - 13 - 13 & Y <= MatisseParams$horizon - 6 - 13) {
          # Ces ménages n'ont pas de rembousement en cours

          # Passer à i+1 tant que [OU (i n'est pas classé et inférieur au max) OU (le ménage i a des remboursement en cours) OU (le ménage achète un veh à l'horizon)]
          while (!i %in% menage_echelle$VE_rank  &
                  i < max(menage_echelle$VE_rank, na.rm = T) ||
                 menage_echelle %>% filter(VE_rank == i) %>% select(mcrevoi_d) > 0 ||
                 menage_echelle %>% filter(VE_rank == i) %>% select(prod_veh) >= 5 * 10 ^ (3)) {
            i = i + 1
          }

          # Selection du ménage im (identifiant_menage)
          if (menage_echelle %>% filter(VE_rank == i) %>% select(mcrevoi_d) ==
              0) {
            #si on sorti du while en sortant du classement par le max
            im <-
              as.numeric(menage_echelle %>% filter(VE_rank == i)  %>% select(ident_men))
          } else{
            im <- NA
          }
        }

        ###
        # Cas 2
        ####
        # DESCRIPTION :
        # Horizon et horizon-13
        # à horizon-13 on sélectionne des ménages qui seront de facto sélectionné
        # pour l'horizon, ils doivent avoir des dépenses non nulles de veh dans
        # leur budget (On considère dep non nulles à partir de 5000 euros ;
        # par exemple Twizzy = 7500euros (bonus non inclus))
        ####
        if (Y == MatisseParams$horizon - 13 || Y == MatisseParams$horizon) {
          while (!i %in% menage_echelle$VE_rank  &
                    i < max(menage_echelle$VE_rank, na.rm = T) ||
                    menage_echelle %>% filter(VE_rank == i) %>% select(prod_veh) < 5 * 10^3) {
            i = i + 1
          }
          if (menage_echelle %>% filter(VE_rank == i) %>% select(prod_veh) >= 5 * 10^3) {
            im <- as.numeric(menage_echelle %>% filter(VE_rank == i)  %>% select(ident_men))
          } else{
            im <- NA
          }
        }

        ###
        # Cas 3
        ####
        # DESCRIPTION :
        # Pas d'achet de véhicule à l'horizon
        # ménages entre (horizon-6)-13 horizon-13
        # ménages entre horizon-6 et horizon exclus
        ####
        if ((Y < MatisseParams$horizon - 13 & Y > (MatisseParams$horizon - 6) - 13) || (Y > MatisseParams$horizon - 6 & Y < MatisseParams$horizon)) {
          while (!i %in% menage_echelle$VE_rank  &
                  i < max(menage_echelle$VE_rank, na.rm = T) ||
                  menage_echelle %>% filter(VE_rank == i) %>% select(prod_veh) >= 5 * 10^3) {
            i = i + 1
          }
          if (menage_echelle %>% filter(VE_rank == i) %>% select(prod_veh) <
              5 * 10 ^ (3)) {
            im <- as.numeric(menage_echelle %>% filter(VE_rank == i) %>% select(ident_men))
          } else{
            im = NA
          }
        }
        ###### Fin des Cas de sélection

        ## Somme des ventes
        # si im=NA c'est qu'aucun ménage en remplissait les conditions
        if (!is.na(im)) {
          sum_ve_nv = sum_ve_nv + as.numeric(menage_echelle %>% filter(ident_men == im) %>% select(pondmen))
          IM <- c(IM, im)
          l_idx <- which(menage_echelle$ident_men == im)
          menage_echelle$VE[l_idx] = TRUE
          menage_echelle$new_VE[l_idx] = TRUE
          menage_echelle$year_VE[l_idx] = Y
          menage_echelle$year_VE_sauv[l_idx] = paste(menage_echelle$year_VE_sauv[l_idx], Y, sep = "_")
          menage_echelle$VE_rank[l_idx] = 0
        }
        #Recherche du prochain ménage
        while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T)){i=i+1}
      } ##### Fin selection ménages



  # Budgets -----------------------------------------------------------------
      ####
      # Cas 1
      ####
      # DESCRIPTION
      #Ces ménages n'ont pas de remboursement en cours, selon les hypothèses de
      # ThreeME, ces ménages ont déjà fini de rembourser leur épargne ou leur dette
      # les modifications de budgets sont le carburant et l'électricité.
      ####
      if (Y > MatisseParams$horizon - 13 & Y <= MatisseParams$horizon - 6) {
        # Elec et carburant
        # Ménages avec plusieurs véhicules : seulement la partie éligible est basculée vers l'élec
        new_VE_idx <- which(menage_echelle$new_VE &
                        menage_echelle$nbvehic > 1)
        menage_echelle$solde_carb[new_VE_idx] <- - 1 * menage_echelle$carb_lubr[new_VE_idx] *
                                                    menage_echelle$percent_pkm_eligible[new_VE_idx]

        # Ménages avec un seul véhicule : tous les trajets sont basculés vers l'élec
        new_VE_idx <- which(menage_echelle$new_VE &
                              menage_echelle$nbvehic == 1)
        menage_echelle$solde_carb[new_VE_idx] <- - 1 * menage_echelle$carb_lubr[new_VE_idx]


        #Tous ménages avec nouveau VE : ajustement des soldes et dépenses
        new_VE_idx <- which(menage_echelle$new_VE)
        menage_echelle$solde_elec[new_VE_idx] <- menage_echelle$km_auto[new_VE_idx] *
                                                menage_echelle$percent_pkm_eligible[new_VE_idx] *
                                                data_veh_inter$cout_VE_km
        menage_echelle$dep_Elec[new_VE_idx] <- menage_echelle$dep_Elec[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]
        menage_echelle$dep_Elec_verif[new_VE_idx] <- menage_echelle$dep_Elec_verif[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]
        menage_echelle$Elec_ElecSpe[new_VE_idx] <- menage_echelle$Elec_ElecSpe[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]
      }


      ####
      # Commun Cas 3 et 2
      ####
      if (Y > MatisseParams$horizon - 6) {
        repayment_VE <- as.numeric(
          calcul_interet_principal(
            loan = data_veh_inter$prix_veh_elec,
            n = 6,
            year_purchase = Y,
            horizon = MatisseParams$horizon,
            i = data_veh_inter$taux_interet,
            pf = 1))

        repayment_VTH <- as.numeric(
          calcul_interet_principal(
            loan = data_veh_inter$prix_veh_th,
            n = 6,
            year_purchase = Y,
            horizon = MatisseParams$horizon,
            i = data_veh_inter$taux_interet,
            pf = 1))
        #Intérêts sur annuités totales (intérêt + remboursement du principal)
        ratio_int_over_annuite <- repayment_VTH[1] / sum(repayment_VTH)
      }


      ###
      # Cas 3
      ####
      if (Y > MatisseParams$horizon - 6 & Y < MatisseParams$horizon) {
        # Elec et carburant
        # Ménages avec plusieurs véhicules : seulement la partie éligible est basculée vers l'élec
        new_VE_idx <- which(menage_echelle$new_VE &
                              menage_echelle$nbvehic > 1)
        menage_echelle$solde_carb[new_VE_idx] <- - 1 * menage_echelle$carb_lubr[new_VE_idx] *
                                                  menage_echelle$percent_pkm_eligible[new_VE_idx]

        # Ménages avec un seul véhicule : tous les trajets sont basculés vers l'élec
        new_VE_idx <- which(menage_echelle$new_VE &
                              menage_echelle$nbvehic == 1)
        menage_echelle$solde_carb[new_VE_idx] <- - 1 * menage_echelle$carb_lubr[new_VE_idx]


        #Tous ménages avec nouveau VE : ajustement des soldes et dépenses
        new_VE_idx <- which(menage_echelle$new_VE)
        menage_echelle$solde_elec[new_VE_idx] <- menage_echelle$km_auto[new_VE_idx] *
                                                  menage_echelle$percent_pkm_eligible[new_VE_idx] *
                                                  data_veh_inter$cout_VE_km
        menage_echelle$dep_Elec[new_VE_idx] <- menage_echelle$dep_Elec[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]
        menage_echelle$dep_Elec_verif[new_VE_idx] <- menage_echelle$dep_Elec_verif[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]
        menage_echelle$Elec_ElecSpe[new_VE_idx] <- menage_echelle$Elec_ElecSpe[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]

        # Gestion de la dette pour les ménages ayant une dette en cours
        # Maj du remboursement et ventilation entre remboursement du principal et intérêt
        new_VE_idx <- which(menage_echelle$new_VE &
                            menage_echelle$mcrevoi_d > 0)

        menage_echelle$solde_int[new_VE_idx] <- menage_echelle$mcrevoi_d[new_VE_idx] *
                                                (data_veh_inter$prix_veh_elec / data_veh_inter$prix_veh_th) *
                                                ratio_int_over_annuite
        menage_echelle$solde_princ[new_VE_idx] <- menage_echelle$mcrevoi_d[new_VE_idx] *
                                                (data_veh_inter$prix_veh_elec / data_veh_inter$prix_veh_th) *
                                                (1 - ratio_int_over_annuite)

        # Cas des ménages ayant payé le véhicule par désépargne
        # Il faut donc augmenter le montant désépargné des revenus du capital. Hypothèse : prix moyen du VE
        new_VE_idx <- which(menage_echelle$new_VE &
                            is.na(menage_echelle$mcrevoi_d))
        menage_echelle$solde_rev_capital[new_VE_idx] <- sum(repayment_VE)
      }

      ###
      # Cas 2
      ####
      if (Y == MatisseParams$horizon) {
        # Elec et carburant : A l'horizon, les gains sont divisés par deux, pour tenir compte de la demi-année de l'achat
        # Ménages avec plusieurs véhicules : seulement la partie éligible est basculée vers l'élec
        new_VE_idx <- which(menage_echelle$new_VE &
                              menage_echelle$nbvehic > 1)
        menage_echelle$solde_carb[new_VE_idx] <- - 1 * menage_echelle$carb_lubr[new_VE_idx] *
                                                  menage_echelle$percent_pkm_eligible[new_VE_idx] * 1/2

        # Ménages avec un seul véhicule : tous les trajets sont basculés vers l'élec
        new_VE_idx <- which(menage_echelle$new_VE &
                              menage_echelle$nbvehic == 1)
        menage_echelle$solde_carb[new_VE_idx] <- - 1 * menage_echelle$carb_lubr[new_VE_idx] * 1/2


        #Tous ménages avec nouveau VE : ajustement des soldes et dépenses
        new_VE_idx <- which(menage_echelle$new_VE)
        menage_echelle$solde_elec[new_VE_idx] <- menage_echelle$km_auto[new_VE_idx] *
                                                  menage_echelle$percent_pkm_eligible[new_VE_idx] *
                                                  data_veh_inter$cout_VE_km * 1/2
        menage_echelle$dep_Elec[new_VE_idx] <- menage_echelle$dep_Elec[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]
        menage_echelle$dep_Elec_verif[new_VE_idx] <- menage_echelle$dep_Elec_verif[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]
        menage_echelle$Elec_ElecSpe[new_VE_idx] <- menage_echelle$Elec_ElecSpe[new_VE_idx] + menage_echelle$solde_elec[new_VE_idx]
        #Effet bonus-malus
        menage_echelle$solde_veh[new_VE_idx] <-  menage_echelle$prod_veh[new_VE_idx] *
                                                (1 + data_veh_2010$bonus_malus_rel) *
                                                (data_veh_inter$prix_veh_elec / data_veh_inter$prix_veh_th - 1)

        # Gestion de la dette pour les ménages ayant une dette en cours
        # Maj du remboursement et ventilation entre remboursement du principal et intérêt
        new_VE_idx <- which(menage_echelle$new_VE &
                              menage_echelle$mcrevoi_d > 0)
        menage_echelle$solde_int[new_VE_idx] <- menage_echelle$mcrevoi_d[new_VE_idx] *
                                                (data_veh_inter$prix_veh_elec / data_veh_inter$prix_veh_th) *
                                                ratio_int_over_annuite * 1/2
        menage_echelle$solde_princ[new_VE_idx] <- menage_echelle$mcrevoi_d[new_VE_idx] *
                                                  (data_veh_inter$prix_veh_elec / data_veh_inter$prix_veh_th) *
                                                  (1 - ratio_int_over_annuite) * 1/2
        # Cas des ménages ayant payé le véhicule par désépargne
        # Il faut donc augmenter le montant désépargné des revenus du capital. Hypothèse : prix moyen du VE
        new_VE_idx <- which(menage_echelle$new_VE &
                              is.na(menage_echelle$mcrevoi_d))
        menage_echelle$solde_rev_capital[new_VE_idx] <- sum(repayment_VE)
      }


  # Solvabilité ex-post -----------------------------------------------------
      # NB : on repart de la solvabilité calculée en step 3.1
      # calcul solvabilité
      menage_echelle <- menage_echelle %>% mutate(solv = ifelse(new_VE == T, solv + (solde_int + solde_princ) / RDB, solv))

      # Les ménages insolvables après la transition sont écartés
      menages_insolvables <- menage_echelle %>% filter(new_VE & solv > 0.33) %>% select(year_VE,ident_men,solv)

      sauv_menages_insolvables <- sauv_menage_echelle_annee_precedente %>% filter(ident_men %in% menages_insolvables$ident_men)

      menages_insolvables_suppr <- rbind(menages_insolvables_suppr,menages_insolvables)

      # les ménages insolvables sont "rebootés" à l'itération précédente, avant leur rénovation
      if(dim(menages_insolvables)[1]>0){
        menage_echelle <-
          rbind(
            menage_echelle %>% filter(!ident_men %in% menages_insolvables$ident_men),
            sauv_menages_insolvables
          ) %>% arrange(ident_men)
      }


      # On reprend à 1 ou plus parce qu'à moins de 6 ans de l'horizon on peut sélectionner les ménages ayant un crédit en cours => on parcourt de nouveau tous les ménages
      i <- min(menage_echelle %>% filter(VE_rank > 0) %>% select(VE_rank))

  } # Année suivante

  # Amélioration de la consommation des veh thermiques ----------------------
  # Efficacité des moteurs => diminution homothétique de la consommation en carburant restante de tous les ménages
  # Diminution des usages (télétravail et voirie)
  forcage_vkm <- read_excel(path = MatisseFiles$forcage_km_xl, sheet = "value")
  gain_vkm <- as.numeric(forcage_vkm %>% filter(year == MatisseParams$horizon) %>% select(gain_vkm))
  gain_efficacite <- as.numeric(forcage_vkm %>% filter(year == MatisseParams$horizon) %>% select(gain_efficacite))

  #Malus pour les VT
  # attention malus BM_rel<0 pour indiquer un surcoût, on rajoute un moins
  l_idx <- which(menage_echelle$prod_veh > 5 * 10^3)
  # Solde_malus positif => va venir diminuer les autres consommations
  menage_echelle$solde_malus[l_idx] <-
    menage_echelle$prod_veh[l_idx] *
    (1 + data_veh_2010$bonus_malus_rel) *
    (-1 * data_veh_horizon$bonus_malus_rel)
  # On ne rajoute pas le bonus des VE, déjà intégré dans les prix de ThreeME
  menage_echelle$solde_malus[which(menage_echelle$new_VE)] <- 0

  # Variable New_VT
  menage_echelle$new_VT <- FALSE
  new_VT_idx <- which(menage_echelle$prod_veh > 5 * 10^3 &
                        menage_echelle$carb_lubr > 0 &
                        menage_echelle$new_VE == FALSE)
  menage_echelle$new_VT[new_VT_idx] <- TRUE

  # Créer les soldes budgétaires
  menage_echelle <-
    menage_echelle %>%
    mutate(carb_lubr = carb_lubr + solde_carb)
  menage_echelle <-
    menage_echelle %>%
    mutate(
      solde_carb = solde_carb + carb_lubr * (1 - (1 + gain_vkm) *  (1 + MatisseParams$veh_effic_VT)),
      carb_lubr = carb_lubr * (1 + gain_vkm) *  (1 + MatisseParams$veh_effic_VT),
      prod_veh = prod_veh + solde_veh + solde_malus,
      autres_services = autres_services + solde_int,
      Hors_budget = Hors_budget + solde_princ,
      rev_patrimoine = rev_patrimoine + solde_rev_capital,
      RDB = RDB + solde_rev_capital
    )

  # Reventilation -----------------------------------------------------------
  solde <-
    menage_echelle %>%
    mutate(solde = solde_elec + solde_carb + solde_rev_capital + solde_int + solde_malus) %>%
    select(ident_men, solde)
  menage_echelle <- ventilate_solde(menage_echelle, solde, FC , step = "VE")

# Evolution des dépenses energétiques : source_usage ---------------------------------------
  for(i in 1:length(sources)){
    sub_source_usage <- list_source_usage[grep(sources[i], list_source_usage)]
    for(source_usage in sub_source_usage){
      # Pour les ménages ayant des dépenses non nulles, on ajuste pour chaque source_usage les dépenses par le ratio des
      # dépenses avant et après la modification des usages
      l_idx <- which(menage_echelle[, dep_sources_verif[i]] > 0)
      menage_echelle[l_idx, source_usage] <-
        menage_echelle[l_idx, source_usage] *
        menage_echelle[l_idx, dep_sources[i]] /
        menage_echelle[l_idx, dep_sources_verif[i]]
      l_idx <- setdiff(1:nrow(menage_echelle), l_idx)
      menage_echelle[l_idx, source_usage] <- 0
    }
    menage_echelle[dep_sources_verif[i]] <-
      rowSums(menage_echelle %>% select(all_of(sub_source_usage)))
  }


# Mise à jour énergie domicile ----------------------------------------------------------------------------------------------------------------------------
  menage_ener_dom <- get_energie_dom_surf(menage_echelle, FC, F)
  menage_echelle <-
    menage_echelle %>%
    select(-ener_dom_surf, -ener_dom, -energie_tot_surf) %>%
    left_join(menage_ener_dom, by = "ident_men")


 # Succes --------------------------------------------------------------------------------------------------------------------------------------------------
  menage_echelle <- menage_echelle %>%
    select(-typmen5,-percent_pkm_eligible, -potentiel_VE, -VE_rank_pess, -VE_rank_opt, -VE_rank, -solde_dette, -solde_elec, -solde_carb, -solde_veh)
  print("Step D / 5_bascule_vehicules : SUCCESS")
  if(save_intermed_file){save(menage_echelle,file = MatisseFiles$menage_echelle_D5_rd)}
  return(menage_echelle)

}






