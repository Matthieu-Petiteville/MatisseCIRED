#Including enertot change

#' @title rehabilitation_inter
#' @description Function that handles the rehabilitation of houses between the start and the horizon
#'
#' @param menage A menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_D3_rd
#'
#' @return A menage dataframe
#'
#' @examples
#' rehabilitation_inter(menage, FALSE)
#'
rehabilitation_inter <- function(menage, save_intermed_file = F){

# Data --------------------------------------------------------------------
  TCO <- get_TCO(MatisseParams$horizon)
  load(MatisseFiles$FC_2010_horizon_rd)

  #Données standard
  list_source_usage <- get_list_source_usage()
  list_dep <- get_list_dep()
  sources <- get_sources()
  dep_sources <- paste("dep", sources, sep = "_")
  dep_sources_verif <- paste(dep_sources, "verif", sep="_")
  list_dep <- get_list_dep()

  #Load data
  load(MatisseFiles$Threeme_rd)
  load(MatisseFiles$coeff_ems_2010_rd)
  coeff_dep_ems <- suppressMessages(read_csv(MatisseFiles$coeff_dep_ems_csv))

# Preparation -------------------------------------------------------------
  menage_echelle <- menage

  # Recoder variable : Stalog et Propri : statut des ménages vs. logements, donc possibilité de faire des réhab
  menage_echelle$stalog_bis <- car::recode(menage_echelle$stalog, " 1:2 =1 ; 3=3 ; 4:5= 2 ; 6=3")
  menage_echelle$propri_bis <- car::recode(menage_echelle$propri, " 1 =3 ; 2=2 ; 3:4= 3 ; 5:6=1; 7=3")
  menage_echelle$stalog_propri <- paste(menage_echelle$stalog_bis, menage_echelle$propri_bis, sep="")
  menage_echelle$stalog_propri[which(is.na(menage_echelle$propri))] <- menage_echelle$stalog_bis[which(is.na(menage_echelle$propri))]

  ## Hypothèse : rénovation prioritaire selon statut logement
  # priority order for rehabilitation :
  ## 1 : propriétaires
  ## 22 : locataires HLM
  ## 21 : locataires bailleurs privés
  ## 23 : locataires bailleurs autres
  ## 2 : locataire bailleur inconnu
  ## 3 : autre statut : usufruit, logé à titré gratuit
  priority <- c(1, 22, 21, 23, 2, 3)

  # solvabilité des ménages
  menage_echelle <-
    menage_echelle %>% mutate(solv = ifelse(RDB == 0, 999, (c13511 + c13221 + c13211) / RDB))

  # Selection ménages -------------------------------------------------------

  # Les ménages ayant acheté des logements neufs sont soit à exclure, soit exclut d'office car étant de classe A.
  # En fonction de l'hypothèse de classe DPE des nouvelles constructions. Le cas ici.
  menage_echelle <-
    menage_echelle %>%
    mutate(DPE_stalog_propri = paste(DPE_dep, stalog_propri, sep = "_")) %>%
    mutate(
      exclus = FALSE,
      REHAB = FALSE,
      year_rehab = 0,
      solde_dette_BP = 0,
      hausse_loyer = 0,
      solde_loyer = 0,
      hausse_loyer_sum = 0
    )
  # On exclut les ménages qui ont acheté du neuf
  menage_echelle$exclus[which(menage_echelle$year_neuf > 0)] <- TRUE
  menage_echelle$exclus[which(menage_echelle$c13711 > 10000)] <- TRUE
  menage_echelle$classe_arr[which(menage_echelle$year_neuf == 0)] <- menage_echelle$DPE_dep[which(menage_echelle$year_neuf == 0)]
  # On exclut les ménages propriétaires non solvables
  solv_idx <- which(menage_echelle$solv > 0.297 & menage_echelle$stalog <= 2)
  menage_echelle$exclus[solv_idx] <- TRUE
  # On ajoute les données de solvabilité
  menage_echelle <-
    menage_echelle %>%
    mutate(
      solde_int = 0,
      solde_ener = 0,
      principal_dette = 0,
      solde_princ = 0,
      subvention = 0,
      count_rehab = "")

  Cout_bailleur_prive=c()
  Dette_bailleur_prive=c()
  Solde_Ener_tot=c()
  table_solv_year=c()
  table_solv_year_ind=c()
  menages_insolvables_suppr=c()

# Loop on years -----------------------------------------------------------
#important pour que les ménages puissent faire plusieurs REHAB
# va surtout avantager les ménages locataires pour qui la solvabilité n'est pas un obstacle (ne portent pas le coût de la réno)


  for (Y in 2010:MatisseParams$horizon){
    print(Y)
    sauv_menage_echelle_annee_precedente <- menage_echelle

    # Données 3ME
    # taux de remboursement des rénovations énergétiques
    R_RMBS_NEWBUIL_H01_CA<-as.numeric(
      ThreeME %>%
        filter(Var == "R_RMBS_REHAB_H01_CB") %>%
        filter(year == all_of(Y)) %>%
        select(value))

    # Taux d'intérêts des emprunts liés aux travaux de réhabilitation des logements en %
    R_I_REHAB_H01_CG_2  <-  as.numeric(
      ThreeME %>%
        filter(Var == "R_I_REHAB_H01_CG_2") %>%
        filter(year == all_of(Y)) %>%
        select(value))

    # Matrice des coûts (année Y) pour travaux de rénovation énergétiques en volume par saut de classe (en M2)
    Cost_m2 <- data.frame()
    for (dep in LETTERS[1:7]){
      for (arr in LETTERS[1:7]){
        if(dep>arr){

          #extraction stock m2 passant de M à L en 2010 (ThreeME)
          stock_m2 <- as.numeric(
            ThreeME %>%
              filter(Var == paste("REHAB_H01_C", dep, "_C", arr, "_2", sep = "")) %>%
              filter(year == all_of(Y)) %>%
              select(value))

          #extraction coût des travaux pour passer de M à L en 2010 (ThreeME) en M€
          stock_euros <- as.numeric(
            ThreeME %>%
              filter(Var == paste("PREHAB_H01_C", dep, "_C", arr, "_2*", "REHAB_H01_C", dep, "_C", arr, "_2", sep="")) %>%
              filter(year == all_of(Y)) %>%
              select(value))

          #   stock_euros/stock_m2 = coût de la réhabiliation par m2 (en €/m2)
          # Création matrice Cost_m2 :
          #   DPE_départ | DPE_arrivée | coût_m2 | coût_total_transition | m2_total_transition
          Cost_m2 <- rbind(Cost_m2,
                           data.frame(classe_dep = dep,
                                      classe_arr = arr,
                                      cost_m2 = as.numeric(stock_euros / stock_m2 * (10 ^ 6)),
                                      transition_tot_Meuros = as.numeric(stock_euros),
                                      transition_tot_m2 = as.numeric(stock_m2),
                                      stringsAsFactors=F))
        }
      }
    }


    # Matrice des gains énergétiques (pour année Y)
    conso_moy_dep <- as.data.frame(array2list(LETTERS[1:7]))
    for (i in LETTERS[1:7]){
      conso_moy_dep[i] <- as.numeric(ThreeME %>%
                                       filter(Var == paste("ENER_BUIL_H01_C",i,"_2*11630/BUIL_H01_C",i,"_2",sep="")) %>%
                                       filter(year == all_of(Y)) %>%
                                       select(value))
    }
    Mat_gain_ener <- get_gain_energie(conso_moy_dep)

    # DPE_STALOG_PROPRI : saut de classe
    order <- paste(rep(LETTERS[1:7], each = 6), rep(priority, 7), sep = "_")
    order_value <- rep(0, length(order))

    # order_value = longueur de chaque classe DPE_stalog_propri pour pouvoir incrémenter le classement médian
    for (i in 1:length(order)) {
      table_order_value <- menage_echelle %>% filter(DPE_stalog_propri == order[i])
      order_value[i] <- ifelse(is.null(dim(table_order_value)),
                               0,
                               length(table_order_value$DPE_stalog_propri))
    }
    order_value <- as.numeric(replace_na(order_value, 0))
    order_ter <- order_value

    # Pour chaque DPE_stalog_propri
    # On va classer par la suite les ménages par DPE, au sein de chaque DPE, classement optimiste et pessimiste en classant par DPE_stalog_propri et par conso d'énergie.
    # On passe d'un classement au sein de chaque classe DPE_stalog_propri => on passe à un classement par DPE en respectant la priorité stalog_propri.
    # 6 DPE et 6 stalog_propri
    for (j in 0:6) {
      # on parcourt tous les DPE
      for (i in 2:6) {
        # on parcourt les 6 catégories stalog_propri
        a <- 6 * j + 1
        b <- 6 * j + i - 1
        # pas la peine d'aller jusqu'à 6*j+i, on veut le seuil des 5 premiers stalog_propri pour l'ajouter au 6e
        order_ter[6 * j + i] <- sum(order_value[a:b])
      }
      order_ter[6 * j + 1] <- 0
    }
    order <- data.frame(DPE_stalog_propri = as.character(order),
                        rank_add = order_ter)
    menage_echelle <- menage_echelle %>% left_join(order, by = "DPE_stalog_propri")
    menage_echelle$ems_tot_chauff_ecs <- get_emissions(menage_echelle, FC)

    # Classement des menages

    # Optimiste : on classe les ménages pour chaque DPE par ordre décroissant de consommation énergétique pour
    #le logement en MWh (absolu => maximisation du gain agrégé)
    menage_echelle <-
      menage_echelle %>%
      group_by(DPE_stalog_propri) %>%
      dplyr::mutate(kWh_rank_opt = row_number(-ener_dom)) %>%
      ungroup()

    # Pessimiste : on classe les ménages par ordre croissant de conso éner pour le logement, les premiers seront les derniers
    menage_echelle <-
      menage_echelle %>%
      group_by(DPE_stalog_propri) %>%
      dplyr::mutate(kWh_rank_pess = max(kWh_rank_opt, na.rm = T) - kWh_rank_opt + 1) %>%
      ungroup()

    # Median : on prend le milieu de kwh_rank_opt et kWh_rank_pess
    menage_echelle <-
      menage_echelle %>%
      group_by(DPE_stalog_propri) %>%
      dplyr::mutate(kWh_rank_med = kWh_rank_pess - kWh_rank_opt) %>%
      mutate_when(kWh_rank_med <= 0, list(kWh_rank_med = -kWh_rank_med + 1)) %>%
      ungroup()

    # Opt_Ener : classement par énergie domestique surfacique décroissante : les plus consommateurs par m² en premier
    menage_echelle<-
      menage_echelle %>%
      mutate(ener_dom_surf=ifelse(is.infinite(ener_dom_surf),0,ener_dom_surf))%>%
      group_by(DPE_stalog_propri) %>%
      dplyr::mutate(kWh_rank_opt_ener =row_number(-ener_dom_surf)) %>%
      ungroup()

    # Pess_ener : classement par énergie domestique surfacique croissant : les plus sobres par m² en premier
    menage_echelle<-
      menage_echelle %>%
      group_by(DPE_stalog_propri) %>%
      dplyr::mutate(kWh_rank_pess_ener =max(kWh_rank_opt_ener,na.rm=T)-kWh_rank_opt_ener+1) %>%
      ungroup()

    # Med_ener : classement médian par consommations domestiques surfaciques
    menage_echelle <-
      menage_echelle %>%
      group_by(DPE_stalog_propri) %>%
      dplyr::mutate(kWh_rank_med_ener = kWh_rank_pess_ener-kWh_rank_opt_ener) %>%
      mutate_when(kWh_rank_med_ener<=0, list(kWh_rank_med_ener=-kWh_rank_med_ener+1)) %>%
      ungroup()

    #Opt_CO2 : classement par émissions surfaciques décroissantes : les plus pollueurs en premier
    menage_echelle <-
      menage_echelle %>%
      mutate(ems_tot_chauff_ecs_surf = ems_tot_chauff_ecs / surfhab_d) %>%
      mutate(ems_tot_chauff_ecs_surf = ifelse(is.infinite(ems_tot_chauff_ecs_surf), 0, ems_tot_chauff_ecs_surf)) %>%
      group_by(DPE_stalog_propri) %>%
      dplyr::mutate(kWh_rank_opt_co2 = row_number(-ems_tot_chauff_ecs_surf)) %>%
      ungroup()

    # Rich : classé par RDb/coeffuc décroissant (revenu par tête) : les plus riches en premiers
    menage_echelle <-
      menage_echelle %>%
      group_by(DPE_dep) %>%
      dplyr::mutate(kWh_rank_rich=row_number(-RDB/coeffuc)) %>%
      ungroup()

    # Poor : classé par RDb/coeffuc croissant (revenu par tête): les plus pauvres en premiers
    menage_echelle <-
      menage_echelle %>%
      group_by(DPE_dep) %>%
      dplyr::mutate(kWh_rank_poor=max(kWh_rank_rich)-kWh_rank_rich+1) %>%
      ungroup()

    # Ajout des rangs liés au DPE_stalog
    menage_echelle <-
      menage_echelle %>%
      mutate(
        kWh_rank_opt = kWh_rank_opt + rank_add,
        kWh_rank_pess = kWh_rank_pess + rank_add,
        kWh_rank_med = kWh_rank_med + rank_add,
        kWh_rank_opt_ener = kWh_rank_opt_ener + rank_add,
        kWh_rank_opt_co2 = kWh_rank_opt_co2 + rank_add) %>%
      select(-rank_add)

    # Sélection du critère à retenir
    if (str_detect(MatisseParams$classement, "Optimal_ener")) {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_opt_ener)
    }
    if (str_detect(MatisseParams$classement, "Optimal_co2")) {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_opt_co2)
    }
    if (str_detect(MatisseParams$classement, "Pess_ener")) {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_pess_ener)
    }
    if (str_detect(MatisseParams$classement, "Med_ener")) {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_med_ener)
    }
    if (str_detect(MatisseParams$classement, "Pessimiste")) {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_pess)
    }
    if (str_detect(MatisseParams$classement, "Optimiste")) {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_opt)
    }
    if (MatisseParams$classement == "Median") {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_med)
    }
    if (MatisseParams$classement == "Rich") {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_rich)
    }
    if (MatisseParams$classement == "Poor") {
      menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_poor)
    }

    # Sélection 2e tour -----------------------------------------------------------------
    # On exclut les ménages logés gratuitement (stalog = 6), les usufruitiers (stalog = 3) et les constructions (year_neuf > 0)
    # On ajoute aussi les insolvables propriétaires (solv > 0.297, stalog = 1)
    excl_idx <- which(menage_echelle$stalog == 6 |
                        menage_echelle$stalog == 3 |
                        menage_echelle$year_neuf > 0 |
                        menage_echelle$exclus == TRUE)
    solv_idx <- which(menage_echelle$solv > 0.297 &
                        menage_echelle$stalog <= 2)
    excl_idx <- union(excl_idx, solv_idx)
    menage_echelle$kWh_rank[excl_idx] <- NA
    menage_echelle$exclus[excl_idx] <- TRUE


    # Bascule des ménages
    for (dep in LETTERS[1:7]){
      # Création d'une table pour la boucle par DPE de départ
      menage_echelle_classe <-
        menage_echelle %>%
        filter(DPE_dep == dep) %>%
        arrange(kWh_rank) %>%
        mutate(surfpond = surfhab_d * pondmen)

      #NB: pas de remise à 0 du compteur de rang en changeant de classe d'arrivée : excepté en VAN (boucle séparée)
      # le classement ne dépend que de la classe DPE de départ, les ménages sont sélectionnés vers la classe la plus
      # haute possible, pour chaque classe_arr, on reprend juste après le ménage sélectionné pour la transition supérieure.
      for (arr in LETTERS[1:7]){
        if(dep>arr){
          # extraction de la quantité de m² à réhabiliter de dep vers arr
          stock_m2_trans <-
            as.numeric((
              Cost_m2 %>%
                filter(classe_dep == dep) %>%
                filter(classe_arr == arr) %>%
                select(transition_tot_m2))[1, ])

          if(as.numeric(stock_m2_trans[1]) > 0){
            #On sélectionne les ménages jusqu'à dépasser l'objectif de m²
            menage_echelle_classe <- menage_echelle_classe[order(menage_echelle_classe$kWh_rank),]
            surf_cumsum <- cumsum(menage_echelle_classe$surfhab_d  * menage_echelle_classe$pondmen)
            l_idx <- which(surf_cumsum >= as.numeric(stock_m2_trans[1]))
            if(length(l_idx) > 0){
              l_idx <- 1:l_idx[1]
            }else{
              l_idx <- 1:length(surf_cumsum)
            }

            menage_echelle_classe$kWh_rank[l_idx] <- NA
            #On enregistre les ménages qui font des réhab dans menage_echelle
            ident_idx <- match(menage_echelle_classe$ident_men[l_idx], menage_echelle$ident_men)
            menage_echelle$REHAB[ident_idx] <- TRUE
            menage_echelle$year_rehab[ident_idx] <- Y
            menage_echelle$classe_arr[ident_idx] <- arr
          }
        }
      }
    }

    # Budget : principal et subventions énergie
    for (dep in LETTERS[1:7]) {
      for (arr in LETTERS[1:7]) {
        if (dep > arr) {
          # Extraction coût de la transition au m2 (dep->arr)
          cost_m2 <- as.numeric((
            Cost_m2 %>%
              filter(classe_dep == dep) %>%
              filter(classe_arr == arr) %>%
              select(cost_m2))[1, ])

          # Taux de subvention des travaux par l'Etat, identique selon les transitions
          subvention_rate <- as.numeric(
            ThreeME %>%
              filter(Var == paste("R_SUB_H01_C", dep, "_C", arr, sep = "")) %>%
              filter(year == all_of(Y)) %>%
              select(value))

          # Coefficient de gain énergétique (à l'horizon, le gain ne s'applique sur la moitié de l'année)
          rate_gain_ener <- as.numeric(
            Mat_gain_ener %>%
              filter(DPE_before == dep) %>%
              filter(DPE_after == arr) %>%
              select(value))
          if (Y == MatisseParams$horizon) {
            rate_gain_ener = rate_gain_ener / 2
          }

          # Modification des budgets
          l_idx <- which(menage_echelle$year_rehab == Y &
                           menage_echelle$DPE_dep == dep &
                           menage_echelle$classe_arr == arr)


          if(length(l_idx)>0){
            #Calcul de la dette & subvention : cout au m² * surface * part non-subventionnée
            menage_echelle$principal_dette[l_idx] <- cost_m2 * menage_echelle$surfhab_d[l_idx] * (1 - subvention_rate)
            menage_echelle$subvention[l_idx] <- cost_m2 * menage_echelle$surfhab_d[l_idx] * subvention_rate
            #Gains énergétiques par source_usages
            menage_echelle[l_idx, list_source_usage] <- menage_echelle[l_idx, list_source_usage] * (1 + rate_gain_ener)
          }
        }
      }
    }

    # Agrégats rénovation locataires
    # Calcul de la somme du montant des travaux avant annulation des ménages loctaires
    # (qui ne le paient pas directement)
    # NB: on annule le montant du principal mais pas la subvention qui sert à calculer la subvention de l'Etat plus bas.

    Dette_bailleur_prive <- as.numeric(
      menage_echelle %>%
        filter(year_rehab == all_of(Y)) %>%
        filter(stalog >= 4 & stalog <= 5) %>% #stalog 4 ou 5 (Locataires / sous-locataires, co-locataire)
        filter(propri == 5 || propri == 6) %>% #propriétaire = membre de la famille ou un autre particulier
        summarise(sum(principal_dette * pondmen)))

    Cout_bailleur_public <- as.numeric(
      menage_echelle %>%
        filter(year_rehab == MatisseParams$horizon) %>%
        filter(stalog >= 4 & stalog <= 5) %>%
        filter(propri == 2) %>%
        summarise(sum((principal_dette) * pondmen)))

    # Hausse des loyers pour les bailleurs privés
    tot_rev504 <- as.numeric(menage_echelle %>% summarise(sum(rev504 * (FC$revpat) * pondmen)))

    #solde_dette_BP_BP solde dette bailleurs privés
    l_idx <- which(!is.na(menage_echelle$rev504) & menage_echelle$rev504 > 0)
    menage_echelle$solde_dette_BP[l_idx] <- menage_echelle$rev504[l_idx] * FC$revpat / tot_rev504 * Dette_bailleur_prive
    if(Dette_bailleur_prive > 0){
      #Bailleur par bailleur car 'amort.period' ne fonctionne pas sur des arrays
      for(i in l_idx){
        menage_echelle$hausse_loyer[i] <-
          amort.period(
            Loan = menage_echelle$solde_dette_BP[i],
            n = as.numeric(1 / R_RMBS_NEWBUIL_H01_CA),
            i = R_I_REHAB_H01_CG_2,
            pf = 1)[2]
        # la hausse de loyers prend en compte le paiement des intérêts et le remboursement du principal
        # la hausse de loyers, hausse de revenu du ménage propriétaire se somme aux hausses des années précédentes
        # Hyp : tous les locataires ont changé sur 15 ans pour justifier une hausse de loyer (interdicition d'augmenter les loyers d'un même locataire
        # sous motif de rénovation énergétique)

        menage_echelle$hausse_loyer_sum[i] <- menage_echelle$hausse_loyer_sum[i] + menage_echelle$hausse_loyer[i]
        menage_echelle$solde_int[i] <- menage_echelle$solde_int[i] +
          as.numeric(calcul_interet_principal(
            loan = menage_echelle$solde_dette_BP[i],
            n = as.numeric(1 / R_RMBS_NEWBUIL_H01_CA),
            year_purchase = Y,
            horizon = MatisseParams$horizon,
            i = R_I_REHAB_H01_CG_2,
            pf = 1)[1])

        menage_echelle$solde_princ[i] <- menage_echelle$solde_princ[i] +
          as.numeric(calcul_interet_principal(
            loan = menage_echelle$solde_dette_BP[i],
            n = as.numeric(1 / R_RMBS_NEWBUIL_H01_CA),
            year_purchase = Y,
            horizon = MatisseParams$horizon,
            i = R_I_REHAB_H01_CG_2,
            pf = 1)[2])
      }




      # répartition des hausses de loyer sur les ménages rénovant
      Hausses_loyer <- menage_echelle %>% summarise(sum(pondmen * hausse_loyer))
      if (Hausses_loyer > 0) {
        menage_echelle <- replace_na(menage_echelle, list(propri = 999999))
        l_idx <- which(menage_echelle$REHAB &
                         menage_echelle$stalog %in% c(4,5) &
                         menage_echelle$propri %in% c(5,6))
        menage_echelle$solde_loyer[l_idx] <- menage_echelle$solde_loyer[l_idx] + as.numeric(Hausses_loyer /
                                                                                              Dette_bailleur_prive *
                                                                                              menage_echelle$principal_dette[l_idx])
      }
      # la hausse de loyer répercutée sur les ménages vaut le montant des travaux (principal_dette) au pro-rata de la hausse des revenus locatif des
      # bailleurs par rapport au montant réel des travaux.
      # Produit en croix Hausse_loyer_locataire/montant travaux locataire=Hausse_loyer_agrégée_bailleur/montant_agrégé_travaux_bailleurs
    }


    # Cas_particulier : LOCATAIRES

    #On utilise une dernière fois le principal_dette des locataires pour la répartition des hausses de loyer, ensuite on le met à zéro pour calculer
    # les solde_intérêt et solde_remboursement du principal pour les ménages propriétaires.
    l_idx <- which(menage_echelle$year_rehab == Y &
                   menage_echelle$stalog >= 4)
    menage_echelle$principal_dette[l_idx] <- 0

    # SOLDE_INT & SOLDE_PRINC
    # Remboursement emprunt pendant 25 ans, Quand l'horizon est à 2035, les rénovations 2010 sont déjà remboursées, pas de solde_int et solde_princ
    if ((MatisseParams$horizon - Y) < 25) {
      #int_price importé depuis Repayment.R, fonction maison
      # Adaptation de la fonction amort.period du package FinancialMath permettant de renvoyer un vecteur comportant le montant payé à l'année n dédié au remboursement du principal et celui des intérêts.
      # solde_int représente le montant d'intérêt payé à l'horizon, on somme les intérêts de toutes les rénovations successives.
      menage_echelle$solde_int <-
        menage_echelle$solde_int + sapply(menage_echelle$principal_dette, function(X)
          as.numeric(calcul_interet_principal(
            loan = X,
            n = 1 /  R_RMBS_NEWBUIL_H01_CA,
            year_purchase = Y,
            horizon = MatisseParams$horizon,
            i =  R_I_REHAB_H01_CG_2  ,
            pf = 1)[1]))
      menage_echelle$solde_princ <-
        menage_echelle$solde_princ + sapply(menage_echelle$principal_dette, function(X)
          as.numeric(calcul_interet_principal(
            loan = X,
            n = 1 /  R_RMBS_NEWBUIL_H01_CA,
            year_purchase = Y,
            horizon = MatisseParams$horizon,
            i =  R_I_REHAB_H01_CG_2  ,
            pf = 1)[2]))
    }

    # SOLVABILITE ex-post
    # calcul solvabilité
    menage_echelle <-
      menage_echelle %>%
      mutate(solv = (c13511 + c13221 + c13211 + solde_int + solde_princ) / RDB)

    # Les ménages insolvables après la transition sont écartés
    menages_insolvables <- menage_echelle %>% filter(REHAB & solv > 0.33 & stalog <= 2) %>% select(ident_men)
    sauv_menages_insolvables <- sauv_menage_echelle_annee_precedente %>% filter(ident_men %in%menages_insolvables$ident_men)
    menages_insolvables_suppr <-
      c(menages_insolvables_suppr, menage_echelle %>%
          filter(REHAB & solv > 0.33 & stalog <= 2) %>% select(year_rehab, ident_men))

    # les ménages insolvables sont "rebootés" à l'itération précédente, avant leur rénovation
    if (dim(menages_insolvables)[1] > 0) {
      menage_echelle <-
        rbind(menage_echelle %>%
                filter(!ident_men %in% menages_insolvables$ident_men),
              sauv_menages_insolvables) %>%
        arrange(ident_men)
    }

    # Mémoire des rénovations de chaque ménage
    menage_echelle <- menage_echelle %>% mutate(count_rehab = ifelse(year_rehab == all_of(Y), paste(count_rehab, Y, sep = "_"), count_rehab))

    # Solde énergétique : Mise à jour des totaux
    #Pour chaque source, rafraichir le calcul de dev_source_verif permet d'obtenir le solde par ménage lié à l'achat neuf
    #Mise à jour des énergies domestiques surfaciques
    for(i in 1:length(sources)){
      menage_echelle[ ,dep_sources_verif[i]] <- rowSums(menage_echelle %>%
                                                          select(all_of(list_source_usage)) %>%
                                                          select(starts_with(all_of(sources[i]))))
    }
    menage_echelle$solde_ener <- rowSums(menage_echelle[dep_sources_verif]) - rowSums(menage_echelle[dep_sources])
    menage_ener_dom <- get_energie_dom_surf(menage_echelle, FC, F)
    menage_echelle <-
      menage_echelle %>%
      select(-ener_dom_surf, -ener_dom, -energie_tot_surf) %>%
      left_join(menage_ener_dom, by = "ident_men")


    # Remise à zéro de principal_dette et des classes DPE
    # sauf à l'horizon, où le principal dette rentre dans le BTP A05
    # pour la rénovation de l'année suivante, les ménages doivent partir de leur nouvelle classe DPE.
    if (Y != MatisseParams$horizon) {
      menage_echelle <-
        menage_echelle %>%
        mutate(DPE_dep = classe_arr) %>%
        mutate(principal_dette = 0) %>%
        mutate(subvention = 0) %>%
        mutate(DPE_stalog_propri= paste(DPE_dep,stalog_propri,sep="_"))
    }
    #FIN BOUCLE sur Y
  }


  # Post traitement budget ---------------------------------------------------------
  menage_echelle[, dep_sources] <- menage_echelle[, dep_sources_verif]
  menage_echelle <-
    menage_echelle %>%
    mutate(
      autres_services = autres_services + solde_int,
      Hors_budget = Hors_budget + solde_princ,
      loyers = loyers + solde_loyer,
      rev_patrimoine = rev_patrimoine + hausse_loyer_sum,
      rev504 = rev504 + hausse_loyer_sum
    )
  # NB :  On ne reventile pas solde_princ (le principal: en effet ce n'est pas une vraie dépense mais une utilisation de l'épargne.
  solde <- menage_echelle %>%
    mutate(solde =
             solde_ener +  #<0 => économie
             solde_int + #>0
             solde_loyer +
             -hausse_loyer_sum) %>%#>0-hausse_loyer_sum) %>% #<0 => comme une économie
    select(ident_men, solde)

  # Budgets à l'horizon -----------------------------------------------------
  # Budget travaux horizon pour propriétaires occupants
  #à l'horizon, le BTP est d'ores et déjà augmenté du montant des travaux
  menage_echelle <-
    menage_echelle %>%
    mutate_when(year_rehab == MatisseParams$horizon & stalog < 3,
                list(BTP = BTP + principal_dette))


  # Budget travaux horizon pour propriétaires bailleurs
  l_idx <- which(!(is.na(menage_echelle$rev504)) &
                   menage_echelle$rev504 > 0)
  menage_echelle$BTP[l_idx] <- menage_echelle$BTP[l_idx] + menage_echelle$solde_dette_BP[l_idx]
  #coût des rénovations à l'horizon pour les propriétaires bailleurs.
  # NB : si pas de rénovation par des bailleurs privés à l'horizon, solde_dette_BP=0

  # Subvention --------------------------------------------------------------
  # On ne compte pas la subvention car déjà prise en compte dans TVA (SUBVENTION)
  # pour les propriétaires, pour les locataires (on compte la subvention via les locataires même si ce sont les propriétaires bailleurs qui la touchent)
  Subvention <-  as.numeric(menage_echelle %>%
                              filter(year_rehab == MatisseParams$horizon) %>%
                              summarise(sum(subvention * pondmen)))


  # ce taux est une baisse de TVA
  # les investissements des offices HLM sont dans la colonne "I" et sont soumis au prix pI (non différencié par agent),
  #le prix total doit donc prendre en compte tous les remises de TVA pour tous les agents (et pas seulement privés)
  sBCE <- as.numeric(Subvention / (Subvention + menage_echelle %>% summarise(sum(pondmen * BTP))))

  # Reventilation -----------------------------------------------------------
  # Pas de ventilation sur les postes énergétiques
  # Que ce soit pour les ménages rénovants ou les propriétaires bailleurs
  # ThreeMe prend en compte les effets rebonds pour les budgets post renovation énergétique (solde_ener+solde_int) => ThreeME est aveugle sur les transferts entre ménage, difficile à dire si les effets rebonds concernent aussi ces soldes. Solution (22/06/2020) => on considère que les effets rebonds macro prennent déjà en compte ces effets
  #
  menage_echelle <- ventilate_solde(menage_echelle, solde, FC , step = "REHAB")
  menage_ener_dom <- get_energie_dom_surf(menage_echelle, FC, F)
  menage_echelle <-
    menage_echelle %>%
    select(-ener_dom_surf, -ener_dom, -energie_tot_surf) %>%
    left_join(menage_ener_dom, by = "ident_men")
  inter_col <- intersect(colnames(menage_echelle), colnames(menage))
  menage_echelle <-
    menage_echelle %>%
    mutate(DPE_horizon=classe_arr) %>%
    select(all_of(inter_col), year_rehab, DPE_horizon, solv)

# Succes ------------------------------------------------------------------
  print("Step D / 3_rehabilitation_inter : SUCCESS")
  if(save_intermed_file){save(menage_echelle, file = MatisseFiles$menage_echelle_D3_rd)}
  return(menage_echelle)

}

