
# simulation_depenses -------------------------------------------------------------------------------------------------------------------------------------
#' @title simulation_depenses
#' @description This function simulates the budgets change
#'
#' @param menage The menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_C1_rd
#'
#' @return A menage dataframe
#'
#' @examples
#' simulation_depenses(menage, FALSE)
#'
simulation_depenses <- function(menage, save_intermed_file = F){


# Data --------------------------------------------------------------------
  menage_echelle <- menage
  load(MatisseFiles$menage_forme_A_rd)
  load(MatisseFiles$FC_2010_horizon_rd)
  load(MatisseFiles$Threeme_rd)
  Elast <- get_elast()


# Extraction des élasticités de Elast --------------------------------------------------------------
  # Nom de tous les catégories de "A01" à "A014")
  list_A <- paste("A", formatC(1:14, width = 2, flag = 0), sep="")
  list_elast_rev <- paste("elast_rev",list_A,sep="_")
  list_elast_prix <- paste("elast_prix",list_A,sep="_")

  Elast_prix <-
    Elast %>%
    filter(typ_elast == "prix") %>%
    select(CODADEME, Typo, Decile, elast) %>%
    dplyr::rename(., elast_prix = elast) %>%
    spread(key = CODADEME, value = elast_prix) %>%
    mutate(Decile = as.numeric(Decile))
  colnames(Elast_prix) <- c("Typo", "Decile", list_elast_prix)

  Elast_rev <-
    Elast %>% filter(typ_elast == "rev") %>%
    mutate(elast_rev = elast) %>%
    select(CODADEME, Typo, Decile, elast_rev) %>%
    spread(key = CODADEME, value = elast_rev) %>%
    mutate(Decile = as.numeric(Decile))
  colnames(Elast_rev) <- c("Typo", "Decile", list_elast_rev)

  #Ajout des élasticités par ménage en fonction de typo et décile
  menage_echelle$decuc2 <- as.numeric(menage_echelle$decuc2)
  menage_echelle<-
    menage_echelle %>%
    left_join(Elast_prix, by = c("typo2010f" = "Typo", "decuc2" = "Decile")) %>%
    left_join(Elast_rev , by = c("typo2010f" = "Typo", "decuc2" = "Decile"))


# Fonction microsimulation des dépenses -----------------------------
  menage_echelle <- microsimulation_depenses(menage_echelle, menage_forme, FC, save_intermed_file)


# Mise à l'échelle des surfaces -------------------------------------------
  # On ne peut pas mettre les surfaces à jour comme les surfaces agrégées, sinon en repondérant pour augmenter le nombre de ménages on aura une trop grande surface.
  # On met donc à l'échelle par l'évolution de la surface par habitant dans 3ME.
  BUIL_H01_2_2010 <-
    as.numeric(ThreeME %>%
              filter(Var == "BUIL_H01_2") %>%
              filter(year == 2010) %>%
              select(value))
  POP_TOT_2010 <-
    as.numeric(ThreeME %>%
              filter(Var == "POP_TOT") %>%
              filter(year == 2010) %>%
              select(value))

  surfhab_hab_2010 <- BUIL_H01_2_2010 / POP_TOT_2010

  BUIL_H01_2_horizon <-
    as.numeric(ThreeME %>%
              filter(Var == "BUIL_H01_2") %>%
              filter(year == MatisseParams$horizon) %>%
              select(value))

  POP_TOT_horizon <-
    as.numeric(ThreeME %>%
              filter(Var == "POP_TOT") %>%
              filter(year == MatisseParams$horizon) %>%
              select(value))

  surfhab_hab_horizon <- BUIL_H01_2_horizon / POP_TOT_horizon
  FC$surface <- surfhab_hab_horizon / surfhab_hab_2010
  save(FC,file=MatisseFiles$FC_2010_horizon_rd)

  menage_echelle <-
    menage_echelle %>%
    mutate(surfhab_d = surfhab_d * FC$surface)


# Mise à l'échelle des dépenses immobilières ------------------------------
  # 13211 Remboursements de prêts pour la résidence principale (yc garage et dépendance)
  # 13221 Remboursements des autres prêts immobiliers (résidence secondaire et autre logement yc dépendance)
  # 13511 Remboursements de crédits à la consommation (voiture, gros travaux, biens durables)

  menage_echelle <-
   menage_echelle %>%
    mutate(c13711 = c13711 * FC$A05 * FC$surface) %>%
    mutate(rev800 = rev800 * FC$A05 * FC$surface)%>%
    mutate(c13211 = c13211 * FC$A05 * FC$surface)%>%
    mutate(c13221 = c13221 * FC$A05 * FC$surface)%>%
    mutate(c13211 = c13211 * FC$A05 * FC$surface)%>%
    mutate(c13511 = c13511 * (1 + TC_RDB_nominal))


# Succes ------------------------------------------------------------------
  print("Step C / 1_simulation_depenses : SUCCESS")
  if(save_intermed_file){save(menage_echelle, file = MatisseFiles$menage_echelle_C1_rd)}
  return(menage_echelle)

}


microsimulation_depenses <- function(menage_echelle, menage_forme, FC, save_intermed_file = F){


# Calcul des parts budgétaires --------------------------------------------

  list_dep <- c("agriculture", "dep_Elec", "dep_Gaz", "dep_autres_energies",
                "BTP", "prod_veh", "carb_lubr", "transp_rail_air", "transp_routes_eau",
                "loisirs_com", "autres_services", "autres", "loyers", "veh_occasion")
  menage_echelle <-
    menage_echelle %>%
    mutate(dep_autres_energies = dep_Fuel + dep_GPL + dep_Solides + dep_Urbain)
  menage_echelle$Rcons_bis <- rowSums(menage_echelle[list_dep])

  list_A <- paste("A", formatC(1:14, width = 2, flag = 0), sep="")
  for (i in 1:14){
    k <- list_dep[i]
    menage_echelle[paste("share_", list_A[i], sep="")] <- menage_echelle[k] / menage_echelle$Rcons_bis
  }

  # Calcul taux de croissance du RDB pour chaque ménage
  menage_echelle$TC_RDB_nominal <- (menage_echelle$RDB - menage_forme$RDB) / menage_forme$RDB

  # Le passage du revenu nominal au revenu reel est assuré par le déflateur de Stone, produit des prix à la puissance des parts budgétaires
  # on calcule le déflateur de stone par ménage => on ne peut donc pas considérer les dépenses de construction neuve ou les achats de véhicules d'occasion
  # à des professionels (traitement agrégé).
  # revenu reel 2010 = revenu nominal 2010
  menage_echelle$IP_stone <-  1
  menage_echelle$IP_stone <-
    FC$A01 ** menage_echelle$share_A01 *
    FC$A02 ** menage_echelle$share_A02 *
    FC$A03 ** menage_echelle$share_A03 *
    FC$A04 ** menage_echelle$share_A04 *
    FC$A05 ** menage_echelle$share_A05 *
    FC$A06 ** menage_echelle$share_A06 *
    FC$A07 ** menage_echelle$share_A07 *
    FC$A08 ** menage_echelle$share_A08 *
    FC$A09 ** menage_echelle$share_A09 *
    FC$A10 ** menage_echelle$share_A10 *
    FC$A11 ** menage_echelle$share_A11 *
    FC$A12 ** menage_echelle$share_A12 *
    FC$A13 ** menage_echelle$share_A13 *
    FC$A14 ** menage_echelle$share_A14


  menage_echelle$RDB_reel <-  menage_echelle$RDB / menage_echelle$IP_stone
  menage_echelle$RDB_reel[which(is.na(menage_echelle$RDB_reel))] <- 0

  menage_echelle$TC_RDB_reel <- (menage_echelle$RDB_reel - menage_forme$RDB) / menage_forme$RDB

  min_TC_rev <-
    min(FC %>% select(starts_with("rev"))) - 1

  menage_echelle <-
    menage_echelle %>%
    mutate(TC_RDB_reel = ifelse(TC_RDB_nominal <= min_TC_rev, FC$rdb / IP_stone - 1, TC_RDB_reel))

# Mise échelle dep --------------------------------------------------------
  KeepLooping <- TRUE
  nb_iter_RDB <- 0
  list_dep_autres_ener <- c("dep_GPL", "dep_Fuel", "dep_Urbain", "dep_Solides")

  list_cat <- c("agriculture", "elec", "gaz_ville", "autres_energies_dom", "BTP", "prod_veh",
             "carb_lubr", "transp_rail_air", "transp_routes_eau", "loisirs_com",
             "autres_services", "autres", "loyers", "veh_occasion")

  while(KeepLooping & nb_iter_RDB < 61){
    sauv_menage_echelle <- menage_echelle
    nb_iter_RDB <- nb_iter_RDB + 1

    #Traitement à part des dépenses issues de l'enquête EDF, application des
    #élasticités correspondantes de BDF pour l'électricité, le gaz, les autres énergies
    # et les carburants.
    #formule : en notant D la dépense, P le prix, RDB le revenu brut disponible,
    #l'apostrophe désignant la variable mise à l'échelle :
    # D'=D*(1+elast_prix*Delta_prix/prix+elast_rev*Delta_RDB/RDB) * P'/P
    # or P'/P=TC_prix
    # or Delta_prix=1+TC_prix
    # Les valeurs de TC pour les prix ne sont pas des taux de croissance mais des ratios de prix (P'/P)
    # En revanche TC_RDB est un vrai taux de croissance

    menage_echelle$dep_Elec <-
      menage_forme$dep_Elec *
      (1 + menage_echelle$elast_prix_A02 * (FC$A02 / menage_echelle$IP_stone - 1)) *
      (1 + menage_echelle$elast_rev_A02 * menage_echelle$TC_RDB_reel) *
      FC$A02

    menage_echelle$dep_Gaz<-
      menage_forme$dep_Gaz *
      (1 + menage_echelle$elast_prix_A03 * (FC$A03 / menage_echelle$IP_stone - 1)) *
      (1 + menage_echelle$elast_rev_A03 * menage_echelle$TC_RDB_reel) *
      FC$A03


    #Urbain et solides : elast sur 04, FC sur gaz, comme 3ME
    menage_echelle$dep_Urbain <-
      menage_forme$dep_Urbain *
      (1 + menage_echelle$elast_prix_A04 * (FC$A03 / menage_echelle$IP_stone - 1)) *
      (1 + menage_echelle$elast_rev_A04 * menage_echelle$TC_RDB_reel) *
      FC$A03
    menage_echelle$dep_Solides  <-
      menage_forme$dep_Solides *
      (1 + menage_echelle$elast_prix_A04 * (FC$A03 / menage_echelle$IP_stone - 1)) *
      (1 + menage_echelle$elast_rev_A04 * menage_echelle$TC_RDB_reel) *
      FC$A03

    #GPL et Fuel : elast et FC sur 04
    menage_echelle$dep_GPL  <-
      menage_forme$dep_GPL *
      (1 + menage_echelle$elast_prix_A04 * (FC$A04 / menage_echelle$IP_stone - 1)) *
      (1 + menage_echelle$elast_rev_A04 * menage_echelle$TC_RDB_reel) *
      FC$A04
    menage_echelle$dep_Fuel  <-
      menage_forme$dep_Fuel *
      (1 + menage_echelle$elast_prix_A04 * (FC$A04 / menage_echelle$IP_stone - 1)) *
      (1 + menage_echelle$elast_rev_A04 * menage_echelle$TC_RDB_reel) *
      FC$A04


    # Dep_logement
    menage_echelle$dep_energie_logement <- rowSums(menage_echelle[c("dep_Elec", "dep_Gaz", list_dep_autres_ener)])

    #Calcul des dépenses par catégorie en prenant en compte les élasticités, facteurs de croissance et inflation
    for(i in c(1, 5:14)){
      elast_prix <- paste("elast_prix_",list_A[i],sep="")
      elast_rev <- paste("elast_rev_",list_A[i],sep="")
      menage_echelle[list_cat[i]] <-
        menage_forme[list_cat[i]] *
        (1 + menage_echelle[elast_prix] * (as.numeric(FC[list_A[i]]) / menage_echelle$IP_stone - 1)) *
        (1 + menage_echelle[elast_rev] * menage_echelle$TC_RDB_reel) * as.numeric(FC[list_A[i]])
    }


    # # #Traitement des hors budgets :
    # # #pas d'élasticité prix, élasticité revenu =1 par hypothèse, taux de croissance du rdb,
    # menage_echelle$Hors_budget<-
    #   menage_forme$Hors_budget*(1 + menage_echelle$TC_RDB_nominal)

    # #pas besoin de le mettre à jour ici !!!!
    # menage_echelle$rev801 <-
    #   menage_forme$rev801*
    #   (1+menage_echelle$elast_prix_A13*(-1+as.numeric(FC$A13)/menage_echelle$IP_stone))*
    #   (1+menage_echelle$elast_rev_A13*menage_echelle$TC_RDB_reel)*as.numeric(FC$A13)
    #

    menage_echelle <-
      menage_echelle %>%
      mutate(dep_autres_energies = dep_Fuel + dep_GPL + dep_Solides + dep_Urbain)
    menage_echelle$Rcons_bis <- rowSums(menage_echelle[list_dep])
    ## Attention on n'a pas rajouté les constructions neuves à l'horizon, elles ne sont pas encore attribuées

    for (i in 1:14){
      k <- list_dep[i]
      menage_echelle[paste("share_", list_A[i], sep = "")] <- menage_echelle[k] / menage_echelle$Rcons_bis
    }


    # revenu reel 2010 = revenu nominal 2010
    menage_echelle$IP_stone <-
      FC$A01 ** menage_echelle$share_A01 *
      FC$A02 ** menage_echelle$share_A02 *
      FC$A03 ** menage_echelle$share_A03 *
      FC$A04 ** menage_echelle$share_A04 *
      FC$A05 ** menage_echelle$share_A05 *
      FC$A06 ** menage_echelle$share_A06 *
      FC$A07 ** menage_echelle$share_A07 *
      FC$A08 ** menage_echelle$share_A08 *
      FC$A09 ** menage_echelle$share_A09 *
      FC$A10 ** menage_echelle$share_A10 *
      FC$A11 ** menage_echelle$share_A11 *
      FC$A12 ** menage_echelle$share_A12 *
      FC$A13 ** menage_echelle$share_A13 *
      FC$A14 ** menage_echelle$share_A14

    menage_echelle$RDB_reel <-  menage_echelle$RDB / menage_echelle$IP_stone
    menage_echelle$RDB_reel[which(is.na(menage_echelle$RDB_reel))] <- 0


    menage_echelle$TC_RDB_reel <- (menage_echelle$RDB_reel - menage_forme$RDB) / menage_forme$RDB

    menage_echelle <-
      menage_echelle %>%
      mutate(TC_RDB_reel = ifelse(TC_RDB_nominal <= min_TC_rev, FC$rdb / IP_stone - 1, TC_RDB_reel))

    tol <- abs((menage_echelle$RDB_reel - sauv_menage_echelle$RDB_reel) / sauv_menage_echelle$RDB_reel)
    if(max(tol, na.rm = T) > 10^-3){KeepLooping <- TRUE}else{KeepLooping <- FALSE}
  }

  # en sortie de boucle, on utilise le dernier IP_stone, celui qui permet la convergence du RDB_reel pour calculer les dépenses maj

  # A02
  menage_echelle$dep_Elec <-
    menage_forme$dep_Elec *
    (1 + menage_echelle$elast_prix_A02 * (FC$A02 / menage_echelle$IP_stone - 1)) *
    (1 + menage_echelle$elast_rev_A02 * menage_echelle$TC_RDB_reel) *
    FC$A02

  # A03
  menage_echelle$dep_Gaz <-
    menage_forme$dep_Gaz *
    (1 + menage_echelle$elast_prix_A03 * (FC$A03 / menage_echelle$IP_stone - 1)) *
    (1 + menage_echelle$elast_rev_A03 * menage_echelle$TC_RDB_reel) *
    FC$A03


  #Urbain et solides : elast sur 04, FC sur gaz, comme 3ME
  menage_echelle$dep_Urbain <-
    menage_forme$dep_Urbain *
    (1 + menage_echelle$elast_prix_A04 * (FC$A03 / menage_echelle$IP_stone - 1)) *
    (1 + menage_echelle$elast_rev_A04 * menage_echelle$TC_RDB_reel) *
    FC$A03
  menage_echelle$dep_Solides  <-
    menage_forme$dep_Solides *
    (1 + menage_echelle$elast_prix_A04 * (FC$A03 / menage_echelle$IP_stone - 1)) *
    (1 + menage_echelle$elast_rev_A04 * menage_echelle$TC_RDB_reel) *
    FC$A03

  #GPL et Fuel : elast et FC sur 04
  menage_echelle$dep_GPL  <-
    menage_forme$dep_GPL *
    (1 + menage_echelle$elast_prix_A04 * (FC$A04 / menage_echelle$IP_stone - 1)) *
    (1 + menage_echelle$elast_rev_A04 * menage_echelle$TC_RDB_reel) *
    FC$A04
  menage_echelle$dep_Fuel  <-
    menage_forme$dep_Fuel *
    (1 + menage_echelle$elast_prix_A04 * (FC$A04 / menage_echelle$IP_stone - 1)) *
    (1 + menage_echelle$elast_rev_A04 * menage_echelle$TC_RDB_reel) *
    FC$A04


  # Dep_logement
  menage_echelle$dep_energie_logement <- rowSums(menage_echelle[c("dep_Elec", "dep_Gaz", list_dep_autres_ener)])


  for (i in c(1, 5:14)){
    elast_prix <- paste("elast_prix_", list_A[i], sep = "")
    elast_rev <- paste("elast_rev_", list_A[i], sep = "")
    menage_echelle[list_cat[i]] <-
      menage_forme[list_cat[i]] *
      (1 + menage_echelle[elast_prix] * (as.numeric(FC[list_A[i]]) / menage_echelle$IP_stone - 1)) *
      (1 + menage_echelle[elast_rev] * menage_echelle$TC_RDB_reel) * as.numeric(FC[list_A[i]])
  }


  # rev801, loyers imputés sont mis à jour en utilisant la même élasticité que le secteur 13 des loyers
  menage_echelle$rev801 <-
    menage_forme$rev801 *
    (1 + menage_echelle$elast_prix_A13 * (as.numeric(FC$A13) / menage_echelle$IP_stone - 1)) *
    (1 + menage_echelle$elast_rev_A13 * menage_echelle$TC_RDB_reel) * as.numeric(FC$A13)


  # les ventes de veh suivent le neuf (plutôt que le rdb) => rev850 issus de ventes de veh par les ménages suit donc le secteur A06
  menage_echelle$rev850 <-
    menage_forme$rev850 *
    (1 + menage_echelle$elast_prix_A06 * (as.numeric(FC$A06) / menage_echelle$IP_stone - 1)) *
    (1 + menage_echelle$elast_rev_A06 * menage_echelle$TC_RDB_reel) * as.numeric(FC$A06)

  return(menage_echelle)

}



