# OBJECTIF : Les ménages achetant un logement en 2010 le sont également à l'horizon, on sélectionne parmi ceux là les ménages achetant un logement neuf.
#Including enertot change

#' @title achat_neuf_horizon
#' @description Function that handles the buying of new houses at horizon year
#'
#' @param menage A menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_D1_rd
#'
#' @return A menage dataframe
#'
#' @examples
#' achat_neuf_horizon(menage, FALSE)
#'
achat_neuf_horizon <- function(menage, save_intermed_file = F){

# Data --------------------------------------------------------------------
  #3ME data
  coeff_dep_ems <- suppressMessages(read_csv(MatisseFiles$coeff_dep_ems_csv))
  load(MatisseFiles$coeff_ems_2010_rd)

  # Donnes ThreeME : m2 et valeurs d'achats de logement neufs trajectoire
  load(MatisseFiles$Threeme_rd)
  load(MatisseFiles$FC_2010_horizon_rd)

  #Données standard
  list_source_usage <- get_list_source_usage()
  list_dep <- get_list_dep()
  sources <- get_sources()
  dep_sources <- paste("dep", sources, sep = "_")
  dep_sources_verif <- paste(dep_sources, "verif", sep="_")

  #Traitement de menage
  menage_echelle <- menage %>% mutate(DPE_dep = DPE_pred)
  menage_echelle$ems_tot_chauff_ecs <- get_emissions(menage_echelle, FC)


# Données 3ME  -------------------------------------------------
  # Constructions neuves en m2 : NEWBUIL_H01_2
  NEWBUIL_H01_2_horizon <-
    as.numeric(ThreeME %>% filter(Var == "NEWBUIL_H01_2") %>% filter(year == MatisseParams$horizon) %>% select(value))
  NEWBUIL_H01_2_2010 <-
    as.numeric(ThreeME %>% filter(Var == "NEWBUIL_H01_2") %>% filter(year == 2010) %>% select(value))
  # Constructions neuves en m2 par classe énergétique : A & B
  NEWBUIL_H01_CA_2 <-
    as.numeric(ThreeME %>% filter(Var == "NEWBUIL_H01_CA_2") %>% filter(year == MatisseParams$horizon) %>% select(value))
  NEWBUIL_H01_CB_2 <-
    as.numeric(ThreeME %>% filter(Var == "NEWBUIL_H01_CB_2") %>% filter(year == MatisseParams$horizon) %>% select(value))

# Données 3ME de gains énergétiques --------------------------------------------
  # Consommation énergétique des logements de classe CA en KWh/m2 #ENER_BUIL_H01_CA_2*11630/BUIL_H01_CA_2
  conso_moy_dep <- data.frame("A" = 0, "B" = 0, "C" = 0, "D" = 0, "E" = 0, "F" = 0, "G" = 0)
  for (i in LETTERS[1:7]){
    conso_moy_dep[i]<-
      as.numeric(ThreeME %>%
                 filter(Var == paste("ENER_BUIL_H01_C",i,"_2*11630/BUIL_H01_C",i,"_2",sep="")) %>%
                 filter(year == MatisseParams$horizon) %>%
                 select(value))
  }
  Mat_gain_ener <- get_gain_energie(conso_moy_dep)

# Classement --------------------------------------------------------------
  menage_echelle$ener_dom[which(is.na(menage_echelle$ener_dom))] <- 0

  # Optimiste : on classe les ménages pour chaque DPE par ordre décroissant de consommation énergétique pour
  #le logement en MWh (absolu => maximisation du gain agrégé)
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_opt = row_number(-ener_dom)) %>%
    ungroup()

  # Pessimiste : on classe les ménages par ordre croissant de conso éner pour le logement, les premiers seront les derniers
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_pess = max(kWh_rank_opt, na.rm = T) - kWh_rank_opt + 1) %>%
    ungroup()

  # Median : on prend le milieu de kwh_rank_opt et kWh_rank_pess
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_med = kWh_rank_pess - kWh_rank_opt) %>%
    ungroup()
  l_idx <- which(menage_echelle$kWh_rank_med <= 0)
  menage_echelle$kWh_rank_med[l_idx] <- menage_echelle$kWh_rank_med[l_idx] * -1 + 1

  # Rich : classé par RDb/coeffuc décroissant (revenu par tête) : les plus riches en premiers
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_rich = row_number(-RDB / coeffuc)) %>%
    ungroup()

  # Poor : classé par RDb/coeffuc croissant (revenu par tête): les plus pauvres en premiers
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_poor = max(kWh_rank_rich) - kWh_rank_rich + 1) %>%
    ungroup()

  # Opt_Ener : classement par énergie domestique surfacique décroissante : les plus consommateurs par m² en premier
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_opt_ener = row_number(-ener_dom_surf)) %>%
    ungroup()

  # Pess_ener : classement par énergie domestique surfacique croissant : les plus sobres par m² en premier
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_pess_ener = max(kWh_rank_opt_ener, na.rm = T) - kWh_rank_opt_ener + 1) %>%
    ungroup()

  # Med_ener : classement médian par consommations domestiques surfaciques
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_med_ener = kWh_rank_pess_ener - kWh_rank_opt_ener) %>%
    ungroup()
  l_idx <- which(menage_echelle$kWh_rank_med_ener <= 0)
  menage_echelle$kWh_rank_med_ener[l_idx] <- menage_echelle$kWh_rank_med_ener[l_idx] * -1 + 1

  #Opt_CO2 : classement par émissions surfaciques décroissantes : les plus pollueurs en premier
  menage_echelle <-
    menage_echelle %>%
    mutate(ems_tot_chauff_ecs_surf = ems_tot_chauff_ecs / surfhab_d) %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_opt_co2 = row_number(-ems_tot_chauff_ecs_surf)) %>%
    ungroup()

  #Sélection de la colonne à utiliser en fonction du MatisseParams$classement
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
  menage_echelle <- menage_echelle %>%
                    select(-kWh_rank_opt_ener, -kWh_rank_opt_co2, -kWh_rank_pess_ener,
                           -kWh_rank_med_ener, -kWh_rank_pess, -kWh_rank_opt, -kWh_rank_med,
                           -kWh_rank_rich, -kWh_rank_poor)


# Selection menages accedant horizon Not VAN -----------------------------------
  # Les dépenses d'achat de logement (c13711) ne sont pas à l'échelle dans c05_forme_2010,
  # mais on récupère uniquement les identifiants ménages
  ident_accedants <-
    menage_echelle %>%
    filter(c13711 > 10000) %>%
    filter(ener_dom_surf > 0) %>% # personne ne fait de travaux si pas de conso d'énergie
    select(ident_men,
           pondmen,
           surfhab_d,
           DPE_dep,
           kWh_rank,
           c13711,
           ener_dom) %>%
    filter(!DPE_dep == "A") %>% # les ménages déjà en A exclus, sinon on se retrouve avec dep=A, arr=B
    mutate(year_neuf = 0) %>%
    mutate(classe_arr = DPE_dep) %>%
    dplyr::arrange(., kWh_rank)
  #Justification : on adopte un système similaire aux autres étapes : sélection uniquement sur le classemenet énergétique, le système s'appuyant sur ancons et anacq
  # est plus complexe à justifier : est-ce qu'on considère plus probable qu'un ménage achetant un logement neuf et efficace à l'horizon soit un ménage qui ait acheté
  # un logement ancien dans le passé ou justement de privilégier des ménages déjà efficaces dans des logements les plus récents possibles ?
  # les deux stratégies se justifient, pour éviter de choisir on adopte la stratégie du "rateau" déjà employée pour les rénovations et les achats avant l'horizon.

# Selection des ménages ---------------------------------------------------
  # DPE A
  count <- 0
  DPE_A <- 0
  while (DPE_A < NEWBUIL_H01_CA_2) {
    count = count + 1
    DPE_A <- ident_accedants[1:count, ] %>% summarise(sum(pondmen * surfhab_d))
    ident_accedants[count, ]$classe_arr <- "A"
    ident_accedants[count, ]$year_neuf <- MatisseParams$horizon
  }
  # DPE B
  DPE_B <- 0
  while (DPE_B < NEWBUIL_H01_CB_2) {
    count = count + 1
    while (ident_accedants[count, ]$DPE_dep == "B") {count = count + 1} #on veut éviter de sélectionner une transition B->B
    DPE_B <- DPE_B + ident_accedants[count, ] %>% summarise(sum(pondmen * surfhab_d))
    ident_accedants[count, ]$classe_arr <- "B"
    ident_accedants[count, ]$year_neuf <- MatisseParams$horizon
  }
  ident_accedants <- ident_accedants %>% select(ident_men,classe_arr,year_neuf)

  menage_echelle <-
    menage_echelle %>%
    left_join(ident_accedants, by = "ident_men") %>%
    mutate(solde_ener = 0)
  menage_echelle$year_neuf[which(is.na(menage_echelle$year_neuf))] <- 0
  l_idx <- which(is.na(menage_echelle$classe_arr))
  menage_echelle$classe_arr[l_idx] <- menage_echelle$DPE_dep[l_idx]


# Altération des conso d'énergie suite à achat neuf ------------------------------------------------
  for (dep in LETTERS[2:7]) {
    # classe de départ
    for (arr in LETTERS[1:2]) {
      #NB :à partir de 2020 on ne construit que des classes A et B (arrêt des classes C en 2019)
      # classe arrivée
      if (dep > arr) {
        # Coefficient de gain énergétique (multiplié par 1/2 pour centrer les consommations des constructions de fin et de début d'année)
        rate_gain_ener <- as.numeric(
          Mat_gain_ener %>%
            filter(DPE_before == dep) %>%
            filter(DPE_after == arr) %>%
            select(value)) * 1/2
        # (coeff 0.5 car en moyenne des gains sur 6 mois
        # (rénovation en début ou fin d'année))

        #s'il existe un ménage dans cette situation dep->arr à l'horizon
        l_idx <- intersect(intersect(
                            which(menage_echelle$year_neuf == MatisseParams$horizon),
                            which(menage_echelle$DPE_dep == dep)),
                            which(menage_echelle$classe_arr == arr))
        if (length(l_idx) > 0){
          for(source_usage in list_source_usage){
            menage_echelle[l_idx, source_usage] <- menage_echelle[l_idx, source_usage] * (1 + rate_gain_ener)
          }
        }
      }
    }
  }

# Maj dépenses énergie ----------------------------------------------------
  #Pour chaque source, rafraichir le calcul de dev_source_verif permet d'obtenir le solde par ménage lié à l'achat neuf
  for(i in 1:length(sources)){
    menage_echelle[ ,dep_sources_verif[i]] <- rowSums(menage_echelle %>%
                                                        select(all_of(list_source_usage)) %>%
                                                        select(starts_with(all_of(sources[i]))))
  }
  menage_echelle$solde_ener <- rowSums(menage_echelle[dep_sources_verif]) - rowSums(menage_echelle[dep_sources])
  menage_echelle[,dep_sources] <- menage_echelle[,dep_sources_verif]


# Reventilation et recalcul des énergies domestiques -------------------------------------------------------------
  solde <- menage_echelle %>%
           mutate(solde = ifelse(abs(solde_ener) < 10 ^ (-9), 0, solde_ener)) %>%
           select(ident_men, solde)

  menage_echelle <- MatisseCIRED:::ventilate_solde(menage_echelle, solde, FC , step = "REHAB")
  menage_ener_dom <- MatisseCIRED:::get_energie_dom_surf(menage_echelle, FC, F)

  menage_echelle <-
    menage_echelle %>%
    select(-ener_dom_surf,-ener_dom, -energie_tot_surf) %>%
    left_join(menage_ener_dom,by="ident_men")


# Succes ------------------------------------------------------------------
  print("Step D / 1_achat_neuf_horizon : SUCCESS")
  if(save_intermed_file){save(menage_echelle, file=MatisseFiles$menage_echelle_D1_rd)}
  return(menage_echelle)

}
