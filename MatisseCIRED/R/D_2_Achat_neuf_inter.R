# Constructions neuves entre 2010 et horizon :
# Selection des ménages
# Mise à jour budgets
#Including enertot change

#' @title achat_neuf_inter
#' @description Function that handles the buying of new houses between the start and the horizon
#'
#' @param menage A menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_D2_rd
#'
#' @return A menage dataframe
#'
#' @examples
#' achat_neuf_inter(menage, FALSE)
#'
achat_neuf_inter <- function(menage, save_intermed_file = F){


# Data --------------------------------------------------------------------
  # Donnes ThreeME
  load(MatisseFiles$FC_2010_horizon_rd)
  load(MatisseFiles$Threeme_rd)
  load(MatisseFiles$coeff_ems_2010_rd)
  coeff_dep_ems <- suppressMessages(read_csv(MatisseFiles$coeff_dep_ems_csv))

  #Menage
  menage_echelle <- menage
  menage_echelle$ems_tot_chauff_ecs <- get_emissions(menage_echelle, FC)

  #Données standard
  list_dep <- get_list_dep()
  sources <- get_sources()
  dep_sources <- paste("dep", sources, sep = "_")
  dep_sources_verif <- paste(dep_sources, "verif", sep="_")
  list_dep <- get_list_dep()
  list_source_usage <- get_list_source_usage()



# Data 3ME ------------------------------------------------------------
  # volume construction neuf (en m2)
  NEWBUIL_H01_CA_2<-
    ThreeME %>%
    filter(year < MatisseParams$horizon & year >=2010) %>%
    filter(Var=="NEWBUIL_H01_CA_2")%>%
    select(year,value)

  NEWBUIL_H01_CB_2<-
    ThreeME %>%
    filter(year<MatisseParams$horizon & year >=2010) %>%
    filter(Var=="NEWBUIL_H01_CB_2")%>%
    select(year,value)

  NEWBUIL_H01_CC_2<-
    ThreeME %>%
    filter(year<MatisseParams$horizon & year >=2010) %>%
    filter(Var=="NEWBUIL_H01_CC_2")%>%
    select(year,value)

# Préparation données -----------------------------------------------------
  # Nouvelles variables : sont "exclus" les ménages qui ne peuvent bénéficier de la construction d'un logement neuf
  # Exclus
  # les ménages c13711>10000 : les ménages qui achètent une maison à l'horizon ne peuvent pas avoir construit un logement peu de temps avant
  # Les ménages déjà en A
  # Les locataires (on garde stalog 1 et 2, propriétaires et propriétaires remboursant un emprunt)
  #NEUF va indiquer les ménages sélectionnés pour rénover leur logement :
  # passer de DPE_pred à class_arr

  menage_echelle <-
    menage_echelle %>%
    mutate(exclus = FALSE, NEUF = FALSE) %>%
    mutate(solde_ener = 0)
  #On exclus les ménages achetant à l'horizon, ayant déjà acheter en année init, déjà en DPE A ou locataires
  excl_idx = which(menage_echelle$year_neuf == MatisseParams$horizon |
                   menage_echelle$c13711 > 10000 |
                   menage_echelle$DPE_dep == "A" |
                   menage_echelle$stalog > 2)
  menage_echelle$exclus[excl_idx] = T
  no_new_idx <- which(menage_echelle$year_neuf != MatisseParams$horizon)
  menage_echelle$classe_arr[no_new_idx] = menage_echelle$DPE_dep[no_new_idx]

  # Classement ménages --------------------------------------------------------------
  # Un seul classement de 2010 à horizon-1
  # décision => plus gros consommateur (resp petit) de 2035 (décision ex post de rénover pour optimiser les émissions macro)

  menage_echelle <- replace_na(menage_echelle, replace = list(ener_dom = 0))

  # Optimiste : on classe les ménages pour chaque DPE par ordre décroissant de consommation énergétique pour
  #le logement en MWh (absolu => maximisation du gain agrégé)
  menage_echelle <-
    menage_echelle %>%
    dplyr::mutate(kWh_rank_opt = row_number(-ener_dom))

  # Pessimiste : on classe les ménages par ordre croissant de conso éner pour le logement, les premiers seront les derniers
  menage_echelle <-
    menage_echelle %>%
    dplyr::mutate(kWh_rank_pess = max(kWh_rank_opt, na.rm = T) - kWh_rank_opt + 1)

  # Median : on prend le milieu de kwh_rank_opt et kWh_rank_pess
  menage_echelle <-
    menage_echelle %>%
    group_by(DPE_dep) %>%
    dplyr::mutate(kWh_rank_med =kWh_rank_pess-kWh_rank_opt) %>%
    mutate_when(
      kWh_rank_med<=0,
      list(kWh_rank_med=-kWh_rank_med+1)) %>%
    ungroup()

  # Rich : classé par RDb/coeffuc décroissant (revenu par tête) : les plus riches en premiers
  menage_echelle <-
    menage_echelle %>%
    dplyr::mutate(kWh_rank_rich=row_number(-RDB/coeffuc))

  # Poor : classé par RDb/coeffuc croissant (revenu par tête): les plus pauvres en premiers
  menage_echelle <-
    menage_echelle %>%
    dplyr::mutate(kWh_rank_poor=max(kWh_rank_rich)-kWh_rank_rich+1)

  # Opt_Ener : classement par énergie domestique surfacique décroissante : les plus consommateurs par m² en premier
  menage_echelle<-
    menage_echelle %>%
    mutate(ener_dom_surf=ifelse(is.infinite(ener_dom_surf),0,ener_dom_surf))%>%
    dplyr::mutate(kWh_rank_opt_ener =row_number(-ener_dom_surf)) %>%
    ungroup()

  # Pess_ener : classement par énergie domestique surfacique croissant : les plus sobres par m² en premier
  menage_echelle<-
    menage_echelle %>%
    dplyr::mutate(kWh_rank_pess_ener =max(kWh_rank_opt_ener,na.rm=T)-kWh_rank_opt_ener+1) %>%
    ungroup()

  # Med_ener : classement médian par consommations domestiques surfaciques
  menage_echelle <-
    menage_echelle %>%
    dplyr::mutate(kWh_rank_med_ener = kWh_rank_pess_ener - kWh_rank_opt_ener) %>%
    mutate_when(kWh_rank_med_ener <= 0, list(kWh_rank_med_ener = -kWh_rank_med_ener + 1)) %>%
    ungroup()

  #Opt_CO2 : classement par émissions surfaciques décroissantes : les plus pollueurs en premier
  menage_echelle<-
    menage_echelle %>%
    mutate(ems_tot_chauff_ecs_surf=ems_tot_chauff_ecs/surfhab_d)%>%
    mutate(ems_tot_chauff_ecs_surf=ifelse(is.infinite(ems_tot_chauff_ecs_surf),0,ems_tot_chauff_ecs_surf))%>%
    dplyr::mutate(kWh_rank_opt_co2 =row_number(-ems_tot_chauff_ecs_surf))


  #Sélection de la colonne à utiliser en fonction du MatisseParams$classement
  if (str_detect(MatisseParams$classement, "Optimal_ener")) {
    menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_opt_ener)
  }
  if (str_detect(MatisseParams$classement, "Pess_ener")) {
    menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_pess_ener)
  }
  if (str_detect(MatisseParams$classement, "Med_ener")) {
    menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_med_ener)
  }
  if (str_detect(MatisseParams$classement, "Optimal_co2")) {
    menage_echelle <- menage_echelle %>% mutate(kWh_rank = kWh_rank_opt_co2)
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

# Selection des ménages ---------------------------------------------------

  # les ménages exclus ne le sont pas du classement ex-ante, mais leur classement est ramené à NA pour permettre l'utilisation de order
  menage_echelle$kWh_rank[which(menage_echelle$exclus)] <- NA

  # ANNEE PAR ANNEE
  # Pourquoi commencer en 2011 et pas en 2010 ?
  # Le parc DPE a été calé sur les volumes 2010, les nouvelles constructions de cette année là ont déjà été prises en compte. Ce ne sera en revanche pas le cas des
  # rénovations thermiques, nous avons corrigé la bdd des gros travaux de rénovations en Step_0.4
  for (Y in 2011:(MatisseParams$horizon-1)){

    # Mat_gain_ener
    # Extraction de la conso moyenne au m2 en kWH par classe DPE
    conso_moy_dep <- as.data.frame(array2list(LETTERS[1:7]))
    for (i in LETTERS[1:7]){
      conso_moy_dep[i] <- as.numeric(ThreeME %>%
                                     filter(Var == paste("ENER_BUIL_H01_C",i,"_2*11630/BUIL_H01_C",i,"_2",sep="")) %>%
                                     filter(year==Y) %>%
                                     select(value))
    }
    Mat_gain_ener <- get_gain_energie(conso_moy_dep)

    #Nouvelles constructions selon 3ME
    NEWBUIL_H01_2_Y<-
      as.numeric(
        ThreeME %>%
          filter(Var=="NEWBUIL_H01_2") %>%
          filter(year==Y) %>%
          select(value))

    # Bascule vers achat neuf en A, B et C
    for (arr in LETTERS[1:3]){
      #On récupère le stock de m² à acheter et on interdit les achats sans changement de classe
      stock_m2_trans <- get(paste("NEWBUIL_H01_C",arr,"_2",sep = "")) %>% filter(year == Y) %>% select(value)
      if(as.numeric(stock_m2_trans[1]) > 0){
        menage_echelle$kWh_rank[which(menage_echelle$DPE_dep == arr)] <- NA

        #On sélectionne les ménages jusqu'à dépasser l'objectif de m²
        menage_echelle <- menage_echelle[order(menage_echelle$kWh_rank),]
        surf_cumsum <- cumsum(menage_echelle$surfhab_d  * menage_echelle$pondmen)
        l_idx <- which(surf_cumsum >= as.numeric(stock_m2_trans[1]))
        l_idx <- 1:l_idx[1]
        menage_echelle$NEUF[l_idx] <- TRUE
        menage_echelle$classe_arr[l_idx] <- arr
        menage_echelle$year_neuf[l_idx] <- Y
        menage_echelle$kWh_rank[l_idx] <- NA
        #On réordonne par ident_men
        menage_echelle <- menage_echelle[order(menage_echelle$ident_men),]
      }
    }

    # Modification des budgets ------------------------------------------------
    for (dep in LETTERS[2:7]){
      for (arr in LETTERS[1:3]){
        #Gain par changement
        rate_gain_ener<-as.numeric(
          Mat_gain_ener %>%
            filter(DPE_before==dep) %>%
            filter(DPE_after==arr) %>%
            select(value))

        #s'il existe (dim>0) un ménage sélectionné pour acheter du neuf de classe arr (classe_arr==arr) à l'année (year_neuf==7)
        #partant de la classe dep (DPE_dep==dep), on lui applique les gains énergétiques (rate_gain_ener)
        achat_idx <- which(menage_echelle$year_neuf == Y &
                             menage_echelle$DPE_dep == dep &
                             menage_echelle$classe_arr == arr)
        if(length(achat_idx>0)){
          menage_echelle[achat_idx, list_source_usage] <- menage_echelle[achat_idx, list_source_usage] * (1+rate_gain_ener)
        }
      }
    }
  }

# Solde énergétique et ventilation --------------------------------------------------------------
  # Mise à jour des totaux : pour chaque source, dep_source_verif reprend la somme des dépenses de source_usages
  # Due à la fusion Sources et Dep_sources sont redondants, la mise à jour de Sources permet de déduire facilement le solde
  # sur toutes les sources d'énergie
  for(i in 1:length(sources)){
    menage_echelle[,dep_sources_verif[i]] <- rowSums(menage_echelle %>% select(all_of(list_source_usage)) %>% select(starts_with(sources[i])))
  }
  menage_echelle$solde_ener <- rowSums(menage_echelle[,dep_sources_verif]) - rowSums(menage_echelle[,dep_sources])
  menage_echelle[, dep_sources] <- menage_echelle[, dep_sources_verif]
  solde <- menage_echelle %>% mutate(solde = solde_ener) %>% select(ident_men, solde)
  menage_echelle$NEUF[which(menage_echelle$year_neuf > 0)] <- TRUE

  #Ventilation et mise à jour des énergies domestiques
  menage_echelle <- ventilate_solde(menage_echelle, solde, FC, step_ventil = "REHAB")

  menage_ener_dom <- get_energie_dom_surf(menage_echelle, FC, F)
  menage_echelle <-
    menage_echelle %>%
    select(-ener_dom_surf, -ener_dom, -energie_tot_surf) %>%
    left_join(menage_ener_dom,by="ident_men")

# Succes ------------------------------------------------------------------
  print("Step D / 2_achat_neuf_inter : SUCCESS")
  if(save_intermed_file){save(menage_echelle, file = MatisseFiles$menage_echelle_D2_rd)}
  return(menage_echelle)

}



