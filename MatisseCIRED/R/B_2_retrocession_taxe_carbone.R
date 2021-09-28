# Objectif : fonction de rétrocession de la taxe carbone en fonction de
# redistribution, le paramètre de type de redist

#' @title retrocession_taxe_carbone
#' @description The function that calculates the retrocession of carbon taxe to households
#'
#' @param menage a menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_1_rd
#'
#' @return A menage dataframe
#' @export
#'
#' @examples
#' retrocession_taxe_carbone(menage, FALSE)
#'
retrocession_taxe_carbone <- function(menage, save_intermed_file = F){

# Data --------------------------------------------------------------------
  #Données IMACLIM
  load(MatisseFiles$IMACLIM_rd)

  # Taxe carbone totale prélevée aux ménages et rétrocédée
  TCO_tot <- as.numeric(IMACLIM %>%
                          filter(year == MatisseParams$horizon) %>%
                          filter(model == "IMACLIM") %>%
                          filter(Variable == "TCO_RTCD_tot") %>%
                          select(value)) * 10^6

  # Taxe carbone €/tonne CO2eq
  TCO <- as.numeric(IMACLIM %>%
                      filter(year == MatisseParams$horizon) %>%
                      filter(model== "IMACLIM") %>%
                      filter(Variable == "TCO") %>%
                      select(value)) * 10^6
  menage_echelle <- menage

  #Facteurs d'ajustement pour les retrocession dec et tuu (garantit 90% de surcomp sur les premiers déciles/tuu et une redistribution exponentielle ensuite)
  load(MatisseFiles$menage_forme_A_rd)
  facteurs <-MatisseCIRED:::calcul_facteurs_dec_tuu(menage_forme = menage_forme)

  #TCO par UC => TCO à redistribuer par unité de consommation en moyenne
  TCO_tot_UC <- TCO_tot / as.numeric(menage_echelle %>%
                                     summarise(sum(pondmen * coeffuc)))

  #RDB total
  RDB_tot <- as.numeric(menage_echelle %>%
                        summarise(sum(pondmen * RDB)))

  #Application des différentes formules de rétrocession en fonction de la variable redistribution
  if(MatisseParams$redistribution == "ssrec"){ #Pas de retrocession
    menage_echelle <-
      menage_echelle %>%
      mutate(rev_TCO = 0)
  }
  if(MatisseParams$redistribution == "forfait"){ #Retrocession forfaitaire par UC
    sum_pond <- as.numeric(menage_echelle %>%
                           summarise(sum(pondmen * coeffuc)))
    menage_echelle <-
      menage_echelle %>%
      mutate(rev_TCO = coeffuc / sum_pond * TCO_tot)
  }
  if(MatisseParams$redistribution == "niveau_vie"){ #Rétrocession niveau_vie sur rev_tot=RDBAI
    #rétrocession niveau_vie sur rev_tot=RDBAI
    #=> on est bien neutre sur le plan distribution du revenu
    #rétrocession neutre (et pas la réforme)
    RDBAI_UC_tot <- as.numeric(menage_echelle %>%
                               summarise(sum(pondmen * RDBAI / coeffuc)))
    menage_echelle <-
      menage_echelle %>%
      mutate(rev_TCO = RDBAI / coeffuc / RDBAI_UC_tot * TCO_tot)
  }
  if(MatisseParams$redistribution == "decile"){ #Retrocession par décile
    Tab_dec<-
      menage_echelle %>%
      dplyr::group_by(decuc2) %>%
      summarise(sum(pondmen * coeffuc))

    Tab_dec <-
      Tab_dec %>%
      mutate(decuc2 = as.numeric(decuc2) - 1)
    #pour se rapprocher de la solution avec tuu, on décale les déciles d'un.
    #Le décile 1, devient 0, annule la pente de la régression

    # Emissions agrégées par UC
    # Tab_emissions<-menage_forme_2010%>%group_by(decuc2)%>%summarise(sum(pondmen*TCO_paid/coeffuc))
    # Tab_dec[i,2]=> la somme des UC sur le décile, on exclut le décile 10. Le décile 1 va toucher une somme égale à sa somme des UC multipliée par le versement par
    # UC du décile 1 défini ci-avant par   facteurs[["dec"]] fois TCO_tot_UC, qui est le versement moyen par UC dans toute la pop.
    # on épuise l'ensemble de la TCO à rétrocéder (TCO_tot) d'où la racine
    x_dec= bisect(function(x) (Tab_dec[1, 2] * (1 - x)^0 + #décile 1
                               Tab_dec[2, 2] * (1 - x)^1 +
                               Tab_dec[3, 2] * (1 - x)^2 +
                               Tab_dec[4, 2] * (1 - x)^3 +
                               Tab_dec[5, 2] * (1 - x)^4 +
                               Tab_dec[6, 2] * (1 - x)^5 +
                               Tab_dec[7, 2] * (1 - x)^6 +
                               Tab_dec[8, 2] * (1 - x)^7 +
                               Tab_dec[9, 2] * (1 - x)^8) * #décile 9
                               as.numeric(facteurs[["dec"]] * TCO_tot_UC) - TCO_tot, 0, 1)$root
    # x_dec=0.1825126

    #fonction de distribution avec i le decuc2
    distrib <- function(i, coeffuc){
      i <- as.double(i) - 1  #on translate le decuc2 pour être raccord avec plus haut
      ifelse(i == 9, return(0), return(coeffuc * ((1 - x_dec) ** i) *
                                         as.numeric(TCO_tot_UC *   facteurs[["dec"]])))
    }

    menage_echelle <-
      menage_echelle %>%
      group_by(1:n()) %>% #on groupe ménage par ménage
      mutate(rev_TCO = as.numeric(distrib(i = decuc2, coeffuc = as.numeric(coeffuc)))) %>%
      ungroup()

    menage_echelle <-
      menage_echelle %>%
      ungroup()
  }
  if(MatisseParams$redistribution == "tuu"){ #Retrocession par tuu, sauf Paris (tuu=9) (idem décile)

    Tab_tuu<-
      menage_echelle %>%
      dplyr::group_by(tuu) %>%
      summarise(sum(pondmen * coeffuc))

    x_tuu= bisect(function(x) (Tab_tuu[1, 2] * (1 - x)^0 +
                               Tab_tuu[2, 2] * (1 - x)^1 +
                               Tab_tuu[3, 2] * (1 - x)^2 +
                               Tab_tuu[4, 2] * (1 - x)^3 +
                               Tab_tuu[5, 2] * (1 - x)^4 +
                               Tab_tuu[6, 2] * (1 - x)^5 +
                               Tab_tuu[7, 2] * (1 - x)^6 +
                               Tab_tuu[8, 2] * (1 - x)^7) *
                               as.numeric(  facteurs[["tuu"]] * TCO_tot_UC) - TCO_tot, 0, 1)$root

    # x_tuu=0.490601
    distrib <- function(i, coeffuc){
      i <- as.double(i)
      ifelse(i == 8, return(0), return(coeffuc * ((1 - x_tuu) ** i) *
                                         as.numeric(TCO_tot_UC *   facteurs[["tuu"]])))
    }

    menage_echelle <-
      menage_echelle %>%
      group_by(1:n()) %>%
      mutate(rev_TCO = distrib( , as.numeric(coeffuc))) %>%
      ungroup()

    menage_echelle <-
      menage_echelle %>%
      ungroup()
  }
  if(MatisseParams$redistribution == "seq_dectuu"){

    #Premier step : idem redistribution == decile
    Tab_dec<-
      menage_echelle %>%
      dplyr::group_by(decuc2) %>%
      summarise(sum(pondmen * coeffuc))

    Tab_dec <-
      Tab_dec %>%
      mutate(decuc2 = as.numeric(decuc2) - 1)
    x_dec= bisect(function(x) (Tab_dec[1, 2] * (1 - x)^0 + #décile 1
                                 Tab_dec[2, 2] * (1 - x)^1 +
                                 Tab_dec[3, 2] * (1 - x)^2 +
                                 Tab_dec[4, 2] * (1 - x)^3 +
                                 Tab_dec[5, 2] * (1 - x)^4 +
                                 Tab_dec[6, 2] * (1 - x)^5 +
                                 Tab_dec[7, 2] * (1 - x)^6 +
                                 Tab_dec[8, 2] * (1 - x)^7 +
                                 Tab_dec[9, 2] * (1 - x)^8) * #décile 9
                    as.numeric(facteurs[["dec"]] * TCO_tot_UC) - TCO_tot, 0, 1)$root

    #fonction de distribution avec i le decuc2
    distrib <- function(i, coeffuc){
      i <- as.double(i) - 1  #on translate le decuc2 pour être raccord avec plus haut
      ifelse(i == 9, return(0), return(coeffuc * ((1 - x_dec) ** i) *
                                         as.numeric(TCO_tot_UC *   facteurs[["dec"]])))
    }

    menage_echelle <-
      menage_echelle %>%
      group_by(1:n()) %>% #on groupe ménage par ménage
      mutate(rev_TCO_dec = as.numeric(distrib(i = decuc2, coeffuc = as.numeric(coeffuc)))) %>%
      ungroup()


    #Deuxième step : on répartit la TC obtenue dans chaque décile parmi les TUU
    dec_vec <- sort(unique(menage_echelle$decuc2))
    tuu_vec <- sort(unique(menage_echelle$tuu))
    #Choix d'un vecteur de distribution par tuu
    tuu_df <- data.frame(tuu = tuu_vec)
    tuu_df$scale <- (9 - tuu_df$tuu) ^ 3
    tuu_df <- tuu_df %>% left_join(menage_echelle %>% group_by(tuu) %>% summarise(sum_pond_uc = sum(pondmen * coeffuc)))
    tuu_df$weight_pond_uc <- tuu_df$sum_pond_uc / sum(tuu_df$sum_pond_uc)
    tuu_df$factor <- tuu_df$scale * tuu_df$weight_pond_uc / (sum(tuu_df$scale * tuu_df$weight_pond_uc))

    res_df <- tibble()
    for(dec_i in dec_vec){
      dec_tuu_df <- tuu_df
      sub_men <- menage_echelle %>% filter(decuc2 == dec_i)
      TC_rev_temp <- sum(sub_men$rev_TCO_dec * sub_men$pondmen)
      dec_tuu_df <- dec_tuu_df %>% mutate(TC_tuu = TC_rev_temp * factor)
      dec_tuu_df <- dec_tuu_df %>%
                    left_join(sub_men %>% group_by(tuu) %>% summarise(subsum_pond_uc = sum(pondmen * coeffuc)), by = "tuu") %>%
                    mutate(TC_uc = TC_tuu / subsum_pond_uc)
      sub_men$TC_uc <- dec_tuu_df$TC_uc[match(sub_men$tuu, dec_tuu_df$tuu)]
      sub_men <- sub_men %>% mutate(rev_TCO = TC_uc * coeffuc)
      res_df <- rbind(res_df, sub_men %>% select(ident_men, rev_TCO))
    }

    menage_echelle$rev_TCO <- res_df$rev_TCO[match(menage_echelle$ident_men, res_df$ident_men)]

    print(paste("Somme TC"  = sum(menage_echelle$rev_TCO * menage_echelle$pondmen)))
    menage_echelle <- menage_echelle %>% select(- rev_TCO_dec)
  }
  if(MatisseParams$redistribution == "dectuu_seq_ucmat"){

    #Premier step : idem redistribution == decile
    Tab_dec<-
      menage_echelle %>%
      dplyr::group_by(decuc2) %>%
      summarise(sum(pondmen * coeffuc))

    Tab_dec <-
      Tab_dec %>%
      mutate(decuc2 = as.numeric(decuc2) - 1)
    x_dec= bisect(function(x) (Tab_dec[1, 2] * (1 - x)^0 + #décile 1
                                 Tab_dec[2, 2] * (1 - x)^1 +
                                 Tab_dec[3, 2] * (1 - x)^2 +
                                 Tab_dec[4, 2] * (1 - x)^3 +
                                 Tab_dec[5, 2] * (1 - x)^4 +
                                 Tab_dec[6, 2] * (1 - x)^5 +
                                 Tab_dec[7, 2] * (1 - x)^6 +
                                 Tab_dec[8, 2] * (1 - x)^7 +
                                 Tab_dec[9, 2] * (1 - x)^8) * #décile 9
                    as.numeric(  facteurs[["dec"]] * TCO_tot_UC) - TCO_tot, 0, 1)$root

    #fonction de distribution avec i le decuc2
    distrib <- function(i, coeffuc){
      i <- as.double(i) - 1  #on translate le decuc2 pour être raccord avec plus haut
      ifelse(i == 9, return(0), return(coeffuc * ((1 - x_dec) ** i) *
                                         as.numeric(TCO_tot_UC *   facteurs[["dec"]])))
    }

    menage_echelle <-
      menage_echelle %>%
      group_by(1:n()) %>% #on groupe ménage par ménage
      mutate(rev_TCO_dec = as.numeric(distrib(i = decuc2, coeffuc = as.numeric(coeffuc)))) %>%
      ungroup()


    #Deuxième step : on répartit la TC obtenue dans chaque décile parmi les TUU
    dec_vec <- sort(unique(menage_echelle$decuc2))
    tuu_vec <- sort(unique(menage_echelle$tuu))
    #Choix d'un vecteur de distribution par tuu
    tuu_df <- data.frame(tuu = tuu_vec)
    tuu_df$scale <- (9 - tuu_df$tuu) ^ 0.5
    tuu_df <- tuu_df %>% left_join(menage_echelle %>% group_by(tuu) %>% summarise(sum_pond_uc = sum(pondmen * coeffuc)))
    tuu_df$weight_pond_uc <- tuu_df$sum_pond_uc / sum(tuu_df$sum_pond_uc)
    tuu_df$factor <- tuu_df$scale * tuu_df$weight_pond_uc / (sum(tuu_df$scale * tuu_df$weight_pond_uc))

    res_df <- tibble()
    for(dec_i in dec_vec){
      sub_men <- menage_echelle %>% filter(decuc2 == dec_i)

      #Choix d'un vecteur de distribution par tuu
      tuu_df <- data.frame(tuu = tuu_vec)
      tuu_df$scale <- (9 - tuu_df$tuu) ^ 0.5
      tuu_df <- tuu_df %>% left_join(sub_men %>% group_by(tuu) %>% summarise(sum_pond_uc = sum(pondmen * coeffuc)))
      tuu_df$weight_pond_uc <- tuu_df$sum_pond_uc / sum(tuu_df$sum_pond_uc)
      tuu_df$factor <- tuu_df$scale * tuu_df$weight_pond_uc / (sum(tuu_df$scale * tuu_df$weight_pond_uc))
      dec_tuu_df <- tuu_df

      TC_rev_temp <- sum(sub_men$rev_TCO_dec * sub_men$pondmen)
      dec_tuu_df <- dec_tuu_df %>% mutate(TC_tuu = TC_rev_temp * factor)
      dec_tuu_df <- dec_tuu_df %>%
        left_join(sub_men %>% group_by(tuu) %>% summarise(subsum_pond_uc = sum(pondmen * coeffuc)), by = "tuu") %>%
        mutate(TC_uc = TC_tuu / subsum_pond_uc)
      sub_men$TC_uc <- dec_tuu_df$TC_uc[match(sub_men$tuu, dec_tuu_df$tuu)]
      sub_men <- sub_men %>% mutate(rev_TCO = TC_uc * coeffuc)
      res_df <- rbind(res_df, sub_men %>% select(ident_men, rev_TCO))
    }

    menage_echelle$rev_TCO <- res_df$rev_TCO[match(menage_echelle$ident_men, res_df$ident_men)]

    print(paste("Somme TC"  = sum(menage_echelle$rev_TCO * menage_echelle$pondmen)))
    menage_echelle <- menage_echelle %>% select(- rev_TCO_dec)
  }
  if(MatisseParams$redistribution == "vector_dectuu"){

    #Rétrocession selon les vecteurs dec puis tuu par UC
    vec_dec <- MatisseParams$vec_dec
    vec_tuu <- MatisseParams$vec_tuu
    vec_dec <- vec_dec / sum(vec_dec)
    vec_tuu <- vec_tuu / sum(vec_tuu)

    #Matrice de comptage des UC par catégorie dec/tuu (somme à 100% sur chaque dec)
    pctuc_perdec_mat <- matrix(NA, nrow = length(vec_dec), ncol = length(vec_tuu))
    sum_dectuu <- menage_echelle %>%
                  group_by(decuc2, tuu) %>%
                  dplyr::summarise(sum_uc = sum(pondmen * coeffuc)) %>%
                  ungroup()

    sum_dec <- sum_dectuu %>%
      group_by(decuc2) %>%
      dplyr::summarise(sum_uc = sum(sum_uc)) %>%
      ungroup()

    for(dec_it in 1:length(vec_dec)){
      for(tuu_it in 1:(length(vec_tuu))){
        pctuc_perdec_mat[dec_it, tuu_it] <-
          as.numeric(sum_dectuu %>% filter(decuc2 == all_of(dec_it), tuu == (all_of(tuu_it) - 1)) %>% select(sum_uc)) /
          as.numeric(sum_dec %>% filter(decuc2 == all_of(dec_it)) %>% select(sum_uc))
      }
    }
    vec_sumtuu <- pctuc_perdec_mat %*% vec_tuu

    #Matrice de rétrocession de la TC en pct dec/tuu
    retropct_mat <- matrix(NA, nrow = length(vec_dec), ncol = length(vec_tuu))
    for(dec_it in 1:length(vec_dec)){
      for(tuu_it in 1:(length(vec_tuu))){
        retropct_mat[dec_it, tuu_it] <-
          vec_dec[dec_it] * vec_tuu[tuu_it] * pctuc_perdec_mat[dec_it, tuu_it] / vec_sumtuu[dec_it]
      }
    }

    #Matrice du nombre d'uc par catégorie
    nbuc_mat <- matrix(NA, nrow = length(vec_dec), ncol = length(vec_tuu))
    for(dec_it in 1:length(vec_dec)){
      for(tuu_it in 1:(length(vec_tuu))){
        nbuc_mat[dec_it, tuu_it] <-
          as.numeric(sum_dectuu %>% filter(decuc2 == all_of(dec_it), tuu == all_of(tuu_it) -1 ) %>% select(sum_uc))
      }
    }

    #Matrice de rétrocession en euros par UC
    retronom_per_uc_mat <- matrix(NA, nrow = length(vec_dec), ncol = length(vec_tuu))
    for(dec_it in 1:length(vec_dec)){
      for(tuu_it in 1:(length(vec_tuu))){
        retronom_per_uc_mat[dec_it, tuu_it] <-
          retropct_mat[dec_it, tuu_it] * TCO_tot / nbuc_mat[dec_it, tuu_it]
      }
    }

    sum_dectuu$retronom_per_uc <- as.numeric(lapply(1:nrow(sum_dectuu), function(x){
      return(retronom_per_uc_mat[sum_dectuu$decuc2[x], sum_dectuu$tuu[x]+1])
    }))
    menage_echelle <- menage_echelle %>%
      left_join(sum_dectuu %>% select(decuc2, tuu, retronom_per_uc), by = c("decuc2", "tuu"))
    menage_echelle <-
      menage_echelle %>%
      mutate(rev_TCO = retronom_per_uc * coeffuc) %>%
      select (-retronom_per_uc)

    #Check de somme totale :
    #sum(menage_echelle$pondmen * menage_echelle$rev_TCO)
    #TCO_tot

  }

  menage_echelle$RDB <- menage_echelle$RDB + menage_echelle$rev_TCO


# Succes ------------------------------------------------------------------
  print("Step B / 2_retrocession_taxe_carbone : SUCCESS")
  if(save_intermed_file){save(menage_echelle, file = MatisseFiles$menage_echelle_B2_rd)}
  return(menage_echelle)

}

calcul_facteurs_dec_tuu <- function(menage_forme){


# Data --------------------------------------------------------------------
  load(MatisseFiles$EMS_scen_rd)
  EMS <- EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")



# Calcul des facteurs -----------------------------------------------------
  # La règle retenue est celle d’une compensation par UC pour le premier décile reflétant l’écart, calculé en 2010, entre les émissions de carbone directes moyennes par UC sur l’ensemble des ménages, et la médiane des mêmes émissions par UC du décile 1.
  #
  # La rétrocession est ainsi étalonnée sur les dernières statistiques disponibles pour compenser entièrement la moitié des ménages du premier décile des surcoûts engendrés par la réforme calculés ex ante, c’est-à-dire sans prendre en compte les stratégies d’adaptation.
  # L’écart en question, de 2.31, correspond par construction à l’écart à la somme qui serait rétrocédée aux ménages du décile 1 en option de rétrocession forfaitaire (ils touchent 2.31 fois plus). Les rétrocessions des déciles 1 à 9 sont alors calculées pour chaque scénario, à chaque horizon et au fil des itérations macro-micro sous l’hypothèse que chaque décile touche une même fraction inférieure à 1 de ce que touche le décile inférieur, de sorte à solder le total de taxe à rétrocéder.
  # prix constant 2010 tCO2/ dépenses par cat
  coeff_CL_2010 <- as.numeric(EMS %>% filter(year == 2010) %>% filter(Var == "EMS_HH_21_2") %>% select(emission)) /
                  as.numeric(menage_forme %>% summarise(sum(pondmen * dep_Solides))) # en tC02

  coeff_Oil_2010 <- as.numeric(EMS %>% filter(year == 2010) %>% filter(Var == "EMS_HH_22_2") %>% select(emission)) /
                    as.numeric(menage_forme %>% summarise(sum(pondmen * (carb_lubr + dep_Fuel + dep_GPL)))) # en tC02/Millions €

  coeff_Gaz_2010 <- as.numeric(EMS %>% filter(year == 2010) %>% filter(Var == "EMS_HH_24_2") %>% select(emission)) /
                    as.numeric(menage_forme %>% summarise(sum(pondmen * (dep_Gaz + dep_Urbain)))) #

  menage_forme <-
    menage_forme %>%
    mutate(ems_CL = 0) %>%
    mutate(ems_Oil = (carb_lubr + dep_Fuel + dep_GPL) * coeff_Oil_2010) %>%
    mutate(ems_Gaz = (dep_Gaz + dep_Urbain + dep_Solides) * coeff_Gaz_2010)
  menage_forme <-
    menage_forme %>%
    mutate(emissions = ems_CL + ems_Oil + ems_Gaz) %>%
    mutate(ems_uc = emissions / coeffuc)

  ems_uc_tot <-
    menage_forme %>%
    summarise(weighted.mean(x = ems_uc, w = pondmen))

  facteurs <- list()

# Décile ------------------------------------------------------------------
  men_dec_1 <-
    menage_forme %>%
    filter(decuc2 == 1) %>%
    summarise(wtd.quantile(x = ems_uc, q = 0.95, weight = pondmen))

  facteurs[["dec"]] <- as.numeric(men_dec_1/ems_uc_tot)
  #2.172059 après FixCharb

# TUU ---------------------------------------------------------------------
  men_tuu_0 <-
    menage_forme %>%
    filter(tuu == 0) %>%
    summarise(wtd.quantile(x = ems_uc, q = 0.95, weight = pondmen))

  facteurs[["tuu"]] <- as.numeric(men_tuu_0/ems_uc_tot)
  # 3.294676 après FixCharb

  return(facteurs)

}


