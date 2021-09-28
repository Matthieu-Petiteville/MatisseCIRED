
#' @title calcule_contraintes
#' @description This functions calculates the blocking values for the reweighting solver
#'
#' @param menage_echelle A menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_contraintes_rd
#'
#' @return A menage dataframe
#'
#' @examples
#' calcule_contraintes(menage, FALSE)
#'
calcule_contraintes <- function(menage, save_intermed_file = F){

# Data ----------------------------------------------------------------------------------------------------------------------------------------------------
  #Menage
  menage_echelle <- menage
  load(MatisseFiles$menage_forme_A_rd)
  menage_calibr_2010 <- menage_forme
  individu <- suppressWarnings(read_excel(MatisseFiles$indiv_bdf_xl))
  load(MatisseFiles$IMACLIM_rd)
  load(MatisseFiles$Threeme_rd)
  men_INSEE <- suppressWarnings(read_excel(MatisseFiles$insee_proj_men_xl, sheet = "men"))

  #Vente VP
  ventes_VP <- suppressMessages(read_excel(path = MatisseFiles$ventes_vp_xl))
  ventil_VP <- as.numeric(ventes_VP %>% filter(Year == MatisseParams$horizon) %>% select(Particuliers))


# Correction stock véhicules ------------------------------------------------------------------------------------------------------------------------------
  #Ventes des véhicules
  Ventes <-
    ThreeME %>%
    filter(Var %in% c("NEWAUTO_TH_H01_2", "NEWAUTO_ELEC_H01_2")) %>%
    mutate(Year = as.numeric(year)) %>%
    filter(Year <= 2035 & Year >= 2010) %>%
    select(year, Var, value) %>%
    distinct() %>%
    spread(key = Var, value = value) %>%
    mutate(Ventes_VP = NEWAUTO_TH_H01_2 + NEWAUTO_ELEC_H01_2) %>%
    mutate(Ventes_VP = Ventes_VP * 1000) %>%
    mutate(Year = as.numeric(year)) %>%
    select(Year, Ventes_VP) %>%
    left_join(ventes_VP, by = "Year") %>%
    mutate(Ventes_Particuliers = Ventes_VP * Particuliers)
  #Stock de véhicules
  Stock_VP <-
    ThreeME %>%
    filter(Var == "AUTO_H01_2") %>%
    mutate(Year = as.numeric(year)) %>%
    filter(Year <= 2035 & Year >= 2010) %>%
    select(Year, value)
  # Stock des véhicules particuliers détenus pas des particuliers en 2010
  Stock_VP_P <- as.numeric(menage_forme %>% summarise(sum(nbvehic * pondmen, na.rm = T)))
  part_VP <- Stock_VP_P / as.numeric(Stock_VP %>% filter(Year == 2010) %>% select(value)) / 1000
  Ventes_particuliers <- as.numeric(Ventes %>% filter(Year == 2010) %>% select(Ventes_Particuliers))
  Stock_list <- c(2010, Stock_VP_P, part_VP, Ventes_particuliers)

  #Hypothese ThreeME : durée de vie de 13 ans des véhicules de toute sorte
  for (Y in 2011:MatisseParams$horizon) {
    # Stock de VP des Particuliers
    Stock_VP_P <- Stock_VP_P * (1 - 1 / 13) + as.numeric(Ventes %>% filter(Year == Y) %>% select(Ventes_Particuliers))
    # Part du stock de VP
    part_VP <- Stock_VP_P / as.numeric(Stock_VP %>% filter(Year == Y) %>% select(value)) / 1000
    Ventes_particuliers <- as.numeric(Ventes %>% filter(Year == Y) %>% select(Ventes_Particuliers))
    Stock_list <- rbind(Stock_list, c(Y, Stock_VP_P, part_VP, Ventes_particuliers))
  }
  Stock_list <- as.data.frame(Stock_list, row.names = FALSE)
  colnames(Stock_list) <-
    c("Year",
      "Stock_VP_Particuliers",
      "Part_VP_Particuliers",
      "Ventes_particuliers")
  Stock_VP_Particuliers_horizon <-
    as.numeric(Stock_list %>% filter(Year == MatisseParams$horizon) %>% select(Stock_VP_Particuliers))


# Ajout Data DPE ----------------------------------------------------------
  #On ajoute les quantités de m² par classe DPE qui serviront de contrainte
  menage_echelle$DPE_horizon <- as.factor(menage_echelle$DPE_horizon)
  dummies_dpe_horizon <-
    model.matrix(~ DPE_horizon,
                 data = menage_echelle,
                 contrasts.arg = list(DPE_horizon = contrasts(menage_echelle$DPE_horizon, contrasts = F)))[,-1]
  colnames(dummies_dpe_horizon) <- paste("DPE_m2", LETTERS[1:7], sep = "_")
  dummies_dpe_horizon <- data.frame("ident_men" = menage_echelle$ident_men, dummies_dpe_horizon)
  menage_echelle <-
    menage_echelle %>%
    left_join(dummies_dpe_horizon, by = 'ident_men') %>%
    mutate_at(vars(starts_with("DPE_m2_")), function(x)
      as.numeric(x * menage_echelle$surfhab_d))


# Type des ménages ----------------------------------------------------------------------------------------------------------------------------------------
  #Contraintes_vec contient les colonnes sur lesquelles les contrainte vont s'appliquer
  contraintes_vec <- c("MI_corr")

  # Colonne typmen indiquant le type de ménage
  menage_calibr_2010$typmen5 <- as.factor(menage_calibr_2010$typmen5)
  dummies_typmen <- model.matrix(~ typmen5,
                 data = menage_calibr_2010,
                 contrasts.arg = list(typmen5 = contrasts(menage_calibr_2010$typmen5, contrasts = F)))[,-1]
  menage_calibr_2010 <- cbind(menage_calibr_2010, dummies_typmen)
  contraintes_vec <- c(contraintes_vec, colnames(dummies_typmen)[1:(ncol(dummies_typmen)-1)])


  #Colonnes indicatrices des vagues d'enquêtes (vague_X)
  for(vague in 1:5){
    col_name <- paste("vague_", vague, sep = "")
    menage_calibr_2010[, col_name] <- ifelse(menage_calibr_2010$vag == vague, 1, 0)
    contraintes_vec <- c(contraintes_vec, col_name)
  }

  #Colonnes indicatrices d'agePR (découpage de la population en plusieurs tranches d'âge)
  age_vec <- c(0,30,45,60,75)
  for(i in 2:length(age_vec)){
    col_name <- paste("agePR", age_vec[i-1], age_vec[i]-1, sep = "_")
    menage_calibr_2010[col_name] <- ifelse(menage_calibr_2010$agepr >= age_vec[i-1] & menage_calibr_2010$agepr < age_vec[i], 1, 0)
    contraintes_vec <- c(contraintes_vec, col_name)
  }

  #Colonnes indicatrices de la ZEAT
  # Méthodo : ZEAT regroupée (8 modalités : 7 France métropole) : région parisienne + bassin parisien, Nord, Est, Ouest, Sud-Ouest, Centre-Est, Méditerranée
  menage_calibr_2010["ZEAT_Paris"] <- ifelse(menage_calibr_2010$zeat == "1" | menage_calibr_2010$zeat == "2", 1, 0)
  menage_calibr_2010["ZEAT_Nord"] <- ifelse(menage_calibr_2010$zeat == "3", 1, 0)
  menage_calibr_2010["ZEAT_Est"] <- ifelse(menage_calibr_2010$zeat == "4", 1, 0)
  menage_calibr_2010["ZEAT_Ouest"] <- ifelse(menage_calibr_2010$zeat == "5", 1, 0)
  menage_calibr_2010["ZEAT_SudOuest"] <- ifelse(menage_calibr_2010$zeat == "7", 1, 0)
  menage_calibr_2010["ZEAT_CentreEst"] <- ifelse(menage_calibr_2010$zeat == "8", 1, 0)
  col_names <- colnames(menage_calibr_2010)
  contraintes_vec <- c(contraintes_vec, col_names[grep("ZEAT_", col_names)])

  #Colonnes indicatrices de la TUU
  menage_calibr_2010["tuu_corr"] <- factor(car::recode(menage_calibr_2010$tuu, "0:3 = 21 ; 4:6 = 22 ; 7 = 23 ; 8 = 24"))
  dummies_tuu <- model.matrix(~ tuu_corr, data = menage_calibr_2010, contrasts.arg = list(tuu_corr = contrasts(menage_calibr_2010$tuu_corr, contrasts = F)))[,-1]
  menage_calibr_2010 <- cbind(menage_calibr_2010,dummies_tuu)
  contraintes_vec <- c(contraintes_vec, colnames(dummies_tuu)[1:(ncol(dummies_tuu)-1)])


  #Colonnes indicatrices Age x Sexe
  # Croisement âge/sexe individus en 12 modalités (voir fichier Insee_Pop dans données brutes, issues projections 2013-2070)
  # 0-14 ans,	15-24 ans, 25-39 ans, 40-59 ans, 60-74 ans, 75 ans et +
  individu$ident_men <- as.numeric(individu$ident_men)
  individu_bis <- individu[which(individu$ident_men %in% menage_calibr_2010$ident_men),]
  sexe_df <- data.frame(sexe_num = 2:1, sexe_let = c("F", "M"))
  age_vec <- c(0, 15, 25, 40, 60, 75)
  col_name_vec <- c()
  for(j in 2:length(age_vec)){
    for(i  in 1:nrow(sexe_df)){
      col_name <- paste(sexe_df$sexe_let[i], age_vec[j - 1], age_vec[j] - 1, sep = "_")
      col_name_vec <- c(col_name_vec, col_name)
      individu_bis[col_name] <- ifelse(individu_bis$age >= age_vec[j - 1] &
                                             individu_bis$age < age_vec[j] &
                                             individu_bis$sexe == sexe_df$sexe_num[i], 1, 0)
    }
  }
  col_name <- paste("F", age_vec[j], "plus", sep = "_")
  col_name_vec <- c(col_name_vec, col_name)
  individu_bis[col_name] <- ifelse(individu_bis$age >= age_vec[j] & individu_bis$sexe == 2, 1, 0)
  menage_calibr_2010[col_name_vec] <- 0
  for (col_name in  col_name_vec) {
    ind <- individu_bis %>%
      mutate(agesexe = get(col_name)) %>%
      select(ident_men, agesexe) %>%
      dplyr::group_by(ident_men) %>%
      dplyr::summarise(sum = sum(agesexe))
    menage_calibr_2010[col_name] <- ind$sum
  }
  contraintes_vec <- c(contraintes_vec, col_name_vec)

# Calage Macro ------------------------------------------------------------
  #Calage sur les grands indicateurs de IMACLIM
  Calage <-
    IMACLIM %>%
    filter(Categorie == "CALAGE") %>%
    filter(year == MatisseParams$horizon)
  Calage_relatif <-
    Calage %>%
    filter(model == "IMACLIM")
  for (i in 1:nrow(Calage_relatif)) {
    var <- as.character(Calage_relatif[i, "Variable"])
    if (is.na(Calage_relatif[i, "value"])) {
      Calage_relatif[i, "value"] <-
        (Calage %>%
           filter(model == "ThreeME") %>%
           filter(Variable == var) %>%
           select(value))[1]
      Calage_relatif[i, "model"] <- "ThreeME"
    }
  }
  Calage_relatif <- Calage_relatif %>% mutate(value = as.numeric(value))


# contraintes -------------------------------------------------------------
  #Définit les contraintes de calage
  contraintes_calage <- Calage_relatif %>% select(Variable, value) %>% spread(key = Variable, value = value)
  # Réarranger dans ordre cohérent avec étape 4.2
  contraintes <-
    data.frame(
      "npers" = contraintes_calage$cal_pop_tot * sum(menage_calibr_2010$npers * menage_calibr_2010$pondmen),
      "pondmen" = contraintes_calage$cal_pop_tot * sum(menage_calibr_2010$npers * menage_calibr_2010$pondmen) /
        as.numeric(men_INSEE %>% filter(year == MatisseParams$horizon) %>% select(nb_pers_men)))
  FC_pondmen <- contraintes$pondmen / menage_calibr_2010 %>% summarise(sum(pondmen))


# Agregats INSEE ----------------------------------------------------------
  #Part pop SEXE x AGE + pondmen
  # Import des contraintes INSEE pop 2025
  pop_INSEE <- read_excel(MatisseFiles$insee_proj_pop_xl, sheet = "Pop")
  pop_INSEE <- pop_INSEE %>% gather(key = year, value = part_age_sexe,-c(1:2)) %>% mutate(cat_sexe_age = paste(Sexe, Age, sep = "_"))
  pop_INSEE_horizon <- pop_INSEE %>% filter(year == MatisseParams$horizon)

  # Création automatique des variables de part (e.g part_F_0_14)
  for (k in pop_INSEE_horizon$cat_sexe_age) {
    value <- as.numeric(pop_INSEE_horizon %>% filter(cat_sexe_age == k) %>% select(part_age_sexe))
    assign(k, value)
  }


# Parts des variables -------------------------------------------------------------------------------------------------------------------------------------
  #On définit un df qui contient toutes les parts des variables
  #Pour chaque variables de calage, calcul des parts dans la population => on ne cale pas sur les variables absolues mais en pourcentage
  part_df <- data.frame(contraintes = contraintes_vec, value = 0, value_pond = 0)
  for(i in 1:nrow(part_df)){
    part_df$value[i] <-
      sum(menage_calibr_2010[part_df$contraintes[i]] * menage_calibr_2010$pondmen) /
      sum(menage_calibr_2010$pondmen)
  }
  part_df$value_pond <- part_df$value * contraintes$pondmen

  #Exception pour les données de population, on s'aligne sur l'INSEE et la pondération est en nbpers
  match_insee_idx <- match(part_df$contraintes, pop_INSEE_horizon$cat_sexe_age)
  l_idx <- which(!(is.na(match_insee_idx)))
  part_df$value[l_idx] <- pop_INSEE_horizon$part_age_sexe[match_insee_idx[l_idx]]
  part_df$value_pond[l_idx] <- part_df$value[l_idx] * contraintes$npers

  #Ajout des valeurs aux contraintes
  contraintes[contraintes_vec] <- part_df$value_pond


# Ajout des contraintes 3ME -------------------------------------------------------------------------------------------------------------------------------
  #On ajoute aux contraintes macro précédentes les sorties de 3ME
  agreg_best <-
    contraintes %>%
    select(-npers) %>%
    mutate(
      "npers" =
        contraintes_calage$cal_pop_tot *
        sum(menage_calibr_2010$npers * menage_calibr_2010$pondmen, na.rm = T),

      "nbactoccup" =
        contraintes_calage$cal_act_occ *
        sum(menage_calibr_2010$nbactoccup * menage_calibr_2010$pondmen, na.rm = T),

      "nbchomeurs" =
        contraintes_calage$cal_chom *
        sum(menage_calibr_2010$nbchomeurs * menage_calibr_2010$pondmen, na.rm = T),

      "rev_activites_sans_etranger" =
        contraintes_calage$cal_revact *
        sum(menage_calibr_2010$rev_activites_sans_etranger * menage_calibr_2010$pondmen, na.rm = T),

      "rev_patrimoine" =
        contraintes_calage$cal_revpat *
        sum(menage_calibr_2010$rev_patrimoine * menage_calibr_2010$pondmen, na.rm = T),

      "chomage" =
        contraintes_calage$cal_revchom *
        sum(menage_calibr_2010$chomage * menage_calibr_2010$pondmen, na.rm = T),

      "rev_sociaux_autres" =
        contraintes_calage$cal_revsoc *
        sum(menage_calibr_2010$rev_sociaux_autres * menage_calibr_2010$pondmen, na.rm = T),

      "rev_etranger" =
        contraintes_calage$cal_revetr *
        sum(menage_calibr_2010$rev_etranger * menage_calibr_2010$pondmen, na.rm = T),

      "surfhab_d" =
        contraintes_calage$cal_m2 *
        sum(menage_calibr_2010$surfhab_d * menage_calibr_2010$pondmen, na.rm = T),

      "DPE_m2_A" =
        contraintes_calage$cal_m2_dpe_A *
        as.numeric(menage_calibr_2010 %>% filter(DPE_pred == "A") %>% summarise(sum(surfhab_d * pondmen))),
      "DPE_m2_B" =
        contraintes_calage$cal_m2_dpe_B *
        as.numeric(menage_calibr_2010 %>% filter(DPE_pred == "B") %>% summarise(sum(surfhab_d * pondmen))),
      "DPE_m2_C" =
        contraintes_calage$cal_m2_dpe_C *
        as.numeric(menage_calibr_2010 %>% filter(DPE_pred == "C") %>% summarise(sum(surfhab_d * pondmen))),
      "DPE_m2_D" =
        contraintes_calage$cal_m2_dpe_D *
        as.numeric(menage_calibr_2010 %>% filter(DPE_pred == "D") %>% summarise(sum(surfhab_d * pondmen))),
      "DPE_m2_E" =
        contraintes_calage$cal_m2_dpe_E *
        as.numeric(menage_calibr_2010 %>% filter(DPE_pred == "E") %>% summarise(sum(surfhab_d * pondmen))),
      "DPE_m2_F" =
        contraintes_calage$cal_m2_dpe_F *
        as.numeric(menage_calibr_2010 %>% filter(DPE_pred == "F") %>% summarise(sum(surfhab_d * pondmen))))

  # Ventes de véhicules depuis Matisse
  ventes_VE<-
    as.numeric((ThreeME %>%
                  filter(year==MatisseParams$horizon)%>%
                  filter(Var=="NEWAUTO_ELEC_H01_2")%>%
                  select(value))[1,]*ventil_VP*1000)
  ventes_VT<-
    as.numeric(ThreeME %>%
                 filter(year==MatisseParams$horizon)%>%
                 filter(Var=="NEWAUTO_TH_H01_2")%>%
                 select(value)*ventil_VP*1000)
  ventes_VE_adj <- sum(menage_echelle$pondmen[which(menage_echelle$new_VE)])
  ventes_VT_adj <- sum(menage_echelle$pondmen[which(menage_echelle$new_VT)])
  agreg_best <-
    agreg_best %>%
    mutate("ventes_VT" = all_of(ventes_VT)) %>%
    mutate("ventes_VE" = ventes_VE)



# Aggregation des contraintes -----------------------------------------------------------------------------------------------------------------------------
  #menage_contraintes est un sous-df qui contient les données pour chaque menage sur les contraintes sur lesquelles
  #la pondération sera faite
  menage_contraintes <-
    menage_calibr_2010 %>%
    select(ident_men,
           all_of(contraintes_vec))%>%
    left_join(
      menage_echelle %>%
        select(ident_men,
               npers,
               nbactoccup,
               nbchomeurs,
               rev_activites_sans_etranger,
               rev_patrimoine,
               chomage,
               rev_sociaux_autres,
               rev_etranger,
               surfhab_d,
               DPE_m2_A,
               DPE_m2_B,
               DPE_m2_C,
               DPE_m2_D,
               DPE_m2_E,
               DPE_m2_F,
               new_VT,
               new_VE), by="ident_men")

  menage_contraintes$pond_init <- menage_echelle$pondmen

  #Sauvegarde du fichier des contraintes agreg_best
  save(agreg_best, file = MatisseFiles$agreg_best_rd)

  # Succes --------------------------------------------------------------------------------------------------------------------------------------------------
  print("Step E / 1_contraintes : SUCCESS")
  if (save_intermed_file) {save(menage_contraintes, file = MatisseFiles$menage_contraintes_E1_rd)}
  return(menage_contraintes)

}





