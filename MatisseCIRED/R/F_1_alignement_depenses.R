
#' @title aligne_depenses
#'
#' @param menage A menage_echelle dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_contraintes_rd
#'
#' @return A menage dataframe
#' @export
#'
#' @examples
aligne_depenses <- function(menage, save_intermed_file = F){

  menage_echelle <- menage
  menage_col <- colnames(menage_echelle)
  list_source_usage <- get_list_source_usage()
  load(MatisseFiles$FC_2010_horizon_rd)

  if(MatisseParams$alignement_3ME){
    Sum_horizon_Mat <- get_summary_dep_table(menage_echelle)

  # MISE A L'ECHELLE PRIX ENERGIE PAR CLASSE --------------------------------

    # Import des prix d'énergie par classe de ménage : en €/MWh
    prix_classe <- read.csv2(MatisseFiles$prix_class_csv, header = TRUE, sep = ";",dec = ".", fill = TRUE)
    # prix energie par classe de ménage : dans l'ordre : par quintile, type de ménage et type de logement (individuel ou collectif)
    prix_classe <- arrange(prix_classe,quintile,typmen_corr,MI)
    # Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025.
    prix_classe_horizon<-prix_classe
    # A02
    prix_classe_horizon$prix_elec<- prix_classe$prix_elec * FC$A02
    # A03
    prix_classe_horizon$prix_gaz<- prix_classe$prix_gaz * FC$A03
    # A04
    prix_classe_horizon[c("prix_fuel","prix_gpl")]<- prix_classe[c("prix_fuel","prix_gpl")]* as.numeric(FC$A04)
    prix_classe_horizon[c("prix_bois","prix_chaleur")]<- prix_classe[c("prix_bois","prix_chaleur")]* as.numeric(FC$A03)
    # Matrice des prix de chaque énergie pour chaque classe
    prix_classe_mat <- data.matrix(prix_classe_horizon[,c("prix_elec","prix_gaz","prix_fuel","prix_gpl","prix_bois","prix_chaleur")], rownames.force = NA)


    # Attribution d'un numéro de classe de ménage à chaque ligne de appmen (de 1 à 60)
    menage_echelle$classe_men <-
      with(
        menage_echelle,
        as.integer(interaction(MI_corr, typmen_corr, quintileuc)))

    menage_echelle$classe_men <-
      as.factor(menage_echelle$classe_men)


    # Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
    dummies_classe_men <- model.matrix(~ classe_men,
                                       data = menage_echelle,
                                       contrasts.arg = list(
                                         classe_men = contrasts(
                                           menage_echelle$classe_men,
                                           contrasts = F)))

    #Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
    dummies_classe_men <- dummies_classe_men[,-1]

    # Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
    prix_menages_horizon <- as.data.frame(dummies_classe_men %*% prix_classe_mat)
    prix_menages_horizon_bis<-as.data.frame(prix_menages_horizon)
    # Rajout colonne "ident_men" pour la fusion avec det_ener
    prix_menages_horizon<-cbind(menage_echelle$ident_men,prix_menages_horizon_bis)
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

  # Choose bascule -------------------------------------------------------------
    menage_echelle <-
      menage_echelle %>%
      mutate(bascule_Fuel = 0, bascule_Gaz = 0) %>%
      mutate(dep_e_Fuel = (dep_Fuel + dep_GPL) * pondmen /  FC$A07) %>%
      mutate(dep_e_Elec = dep_Elec * pondmen /  FC$A02) %>%
      mutate(dep_e_Gaz = (dep_Gaz + dep_Urbain) * pondmen/  FC$A03) %>%
      mutate(DPE_jump = - match(DPE_horizon,LETTERS) + match(DPE_pred,LETTERS)) %>%
      mutate(dep_surf_Fuel = (dep_Fuel + dep_GPL) /  FC$A07 / surfhab_d) %>%
      mutate(dep_surf_Gaz = (dep_Gaz + dep_Urbain) /  FC$A04 / surfhab_d)
    menage_echelle$dep_surf_Fuel[which(is.na(menage_echelle$dep_surf_Fuel))] <- 0
    menage_echelle$dep_surf_Gaz[which(is.na(menage_echelle$dep_surf_Gaz))] <- 0


    save_bascule_details <- data.frame()
    # Bascule FioulOnly first puis GazOnly
    for(ener in c("Fuel","Gaz")){
      dep_surf_ener <- paste("dep_surf_", ener, sep = "")
      bascule_ener <- paste("bascule_", ener, sep = "")
      dep_ener <- paste("dep_e_", ener, sep = "")

      #On bascule par classe DPE finale
      for(classe in MatisseParams$align_class_bascule){
        sub_men_ech <- menage_echelle %>% filter(DPE_horizon == classe)
        sub_men_ech$dep_surf_to_rank <- -sub_men_ech[,dep_surf_ener]

        select_res <- select_bascule(sub_men_ech, Sum_horizon_Mat,
                                                   MatisseParams$classement_bascule,
                                                   MatisseParams$redistribution, ener)
        Sum_horizon_Mat <- select_res$Sum_horizon_Mat
        sub_men_ech     <- select_res$sub_men_ech
        nb_basc         <- select_res$nb_basc
        left_to_basc <- Sum_horizon_Mat %>% filter(type == ener) %>% select(left_to_bascule)

        bascule_details = data.frame(ener = ener, type = "Classe", current = classe, men_basc = nb_basc,
                                     men_tot = nrow(sub_men_ech), left_to_basc = left_to_basc)

        save_bascule_details <- rbind(save_bascule_details, bascule_details)
        men_to_bascule_idx <- match(sub_men_ech$ident_men[which(sub_men_ech[,bascule_ener] == 1)], menage_echelle$ident_men)
        menage_echelle[men_to_bascule_idx,bascule_ener] <- 1
        menage_echelle[men_to_bascule_idx,dep_ener] <- 0
      }


      #On bascule par nouvelles constructions
      for(year_new in MatisseParams$horizon:MatisseParams$align_yearnew_bascule){
        sub_men_ech <- menage_echelle %>% filter(year_neuf == year_new)
        sub_men_ech$dep_surf_to_rank <- -sub_men_ech[,dep_surf_ener]

        select_res <- select_bascule(sub_men_ech, Sum_horizon_Mat,
                                     MatisseParams$classement_bascule,
                                     MatisseParams$redistribution, ener)
        Sum_horizon_Mat <- select_res$Sum_horizon_Mat
        sub_men_ech     <- select_res$sub_men_ech
        nb_basc         <- select_res$nb_basc
        left_to_basc <- Sum_horizon_Mat %>% filter(type == ener) %>% select(left_to_bascule)

        bascule_details = data.frame(ener = ener, type = "NewBuild", current = year_new, men_basc = nb_basc,
                                     men_tot = nrow(sub_men_ech), left_to_basc = left_to_basc)
        save_bascule_details <- rbind(save_bascule_details, bascule_details)
        men_to_bascule_idx <- match(sub_men_ech$ident_men[which(sub_men_ech[,bascule_ener] == 1)], menage_echelle$ident_men)
        menage_echelle[men_to_bascule_idx,bascule_ener] <- 1
        menage_echelle[men_to_bascule_idx,dep_ener] <- 0

      }

      #On bascule par saut de classes DPE décroissantes
      if(MatisseParams$align_jump_bascule < 6){
        for(jump in 6:MatisseParams$align_jump_bascule){
          sub_men_ech <- menage_echelle %>% filter(DPE_jump == jump)
          sub_men_ech$dep_surf_to_rank <- -1 * sub_men_ech[,dep_surf_ener]

          select_res <- select_bascule(sub_men_ech, Sum_horizon_Mat,
                                       MatisseParams$classement_bascule,
                                       MatisseParams$redistribution, ener)
          Sum_horizon_Mat <- select_res$Sum_horizon_Mat
          sub_men_ech     <- select_res$sub_men_ech
          nb_basc         <- select_res$nb_basc
          left_to_basc <- Sum_horizon_Mat %>% filter(type == ener) %>% select(left_to_bascule)

          bascule_details = data.frame(ener = ener, type = "DPEJump", current = jump, men_basc = nb_basc,
                                       men_tot = nrow(sub_men_ech), left_to_basc = left_to_basc)
          save_bascule_details <- rbind(save_bascule_details, bascule_details)
          men_to_bascule_idx <- match(sub_men_ech$ident_men[which(sub_men_ech[,bascule_ener] == 1)], menage_echelle$ident_men)
          menage_echelle[men_to_bascule_idx,bascule_ener] <- 1
          menage_echelle[men_to_bascule_idx,dep_ener] <- 0
        }
      }
    }

  #Check
  # sub_men_ech[1:10,c("bascule_Fuel","bascule_Gaz","dep_e_Gaz","dep_e_Gaz")]
  # nrow(menage_echelle %>% filter(DPE_jump == 0, dep_e_Gaz > 1))
  # nrow(menage_echelle %>% filter(DPE_jump == 0))
  # menage_echelle %>% filter(DPE_jump == 0, dep_e_Gaz > 1) %>% summarise(sum(pondmen))
  # menage_echelle %>% filter(DPE_jump == 0) %>% summarise(sum(pondmen))


  # Do bascule Fioul --------------------------------------------------------
    vec_fio <- c("GPL_ECS","Fuel_ECS","GPL_chauff","Fuel_chauff","GPL_Cuisson","Fuel_Cuisson")

    for (x in vec_fio){
      if (str_detect(x,"GPL")){menage_echelle[paste(x,"vol_fio",sep="_")]<-menage_echelle[x]/menage_echelle$prix_GPL}
      if (str_detect(x,"Fuel")){menage_echelle[paste(x,"vol_fio",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Fuel}
    }
    menage_echelle$vol_basc_elec_ECS_fio <- rowSums(menage_echelle %>% select(ends_with("ECS_vol_fio")))
    menage_echelle$vol_basc_elec_chauff_fio <- rowSums(menage_echelle %>% select(ends_with("chauff_vol_fio")))
    menage_echelle$vol_basc_elec_cuisson_fio <- rowSums(menage_echelle %>% select(ends_with("Cuisson_vol_fio")))

    menage_echelle <-
      menage_echelle %>%
      mutate(dep_basc_elec_ECS_fio = vol_basc_elec_ECS_fio*prix_Elec)%>%
      mutate(dep_basc_elec_chauff_fio = vol_basc_elec_chauff_fio*prix_Elec)%>%
      mutate(dep_basc_elec_cuisson_fio = vol_basc_elec_cuisson_fio*prix_Elec)

    menage_echelle$dep_fio<-rowSums(menage_echelle %>% select(all_of(vec_fio)))

    solde_fio <-
      menage_echelle %>%
      mutate(solde_fio = dep_basc_elec_ECS_fio + dep_basc_elec_chauff_fio + dep_basc_elec_cuisson_fio - dep_fio) %>% #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en Gaz,fuel, solides,etc
      mutate_when(bascule_Fuel == 0, list(solde_fio = 0))%>%
      select(ident_men, solde_fio, bascule_Fuel)



    menage_echelle <-
      menage_echelle %>%
      mutate_when(
        bascule_Fuel == 1,
        list(
          Gaz_ECS = Gaz_ECS,
          GPL_ECS = 0,
          Fuel_ECS = 0,
          Solides_ECS = Solides_ECS,
          Urbain_ECS = Urbain_ECS,
          Gaz_chauff = Gaz_chauff,
          GPL_chauff = 0,
          Fuel_chauff = 0,
          Solides_chauff = Solides_chauff,
          Urbain_chauff = Urbain_chauff,
          Gaz_Cuisson = Gaz_Cuisson,
          GPL_Cuisson = 0,
          Solides_Cuisson = Solides_Cuisson,
          Urbain_Cuisson = Urbain_Cuisson,
          Fuel_Cuisson = 0,
          Elec_chauff = Elec_chauff + dep_basc_elec_chauff_fio,
          Elec_ECS = Elec_ECS + dep_basc_elec_ECS_fio,
          Elec_Cuisson = Elec_Cuisson + dep_basc_elec_cuisson_fio,
          dep_Gaz = dep_Gaz,
          dep_GPL = dep_GPL - GPL_ECS - GPL_chauff -  GPL_Cuisson,
          dep_Fuel = dep_Fuel - Fuel_ECS - Fuel_chauff - Fuel_Cuisson,
          dep_Solides = dep_Solides,
          dep_Urbain = dep_Urbain,
          dep_Elec = dep_Elec + dep_basc_elec_chauff_fio + dep_basc_elec_ECS_fio + dep_basc_elec_cuisson_fio
        )
      )

  # Final adjust fioul --------------------------------------------------------------------------------------------------------------------------------------
    ener_horizon <- as.data.frame(Sum_horizon_Mat %>% filter(type == "Fuel"))
    adjust_factor <- -1 * ener_horizon$left_to_bascule / (ener_horizon$target_sum_Me + ener_horizon$left_to_bascule)
    solde_fio <- solde_fio %>%
      left_join(menage_echelle %>% select(ident_men, dep_GPL, dep_Fuel), by = "ident_men") %>%
      mutate(solde_adjust = (dep_GPL + dep_Fuel) * adjust_factor) %>%
      mutate(solde_fio = solde_fio + solde_adjust) %>%
      select(ident_men, solde_fio, bascule_Fuel)
    for(dep_item in vec_fio){
      menage_echelle[, dep_item] <- menage_echelle[, dep_item] * (1 + adjust_factor)
    }
    menage_echelle <- menage_echelle %>% mutate(dep_Fuel = dep_Fuel * (1 + adjust_factor))
    menage_echelle <- menage_echelle %>% mutate(dep_GPL  = dep_GPL  * (1 + adjust_factor))

    bascule_details = data.frame(ener = "Fuel", type = "FinalAdjust", current = adjust_factor, men_basc = 0,
                                 men_tot = 0, left_to_bascule = 0)
    save_bascule_details <- rbind(save_bascule_details, bascule_details)



  # Do bascule Gaz --------------------------------------------------------
    vec_gaz <- c("Gaz_ECS","Urbain_ECS","Gaz_chauff","Urbain_chauff","Gaz_Cuisson","Urbain_Cuisson")

    for (x in vec_gaz){
      if (str_detect(x,"Gaz")){menage_echelle[paste(x,"vol_gaz",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Gaz}
      if (str_detect(x,"Urbain")){menage_echelle[paste(x,"vol_gaz",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Urbain}
    }
    menage_echelle$vol_basc_elec_ECS_gaz <- rowSums(menage_echelle %>% select(ends_with("ECS_vol_gaz")))
    menage_echelle$vol_basc_elec_chauff_gaz <- rowSums(menage_echelle %>% select(ends_with("chauff_vol_gaz")))
    menage_echelle$vol_basc_elec_cuisson_gaz <- rowSums(menage_echelle %>% select(ends_with("Cuisson_vol_gaz")))

    menage_echelle <-
      menage_echelle %>%
      mutate(dep_basc_elec_ECS_gaz = vol_basc_elec_ECS_gaz*prix_Elec)%>%
      mutate(dep_basc_elec_chauff_gaz = vol_basc_elec_chauff_gaz*prix_Elec)%>%
      mutate(dep_basc_elec_cuisson_gaz = vol_basc_elec_cuisson_gaz*prix_Elec)

    menage_echelle$dep_gaz <- rowSums(menage_echelle %>% select(all_of(vec_gaz)))

    solde_gaz <-
      menage_echelle %>%
      mutate(solde_gaz = dep_basc_elec_ECS_gaz + dep_basc_elec_chauff_gaz + dep_basc_elec_cuisson_gaz - dep_gaz) %>% #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en Gaz,fuel, solides,etc
      mutate_when(bascule_Gaz == 0, list(solde_gaz = 0))%>%
      select(ident_men, solde_gaz, bascule_Gaz)


    menage_echelle <-
      menage_echelle %>%
      mutate_when(bascule_Gaz==1,list(Gaz_ECS=0,
                                      GPL_ECS=GPL_ECS,
                                      Fuel_ECS=Fuel_ECS,
                                      Solides_ECS=Solides_ECS,
                                      Urbain_ECS=0,
                                      Gaz_chauff=0,
                                      GPL_chauff=GPL_chauff,
                                      Fuel_chauff=Fuel_chauff,
                                      Solides_chauff=Solides_chauff,
                                      Urbain_chauff=0,
                                      Gaz_Cuisson=0,
                                      GPL_Cuisson=GPL_Cuisson,
                                      Solides_Cuisson=Solides_Cuisson,
                                      Urbain_Cuisson=0,
                                      Fuel_Cuisson=Fuel_Cuisson,
                                      Elec_chauff=Elec_chauff+dep_basc_elec_chauff_gaz,
                                      Elec_ECS=Elec_ECS+dep_basc_elec_ECS_gaz,
                                      Elec_Cuisson=Elec_Cuisson+dep_basc_elec_cuisson_gaz,
                                      dep_Gaz=dep_Gaz - Gaz_ECS - Gaz_chauff - Gaz_Cuisson,
                                      dep_GPL=dep_GPL,
                                      dep_Fuel=dep_Fuel,
                                      dep_Solides=dep_Solides - Solides_ECS - Solides_chauff - Solides_Cuisson,
                                      dep_Urbain=dep_Urbain - Urbain_ECS - Urbain_chauff - Urbain_Cuisson,
                                      dep_Elec=dep_Elec + dep_basc_elec_chauff_gaz + dep_basc_elec_ECS_gaz + dep_basc_elec_cuisson_gaz))


  # Final adjust fioul --------------------------------------------------------------------------------------------------------------------------------------
    ener_horizon <- as.data.frame(Sum_horizon_Mat %>% filter(type == "Gaz"))
    adjust_factor <- -1 * ener_horizon$left_to_bascule / (ener_horizon$target_sum_Me + ener_horizon$left_to_bascule)
    solde_gaz <- solde_gaz %>%
      left_join(menage_echelle %>% select(ident_men, dep_Gaz, dep_Urbain), by = "ident_men") %>%
      mutate(solde_adjust = (dep_Gaz + dep_Urbain) * adjust_factor) %>%
      mutate(solde_gaz = solde_gaz + solde_adjust) %>%
      select(ident_men, solde_gaz, bascule_Gaz)
    for(dep_item in vec_gaz){
      menage_echelle[, dep_item] <- menage_echelle[, dep_item] * (1 + adjust_factor)
    }
    menage_echelle <- menage_echelle %>% mutate(dep_Gaz = dep_Gaz * (1 + adjust_factor))
    menage_echelle <- menage_echelle %>% mutate(dep_Urbain  = dep_Urbain  * (1 + adjust_factor))

    bascule_details = data.frame(ener = "Gaz", type = "FinalAdjust", current = adjust_factor, men_basc = 0,
                                 men_tot = 0, left_to_bascule = 0)
    save_bascule_details <- rbind(save_bascule_details, bascule_details)

    solde <- solde_fio %>%
      left_join(solde_gaz, by = "ident_men") %>%
      mutate(solde = solde_fio + solde_gaz) %>%
      select(c("ident_men","solde"))


  # Final adjust Elec ---------------------------------------------------------------------------------------------------------------------------------------
    Sum_horizon_Mat <- get_summary_dep_table(menage_echelle)
    ener_horizon <- as.data.frame(Sum_horizon_Mat %>% filter(type == "Elec"))
    adjust_factor <- -1 * ener_horizon$left_to_bascule / (ener_horizon$sum_Me)

    solde_elec <-
      menage_echelle %>%
      mutate(solde_elec = 0) %>% #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en Gaz,fuel, solides,etc
      select(ident_men, solde_elec)


    #Ajustement efficacité domicile et solde
    usages <- c("ECS", "chauff", "clim", "Cuisson", "ecl", "ElecSpe")
    sources <- c("Elec")
    for(so in sources){
      for(us in usages){
        my_col <- paste(so,us,sep="_")
        if(my_col %in% names(menage_echelle)){
          menage_echelle[,my_col] <- menage_echelle[,my_col] * (1 + adjust_factor)
        }
      }
      solde_elec$solde_elec  <-  menage_echelle[paste("dep",so,sep="_")] * adjust_factor
      menage_echelle[paste("dep",so,sep="_")] <- menage_echelle[paste("dep",so,sep="_")] * (1 + adjust_factor)
    }
    bascule_details = data.frame(ener = "Elec", type = "FinalAdjust", current = adjust_factor, men_basc = 0,
                                 men_tot = 0, left_to_bascule = 0)
    save_bascule_details <- rbind(save_bascule_details, bascule_details)




  # Reventilation -----------------------------------------------------------
    menage_echelle <- menage_echelle %>% select(c(all_of(menage_col),"bascule_Fuel","bascule_Gaz"))
    menage_echelle <- ventilate_solde(menage_echelle, solde, FC, step="REHAB")
    write.csv(save_bascule_details, file = MatisseFiles$bascule_details_csv, row.names = FALSE, quote = FALSE)
  }

  if (save_intermed_file) {save(menage_echelle, file = MatisseFiles$menage_echelle_F1_rd)}
  return(menage_echelle)

}


