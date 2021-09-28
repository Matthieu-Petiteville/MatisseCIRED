

# format_energie ------------------------------------------------------------------------------------------------------------------------------------------
#' @title format_energie
#' @description Adds the energy calculations to a menage data.frame. Not exported by default.
#'
#' @param menage A menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_forme_3_rd
#'
#' @return A menage dataframe
#'
#'
#' @examples
#' format_energie(menage, F)
#'
format_energie <- function(menage, save_intermed_file = FALSE){

# Data Appmen -----------------------------------------------------------------
  menage_forme <- menage
  load(MatisseFiles$appmen_intensites_2010_rd)
  # NB : dans appmen_intensites : dépenses détaillées par source_activite et par usage_activite. Variables souhaitée : source_usage.
  # Selection des ménages en commun dans appmen_intensities et menage
  appmen_intensites <- appmen_intensites[which(appmen_intensites$ident_men %in% intersect(menage_forme$ident_men, appmen_intensites$ident_men)), ]
  # Selection des colonnes "ident_men" et de la forme "usages_activite"
  dep_usage_activite <- appmen_intensites[c(1, 187:257)]
  # Selection des colonnes "ident_men" et de la forme "source_activite"
  dep_source_activite <- appmen_intensites[c(1,258:347)]


# Sources, usages, activités --------------------------------------------
  activites <- c("sommeil", "physio", "repas_dom", "repas_hors", "trav_etu", "achats",
                 "travdom_alim", "travdom_vetem", "travdom_maison", "assistance", "loisirs_lecture",
                 "loisirs_tele", "pleinair_sport", "trajets_domtrav", "trajets_autres")
  usages <- get_list_usages()
  sources <- get_sources()
  dep_sources <- paste("dep", sources, sep="_")
  reventil_sources <- paste("reventil", sources, sep="_")


# Somme dépenses par activité, source, usage --------------------------------------------
  # Pour chacun des listes source et usage, sommer toutes les activites en dépenses énergétiques.
  for (act in activites){
    dep_usage_activite[act] <-
      dep_usage_activite %>%
      select(contains(act)) %>%
      rowSums()
    dep_source_activite[act] <-
      dep_source_activite %>%
      select(contains(act)) %>%
      rowSums()
  }

  # Sommer pour chaque ménage les dépenses pour chaque source d'énergie
  for (source in sources){
    dep_source_activite[source] <- dep_source_activite %>%
      select(contains(source)) %>%
      rowSums()
  }

  # Sommer pour chaque ménage les dépenses pour chaque usage d'énergie (non exhaustif)
  for (usage in usages){
    dep_usage_activite[usage] <-
      dep_usage_activite %>%
      select(starts_with(usage)) %>%
      rowSums()
  }

  #On a vérifié que la somme sur les activités étaient les mêmes dont on excluent les act de dep_usages_activites,
  dep_usage_activite2 <-
    dep_usage_activite %>%
    select(- all_of(activites))
  dep_ener<-
    dep_source_activite %>%
    left_join(dep_usage_activite2 , by = c("ident_men"))

# Ventilation élec -------------------------------------------------------------
  # OBJECTIF : obtenir des variables sources_usages, typiquement : Fuel_Cuisson ou Elec_chauff
  # PRINCIPE : on multiplie les dépenses liées à un usage et une activité
  # (le chaffage pour le sommeil) qu'on multiplie par le pourcentage lié à une source d'énergie (gaz et sommeil) : on somme cette quantité pour toutes les activités.
  # Les seules exceptions sont ElecSpé, Clim et ecl puisque ventiler des dépenses d'ElecSpé sur de l'Urbain ou du Fuel n'a pas de sens
  # la quantité d'ElecSpé est retirée de la consommation électrique des ménages
  # on suppose que ElecSpé, Clim et ECL sont nécessairement en ELEC,
  # on va donc ventiler les autres usages sur le reste d'Elec
  dep_ener$Elec<-
    dep_source_activite$Elec -
    dep_usage_activite$ElecSpe -
    dep_usage_activite$clim -
    dep_usage_activite$ecl

  dep_ener$Elec_ElecSpe <- dep_usage_activite$ElecSpe
  dep_ener$Elec_clim <- dep_usage_activite$clim
  dep_ener$Elec_ecl <- dep_usage_activite$ecl
  usages_bis <- c("ECS", "chauff", "Cuisson") #exclusion ElecSpe, Clim, ecl
  dep_ener[is.na(dep_ener)] <- 0

  # On crée des séries artificielles à zéro pour s'assurer de la bonne exécution des boucles (tous les couples usages_activités existent)
  for (act in activites){
    for (usage in usages){
      usage_act <- paste(usage, act, sep="_")
      if (!usage_act %in% colnames(dep_ener)){
        dep_ener[usage_act]<-0
      }
    }
  }

  #fonction inverse qui renvoie 0 si le dénominateur est nul
  inverse <- function(x) {ifelse(x == 0, 0, 1/x)}
  #On inverse les totaux d'activités pour la simplicité des calculs ci-après
  for (act in activites){
    source_act_list <- c()
    for (source in sources){
      source_act <- paste(source, act, sep="_")
      source_act_list <- c(source_act_list, source_act)
    }
    #correspond aux dépenses énergétiques pour cette catégories hors ElecSpe, clim et ecl, qu'il nous reste à ventiler sur un usage
    dep_ener[act] <-
      rowSums(dep_ener[source_act_list]) -
      dep_ener[paste("ElecSpe", act, sep="_")] -
      dep_ener[paste("clim", act, sep="_")] -
      dep_ener[paste("ecl", act, sep="_")]

    dep_ener[paste(act,"bis", sep="_")] <- apply(dep_ener[act], 1, inverse) #on inverse la quantité pour le calcul du ratio source_act/act
  }


  list_source_usage <- c("Elec_ElecSpe","Elec_clim","Elec_ecl") #déjà traités
  for (usage in usages_bis){ #usages_bis = ECS, chauff, Cuisson
    for (source in sources){
      source_usage <- paste(source, usage, sep="_")
      Table <- dep_ener
      list_table <- c()
      for (act in activites){
        usage_act <- paste(usage, act, sep="_")
        source_usage_act <- paste(source, usage, act, sep="_")
        source_act <- paste(source, act, sep="_")

        # X = ratio de l'usage d'une source d'énergie pour une activité sur le total de cette activité e.g. xx% de l'énergie nécessaire au "repas_dom" provient de l'électricité.
        if(source == "Elec"){ # on traite l'Elec à part parce qu'il faut retrancher les usages nécessairement électrifiés
          X <- (Table[source_act] -
                  Table[paste("ElecSpe", act, sep="_")] -
                  Table[paste("clim", act, sep="_")] -
                  Table[paste("ecl", act, sep="_")]) * # Il reste donc Elec_ECS_act+Elec_chauff_act+Elec_cuisson_act
            Table[paste(act, "bis", sep="_")]
        }else{
          X <- Table[source_act] * Table[paste(act, "bis", sep="_")]  #e.g. Fuel_physio/physio
        }

        Table[paste(source, act, "ratio", sep="_")] <- X

        # On fait l'hypothèse que les différents usages liés à une activité utilisent le même mix énergétique que cette activité
        # Fuel_chauff_physio =chauff_physio * Fuel_physio/physio, id est : si 48% de l'énergie physio est fourni par le fuel alors 48% du chauff_physio est fournit par le fuel également
        ifelse(usage_act %in% colnames(Table),
               Table[source_usage_act] <- Table[usage_act] * Table[paste(source, act, "ratio", sep="_")], #if yes
               Table[source_usage_act] <- 0 # if no
        )
        list_table <- c(list_table, source_usage_act)
      }

      # print(source_usage)
      list_source_usage <- c(list_source_usage, source_usage)
      dep_ener[source_usage] <- rowSums(Table[list_table]) # on somme tous les source_usage_act pour chaque activité

    }
  }

  # on rajoute de nouveau ElecSpe au total de l'elec (cf ligne 170)
  dep_ener$Elec <- dep_ener$Elec + dep_ener$ElecSpe + dep_ener$clim + dep_ener$ecl
  # on rajoute de nouveau ElecSpe aux totaux d'activités # cf ligne 207
  for (act in activites){
    source_act_list <- c()
    for (source in sources){
      source_act <- paste(source, act, sep="_")
      source_act_list <- c(source_act_list, source_act)
    }
    dep_ener[act] <- rowSums(dep_ener[source_act_list])
  }
  dep_ener <- dep_ener[c("ident_men", list_source_usage, sources)]

  menage_forme<-
    menage_forme %>%
    left_join(dep_ener %>% select("ident_men", all_of(list_source_usage)), by = "ident_men")
  menage_forme$dep_Elec_verif <- dep_ener$Elec
  menage_forme$dep_Gaz_verif <- dep_ener$Gaz
  menage_forme$dep_GPL_verif <- dep_ener$GPL
  menage_forme$dep_Solides_verif <- dep_ener$Solides
  menage_forme$dep_Fuel_verif <- dep_ener$Fuel
  menage_forme$dep_Urbain_verif <- dep_ener$Urbain

# Success ------------------------------------------------------------------
  print("Step A / 2_format_energie : SUCCESS")
  if(save_intermed_file){save(menage_forme, file = MatisseFiles$menage_forme_A2_rd)}
  return(menage_forme)

}
