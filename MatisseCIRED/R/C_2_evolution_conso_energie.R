
# OBJECTIF ----------------------------------------------------------------

#Mise à l'échelle des dépenses d'énergie détaillées de la forme source_usage. Calcul de la consommation physique (MWh)
# Vocabulaire
# source = source d'énergie => Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain"
# usage : usages=c("ECS","chauff","clim","Cuisson","ecl","ElecSpe")
# activité : sommeil, trav_etud, trajet, etc


#' @title evolution_conso_energie
#' @description The evolution of energy consumption
#'
#' @param menage A menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_echelle_2_rd
#'
#' @return A menage dataframe
#'
#' @examples
#' evolution_conso_energie(menage, FALSE)
#'
evolution_conso_energie <- function(menage, save_intermed_file = F){

# Data -----------------------------------------------------------------
  menage_echelle <- menage
  load(MatisseFiles$FC_2010_horizon_rd)
  list_source_usage <- get_list_source_usage()
  sources <- get_sources()


# Mise à l'échelle des conso d'énergie --------------------------------------------------------
  # A02 : Electricité
  menage_echelle[c("Elec_ElecSpe","Elec_clim","Elec_ecl","Elec_ECS","Elec_chauff", "Elec_Cuisson","dep_Elec_verif" )]<-
    menage_echelle[c("Elec_ElecSpe","Elec_clim","Elec_ecl","Elec_ECS","Elec_chauff", "Elec_Cuisson","dep_Elec_verif")]*
    (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) * (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*FC$A02

  # A03 : Gaz
  menage_echelle[c("Gaz_ECS","Gaz_chauff","Gaz_Cuisson","dep_Gaz_verif")]<-
    menage_echelle[c("Gaz_ECS","Gaz_chauff","Gaz_Cuisson","dep_Gaz_verif")]*(1+menage_echelle$elast_prix_A03*(-1+FC$A03/menage_echelle$IP_stone)) *
    (1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*FC$A03

  # A04 : Autres energies domestiques
  menage_echelle[c("dep_GPL_verif","GPL_Cuisson","GPL_chauff", "GPL_ECS")]<-
    menage_echelle[c("dep_GPL_verif","GPL_Cuisson","GPL_chauff", "GPL_ECS")]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04
  menage_echelle[c("dep_Fuel_verif","Fuel_Cuisson","Fuel_chauff","Fuel_ECS")]<-
    menage_echelle[c("dep_Fuel_verif","Fuel_Cuisson","Fuel_chauff","Fuel_ECS")]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04
  menage_echelle[c("dep_Solides_verif","Solides_Cuisson", "Solides_chauff","Solides_ECS")]<-
    menage_echelle[c("dep_Solides_verif","Solides_Cuisson", "Solides_chauff","Solides_ECS")]*(1+menage_echelle$elast_prix_A04*(-1+FC$A03/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A03
  menage_echelle[c("Urbain_ECS","Urbain_chauff","Urbain_Cuisson","dep_Urbain_verif")]<-
    menage_echelle[c("Urbain_ECS","Urbain_chauff","Urbain_Cuisson","dep_Urbain_verif")]*(1+menage_echelle$elast_prix_A04*(-1+FC$A03/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A03

  # Energie surfacique ------------------------------------------------------
  menage_ener_dom <- get_energie_dom_surf(menage_echelle, FC, F)
  menage_echelle <- menage_echelle %>% left_join(menage_ener_dom, by = "ident_men")

# Succes --------------------------------------------------------------------
  print("Step C / 2_evolution_conso_energie : SUCCESS")
  if(save_intermed_file){save(menage_echelle, file = MatisseFiles$menage_echelle_C2_rd)}
  return(menage_echelle)

}



