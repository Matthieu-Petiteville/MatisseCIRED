
#' @title changement_technique
#' @description Step 3 of Matisse
#'
#' @param menage A menage_echelle dataframe
#'
#' @return A menage_echelle dataframe
#' @export
#'
#' @examples
#' changement_technique(menage)
#'
changement_technique <- function(menage){

  #Changements domicile
  menage_echelle <- MatisseCIRED:::achat_neuf_horizon(menage, MatisseParams$save_intermed_file)
  menage_echelle <- MatisseCIRED:::achat_neuf_inter(menage_echelle, MatisseParams$save_intermed_file)
  menage_echelle <- MatisseCIRED:::rehabilitation_inter(menage_echelle, MatisseParams$save_intermed_file)
  menage_echelle <- MatisseCIRED:::bascule_domicile(menage_echelle, MatisseParams$save_intermed_file)

  #Changements véhicule
  menage_echelle <- MatisseCIRED:::bascule_efficacite_vehicule(menage_echelle, MatisseParams$save_intermed_file)


  #Sauve
  save(menage_echelle, file = MatisseFiles$menage_echelle_D_rd)
  return(menage_echelle)

}
