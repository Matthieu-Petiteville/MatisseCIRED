
#' @title ponderation
#'
#' @param menage_echelle A menage dataframe
#'
#' @return A menage dataframe
#' @export
#'
#' @examples
#' reponderation(menage)
#'
reponderation <- function(menage_echelle){


  menage_contraintes <- MatisseCIRED:::calcule_contraintes(menage_echelle, MatisseParams$save_intermed_file)
  menage_echelle <- MatisseCIRED:::ponderation(menage_contraintes, menage_echelle, MatisseParams$save_intermed_file)

  #Sauve
  save(menage_echelle, file = MatisseFiles$menage_echelle_E_rd)
  return(menage_echelle)

}
