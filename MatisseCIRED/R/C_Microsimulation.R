
#' @title microsimulation
#' @description Step 2 of Matisse
#'
#' @param menage A menage_echelle dataframe
#'
#' @return A menage_echelle dataframe
#' @export
#'
#' @examples
#' microsimulation(menage)
#'
microsimulation <- function(menage){

  #Simulation des dépenses
  menage_echelle <- simulation_depenses(menage, MatisseParams$save_intermed_file)
  menage_echelle <- evolution_conso_energie(menage_echelle, MatisseParams$save_intermed_file)

  #Sauve
  save(menage_echelle, file = MatisseFiles$menage_echelle_C_rd)
  return(menage_echelle)

}
