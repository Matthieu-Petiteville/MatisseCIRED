
#' @title mise_a_l_echelle
#' @description Step 1 of Matisse
#'
#' @param menage A menage_forme dataframe
#'
#' @return A menage_echelle dataframe
#' @export
#'
#' @examples
#' mise_a_l_echelle(menage_forme)
#'
mise_a_l_echelle <- function(menage){

  menage_echelle <- mise_echelle_revenus(menage, MatisseParams$save_intermed_file)
  menage_echelle <- retrocession_taxe_carbone(menage_echelle, MatisseParams$save_intermed_file)

  #Sauve
  save(menage_echelle, file = MatisseFiles$menage_echelle_B_rd)
  return(menage_echelle)

}
