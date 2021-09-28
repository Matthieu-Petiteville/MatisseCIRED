#' @title alignement_3ME
#' @description Step 5 of Matisse : aligns the emissions on 3ME for fossiles and electricity
#'
#' @param menage_echelle A menage dataframe
#'
#' @return A menage dataframe
#' @export
#'
#' @examples
alignement_3ME <- function(menage_echelle){

  menage_echelle <- MatisseCIRED::aligne_depenses(menage_echelle)

  #Sauve
  save(menage_echelle, file = MatisseFiles$menage_echelle_F_rd)
  return(menage_echelle)

}
