

#' @title launch_Matisse
#' @description The main launcher of Matisse. Must have a MatisseParams and MatisseFiles set up to be used
#'
#' @return
#' @export
#'
#' @examples
launch_Matisse <- function(){

  menage_forme <- format_menage_data()
  menage_echelle <- mise_a_l_echelle(menage_forme)
  menage_echelle <- microsimulation(menage_echelle)
  menage_echelle <- changement_technique(menage_echelle)
  menage_echelle <- reponderation(menage_echelle)
  menage_echelle <- alignement_3ME(menage_echelle)
  write.csv(menage_echelle, MatisseFiles$menage_echelle_final_csv, row.names = FALSE, quote = FALSE)

  unlist_vec <- unlist(MatisseParams)
  params_df <- data.frame(type = names(unlist_vec), value = as.vector(unlist_vec))
  write.csv(params_df, MatisseFiles$params_save_csv, row.names = FALSE, quote = FALSE)
}
