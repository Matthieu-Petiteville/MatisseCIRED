

# format_menage_data --------------------------------------------------------------------------------------------------------------------------------------
#' @title format_menage_data
#' @description This is the routing for step A of Matisse run.
#'
#'
#' @return A menage_forme dataframe
#' @export
#'
#' @examples
#' format_menage_data()
#'
format_menage_data <- function(){

  #Data
  menage_init <- suppressWarnings(suppressMessages(
    as.data.frame(read_excel(MatisseFiles$menage_bdf_xl) , stringsAsFactors = F)
    ))

  #Functions
  menage_forme <- MatisseCIRED:::format_BDF(menage_init, save_intermed_file = MatisseParams$save_intermed_file)
  menage_forme <- MatisseCIRED:::format_energie(menage_forme, save_intermed_file = MatisseParams$save_intermed_file)
  menage_forme <- MatisseCIRED:::estimate_DPE(menage_forme, save_intermed_file = MatisseParams$save_intermed_file)
  menage_forme <- MatisseCIRED:::traitement_GT(menage_forme, save_intermed_file = MatisseParams$save_intermed_file)
  MatisseCIRED:::format_data_3ME()

  #Sauve
  save(menage_forme, file = MatisseFiles$menage_forme_A_rd)
  return(menage_forme)

}


