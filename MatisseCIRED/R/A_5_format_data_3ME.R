# Import data
# Mise en forme des hypothèses et trajectoires de ThreeME
# Lecture depuis fichier excel, converti en Rdata format long
# Lecture des données IMACLIM
# Output : ThreeME.RData



# format_data_3ME -----------------------------------------------------------------------------------------------------------------------------------------
#' @title format_data_3ME
#' @description The function standardizes the 3ME data.
#'
#' @return
#'
#' @examples
#' format_data_3ME()
#'
format_data_3ME <- function(){


# Data -----------------------------------------------------------------
  # To open Sorties_ThreeME, tab names
  SheetToRead <- switch (MatisseParams$scenario,
    "AMS" = "scen AMS",
    "AME" = "scen AME",
    "ssTCO" = "scen AMS ss TCO",
    "ssRES" = "scen AMS ss residentiel",
    "ssVE" = "scen AMS ss VE")

  #3ME by scenario
  suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl, sheet = SheetToRead)))
  #IMACLIM
  if(MatisseParams$classement == "ssrec"){
    suppressMessages(suppressWarnings(output_macro <- read_excel(path = MatisseFiles$output_macro_code_iter_0_ssrec_xl, sheet = MatisseParams$scenario, skip = 1)))
  }else{
    suppressMessages(suppressWarnings(output_macro <- read_excel(path = MatisseFiles$output_macro_code_iter_0_xl, sheet = MatisseParams$scenario, skip = 1)))
  }
  #EMS
  EMS <- read_excel(path = MatisseFiles$EMS_xl, range= paste(MatisseParams$scenario, "!B1:AF5", sep=""), col_names = T)

# Traitement data ---------------------------------------------------------
  ThreeME <-
    scen %>%
    select(-Def) %>%
    gather(key = year, value = value, -c(1))

  output_macro <-
    output_macro %>%
    gather(key = year_model, value = value, -c(1:3))

  IMACLIM<- suppressWarnings(
    output_macro  %>%
    separate(col = "year_model", into = c("year", "model"), sep = "_"))



# Facteur_croissance_2010_horizon -----------------------------------------
  var_fc_vec <- c("revact", "revpat", "revchom", "revsoc", "revetranger",
                          "rdb", "tauIR", "tauAID", "A01", "A02", "A03", "A04",
                          "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12",
                          "A13", "A14")
  FC <-
    IMACLIM %>%
    filter(year == MatisseParams$horizon) %>%
    filter(model == "IMACLIM") %>%
    filter(Variable %in% var_fc_vec) %>%
    select(Variable, value) %>%
    mutate(value = as.numeric(value)) %>%
    spread(key = Variable, value = value)


# Save --------------------------------------------------------------------
  print("Step A / 5_format_data_3ME : SUCCESS")
  save(FC, file = MatisseFiles$FC_2010_horizon_rd)
  save(EMS, file = MatisseFiles$EMS_scen_rd)
  save(ThreeME, file = MatisseFiles$Threeme_rd)
  save(IMACLIM, file = MatisseFiles$IMACLIM_rd)
  return(TRUE)

}
