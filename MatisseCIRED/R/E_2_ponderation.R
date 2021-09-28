
#' @title ponderation
#' @description Function reweighting
#'
#' @param menage_contraintes The constraints dataframe
#' @param menage_echelle The menage dataframe
#'
#' @return A menage dataframe
#'
#' @examples
#' ponderation(menage_contraintes, menage_echelle)
#'
ponderation <- function(menage_contraintes, menage_echelle, save_intermed_file = F){

  #Comment 06/05/2019 : en faisant tourner le code sans les contraintes INSEE ni le taux d'épargne, on obtient des résultats tels qu'attendus
  #(pas de ménage qui tombe à une pondération nulle), 25 minutes pour faire tourner l'algorithme.
  # a faire : vérifier que les agrégats INSEE ne sont pas trop loin des agrégats à respecter, s'ils sont proches c'est embêtant,
  #cela veut dire que les contraintes INSEE sont trop fortes pour respecter les contraintes macro.
  # Iter=1

# Data --------------------------------------------------------------------
  load(MatisseFiles$agreg_best_rd)


# Importation ---------------------------------------------------------------------------------------------------------------------------------------------
  pond_init <- as.matrix(menage_contraintes$pond_init)
  data <- menage_contraintes %>% select(-ident_men, -pond_init)
  ones <- cbind(rep(1, dim(data)[1]))
  data <- as.matrix(cbind(ones, data))
  # (S) AGREGAT INITIAUX
  Amat <- t(data)
  agreg_init <- (Amat %*% pond_init)

  # (T) AGREGAT A RESPECTER
  agreg_best <- t(agreg_best)


# Transformation ------------------------------------------------------------------------------------------------------------------------------------------
  # (Vmat) Matrice diagonale des poids à minimiser
  Vmat <- as.numeric(t(1 / ((pond_init) ^ 2)))
  Vmat <- as.matrix(diag(Vmat))
  nb_row <- nrow(Vmat)

  # (dvec) Vecteur à minimiser
  dvec <- t(vector("numeric",nb_row))

  # (Amat) Contrainte matricielle

  # (bvec) Contrainte d'égalité (valeurs de départ)
  bvec <- agreg_best - agreg_init

  # (uvec) Contrainte d'inégalité
  uvec <- -1 * pond_init

  A <- cbind(t(Amat), diag(nb_row))
  b <- rbind(bvec, uvec)

  # test (permet de comparer les tables cible et de départ avec une distance en % way to go)
  # View(cbind(rownames(agreg_init),rownames(agreg_best)))
  # View(cbind("Init"=agreg_init, "Best"=agreg_best,"Way to go"=bvec/agreg_init))


# Solveur -------------------------------------------------------------------------------------------------------------------------------------------------
  print("Repondération : Work in Progress")
  print(strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"))
  # Start the clock!
  ptm <- proc.time()
  #For memory issues
  memory.limit(size = 32000)
  sol <- solve.QP(Vmat, dvec, A, b, meq = length(agreg_best))

  # Stop the clock
  print(proc.time() - ptm)

  print(paste("Repondation : DONE (", (proc.time() - ptm)[3] / 60, " min)", sep = ""))

# Output --------------------------------------------------------------------------------------------------------------------------------------------------
  # Pondérations et agregats finales
  pond_final <- pond_init + sol$solution
  agreg_final <- (Amat %*% pond_final)

  menage_echelle <-
    menage_echelle %>%
    mutate(pondmen = pond_final)

# Succes --------------------------------------------------------------------------------------------------------------------------------------------------
  print("Step E / 2_ponderation : SUCCESS")
  write.csv2(t(agreg_final),file = MatisseFiles$agreg_final_csv)
  if (save_intermed_file) {save(menage_echelle, file = MatisseFiles$menage_echelle_E2_rd)}
  return(menage_echelle)

}


