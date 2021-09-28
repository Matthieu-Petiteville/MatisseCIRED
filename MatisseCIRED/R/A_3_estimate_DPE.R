# OBJECTIF
# L'objectif de cette fonction est d'estimer une fonction de régression logistique permettant d'estimer les classes
# de DPE des ménages de BDF,le résultat se trouve dans menages_DPE, colonne DPE_pred
# Pas besoin de faire fonctionner un autre code que celui-ci, il se charger de sourcer tous les scripts R qu'il requiert.
#
#


# estimate_DPE --------------------------------------------------------------------------------------------------------------------------------------------
#' @title estimate_DPE
#' @description This function estimates the DPE class of the menage argument based on the Phebus distribution of DPE and various measures
#' for each household. Not exported by default.
#'
#' @param menage A menage data frame
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_dpe_rd
#'
#' @return A menage dataframe
#'
#'
#' @examples
#' estimate_DPE(menage, F)
#'
estimate_DPE <- function(menage, save_intermed_file = FALSE){


# Data ---------------------------------------------------------------
  menage_forme <- menage
  # Base INSEE menage
  depmen <- read.csv(MatisseFiles$depmen_bdf_csv , header=TRUE , sep=";" , dec="." , stringsAsFactors = FALSE)
  # load menage_calibr_2010 avec ménages pré_selectionnés
  load(MatisseFiles$menage_calibr_2010_rd)
  #Données Phébus
  Phebus <- read.table(MatisseFiles$phebus_csv , header=TRUE , sep=";" , dec=".")
  dpe_stock_2010 <- get_DPE_stock_year(2010)


# Calcul de la régression DPE sur données socio-économiques ----------------------------------------------------------

  # Load estm_dpe_acp,
  # list 27 paramètres
  Variables=c("BATI_PERIODE",
              "ESTOC", #a vérifier
              "Revenu_Insee_quintile",
              "EHST",
              "TYP_LOG",
              "RP_TAILLE_UU",
              "Typ_Energie_Chauff_P2",
              "TROIS_CL_DPE",
              "ident")

  #création variable is_elec, 1 si source principale énergie de chauffage, 2 si non, à partir Typ_energie_chauff_p2
  Phebus$is_elec <- ifelse(Phebus$Typ_Energie_Chauff_P2 == "Elec", 1, 2)
  Phebus$is_elec <- ifelse(is.na(Phebus$is_elec), 2, Phebus$is_elec)
  Variables <- Variables[Variables != "Typ_Energie_Chauff_P2"] #On retire la variable typ_energie_chauff_p2
  Variables <- c(Variables, "is_elec")

  #codage Quintile
  Phebus$Revenu_Insee_quintile <- as.numeric(str_sub(Phebus$Revenu_Insee_quintile, 2))

  #codage DPE
  Phebus$DPEb<- ifelse(Phebus$TROIS_CL_DPE == "A", 1,
                       ifelse(Phebus$TROIS_CL_DPE == "B", 2,
                              ifelse(Phebus$TROIS_CL_DPE == "C", 3,
                                     ifelse(Phebus$TROIS_CL_DPE == "D", 4,
                                            ifelse(Phebus$TROIS_CL_DPE == "E", 5,
                                                   ifelse(Phebus$TROIS_CL_DPE == "F", 6,
                                                          ifelse(Phebus$TROIS_CL_DPE == "G", 7, 0)))))))
  Variables <- Variables[Variables != "TROIS_CL_DPE"]
  Variables <- c(Variables,"DPEb")
  Phebus$MI <- ifelse(Phebus$TYP_LOG == "Maison", 1, ifelse(Phebus$TYP_LOG == "Appart", 0, 3))
  Variables <- Variables[Variables != "TYP_LOG"]
  Variables <- c(Variables, "MI")

  # Bati_periode
  Phebus$BATI_PERIODE <- car::recode(Phebus$BATI_PERIODE, "1:2 = 1 ; 3=2 ; 4=3 ; 5=4 ; 6=5")

  # Selection des données
  Phebus_bis <- Phebus[which(Phebus$DPEb %in% seq(1, 8, 1)), Variables]
  Phebus_bis <- Phebus_bis[which(!is.na(Phebus_bis$DPEb)), ]
  Phebus_bis <- Phebus_bis[which(Phebus_bis$MI %in% c(0,1)), ]
  Phebus_bis <- Phebus_bis[which(!is.na(Phebus_bis$Revenu_Insee_quintile)), ]
  Regresseurs <- Variables[!Variables %in% c("ident","DPEb")]

  # ACP

  ind_sup=c()
  list_exclus=c()
  #Individus originels, variables originelles = Regresseurs
  data_regresseurs <- cbind(Phebus_bis[Regresseurs], Phebus_bis['DPEb'])
  data_regresseurs2 <- data_regresseurs
  init <- 1
  C <- 1
  iter <- 0

  #Tant que l'étape d'avant enlève des individus sauf si on a déjà effectué trop d'itérations
  suppressWarnings(while(C > 0 & iter < 6){
    iter <- iter + 1
    data_regresseurs <- cbind(data_regresseurs2[Regresseurs], data_regresseurs2['DPEb'])
    list_exclus <- cbind(list_exclus[Regresseurs], list_exclus['DPEb'])
    ind_sup <- rbind(ind_sup, list_exclus)

    #pour les passages suivants le premier
    if(init != 1){
      #on agrège les individus qu'on utilise dans la regression et ceux qu'on a exclu
      data_reg_ind_sup <- rbind(data_regresseurs, ind_sup)
      borne_inf <- dim(data_regresseurs)[1] + 1
      borne_sup <- borne_inf + dim(ind_sup)[1] - 1
      #bornes sup et inf définissent les indices des individus exclus
      dpe_pca <- FactoMineR::PCA(data_reg_ind_sup, ind.sup=borne_inf:borne_sup, scale.unit=TRUE, quanti.sup=c(8), ncp=7, graph=T)
    }else{
      #Premier passage
      dpe_pca <- FactoMineR::PCA(data_regresseurs, scale.unit=TRUE, quanti.sup=c(8),ncp=7, graph=T)
      init <- 0
    }

    dpe_pca2 <- dpe_pca
    dpe_pca2 <- as.data.frame(dpe_pca2$ind$contrib)
    colnames(dpe_pca2) <- c("Dim.1","Dim.2","Dim.3","Dim.4","Dim.5","Dim.6","Dim.7")
    data_regresseurs2 <- cbind(data_regresseurs, dpe_pca2)
    list_exclus=c()

    #on ne filtre les individus que sur trois premières dimensions
    for(dim in c("Dim.1", "Dim.2", "Dim.3")){
      m <- apply(dpe_pca2[dim], 2, mean)
      #on définit 5 fois la moyenne comme sensibilité
      m <- 4 * as.numeric(m)
      list_exclus <- rbind(list_exclus, data_regresseurs2[which(data_regresseurs2[dim] >= m), ])
      data_regresseurs2 <- data_regresseurs2[which(data_regresseurs2[dim] < m), ]
    }
    # C désigne le nombre d'exclus lors de la dernière itérations sur les deux premières dimensions
    C <- dim(list_exclus)
    if(is.null(list_exclus)){C <- 0}
  })

  #Estimation multinomiale (différentes solutions pour l'estimation : sur variables originelles, variables
  #issues de l'ACP ou mix des deux, nous retenons les variables originelles dont les performances sont
  #équivalentes et l'interprétation intuitive)
  estm_dpe_acp <- nnet::multinom(DPEb ~ BATI_PERIODE + ESTOC + Revenu_Insee_quintile + EHST +
                             RP_TAILLE_UU + is_elec + MI, data = data_regresseurs2, Hess=T)


# Créer base d'appariement --------------------------------------------------
  #appariement entre la base ménage (variables explicatives) et la nouvelle estimation de DPE
  appariement_menages_DPE <-
    menage_forme %>%
    select(ident_men, typlog, tuu, decuc2)
  pond <-
    menage_calibr_2010 %>%
    select(ident_men, pondmen)
  depmen <-
    depmen %>%
    select(ident_men, stalog, sourcp, ancons, surfhab_d)
  appariement_menages_DPE <-
    pond %>%
    left_join(., appariement_menages_DPE, by = "ident_men") %>%
    left_join(., depmen, by = "ident_men")

  #pour équivalence variables Phébus/BDF se référer au document "2018-09-26 Mapping données Phébus x BDF.odt"
  # BATI_PERIODE + ESTOC + Revenu_Insee_quintile + EHST + RP_TAILLE_UU + is_elec + MI

  # Reformattage des variables de la base d'appariement pour correspondante BDF/Phébus
  ## ANCONS
  appariement_menages_DPE$BATI_PERIODE<- car::recode(appariement_menages_DPE$ancons,
                                                     "1 = 1 ; 2:3=2 ; 4:6=3 ; 7:8=4 ; 9:10=5")
  ## STALOG
  appariement_menages_DPE$ESTOC<- car::recode(appariement_menages_DPE$stalog,
                                              "1:2 = 1 ; 3=2 ; 4:5=3 ; 6=4")
  ## DECU1
  appariement_menages_DPE <- appariement_menages_DPE%>%rename(replace=c("decuc2"="Revenu_Insee_quintile"))
  ## Surfhab_d
  appariement_menages_DPE$EHST <- appariement_menages_DPE$surfhab_d
  appariement_menages_DPE[which(appariement_menages_DPE$EHST==999) , "EHST"] <- 0
  appariement_menages_DPE[which(is.na(appariement_menages_DPE$EHST)) , "EHST"] <- 0
  ## MI_corr
  appariement_menages_DPE$MI <- car::recode(appariement_menages_DPE$typlog ,"1:2=1 ; 3:6=0")
  ## TUU (menage)
  appariement_menages_DPE$RP_TAILLE_UU <- as.numeric(appariement_menages_DPE$tuu)
  ## sourcp
  appariement_menages_DPE$is_elec <- appariement_menages_DPE$sourcp
  appariement_menages_DPE[which(appariement_menages_DPE$is_elec>1),"is_elec"] <- 0
  appariement_menages_DPE[which(is.na(appariement_menages_DPE$is_elec)),"is_elec"] <- 0

  # select col
  appariement_menages_DPE <-
    appariement_menages_DPE %>%
    select(ident_men, pondmen, BATI_PERIODE,ESTOC,Revenu_Insee_quintile,EHST,MI, RP_TAILLE_UU,is_elec)


# Estimation probabilité DPE--------------------------------------------------------------
  # Estime la probabilité d'appartenance de chaque ménage à chaque classe DPE
  pred <- predict(estm_dpe_acp, appariement_menages_DPE, type="probs", na.pass=TRUE)
  colnames(pred) <- LETTERS[1:7]
  appariement_menages_DPE[LETTERS[1:7]] <- pred
  appariement_menages_DPE$DPE_pred <- 19

# Stock m² par classe -----------------------------------------------------
 # Mise à l'échelle des stocks de m2
 stock_m2_bdf <-
    appariement_menages_DPE %>%
    summarise(sum(EHST*pondmen))
  stock_m2_threeME <-
    dpe_stock_2010 %>%
    summarise(sum(value))
  dpe_stock_2010_3ME <-
    dpe_stock_2010 %>%
    mutate(value=value*as.numeric(stock_m2_bdf/stock_m2_threeME))
  #verification, ratio=1
  stock_m2_threeME2 <-
    dpe_stock_2010_3ME %>%
    summarise(sum(value))


# Attribution DPE ---------------------------------------------------------
  # On parcourt les DPE dans l'ordre décroissant. On attribue les DPE élevées aux ménages qui ont le plus de chance de s'y situer.
  # On les x ménages les plus probablement dans la classe considérée pour atteindre le stock de m2 de cette classe dans ThreeME.
  # les ménages déjà sélectionnées se voient attribuer une probabilité de -1 d'appartenance à toutes les DPE pour les exclure du choix.
  for (dpe in LETTERS[1:6]){
    appariement_menages_DPE <- appariement_menages_DPE[order(-appariement_menages_DPE[,dpe]),]
    app_cumsum <- cumsum(appariement_menages_DPE$EHST * appariement_menages_DPE$pondmen)
    l_idx <- which(app_cumsum <= as.numeric(dpe_stock_2010_3ME %>% filter(DPE == dpe) %>% select(value)))
    appariement_menages_DPE$DPE_pred[l_idx] <- dpe
    appariement_menages_DPE[l_idx, LETTERS[1:6]] <- -1
  }
  l_idx <- which(!(appariement_menages_DPE$DPE_pred %in% LETTERS[1:6]))
  appariement_menages_DPE$DPE_pred[l_idx] <- "G"
  appariement_menages_DPE <- appariement_menages_DPE[order(appariement_menages_DPE$ident_men), ]

  menage_forme <- menage_forme[which(menage_forme$ident_men %in% appariement_menages_DPE$ident_men), ]
  menage_forme <-
    menage_forme %>%
    left_join(appariement_menages_DPE %>% select(ident_men, DPE_pred), by = "ident_men")

# Success ------------------------------------------------------------------
  print("Step A / 3_estimate_DPE : SUCCESS")
  if(save_intermed_file){save(menage_forme, file = MatisseFiles$menage_forme_A3_rd)}
  return(menage_forme)

}
