
# format_BDF ----------------------------------------------------------------------------------------------------------------------------------------------
#' @title format_BDF
#' @description Formats the initial BDF menage file to get 'standardized' variables on all columns. Not exported by default.
#'
#' @param menage A menage data.frame
#' @param save_intermed_file Boolean indicating whether the output should be saved under the MatisseFiles$menage_forme_2_rd file
#'
#' @return A menage data.frame
#'
#'
#' @examples
#'format_BDF(menage, F)
#'
format_BDF <- function(menage, save_intermed_file = FALSE){

# Data --------------------------------------------------------------------
  #Donnees Brutes BDF 2010
  individu <- suppressWarnings(suppressMessages(read_excel(MatisseFiles$indiv_bdf_xl)))
  c05 <- suppressMessages(read_csv2(MatisseFiles$c05_bdf_csv))
  depmen <- suppressWarnings(suppressMessages(read_csv2(MatisseFiles$depmen_bdf_csv)))

  # Typologie de vulnérabilité
  typo_vuln <- suppressMessages(read_excel(MatisseFiles$typovuln_xl , sheet="identmen"))

  #BDFE S. De Lauretis
  load(MatisseFiles$appmen_depact_2010_rd)
  appmen_depensesactiv_2010 <- appmen_depensesactiv

# Selection ménages -------------------------------------------------------
  # Exclusion DOM
  menage <-
    menage %>%
    filter(zeat>0)
  menage$ident_men <- as.numeric(menage$ident_men)

  #Selection des ménages présents dans la base appmen de Simona pour bénéficier des données énergies EDF
  appmen_depensesactiv_2010 <- appmen_depensesactiv_2010[which(appmen_depensesactiv_2010$ident_men %in% menage$ident_men), ]

  # Selection des ménages dans base c05
  c05$ident_men <-  as.numeric(c05$ident_men)
  c05 <- c05[which(c05$ident_men %in% menage$ident_men), ]

  # Selection des ménages dans base individus
  individu$ident_men <- as.numeric(individu$ident_men)
  individu <- individu[which(individu$ident_men %in% menage$ident_men), ]

# Recoder variables ---------------------------------------------------------------
  # creation variables type menage corrige, quintile UC et maison individuelle pour recreer classes

  menage <- within(menage, {
    typmen_corr <- ifelse(typmen5 == 1 & agepr <= 65, 1,
                          # celib <= 65
                          ifelse(typmen5 == 1 & agepr > 65, 2,
                                 # celib > 65
                                 ifelse(typmen5 == 2, 5,
                                        # famille monoparentale
                                        ifelse(typmen5 == 3 & agepr <= 65, 3,
                                               # couple sans enfants, <=65
                                               ifelse(typmen5 == 3 & agepr > 65, 4,
                                                      # couple sans enfants, > 65
                                                      ifelse(typmen5 == 4, 6,
                                                             # couple avec enfants
                                                             ifelse(typmen5 == 5 & nenfants > 0, 6,
                                                                    # ménages complexes : 6,3 ou 4
                                                                    ifelse(typmen5 == 5 & nenfants == 0 & agepr <= 65, 3,
                                                                           ifelse(typmen5 == 5 & nenfants == 0 & agepr > 65, 4, NA
                                                                           )))))))))})
  menage$quintileuc <- car::recode(menage$decuc2," 1:2 = 1 ; 3:4 = 2 ; 5:6 = 3 ; 7:8 = 4 ; 9:10 = 5 ")
  menage$MI_corr <- car::recode(menage$typlog, "1:2 = 1 ; 3:6 = 0")

# Agreger variables individus ---------------------------------------------
  #A partir de la table individus, calcul du nombre de retraités, de chômeurs =>
  #l'objectif est de corriger les donnes de la table menage dont les variables nactifs
  #et nactoccup ne sont pas fiables (nombre negatif de chomeurs en les sommant)
  nbactoccup <-
    individu %>%
    filter(situa<=2) %>%
    group_by(ident_men) %>%
    dplyr::summarise("nbactoccup"=n())
  nbchomeurs <-
    individu %>%
    filter(situa==4) %>%
    group_by(ident_men) %>%
    dplyr::summarise("nbchomeurs"=n())
  nbretraites <-
    individu %>%
    filter(situa==5) %>%
    group_by(ident_men) %>%
    dplyr::summarise("nbretraites"=n())

# Creation menage_forme, table des ménages enrichie de données ---------------------------------------------------
  # menage_forme regroupe l'essentiel des variables utilisées dans la suite du code
  # On utilise decuc2 car défini sur la france métropolitaine.
  # DECUC2 :Décile de revenu par unité de consommation - Calculé sur la France métropolitaine
  # d’une part et sur l’ensemble des DOM d’autre part
  menage_forme <-
    menage %>%
    select(ident_men, pondmen, quintileuc, typmen_corr, typmen5, MI_corr, npers, coeffuc,
           chomage, retraites, decuc2, tuu, agepr, codcspr, zeat, salaires, revindep, rev_etranger, typlog) %>%
            left_join(nbchomeurs, by="ident_men") %>%
            left_join(nbactoccup, by="ident_men") %>%
            left_join(nbretraites, by="ident_men") %>%
            left_join(depmen %>% select(ident_men, surfhab_d, stalog, propri, vag), by="ident_men")

  # Correction nb chomeurs
  # il existe 2605 ménages dont nbchomeurs est NA mais où il existe des revenus du chômage.
  menage_forme <-
    menage_forme %>%
    #npers_identifiés assure qu'on ne compte pas plus de personnes dans le ménage qu'il n'en contient
    mutate(npers_identifiees = ifelse(is.na(nbchomeurs), 0, nbchomeurs) +
             ifelse(is.na(nbactoccup), 0, nbactoccup) +
             ifelse(is.na(nbretraites), 0, nbretraites)) %>%
    # #est considéré comme contenant un chomeur, un ménage qui tire des revenus d'une allocation chomage
    # NB : avec le système de pré-retraites ne fonctionne pas => surestime le nombre de chomeurs
    # mutate(nbchomeurs=ifelse(is.na(nbchomeurs) & chomage==0,0,ifelse(is.na(nbchomeurs) & !chomage==0 & (npers-npers_identifiees)>0,1,nbchomeurs)))%>%
    #actifs occupés
    mutate(nbactoccup = ifelse(is.na(nbactoccup) & (salaires + revindep + rev_etranger) == 0, 0,
                               ifelse(is.na(nbactoccup) & !((salaires + revindep + rev_etranger) == 0) & (npers - npers_identifiees) > 0, 1, nbactoccup))) %>%
    #actifs occupés
    mutate(nbretraites = ifelse(is.na(nbretraites) & retraites == 0, 0,
                                ifelse(is.na(nbretraites) & !retraites == 0 & (npers-npers_identifiees) > 0, 1, nbretraites))) %>%
    # le reste des NA est mis à 0 (signifie que le foyer est "plein", la somme des catégories renseignées suffit à égal npers)
    mutate(nbchomeurs = ifelse(is.na(nbchomeurs), 0, nbchomeurs),
           nbactoccup = ifelse(is.na(nbactoccup), 0, nbactoccup),
           nbretraites = ifelse(is.na(nbretraites), 0, nbretraites)) %>%
    select(-npers_identifiees, -revindep, -salaires, -rev_etranger)

  # Actifs
  menage_forme <-
    menage_forme %>%
    mutate(nbactifs = nbchomeurs + nbactoccup)

  #Correction des surfaces entre menage_forme et depmen
  l_idx <- which(is.na(menage_forme$surfhab_d))
  if(length(l_idx)>0){
    menage_forme[l_idx,"surfhab_d"] <- depmen$surfhab[match(menage_forme$ident_men[l_idx],depmen$ident_men)]
  }

  # Calcul nombre inactifs
  menage_forme$nbinact <- menage_forme$npers - menage_forme$nbretraites - menage_forme$nbactifs

  # Rajout nombre de véhicules
  auto <- suppressWarnings(read_excel(MatisseFiles$auto_bdf_xl,sheet="AUTO_METROPOLE")) #uniquement AUTOMOBILE
  menage_forme <-
    menage_forme %>%
    left_join(auto %>% select(ident_men, nbvehic) %>% distinct(),by = "ident_men")



# Formattage des revenus  -----------------------------------------------------------------
  #Categories de revenus definies dans excel
  def_rev <- suppressWarnings(read_excel(MatisseFiles$def_rev_xl))

  # Revenus de l'activité salariale et/ou independante + revenus de l'étranger
  menage_forme$rev_activites <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVACT"), ]$rev)])

  # Revenus de l'activité salariale et/ou independante SANS revenus de l'étranger
  menage_forme$rev_activites_sans_etranger <- rowSums(menage[c("salaires", "revindep")])

  # Revenus de l'étranger (inclus dans revact)
  menage_forme$rev_etranger <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVETRANGER"), ]$rev)])

  # Revenus exceptionnels
  menage_forme$rev_exceptionnel <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVEXC"), ]$rev)])

  # Revenus du patrimoine
  menage_forme$rev_patrimoine <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVPAT"), ]$rev)])

  # Revenus sociaux (tous)
  menage_forme$rev_sociaux <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVSOC"), ]$rev)])

  # Revenus sociaux (excl chômage pour calage)
  menage_forme$rev_sociaux_autres <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVSOC_AUTRES"), ]$rev)])

  # Revenus pour le RDB, revenus réguliers, rentrant dans le revenu disponible brut. rev700
  menage_forme$rev700 <- menage$rev700 #Sommes reçues régulièrement d'un autre ménage (qui doit les verser obligatoirement)
  menage_forme$rev701 <- menage$rev701 #Sommes reçues régulièrement d'un autre ménage (qui les verse librement)
  menage_forme$rev999 <- menage$rev999 #Autres ressources
  # Revenus pour le RDB_macro (RDB_new)
  menage_forme$rev801 <- menage$rev801 #Loyers imputés (pour les propriétaires et logés gratuitement)
  menage_forme$rev800 <- menage$rev800 #Vente de logements, terrains, garages => sert dans calcul épargne 2010
  menage_forme$rev850 <- menage$rev850 #Vente de véhicule
  menage_forme$rev60x <-
    menage$rev601 + #Gains aux jeux de hasard
    menage$rev602 + #Sommes versées par une compagnie d’assurance
    menage$rev603 + #Dommages et intérêts
    menage$rev604 + #Indemnités de licenciement, primes de départ
    menage$rev605 + #Prime à l’amélioration de l’habitat
    menage$rev606 + #Déblocage de participation, vente d’actions,  d’obligations
    menage$rev699 #Autres ressources exceptionnelles
  #NB : on ne compte pas le rev600, héritages et donc qui sont des transferts entre ménages.
  # Revenus locatif pour TC_DPE 3.3
  menage_forme$rev504 <- menage$rev504 #Vente de véhicule
  #on rajoute la colonne correspondant au recyclage d'une partie des revenus de la taxe carbone dans les scénarios.
  menage_forme$rev_TCO <-0

  # #Revenus totaux
  # # (revact + revsoc + revpat + rev700 +rev 701 + rev 999)
  menage_forme$RDBAI <- rowSums(menage_forme[c("rev_activites", "rev_patrimoine",
                                               "rev_sociaux", "rev700", "rev701", "rev999","rev_TCO")])



# Formattage des dépenses énergétiques --------------------------------------------------------
  #les depenses energetiques ne proviennent pas de BDF mais de l'ENL 2013.
  # On les importe donc de appmen_depensesactiv_2010 : base appairée BDFE (de Lauretis, 2017)
  # On sélectionne les variables suivantes pour "Elec" :
  # [1] "Elec_sommeil"         "Elec_physio"          "Elec_repas_dom"
  # [4] "Elec_repas_hors"      "Elec_trav_etu"        "Elec_achats"
  # [7] "Elec_travdom_alim"    "Elec_travdom_vetem"   "Elec_travdom_maison"
  # [10] "Elec_assistance"      "Elec_loisirs_lecture" "Elec_loisirs_tele"
  # [13] "Elec_pleinair_sport"  "Elec_trajets_domtrav" "Elec_trajets_autres"
  menage_forme <- menage_forme[which(menage_forme$ident_men %in% appmen_depensesactiv_2010$ident_men),]

  #A02
  menage_forme$dep_Elec <- rowSums(appmen_depensesactiv_2010[grep("Elec_", names(appmen_depensesactiv_2010))])
  #A03
  menage_forme$dep_Gaz <- rowSums(appmen_depensesactiv_2010[grep("Gaz_", names(appmen_depensesactiv_2010))])
  #A04
  menage_forme$dep_GPL <- rowSums(appmen_depensesactiv_2010[grep("GPL_", names(appmen_depensesactiv_2010))])
  menage_forme$dep_Fuel <- rowSums(appmen_depensesactiv_2010[grep("Fuel_", names(appmen_depensesactiv_2010))])
  menage_forme$dep_Urbain <- rowSums(appmen_depensesactiv_2010[grep("Urbain_", names(appmen_depensesactiv_2010))])
  menage_forme$dep_Solides <- rowSums(appmen_depensesactiv_2010[grep("Solides_", names(appmen_depensesactiv_2010))])



# Mise en forme des dépenses - Nomenclature ADEME ---------------------------------------------------------
  #On utilise la nomenclature ADEME pour attribuer les dépenses par secteur
  Nomenclature_ADEME_COICOP <-  suppressWarnings(read_excel(MatisseFiles$nom_coicop_3me_xl))
  c05 <- c05[which(c05$ident_men %in% menage_forme$ident_men),]
  #A01
  menage_forme$agriculture <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A01"),]$COICOP_2011)])

  # A02 électricité, A03 Gaz, A04 autres énergies
  # Pas d'intérêt à sommer les dépenses BDF d'énergie, on utilise les données ENL

  #A05
  menage_forme$BTP <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A05"),]$COICOP_2011)])
  #A06
  menage_forme$prod_veh <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A06"),]$COICOP_2011)])
  #A07
  menage_forme$carb_lubr <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A07"),]$COICOP_2011)])
  #A08
  menage_forme$transp_rail_air <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A08"),]$COICOP_2011)])
  #A09
  menage_forme$transp_routes_eau <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A09"),]$COICOP_2011)])
  #A10
  menage_forme$loisirs_com <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A10"),]$COICOP_2011)])
  #A11
  menage_forme$autres_services <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A11"),]$COICOP_2011)])
  #A12
  menage_forme$autres <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A12"),]$COICOP_2011)])
  #A13  # sans loyers imputés
  menage_forme$loyers <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A13"),]$COICOP_2011)])
  #A14
  menage_forme$veh_occasion <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A14"),]$COICOP_2011)])
  #Hors postes dépenses
  menage_forme$Hors_budget <-
    rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="Hors_budget"),]$COICOP_2011)])

  # Rajouter c13711: Achats de logements, garages, parkings, box et terrains,
  # compris dans le Hors Budget
  # 13211 Remboursements de prêts pour la résidence principale (yc garage et dépendance)
  # 13221 Remboursements des autres prêts immobiliers (résidence secondaire et autre logement yc dépendance)
  # 13511 Remboursements de crédits à la consommation (voiture, gros travaux, biens durables)
  menage_forme <-
    menage_forme %>%
    left_join(c05 %>% select(c13711, c13211, c13221, c13511, ident_men), by = "ident_men")


  # Dans le Hors Budget on trouve
  # *Stupéfiants c02311 => 0 pour tous les ménages
  # *Autres dépenses occasionnées par une cérémonie c12811
  # *Dépenses SAI des personnes vivant hors du domicile au moins un jour par semaine c12911 => ?
  # *Remboursements de prêts pour la résidence principale (yc garage et dépendance) c13211 => utilisation de l'épargne
  # *Remboursements des autres prêts immobiliers (résidence secondaire et autre logement yc dépendance) c13221 => utilisation de l'épargne
  # *Aides et dons (occasionnels ou réguliers) en argent offerts par le ménage et pensions alimentaires c13311 => transferts entre ménages
  # *Remboursements de crédits à la consommation (voiture, gros travaux, biens durables) c13511 => utilisation de l'épargne
  # *Prélèvements de l'employeur c13611 => 0 pour tous les ménages
  # *Achats de logements, garages, parkings, box et terrains c13711
  # *Epargne salariale : achat d’actions de l’entreprise c13722 => utilisation de l'épargne

# Variables macro ménages ---------------------------------------------------------------
  #IR
  menage_forme$impot_revenu <-
    c05$c13141
  # Autres impôts directs (AID)
  menage_forme$AID <- rowSums(c05[c("c13111","c13121","c13151","c13161")])
  # Revenu Brut Disponible (RDB)
  menage_forme$RDB <- menage_forme$RDBAI - rowSums(menage_forme[c("impot_revenu", "AID")])
  # Taux d'imposition
  menage_forme$taux_IR <- ifelse(menage_forme$RDB == 0, 0, menage_forme$impot_revenu/menage_forme$RDBAI)
  menage_forme$taux_AID <- ifelse(menage_forme$RDB == 0, 0, menage_forme$AID/menage_forme$RDBAI)

# Typologie vulnérabilités -------------------------------------------------
  # Rajout des typologies de vulnérabilité dans la base menage_forme
  typo_vuln <-
    typo_vuln %>%
    separate(col="IDENTMEN", into=c("year","ident_men"),sep="2011")
  typo_vuln$ident_men <- as.numeric(typo_vuln$ident_men)

  # Selection des ménages
  typo_vuln_bis <-
    typo_vuln %>%
    filter(ident_men %in% menage_forme$ident_men)
  # rajout typo
  menage_forme <-
    menage_forme %>%
    left_join(., typo_vuln_bis[c("ident_men", "typo2010f")], by = "ident_men")


# Success ------------------------------------------------------------------
  print("Step A / 1_format_BDF : SUCCESS")
  if(save_intermed_file){save(menage_forme, file = MatisseFiles$menage_forme_A1_rd)}
  return(menage_forme)

}
