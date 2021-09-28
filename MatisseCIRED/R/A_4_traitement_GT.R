# OBJECTIF : Désagreger les dépenses de gros_travaux de 2010 entre les travaux de rénovation "classique" et de "réhabiliation énergétique".

# Justification : Nous retraitons la base micro afin d’effacer les rénovations énergétiques qu’elle décrit en 2010, ceci afin de ne pas mélanger la prise d’hypothèse
# sur l’investissement des ménages rénovant leur logement et la prise en compte des rénovations effectivement inscrites et mises à l’échelle dans les données de la base.
# En effet nous ne pouvons pas lier la décision de rénovation en 2010 à des caractéristiques socio-économiques particulières. Nous faisons le choix d’utiliser différents
# jeux d’hypothèses réunis en scénarios afin de polariser les résultats de la modélisation en fonction du mode de sélection des ménages investisseurs retenu. Le montant des
# rénovations énergétiques de 2010 retranché au budget des ménages augmente ainsi mécaniquement le flux d’épargne nette des ménages


# PRINCIPE :  On trace les seuils correspondant à la rénovation de la classe X à A, de X à B, X à C, etc.
# On a de ThreeME on a la somme en € de transition de X à A, de X à B, de X à C, etc. On remplit cette somme d'investissement ThreeME en partant des seuils et en montant
# dans les dépenses GT par m2, juste en dessous de ce seuil si ça ne marche pas. On part des seuils pour rester au plus des montants théoriques. En effet plus les GT sont
# important plus il y a de chance qu'il s'agisse d'une unique salve de travaux pour les ménages (peu de proba de faire deux fois des GT dans l'année)
# Par définition, n a enlevé des ménages les 7 milliards de rénovation énergétique sans changer leur classe de DPE => on a fait aucune rehab en 2010 et toutes les rehab 2025
# vont être exogènes, les dep gros travaux qui restent sont hors rehab énergétiques.
#
# Cette désagrégation en 2010 va nous donner Gros travaux = gros_travaux_reno + gros_travaux_rehab
#



# traitement_GT -------------------------------------------------------------------------------------------------------------------------------------------
#' @title traitement_GT
#' @description This function handles the 'gros travaux' (large housework) calculation to differentiate between regular houshold
#' work and the one that are considered 'renovation'. Not exported by default.
#'
#' @param menage The standard menage dataframe
#' @param save_intermed_file Boolean indicating whether the output dataframe should be saved under MatisseFiles$menage_forme_A4_rd
#'
#' @return
#'
#' @examples
#' traitement_GT(menage, FALSE)
#'
traitement_GT <- function(menage, save_intermed_file = FALSE){

# Data --------------------------------------------------------------------
  suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl, sheet = "scen AMS")))
  ThreeME <-
    scen %>%
    select(-Def) %>%
    gather(key = year, value = value, -c(1)) %>%
    filter(year == 2010)
  c05 <- read.csv2(MatisseFiles$c05_bdf_csv)

# Data cost/m² 3ME ----------------------------------
  Cost_m2 <- data.frame(classe_dep = character(), classe_arr = character(), cost_m2 = double(), transition_tot_Meuros = double(), stringsAsFactors = F)
  # travaux de rénovation énergétiques en volume par saut de classe (en M2) Transition de L vers M
  for (dep in LETTERS[1:7]){
    #dep : classe DPE de départ
    for (arr in LETTERS[1:7]){
      # arr : classe DPE d'arrivée
      # On vérifie que la transition est une amélioration (arr "mieux" que dep)
      if(dep > arr){
        #extraction stock m2 passant de M à L en 2010 (ThreeME)
        stock_m2 <- as.numeric(ThreeME %>%
                               filter(Var == paste("REHAB_H01_C", dep, "_C", arr, "_2", sep="")) %>%
                               select(value))

        #extraction coût des travaux pour passer de M à L en 2010 (ThreeME) en M€
        stock_euros <- as.numeric(ThreeME %>%
                                  filter(Var == paste("PREHAB_H01_C", dep, "_C", arr, "_2*REHAB_H01_C", dep, "_C", arr, "_2", sep = "")) %>%
                                  select(value))

        # stock_euros/stock_m2 = coût de la réhabiliation par m2 (en €/m2)
        # Création matrice Cost_m2 : DPE_départ | DPE_arrivée | coût_m2 | coût_total_transition
        Cost_m2 <- rbind(Cost_m2, data.frame(classe_dep = dep, classe_arr = arr, cost_m2 = stock_euros / stock_m2 * (10^6), transition_tot_Meuros = stock_euros))
      }
    }
  }
  Cost_m2$dep_arr <- paste(Cost_m2$classe_dep, Cost_m2$classe_arr, sep = "_")

# Data gros travaux ---------------------------------------------
  menage_GT_2010 <-
    menage %>%
    select(ident_men, pondmen, surfhab_d, DPE_pred)

  # Sélection des ménages ayant réalisé des gros travaux en 2010 (résidence principale ou secondaire)
  # c13411 : Gros travaux pour la résidence principale yc matériaux de construction de gros oeuvre et de gros équipementsencastrés dans le bâti
  # c13421 : Gros travaux pour une résidence secondaire ou un autre logement yc matériaux de construction de gros oeuvre et de gros équipements encastrés dans le bâti
  c13_2010_GT<-
    c05 %>%
    filter(ident_men %in% menage$ident_men) %>%
    filter(c13411 + c13421 > 0) %>%
    select(c13411, c13421, ident_men) %>%
    mutate(GT = c13411 + c13421) %>%
    left_join(menage_GT_2010, by = "ident_men") %>%
    mutate(GT_m2 = GT / surfhab_d) #gros travaux par m2

# CLASSEMENT DES MENAGES PAR GT AU M2 PAR DPE -----------------------------
  c13_2010_GT<-
    c13_2010_GT %>%
    group_by(DPE_pred) %>%
    dplyr::mutate(GT_rank = row_number(-GT_m2)) %>%  # les plus gros travaux au m2 en premiers
    ungroup()

# MENAGE REHAB ENERGETIQUE 2010 -------------------------------------------
  Seuils <- data.frame(dep = character(), arr = character(), haut = double(), bas = double(), stringsAsFactors = F)
  # classe DPE de départ
  for (dep in LETTERS[2:7]){
    # restriction de la bdd des ménages à GT>0 tq leur classe DPE == dep, classement selon rang
    c13_2010_GT_classe <-
      c13_2010_GT %>%
      filter(DPE_pred == dep) %>%
      arrange(GT_rank)
    #création pour chaque DPE de départ d'une matrice donnant le rang des ménages pratiquant une REHAB
    seuil_rank=c()

    for (arr in LETTERS[1:7]){
      if(dep > arr){
        # extraction du prix au m2 de la transition en question : dep -> arr
        cost_m2_trans <- as.numeric(Cost_m2 %>%
                                      filter(classe_dep == dep) %>%
                                      filter(classe_arr == arr) %>%
                                      select(cost_m2))

        # seuil_rank indique le dernier ménage pouvant "s'offir" une telle réhabilitation,
        # dont les dépenses en GT par m2 couvrent le coût de la réhabilitation
        suppressWarnings(seuil_rank <- c(as.numeric(c13_2010_GT_classe %>%
                                                      filter(GT_m2 >= cost_m2_trans) %>%
                                                      summarise(max(GT_rank))), seuil_rank))

        # Si la première valeur de seuil_rank (dernière ajoutée, pour la dernière transition dep-> arr)
        # est infinie alors cela signifie qu'aucun ménage n'a dépensé assez pour une telle transition
        # le premier ménage éligible est donc le premier (décalage ensuite si réalise une transition plus chère)
        if(is.infinite(seuil_rank[1])){seuil_rank[1] <- 1}

        # GT_rehab : coût de la réhabiliation acquitée par le ménage et le total représenté par le ménage (pondmen)
        c13_2010_GT_classe <-
          c13_2010_GT_classe %>%
          mutate(GT_rehab = cost_m2_trans * surfhab_d) %>%
          mutate(GT_pond_rehab = cost_m2_trans * surfhab_d * pondmen) %>%
          mutate(GT_pond = GT * pondmen)

        # extraction du total macro de la transition dep->arr
        cost_tot_transition <- as.numeric(Cost_m2 %>%
                                            filter(classe_dep == dep) %>%
                                            filter(classe_arr == arr) %>%
                                            select(transition_tot_Meuros))

        #init
        sum_renov <- 0
        #i va désigner le nombre de ménage au dessus du ménage "seuil" de la transition
        # concerné par la réhabilitation dep->arr
        i <- 0
        # j désigne le nombre de ménage en dessous du ménage "seuil" de la transition
        # concerné par la réhabilitation
        # Rmq : dans le cas de la transition de G vers les classes les plus hautes, aucun ménage
        # n'a de dépenses suffisament élevées, seuil_rank est donc toujours égal à 1,
        # j est donc alors égal au j atteint par la transition précédente +1.
        # Si length(seuil_rank)=1 alors seuil_rank[2]=NA => implique que arr= DPE_A, donc j=1,
        # le premier ménage à regarder "sous le seuil" est celui qui vient directement sous le seuil
        # si on a déjà réalisé des transitions, il faut s'assurer que le seuil pour
        # la seconde transition est plus élevé que pour la première, sinon, j est
        # d'autant plus elevé que les transitions déjà effectuées pour ne pas compter deux fois le même ménage
        j <- ifelse(is.na(seuil_rank[2]), # si on veut aller vers A on n'a encore qu'un seuil celui des travaux vers A, on regarde directement sous la limite i
                    1, #donc j=1
                    ifelse(
                      seuil_rank[1] - seuil_rank[2]>0, # si ce n'est pas une transition vers A, alors seuil_rank indique non plus le seuil théorique de travaux de X vers Y, mais le dernier ménage sélectionné pour la dernière transition, si on n'est pas déjà passé sous la limite alors j=1
                      1,
                      1 + seuil_rank[2] - seuil_rank[1] # sinon on rend le premier ménage "disponible" dans la liste.
                    ))
        j_init <- j

        # Tant que le nombre de ménage réhabilitant leur logement ne somme pas au montant macro de dep -> arr
        # et qu'on n'est pas au bout de la liste
        while(sum_renov < cost_tot_transition & seuil_rank[1] + j <= max(c13_2010_GT_classe$GT_rank)){
          if(
            ((seuil_rank[1] - i > seuil_rank[2]) ||  (is.na(seuil_rank[2]) & seuil_rank[1] - i > 0)) & #cond 1
            (1 - is.na(seuil_rank[1] - i > seuil_rank[2] || (is.na(seuil_rank[2]) & seuil_rank[1] - i >0))) #cond 2
          )
            # tant de conditions compliquées :
            # dans la première on vérifie qu'on
            # n'a pas encore atteint le seuil précédent, ie qu'on ne compte pas deux fois les GT rehab d'un ménage
            # Deuxième condition : dans le cas où on transitionne vers A, donc on est au premier seuil,
            # il n'y a pas de seuil_rank[2], on vérifie qu'on peut encore monter dans le classement
            # Troisième condition : dans le cas particulier de B vers A et que la seconde
            # condition est fausse alors la première équivaut à NA or (NA OR FALSE = NA)
            # donc on rajoute une 3e condition qui est justement fausse quand les
            # deux premières somment à NA. (NA & 0 = FALSE)

          {
            # somme des montants pondérés des GT_rehab des ménages juste au dessus du seuil.
            sum_renov <- sum_renov + as.numeric(c13_2010_GT_classe %>%
                                                  filter(GT_rank==seuil_rank[1]-i) %>%
                                                  select(GT_pond_rehab)) /(10^6)
            i <- i + 1
          }else{
            # Si on ne peut plus monter au dessus du seuil, soit qu'on soit arrivé au ménage classé n°1 ou que ces ménages réalisent déjà une réhabilitation
            # on transitionne des ménages juste en dessous du seuil (qui techniquement ne peuvent pas se permettre ces travaux ...). On ne compte donc plus dans
            # le total des travaux le montant théorique en fonction de la surface, mais le montant total qu'ils peuvent débourser, soit GT
            sum_renov <- sum_renov + as.numeric(c13_2010_GT_classe %>%
                                                  filter(GT_rank == seuil_rank[1] + j) %>%
                                                  select(GT_pond)) / (10^6)
            j <- j + 1
          }
        }

        #  le ménage le mieux classé réhabilité au cours de cette boucle est au rang :
        # si j_init>1 alors c'est qu'on est déjà descendu sous le seuil au cours d'une itération précédente,
        # donc qu'a fortiori que i=0, donc que le premier ménage sélectionné est seuil_rank+j_init, si j_init=1 alors
        # on rajoute 1 pour compenser le fait qu'on ait itéré i+1 avant de sortir du while(sum_renov<cost_tot_transition)
        seuil_haut <- seuil_rank[1] - i + j_init
        #  le ménage le moins bien classé réhabilité au cours de cette boucle est au rang :
        #  si j==1 alors nous n'avons pas eu besoin d'aller chercher des ménages sous le seuil (sinon j=2)
        seuil_bas <- ifelse(j == 1, seuil_rank[1], seuil_rank[1]+j-1) # -1 pour compenser la dernière itération j+1 en sortie de boucle

        # si on a eu besoin de passer sous le seuil alors on "baisse" le seuil à la
        # valeur de seuil_bas pour indiquer à la prochaine boucle ne pas réhabiliter des ménages au dessus de ce seuil
        if(seuil_bas > seuil_rank[1]){seuil_rank[1] <- seuil_bas}

        # Dans la matrice Seuils on indique que la transition de dep à arr concerne
        # les ménages entre seuil_haut et seuil_bas (inclus)
        Seuils <- rbind(Seuils, data.frame(dep = dep, arr = arr, haut = seuil_haut, bas = seuil_bas))
      }
    }
  }

  # création variable REHAB,
  # TRUE si le ménage réalise des GT de réhabilitation énergétique, FALSE si ce sont des gros travaux de rénovation.
  # REHAB_M2 : prix des réhabilitations le cas échéant
  c13_2010_GT<-
    c13_2010_GT %>%
    mutate(REHAB = FALSE) %>%
    mutate(REHAB_m2 = 0)


  # on parcourt tous les transitions dep->arr
  for(i in 1:nrow(Seuils)){
    seuil <- Seuils[i, ]
    seuil$bas <- as.numeric(seuil$bas)
    seuil$haut <- as.numeric(seuil$haut)

    #Anciennement mutate_when, obsolete en mode function
    l_idx = intersect(intersect(which(c13_2010_GT$DPE_pred == seuil$dep),
                                which(c13_2010_GT$GT_rank <= seuil$bas)),
                                which(c13_2010_GT$GT_rank >= seuil$haut))
    c13_2010_GT$REHAB[l_idx] <- TRUE
    c13_2010_GT$REHAB_m2[l_idx] <- as.numeric(Cost_m2 %>%
                                              filter(classe_dep==seuil$dep) %>%
                                              filter(classe_arr==seuil$arr) %>%
                                              select(cost_m2))
  }

  # création variable GT_REHAB
  c13_2010_GT <-
    c13_2010_GT %>%
    mutate(GT_REHAB = REHAB_m2 * surfhab_d)
  # si GT_REHAB > GT (quand on rehabilité des ménages sous le seuil)
  # alors les travaux de réhabiliation correspondent à toute la dépense en GT
  c13_2010_GT[which(c13_2010_GT$GT_REHAB > c13_2010_GT$GT), "GT_REHAB"] <-
    c13_2010_GT[which(c13_2010_GT$GT_REHAB>c13_2010_GT$GT),"GT"]


  # Variable GT_RENO : reste des travaux non énergétiques
  c13_2010_GT <-
    c13_2010_GT %>%
    mutate(GT_RENO = GT - GT_REHAB)


  Gros_travaux_2010 <-
    c13_2010_GT %>%
    select(ident_men, pondmen, GT_REHAB, GT_RENO)


# Update Budgets ----------------------------------------------------------
  menage <-
    menage %>%
    left_join(Gros_travaux_2010 %>% select(ident_men, GT_REHAB, GT_RENO), by = "ident_men") %>%
    mutate(GT_REHAB = ifelse(is.na(GT_REHAB), 0, GT_REHAB)) %>%
    mutate(BTP = BTP - GT_REHAB)

  menage <-
    menage %>%
    select(-GT_REHAB, -GT_RENO)


# Save --------------------------------------------------------------------
  print("Step A / 4_traitement_GT : SUCCESS")
  if(save_intermed_file){save(menage, file = MatisseFiles$menage_forme_A4_rd)}
  return(menage)

}
