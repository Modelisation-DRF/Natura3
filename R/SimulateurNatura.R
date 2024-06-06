################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Main function of Natura 3.0 simulator                      #
#                                                              #
#                                                              #
################################################################

#' Fonction principale pour une simulation de la croissance d'un peuplement avec le simulateur Natura 3.0
#'
#' @description \code{SimulNatura()} est la fonction principale pour exécuter une simulation de la croissance d'un peuplement avec le simulateur Natura 3.0 (version qui utilise l'origine du peuplement et le temps depuis l'origine).
#'
#' @details Le simulateur fonctionne pour des peuplements situés sur les végétations potentielles suivantes:
#' \itemize{
#'    \item ME1, MS2, MS4, MS6
#'    \item RB1, RB5,
#'    \item RP1
#'    \item RE1, RE2, RE3, RE4
#'    \item RS1, RS2, RS3, RS4, RS5, RS7
#'    }
#' Les groupes d'essences du simulateur sont composés des essences suivantes:
#' \itemize{
#'    \item bop: bouleau à papier
#'    \item peu: peuplier faux-tremble, peuplier baumier, peuplier à grandes dents
#'    \item ft: feuillus tolérants: érable rouge, bouleau jaune, érable à sucre, frêne noir
#'    \item epn: épinette noire
#'    \item epx: épinette blanche, épinette rouge
#'    \item sab: sapin baumier
#'    \item ri: résineux intolérants: pin gris, mélèze laricin, pin rouge
#'    \item rt: résineux tolérants: thuya, pin blanc, pruche
#'    }
#'
#' @param file_arbre Nom du fichier contenant les informations sur les arbres et les placettes (table, xlsx ou csv). Le fichier doit contenir une ligne par arbre ou par classe de dhp/essence.
#' Si ce paramètre n'est pas utilisé, \code{file_compile} doit l'être. Le nom des colonnes doit être:
#'  \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item origine: code d'origine de la placette (ES: épidémie sévère, BR: brulis, CT: coupe totale)
#'    \item temps: temps depuis l'origine (ans)
#'    \item type_eco: code du type écologique (ex: MS22)
#'    \item essence: code de l'essences de l'arbre (ex: SAB, EPN, BOP)
#'    \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (seuls les arbres de plus de 9 cm sont retenus)
#'    \item tige_ha: nombre d'arbres de l'essence et de la classe de dhp, à l'ha
#'    \item etat: code d'état de l'arbre, seuls les vivants sont retenus, codes 10, 12, 30, 32, 40, 42, 50, 52
#'    \item sdom_bio: code du sous-domaine bioclimatique, en majuscule (ex: 2E, 4O).
#'    \item altitude: altitude (m). Optionel: nécessaire seulement si la hauteur des arbres est à estimer
#'    \item p_tot: précipitations totales annuelles, moyenne sur la période 1980-2010 (mm). Optionel: nécessaire seulement si la hauteur des arbres est à estimer
#'    \item t_ma: température moyenne annuelle sur la période 1980-2010 (Celcius). Optionel: nécessaire seulement si la hauteur des arbres est à estimer
#'    \item haut_pred: hauteur de l'arbre (m). Optionnel: peut être estimer avec le package TarifQC en utilisant \code{ht=T}
#'    \item vol_dm3: volume de l'arbre ou volume d'un arbre de la classe de dhp  (dm3). Optionnel: peut être estimer avec le package TarifQC en utilisant \code{vol=T}
#'    \item latitude, longitude: coordonnées des placettes, en degré décimal. Optionel: nécessaires si \code{iqs=T} ou \code{sol=T} ou \code{climat=T}
#'    \item an_mes: année de mesure de la placette. Optionnel, nécessaire si \code{climat=T}
#'    \item propriétés de sol SIIGSOL dans la profondeurt 0-5 cm: ph, clay (proportion d'argile (pct)), sand (pct) proportion de sable (pct)), cec (capacité d'échange cationique (méq/100g)), oc (proportion de matière organique (pct)). Optionnel: peuvent être récupérés avec le package ExtractMap en utilisant \code{sol=T}
#'    \item IQS potentiels (m, hauteur dominante à 50 ans): iqs_pot_sab, iqs_pot_epn, iqs_pot_epb, iqs_pot_pig, iqs_pot_tho, iqs_pot_pib, iqs_pot_bop, iqs_pot_pex (peupliers). Optionel, peuvent être récupérés avec le package ExtractMap en utilisant \code{iqs=T}
#'    \item variable climatiques: prec_gs (précipitations durant la saison de croissance, moyenne sur les 10 dernières années avant le mesurage de la placette (mm), temp_gs (température moyenne durant la saison de croissance, moyenne sur les 10 dernières années avant le mesurage de la placette (Celcius)). Optionnel: peuvent être récupérés avec le package ExtractMap en utilisant \code{climat=T}
#'  }
#' @param file_etude Nom du fichier contenant les arbres-études (table, xlsx ou csv). Le fichier doit contenir une ligne par arbre.
#'  Si ce paramètre n'est pas utilisé, \code{file_compile} doit l'être. Le nom des colonnes doit être:
#'  \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item etage: code d'étage de l'arbre: C, D, O, I, V, seuls les C et D sont retenus
#'    \item essence: code de l'essences de l'arbre-étude (ex: SAB, EPN, BOP)
#'    \item dhpcm: dhp de l'arbre-étude (cm)
#'    \item hauteur: hauteur totale de l'arbre-étude (m)
#'  }
#' @param file_compile Optionnel. Nom du fichier contenant les caractéristiques dendrométriques des placettes (table, xlsx ou csv). Une ligne par placette.
#'  Si ce paramètre n'est pas utilisé, \code{file_arbre} et \code{file_etude} doivent l'être. La composition de chacun des groupes d'essences est décrite dans la section Details.
#'  Le nom des colonnes doit être:
#'  \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item origine: code d'origine de la placette (ES: épidémie sévère, BR: brulis, CT: coupe totale)
#'    \item temps: temps depuis l'origine (ans)
#'    \item type_eco: code du type écologique (ex: MS22)
#'    \item sdom_bio: code du sous-domaine bioclimatique, en majuscule (ex: 2E, 4O).
#'    \item is: indice de structure diamètrale de Shannon (valeur entre 0 et 1; 0 : toutes les tiges sont dans la même classe de dhp; 1 : les tiges sont également distribuées dans les classes de dhp)
#'    \item hd: hauteur dominante, hauteur moyenne des 100 plus gros arbres à l'ha (m)
#'    \item nxxx : nombre d'arbres de plus de 9 cm du groupe d'essences xxx (/ha), mettre 0 si absent. xxx : chacun des groupes d'essences (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item stxxx: surface terrière marchande du groupe d'essences xxx (m2/ha), mettre 0 si absent. xxx : chacun des groupes d'essences (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item vxxx: volume marchand brut du groupe d'essences xxx (m3/ha), mettre 0 si absent. Si aucun volume, mettre les 8 groupes à 0 et l'évolution du volume ne sera pas effectué. xxx : chacun des groupes d'essences (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item propriétés de sol SIIGSOL dans la profondeurt 0-5 cm: ph, clay (proportion d'argile (pct)), sand (pct) proportion de sable (pct)), cec (capacité d'échange cationique (méq/100g)), oc (proportion de matière organique (pct)). Optionnel: peuvent être récupérés avec le package ExtractMap en utilisant \code{sol=T}
#'    \item IQS potentiels (m, hauteur dominante à 50 ans): iqs_pot_sab, iqs_pot_epn, iqs_pot_epb, iqs_pot_pig, iqs_pot_tho, iqs_pot_pib, iqs_pot_bop, iqs_pot_pex (peupliers). Optionel, peuvent être récupérés avec le package ExtractMap en utilisant \code{iqs=T}
#'    \item variable climatiques: prec_gs (précipitations durant la saison de croissance, moyenne sur les 10 dernières années avant le mesurage de la placette (mm), temp_gs (température moyenne durant la saison de croissance, moyenne sur les 10 dernières années avant le mesurage de la placette (Celcius)). Optionnel: peuvent être récupérés avec le package ExtractMap en utilisant \code{climat=T}
#'    \item latitude, longitude: coordonnées des placettes, en degré décimal. Optionel: nécessaires si \code{iqs=T} ou \code{sol=T} ou \code{climat=T}
#'    \item an_mes: année de mesure de la placette. Optionnel, nécessaire si \code{climat=T}
#' }
#' @param file_export Nom et emplacement du fichier à exporter, contenant les résultats de la simulation (format csv), optionnel.
#' @param horizon Nombre de décennies à simuler, un chiffre de 1 à 15.
#' @param mode_simul Mode de simulation: \code{"DET"} : déterministe ou \code{"STO"} : stochastique
#' @param nb_iter Si mode stochastique, le nombre d'itérations à effectuer (> 1), ignoré si \code{mode_simul="DET"}
#' @param iqs Booléen
#' \itemize{
#'    \item \code{TRUE} (par défaut): si les IQS potentils doivent être extraits des cartes. Les colonnes latitude et longitude doivent alors être fournies dans \code{file_arbre} ou dans \code{file_compile}.
#'    \item \code{FALSE}: si les IQS potentiels (m, hauteur dominante à 50 ans) sont fournis dans \code{file_arbre} ou \code{in file_compile}.
#'  }
#' @param climat Booléen
#' \itemize{
#'   \item \code{TRUE} (par défaut): si les variables climatiques doivent être extraites des cartes. Les colonnes latitude, longitude et l'année de mesure de la placette doivent alors être fournies dans \code{file_arbre} ou dans \code{file_compile}.
#'   \item \code{FALSE}: si les variables climatiques sont fournies dans \code{file_arbre} ou \code{file_compile}.
#' }
#' @param sol Booléen
#' \itemize{
#'   \item \code{TRUE} (par défaut): si les variables sur les propriétés des sols sont à extraires des cartes. Les colonnes latitude et longitude doivent alors être fournies dans \code{file_arbre} ou dans \code{file_compile}.
#'   \item \code{FALSE}: si les variables sur les propriétés des sols sont fournies dans \code{file_arbre} ou dans \code{file_compile}.
#' }
#' @param ht Booléen
#' \itemize{
#'   \item \code{TRUE} (par défaut): si la hauteur des arbres doit être estimée, seulement pour un fichier \code{file_arbre}
#'   \item \code{FALSE}: si la hauteur des arbres est fournie dans \code{file_arbre}, ignoré si \code{file_compile}. Le nom de la colonne doit être hauteur_pred (en mètres)
#'   }
#' @param vol Booléen
#' \itemize{
#'   \item \code{TRUE} (par défaut): si le volume des arbres doit être estimé, seulement pour un fichier \code{file_arbre}
#'   \item \code{FALSE}: si le volume des arbres est fourni dans \code{file_arbre}, ignoré si \code{file_compile}. Le nom de la colonne doit être vol_dm3 (en dm3)
#'   }
#' @param dec_perturb Optionnel. Le numéro de la décennie dans laquelle on simule la présence d'une perturbation partielle autre qu'une épidémie de TBE (chablis partiel, verglas pertiel, dépérissement partiel, brulis partiel).
#' @param dec_tbe1 Optionnel. Le numéro de la décennie dans laquelle on simule une défoliation due à la TBE. \code{tbe1} doit alors est spécifié.
#' @param tbe1 Optionnel. Indice de réduction de croissance due à la TBE durant la décennie \code{dec_tbe1}, un entier 1 à 5. \code{dec_tbe1} doit alors est spécifié.
#' \itemize{
#'   \item 1 = 1 année de défoliation sévères
#'   \item 2 = 4 années de défoliations sévères
#'   \item 3 = 6 années de défoliations sévères
#'   \item 4 = 8 années de défoliations sévères
#'   \item 5= 10 années de défoliations sévères.
#'   }
#' @param dec_tbe2 Optionnel. Le numéro de la 2e décennie dans laquelle on simule une défoliation due à la TBE. \code{tbe2} doit alors est spécifié.
#' @param tbe2 Optionnel. Indice de réduction de croissance due à la TBE durant la décennie \code{dec_tbe2}, un entier 1 à 5. Voir \code{tbe1} pour définition des niveaux.
#' @param seed_value Optionnel. Valeur de départ pour la génération de nombres aléatoires. Utilisé lors des tests pour la reproductibilité des résultats.
#'
#' @return Une table contenant les résultats de la simulation, une ligne par placette/décennie/itération:
#'  \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item iter: Numéro de l'itération si le mode stochastique a été utilisé, la colonne n'est pas présente si mode déterministe
#'    \item type_eco: code du type écologique
#'    \item sdom_bio: code du sous-domaine bioclimatique
#'    \item origine: code d'origine de la placette (ES: épidémie sévère, BR: brulis, CT: coupe totale)
#'    \item annee: nombre d'années depuis le début de la simulation (ans)
#'    \item temps: temps depuis l'origine (ans)
#'    \item tbe: indice de réduction de croissance due à la TBE durant la décennie
#'    \item pert: présence d'une perturbation partielle autre qu'une épidémie de TBE dans la décennie (1=présence, 0=absence)
#'    \item hd: hauteur dominante, hauteur moyenne des 100 plus gros arbres à l'ha (m)
#'    \item is: indice de structure diamètrale de Shannon
#'    \item stxxx: surface terrière marchande du groupe d'essences xxx (m2/ha) (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item sttot: surface terrière marchande de l'ensemble des essences (m2/ha)
#'    \item nxxx : nombre d'arbres de plus de 9 cm du groupe d'essences xxx (tiges/ha) (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item ntot : nombre d'arbres de plus de 9 cm de l'ensemble des essences (tiges/ha)
#'    \item vxxx: volume marchand brut du groupe d'essences xxx (m3/ha) (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item vtot: volume marchand brut du groupe de l'ensemble des essences (m3/ha)
#'    \item pct_xxx: pourcentage de la surface terrière du groupe d'essences xxx (pourcentage) (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item dqxxx: diamètre quadratique moyen du groupe d'essences xxx (cm) (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item dqtot: diamètre quadratique moyen de l'ensemble des essences (cm)
#'    \item message: si la placette n'a pas été simulée, message indiquant la raison. Colonne absente si toutes les placettes ont été simulées
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulation déterministe sur 50 ans à partir d'un fichier à l'échelle de l'arbre, hauteur et volume des arbres doit être estimés et les variables d'IQS, de climat et de sol doivent être extraites
#' data_simul <- SimulNatura(file_arbre=fichier_arbres_sanscov, file_etude=fichier_arbres_etudes, horizon=5)
#'
#' #' # Simulation déterministe sur 50 ans à partir d'un fichier à l'échelle de l'arbre, hauteur et volume des arbres doit être estimés, mais les variables d'IQS, de climat et de sol sont fournies dans le fichier d'entrée
#' data_simul <- SimulNatura(file_arbre=fichier_arbres_aveccov, file_etude=fichier_arbres_etudes, horizon=5, iqs=FALSE, sol=FALSE, climat=FALSE)
#'
#' # Simulation stochastique sur 50 ans à partir d'un fichier à l'échelle de l'arbre, hauteur et volume des arbres doit être estimés, mais les variables d'IQS, de climat et de sol sont fournies dans le fichier d'entrée
#' data_simul <- SimulNatura(file_arbre=fichier_arbres_aveccov, file_etude=fichier_arbres_etudes, horizon=5, mode_simul='STO', nb_iter=30, iqs=FALSE, sol=FALSE, climat=FALSE)
#'
#' # Simulation déterministe sur 50 ans à partir d'un fichier à l'échelle de la placette, les variables d'IQS, de climat et de sol sont fournies dans le fichier d'entrée
#' data_simul <- SimulNatura(file_compile=fichier_compile_aveccov, horizon=5, iqs=FALSE, sol=FALSE, climat=FALSE)
#' }

# file_arbre=fichier_arbres_aveccov; file_etude=fichier_arbres_etudes; horizon=5; mode_simul='DET'; nb_iter=1; iqs=FALSE; climat=FALSE; sol=FALSE; ht=TRUE; vol=TRUE; seed_value=NULL; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;
# file_arbre=fichier_arbres_sanscov; file_etude=fichier_arbres_etudes; horizon=5; mode_simul='DET'; nb_iter=1; iqs=T; climat=T; sol=T; ht=TRUE; vol=TRUE; seed_value=NULL; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;

SimulNatura <- function(file_arbre, file_etude, file_compile, file_export, horizon, mode_simul='DET', nb_iter=1, iqs=TRUE, climat=TRUE, sol=TRUE, ht=TRUE, vol=TRUE,
                        dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0, seed_value=NULL)
  {

  # pour pouvoir remettre l'option par defaut à la fin de la fonction
  #onMisuse <- getOption("doFuture.rng.onMisuse", getOption("future.rng.onMisuse"))
  #oopts <- options(future.rng.onMisuse = onMisuse)
  #on.exit(options(oopts), add = TRUE)


  # pour ne pas afficher le message de warning de %dopar% sur les random number. Utiliser dorng serait la solution, mais ne s'utilise pas en double %dorng%
  #options(doFuture.rng.onMisuse = "ignore")

  variable_climat_ <- c("p_tot", "t_ma", "prec_gs", "temp_gs")
  variable_sol_ <- c("ph", "clay", "cec", "oc", "sand" )
  variable_iqs_ <- c("iqs_pot_epn", "iqs_pot_epb", "iqs_pot_sab", "iqs_pot_pex", "iqs_pot_bop", "iqs_pot_tho", "iqs_pot_pib", "iqs_pot_pig")

  dt <- 10 #Longueur d'un pas de simulation: FIXE

  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  # variables nécessaires à Natura et à extraire des rasters
  variable_climat <- c("totalprecipitation","tmean")
  variable_climat_an <- c("growingseasonprecipitation","growingseasontmean")
  variable_sol <- c("cec","ph","sable","argile","mat_org")
  variable_iqs <- c("iqs_pot_bop","iqs_pot_epb","iqs_pot_epn","iqs_pot_pex","iqs_pot_pib","iqs_pot_pig","iqs_pot_sab","iqs_pot_tho")

  # variables fixes dans le temps nécessaires au modèle Natura (id_pe doit être en premier dans la liste)
  # variables_fixes_temps <- c("id_pe", "sdom_bio", "prec_gs", "temp_gs", "type_eco", "veg_pot", "milieu", "origine",
  #                            "cec", "oc", "ph", "sand", "clay",
  #                            "origBR", "origCT", "origES",
  #                            "vpms2", "vpms6", "vprb_", "vpre1", "vpre2", "vpre3", "vprp1", "vprs1", "vprs2", "vprs3",
  #                            "mp0","mp1","mp2","mp3","mp4","mp5","mp6","mp789",
  #                            "iqs_epn", "iqs_epx", "iqs_rt", "iqs_ri", "iqs_bop", "iqs_peu", "iqs_ft", "iqs_sab")

#############################################################################################
################### Vérification des arguments de la fonction        #######################
############################################################################################

  # Vérification des arguments de la fonction principale
  Erreur <- CheckArguments(file_arbre=file_arbre, file_etude=file_etude, file_compile=file_compile, horizon=horizon, mode_simul=mode_simul, nb_iter=nb_iter,
                           iqs=iqs, climat=climat, sol=sol, ht=ht, vol=vol, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
  if (Erreur != "ok") {stop(Erreur)}


  if (length(seed_value)>0) {set.seed(seed_value)}
  if (mode_simul != 'STO') {nb_iter=1}



  ###########################################################################################################
  ################### Importation des fichiers échelle arbre et préparation des données #####################
  ###########################################################################################################


  if (!missing(file_arbre)) {


    if (isTRUE(climat)){
      file_arbre <- remove_columns(file_arbre, variable_climat_)
    }

    if (isTRUE(sol)){
      file_arbre <- remove_columns(file_arbre, variable_sol_)
    }

    if (isTRUE(iqs)){
      file_arbre <- remove_columns(file_arbre, variable_iqs_)
    }



    ##################################################################################
    ################### Lecture des fichiers arbres         ##########################
    ##################################################################################

    # si le volume est fourni, on bypass le calcul de la hauteur car on en n'a pas besoin
    if (isFALSE(vol)) ht <- FALSE

    # Lecture du fichier des arbres
    Arbres <- Lecture_arbres(file=file_arbre, ht=ht, vol=vol, iqs=iqs, climat=climat, sol=sol)
    if (is.character(Arbres)) {stop(Arbres)}
    # Valider le contenu des colonnes reliées aux arbres (si ht et/ou vol fournis, ils ne sont pas validés. Ils seront validés indirectement par les vxxx et vtot)
    Arbres <- valid_arbre(type_fic='arbres', fichier=Arbres)
    if (is.character(Arbres)) {stop(Arbres)} # s'il y a des erreurs, on arrête la simulation

    Arbres <- Arbres %>%
      filter(dhpcm>9) %>%
      mutate(no_arbre=row_number())


    # Lecture du fichier des arbres études
    EtudeA <- Lecture_etudes(file=file_etude)
    if (is.character(EtudeA)) {stop(EtudeA)}
    # Valider le contenu des colonnes reliées aux arbres etudes
    EtudeA <- valid_arbre(type_fic='etudes', fichier=EtudeA)
    if (is.character(EtudeA)) {stop(EtudeA)} # s'il y a des erreurs, on arrête la simulation

    EtudeA <- EtudeA %>%
      filter(dhpcm>9, toupper(etage) %in% c('C','D')) %>%
      dplyr::select(id_pe, essence, dhpcm, hauteur)


    ##################################################################################
    ################### Filtrer et préparer les données        #######################
    ##################################################################################

    # Filtrer les placettes
    filtre <- valid_placette(type_fic='arbres', fichier=Arbres, ht=ht, iqs=iqs, climat=climat, sol=sol)

    Arbres <- filtre[[1]] # fichier filtré
    placette_rejet <- filtre[[2]] # liste des placettes rejetées avec le message

    # si toutes les placettes ont été rejetées, arrêter la simulation
    if (nrow(Arbres)==0) stop("Aucune placette valide dans le fichier des arbres")

    # filtrer les placettes et créer les variables échelle placettes (mp, vp, sdom)
    # Arbres <- Filtrer_place(fichier=Arbres)

    # liste des placettes
    liste_place <- unique(Arbres$id_pe)
    # ne garder les placettes qui sont dans les arbres et dans les etude
    EtudeA <- EtudeA[EtudeA$id_pe %in% liste_place,]


    liste_place_etude <- unique(EtudeA$id_pe)
    # ne garder que les placettes qui ont des arbres études
    Arbres <- Arbres[Arbres$id_pe %in% liste_place_etude,]

    # Vérifier s'il reste des placettes valides
    if (nrow(Arbres)==0) {stop("Aucune placette valide dans le fichier des arbres-études")}
    # faire un fichier des variables fixes dans le temps: PAS NÉCESSAIRE ICI
    #data_info0 <- Arbres %>% dplyr::select(id_pe, sdom_bio, type_eco, origine) %>% unique()
    #print("fin prep des fichiers")

    # extraire les variables de climat pour le modèle de hauteur si nécessaire
    if (isTRUE(climat & isTRUE(ht))) {
      Arbres <- ExtractMap::extract_map_plot(file=Arbres, liste_raster="cartes_climat", variable=variable_climat) %>%
        rename(t_ma = tmean, p_tot = totalprecipitation)
    }


    ##################################################################################
    ################### Compiler les arbres à la placette   ##########################
    ##################################################################################

    # Calculer la ht et le volume des arbres à la step 0 pour toutes les itérations et compiler le fichier à l'échelle de la placettes pour toutes les itérations
    # est-ce que le fichier va être trop gros, ça va créer une ligne par placette par iter
    prep_arb <- Prep_arbres(fic_arbre=Arbres, ht=ht, vol=vol, nb_iter=nb_iter, nb_step=1, mode_simul=mode_simul, seed_value=seed_value, dt=dt)
    DataCompile <- prep_arb[[2]]      # extraire le fichier compile a la placette
    Arbres_prep <- prep_arb[[1]]      # extraire le fichier des arbres pour le calcul de la hauteur dominante

    # appliquer la fonction qui calcule la hdom de la placette à partir des études d'arbres, pour toutes les itérations
    HD <- Prep_etude(fic_etude=EtudeA, fic_arbre=Arbres_prep, nb_iter=nb_iter, mode_simul=mode_simul, seed_value = seed_value)
    #print("fin Prep_etude()")

    # ajouter la hdom au fichier compile
    # ce fichier contient la compilation de toutes les placettes à la step 0, pour toutes les itérations
    # si déterministe, iter=1
    DataCompile_final0 <- left_join(DataCompile, HD, by = c("iter", "id_pe"))


  } # fin de s'il y a un fichier arbre


  ###########################################################################################################
  ################### Importation des fichiers échelle placette et préparation des données ##################
  ###########################################################################################################

    if (!missing(file_compile)) {

        # file_compile = fichier_compile_sanscov; horizon=5; mode_simul='STO'; nb_iter = 30; iqs=T; climat=TRUE; sol=T; seed_value=NULL; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;
        # file_compile = fichier_compile_aveccov; horizon=5; mode_simul='DET'; nb_iter = 1; iqs=F; climat=F; sol=F; seed_value=NULL; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;
        # file_compile = fic; horizon=9; mode_simul='DET'; nb_iter = 1; iqs=F; climat=F; sol=F; seed_value=NULL; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;
        # file_compile = fic; horizon=5; mode_simul='DET'; iqs=F; sol=F; climat=F;

      ##################################################################################
      ################### Lecture du fichier compilé placette ##########################
      ##################################################################################

      if (isTRUE(climat)){
        file_compile <- remove_columns(file_compile, variable_climat)
      }

      if (isTRUE(sol)){
        file_compile <- remove_columns(file_compile, variable_sol)
      }

      if (isTRUE(iqs)){
        file_compile <- remove_columns(file_compile, variable_iqs)
      }

        # Lecture du fichier compilé à la placette
        DataCompile_final0 <- Lecture_compile(file=file_compile, iqs=iqs, climat=climat, sol=sol)
        if (is.character(DataCompile_final0)) {stop(DataCompile_final0)}

        ##################################################################################
        ################### Filtrer et préparer les données        #######################
        ##################################################################################

        # Filtrer les placettes
        #DataCompile_final0 <- Filtrer_place(fichier=DataCompile_final0)

        # Filtrer les placettes
        filtre <- valid_placette(type_fic='compile', fichier=DataCompile_final0, iqs=iqs, climat=climat, sol=sol)

        DataCompile_final0 <- filtre[[1]] # fichier filtré
        placette_rejet <- filtre[[2]] # liste des placettes rejetées avec le message

        # si toutes les placettes ont été rejetées, arrêter la simulation
        if (nrow(DataCompile_final0)==0) stop("Aucune placette valide dans le fichier file_compile")

        # si stochastique, créer un fichier avec toutes les itérations
        if (mode_simul=='STO'){
          DataCompile_final0 <- do.call(rbind, replicate(nb_iter, DataCompile_final0, simplify = FALSE))
          DataCompile_final0 <- DataCompile_final0 %>%
            group_by(id_pe) %>%
            mutate(iter = row_number()) %>%
            ungroup()
        }
        else{
          DataCompile_final0 <- DataCompile_final0 %>% mutate(iter=1)
        }

        # créer et renommer les variables qui seront nécessaires
        DataCompile_final0 <- DataCompile_final0 %>%
          mutate(nbop1 = nbop/25, npeu1 = npeu/25, nft1 = nft/25, nepn1 = nepn/25, nepx1 = nepx/25, nsab1 = nsab/25, nri1 = nri/25, nrt1 = nrt/25) %>%
          dplyr::select(-nbop, -npeu, -nft, -nri, -nrt, -nepx, -nepn, -nsab) %>%
          rename(stbop1=stbop, stpeu1=stpeu, stft1=stft, stri1=stri, strt1=strt, stepx1=stepx, stepn1=stepn, stsab1=stsab,
                 vbop1=vbop, vpeu1=vpeu, vft1=vft, vri1=vri, vrt1=vrt, vepx1=vepx, vepn1=vepn, vsab1=vsab,
                 hd1=hd, is1=is)

    } # fin de la préparation du fichier compilé


    ################################################################################################
    ################### Évolution du fichier compilé        ########################################
    ################################################################################################

  # validation des ntot, sttot et vtot
  filtre <- valid_placette(type_fic='valid', fichier=DataCompile_final0)
  #if (is.character(DataCompile_final0)) {stop(DataCompile_final0)}
  DataCompile_final0 <- filtre[[1]] # fichier filtré
  placette_rejet2 <- filtre[[2]] # liste des placettes rejetées avec le message

  # si toutes les placettes ont été rejetées, arrêter la simulation
  if (nrow(DataCompile_final0)==0) stop("Aucune placette valide selon les totaux de N, St ou V")


  ##################################################################################
  ################### Préparer les variables nécessaires au modèle #################
  ##################################################################################

  # pour les extractions dans les carte, on peut passer le fichier avec les itérations, car dans extract_map, il y a une unique de id_pe et un merge à la fin avec le fichier d'origine

  # extraire les variables de sol si nécessaire, couche=1 ==> 0-5cm
  if (isTRUE(sol)) {
    DataCompile_final0 <- ExtractMap::extract_map_plot(file=DataCompile_final0, liste_raster="cartes_sol", variable=variable_sol) %>%
      rename(sand=sable, oc=mat_org, clay=argile)
  }

  # extraire les variables de climat si nécessaire
  if (isTRUE(climat)) {
    DataCompile_final0 <- ExtractMap::extract_climat_an(file=DataCompile_final0, variable=variable_climat_an, periode=dt) %>%
      rename(prec_gs = growingseasonprecipitation, temp_gs = growingseasontmean)
  }
  # extraire les variables de climat si nécessaire, avec le package de MF
  #if (isTRUE(climat)) {
  #  DataCompile_final0 <- ExtractMap::extract_climat_an(file=DataCompile_final0, variable=variable_climat_an, periode=dt) %>%
  #    rename(prec_gs = growingseasonprecipitation, temp_gs = growingseasontmean)
  #}
  # extraire les variables d'iqs si nécessaire
  if (isTRUE(iqs)) {
    DataCompile_final0 <- DataCompile_final0 <- ExtractMap::extract_map_plot(file=DataCompile_final0, liste_raster="cartes_iqs", variable=variable_iqs)
  }

  # Préparer les variables binaires, pour toutes les itérations, car on calcule vtot, qui peut varier s'il provient d'un fichier arbre en mode stochastique
  DataCompile_final0 <- Prep_compile(fichier_compile=DataCompile_final0)



        # générer les paramètres pour l'évolution de IS et HD, N-ST-V
        liste_place <- unique(DataCompile_final0$id_pe)
        param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon, seed_value = seed_value)
        param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon, liste_ess=liste_gress, seed_value = seed_value)

        # Temps 0
        PlacT0 <- DataCompile_final0 %>%
          dplyr::select(iter, id_pe, annee, temps, tbe, pert, hd1, is1,
                        stbop1, stpeu1, stft1, stri1, strt1, stsab1, stepn1, stepx1, sttot1,
                        nbop1, npeu1, nft1, nri1, nrt1, nsab1, nepn1, nepx1, ntot1,
                        vbop1, vpeu1, vft1, vri1, vrt1, vsab1, vepn1, vepx1, vtot1,
                        pct_bop1, pct_peu1, pct_ft1, pct_ri1, pct_rt1, pct_sab1, pct_epn1, pct_epx1)

        # verif <- DataCompile_final0 %>% dplyr::select(-all_of(variables_fixes_temps))

        # faire un fichier des variables fixes dans le temps (on n'a pas besoin de altitude, p_tot, t_ma, )
        data_info <- DataCompile_final0 %>% filter(iter==1) %>%
          dplyr::select(id_pe, sdom_bio, prec_gs, temp_gs, contains("iqs_"), contains('orig'),
                        type_eco, veg_pot, milieu,
                        mp0, mp1, mp2, mp3, mp4, mp5, mp6, mp789,
                        cec, oc, ph, sand, clay,
                        contains('vpr'), contains('vpm')) # ne pas faire contains("vp)" car ça l'inclut vpeu1, et il ne faut pas écrire contrains("mp") car ça garde temps
        # data_info <- DataCompile_final0[DataCompile_final0$iter==1, variables_fixes_temps]


        # enlever les variable de data_info du fichier DataCompile_final, sauf id_pe
        DataCompile_final0[colnames(data_info)[-1]]<- list(NULL) # [-1] car il faut garder le premier id_pe

        if (mode_simul=='STO') {
        # paralleliser les itérations
        # doFuture::registerDoFuture() # pas nécessaire quand on utilise %dofuture%
        # future::plan(multisession)
        n.cores <- parallel::detectCores() - 1
        #my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK") # type = "PSOCK" est par défaut
        my.cluster <- parallel::makeCluster(n.cores)
        doParallel::registerDoParallel(cl = my.cluster)
        outputFinal<- bind_rows(
          foreach(x = 1:nb_iter) %dopar% #.packages = "Natura3"
          #foreach(x = 1:nb_iter) %dofuture% #.packages = "Natura3"
          #foreach(x = 1:nb_iter) %dopar%
            {

              simul_oneIter_compileV2(fichier=DataCompile_final0[DataCompile_final0$iter==x,], horizon=horizon, PlacT0=PlacT0[PlacT0$iter==x,], data_info=data_info,
                                      iteration=x, mode_simul=mode_simul,
                                      param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress,
                                      long_int=dt, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
            }
        )
        parallel::stopCluster(cl = my.cluster)
        }
        # initialiser le mode parallèle prend plusieurs secondes, on n'en a pas besoin en mode déterministe
        if (mode_simul=='DET') {
          x <- 1
          outputFinal <- simul_oneIter_compileV2(fichier=DataCompile_final0[DataCompile_final0$iter==x,], horizon=horizon, PlacT0=PlacT0[PlacT0$iter==x,], data_info=data_info,
                                  iteration=x, mode_simul=mode_simul,
                                  param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress,
                                  long_int=dt, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
        }


  ################################################################################################
  ################### Préparation du fichier de sortie    ########################################
  ################################################################################################

  # faire un fichier des variables fixes dans le temps
    data_info0 <- data_info %>%  dplyr::select(id_pe, sdom_bio, type_eco, origine)
    outputFinal2 <- right_join(data_info0, outputFinal, by="id_pe") %>%
      mutate(nft1 = nft1*25,
             nrt1 = nrt1*25,
             nri1 = nri1*25,
             nepn1 = nepn1*25,
             nepx1 = nepx1*25,
             nbop1 = nbop1*25,
             npeu1 = npeu1*25,
             nsab1 = nsab1*25,
             ntot1 = ntot1*25,
             dqrt1 = ifelse(nrt1>0, sqrt((strt1*40000)/(nrt1*pi)), NA),
             dqft1 = ifelse(nft1>0, sqrt((stft1*40000)/(nft1*pi)), NA),
             dqri1 = ifelse(nri1>0, sqrt((stri1*40000)/(nri1*pi)), NA),
             dqepn1 = ifelse(nepn1>0, sqrt((stepn1*40000)/(nepn1*pi)), NA),
             dqepx1 = ifelse(nepx1>0, sqrt((stepx1*40000)/(nepx1*pi)), NA),
             dqsab1 = ifelse(nsab1>0, sqrt((stsab1*40000)/(nsab1*pi)), NA),
             dqbop1 = ifelse(nbop1>0, sqrt((stbop1*40000)/(nbop1*pi)), NA),
             dqpeu1 = ifelse(npeu1>0, sqrt((stpeu1*40000)/(npeu1*pi)), NA),
             dqtot1 = ifelse(ntot1>0, sqrt((sttot1*40000)/(ntot1*pi)), NA)
      ) %>%
      ungroup()
    # enlever les 1 au bout des noms de colonnes
    names(outputFinal2) <- gsub('1','', names(outputFinal2))

    if (mode_simul=='DET'){
      outputFinal2 <- outputFinal2 %>% dplyr::select(-iter)
    }

   # Exporter la simulation
   if (!missing(file_export)) {
     write_delim(outputFinal2, file_export, delim = ';')
   }

    # liste des placettes rejetées
    if (!is.null(placette_rejet) | !is.null(placette_rejet2)) {
      placette_rejet_tous <- as.data.frame(bind_rows(placette_rejet, placette_rejet2) %>% arrange(id_pe))
      # ajouter les placettes rejetées à la fin du fichier des simulations
      outputFinal2 <- bind_rows(outputFinal2, placette_rejet_tous)
      }


   return(as.data.frame(outputFinal2))

} # fin de la fct principale






