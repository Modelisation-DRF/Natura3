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

#' Main function of Natura 3.0 growth simulator
#'
#' @description \code{SimulNatura()} is the main function of Natura 3.0 growth simulator, the version using plot origine and time since origine.
#'
# @details
#'
#' @param file_arbre Name of the file containing tree and plot informations (dataframe, xlsx, csv). One line per tree or per species/dbh class.
#' If not specified, \code{file_compile} must be provided. Column names must be:
#'  \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item origine: code d'origine de la placette (ES: épidémie sévère, BR: brulis, CT: coupe totale)
#'    \item temps_origine: temps depuis l'origine (ans)
#'    \item sdom_bio: sous-domaine bioclimatique, en majuscule (ex: 2E, 4O)
#'    \item altitude: altitude en m
#'    \item type_eco: type écologique (ex: MS22)
#'    \item essence: code d'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (seul les arbres de plus de 9 cm sont retenus)
#'    \item nb_tige: nombre d'arbres de l'essence et de la classe de dhp, à l'ha
#'    \item etat: code d'état de l'arbre, seuls les vivants sont retenus, codes 10, 12, 30, 32, 40, 42, 50, 52
#'  }
#' @param file_etude Name of the file containing the study trees (dataframe, xlsx, csv). One line per tree.
#'  if not specified, \code{file_compile} must be specified. Column names must be:
#'  \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item étage: code d'étage de l'arbre: C, D, O, I, V, seul les C-D sont retenus
#'    \item essence: code d'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item dhpcm: dhp de l'arbre-étude (cm)
#'    \item hauteur: hauteur de l'arbre étude (m)
#'  }
#' @param file_compile Name of the file containig the compiled plots (dataframe, xlsx, csv). One line per plot.
#'  if not specified, \code{file_arbre} and \code{file_etude} must be provided. Column names must be:
#'  \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item origine: code d'origine de la placette (ES: épidémie sévère, BR: brulis, CT: coupe totale)
#'    \item temps_origine: temps depuis l'origine (ans)
#'    \item sdom_bio: sous-domaine bioclimatique, en majuscule (ex: 2E, 4O)
#'    \item altitude: altitude en m
#'    \item type_eco: type écologique (ex: MS22)
#'    \item is: indice de structure diamètrale de Shannon (valeur entre 0 et 1; 0 : toutes les tiges sont dans la même classe de dhp; 1 : les tiges sont également distribuées dans 19 classes de dhp)
#'    \item hd: hauteur dominante, hauteur moyenne des 100 plus gros arbres à l'ha (m)
#'    \item nxxx : nombre d'arbres de plus de 9cm du groupe d'essences xxx (/ha), mettre 0 si absent. xxx : each of 8 species groups (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item stxxx: surface terrière marchande du groupe d'essences xxx (m2/ha), mettre 0 si absent. xxx : each of 8 species groups (bop, peu, ft, ri, rt, epx, epn, sab)
#'    \item vxxx: volume marchand brut du groupe d'essences xxx (m3/ha), mettre 0 si absent. Si aucun volume, mettre les 8 groupes à 0 et l'évolution du volume ne sera pas effectué. xxx : each of 8 species groups (bop, peu, ft, ri, rt, epx, epn, sab)
#' }
#' @param file_export Name of the file to export, will contain the simulation results (format csv), facultative
#' @param horizon Number of 10-year time steps to simulate, 1 à 15
#' @param mode_simul Simulation mode: \code{"DET"} : deterministic or \code{"STO"} : stochastic
#' @param nb_iter Number of iteration if stochastic mode, ignored if \code{mode_simul="DET"}
#' @param iqs A boolean
#' \itemize{
#'    \item \code{TRUE} (default): if potential site index are to be extrated from maps. Columns latitude and longitude must be provided in \code{file_arbre} or in \code{file_compile}.
#'    \itemize{
#'       \item latitude, longitude: coordinates of plots, in decimal degrees
#'    }
#'    \item \code{FALSE}: if potential site index (m) are provided in \code{file_arbre} or \code{in file_compile}. Column names must be:
#'    \itemize{
#'      \item iqs_pot_sab, iqs_pot_epn, iqs_pot_epb, iqs_pot_pig, iqs_pot_tho, iqs_pot_pib, iqs_pot_bop, iqs_pot_pex.
#'    }
#'  }
#' @param climat A boolean
#' \itemize{
#'    \item \code{TRUE} (default): if climate variables are to be extrated from maps. Columns latitude, longitude, an_mes must be provided in \code{file_arbre} or in \code{file_compile}.
#'    \itemize{
#'      \item latitude, longitude: coordinates of plots, in decimal degrees
#'      \item an_mes: measurement year of the plot
#'    }
#'   \item \code{FALSE}: if climate variables are in \code{file_arbre} or in \code{file_compile}. Column names must be:
#'   \itemize{
#'     \item p_tot: 1980-2010 mean total precipitations (mm)
#'     \item t_ma: 1980-2010 mean annual temprature (Celcius)
#'     \item prec_gs: last 10-years mean growing season precipitations (mm)
#'     \item temp_gs: last 10-years mean growing season temperature (Celcius)
#'   }
#' }
#' @param sol A boolean
#' \itemize{
#'   \item \code{TRUE} (default): if soil variables are to be extrated from maps. Columns latitude and longitude must be provided in \code{file_arbre} or in \code{file_compile}.
#'   \itemize{
#'     \item latitude, longitude: coordinates of plots, in decimal degrees
#'     }
#'   \item \code{FALSE}: if SIIGSOL soil variables are in \code{file_arbre} or in \code{file_compile}. Soils properties at depth 0-5 cm. Column names must be:
#'   \itemize{
#'     \item ph: pH
#'     \item clay: fraction of clay
#'     \item silt: fraction of silt
#'     \item sand: fraction of sand
#'     \item cec: capacité d'échange cationique
#'     \item oc: matière organique
#'   }
#' }
#' @param ht A boolean
#' \itemize{
#'   \item \code{TRUE} (default): if tree height must be estimated, when \code{file_arbre} is specified
#'   \item \code{FALSE}: if tree height is in \code{file_arbre}, ignored if \code{file_compile} is specified. Column name must be hauteur_pred (in meters)
#'   }
#' @param vol A boolean
#' \itemize{
#'   \item \code{TRUE} (default): if tree volume must be estimated, when \code{file_arbre} is specified
#'   \item \code{FALSE}: if tree volume is in \code{file_arbre}, ignored if \code{file_compile} is specified. If provided, column name must be vol_dm3 (in dm3)
#'   }
#' @param dec_perturb The simulation step number with a presence of perturbation (other than spruce budworm)
#' @param dec_tbe1 The simulation step number with a presence of a first spruce budworm defoliation
#' @param tbe1 Severity of the first spruce budworm defoliation, integer 0 to 5, 0  : no defoliation, 5 : 100 \% defoliation
#' @param dec_tbe2 The simulation step number with a presence of a second spruce budworm defoliation
#' @param tbe2 Severity of the second spruce budworm defoliation, integer 0 to 5, 0  : no defoliation, 5 : 100 \% defoliation
#'
#' @return A data frame with the simulation results (HD, IS, Nxxx, STxxx, Vxxx, DQxxx), one line per plot/time step/iteration
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulation déterministe sur 50 ans à partir d'un fichier à l'échelle de l'arbre, en ne fournissant pas les covariables nécessaires
#' data_simul <- SimulNatura(file_arbre=fichier_arbres_aveccov, file_etude=fichier_arbres_etudes, horizon=5)
#'
#' # Simulation stochastique sur 50 ans à partir d'un fichier à l'échelle de l'arbre, en fournissant les covariables nécessaires
#' data_simul <- SimulNatura(file_arbre=fichier_arbres_aveccov, file_etude=fichier_arbres_etudes, horizon=5, mode_simil='STO', iqs=FALSE, sol=FALSE, climat=FALSE)
#'
#' # Simulation déterministe sur 50 ans à partir d'un fichier à l'échelle de la placette, en ne fournissant pas les covariables nécessaires
#' data_simul <- SimulNatura(file_compile=fichier_compile_sanscov, horizon=5)
#' }

#file_arbre=fichier_arbres_select; file_etude=fichier_etudes_select; horizon=5; mode_simul='DET'; iqs=TRUE; climat=TRUE; sol=TRUE; ht=TRUE; vol=TRUE; nb_iter=1;

SimulNatura <- function(file_arbre, file_etude, file_compile, file_export, horizon, mode_simul='DET', nb_iter=1, iqs=TRUE, climat=TRUE, sol=TRUE, ht=TRUE, vol=TRUE,
                        dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  {

  # pour pouvoir remettre l'option par defaut à la fin de la fonction
  onMisuse <- getOption("doFuture.rng.onMisuse", getOption("future.rng.onMisuse"))
  oopts <- options(future.rng.onMisuse = onMisuse)
  on.exit(options(oopts), add = TRUE)

  # pour ne pas afficher le message de warning de %dopar% sur les random number. Utiliser dorng serait la solution, mais ne s'utilise pas en double %dorng%
  options(doFuture.rng.onMisuse = "ignore")

  if (missing(dec_perturb)) dec_perturb <- 0
  if (missing(dec_tbe1)) dec_tbe1 <- 0
  if (missing(tbe1)) tbe1 <- 0
  if (missing(dec_tbe2)) dec_tbe2 <- 0
  if (missing(tbe2)) tbe2 <- 0
  if (missing(ht)) ht <- TRUE
  if (missing(vol)) vol <- TRUE

  dt = 10 #Longueur d'un pas de simulation: FIXE

  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  liste_ess_ht <- unique(ht_ass_ess$essence_hauteur)

#############################################################################################
################### Vérification des arguments de la fonction        #######################
############################################################################################

  # Vérification des arguments de la fonction principale
  Erreur <- CheckArguments(file_arbre=file_arbre, file_etude=file_etude, file_compile=file_compile, horizon=horizon, mode_simul=mode_simul, nb_iter=nb_iter,
                           iqs=iqs, climat=climat, sol=sol, ht=ht, vol=vol, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
  if (Erreur != 'ok') {
    print(Erreur)
    Data_simule_tous <- Erreur
  }
  # Commencer l'exécution de la simulation si tous les arguments sont ok
  else {

    if (mode_simul!='STO') {nb_iter=1}

  #############################################################################################
  ################### Importation des fichiers et préparatin des données ######################
  ############################################################################################


  if (!missing(file_arbre)) {

    # si le volume est fourni, on bypass le calcul de la hauteur car on en n'a pas besoin
    if (isFALSE(vol)) ht <- FALSE

    # Lecture du fichier des arbres
    Arbres <- Lecture_arbres(file=file_arbre, ht=ht, vol=vol, iqs=iqs, climat=climat, sol=sol)
    if (is.character(Arbres)) print(Arbres) # Erreur lors de la lecture du fichier des arbres
    EtudeA <- Lecture_etudes(file=file_etude)
    if (is.character(EtudeA)) print(EtudeA) # Erreur de la lecture des arbres études

    # extraire les variables de sol si nécessaire (couche 1 = 0-5 cm)
    if (isTRUE(sol)) {
      Arbres <- extract_map_plot(file=Arbres, liste_raster=cartes_sol, couche=1) %>%
        rename(cec=cec.x000.005cm, oc=oc.x000.005cm, ph=ph.x000.005cm, sand=sand.x000.005cm, silt=silt.x000.005cm, clay=clay.x000.005cm)
    }

    # extraire les variables de climat si nécessaire
    if (isTRUE(climat)) {
      Arbres <- extract_map_plot(file=Arbres, liste_raster=cartes_climat, couche=1)
      Arbres <- extract_climat(file=Arbres, liste_raster=cartes_climat_an, couche=1) %>%
        rename(prec_gs = growingseasonprecipitation, temp_gs = growingseasontmean, t_ma = tmean, p_tot = totalprecipitation)
    }

    # extraire les variables d'iqs si nécessaire
    if (isTRUE(iqs)) {
      Arbres <- extract_map_plot(file=Arbres, liste_raster=cartes_iqs, couche=1)
    }

    # filtrer les placettes
    Arbres <- Filtrer_place(fichier=Arbres)
    # liste des placettes
    liste_place <- unique(Arbres$id_pe)
    # ne garder les placettes qui sont dans les arbres et dans les etude
    EtudeA <- EtudeA[EtudeA$id_pe %in% liste_place,]
    liste_place_etude <- unique(EtudeA$id_pe)
    # ne garder que les placettes qui ont des arbres études
    Arbres <- Arbres[Arbres$id_pe %in% liste_place_etude,]
    ### AJOUTER UNE VÉRIFICATION SI LE FICHIER ARBRES EST VIDE APRÈS AVOIR FILTER LES PLACETTES


    # faire un fichier des variables fixes dans le temps
    data_info0 <- Arbres %>% dplyr::select(id_pe, sdom_bio, type_eco, origine) %>% unique()

    #print("fin prep des fichiers")
    if (!is.character(Arbres) & !is.character(EtudeA)) { # si tout es ok

      # générer les paramètres des modèles utilisés à la step 0
      liste_arbre <-  Arbres[,c('id_pe', 'no_arbre')]
      parametre_ht <- param_ht(fic_arbres=liste_arbre, nb_iter=nb_iter, mode_simul=mode_simul)
      parametre_vol <- param_vol(fic_arbres=liste_arbre, nb_iter=nb_iter, mode_simul=mode_simul)
      param_hdom0_ess <- param_hdom0_ess_stoch(liste_arbre=liste_arbre, nb_iter=nb_iter, mode_simul=mode_simul)
      param_hdom0_global <- param_hdom0_stoch(liste_arbre=liste_arbre, nb_iter=nb_iter, mode_simul=mode_simul)
      # générer les paramètres pour l'évolution de IS et HD, N-ST-V
      liste_place <- unique(Arbres$id_pe)
      param_is_evol <- param_is_evol_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon)
      param_hd_evol <- param_hd_evol_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon)
      param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon, liste_ess=liste_gress)

      # paralleliser les itérations
      # faire des groupes 5000 placettes, car plus de 5000, c'est long

        # Erreur obtenue avec un ficgier de 100000 placettes et 100 itérations:
        # Error in getGlobalsAndPackages(expr, envir = globals_envir, globals = globals) :
        # The total size of the 22 globals exported for future expression (‘{; doFuture::registerDoFuture(); lapply(seq_along(...future.x_ii), FUN = function(jj) {; ...future.x_jj <- ...future.x_ii[[jj]]; {; NULL; ...; }, error = identity); }); }’)
        # is 19.49 GiB.. This exceeds the maximum allowed size of 500.00 MiB (option 'future.globals.maxSize').
        # The three largest globals are ‘parametre_vol’ (16.74 GiB of class ‘list’), ‘param_hdom0_global’ (2.38 GiB of class ‘list’) and ‘Arbres’ (368.78 MiB of class ‘list’)

      liste_groupe <- Arbres %>% dplyr::select(id_pe) %>% unique() # liste des placettes
      nb_groupe <- round(nrow(liste_groupe)/5000,0)+1 # nombre de groupes de 5000 placettes
      liste_groupe <- liste_groupe %>%
        mutate(groupe = row_number() %% nb_groupe+1)
      #print(nb_groupe)
      #print(table(liste_groupe$groupe))
      # ajouter le numéro de groupe au fichier des arbres et au fichier des arbres-etudes
      Arbres <- inner_join(Arbres, liste_groupe, by="id_pe")
      EtudeA <- inner_join(EtudeA, liste_groupe, by="id_pe")

      registerDoFuture()
      plan(multisession)
      outputFinal <- #bind_rows(
        foreach(gr = 1:nb_groupe, .combine = rbind) %:%
          foreach(x = 1:nb_iter, .combine = rbind) %dopar%
          {
            simul_oneIter_arbre(fic_arbre=Arbres[Arbres$groupe==gr,], fic_etude=EtudeA[EtudeA$groupe==gr,], ht=ht, vol=vol, horizon=horizon, iteration=x, mode_simul=mode_simul,
                              param_hdom0_ess=param_hdom0_ess, param_hdom0_global=param_hdom0_global[[x]],
                              parametre_ht=parametre_ht, parametre_vol=parametre_vol, liste_ess_ht=liste_ess_ht,
                              param_is_evol=param_is_evol, param_hd_evol=param_hd_evol,
                              param_n_st_v=param_n_st_v, liste_ess=liste_gress,
                              long_int=dt, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
          }
      #)
      plan(sequential)
    } # fin de si tout est ok
  } # fin de s'il y a un fichier arbre

  else { # si le fichier d'inventaire est déjà un fichier compile
    if (!missing(file_compile)) {

        # file_compile = fichier_compile_select; horizon=5; mode_simul='STO'; nb_iter = 100; iqs=TRUE; climat=TRUE; sol=TRUE;

        # Lectude du fichier compilé à la placette
        DataCompile_final0 <- Lecture_compile(file=file_compile, iqs=iqs, climat=climat, sol=sol)
        if (is.character(DataCompile_final0)) print(DataCompile_final0) # Erreur lors de la lecture du fichier compilé

        # extraire les variables de sol si nécessaire, couche=1 ==> 0-5cm
        if (isTRUE(sol)) {
          DataCompile_final0 <- extract_map_plot(file=DataCompile_final0, liste_raster=cartes_sol, couche=1) %>%
            rename(cec=cec.x000.005cm, oc=oc.x000.005cm, ph=ph.x000.005cm, sand=sand.x000.005cm, silt=silt.x000.005cm, clay=clay.x000.005cm)
        }

        # extraire les variables de climat si nécessaire
        if (isTRUE(climat)) {
          DataCompile_final0 <- extract_map_plot(file=DataCompile_final0, liste_raster=cartes_climat, couche=1)
          DataCompile_final0 <- extract_climat(file=DataCompile_final0, liste_raster=cartes_climat_an, couche=1) %>%
            rename(prec_gs = growingseasonprecipitation, temp_gs = growingseasontmean, t_ma = tmean, p_tot = totalprecipitation)
        }

        # extraire les variables d'iqs si nécessaire
        if (isTRUE(iqs)) {
          DataCompile_final0 <- DataCompile_final0 <- extract_map_plot(file=DataCompile_final0, liste_raster=cartes_iqs, couche=1)
        }

        # Filtrer les placettes
        DataCompile_final0 <- Filtrer_place(fichier=DataCompile_final0)

        # faire un fichier des variables fixes dans le temps
        data_info0 <- DataCompile_final0 %>% dplyr::select(id_pe, sdom_bio, type_eco, origine)

        # générer les paramètres pour l'évolution de IS et HD, N-ST-V
        liste_place <- unique(DataCompile_final0$id_pe)
        #param_is_evol <- param_is_evol_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon)
        #param_hd_evol <- param_hd_evol_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon)
        param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon)
        param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_place, nb_iter=nb_iter, mode_simul=mode_simul, horizon=horizon, liste_ess=liste_gress)

        # je peux sortir ça de la boucle des itérations car rien de stochastique au temps 0 quand le fichier est déjà compilé
        PlacT0 <- DataCompile_final0 %>%
          dplyr::select(id_pe, annee, temps, tbe, pert, hd1, is1,
                        stbop1, stpeu1, stft1, stri1, strt1, stsab1, stepn1, stepx1, sttot1,
                        nbop1, npeu1, nft1, nri1, nrt1, nsab1, nepn1, nepx1, ntot1,
                        vbop1, vpeu1, vft1, vri1, vrt1, vsab1, vepn1, vepx1, vtot1,
                        pct_bop1, pct_peu1, pct_ft1, pct_ri1, pct_rt1, pct_sab1, pct_epn1, pct_epx1)
        # faire un fichier des variables fixes dans le temps
        data_info <- DataCompile_final0 %>% dplyr::select(id_pe, sdom_bio, altitude, p_tot, t_ma, prec_gs, temp_gs, contains("iqs_"), contains('orig'),
                                                          type_eco, veg_pot, contains('vpm'), contains('vpr'), milieu, mp0, mp1, mp2, mp3, mp4, mp5, mp6, mp789,
                                                         cec, oc, ph, sand, silt, clay)
        # enlever les data_info du fichier DataCompile_final
        DataCompile_final0[colnames(data_info)[-1]]<- list(NULL)
        #liste_nom <- names(DataCompile_final0)

        # paralleliser les itérations
        #registerDoFuture()
        #plan(multisession)
        n.cores <- parallel::detectCores() - 1
        my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
        doParallel::registerDoParallel(cl = my.cluster)
        outputFinal<- bind_rows(
          foreach(x = 1:nb_iter) %dopar%
            {
              # simul_oneIter_compile(fichier=DataCompile_final0, horizon=horizon, PlacT0=PlacT0, data_info=data_info, iteration=x, mode_simul=mode_simul,
              #                     param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress,
              #                     long_int=dt, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
              simul_oneIter_compileV2(fichier=DataCompile_final0, horizon=horizon, PlacT0=PlacT0, data_info=data_info, iteration=x, mode_simul=mode_simul,
                                    param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress,
                                    long_int=dt, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
            }
        )
        parallel::stopCluster(cl = my.cluster)
      } # fin quand il y a un fichier deja compilé
    } # fin du else si pas un fichier arbre
    outputFinal2 <- inner_join(data_info0, outputFinal, by="id_pe", multiple='all') %>%
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
             )
  } # fin de si tous les arguments sont ok

  # Exporter la simulation
  if (!missing(file_export)) {
    write_delim(outputFinal2, file_export, delim = ';')
  }
  return(outputFinal2)

} # fin de la fct principale


# ajouter les placettes non simulées aux placettes simulées




