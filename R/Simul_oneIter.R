################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Functions for a complete simulation (all time steps)       #
#   of one iteration with Natura simulator                     #
#                                                              #
#                                                              #
################################################################


#' Complete simulation (all time steps) for one iteration with Natura 3.0 simulator when the start file is already compiled at the plot scale
#'
#' @description Do a Complete simulation (all time steps) for one iteration with Natura 3.0 simulator when the start file is already compiled at the plot scale.
#'
#' @param fichier Dataframe with stand characteristics at step 0 and all covariates, ready to be simulated, one line per plot
#' @param iteration Iteration number
#' @param PlacT0 Dataframe with only stand characteristics at step 0 (no covariates)
#' @param data_info Dataframe with only fixed stand characteristics over time
#' @param param_is_evol Object with shannon index growth model parameter values for all time steps and all iterations
#' @param param_hd_evol Object with dominant height growth model parameter values for all time steps and all iterations
#' @param param_n_st_v Object with n-st-v growth models parameter values for all time steps and all iterations
#' @param liste_ess Vector with Natura group species code names
#' @param long_int Length of time steps (years)
#' @inheritParams SimulNatura
#'
#' @return Dataframe with one line per plot per time step with predicted values of HD and IS, and N, ST and V per group species
#' @export
#'
# @examples
simul_oneIter_compile <- function(fichier, horizon, iteration, PlacT0, data_info, mode_simul, param_is_evol, param_hd_evol, param_n_st_v, liste_ess,
                                  long_int, dec_perturb, dec_tbe1, tbe1, dec_tbe2, tbe2)
{
  DataCompile_final <- fichier

   # pour chaque pas de simulation
  data_simule_tous <- NULL
  for (k in 1:horizon) {

    # ne garder que la liste de placettes avec les variables de pct_ess
    data_temp <-DataCompile_final %>% dplyr::select(id_pe, contains('pct_')) %>% ungroup()
    # ajouter les paramètres des équations de Natura à la liste de placette
    Data_param <- Prep_parametre(data=data_temp , data_info=data_info, iteration=iteration, mode_simul=mode_simul, pas=k, param_is_evol=param_is_evol,
                                 param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_ess)
    # Simulation pour une irétation et un pas de simulation
    DataCompile_final <- Natura_oneStep(data=DataCompile_final, param=Data_param, data_info=data_info, long_int=long_int, pas=k,
                                        dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
    # append des pas de simulations
    data_simule_tous <- bind_rows(data_simule_tous, DataCompile_final)
  }
  # Ajout du point de départ de la simulation aux simulations
  outputFinaliter <- bind_rows(PlacT0, data_simule_tous) %>% mutate(iter=iteration)
  return(outputFinaliter)
}

#########################################################################################################
#########################################################################################################
#########################################################################################################


#' Complete simulation (all time steps) for one iteration with Natura 3.0 simulator when the start file is already compiled at the plot scale, version where parameters of HD and IS are in the same objects
#'
#' @description Do a Complete simulation (all time steps) for one iteration with Natura 3.0 simulator when the start file is already compiled at the plot scale. Function version where parameters of HD and IS are in the same objects.
#'
#' @param fichier Dataframe with stand characteristics at step 0 and all covariates, ready to be simulated, one line per plot
#' @param iteration Iteration number
#' @param PlacT0 Dataframe with only stand characteristics at step 0 (no covariates)
#' @param data_info Dataframe with only fixed stand characteristics over time
#' @param param_ishd_evol Object with shannon index and dominant height growth model parameter values for all time steps and all iterations
#' @param param_n_st_v Object with n-st-v growth models parameter values for all time steps and all iterations
#' @param liste_ess Vector with Natura group species code names
#' @param long_int Length of time steps (years)
#' @inheritParams SimulNatura
#'
#' @return Dataframe with one line per plot per time step with predicted values of HD and IS, and N, ST and V per group species
#' @export
#'
# @examples
simul_oneIter_compileV2 <- function(fichier, horizon, iteration, PlacT0, data_info, mode_simul, param_ishd_evol, param_n_st_v, liste_ess,
                                  long_int, dec_perturb, dec_tbe1, tbe1, dec_tbe2, tbe2)
{
  DataCompile_final <- fichier

  # faire la préparation des paramètres qui changent seulement avec l'itération
  Data_param_iter <- Prep_parametre_iter(iteration=iteration, param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_ess)

  # pour chaque pas de simulation
  data_simule_tous <- NULL
  for (k in 1:horizon) {

    # ne garder que la liste de placettes avec les variables de pct_ess
    data_temp <-DataCompile_final %>% dplyr::select(id_pe, contains('pct_')) %>% ungroup()
    # ajouter les paramètres des équations de Natura à la liste de placette
    #Data_param <- Prep_parametre(data=data_temp , data_info=data_info, iteration=iteration, mode_simul=mode_simul, pas=k, param_is_evol=param_is_evol,
    #                             param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_ess)
    # ajouter les paramètres des équations de Natura à la liste de placette, ceux qui changent à chaque pas
    Data_param_pas <- Prep_parametre_pas(data=data_temp , data_info=data_info, iteration=iteration, mode_simul=mode_simul, pas=k,
                                         param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_ess)
    # ajouter les parametres qui ne changent pas à chaque pas
    Data_param <- inner_join(Data_param_pas, Data_param_iter, by=c("ess_dom"))
    # Simulation pour une irétation et un pas de simulation
    DataCompile_final <- Natura_oneStep(data=DataCompile_final, param=Data_param, data_info=data_info, long_int=long_int, pas=k,
                                        dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
    # append des pas de simulations
    data_simule_tous <- bind_rows(data_simule_tous, DataCompile_final)
  }
  # Ajout du point de départ de la simulation aux simulations
  outputFinaliter <- bind_rows(PlacT0, data_simule_tous) %>% mutate(iter=iteration)
  return(outputFinaliter)
}

#########################################################################################################
#########################################################################################################
#########################################################################################################


#' Complete simulation (all time steps) of one iteration with Natura 3.0 simulator when the start file is at the tree scale
#'
#' @description Do a complete simulation (all time steps) of one iteration with Natura 3.0 simulator when the start file is at the tree scale
#'
#' @param fic_arbre Dataframe of trees per plots, filtered and with the covariates
#' @param fic_etude Dataframe of study trees per plots
#' @param iteration Iteration number
#' @param param_hdom0_ess Object with step 0 species tree height estimation model parameter values for all iterations (for HD estimation at step 0)
#' @param param_hdom0_global Object with step 0 global tree height estimation model parameter values for all iterations (for HD estimation at step 0)
#' @param parametre_ht Object with tree height estimation model parameter values for all time steps and all iterations
#' @param parametre_vol Object with tree volume estimation model parameter values for all time steps and all iterations
#' @param liste_ess_ht Vector with species code name for which there is a tree height model
#' @param long_int Length of time steps (years)
#' @inheritParams SimulNatura
#' @inheritParams simul_oneIter_compile
#'
#' @return Dataframe with one line per plot per time step with predicted values of HD and IS, and N, ST and V per group species
#' @export
#'
# @examples
simul_oneIter_arbre <- function(fic_arbre, fic_etude, ht, vol, horizon, iteration, mode_simul, param_is_evol, param_hdom0_ess, param_hdom0_global, parametre_ht, parametre_vol,
                              liste_ess_ht, param_hd_evol, param_n_st_v, liste_ess, long_int, dec_perturb, dec_tbe1, tbe1, dec_tbe2, tbe2)
{

  # data <- fic_arbre
  # # générer les paramètres des modèles utilisés à la step 0
  # liste_arbre <-  data[,c('id_pe', 'no_arbre')]
  # parametre_ht <- param_ht_stoch(liste_arbre=liste_arbre, nb_iter=1, mode_simul=mode_simul, liste_ess=liste_ess_ht)
  # #print("fin parametre_ht()")
  # parametre_vol <- param_vol_stoch(liste_arbre=liste_arbre, nb_iter=1, mode_simul=mode_simul)
  # #print("fin parametre_vol()")
  # param_hdom0_ess <- param_hdom0_ess_stoch(liste_arbre=liste_arbre, nb_iter=1, mode_simul=mode_simul)
  # #print("fin param_hdom0_ess()")
  # param_hdom0_global <- param_hdom0_stoch(liste_arbre=liste_arbre, nb_iter=1, mode_simul=mode_simul)
  # #print("fin param_hdom0_global()")
  # # générer les paramètres pour l'évolution de IS et HD, N-ST-V
  # liste_place <- unique(data$id_pe)
  # param_is_evol <- param_is_evol_stoch(liste_place=liste_place, nb_iter=1, mode_simul=mode_simul, horizon=horizon)
  # #print("fin param_is_evol()")
  # param_hd_evol <- param_hd_evol_stoch(liste_place=liste_place, nb_iter=1, mode_simul=mode_simul, horizon=horizon)
  # #print("fin param_hd_evol()")
  # param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_place, nb_iter=1, mode_simul=mode_simul, horizon=horizon, liste_ess=liste_gress)
  # #print("fin param_n_st_v()")

  # fic_arbre=Arbres[Arbres$groupe==1,]; fic_etude=EtudeA[EtudeA$groupe==1,];
  # iteration=1; param_hdom0_global=param_hdom0_global[[1]]; parametre_vol=parametre_vol; long_int=dt;

  prep_arb <- Prep_arbres(fic_arbre=fic_arbre, ht=ht, vol=vol, iteration=iteration, mode_simul=mode_simul, parametre_ht=parametre_ht, parametre_vol=parametre_vol, liste_ess_ht=liste_ess_ht) # la fct retourne maintenant 2 fichiers: la liste des arbres et la compilation placette
  #print("fin prep_arb()")
  DataCompile <- prep_arb[[2]]      # extraire le fichier compile a la placette
  Arbres_prep <- prep_arb[[1]]      # extraire le fichier des arbres pour le calcul de la hauteur dominante
  # appliquer la fonction qui calcule la hdom de la placette à partir des études d'arbres
  HD <- Prep_etude(fic_etude=fic_etude, fic_arbre=Arbres_prep, iteration=iteration, mode_simul=mode_simul, param_hdom0_ess=param_hdom0_ess, param_hdom0_global=param_hdom0_global)
  #print("fin Prep_etude()")
  # ajouter la hdom au fichier compile
  DataCompile_final <- left_join(DataCompile, HD, by = "id_pe")
  # Placette au temps 0 de l'iteration i
  PlacT0 <- DataCompile_final %>%
    dplyr::select(id_pe, annee, temps, tbe, pert, hd1, is1,
                  stbop1, stpeu1, stft1, stri1, strt1, stsab1, stepn1, stepx1, sttot1,
                  nbop1, npeu1, nft1, nri1, nrt1, nsab1, nepn1, nepx1, ntot1,
                  vbop1, vpeu1, vft1, vri1, vrt1, vsab1, vepn1, vepx1, vtot1,
                  pct_bop1, pct_peu1, pct_ft1, pct_ri1, pct_rt1, pct_sab1, pct_epn1, pct_epx1)
  # faire un fichier des variables fixes dans le temps
  data_info <- DataCompile_final %>% dplyr::select(id_pe, sdom_bio, altitude, p_tot, t_ma, prec_gs, temp_gs, contains("iqs_"), contains('orig'),
                                                   type_eco, veg_pot, contains('vpm'), contains('vpr'), milieu, mp0, mp1, mp2, mp3, mp4, mp5, mp6, mp789,
                                                   cec, oc, ph, sand, silt, clay)
  # enlever les data_info du fichier DataCompile_final
  DataCompile_final[colnames(data_info)[-1]]<- list(NULL)
  liste_nom <- names(DataCompile_final)
  # pour chaque pas de simulation
  data_simule_tous <- NULL
  for (k in 1:horizon) {
    # ne garder que la liste de placettes avec les pct_ess
    data_temp <-DataCompile_final %>% dplyr::select(id_pe, contains('pct_')) %>% ungroup()
    # ajouter les paramètres des équations de Natura à la liste de placette
    Data_param <- Prep_parametre(data=data_temp, data_info=data_info, iter=iteration, mode_simul=mode_simul, pas=k, param_is_evol=param_is_evol,
                                 param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_ess)
    #print("fin prep_parametre()")
    # Simulation pour une irétation et un pas de simulation
    DataCompile_final <- Natura_oneStep(data=DataCompile_final, param=Data_param, data_info=data_info, long_int=long_int, pas=k,
                                dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
    #print("fin natura_onestep()")
    # append des pas de simulations: POUURA SE FAIRE AVEC UN LAPPLY
    data_simule_tous <- bind_rows(data_simule_tous, DataCompile_final)
  }
  # Ajout du point de départ de la simulation aux simulations
  outputFinaliter <- bind_rows(PlacT0, data_simule_tous)  %>% mutate(iter=iteration)

  return(outputFinaliter)
}


