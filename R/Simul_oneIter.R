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



#########################################################################################################
#########################################################################################################
#########################################################################################################


#' Effectue une simulation complète (tous les pas de simulation) pour une itération, lorsque le fichier en entrée est à l'échelle de la placette, version à utiliser lorsque les paramètres IS et HS sont dans le même objet
#'
#' @description Effectue une simulation complète (tous les pas de simulation) pour une itération, lorsque le fichier en entrée est à l'échelle de la placette, version à utiliser lorsque les paramètres IS et HS sont dans le même objet.
#'
#' @param fichier Table contenant toutes les variables nécessaires pour effectuer une simulation, incluant les paramètres, une ligne par placette
#' @param iteration Numéro de l'itération
#' @param PlacT0 Table contenant seulement les colonnes associées aux caractéristiques dendrométriques des placettes
#' @param data_info Table contenant seulement les colonnes associées aux caractéristiques des placettes qui ne changent pas dans le temps
#' @param param_ishd_evol Objet contenant les paramètres du modèle de l'indice de Shannon et du modèle de hauteur dominante, pour toutes les itérations et pas de simulation
#' @param param_n_st_v Objet contenant les paramètres des modèles de Nxxx, STxxx et Vxxx, pour toutes les itération et pas de smulation
#' @param liste_ess Vecteur contenant les codes des groupes d'essences de Natura
#' @param long_int Nombre d'années correspondant à un pas de simulation
#' @inheritParams SimulNatura
#'
#' @return Table avec une ligne par placette et pas de simulation contenant les prévision de HD et IS et N, ST et V de chacun des groupes d'essences
#' @export
#'
# @examples
simul_oneIter_compileV2 <- function(fichier, horizon, iteration, PlacT0, data_info, mode_simul, param_ishd_evol, param_n_st_v, liste_ess,
                                  long_int, dec_perturb, dec_tbe1, tbe1, dec_tbe2, tbe2)
{

  # fichier=DataCompile_final0[DataCompile_final0$iter==1,]; horizon=horizon; PlacT0=PlacT0[PlacT0$iter==1,]; data_info=data_info;  iteration=1; mode_simul=mode_simul;
  # param_ishd_evol=param_ishd_evol; param_n_st_v=param_n_st_v; liste_ess=liste_gress;
  # long_int=dt; dec_perturb=dec_perturb; dec_tbe1=dec_tbe1; tbe1=tbe1; dec_tbe2=dec_tbe2; tbe2=tbe2;


  DataCompile_final <- fichier

  # faire la préparation des paramètres qui changent seulement avec l'itération
  Data_param_iter <- Prep_parametre_iter(iteration=iteration, param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_ess)

  # pour chaque pas de simulation
  data_simule_tous <- NULL
  for (k in 1:horizon) {

    # k=1

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
    # DataCompile_final <- Plac3
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

