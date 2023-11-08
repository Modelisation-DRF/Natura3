################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Select and add to plot dataframe the growth model          #
#   parameters associated with the time step and iteration     #
#                                                              #
#                                                              #
################################################################


#' Select the growth model parameters associated with a time step and an iteration for a list of plots for a simulation with Natura 3.0
#'
#' @description Select and add to plot dataframe all the growth models parameters associated with a time step and an iteration.
#' It also determine the dominant species of the plot and select iqs associated with this species.
#'
#' @param data Dataframe with only plot id (id_pe) and variables of percentage of group species basal area (pct_epn, pct_epx, etc)
#' @param pas Time step number
#' @inheritParams simul_oneIter_compile
#'
#' @return Dataframe of plot characteristics and all their growth models parameter values for a time step and an iteration, ready to be simulated
#' @export
#'
# @examples
Prep_parametre <- function(data, data_info, iteration, pas, mode_simul, param_is_evol, param_hd_evol, param_n_st_v, liste_ess){

  ii <- iteration
  k <- pas

  # déterminer l'essence principale de la placette à chaque pas de simulation pour les modèles hd et is selon le max de pct_xxx
  # déterminer aussi l'iqs de l'essence dominante pour le modèle de hd
  ess_max <- data %>% dplyr::select(-id_pe)
  ess_max$ess_dom <- gsub('1','', substr(names(ess_max)[max.col(ess_max[,1:8])],5,7)) # il faut enlever le 1 au bout du nom de l'ess
  ess_max$iqs <- eval(parse(text = paste0('data_info$iqs_',ess_max$ess_dom)))
  data <- bind_cols(data['id_pe'],ess_max[,c('ess_dom','iqs')])
  data$ess_dom[data$ess_dom %in% c('rt','epx')] <- 'epn'
  data$ess_dom[data$ess_dom =='ft'] <- 'bop'

  liste_ess <- toupper(liste_ess)
  # N-St-V: effets fixes de chaque essence, tous sur une même ligne, change seulement avec l'itération
  param_nstv <- bind_cols(lapply(liste_ess, function(x) param_n_st_v[[x]]$effet_fixe %>% filter(iter==ii) %>% dplyr::select(-iter)))
  # N-St-V: erreur résiduelle de placette : une ligne par placette/pas
    if (mode_simul=='STO') {
      resid_tous <- data['id_pe']
      for (x in liste_ess) {
        # erreur résiduelle de n-st-v de chaque groupe d''essence à l'échelle de la placette, qui change à chaque pas de simulation pour une même placette
        resid <-param_n_st_v[[x]]$erreur_residuelle %>% filter(iter==ii, pas==k) %>% dplyr::select(-iter, -pas)
        # ajouter le nom de l'essence au bout de res_n, res_st, res_v
        names(resid) <- c('id_pe', paste0('res_n',tolower(x)), paste0('res_st',tolower(x)), paste0('res_v',tolower(x)))
        resid_tous <- inner_join(resid_tous, resid, by='id_pe')
    }
    Data2 <- inner_join(data, resid_tous, by='id_pe') %>% bind_cols(param_nstv)
  }
  if (mode_simul!='STO'){
    Data2 <- data %>%
      mutate(res_nepn=0, res_stepn=0, res_vepn=0, res_nepx=0, res_stepx=0, res_vepx=0, res_nsab=0, res_stsab=0, res_vsab=0,
             res_nrt=0, res_strt=0, res_vrt=0, res_nri=0, res_stri=0, res_vri=0, res_nbop=0, res_stbop=0, res_vbop=0,
             res_npeu=0, res_stpeu=0, res_vpeu=0, res_nft=0, res_stft=0, res_vft=0) %>%
    bind_cols(param_nstv)
  }

  # IS et HD: effet fixe de l'essence dominante
  liste <- names(param_is_evol)
  param_is <- bind_rows(lapply(liste, function(x)  param_is_evol[[x]]$effet_fixe %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x))))
  param_hd <- bind_rows(lapply(liste, function(x)  param_hd_evol[[x]]$effet_fixe %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x))))
  param_is_hd <- inner_join(param_is, param_hd, by='ess_dom')# %>% mutate(ess_dom_ass=ess_dom) %>% dplyr::select(-ess_dom)

  # ajouter les paramètres de is et hs selon l'essence dominante
  # il n'y a pas de modèle pour rt-ft-epx, les associer avec une des 5 essences modélisées
  Data3 <- left_join(Data2, param_is_hd, by='ess_dom')

  if (mode_simul=='STO') {
    rand_is <- bind_rows(lapply(liste, function(x) param_is_evol[[x]]$random_placette %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x)) %>% rename(rand_plot_is=random_plot)))
    resid_is <- bind_rows(lapply(liste, function(x) param_is_evol[[x]]$erreur_residuelle %>% filter(iter==ii, pas==k) %>% dplyr::select(-iter, -pas) %>% mutate(ess_dom=tolower(x)) %>% rename(res_plot_is=res_plot))) %>% dplyr::select(res_plot_is)

    rand_hd <- bind_rows(lapply(liste, function(x) param_hd_evol[[x]]$random_placette %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x))%>% rename(rand_plot_hd=random_plot))) %>% dplyr::select(rand_plot_hd)
    resid_hd <- bind_rows(lapply(liste, function(x) param_hd_evol[[x]]$erreur_residuelle %>% filter(iter==ii, pas==k) %>% dplyr::select(-iter, -pas) %>% mutate(ess_dom=tolower(x)) %>% rename(res_plot_hd=res_plot)))   %>% dplyr::select(res_plot_hd)

    erreur_tous <- bind_cols(rand_is, resid_is, rand_hd, resid_hd)

    # ajouter les erreurs de is et hs selon l'essence dominante de la placette
    Data4 <- left_join(Data3, erreur_tous, by=c('id_pe','ess_dom') )
  }
  if (mode_simul!='STO') {
    Data4 <- Data3 %>% mutate(rand_plot_is=0, res_plot_is=0, rand_plot_hd=0, res_plot_hd=0)
  }

  #print("Fin Prep_parametre()")
  return(Data4)
}



#' Select the growth model parameters associated with a time step for a list of plots for a simulation with Natura 3.0
#'
#' @description Select and add to plot dataframe all the growth models parameters associated with a time step.
#' It also determine the dominant species of the plot and select iqs associated with this species.
#'
#' @param data Dataframe with only plot id (id_pe) and variables of percentage of group species basal area (pct_epn, pct_epx, etc)
#' @param pas Time step number
#' @inheritParams simul_oneIter_compile
#'
#' @return Dataframe of plot characteristics and all their growth models parameter values for a time step
#' @export
#'
# @examples
Prep_parametre_pas <- function(data, data_info, iteration, pas, mode_simul, param_ishd_evol, param_n_st_v, liste_ess){

  ii <- iteration
  k <- pas


  # déterminer l'essence principale de la placette à chaque pas de simulation pour les modèles hd et is selon le max de pct_xxx
  # déterminer aussi l'iqs de l'essence dominante pour le modèle de hd
  ess_max <- data %>% dplyr::select(-id_pe)
  ess_max$ess_dom <- gsub('1','', substr(names(ess_max)[max.col(ess_max[,1:8])],5,7)) # il faut enlever le 1 au bout du nom de l'ess
  ess_max$iqs <- eval(parse(text = paste0('data_info$iqs_',ess_max$ess_dom)))
  data <- bind_cols(data['id_pe'],ess_max[,c('ess_dom','iqs')])
  data$ess_dom[data$ess_dom %in% c('rt','epx')] <- 'epn'
  data$ess_dom[data$ess_dom =='ft'] <- 'bop'


  # N-St-V: erreur résiduelle de placette : une ligne par placette/pas
  if (mode_simul=='STO') {
    liste_ess <- toupper(liste_ess)
    resid_tous <- data['id_pe']
    for (x in liste_ess) {
      # erreur résiduelle de n-st-v de chaque groupe d''essence à l'échelle de la placette, qui change à chaque pas de simulation pour une même placette
      resid <-param_n_st_v[[x]]$erreur_residuelle %>% filter(iter==ii, pas==k) %>% dplyr::select(-iter, -pas)
      # ajouter le nom de l'essence au bout de res_n, res_st, res_v
      names(resid) <- c('id_pe', paste0('res_n',tolower(x)), paste0('res_st',tolower(x)), paste0('res_v',tolower(x)))
      resid_tous <- inner_join(resid_tous, resid, by='id_pe')
    }
    Data2 <- inner_join(data, resid_tous, by="id_pe")
  }
  if (mode_simul!='STO'){
    Data2 <- data %>%
      mutate(res_nepn=0, res_stepn=0, res_vepn=0, res_nepx=0, res_stepx=0, res_vepx=0, res_nsab=0, res_stsab=0, res_vsab=0,
             res_nrt=0, res_strt=0, res_vrt=0, res_nri=0, res_stri=0, res_vri=0, res_nbop=0, res_stbop=0, res_vbop=0,
             res_npeu=0, res_stpeu=0, res_vpeu=0, res_nft=0, res_stft=0, res_vft=0)
  }


  if (mode_simul=='STO') {
    liste <- names(param_ishd_evol)

    # rand_is <- bind_rows(lapply(liste, function(x) param_is_evol[[x]]$random_placette %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x)) %>% rename(rand_plot_is=random_plot)))
    # resid_is <- bind_rows(lapply(liste, function(x) param_is_evol[[x]]$erreur_residuelle %>% filter(iter==ii, pas==k) %>% dplyr::select(-iter, -pas) %>% mutate(ess_dom=tolower(x)) %>% rename(res_plot_is=res_plot))) %>% dplyr::select(res_plot_is)
    #
    # rand_hd <- bind_rows(lapply(liste, function(x) param_hd_evol[[x]]$random_placette %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x))%>% rename(rand_plot_hd=random_plot))) %>% dplyr::select(rand_plot_hd)
    # resid_hd <- bind_rows(lapply(liste, function(x) param_hd_evol[[x]]$erreur_residuelle %>% filter(iter==ii, pas==k) %>% dplyr::select(-iter, -pas) %>% mutate(ess_dom=tolower(x)) %>% rename(res_plot_hd=res_plot)))   %>% dplyr::select(res_plot_hd)
    # erreur_tous <- bind_cols(rand_is, resid_is, rand_hd, resid_hd)

    # avec la fct param_ishd_evol_stoch(), is et hd sont dans le même fichier
    rand <- bind_rows(lapply(liste, function(x) param_ishd_evol[[x]]$random_placette %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x))))
    resid <- bind_rows(lapply(liste, function(x) param_ishd_evol[[x]]$erreur_residuelle %>% filter(iter==ii, pas==k) %>% dplyr::select(-iter, -pas, -id_pe)))
    erreur_tous <- bind_cols(rand, resid)

    # ajouter les erreurs de is et hs selon l'essence dominante de la placette
    Data3 <- left_join(Data2, erreur_tous, by=c('id_pe','ess_dom') )
  }
  if (mode_simul!='STO') {
    Data3 <- Data2 %>% mutate(rand_plot_is=0, res_plot_is=0, rand_plot_hd=0, res_plot_hd=0)
  }
  return(Data3) # un fichier du même nombre de lignes que de placettes
}


#' Select the growth model parameters associated with an iteration for a list of plots for a simulation with Natura 3.0
#'
#' @description Select the growth models parameters associated with an iteration.
#'
#' @inheritParams simul_oneIter_compile
#'
#' @return Dataframe of one observation with all the growth models parameter values for an iteration
#' @export
#'
# @examples
Prep_parametre_iter <-function(iteration, param_ishd_evol, param_n_st_v, liste_ess){

  ii <- iteration

  liste_ess <- toupper(liste_ess)
  # N-St-V: effets fixes de chaque essence, tous sur une même ligne, change seulement avec l'itération, retourne un data d'une ligne
  param_nstv <- bind_cols(lapply(liste_ess, function(x) param_n_st_v[[x]]$effet_fixe %>% filter(iter==ii) %>% dplyr::select(-iter)))
  # IS-HD: effets fixes de chaque essence, une ligne par essence, change seulement avec l'itération, retourne chacun un data de 5 lignes
  liste <- names(param_ishd_evol)
  param_ishd <- bind_rows(lapply(liste, function(x)  param_ishd_evol[[x]]$effet_fixe %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x))))
  #param_hd <- bind_rows(lapply(liste, function(x)  param_hd_evol[[x]]$effet_fixe %>% filter(iter==ii) %>% dplyr::select(-iter) %>% mutate(ess_dom=tolower(x))))
  #param_is_hd <- inner_join(param_is, param_hd, by='ess_dom')

  param <- bind_cols(param_ishd, param_nstv) # param_nstv sera copié surles 5 lignes

  return(param) # un fichier de 5 lignes, une par ess_dom

}

