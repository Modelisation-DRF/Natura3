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






#' Sélectionne les paramètres des modèles associés à un pas de simulation, pour le cas où un fichier à l'échelle de la placette est fourni
#'
#' @description Sélectionne les paramètres des modèles associés à un pas de simulation, pour le cas où un fichier à l'échelle de la placette est fourni en entrée.
#' Ces paramètres sont ceux qui changent à chauqe pas de simulation pour une itération donnée, soit les erreurs résiduelles.
#' Ajoute les paramètres au fichier compilé à la placette. Détermine le groupe d'essences dominant et sélectionne l'IQS associé.
#'
#' @param data Table contenant seulement l'identifiant des placettes (id_pe) et les variables contenant le pourcentage de surface terrière de chaque groupe d'essences (pct_epn1, pct_epx1, etc),  une ligne par placette
#' @param pas Numéro du pas de simulation
#' @inheritParams simul_oneIter_compileV2
#'
#' @return Table contenant toutes les colonnes nécessaires pour effectuer un pas de simulation pour un pas de simulation.
# #' @export
#'
# @examples
Prep_parametre_pas <- function(data, data_info, iteration, pas, mode_simul, param_ishd_evol, param_n_st_v, liste_ess){


  # data=data_temp; data_info=data_info; iteration=iteration; mode_simul=mode_simul; pas=k; param_ishd_evol=param_ishd_evol; param_n_st_v=param_n_st_v; liste_ess=liste_ess;

  ii <- iteration
  k <- pas


  # déterminer l'essence principale de la placette à chaque pas de simulation pour les modèles hd et is selon le max de pct_xxx
  # déterminer aussi l'iqs de l'essence dominante pour le modèle de hd
  iqs_info <- data_info %>% ungroup() %>% dplyr::select(contains("iqs"))
  ess_max <- data %>% dplyr::select(-id_pe)
  ess_max$ess_dom <- gsub('1','', substr(names(ess_max)[max.col(ess_max[,1:8], ties.method = "first")],5,7)) # il faut enlever le 1 au bout du nom de l'ess, par defaut la méthode est random pour choisir en cas d'égalité, je vais mettre first
  # il n'y a pas de modèles de hd pour epx-rt-ft, on emprunte une essence;
  ess_max$ess_dom[ess_max$ess_dom %in% c('rt','epx')] <- 'epn'
  ess_max$ess_dom[ess_max$ess_dom =='ft'] <- 'bop'
  # ajouter les 8 iqs
  ess_max <- bind_cols(ess_max,iqs_info)
  #ess_max$iqs <- eval(parse(text = paste0('data_info$iqs_',ess_max$ess_dom))) # ça ne marche pas ici: c'est le bon data_info$iqs_xxx qui s'écrit à chaque ligne, mais quand on fait eval(data_info$iqs_xxx) sur une ligne, ça retourne toues les iqs_xxx du fichier, et non pas un seul,
  ess_max <- ess_max %>% mutate(
    iqs = ifelse(ess_dom=='epn', iqs_epn,
                    ifelse(ess_dom=='sab', iqs_sab,
                           ifelse(ess_dom=='ri', iqs_ri,
                                  ifelse(ess_dom=='bop', iqs_bop,
                                        ifelse(ess_dom=='peu',iqs_peu, NA)))))
  )
  data <- bind_cols(data['id_pe'],ess_max[,c('ess_dom','iqs')])


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



#' Sélectionne les paramètres des modèles associés à une itération, pour le cas où un fichier à l'échelle de la placette est fourni
#'
#' @description Sélectionne les paramètres des modèles associés à une itération, pour le cas où un fichier à l'échelle de la placette est fourni en entrée.
#' Ces paramètres sont ceux qui sont fixes pour toute une itération, donc pour tous les pas de simulation, soit les paramètres des effets fixes.
#'
#' @inheritParams simul_oneIter_compileV2
#'
#' @return Table avec une seule ligne contenant tous les paramètres des effets fixes des modèles
# #' @export
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

