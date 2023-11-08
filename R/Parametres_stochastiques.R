################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Functions generating parameter values of Natura models     #
#                                                              #
#   Use                                                        #
#   hdom_param_ess_fixe.rda                                    #
#   hdom_param_global_fixe.rda                                 #
#   is_param_fixe.rda                                          #
#   is_param_cov.rda                                           #
#   hdevol_param_fixe.rda                                      #
#   hdevol_param_cov.rda                                       #
#   n_st_v_param_fixe.rda                                      #
#   n_st_v_param_cov.rda                                       #
#   n_st_v_param_random.rda                                    #
#                                                              #
#                                                              #
################################################################


#######################################################################################################################


#' Generating parameters of the tree species height model for step 0 domimant height estimation
#'
#' @description Generate parameters of the tree species height model for step 0 dominant height estimation, deterministic or stochastic, for each tree of each plot and for all iterations.
#'
#' @details
#' The species tree height estimation model for dominant height estimation is a non linear mixed model calibrated per species. There is a tree random effect
#' and the residual errors are not corralated. There is no covariance matrix of fixed effects, since there is only one parameter.
#'
#' @param liste_arbre Dataframe with tree ID (no_arbre) and plot Id (id_pe) for which tree height must be predicted for dominant height estimation
#' @inheritParams SimulNatura
#'
#' @return  A list of lists, one list per species for which there is a tree height model, and for each species, a list with 3 elements:
#' \enumerate{
#'   \item essence: string with species code name
#'   \item effet_fixe: dataframe of fixed effect parameter values (one line per iteration)
#'   \item erreur_arbre: dataframe of residual error values (random tree effect values and residual error values, one line per tree per iteration)
#' }
#' @export
#'
# @examples
param_hdom0_ess_stoch <- function(liste_arbre, nb_iter, mode_simul) {

  # fichier des paramètres effets fixes (hdom_param_ess_fixe.rda)
  param_ess <- hdom_param_ess_fixe

  # liste des arbres x nb_iter pour accueillir les erreurs résiduelle et les random arbre
  data_arbre <- as.data.frame(unclass(expand_grid(iter = 1:nb_iter, id_pe = liste_arbre)))

    liste_ess = unique(param_ess$essence2) #24
    tous <- list()
    for (ess in liste_ess) {
      # générer effets fixes de l'essence
      param_i <- param_ess[param_ess$essence2==ess, 2:4]

      if (mode_simul=='STO') {

      param_hd = data.frame('b2'=rnorm(n = nb_iter, mean=as.matrix(param_i[1,2]), sd = as.matrix(param_i[1,3]))) %>%
        mutate(iter = row_number(), ess_eq_hd=ess)

      # générer erreurs échelle arbre de l'essence
      rand_arbre_hd = data.frame('random_arbre'=rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(param_i[3,2]))))
      resi_arbre_hd = data.frame('resid_arbre'=rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(param_i[2,2]))))
      rand_arbre_hd = as.data.frame(bind_cols(data_arbre,rand_arbre_hd,resi_arbre_hd))
      names(rand_arbre_hd) = c('iter',names(liste_arbre),'random_arbre','resid_arbre')
      rand_arbre_hd$ess_eq_hd=ess
    }
    else
      {  # si mode déterministe
        param_hd <- data.frame('b2'=param_i[param_i$Parameter=='b2', 2]) %>% mutate(iter=1, ess_eq_hd=ess)
        rand_arbre_hd <- 0
    }
      temp <- list('essence'=ess, 'effet_fixe'=param_hd, 'erreur_arbre'=rand_arbre_hd)
      tous <- append(tous, list(temp))
  }
   names(tous) <- liste_ess
 return(tous)
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#' Generating parameters of the global tree height model for step 0 domimant height estimation
#'
#' @description Generate parameters of the global tree height model for step 0 dominant height estimation, deterministic or stochastic, for each tree of each plot and for all iterations.
#'
#' @details
#' The global tree height estimation model for dominant height estimation is a non linear mixed model calibrated globally for all species. There is a tree random effect
#' and the residual errors are not corralated. There is no covariance matrix of fixed effects, since there is only one parameter.
#' This model is used when a species is present in the tree list but not in the study-tree list.
#'
#' @param liste_arbre Dataframe with tree ID (no_arbre) and plot Id (id_pe) for which height must be predicted for dominant height estimation
#' @inheritParams SimulNatura
#'
#' @return A list of 2 elements:
#' \enumerate{
#'   \item effet_fixe: dataframe of fixed effect parameter values (one line per iteration)
#'   \item erreur_arbre: dataframe of residual error values (random tree effect values and residual error values, one line per tree per iteration)
#' }
#'
#' @export
#'
# @examples
param_hdom0_stoch <- function(liste_arbre, nb_iter, mode_simul) {

  # fichier des paramètres (hdom_param_global_fixe.rda)
  param_global <- hdom_param_global_fixe[, 1:3]

  # liste des arbres x nb_iter pour accueillir les erreurs résiduelle et les random arbre
  data_arbre <- as.data.frame(unclass(expand_grid(iter = 1:nb_iter, id_pe = liste_arbre)))

  if (mode_simul=='STO') {

    # générer effets fixes du modele global
    param_hd = data.frame('b2'=rnorm(n = nb_iter, mean=as.matrix(param_global[1,2]), sd = as.matrix(param_global[1,3]))) %>%
      mutate(iter = row_number())

    # générer erreurs échelle arbre du modèle global
    rand_arbre_hd = data.frame('random_arbre'=rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(param_global[3,2]))))
    resi_arbre_hd = data.frame('resid_arbre'=rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(param_global[2,2]))))
    rand_arbre_hd = as.data.frame(bind_cols(data_arbre,rand_arbre_hd,resi_arbre_hd))
    names(rand_arbre_hd) = c('iter',names(liste_arbre),'random_arbre','resid_arbre')

    list_result <- list()
    for (i in 1:nb_iter) {
      list_result[[i]] <- list('effet_fixe'=param_hd[param_hd$iter==i,], 'erreur_arbre'=rand_arbre_hd[rand_arbre_hd$iter==i,])
    }
  }
  else
    {
      param_hd <- param_global %>% filter(Parameter=='b2') %>% dplyr::select(Estimate) %>% mutate(iter=1) %>% rename(b2=Estimate)
      rand_arbre_hd <- 0

      list_result <- list()
      list_result[[1]] <- list('effet_fixe'=param_hd, 'erreur_arbre'=rand_arbre_hd)
  }
  #tous <- list('effet_fixe'=param_hd, 'erreur_arbre'=rand_arbre_hd)
  return(list_result)
}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


#' Generating shannon index growth model parameter values of Natura 3.0
#'
#' @description Generate parameters for shannon index growth model of Natura 3.0, deterministic or stochastic, for each plot for each time step and for all iterations
#'
#' @details
#' The shannon index growth model is a linear mixed model calibrated for each dominant species. There is a plot random effect
#' and the plot residual errors are not corralated. There is a covariance matrix of fixed effects.
#'
#' @param liste_place Dataframe with plot id (id_pe)
#' @inheritParams SimulNatura
#'
#' @return A list of lists, one list per species group for which there is a shannon index growth model, and for each species group, a list with 4 elements:
#' \enumerate{
#'   \item essence: string with species group code name
#'   \item effet_fixe: dataframe of fixed effect parameter values (one line per time step per iteration per species)
#'   \item random_placette: dataframe of random plot effect values (one line per plot per time step per iteration per species)
#'   \item erreur_residuelle: dataframe of residual error values (one line per plot per time step per iteration per species)
#' }
#'
#' @export
#'
# @examples
param_is_evol_stoch <- function(liste_place, nb_iter, mode_simul, horizon){

  nb_pas <- horizon

  # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
  data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)
  # liste des placettes x nb_iter x nb_pas pour accueillir les erreurs résiduelles de placette
  data_plot_pas <- as.data.frame(unclass(expand_grid(pas = 1:nb_pas, id_pe = data_plot)))

  # lecture des paramètres (is_param_fixe.rda, is_param_cov.rda)
  param <-  is_param_fixe
  param_cov <-  is_param_cov

  # générer des paramètres des effets fixes pour une essence dominante donnée
  liste_ess <- unique(param$ess_dom)
  tous <- list()
  for (ess in liste_ess)
    {
    if (mode_simul == 'STO') {
      param_is = as.data.frame(matrix(mvrnorm(n = nb_iter, mu = as.matrix(param[param$ess_dom==tolower(ess),c(1:4,7)]), Sigma = as.matrix(param_cov[param_cov$ess_dom==toupper(ess),2:6])),
                                      nrow=nb_iter))
      names(param_is) <- names(param[param$ess_dom==tolower(ess),c(1:4,7)])
      param_is <- param_is %>% mutate(iter = row_number())

      # générer un effet aléatoire de placette, qui sera le même pour la placette pour toutes ses itérations
      rand_plot <- data.frame('random_plot'=rnorm(n = nb_iter*length(liste_place), mean = 0, sd = sqrt(as.matrix(param[param$ess_dom==tolower(ess),6]))))
      rand_plot <- bind_cols(data_plot,rand_plot)

      # générer une erreur résiduelle à l'échelle de la placette, une pour chaque chaque pas de simulation
      res_plot = data.frame('res_plot'=rnorm(n = nb_iter*length(liste_place)*nb_pas, mean = 0, sd = sqrt(as.matrix(param[param$ess_dom==tolower(ess),5]))))
      res_plot <- bind_cols(data_plot_pas,res_plot)
      names(res_plot) <- c('pas','iter','id_pe','res_plot')
    }
    else{
      param_is <- param[param$ess_dom==tolower(ess),c(1:4,7)] %>% mutate(iter=1)
      rand_plot <- 0
      res_plot <- 0
    }
    temp <- list('essence'=ess, 'effet_fixe'=param_is, 'random_placette'=rand_plot, 'erreur_residuelle'=res_plot)
    tous <- append(tous, list(temp))
  }
  names(tous) <- toupper(liste_ess)
  return(tous)

}


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################



#' Generating dominant height growth model parameter values of Natura 3.0
#'
#' @description Generate parameters for dominant height growth model of Natura 3.0, deterministic or stochastic, for each plot for each time step and for all iterations
#'
#' @details
#' The dominant height growth model is a nonlinear mixed model calibrated for each dominant species. There is a plot random effect
#' and the plot residual errors are not corralated. There is a covariance matrix of fixed effects.
#'
#' @param liste_place Dataframe with plot id (id_pe)
#' @inheritParams SimulNatura
#'
#' @return A list of lists, one list per species group for which there is a dominant height growth model, and for each species group, a list with 4 elements:
#' \enumerate{
#'   \item essence: string with species group code name
#'   \item effet_fixe: dataframe of fixed effect parameter values (one line per time step per iteration per species)
#'   \item random_placette: dataframe of random plot effect values (one line per plot per time step per iteration per species)
#'   \item erreur_residuelle: dataframe of residual error values (one line per plot per time step per iteration per species)
#' }
#' @export
#'
# @examples
param_hd_evol_stoch <- function(liste_place, nb_iter, mode_simul, horizon){

  nb_pas <- horizon

   # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
  data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)

  # liste des placettes x nb_iter x nb_pas pour accueillir les erreurs résiduelles de placette
  data_plot_pas <- as.data.frame(unclass(expand_grid(pas = 1:nb_pas, id_pe = data_plot)))

  # lecture des paramètres (hdevol_param_fixe.rda, hdevol_param_cov.rda)
  param <-  hdevol_param_fixe
  param_cov <-  hdevol_param_cov

  # générer des paramètres des effets fixes pour une essence dominante donnée
  liste_ess <- unique(param$ess_dom)
  tous <- list()
  for (ess in liste_ess)
  {
    if (mode_simul == 'STO') {
      # générer des paramètres des effets fixes pour une essence dominante donnée
      param_hd = as.data.frame(matrix(mvrnorm(n = nb_iter, mu = as.matrix(param[param$ess_dom==tolower(ess),c(1:4,7)]), Sigma = as.matrix(param_cov[param_cov$ess_dom==toupper(ess),2:6])),
                                      nrow=nb_iter))
      names(param_hd) <- names(param[param$ess_dom==tolower(ess),c(1:4,7)])
      param_hd <- param_hd %>% mutate(iter = row_number())

      # générer un effet aléatoire de placette, qui sera le même pour la placette pour toutes ses itérations
      rand_plot <- data.frame('random_plot'=rnorm(n = nb_iter*length(liste_place), mean = 0, sd = sqrt(as.matrix(param[param$ess_dom==tolower(ess),6]))))
      rand_plot <- bind_cols(data_plot,rand_plot)

      # générer une erreur résiduelle à l'échelle de la placette, une pour chaque chaque pas de simulation
      res_plot = data.frame('res_plot'=rnorm(n = nb_iter*length(liste_place)*nb_pas, mean = 0, sd = sqrt(as.matrix(param[param$ess_dom==tolower(ess),5]))))
      res_plot <- bind_cols(data_plot_pas,res_plot)
      names(res_plot) <- c('pas','iter','id_pe','res_plot')
    }
    else{
        param_hd <- param[param$ess_dom==tolower(ess),c(1:4,7)] %>% mutate(iter=1)
        rand_plot <- 0
        res_plot <- 0
    }
    temp <- list('essence'=ess, 'effet_fixe'=param_hd, 'random_placette'=rand_plot, 'erreur_residuelle'=res_plot)
    tous <- append(tous, list(temp))
  }
    names(tous) <- toupper(liste_ess)
    return(tous)
}


#' Generating dominant height growth model parameters and shannon index growth model parameters of Natura 3.0
#'
#' @description Generate parameters for dominant height growth model and shannon index growth model parameters of Natura 3.0, deterministic or stochastic, for each plot for each time step and for all iterations
#'
#' @details
#' The dominant height growth model is a nonlinear mixed model calibrated for each dominant species. There is a plot random effect
#' and the plot residual errors are not corralated. There is a covariance matrix of fixed effects.
#' The shannon index growth model is a linear mixed model calibrated for each dominant species. There is a plot random effect
#' and the plot residual errors are not corralated. There is a covariance matrix of fixed effects.
#'
#' @param liste_place Dataframe with plot id (id_pe)
#' @inheritParams SimulNatura
#'
#' @return A list of lists, one list per species group for which there is a dominant height/shannon index growth model, and for each species group, a list with 4 elements:
#' \enumerate{
#'   \item essence: string with species group code name
#'   \item effet_fixe: dataframe of fixed effect parameter values (one line per time step per iteration per species)
#'   \item random_placette: dataframe of random plot effect values (one line per plot per time step per iteration per species)
#'   \item erreur_residuelle: dataframe of residual error values (one line per plot per time step per iteration per species)
#' }
#' @export
#'
# @examples
param_ishd_evol_stoch <- function(liste_place, nb_iter, mode_simul, horizon){

  nb_pas <- horizon

  # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
  data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)
  # liste des placettes x nb_iter x nb_pas pour accueillir les erreurs résiduelles de placette
  data_plot_pas <- as.data.frame(unclass(expand_grid(pas = 1:nb_pas, id_pe = data_plot)))

  # utilise is_param_fixe.rda, is_param_cov.rda, hdevol_param_fixe.rda, hdevol_param_cov.rda

  # générer des paramètres des effets fixes pour une essence dominante donnée
  liste_ess <- unique(is_param_fixe$ess_dom)
  tous <- list()
  for (ess in liste_ess)
  {
    if (mode_simul == 'STO') {

      # effets fixes, une ligne par ess
      param_is = as.data.frame(matrix(mvrnorm(n = nb_iter, mu = as.matrix(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),c(1:4,7)]),
                                              Sigma = as.matrix(is_param_cov[is_param_cov$ess_dom==toupper(ess),2:6])),
                                      nrow=nb_iter))
      names(param_is) <- names(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),c(1:4,7)])
      param_hd = as.data.frame(matrix(mvrnorm(n = nb_iter, mu = as.matrix(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),c(1:4,7)]),
                                              Sigma = as.matrix(hdevol_param_cov[hdevol_param_cov$ess_dom==toupper(ess),2:6])),
                                      nrow=nb_iter))
      names(param_hd) <- names(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),c(1:4,7)])
      #param <- inner_join(param_is,param_hd,by="ess_dom") %>% mutate(iter = row_number())
      param <- bind_cols(param_is,param_hd) %>% mutate(iter = row_number())


      # is: générer un effet aléatoire de placette, qui sera le même pour la placette pour toutes ses itérations
      rand_plot_is <- data.frame('random_plot'=rnorm(n = nb_iter*length(liste_place), mean = 0, sd = sqrt(as.matrix(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),6]))))
      names(rand_plot_is) <- 'rand_plot_is'
      # hd: générer un effet aléatoire de placette, qui sera le même pour la placette pour toutes ses itérations
      rand_plot_hd <- data.frame('random_plot'=rnorm(n = nb_iter*length(liste_place), mean = 0, sd = sqrt(as.matrix(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),6]))))
      names(rand_plot_hd) <- 'rand_plot_hd'
      rand_plot <- bind_cols(data_plot, rand_plot_is, rand_plot_hd)


      # is: générer une erreur résiduelle à l'échelle de la placette, une pour chaque chaque pas de simulation
      res_plot_is = data.frame('res_plot'=rnorm(n = nb_iter*length(liste_place)*nb_pas, mean = 0, sd = sqrt(as.matrix(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),5]))))
      names(res_plot_is) <- 'res_plot_is'
      # hd: générer une erreur résiduelle à l'échelle de la placette, une pour chaque chaque pas de simulation
      res_plot_hd = data.frame('res_plot'=rnorm(n = nb_iter*length(liste_place)*nb_pas, mean = 0, sd = sqrt(as.matrix(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),5]))))
      names(res_plot_hd) <- 'res_plot_hd'
      res_plot <- bind_cols(data_plot_pas,res_plot_is,res_plot_hd)
      names(res_plot) <- c('pas','iter','id_pe','res_plot_is','res_plot_hd')
    }
    else{
      param_is <- is_param_fixe[is_param_fixe$ess_dom==tolower(ess),c(1:4,7)]
      param_hd <- hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),c(1:4,7)]
      param <- bind_cols(param_is,param_hd) %>% mutate(iter = row_number())
      rand_plot_is <- 0
      rand_plot_hd <- 0
      rand_plot <- cbind(rand_plot_is, rand_plot_hd)
      res_plot_is <- 0
      res_plot_hd <- 0
      res_plot <- cbind(res_plot_is,res_plot_hd)
    }
    temp <- list('essence'=ess, 'effet_fixe'=param, 'random_placette'=rand_plot, 'erreur_residuelle'=res_plot)
    tous <- append(tous, list(temp))
  }
  names(tous) <- toupper(liste_ess)
  return(tous)

}




#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


#' Generating n, st and v growth models parameter values of Natura 3.0
#'
#' @description Generate parameters of n, st and v growth models of Natura 3.0, deterministic or stochastic, for each plot for each time step and for all iterations
#'
#' @details
#' The n, st and v growth models of Natura 3.0 form a system of linear equations calibrated for each species groups. There is no random effect.
#' The plot residual errors are corralated among equations. There is a covariance matrix of fixed effects.
#'
#' @param liste_place Dataframe with plot id (id_pe)
#' @param liste_ess Vector of species group code name of Natura 3.0 models
#' @inheritParams SimulNatura
#'
#' @return A list of lists, one list per species group for which there is n-st-v growth models, and for each species group, a list with 3 elements:
#' \enumerate{
#'   \item essence: string with species group code name
#'   \item effet_fixe: dataframe of fixed effect parameter values (one line per time step per iteration)
#'   \item erreur_residuelle: dataframe of residual error values (one line per plot per time step per iteration)
#' }
#' @export
#'
# @examples
param_evol_n_st_v_stoch <- function(liste_place, nb_iter, mode_simul, horizon, liste_ess){

  nb_pas <- horizon

  # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
  data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)
  # liste des placettes x nb_iter x nb_pas pour accueillir les erreurs résiduelles de placette
  data_plot_pas <- as.data.frame(unclass(expand_grid(pas = 1:nb_pas, id_pe = data_plot)))

  tous <- list()
  for (ess in liste_ess) {

    # fichiers des parametres : n_st_v_param_fixe.rda, n_st_v_param_cov.rda, n_st_v_param_random.rda"
    param <- n_st_v_param_fixe[[ess]]
    param$essence <- NULL

    if (mode_simul=='STO') {
      # lecture de la matrice de covariance des effets fixes
      param_covb <-  n_st_v_param_cov[[ess]]
      param_covb$essence <- NULL

      # générer un vecteur d'effet fixes pour une placette
      param_ess = as.data.frame(matrix(mvrnorm(n = nb_iter, mu = as.matrix(param), Sigma = as.matrix(param_covb)),
                                       nrow=nb_iter))
      names(param_ess) <- names(param)
      param_ess <- param_ess %>% mutate(iter = row_number())

      # lecture de la matrice de covariance des erreurs résiduelles du systeme d'équations
      param_covr =  n_st_v_param_random[n_st_v_param_random$essence==ess,]
      # enlever les NA, ça correspond à des variables du modèle d'une autre essence
      param_covr[colnames(param_covr)[colSums(is.na(param_covr)) > 0]] <- list(NULL)
      param_covr$essence <- NULL

      # générer un vecteur d'erreurs résiduelles à l'échelle de la placette, mais qui doit changer à chaque pas de temps, non corrélées
      res_ess = as.data.frame(matrix(mvrnorm(n = nb_iter*length(liste_place)*nb_pas, mu = c(0,0,0), Sigma = as.matrix(param_covr)),
                                     nrow=nb_iter*length(liste_place)*nb_pas))
      res_ess <- bind_cols(data_plot_pas,res_ess)
      names(res_ess) <- c('pas','iter','id_pe','res_n','res_st','res_v')
    }
    else{
      param_ess  <- param %>% mutate(iter=1)
      res_ess <- 0
    }
    temp <- list('essence'=ess, 'effet_fixe'=param_ess, 'erreur_residuelle'=res_ess)
    tous <- append(tous, list(temp))
  }
  names(tous) <- toupper(liste_ess)
  return(tous)
}

