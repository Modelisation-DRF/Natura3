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


#' Génère les paramètres du modèle pour estimer la hauteur des arbres par essence à la step 0 pour le calcul de la hauteur dominante.
#'
#' @description Génère les paramètres du modèle pour estimer la hauteur des arbres par essence à la step 0 pour le calcul de la hauteur dominante, déterministe ou stochastique, pour chacun des arbres de chaque placette pour toutes les itérations.
#'
#' @details
#' Le modèle est un modèle non-linéaire mixte calibré par essence, avec un effet aléatoire d'arbre. Il n'y a pas de matrice de corrélation sur les résidus.
#' Il n'y a pas de matrice de covariances des effets fixes car il n'y a qu'un seul paramètre.
#'
#' @param liste_arbre Table contenant l'identifiant de l'arbre (no_arbre) et l'identifiant de la placette (id_pe) pour lesquels la hauteur doit être estimée.
#' @inheritParams SimulNatura
#'
#' @return  Une liste de listes. Une liste par essence pour lesquelles il y a un modèle de hauteur, et pour chaque essence, une liste avec ces 3 éléments:
#' \enumerate{
#'   \item essence: une chaine de caractère contenant le code de l'essence
#'   \item effet_fixe: table contenant les paramètres des effets fixes, une ligne par itération
#'   \item erreur_arbre: table contenant les valeurs des erreurs résiduelles et les valeurs des effets aléatoires d'arbres, une ligne par arbre par itération
#' }
#' @export
#'
# @examples
param_hdom0_ess_stoch <- function(liste_arbre, mode_simul='DET', nb_iter=1, seed_value = NULL) {

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  if (length(seed_value)>0) {set.seed(seed_value)}

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

      #param_hd = data.frame('b2'=rnorm(n = nb_iter, mean=as.matrix(param_i[1,2]), sd = as.matrix(param_i[1,3]))) %>% # la fct rnorm ne donne pas moyenne et var axact à ceux specifies meme avec 1000000 iteration. Voir explication https://stackoverflow.com/questions/18919091/generate-random-numbers-with-fixed-mean-and-sd. Il est préférable dans mon cas d'utiliser mvrnorm avec empiracal =T, puisque  means et sd sont empiriques
      param_hd = data.frame('b2'=mvrnorm(n = nb_iter, mu=as.matrix(param_i[1,2]), Sigma = as.matrix(param_i[1,3])^2, empirical = T)) %>% # sigma est une matrice de covariance, donc un sigma2
        mutate(iter = row_number(), ess_eq_hd=ess)

      # générer erreurs échelle arbre de l'essence
      #rand_arbre_hd = data.frame('random_arbre'=rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(param_i[3,2]))))
      #resi_arbre_hd = data.frame('resid_arbre'=rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(param_i[2,2]))))
      rand_arbre_hd = data.frame('random_arbre'=mvrnorm(n=nb_iter*length(liste_arbre$no_arbre), mu=0, Sigma = as.matrix(param_i[3,2]), empirical = T))
      resi_arbre_hd = data.frame('resid_arbre'=mvrnorm(n=nb_iter*length(liste_arbre$no_arbre), mu=0, Sigma = as.matrix(param_i[2,2]), empirical = T))

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

#' Génère les paramètres du modèle global pour estimer la hauteur des arbres à la step 0 pour le calcul de la hauteur dominante.
#'
#' @description Génère les paramètres du modèle global pour estimer la hauteur des arbres à la step 0 pour le calcul de la hauteur dominante, déterministe ou stochastique, pour chacun des arbres de chaque placette pour toutes les itérations.
#'
#' @details
#' Le modèle est un modèle non-linéaire mixte calibré globalement pour toutes les essences, avec un effet aléatoire d'arbre. Il n'y a pas de matrice de corrélation sur les résidus.
#' Il n'y a pas de matrice de covariances des effets fixes car il n'y a qu'un seul paramètre.
#' Ce modèle est utilisé lorsqu'une essence est présente dans la liste d'arbres mias pas dans les arbres-études.
#'
#' @param liste_arbre Table contenant l'identifiant de l'arbre (no_arbre) et l'identifiant de la placette (id_pe) pour lesquels la hauteur doit être estimée.
#' @inheritParams SimulNatura
#'
#' @return Une liste de 2 éléments:
#' \enumerate{
#'   \item effet_fixe: table contenant les paramètres des effets fixes, une ligne par itération
#'   \item erreur_arbre: table contenant les valeurs des erreurs résiduelles et les valeurs des effets aléatoires d'arbres, une ligne par arbre par itération
#' }
#'
#' @export
#'
# @examples
param_hdom0_stoch <- function(liste_arbre, mode_simul='DET', nb_iter=1, seed_value = NULL ) {

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  if (length(seed_value)>0) {set.seed(seed_value)}

  # fichier des paramètres (hdom_param_global_fixe.rda)
  param_global <- hdom_param_global_fixe[, 1:3]

  # liste des arbres x nb_iter pour accueillir les erreurs résiduelle et les random arbre
  data_arbre <- as.data.frame(unclass(expand_grid(iter = 1:nb_iter, id_pe = liste_arbre)))

  if (mode_simul=='STO') {

    # générer effets fixes du modele global
    #param_hd = data.frame('b2'=rnorm(n = nb_iter, mean=as.matrix(param_global[1,2]), sd = as.matrix(param_global[1,3]))) %>%
    param_hd = data.frame('b2'=mvrnorm(n = nb_iter, mu=as.matrix(param_global[1,2]), Sigma = as.matrix(param_global[1,3])^2, empirical = T)) %>%
      mutate(iter = row_number())

    # générer erreurs échelle arbre du modèle global
    #rand_arbre_hd = data.frame('random_arbre'=rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(param_global[3,2]))))
    #resi_arbre_hd = data.frame('resid_arbre'=rnorm(nb_iter*length(liste_arbre$no_arbre), mean=0, sd = sqrt(as.matrix(param_global[2,2]))))
    rand_arbre_hd = data.frame('random_arbre'=mvrnorm(nb_iter*length(liste_arbre$no_arbre), mu=0, Sigma = as.matrix(param_global[3,2]), empirical = T))
    resi_arbre_hd = data.frame('resid_arbre'=mvrnorm(nb_iter*length(liste_arbre$no_arbre), mu=0, Sigma = as.matrix(param_global[2,2]), empirical = T))

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


#' Génère les paramètres du modèle d'évolution de l'indice de Shannon
#'
#' @description Génère les paramètres du modèle d'évolution de l'indice de Shannon, déterministe ou stochastique, pour chacune des placettes et tous leurs pas de simulation, pour toutes les itérations.
#'
#' @details
#' Le modèle est un modèle linéaire mixte calibré séparément par groupe d'essences dominantes, avec un effet aléatoire de placette.
#' Il n'y a pas de matrice de corrélation sur les résidus. Il y a une matrice de covariance des effets fixes.
#'
#' @param liste_place Vecteur contenant les identifiants des placettes.
#' @inheritParams SimulNatura
#'
#' @return Une liste de listes. Une liste par groupe d'essences, et pour chaque groupe d'essences, une liste de 4 éléments:
#' \enumerate{
#'   \item essence: chaine de caractère contenant le code du groupe d'essences
#'   \item effet_fixe: table contenant les paramètres des effets fixes, une ligne par pas de simulation et itération
#'   \item random_placette: table contenant les valeurs des effets aléatoires, une ligne par placette, pas de simulation et itération
#'   \item erreur_residuelle: table contenant les valeurs des erreurs résiduelles, une ligne par placette, pas de simulation et itération
#' }
#'
#' @export
#'
# @examples
param_is_evol_stoch <- function(liste_place, mode_simul='DET', nb_iter=1, horizon, seed_value = NULL){

  #liste_place=fic; nb_iter=200; mode_simul='STO'; horizon=2; seed_value = 20;
  # liste_place=liste_plot; mode_simul='STO'; horizon=5; nb_iter=1; seed_value = NULL

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  if (length(seed_value)>0) {set.seed(seed_value)}

  nb_pas <- horizon

  # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
  data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)
  # liste des placettes x nb_iter x nb_pas pour accueillir les erreurs résiduelles de placette
  data_plot_pas <- as.data.frame(unclass(expand_grid(pas = 1:nb_pas, id_pe = data_plot)))
  names(data_plot_pas) <- c('pas','iter','id_pe')

  # lecture des paramètres (is_param_fixe.rda, is_param_cov.rda)
  param <-  is_param_fixe
  param_cov <-  is_param_cov

  # générer des paramètres des effets fixes pour une essence dominante donnée
  liste_ess <- unique(param$ess_dom)
  tous <- list()
  for (ess in liste_ess)
    {
    #ess="bop"
    if (mode_simul == 'STO') {
      # pour que mvrnorm() fonctionne avec empirical=T, il faut autant de n que la longueur du vecteur mu à simuler
      # ici, mu a une longueur fixe de 5
      mu = as.matrix(param[param$ess_dom==tolower(ess),c(1:4,7)])
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
        else{nb_iter_temp=nb_iter}
      param_is = as.data.frame(matrix(mvrnorm(n = nb_iter_temp, mu = mu,
                                              Sigma = as.matrix(param_cov[param_cov$ess_dom==toupper(ess),2:6]),
                                              empirical = T),
                                      nrow=nb_iter_temp))[1:nb_iter,]
      names(param_is) <- names(param[param$ess_dom==tolower(ess),c(1:4,7)])
      param_is <- param_is %>% mutate(iter = row_number())

      # générer un effet aléatoire de placette, qui sera le même pour la placette pour toutes ses itérations
      #rand_plot <- data.frame('random_plot'=rnorm(n = nb_iter*length(liste_place), mean = 0, sd = sqrt(as.matrix(param[param$ess_dom==tolower(ess),6]))))
      rand_plot <- data.frame('random_plot'=mvrnorm(n = nb_iter*length(liste_place), mu = 0, Sigma = as.matrix(param[param$ess_dom==tolower(ess),6]), empirical = T))
      rand_plot <- bind_cols(data_plot,rand_plot)

      # générer une erreur résiduelle à l'échelle de la placette, une pour chaque chaque pas de simulation
      #res_plot = data.frame('res_plot'=rnorm(n = nb_iter*length(liste_place)*nb_pas, mean = 0, sd = sqrt(as.matrix(param[param$ess_dom==tolower(ess),5]))))
      res_plot = data.frame('res_plot'=mvrnorm(n = nb_iter*length(liste_place)*nb_pas, mu = 0, Sigma = as.matrix(param[param$ess_dom==tolower(ess),5]), empirical = T))
      res_plot <- bind_cols(data_plot_pas,res_plot)
      names(res_plot) <- c(names(data_plot_pas),'res_plot')
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


#' Génère les paramètres du modèle d'évolution de la hauteur dominante
#'
#' @description Génère les paramètres du modèle d'évolution de la hauteur dominante, déterministe ou stochastique, pour chacune des placettes et tous leurs pas de simulation, pour toutes les itérations.
#'
#' @details
#' Le modèle est un modèle non-linéaire mixte calibré séparément par groupe d'essences dominantes, avec un effet aléatoire de placette.
#' Il n'y a pas de matrice de corrélation sur les résidus. Il y a une matrice de covariance des effets fixes.
#'
#' @param liste_place Vecteur contenant les identifiants des placettes.
#' @inheritParams SimulNatura
#'
#' @return Une liste de listes. Une liste par groupe d'essences, et pour chaque groupe d'essences, une liste de 4 éléments:
#' \enumerate{
#'   \item essence: chaine de caractère contenant le code du groupe d'essences
#'   \item effet_fixe: table contenant les paramètres des effets fixes, une ligne par pas de simulation et itération
#'   \item random_placette: table contenant les valeurs des effets aléatoires, une ligne par placette, pas de simulation et itération
#'   \item erreur_residuelle: table contenant les valeurs des erreurs résiduelles, une ligne par placette, pas de simulation et itération
#' }
#' @export
#'
# @examples
param_hd_evol_stoch <- function(liste_place, mode_simul='DET', nb_iter=1, horizon, seed_value = NULL){

  # liste_place=liste_plot; nb_iter=2; mode_simul='STO'; horizon=5; seed_value = 20;

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  if (length(seed_value)>0) {set.seed(seed_value)}

  nb_pas <- horizon

   # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
  data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)

  # liste des placettes x nb_iter x nb_pas pour accueillir les erreurs résiduelles de placette
  data_plot_pas <- as.data.frame(unclass(expand_grid(pas = 1:nb_pas, id_pe = data_plot)))
  names(data_plot_pas) <- c('pas','iter','id_pe')

  # lecture des paramètres (hdevol_param_fixe.rda, hdevol_param_cov.rda)
  param <-  hdevol_param_fixe
  param_cov <-  hdevol_param_cov

  # générer des paramètres des effets fixes pour une essence dominante donnée
  liste_ess <- unique(param$ess_dom)
  tous <- list()
  for (ess in liste_ess)
  {
    if (mode_simul == 'STO') {
      mu = as.matrix(param[param$ess_dom==tolower(ess),c(1:4,7)])
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
      else{nb_iter_temp=nb_iter}
      # générer des paramètres des effets fixes pour une essence dominante donnée
      param_hd = as.data.frame(matrix(mvrnorm(n = nb_iter_temp, mu = mu, Sigma = as.matrix(param_cov[param_cov$ess_dom==toupper(ess),2:6]),empirical = T),
                                      nrow=nb_iter_temp))[1:nb_iter,]
      names(param_hd) <- names(param[param$ess_dom==tolower(ess),c(1:4,7)])
      param_hd <- param_hd %>% mutate(iter = row_number())

      # générer un effet aléatoire de placette, qui sera le même pour la placette pour toutes ses itérations
      #rand_plot <- data.frame('random_plot'=rnorm(n = nb_iter*length(liste_place), mean = 0, sd = sqrt(as.matrix(param[param$ess_dom==tolower(ess),6]))))
      rand_plot <- data.frame('random_plot'=mvrnorm(n = nb_iter*length(liste_place), mu = 0, Sigma = as.matrix(param[param$ess_dom==tolower(ess),6]), empirical = T))
      rand_plot <- bind_cols(data_plot,rand_plot)

      # générer une erreur résiduelle à l'échelle de la placette, une pour chaque chaque pas de simulation
      #res_plot = data.frame('res_plot'=rnorm(n = nb_iter*length(liste_place)*nb_pas, mean = 0, sd = sqrt(as.matrix(param[param$ess_dom==tolower(ess),5]))))
      res_plot = data.frame('res_plot'=mvrnorm(n = nb_iter*length(liste_place)*nb_pas, mu = 0, Sigma = as.matrix(param[param$ess_dom==tolower(ess),5]), empirical = T))
      res_plot <- bind_cols(data_plot_pas,res_plot)
      names(res_plot) <- c(names(data_plot_pas),'res_plot')
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



#' Génère les paramètres des modèles d'évolution de la hauteur dominante et de l'indice de Shannon
#'
#' @description Génère les paramètres des modèles d'évolution de la hauteur dominante et ceux de l'indice de Shannon, déterministe ou stochastique, pour chacune des placettes et tous leurs pas de simulation, pour toutes les itérations.
#'
#' @details
#' Le modèle de hauteur dominante est un modèle non-linéaire mixte calibré séparément par groupe d'essences dominantes, avec un effet aléatoire de placette.
#' Il n'y a pas de matrice de corrélation sur les résidus. Il y a une matrice de covariance des effets fixes.
#' Le modèle de l'indice de Shannon est un modèle linéaire mixte calibré séparément par groupe d'essences dominantes, avec un effet aléatoire de placette.
#' Il n'y a pas de matrice de corrélation sur les résidus. Il y a une matrice de covariance des effets fixes.
#'
#' @param liste_place Vecteur contenant les identifiants des placettes.
#' @inheritParams SimulNatura
#'
#' @return Une liste de listes. Une liste par groupe d'essences, et pour chaque groupe d'essences, une liste de 4 éléments:
#' \enumerate{
#'   \item essence: chaine de caractère contenant le code du groupe d'essences
#'   \item effet_fixe: table contenant les paramètres des effets fixes des deux modèles, une ligne par pas de simulation et itération
#'   \item random_placette: table contenant les valeurs des effets aléatoires des deux modèles, une ligne par placette, pas de simulation et itération
#'   \item erreur_residuelle: table contenant les valeurs des erreurs résiduelles des deux modèles, une ligne par placette, pas de simulation et itération
#' }
#' @export
#'
# @examples
param_ishd_evol_stoch <- function(liste_place, mode_simul='DET', nb_iter=1, horizon, seed_value = NULL){

  #liste_place=fic; nb_iter=200; mode_simul='STO'; horizon=2; seed_value = 20;

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  if (length(seed_value)>0) {set.seed(seed_value)}

  nb_pas <- horizon

  # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
  data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)
  # liste des placettes x nb_iter x nb_pas pour accueillir les erreurs résiduelles de placette
  data_plot_pas <- as.data.frame(unclass(expand_grid(pas = 1:nb_pas, id_pe = data_plot)))
  names(data_plot_pas) <- c('pas','iter','id_pe')

  # utilise is_param_fixe.rda, is_param_cov.rda, hdevol_param_fixe.rda, hdevol_param_cov.rda

  # générer des paramètres des effets fixes pour une essence dominante donnée
  liste_ess <- unique(is_param_fixe$ess_dom) # ordre alphabetique
  tous <- list()
  for (ess in liste_ess)
  {
    if (mode_simul == 'STO') {

      # effets fixes, une ligne par ess
      mu = as.matrix(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),c(1:4,7)])
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
      else{nb_iter_temp=nb_iter}

      param_is = as.data.frame(matrix(mvrnorm(n = nb_iter_temp, mu = mu,
                                              Sigma = as.matrix(is_param_cov[is_param_cov$ess_dom==toupper(ess),2:6]),
                                              empirical = T),
                                      nrow=nb_iter_temp))[1:nb_iter,]
      names(param_is) <- names(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),c(1:4,7)])


      mu = as.matrix(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),c(1:4,7)])
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
      else{nb_iter_temp=nb_iter}
      param_hd = as.data.frame(matrix(mvrnorm(n = nb_iter_temp, mu = mu,
                                              Sigma = as.matrix(hdevol_param_cov[hdevol_param_cov$ess_dom==toupper(ess),2:6]),
                                              empirical = T),
                                      nrow=nb_iter_temp))[1:nb_iter,]

      names(param_hd) <- names(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),c(1:4,7)])
      #param <- inner_join(param_is,param_hd,by="ess_dom") %>% mutate(iter = row_number())
      param <- bind_cols(param_is,param_hd) %>% mutate(iter = row_number())


      # is: générer un effet aléatoire de placette, qui sera le même pour la placette pour toutes ses itérations
      #rand_plot_is <- data.frame('random_plot'=rnorm(n = nb_iter*length(liste_place), mean = 0, sd = sqrt(as.matrix(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),6]))))
      rand_plot_is <- data.frame('random_plot'=mvrnorm(n = nb_iter*length(liste_place), mu = 0, Sigma = as.matrix(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),6]), empirical = T))
      names(rand_plot_is) <- 'rand_plot_is'
      # hd: générer un effet aléatoire de placette, qui sera le même pour la placette pour toutes ses itérations
      #rand_plot_hd <- data.frame('random_plot'=rnorm(n = nb_iter*length(liste_place), mean = 0, sd = sqrt(as.matrix(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),6]))))
      rand_plot_hd <- data.frame('random_plot'=mvrnorm(n = nb_iter*length(liste_place), mu = 0, Sigma = as.matrix(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),6]), empirical = T))
      names(rand_plot_hd) <- 'rand_plot_hd'
      rand_plot <- bind_cols(data_plot, rand_plot_is, rand_plot_hd)


      # is: générer une erreur résiduelle à l'échelle de la placette, une pour chaque chaque pas de simulation
      #res_plot_is = data.frame('res_plot'=rnorm(n = nb_iter*length(liste_place)*nb_pas, mean = 0, sd = sqrt(as.matrix(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),5]))))
      res_plot_is = data.frame('res_plot'=mvrnorm(n = nb_iter*length(liste_place)*nb_pas, mu = 0, Sigma = as.matrix(is_param_fixe[is_param_fixe$ess_dom==tolower(ess),5]), empirical = T))
      names(res_plot_is) <- 'res_plot_is'
      # hd: générer une erreur résiduelle à l'échelle de la placette, une pour chaque chaque pas de simulation
      #res_plot_hd = data.frame('res_plot'=rnorm(n = nb_iter*length(liste_place)*nb_pas, mean = 0, sd = sqrt(as.matrix(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),5]))))
      res_plot_hd = data.frame('res_plot'=mvrnorm(n = nb_iter*length(liste_place)*nb_pas, mu = 0, Sigma = as.matrix(hdevol_param_fixe[hdevol_param_fixe$ess_dom==tolower(ess),5]), empirical = T))
      names(res_plot_hd) <- 'res_plot_hd'
      res_plot <- bind_cols(data_plot_pas,res_plot_is,res_plot_hd)
      names(res_plot) <- c(names(data_plot_pas),'res_plot_is','res_plot_hd')
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


#' Génère les paramètres des modèles d'évolution de Nxxx, STxxx et Vxxx
#'
#' @description Génère les paramètres des modèles d'évolution de Nxxx, STxxx et Vxxx (où xxx est chacun des 8 groupes d'essences, donc 24 modèles), déterministe ou stochastique, pour chacune des placettes pour tous les pas de simulation et toutes les itérations
#'
#' @details
#' Les modèles de Nxxx, STxxx et Vxxx forment un système d'équations linéaires, calibrés séparément par groupe d'essences. Il n'y a pas d'effets aléatoires.
#' Les erreurs résiduelles sont corrélées entre les 3 équations. Il y a une matrice de covariances des effets fixes.
#'
#' @param liste_place Vecteur contenant les identifiants des placettes
#' @param liste_ess Vecteur contenant les codes des groupes d'essences de Natura
#' @inheritParams SimulNatura
#'
#' @return Une liste de listes. Une liste par groupe d'essences, et pour chaque groupe d'essences, une liste de 3 éléments:
#' \enumerate{
#'   \item essence: chaine de caractères contenant le code du groupe d'essences
#'   \item effet_fixe: table contenant les paramètres des effets fixes, une ligne par pas de simulation et itération
#'   \item erreur_residuelle: table contenant les valeurs des erreurs résiduelles, une ligne par placette, pas de simulation et itération
#' }
#' @export
#'
# @examples
param_evol_n_st_v_stoch <- function(liste_place, mode_simul='DET', nb_iter=1, horizon, liste_ess, seed_value = NULL){

  # liste_place=liste_plot; nb_iter=2; mode_simul='STO'; horizon=5; liste_ess=liste_gress; seed_value = 20;

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'itérations doit être plus grand que 1 en mode stochastique")}
  }

  if (length(seed_value)>0) {set.seed(seed_value)}

  nb_pas <- horizon

  # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
  data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)
  # liste des placettes x nb_iter x nb_pas pour accueillir les erreurs résiduelles de placette
  data_plot_pas <- as.data.frame(unclass(expand_grid(pas = 1:nb_pas, id_pe = data_plot)))
  names(data_plot_pas) <- c('pas','iter','id_pe')

  tous <- list()
  for (ess in liste_ess) {

    # ess = 'epn'

    # fichiers des parametres : n_st_v_param_fixe.rda, n_st_v_param_cov.rda, n_st_v_param_random.rda"
    param <- n_st_v_param_fixe[[ess]]
    param$essence <- NULL

    if (mode_simul=='STO') {
      # lecture de la matrice de covariance des effets fixes
      param_covb <-  n_st_v_param_cov[[ess]]
      param_covb$essence <- NULL

      # générer un vecteur d'effet fixes pour une placette
      # nb_iter=2
      mu = as.matrix(param)
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu}
      else{nb_iter_temp=nb_iter}

      param_ess = as.data.frame(matrix(mvrnorm(n = nb_iter_temp, mu = as.matrix(param), Sigma = as.matrix(param_covb), empirical = T),
                                       nrow=nb_iter_temp))[1:nb_iter,]
      names(param_ess) <- names(param)
      param_ess <- param_ess %>% mutate(iter = row_number())

      # lecture de la matrice de covariance des erreurs résiduelles du systeme d'équations
      param_covr =  n_st_v_param_random[n_st_v_param_random$essence==ess,]
      # enlever les NA, ça correspond à des variables du modèle d'une autre essence
      param_covr[colnames(param_covr)[colSums(is.na(param_covr)) > 0]] <- list(NULL)
      param_covr$essence <- NULL

      # générer un vecteur d'erreurs résiduelles à l'échelle de la placette, mais qui doit changer à chaque pas de temps, non corrélées
      res_ess = as.data.frame(matrix(mvrnorm(n = nb_iter*length(liste_place)*nb_pas, mu = c(0,0,0), Sigma = as.matrix(param_covr), empirical = T),
                                     nrow=nb_iter*length(liste_place)*nb_pas))
      res_ess <- bind_cols(data_plot_pas,res_ess) ###### C'EST ICI QUE ÇA BUG
      names(res_ess) <- c(names(data_plot_pas),'res_n','res_st','res_v')
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

