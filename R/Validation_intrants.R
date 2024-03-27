#' Vérifier le contenu des variables des fichier d'entrée
#'
#' @description Vérifier le contenu des variables des fichier d'entrée
#
#' @param type_fic Type du fichier à vérifier: arbres, etudes ou compil
#' @param fichier Nom de la table à vérifier
#' @inheritParams SimulNatura
#'
#' @return Table ou message d'erreur
# #' @export
#'
# @examples
valid_fic <- function(type_fic, fichier, ht=NULL, iqs=NULL, climat=NULL, sol=NULL){

  # type_fic = 'arbres'

  # essence = c('SAB','EPX','EPB','BOP')
  # dhpcm = c(8,10, 400, 50)
  # etat = c('10','10','10','11')
  # longitude = c(-70, -70, -70, -82)
  # fichier <- data.frame(essence, dhpcm, etat, longitude)

# type_fic="arbres"; fichier=fichier_arbres_aveccov; ht=T; vol=T; iqs=F; climat=F; sol=F;

  if (type_fic=='arbres') {

    valid1 <- NULL; valid2 <- NULL; valid3 <- NULL; valid4 <- NULL; valid5 <- NULL; valid6 <- NULL; valid7 <- NULL; valid8 <- NULL;

    valid1 <- fic_validation %>% filter(fichier %in% c("arbres", "arbres, etudes", "peup"))

    # si on fournit le climat et pas besoin de calculer ht, il faut juste verfier prec_gs et temp_gs
    if (isFALSE(climat) & isFALSE(ht)) valid2 <- fic_validation %>% filter(fichier == 'climat')
    # si on fournit le climat et qu'il faut calculer ht, il faut prec_gs et temp_gs et t_mat et ptot et altitude
    if (isFALSE(climat) & isTRUE(ht)) valid3 <- fic_validation %>% filter(fichier %in% c('climat', 'climat_ht', 'ht'))
    # si on ne fournit pas le climat et qu'il faut calculer ht, il altitude
    if (isTRUE(climat) & isTRUE(ht)) valid4 <- fic_validation %>% filter(fichier == 'ht')

    # s'il faut aller dans les cartes pour au moins une variables, il faut les coord
    if (isTRUE(climat) | isTRUE(sol) | isTRUE(iqs)) valid5 <- fic_validation %>% filter(fichier == 'coord')

    # s'il faut aller dans les cartes pour le climat il faut l'an_mes
    if (isTRUE(climat)) valid6 <- fic_validation %>% filter(fichier == 'climat_an')

    # si on fournit iqs
    if (isFALSE(iqs)) valid7 <- fic_validation %>% filter(fichier == 'iqs')
    # si on fournit sol
    if (isFALSE(sol)) valid8 <- fic_validation %>% filter(fichier == 'sol')


    valid <- bind_rows(valid1, valid2, valid3, valid4, valid5, valid6, valid7, valid8)

  }
  if (type_fic=='etudes') {
    valid <- fic_validation %>% filter(fichier %in% c("etudes", "arbres, etudes"))
  }

  if (type_fic=='compile') {

    valid1 <- NULL; valid2 <- NULL; valid3 <- NULL; valid4 <- NULL; valid5 <- NULL; valid6 <- NULL;

    valid1 <- fic_validation %>% filter(fichier %in% c("compil", "peup"))

    # si on fournit climat
    if (isFALSE(climat)) valid2 <- fic_validation %>% filter(fichier %in% c('climat'))

    # si on fournit iqs
    if (isFALSE(iqs)) valid3 <- fic_validation %>% filter(fichier == 'iqs')

    # si on fournit sol
    if (isFALSE(sol)) valid4 <- fic_validation %>% filter(fichier == 'sol')

    # s'il faut aller dans les cartes pour le climat il faut l'an_mes
    if (isTRUE(climat)) valid5 <- fic_validation %>% filter(fichier == 'climat_an')

    # s'il faut aller dans les cartes pour au moins une variables, il faut les coord
    if (isTRUE(climat) | isTRUE(sol) | isTRUE(iqs)) valid6 <- fic_validation %>% filter(fichier == 'coord')


    valid <- bind_rows(valid1, valid2, valid3, valid4, valid5, valid6)
  }

  if (type_fic=='valid') {
    valid <- fic_validation %>% filter(fichier=='valid')
  }

  erreur <- NULL  # on accumule tous les messages
  for (i in 1:nrow(valid)) {
    val <-   as.character(valid[i,2])  # les valeurs possibles
    message <- as.character(valid[i,3]) # le message d'erreur si mauvaises valeurs
    fichier_val <- fichier %>% filter(!eval(parse(text = val)))  # on garde les lignes qui ne sont pas dans les valeurs possible d'une variable
    if (nrow(fichier_val)>0) {erreur <- c(erreur, message)} # s'il y a des lignes, on ajoute le message d'erreur
  }


  # si erreur n'est pas vide on retourne l'erreur, sinon on retourne le fichier
  if (length(erreur)>0) {result <- erreur} else result <- fichier

 return(result)

}


