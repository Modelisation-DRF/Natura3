#' Vérifier le contenu des variables des fichier d'entrée
#'
#' @description Vérifier le contenu des variables des fichiers d'entrée
#
#' @param type_fic Type du fichier à vérifier: arbres, etudes
#' @param fichier Nom de la table à vérifier
#'
#' @return Table ou message d'erreur
#' @export
#'
# @examples
valid_arbre <- function(type_fic, fichier){

  names(fichier) <- tolower(names(fichier))
  # type_fic = 'arbres'

  # essence = c('SAB','EPX','EPB','BOP')
  # dhpcm = c(8,10, 400, 50)
  # etat = c('10','10','10','11')
  # longitude = c(-70, -70, -70, -82)
  # fichier <- data.frame(essence, dhpcm, etat, longitude)
  # ht=T; vol=T; iqs=T; climat=T; sol=T;

  # test: test <- fichier_arbres_aveccov %>% mutate(type_eco = ifelse(type_eco=='RE20','FE32',type_eco)); names(test) <- tolower(names(test))
  # type_fic="arbres"; fichier=test; ht=T; vol=T; iqs=F; climat=F; sol=F;

  if (type_fic=='arbres') {

    valid <- fic_validation %>% filter(fichier %in% c("arbres", "arbres, etudes"))

  }
  if (type_fic=='etudes') {
    valid <- fic_validation %>% filter(fichier %in% c("etudes", "arbres, etudes"))
  }

  erreur <- NULL  # on accumule tous les messages
  for (i in 1:nrow(valid)) {
    #i=1
    val <-   as.character(valid[i,2])  # les valeurs possibles
    message <- as.character(valid[i,3]) # le message d'erreur si mauvaises valeurs
    fichier_val <- fichier %>% filter(!eval(parse(text = val)))  # on garde les lignes qui ne sont pas dans les valeurs possibles d'une variable
    if (nrow(fichier_val)>0) {erreur <- c(erreur, message)} # s'il y a des lignes avec des erreurs, on ajoute le message d'erreur
  }

  # si erreur n'est pas vide on retourne l'erreur, sinon on retourne le fichier
  if (!is.null(erreur)) {result <- erreur} else result <- fichier


  return(result)


}


