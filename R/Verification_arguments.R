################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#  Function checking the values of the main function arguments #
#                                                              #
#                                                              #
################################################################

#' Vérification des arguments de la fonction principale du simulateur Natura
#'
#' @description Vérification des arguments de la fonction principale \code{SimulNatura()} du simulateur Natura
#'
#' @inheritParams SimulNatura
#'
#' @return Une chaine de caractères contant "ok" s'il n'y a pas d'erreur, sinon, contient un message d'erreur.
# #' @export
#'
# @examples
CheckArguments <- function(file_arbre, file_etude, file_compile, horizon, mode_simul, nb_iter, iqs, climat, sol, ht, vol, dec_perturb, dec_tbe1, tbe1, dec_tbe2, tbe2) {

  # on doit spécifier au moins un des 3 fichiers
  if (missing(file_arbre) & missing(file_etude) & missing(file_compile)) {
    erreur <- "Au moins un des deux: file_arbre+file_etude OU file_compile doit être specifié"
  }
  # il faut ne faut pas spécifier les types de fichier en même temps
  #else if (!missing(file_arbre) & !missing(file_etude) & !missing(file_compile)) {
  #  erreur <- "Seulement un des deux: file_arbre+file_etude OU file_compile doit être specifé"
  #}
  # Si on spécifie le fichier compilé, les 2 autres fichiers doivent être vides
  else if ((!missing(file_arbre)  | !missing(file_etude)) & !missing(file_compile)){
    erreur <- "Seulement un des deux: file_arbre+file_etude OU file_compile doit être specifé"
  }
  # Si on ne spécifie pas de fichier compilé, il faut spécifier les 2 autres
  else if ((missing(file_arbre)  | missing(file_etude)) & missing(file_compile)){
    erreur <- "Si file_compile n'est pas specifié, file_arbre ET file_etude doivent être specifiés"
  }
  # l'horizon doit être entre 1 et 15
  else if (horizon>15 | horizon<1) {
    erreur <- c("horizon doit être de 1 a 15")
  }
  # la décennie de la perturbation ne doit pas dépasser l'horizon
  else if (dec_perturb > horizon) {
    erreur <- c("dec_perturb doit être <= horizon")
  }
  # la décennie de la 1e tbe ne doit pas dépasser l'horizon
  else if (dec_tbe1 > horizon) {
    erreur <- c("dec_tbe1 doit être <= horizon")
  }
  # si on spécifie une décennie de tbe, on doit aussi spécifier l'indice de tbe
  else if (dec_tbe1>0 & tbe1==0) {
    erreur <- c("Si dec_tbe1 est specifié, tbe1 doit être > 0")
  }
  # si on spécifie un indice de tbe, on doit aussi spécifier la décennie de tbe
  else if (dec_tbe1==0 & tbe1>0) {
    erreur <- c("Si tbe1 est specifié, dec_tbe1 doit être specifié aussi")
  }
  # l'indice de tbe doit être un entier 1-2-3-4-5
  else if (dec_tbe1>0 & !tbe1 %in% c(1,2,3,4,5)) {
    erreur <- c("Si dec_tbe1 est specifié, tbe1 doit etre 1, 2, 3, 4 ou 5")
  }
  # pour spécifier une 2e tbe, il faut en avoir spécifié une premiere
  else if (dec_tbe2>0 & (dec_tbe1==0 | tbe1==0)) {
    erreur <- c("dec_tbe1 et tbe1 doivent être specifiés pour pouvoir utiliser dec_tbe2")
  }
  # la décennie de la 2e tbe ne doit pas dépasser l'horizon
  else if (dec_tbe2 > horizon) {
    erreur <- c("dec_tbe2 doit être <= horizon")
  }
  # la 2e tbe doit être après la 1e tbe
  else if (dec_tbe2>0 & dec_tbe2 <= dec_tbe1) {
    erreur <- c("dec_tbe2 doit être > dec_tbe1")
  }
  # si on spécifie une décennie de tbe, on doit aussi spécifier l'indice de tbe
  else if (dec_tbe2>0 & tbe2==0) {
    erreur <- c("Si dec_tbe2 est specifié, tbe2 doit etre > 0")
  }
  # si on spécifie un indice de tbe, on doit aussi spécifier la décennie de tbe
  else if (dec_tbe2==0 & tbe2>0) {
    erreur <- c("Si tbe2 est specifié, dec_tbe2 doit être specifié aussi")
  }
  # l'indice de tbe doit être un entier 1-2-3-4-5
  else if (dec_tbe2>0 & !tbe2 %in% c(1,2,3,4,5)) {
    erreur <- c("Si dec_tbe2 est specifié, tbe2 doit être 1, 2, 3, 4 ou 5")
  }
  # l'argument du calcul de la hauteur doit être binaire
  else if (!missing(file_arbre) & !ht %in% c(TRUE, FALSE)) {
    erreur <- c("ht doit être TRUE ou FALSE")
  }
  # l'argument du calcul du volume doit être binaire
  else if (!missing(file_arbre) & !vol %in% c(TRUE, FALSE)) {
    erreur <- c("vol doit être TRUE ou FALSE")
  }
  # l'argument des iqs doit être binaire
  else if (!iqs %in% c(TRUE, FALSE)) {
    erreur <- c("iqs doit être TRUE ou FALSE")
  }
  # l'argument du climat doit être binaire
  else if (!climat %in% c(TRUE, FALSE)) {
    erreur <- c("climat doit être TRUE ou FALSE")
  }
  # l'argument du sol doit être binaire
  else if (!sol %in% c(TRUE, FALSE)) {
    erreur <- c("sol doit être TRUE ou FALSE")
  }
  # l'argument mode_simul doit être binaire
  else if (!mode_simul %in% c('DET', 'STO')) {
    erreur <- c("mode_simul doit être DET ou STO")
  }
  # l'argument nb_iter doit être >30 si mode_simul=STO
  else if (mode_simul=='STO' & nb_iter<30) {
    erreur <- c("nb_iter doit être >=30 en mode stochastique")
  }
  else erreur <- c("ok")
  return(erreur)
}
