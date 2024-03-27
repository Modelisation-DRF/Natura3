################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function importing the tree file                           #
#                                                              #
#   Use nom_variables                                          #
#                                                              #
################################################################


#' Lire le fichier des arbres à simuler avec Natura et valider le nom des colonnes
#'
#' @description Lire le fichier des arbres à simuler avec Natura et valider le nom des colonnes. Filtrer les arbres selon leur état et leur dhp.
#'
#' @param file Nom du fichier des arbres à lire (table, Excel ou csv)
#' @inheritParams SimulNatura
#'
#' @return Table dont les arbres ont été filtrés ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
# #' @export
#'
# @examples
Lecture_arbres <- function(file, ht, vol, iqs, climat, sol){

  # file=file_arbre; ht=ht; vol=vol; iqs=iqs; climat=climat; sol=sol;

  # vérifier si le fichier est un objet R, sinon importer le fichier
  if (!is.data.frame(file)) {
    suppressMessages(
    if (grepl(".xls", file)) {arbres <- readxl::read_excel(file)}
    else if (grepl(".csv", file)) {arbres <- read_delim(file, delim = ";")} # fread met ID_PE numérique, mais pas read_delim
    )
  }
  else {arbres <- file}
  names(arbres) <- tolower(names(arbres))

  nom <- names(arbres)

  nom_coor <- as.matrix(nom_variables[nom_variables$categorie=="coor","variable"])
  nom_mod_ht <- as.matrix(nom_variables[nom_variables$categorie=="modele_ht","variable"])
  nom_plot <- as.matrix(nom_variables[nom_variables$categorie=="plot","variable"])
  nom_iqs <- as.matrix(nom_variables[nom_variables$categorie=="iqs","variable"])
  nom_clim <- as.matrix(nom_variables[nom_variables$categorie=="clim","variable"])
  nom_sol <- as.matrix(nom_variables[nom_variables$categorie=="sol","variable"])
  nom_arbre <- as.matrix(nom_variables[nom_variables$categorie=="arbre","variable"])

  nom_an_mes <- as.matrix(nom_variables[nom_variables$categorie=="an_mes","variable"])
  nom_vol <- as.matrix(nom_variables[nom_variables$categorie=="vol","variable"])
  nom_ht <- as.matrix(nom_variables[nom_variables$categorie=="ht","variable"])

  nom_base <- c(nom_plot, nom_arbre)


  # setdiff : Find Elements that Exist Only in First, But Not in Second Vector

  # vérification des noms de variables de base
  if (length(setdiff(nom_base, nom)) >0)
    {arbres = paste0("Nom des variables de base incorrect dans le fichier des arbres")} else {

    # vérification des variables si ht est à estimer et que le climat est fourni
    if (isTRUE(ht) & isFALSE(climat)) {
      if (length(setdiff(nom_mod_ht, nom)) >0) {arbres = paste0("Nom des variables incorrect dans le fichier des arbres pour estimer la hauteur")}
    }
      # vérification des variables si ht est à estimer et que le climat n'est pas fourni
      if (isTRUE(ht) & isTRUE(climat)) {
        nom_mod_ht2 <- nom_mod_ht[-which(nom_mod_ht=="t_ma")] # enlever t_ma de la liste
        nom_mod_ht2 <- nom_mod_ht2[-which(nom_mod_ht2=="p_tot")] # enlever p_tot de la liste
        if (length(setdiff(c(nom_mod_ht2, nom_coor), nom)) >0) {arbres = paste0("Nom des variables incorrect dans le fichier des arbres pour estimer la hauteur")}
      }
      # vérification des noms de variables iqs ou sol à extraire : il faut lat-long
      if (isTRUE(iqs) | isTRUE(sol)) {
        if (length(setdiff(nom_coor, nom)) >0) {arbres = paste0("Coordonnées des placettes manquantes pour extraire iqs/sol")}
      }
      # vérification des noms de variables si climat sont à extraire : il faut lat-long-an_mes
      if (isTRUE(climat)) {
        if (length(setdiff(c(nom_coor, nom_an_mes), nom)) >0) {arbres = paste0("Coordonnées des placettes manquantes et année de mesure pour extraire climat")}
      }
      # vérification des iqs s'ils sont fournis dans le fichier d'inventaire
      if (isFALSE(iqs)) {
        if (length(setdiff(nom_iqs, nom)) >0) {arbres = paste0("Nom des variables d'iqs incorrect dans le fichier des arbres")}
      }
      # vérification des variables de sol s'ils sont fournis dans le fichier d'inventaire
      if (isFALSE(sol)) {
        if (length(setdiff(nom_sol, nom)) >0) {arbres = paste0("Nom des variables de sol incorrect dans le fichier des arbres")}
      }
      # vérification de la ht si elle est fournie dans le fichier d'inventaire
      if (isFALSE(ht)) {
        if (length(setdiff(nom_ht, nom)) >0) {arbres = paste0("Nom de la variable de hauteur incorrect dans le fichier des arbres")}
      }
      # vérification du volume s'il est fourni dans le fichier d'inventaire
      if (isFALSE(vol)) {
        if (length(setdiff(nom_vol, nom)) >0) {arbres = paste0("Nom de la variable du volume incorrect dans le fichier des arbres")}
      }
      # vérification des variables climatiques s'ils sont fournis dans le fichier d'inventaire mais pas besoin d'estimer la hauteur
      if (isFALSE(climat) & isFALSE(ht)) {
       if (length(setdiff(nom_clim, nom)) >0) {arbres = paste0("Nom des variables climatiques annuelles incorrect dans le fichier des arbres")}
      }
      # vérification des variables climatiques s'ils sont fournis dans le fichier d'inventaire mais aussi besoin d'estimer la hauteur
      if (isFALSE(climat) & isTRUE(ht)) {
        if (length(setdiff(c(nom_clim,"t_ma","p_tot"), nom)) >0) {arbres = paste0("Nom des variables climatiques incorrect dans le fichier des arbres")}
      }
  }




  # ajouter un no_arbre et filter les arbres
  if (!is.character(arbres)) {

    # Valider le contenu des colonnes
    arbres <- valid_fic(type_fic='arbres', fichier=arbres, ht=ht, iqs=iqs, climat=climat, sol=sol)

    # vérifier les arbres etude dans lecture_etude
    # verifier les covariables de peuplements dans filter_place
    # verfier les compil dans lecture_compil

    if (!is.character(arbres)) {

    # # retier les variables non nécessaires
    #
    # # listes des variables nécessaires
    # if (isTRUE(ht)){ # si hauteur à estimer, il faut en plus les variables nom_mod_ht
    #   var_base <- c(nom_plot, nom_iqs, nom_clim, nom_sol, nom_coor, nom_an_mes, nom_arbre, nom_vol, nom_ht, nom_mod_ht)
    # }
    # if (isFALSE(ht)){
    #   var_base <- c(nom_plot, nom_iqs, nom_clim, nom_sol, nom_coor, nom_an_mes, nom_arbre, nom_vol, nom_ht)
    # }
    # # nom des variables dans le fichier
    # nom_fic <- names(arbres)
    # # variables non nécessaires
    # retrait <- setdiff(nom_fic, var_base) # setdiff(x, y) finds all rows in x that aren't in y
    # # retirer les variables non nécessaires
    # arbres[retrait] <- list(NULL)

  # créer et filtrer les variables qui seront nécessaires
  arbres <- arbres %>%
    filter(dhpcm>9) %>%
    mutate(no_arbre=row_number())
    }

  }

  return(arbres)
}
