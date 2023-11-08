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


#' Function reading the tree file to be simulated with Natura 3.0
#'
#' @description Read a file at the tree level to be simulated with Natura 3.0. Check the names of the variables and filter trees from variables etat and dhp.
#'
#' @param file Name of the file to read (dataframe, Excel or csv file)
#' @inheritParams SimulNatura
#'
#' @return Dataframe of filtered trees or an error message
#' @export
#'
# @examples
Lecture_arbres <- function(file, ht, vol, iqs, climat, sol){

  # vérifier si le fichier est un objet R, sinon importer le fichier
  if (!is.data.frame(file)) {
    suppressMessages(
    if (grepl(".xls", file)) {arbres <- read_excel(file)}
    else if (grepl(".csv", file)) {arbres <- read_delim(file, delim = ";")} # fread met ID_PE numérique, mais pas read_delim
    )
  }
  else {arbres <- file}
  names(arbres) <- tolower(names(arbres))

  nom <- names(arbres)

  nom_coor <- as.matrix(nom_variables[nom_variables$categorie=="coor","variable"])
  nom_iqs <- as.matrix(nom_variables[nom_variables$categorie=="iqs","variable"])
  nom_clim <- as.matrix(nom_variables[nom_variables$categorie=="clim","variable"])
  nom_sol <- as.matrix(nom_variables[nom_variables$categorie=="sol","variable"])
  nom_arbre <- as.matrix(nom_variables[nom_variables$categorie=="arbre","variable"])
  nom_plot <- as.matrix(nom_variables[nom_variables$categorie=="plot","variable"])
  nom_an_mes <- as.matrix(nom_variables[nom_variables$categorie=="an_mes","variable"])
  nom_vol <- as.matrix(nom_variables[nom_variables$categorie=="vol","variable"])
  nom_ht <- as.matrix(nom_variables[nom_variables$categorie=="ht","variable"])

  nom_base <- c(nom_plot, nom_arbre)
  nom_base_coor <- c(nom_base, nom_coor)
  nom_base_coor_clim <- c(nom_base, nom_coor, nom_an_mes)

  # setdiff : Find Elements that Exist Only in First, But Not in Second Vector

  # vérification des noms de variables de base si iqs/sol/climat ne sont pas à extraire
  if (isFALSE(climat) & isFALSE(iqs) & isFALSE(sol)) {
    if (length(setdiff(nom_base, nom)) >0) {arbres = paste0("Nom des variables incorrect dans le fichier des arbres")}
  }
  # vérification des noms de variables de base si iqs ou sol sont à extraire car il faut lat-long
  if (isTRUE(iqs) | isTRUE(sol)) {
    if (length(setdiff(nom_base_coor, nom)) >0) {arbres = paste0("Nom des variables incorrect dans le fichier des arbres")}
  }
  # vérification des noms de variables de base si climat sont à extraire car il faut lat-long-an_mes
  if (isTRUE(climat)) {
    if (length(setdiff(nom_base_coor_clim, nom)) >0) {arbres = paste0("Nom des variables incorrect dans le fichier des arbres")}
  }
  # vérification des iqs s'ils sont fournis dans le fichier d'inventaire
  if (isFALSE(iqs)) {
    if (length(setdiff(nom_iqs, nom)) >0) {arbres = paste0("Nom des variables d'iqs incorrect dans le fichier des arbres")}
  }
  # vérification des variables climatiques s'ils sont fournis dans le fichier d'inventaire
  if (isFALSE(climat)) {
    if (length(setdiff(nom_clim, nom)) >0) {arbres = paste0("Nom des variables climatiques incorrect dans le fichier des arbres")}
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

  # ajouter un no_arbre et filter les arbres
  if (!is.character(arbres)) {
  arbres <- arbres %>%
    filter(dhpcm>9,
           etat %in% c(10,12,40,42,30,32,50,52)) %>%
    mutate(no_arbre=row_number())
  }

  return(arbres)
}
