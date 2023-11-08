################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function importing the study-tree file                     #
#                                                              #
#   Use nom_variables                                          #
#                                                              #
################################################################

#' Function reading the study-tree file for a simulation with Natura 3.0
#'
#' @description Read a study-tree file for a simulation with Natura 3.0. Check the names of the variables and filter trees from variable dhp, hauteur and etage
#'
#' @param file Name of the file to read (dataframe, Excel or csv file)
#'
#' @return Dataframe of filtered study-tree or an error message
#' @export
#'
# @examples
Lecture_etudes <- function(file){

  # vérifier si le fichier est un objet R, sinon importer le fichier
  if (!is.data.frame(file)) {
    suppressMessages(
    if (grepl(".xls", file)) {etudes <- read_excel(file)}
    else if (grepl(".csv", file)) {etudes <- read_delim(file, delim = ";")} # fread met ID_PE numérique, mais pas read_delim
    )
  }
  else etudes <- file
  names(etudes) <- tolower(names(etudes))

  # vérification des variables obligatoires: placette, etage, essence, dhpcm, hauteur
  nom <- names(etudes)
  nom_base <- as.matrix(nom_variables[nom_variables$categorie=="etude","variable"])

  # setdiff : Find Elements that Exist Only in First, But Not in Second Vector

  # vérification des noms de variables de base
  if (length(setdiff(nom_base, nom)) >0) {etudes = paste0("Nom des variables incorrect dans le fichier des arbres-etudes")}

  # filtrer les etudes d'arbres
  if (!is.character(etudes)) {
    etudes <- etudes %>%
      filter(dhpcm>9, etage %in% c('C','D','c','d')) %>%
      filter(!is.na(hauteur), hauteur>=2) %>%
      dplyr::select(id_pe, essence, dhpcm, hauteur)
  }
  return(etudes)
}
