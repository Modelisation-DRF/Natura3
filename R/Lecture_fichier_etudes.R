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

#' Lire le fichier des arbres-études et valider le nom des colonnes
#'
#' @description Lire le fichier des arbres-études et valider le nom des colonnes.
#'
#' @param file Nom du fichier à lire (table, Excel ou csv)
#'
#' @return Table arbres-études ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
#' @export
#'
# @examples
Lecture_etudes <- function(file){

  # vérifier si le fichier est un objet R, sinon importer le fichier
  if (!is.data.frame(file)) {
    suppressMessages(
      if (grepl(".xls", file)) {etudes <- readxl::read_excel(file)}
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
  difference_nom_etude <- setdiff(nom_base, nom)

  if (length(difference_nom_etude) >0) {etudes = paste0("Nom des variables incorrect dans le fichier des arbres-études. Les variables suivantes sont requises : ",
                                                        paste(difference_nom_etude, collapse = ', '))
  }else if(!any(file$etage %in% c("C", "D"))){

    etudes = paste0("Aucun arbre avec l'étage C ou D " )
  }


  # filtrer les etudes d'arbres
  # if (!is.character(etudes)) {
  #
  #   # Valider le contenu des colonnes
  #   etudes <- valid_fic(type_fic='etudes', fichier=etudes)
  #
  #   if (!is.character(etudes)) {
  #
  #   etudes <- etudes %>%
  #     filter(dhpcm>9, toupper(etage) %in% c('C','D')) %>%
  #     #filter(!is.na(hauteur), hauteur>=2) %>%
  #     dplyr::select(id_pe, essence, dhpcm, hauteur)
  #
  #   }
  # }

  return(etudes)
}
