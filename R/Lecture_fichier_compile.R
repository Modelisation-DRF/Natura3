################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function importing the compiled plot file                  #
#                                                              #
#   Use nom_variables                                          #
#                                                              #
################################################################

#' Lire le fichier des placettes à simuler avec Natura et valider le nom des colonnes
#'
#' @description Lire le fichier des placettes à simuler avec Natura et valider le nom des colonnes.
#'
#' @param file Nom du fichier des arbres à lire (table, Excel ou csv)
#' @inheritParams SimulNatura
#'
#' @return Table à l'échelle de la placette ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
# #' @export
#'
# @examples
Lecture_compile <- function(file, iqs, climat, sol){


  # vérifier si le fichier est un objet R, sinon importer le fichier
  if (!is.data.frame(file)) {
    suppressMessages(
      if (grepl(".xls", file)) {comp <- readxl::read_excel(file)}
      else if (grepl(".csv", file)) {comp <- read_delim(file, delim = ";")} # fread met ID_PE numérique, mais pas read_delim
    )
  }
  else comp <- file

  names(comp) <- tolower(names(comp))
  nom <- names(comp)

   # liste des noms de variables attendues
   nom_coor <- as.matrix(nom_variables[nom_variables$categorie=="coor","variable"])
   nom_iqs <- as.matrix(nom_variables[nom_variables$categorie=="iqs","variable"])
   nom_clim <- as.matrix(nom_variables[nom_variables$categorie=="clim","variable"])
   nom_sol <- as.matrix(nom_variables[nom_variables$categorie=="sol","variable"])
   nom_plot <- as.matrix(nom_variables[nom_variables$categorie=="plot","variable"])
   nom_an_mes <- as.matrix(nom_variables[nom_variables$categorie=="an_mes","variable"])
   nom_dendro <- as.matrix(nom_variables[nom_variables$categorie=="dendro","variable"])

   nom_base <- c(nom_plot, nom_dendro)


   # vérification des noms de variables de base
   if (length(setdiff(nom_base, nom)) >0)
      {comp = paste0("Nom des variables de base incorrect dans le fichier d'inventaire compile")}

   else{
   # vérification des noms de variables de base si iqs ou sol sont à extraire car il faut lat-long
     if (isTRUE(iqs) | isTRUE(sol)) {
       if (length(setdiff(nom_coor, nom)) >0) {comp = paste0("Coordonnées des placettes manquantes pour extraire iqs/sol")}
     }
     # vérification des noms de variables si climat sont à extraire : il faut lat-long-an_mes
     if (isTRUE(climat)) {
       if (length(setdiff(c(nom_coor, nom_an_mes), nom)) >0) {comp = paste0("Coordonnées des placettes manquantes et année de mesure pour extraire climat")}
     }
     # vérification des iqs s'ils sont fournis dans le fichier d'inventaire
     if (isFALSE(iqs)) {
       if (length(setdiff(nom_iqs, nom)) >0) {comp = paste0("Nom des variables d'iqs incorrect dans le fichier d'inventaire compile")}
     }
     # vérification des variables de sol s'ils sont fournis dans le fichier d'inventaire
     if (isFALSE(sol)) {
       if (length(setdiff(nom_sol, nom)) >0) {comp = paste0("Nom des variables de sol incorrect dans le fichier d'inventaire compile")}
     }
     # vérification des variables climatiques s'ils sont fournis dans le fichier d'inventaire
     if (isFALSE(climat)) {
       if (length(setdiff(nom_clim, nom)) >0) {comp = paste0("Nom des variables climatiques annuelles incorrect dans le fichier d'inventaire compile")}
     }

}
   # if (!is.character(comp)){
   #
   #   # Valider le contenu des colonnes
   #   comp <- valid_fic(type_fic='compile', fichier=comp, iqs=iqs, climat=climat, sol=sol)
   #
   #
   #   if (!is.character(comp)){
   #
   #   # créer les variables qui seront nécessaires
   #   comp <- comp %>%
   #     mutate(nbop1 = nbop/25, npeu1 = npeu/25, nft1 = nft/25, nepn1 = nepn/25, nepx1 = nepx/25, nsab1 = nsab/25, nri1 = nri/25, nrt1 = nrt/25) %>%
   #     dplyr::select(-nbop, -npeu, -nft, -nri, -nrt, -nepx, -nepn, -nsab) %>%
   #     rename(stbop1=stbop, stpeu1=stpeu, stft1=stft, stri1=stri, strt1=strt, stepx1=stepx, stepn1=stepn, stsab1=stsab,
   #            vbop1=vbop, vpeu1=vpeu, vft1=vft, vri1=vri, vrt1=vrt, vepx1=vepx, vepn1=vepn, vsab1=vsab,
   #            hd1=hd, is1=is)
   #   }
   # }
   return(comp)
}



# # listes des variables nécessaires
# var_base <- c(nom_plot, nom_iqs, nom_clim, nom_sol, nom_dendro, nom_coor, nom_an_mes)
# # nom des variables dans le fichier
# nom_fic <- names(comp)
# # variables non nécessaires
# retrait <- setdiff(nom_fic, var_base) # setdiff(x, y) finds all rows in x that aren't in y
# # retirer les variables non nécessaires
# comp[retrait] <- list(NULL)
