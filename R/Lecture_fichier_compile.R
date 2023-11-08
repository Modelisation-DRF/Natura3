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

#' Function reading the compiled plot file to be simulated with Natura 3.0
#'
#' @description Read a compiled plot file to be simulated with Natura 3.0. check the names of the variables, create total and  percentage variables, rename variables.
#'
#' @param file Name of the file to read (dataframe, Excel or csv file)
#' @inheritParams SimulNatura
#'
#' @return Dataframe at the plot scale ready to be simulate or an error message
#' @export
#'
# @examples
Lecture_compile <- function(file, iqs, climat, sol){


  # vérifier si le fichier est un objet R, sinon importer le fichier
  if (!is.data.frame(file)) {
    suppressMessages(
      if (grepl(".xls", file)) {comp <- read_excel(file)}
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
   nom_base_coor <- c(nom_base, nom_coor)
   nom_base_coor_clim <- c(nom_base, nom_coor, nom_an_mes)

   # vérification des noms de variables de base si iqs/sol/climat ne sont pas à extraire
   if (isFALSE(climat) & isFALSE(iqs) & isFALSE(sol)) {
     if (length(setdiff(nom_base, nom)) >0) {comp = paste0("Nom des variables incorrect dans le fichier d'inventaire compile")}
   }
   # vérification des noms de variables de base si iqs ou sol sont à extraire car il faut lat-long
   if (isTRUE(iqs) | isTRUE(sol)) {
     if (length(setdiff(nom_base_coor, nom)) >0) {comp = paste0("Nom des variables incorrect dans le fichier d'inventaire compile")}
   }
   # vérification des noms de variables de base si climat sont à extraire car il faut lat-long-an_mes
   if (isTRUE(climat)) {
     if (length(setdiff(nom_base_coor_clim, nom)) >0) {comp = paste0("Nom des variables incorrect dans le fichier d'inventaire compile")}
   }
   # vérification des iqs s'ils sont fournis dans le fichier d'inventaire
   if (isFALSE(iqs)) {
     if (length(setdiff(nom_iqs, nom)) >0) {comp = paste0("Nom des variables d'IQS incorrect dans le fichier d'inventaire compile")}
   }
   # vérification des variables climatiques s'ils sont fournis dans le fichier d'inventaire
   if (isFALSE(climat)) {
     if (length(setdiff(nom_clim, nom)) >0) {comp = paste0("Nom des variables climatiques incorrect dans le fichier des arbres")}
   }
   # vérification des variables de sol s'ils sont fournis dans le fichier d'inventaire
   if (isFALSE(sol)) {
     if (length(setdiff(nom_sol, nom)) >0) {comp = paste0("Nom des variables de sol incorrect dans le fichier des arbres")}
   }

   if (!is.character(comp)){

     comp2 <- comp %>%
       mutate(nbop1 = nbop/25, npeu1 = npeu/25, nft1 = nft/25, nepn1 = nepn/25, nepx1 = nepx/25, nsab1 = nsab/25, nri1 = nri/25, nrt1 = nrt/25,
              ntot1 = nbop1 + npeu1 + nft1 + nri1 + nrt1 + nepx1 + nepn1 + nsab1,
              sttot1 = stbop + stpeu + stft + stri + strt + stepx + stepn + stsab,
              vtot1 = vbop + vpeu + vft + vri + vrt + vepx + vepn + vsab,
              pct_epn1 = stepn/sttot1*100,
              pct_epx1 = stepx/sttot1*100,
              pct_sab1 = stsab/sttot1*100,
              pct_ft1 = stft/sttot1*100,
              pct_bop1 = stbop/sttot1*100,
              pct_peu1 = stpeu/sttot1*100,
              pct_ri1 = stri/sttot1*100,
              pct_rt1 = strt/sttot1*100,
              annee=0, tbe=0, pert=0) %>%
       dplyr::select(-nbop, -npeu, -nft, -nri, -nrt, -nepx, -nepn, -nsab) %>%
       rename(stbop1=stbop, stpeu1=stpeu, stft1=stft, stri1=stri, strt1=strt, stepx1=stepx, stepn1=stepn, stsab1=stsab,
              vbop1=vbop, vpeu1=vpeu, vft1=vft, vri1=vri, vrt1=vrt, vepx1=vepx, vepn1=vepn, vsab1=vsab,
              hd1=hd, is1=is
       )
       # %>%
       # dplyr::select(id_pe, annee, tbe, pert, an_mes, latitude, longitude, sdom_bio, altitude, type_eco, t_ma, p_tot, temp_gs, prec_gs, origine, temps,
       #        iqs_pot_epn, iqs_pot_epb, iqs_pot_bop, iqs_pot_pex, iqs_pot_pig, iqs_pot_sab, iqs_pot_tho, iqs_pot_pib,
       #        ph, cec, oc, sand, clay, silt,
       #        hd1, is1,
       #        ntot1, sttot1, vtot1,
       #        nbop1, npeu1, nft1, nri1, nrt1, nepx1, nepn1, nsab1,
       #        stbop1, stpeu1, stft1, stri1, strt1, stepx1, stepn1, stsab1,
       #        vbop1, vpeu1, vft1, vri1, vrt1, vepx1, vepn1, vsab1,
       #        contains("pct_"))
   }
   return(comp2)
}

