################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function filtering plots and preparing covariates          #
#                                                              #
#   Use vp_retenues.rda                                        #
#                                                              #
################################################################

#' Sélectionne les placettes qui peuvent être simulées avec Natura 3.0
#'
#' @description Sélectionne les placettes qui peuvent être simulées avec Natura 3.0
#'
#' @details
#' Les placettes sont sélectionnées selon le type écologique (type_eco), le sous-domaine bioclimatique (sdom_bio) et la perturbation d'origine (origine).
#' Les variables végétation potentielle (veg_pot) et milieu physique (milieu) sont créées à partir de type_eco, des variables dichotomiques sont créées pour chaque niveau de sdom_bio,
#' veg_pot, milieu et origine. Les variables d'IQS potentiel sont renommées. La variable annee=0 est créée.
#'
#' @param fichier Nom de la table à l'échelle de l'arbre ou de la placette avec les colonnes nécessaires
#'
#' @return Table dont les placettes ont été filtrées et contenant des nouvelles covariables.
#' @export
#'
#' @examples
#' \dontrun{
#' plots <- Filtrer_place(fichier=liste_plot)
#' }
Filtrer_place <- function(fichier) {

  #fichier=fic_tous
  #fichier=DataCompile_final0; type="compile";
  # il y aurait pu avoir un filtre seulement une fois le fichier compilé, on n'aurait alors pas eu besoin du paramètre type, mais si le fichier fourni à l'échelle de l'arbre contient bcp de variable et qu'on est en mode stochastique, ça peut devenir lourd, alors aussi bien le filtrer aussi

  # # listes des variables nécessaires
  # nom_iqs <- as.matrix(nom_variables[nom_variables$categorie=="iqs","variable"])
  # nom_clim <- as.matrix(nom_variables[nom_variables$categorie=="clim","variable"])
  # nom_sol <- as.matrix(nom_variables[nom_variables$categorie=="sol","variable"])
  # nom_arbre <- as.matrix(nom_variables[nom_variables$categorie=="arbre","variable"])
  # nom_plot <- as.matrix(nom_variables[nom_variables$categorie=="plot","variable"])
  # nom_dendro <- as.matrix(nom_variables[nom_variables$categorie=="dendro","variable"])
  # nom_ht <- as.matrix(nom_variables[nom_variables$categorie=="ht","variable"])
  # nom_vol <- as.matrix(nom_variables[nom_variables$categorie=="vol","variable"])
  #
  # var_ht <- NULL
  # var_vol <- NULL
  # nom_fic <- names(fichier)
  # if (nom_ht %in% nom_fic) {var_ht=nom_ht}
  # if (nom_vol %in% nom_fic) {var_ht=nom_vol}
  #
  # var_base <- c(nom_plot, nom_iqs, nom_clim, nom_sol)
  # var_place <- c(nom_plot, nom_dendro)
  # var_arbre <- c(var_base, nom_arbre, "no_arbre", var_ht, var_vol)
  #
  # retrait <- setdiff(nom_fic, var_place)
  # DataCompile_final0[colnames(fic_tous)[-1]] <- list(NULL)
  #
  #
  # # ne garder que les colonnes nécessaires au modèle
  # if (type=="compile") {
  #   fic0 <- fichier[,var_place]
  #   }
  #
  # if (type=="arbre") {
  #   fic0 <- fichier[,var_arbre]
  # }
  #

  # fic1 <- fichier %>%
  #   mutate(veg_pot = substr(type_eco,1,3),
  #          milieu = substr(type_eco,4,4)) %>%
  #   filter(sdom_bio %in% c('1', '2E','2O','3E','3O','4E','4O','5E','5O','6E','6O'),
  #          milieu %in% c('0','1','2','3','4','5','6','7','8','9'),
  #          origine %in% c('BR','CT','ES')) %>%
  #   inner_join(vp_retenues, by='veg_pot')  %>%
  #   mutate(annee = 0,
  #
  #          origBR = ifelse(origine=='BR',1,0),
  #          origCT = ifelse(origine=='CT',1,0),
  #          origES = ifelse(origine=='ES',1,0),
  #
  #          vpms2 = ifelse(veg_pot %in% c('MS2','MS4','ME1'), 1, 0),
  #          vpms6 = ifelse(veg_pot=='MS6', 1, 0),
  #          vprb_ = ifelse(veg_pot %in% c('RB5','RB1','RB2'), 1, 0),
  #          vpre1 = ifelse(veg_pot=='RE1', 1, 0),
  #          vpre2 = ifelse(veg_pot %in% c('RE2','RE4'), 1, 0),
  #          vpre3 = ifelse(veg_pot=='RE3', 1, 0),
  #          vprp1 = ifelse(veg_pot=='RP1', 1, 0),
  #          vprs1 = ifelse(veg_pot=='RS1', 1, 0),
  #          vprs2 = ifelse(veg_pot %in% c('RS2','RS4','RS5','RS7'), 1, 0),
  #          vprs3 = ifelse(veg_pot=='RS3', 1, 0),
  #
  #          mp0 = ifelse(milieu=='0', 1, 0),
  #          mp1 = ifelse(milieu=='1', 1, 0),
  #          mp2 = ifelse(milieu=='2', 1, 0),
  #          mp3 = ifelse(milieu=='3', 1, 0),
  #          mp4 = ifelse(milieu=='4', 1, 0),
  #          mp5 = ifelse(milieu=='5', 1, 0),
  #          mp6 = ifelse(milieu=='6', 1, 0),
  #          mp789 = ifelse(milieu %in% c('7','8','9'), 1, 0)) %>%
  #   rename(iqs_epn=iqs_pot_epn, iqs_epx=iqs_pot_epb, iqs_rt=iqs_pot_tho,
  #          iqs_ri=iqs_pot_pig, iqs_bop=iqs_pot_bop, iqs_peu=iqs_pot_pex, iqs_ft=iqs_pot_pib, iqs_sab=iqs_pot_sab)

  fic1 <- fichier %>%
    mutate(veg_pot = substr(type_eco,1,3),
           milieu = substr(type_eco,4,4)) %>%
    inner_join(vp_retenues, by='veg_pot') %>%
    filter(sdom_bio %in% c('1', '2E','2O','3E','3O','4E','4O','5E','5O','6E','6O'), # déjà traité lors de la lecture du fichier
           milieu %in% c('0','1','2','3','4','5','6','7','8','9'),
           origine %in% c('BR','CT','ES'), # déjà traité lors de la lecture du fichier
           temps>=10) # déjà traité lors de la lecture du fichier



  return(fic1)
}
