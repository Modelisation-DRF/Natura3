#' Préparation du fichier compilé à la step 0 pour la simulation dans Natura
#'
#' @description Préparation du fichier compilé à la step 0 pour la simulation dans Natura: création des variables nécessaires et retrait des variables inutiles
#'
#' @param fichier_compile Table avec une ligne par placette
#'
#' @return Le fichier \code{fichier_compile} avec toutes les variables nécessaires au modèle Natura
#' @export
#'
# @examples
Prep_compile <- function(fichier_compile){

 # fichier_compile=DataCompile_final0

  nom_plot <- as.matrix(nom_variables[nom_variables$categorie=="plot","variable"])
  nom_dendro <- as.matrix(nom_variables[nom_variables$categorie=="dendro","variable"])
  nom_iqs <- as.matrix(nom_variables[nom_variables$categorie=="iqs","variable"])
  nom_clim <- as.matrix(nom_variables[nom_variables$categorie=="clim","variable"]) # on n'a pas besoin de p_tot/t_ma, car seulement pour modèle de ht d'arbres
  nom_sol <- as.matrix(nom_variables[nom_variables$categorie=="sol","variable"])

  # il faut ajouter 1 au bout de chaque nom de variable de dendro
  nom_dendro1 <- paste0(nom_dendro, "1")

  # listes des variables nécessaires
  var_base <- c(nom_plot, nom_dendro1, nom_iqs, nom_clim, nom_sol, "iter")

  # nom des variables dans le fichier
  nom_fic <- names(fichier_compile)
  # variables non nécessaires
  retrait <- setdiff(nom_fic, var_base) # setdiff(x, y) finds all rows in x that aren't in y
  # retirer les variables non nécessaires
  fichier_compile[retrait] <- list(NULL)


  # préparation des variables nécessaires
  compile <- fichier_compile %>%
    mutate(annee=0,
           tbe=0,
           pert=0,

           veg_pot = substr(type_eco,1,3),
           milieu = substr(type_eco,4,4),

           ntot1 = nbop1+npeu1+nft1+nri1+nrt1+nsab1+nepn1+nepx1,
           sttot1 = stbop1+stpeu1+stft1+stri1+strt1+stsab1+stepn1+stepx1,
           vtot1 = vbop1+vpeu1+vft1+vri1+vrt1+vsab1+vepn1+vepx1,

           pct_epn1 = stepn1/sttot1*100,
           pct_epx1 = stepx1/sttot1*100,
           pct_sab1 = stsab1/sttot1*100,
           pct_ri1 = stri1/sttot1*100,
           pct_rt1 = strt1/sttot1*100,
           pct_bop1 = stbop1/sttot1*100,
           pct_peu1 = stpeu1/sttot1*100,
           pct_ft1 = stft1/sttot1*100,

           origBR = ifelse(origine=='BR',1,0),
           origCT = ifelse(origine=='CT',1,0),
           origES = ifelse(origine=='ES',1,0),

           vpms2 = ifelse(veg_pot %in% c('MS2','MS4','ME1'), 1, 0),
           vpms6 = ifelse(veg_pot=='MS6', 1, 0),
           vprb_ = ifelse(veg_pot %in% c('RB5','RB1','RB2'), 1, 0),
           vpre1 = ifelse(veg_pot=='RE1', 1, 0),
           vpre2 = ifelse(veg_pot %in% c('RE2','RE4'), 1, 0),
           vpre3 = ifelse(veg_pot=='RE3', 1, 0),
           vprp1 = ifelse(veg_pot=='RP1', 1, 0),
           vprs1 = ifelse(veg_pot=='RS1', 1, 0),
           vprs2 = ifelse(veg_pot %in% c('RS2','RS4','RS5','RS7'), 1, 0),
           vprs3 = ifelse(veg_pot=='RS3', 1, 0),

           mp0 = ifelse(milieu=='0', 1, 0),
           mp1 = ifelse(milieu=='1', 1, 0),
           mp2 = ifelse(milieu=='2', 1, 0),
           mp3 = ifelse(milieu=='3', 1, 0),
           mp4 = ifelse(milieu=='4', 1, 0),
           mp5 = ifelse(milieu=='5', 1, 0),
           mp6 = ifelse(milieu=='6', 1, 0),
           mp789 = ifelse(milieu %in% c('7','8','9'), 1, 0)) %>%
           rename(iqs_epn=iqs_pot_epn, iqs_epx=iqs_pot_epb, iqs_rt=iqs_pot_tho, iqs_ri=iqs_pot_pig, iqs_bop=iqs_pot_bop, iqs_peu=iqs_pot_pex, iqs_ft=iqs_pot_pib, iqs_sab=iqs_pot_sab)

return(compile)

}
