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

#' Filtering plots and preparing covariates for a simulation with Natura 3.0 growth simulator
#'
#' @description Filter plots and prepare covariates for all the models of Natura 3.0 growth simulator.
#'
#' @details
#' The plots are filtered for type_eco, sdom_bio and origine values. Variables veg_pot and milieu are extracted from type_eco, binary variables are created for each category of sdom_bio, veg_pot, milieu and origine.
#' Variables containing potential iqs are renamed. The variable annee=0 is created.
#'
#' @param fichier Dataframe of trees or of plots with columns: type_eco, sdom_bio, origine, iqs_pot_epn, iqs_pot_epb, iqs_pot_pig, iqs_pot_sab, iqs_pot_tho, iqs_pot_pib, iqs_pot_bop, iqs_pot_pex
#'
#' @return Dataframe filtered and with all necessary plots covariates.
#' @export
#'
#' @examples
#' \dontrun{
#' plots <- Filtrer_place(fichier=liste_plot)
#' }
Filtrer_place <- function(fichier) {

  fic1 <- fichier %>%
    mutate(veg_pot = substr(type_eco,1,3),
           milieu = as.numeric(substr(type_eco,4,4))) %>%
    filter(sdom_bio %in% c('1', '2E','2O','3E','3O','4E','4O','5E','5O','6E','6O'),
           milieu>=0, milieu<=9,
           origine %in% c('BR','CT','ES')) %>%
    inner_join(vp_retenues, by='veg_pot')  %>%
    mutate(annee = 0,

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

           mp0 = ifelse(milieu==0, 1, 0),
           mp1 = ifelse(milieu==1, 1, 0),
           mp2 = ifelse(milieu==2, 1, 0),
           mp3 = ifelse(milieu==3, 1, 0),
           mp4 = ifelse(milieu==4, 1, 0),
           mp5 = ifelse(milieu==5, 1, 0),
           mp6 = ifelse(milieu==6, 1, 0),
           mp789 = ifelse(milieu %in% c(7,8,9), 1, 0)) %>%
    rename(iqs_epn=iqs_pot_epn, iqs_epx=iqs_pot_epb, iqs_rt=iqs_pot_tho,
           iqs_ri=iqs_pot_pig, iqs_bop=iqs_pot_bop, iqs_peu=iqs_pot_pex, iqs_ft=iqs_pot_pib, iqs_sab=iqs_pot_sab)

  return(fic1)
}
