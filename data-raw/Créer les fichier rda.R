# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw

# param_tarif = read.sas7bdat("c:/Mes docs/ data/ beta_volume.sas7bdat")
# param_ht = read.sas7bdat("c:/Mes docs/ data/ beta_ht.sas7bdat")
# Puis utiliser la ligne de code suivant (toujours dans le projet du package)
# usethis::use_data(param_tarif, param_ht, internal=TRUE): ça fonctionne seulement si le projet est un package

# library(readxl)
# library(sas7bdat)
# library(tidyverse)

### J'AI MIS CERTAINS CHEMINS D'ACCÈS EN COMMENTAIRE POUR NE PAS FAIRE DE MESSAGE LORS DU BUILD DU PACKAGE


########################################################################################

# liste des veg_pot traitées dans Natura (utilisées dans la fonction Filtrer_place())
vp_retenues <- read_delim("data-raw/Vegpot.csv", delim = ';') %>%
    dplyr::select(VegPotName) %>%
    rename(veg_pot=VegPotName)


########################################################################################

# fichier d'association des essences en groupe d'essences Natura
n_st_v_ass_ess <- read_delim("data-raw/Especes.csv", delim = ';')  %>%
    dplyr::select(Essence, Groupe_ess, ess_eq_hd) %>%
    filter(Groupe_ess != 'NC')
names(n_st_v_ass_ess) <- tolower(names(n_st_v_ass_ess))


########################################################################################


# fichier des paramètres pour le calcul des ht des arbres pour la hdom, dans la fct param_hdom0_ess_stoch()
hdom_param_ess_fixe <- read.sas7bdat("data-raw/parmsHD_random_arbre_20211108.sas7bdat")
hdom_param_global_fixe <-  read.sas7bdat("data-raw/parmsHD_tous_20211108.sas7bdat")


########################################################################################

# Fichiers des paramètres pour l'évolution de IS
is_param_fixe =  read.sas7bdat("data-raw/is_parms_20230209_lin2r.sas7bdat")
is_param_cov =  read.sas7bdat("data-raw/is_parmscov_20230209_lin2.sas7bdat")


########################################################################################

# Fichiers des paramètres pour l'évolution de HD
hdevol_param_fixe =  read.sas7bdat("data-raw/hd_parms_20230602.sas7bdat")
hdevol_param_cov =  read.sas7bdat("data-raw/hd_parmscov_20230602.sas7bdat")


########################################################################################

# Fichiers pour l'évolution de N-St-V, fichiers par essence
n_st_v_param_fixe <- list()
n_st_v_param_cov <- list()
n_st_v_param_random <- NULL
# il faut le fichier séparés par essence
liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
for (ess in liste_gress) {

  #ess="epn"
  param   <- read.sas7bdat(paste("data-raw/Systemes_equations/Nom_fichier_pour_simulateur/",ess,"_parms.sas7bdat", sep='')) %>% dplyr::select(Parameter, Estimate) %>%
    pivot_wider(names_from = Parameter, values_from = Estimate) %>%
    mutate(essence=ess)

  # lecture de la matrice de covariance des effets fixes
  param_covb =  read.sas7bdat(paste("data-raw/Systemes_equations/Nom_fichier_pour_simulateur/",ess,"_covb.sas7bdat", sep='')) %>% dplyr::select(-EstType, -Parameter) %>%
    mutate(essence=ess)

  # lecture de la matrice de covariance des erreurs résiduelles du systeme d'équations
  param_covr =  read.sas7bdat(paste("data-raw/Systemes_equations/Nom_fichier_pour_simulateur/",ess,"_covr.sas7bdat", sep='')) %>% dplyr::select(-EstType, -Equation)
  param_covr <- param_covr[1:3, 1:3] %>% # on garde seulement n-st-v, on n'a pas besoin de dq et vtige
    mutate(essence=ess)

  n_st_v_param_fixe <- append(n_st_v_param_fixe, list(param))
  n_st_v_param_cov <- append(n_st_v_param_cov, list(param_covb))
  n_st_v_param_random <- bind_rows(n_st_v_param_random, param_covr)
}
names(n_st_v_param_fixe) <- liste_gress
names(n_st_v_param_cov) <- liste_gress



########################################################################################


# Fichiers des équations de is-hd-n-st-v
is_eq <- read.sas7bdat("data-raw\\is_model_20230209_lin2.sas7bdat")
hdevol_eq <- read.sas7bdat("data-raw\\hd_model_20230602.sas7bdat")

n_st_v_eq <- NULL
liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
for (ess in tolower(liste_gress)) {
  #ess = 'sab'
  eq <- read.sas7bdat(paste0("data-raw\\Systemes_equations\\Nom_fichier_pour_simulateur\\",ess,"_model.sas7bdat"))
  eq$essence <- ess
  n_st_v_eq <- bind_rows(n_st_v_eq,eq)
}


########################################################################################

# fichier des maximum de n et st en évolution pour ne pas déraper
n_st_max <- read_excel("data-raw\\N_ST_max.xlsx")

########################################################################################

# fichier des valeurs admissibles de fichiers d'intrant
fic_validation <- read_excel("data-raw\\Validation_intrants.xlsx")

########################################################################################



# fichier avec les noms de variables attendus dans les fichiers d'intrants
# nom_coor <- data.frame ("variable"=c("latitude","longitude"), "categorie"=rep(x="coor", times=2))
# nom_iqs <- data.frame("variable"=c("iqs_pot_sab", "iqs_pot_epn", "iqs_pot_epb", "iqs_pot_pib", "iqs_pot_bop", "iqs_pot_pex", "iqs_pot_pig", "iqs_pot_tho"),
#                       "categorie"=rep("iqs",8))
# nom_clim <- data.frame("variable"=c("p_tot", "t_ma", "prec_gs", "temp_gs"), "categorie"=rep("clim",4))
# nom_sol <- data.frame("variable"=c("cec","oc","ph","sand","clay"), "categorie"=rep("sol",5))
# nom_arbre <- data.frame("variable"=c( "essence", "dhpcm", "tige_ha", "etat"), "categorie"=rep("arbre",4))
# nom_etude <- data.frame("variable"=c("id_pe", "etage", "essence", "dhpcm","hauteur"), "categorie"=rep("etude",5))
# nom_plot <- data.frame("variable"=c("id_pe", "sdom_bio", "altitude", "type_eco", "temps", "origine"), "categorie"=rep("plot",6))
# nom_an_mes <- data.frame("variable"=c("an_mes"), "categorie"=rep("an_mes",1))
# nom_vol <- data.frame("variable"=c("vol_dm3"), "categorie"=rep("vol",1))
# nom_ht <- data.frame("variable"=c("hauteur_pred"), "categorie"=rep("ht",1))
# nom_dendro <- data.frame("variable"=c("nbop","npeu","nft","nepn","nepx","nsab","nri","nrt",
#                                       "stbop","stpeu","stft","stepn","stepx","stsab","stri","strt",
#                                       "vbop","vpeu","vft","vepn","vepx","vsab","vri","vrt",
#                                       "hd","is"),
#                          "categorie"=rep("dendro",26))
# nom_variables <- bind_rows(nom_coor,nom_iqs,nom_clim,nom_sol,nom_arbre,nom_etude,nom_plot,nom_vol,nom_ht, nom_dendro, nom_an_mes)
# write_delim(nom_variables, file="data-raw\\nom_variables.csv", delim = ';')
# nom_variables <- read_delim("data-raw\\nom_variables.csv",delim = ';')



########################################################################################
# ESSAI/TEST
# fichier avec les noms de variables attendus dans les fichiers d'intrants
nom_coor <- data.frame ("variable"=c("latitude","longitude"), "categorie"=rep(x="coor", times=2))
nom_mod_ht <- data.frame("variable"=c("p_tot", "t_ma", "altitude"), "categorie"=rep("modele_ht",3)) # nom des variables supplémentaires pour modèle de ht

# variables pour Natura
nom_plot <- data.frame("variable"=c("id_pe", "sdom_bio", "type_eco", "temps", "origine"), "categorie"=rep("plot",5))
nom_iqs <- data.frame("variable"=c("iqs_pot_sab", "iqs_pot_epn", "iqs_pot_epb", "iqs_pot_pib", "iqs_pot_bop", "iqs_pot_pex", "iqs_pot_pig", "iqs_pot_tho"), "categorie"=rep("iqs",8))
nom_clim <- data.frame("variable"=c("prec_gs", "temp_gs"), "categorie"=rep("clim",2))
nom_sol <- data.frame("variable"=c("cec","oc","ph","sand","clay"), "categorie"=rep("sol",5))

nom_arbre <- data.frame("variable"=c( "essence", "dhpcm", "tige_ha", "etat"), "categorie"=rep("arbre",4))
nom_etude <- data.frame("variable"=c("id_pe", "etage", "essence", "dhpcm","hauteur"), "categorie"=rep("etude",5))

nom_an_mes <- data.frame("variable"=c("an_mes"), "categorie"=rep("an_mes",1))
nom_vol <- data.frame("variable"=c("vol_dm3"), "categorie"=rep("vol",1))
nom_ht <- data.frame("variable"=c("hauteur_pred"), "categorie"=rep("ht",1))
nom_dendro <- data.frame("variable"=c("nbop","npeu","nft","nepn","nepx","nsab","nri","nrt",
                                      "stbop","stpeu","stft","stepn","stepx","stsab","stri","strt",
                                      "vbop","vpeu","vft","vepn","vepx","vsab","vri","vrt",
                                      "hd","is"),
                         "categorie"=rep("dendro",26))
nom_variables <- bind_rows(nom_coor,nom_mod_ht,nom_iqs,nom_clim,nom_sol,nom_arbre,nom_etude,nom_plot,nom_vol,nom_ht, nom_dendro, nom_an_mes)
write_delim(nom_variables, file="data-raw\\nom_variables.csv", delim = ';')
nom_variables <- read_delim("data-raw\\nom_variables.csv",delim = ';')




########################################################################################


# save(vp_retenues, file="data/vp_retenues.rda")
# save(n_st_v_ass_ess, file="data/n_st_v_ass_ess.rda")
# save(hdom_param_ess_fixe, file="data/hdom_param_ess_fixe.rda")
# save(hdom_param_global_fixe, file="data/hdom_param_global_fixe.rda")
# save(is_param_fixe, file="data/is_param_fixe.rda")
# save(is_param_cov, file="data/is_param_cov.rda")
# save(hdevol_param_fixe, file="data/hdevol_param_fixe.rda")
# save(hdevol_param_cov, file="data/hdevol_param_cov.rda")
# save(n_st_v_param_fixe, file="data/n_st_v_param_fixe.rda")
# save(n_st_v_param_cov, file="data/n_st_v_param_cov.rda")
# save(n_st_v_param_random, file="data/n_st_v_param_random.rda")
# save(is_eq, file="data/is_eq.rda")
# save(hdevol_eq, file="data/hdevol_eq.rda")
# save(n_st_v_eq, file="data/n_st_v_eq.rda")
# save(n_st_max,file="data/n_st_max.rda")
# save(nom_variables, file="data/nom_variables.rda")

# tous les fichiers à mettre dans le rda
usethis::use_data(vp_retenues, n_st_max, nom_variables, n_st_v_ass_ess,
                  hdom_param_ess_fixe, hdom_param_global_fixe,
                  is_param_fixe, is_param_cov,
                  hdevol_param_fixe, hdevol_param_cov,
                  n_st_v_param_fixe, n_st_v_param_cov, n_st_v_param_random,
                  is_eq,
                  hdevol_eq,
                  n_st_v_eq,
                  fic_validation,
                  internal=TRUE, overwrite = TRUE)


