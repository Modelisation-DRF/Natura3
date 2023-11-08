# créer des fichiers d'exemples à ajouter au package


##############################################
# Fichiers à l'échelle de l'arbre
file_arbre="U:\\Projets\\IsabelleAuger\\Natura-2020\\PET\\PET_arbres.csv" # ce fichier a tous temps=50 et origine=BR car je n'ai pas l'origine  pour les PET
file_etude="U:\\Projets\\IsabelleAuger\\Natura-2020\\PET\\PET_arbres_etudes.csv"
fichier_arbres_complet <- read_delim(file=file_arbre, delim = ';')
fichier_etudes_complet <- read_delim(file=file_etude, delim = ';')


# choisir les vp et sdom
fichier_arbres_complet$vp <- substr(fichier_arbres_complet$type_eco,1,3)
fichier_arbres_vp <- fichier_arbres_complet %>% filter(vp %in% c('MS2','RE1','RE2','RS2','RE3'), SDOM_BIO %in% c('4E','4O','5E','5O','6E','6O')) %>% dplyr::select(-vp)
listeplot <- unique(fichier_arbres_vp$ID_PE) # 43114
fichier_etudes_vp <- fichier_etudes_complet[fichier_etudes_complet$ID_PE %in% listeplot,]

# pour faire le test de lire iqs climat et sol dans des cartes
fichier_arbres_sanscov <- fichier_arbres_vp %>% dplyr::select(-contains("iqs_"), -clay, -silt, -ph, -cec, -oc, -sand, -t_ma, -prec_gs, -temp_gs, -p_tot)


# ne choisir que 2 placettes
liste <- fichier_arbres_vp %>% filter(ID_PE %in% c('0400102903','0319801702')) %>% dplyr::select(ID_PE)
liste <- unique(liste$ID_PE)


fichier_arbres_select <- fichier_arbres_sanscov[fichier_arbres_sanscov$ID_PE %in% liste,]
fichier_arbres_select2 <- fichier_arbres_vp[fichier_arbres_vp$ID_PE %in% liste,]
fichier_etudes_select <- fichier_etudes_vp[fichier_etudes_vp$ID_PE %in% liste,]

fichier_arbres_sanscov <- fichier_arbres_select %>% dplyr::select(-pet,-an_mes, -id)
fichier_arbres_aveccov <- fichier_arbres_select2 %>% dplyr::select(-pet,-an_mes, -id, -iqs_pot_EPR)
fichier_arbres_etudes <- fichier_etudes_select

# sauvegarder le fichier en rda sous /data
usethis::use_data(fichier_arbres_sanscov, fichier_arbres_aveccov, fichier_arbres_etudes, overwrite = TRUE)

############################################
# Fichier compilé à la placette
file_compile="U:\\Projets\\IsabelleAuger\\Natura-2020\\PET\\PEP_MES1_compile.csv"
fichier_compile_complet <- read_delim(file=file_compile, delim = ';')

liste <- fichier_compile_complet %>% filter(id_pe %in% c('0700200501_N_1970', '1419610501_N_1922')) %>% dplyr::select(id_pe)
liste <- unique(liste$id_pe)

fichier_compile_aveccov <- fichier_compile_complet[fichier_compile_complet$id_pe %in% liste,] %>% dplyr::select(-an_mes)
fichier_compile_sanscov <- fichier_compile_select %>% dplyr::select(-contains("iqs_"), -an_mes, -clay, -silt, -ph, -cec, -oc, -sand, -t_ma, -prec_gs, -temp_gs, -p_tot)

# sauvegarder le fichier en rda sous /data
usethis::use_data(fichier_compile_aveccov, fichier_compile_sanscov, overwrite = TRUE)


