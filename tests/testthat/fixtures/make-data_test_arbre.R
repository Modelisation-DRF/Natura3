# Qu'est ce que je veux tester dans Natura?
# je mets seulement quelques tests dans la fct principale? ça ne testera vraiment pas tous les cas
# je teste toutes les fct une par une, ça va etre long... mais c'est la façon la plus efficace de trouver les bogs/failles, il y a 13 fonctions, dont certaines assez grosses

# tester un fichier compilé
# tester un fichier echelle arbre

# Fichier test, une placette par veg_pot traitée (34)
# un leur associe un des 11 sdom et 10 milieu traités
# on met un arbre de chacune des 63 essences traitées dans chacune des 34 placettes

# fichier association des essences pour le modèle de hauteur-dhp
unique(ht_ass_ess$essence) # 63
unique(ht_ass_ess$essence_hauteur) # 27
view(ht_ass_ess)
# fichier d'associasions des sdom, pert, etc. pour les modèles ht-dhp, un fichier par essence
ht_liste_ess # 27

unique(ht_ass_vp$veg_pot) # 34
unique(ht_ass_vp$vp) # 31

unique(ht_ass_sd$sdom_bio) # 11
unique(ht_ass_sd$sdom) # 11

unique(ht_ass_mil$type_eco4) # 10
unique(ht_ass_mil$milieu) # 11

# créer les 34 placettes
id1<-c("FC1", "FE1", "FE2")
id2<-c("FE3", "FE4", "FE5")
id3<-c("FE6", "FO1", "ME1")
id4<-c("MF1", "MJ1", "MJ2")
id5<-c("MS2", "MS4", "MS6")
id6<-c("MS7", "RP1", "RT1")
id7<-c("RC3", "MS1")
id8<-c("RE2", "RE3", "RB1", "RB2", "RB5")
id9<-c("RE4", "RE7", "RE1")
id10<-c("RS1", "RS2", "RS3")
id11<-c("RS4", "RS5", "RS7")

id1 <- data.frame(veg_pot=id1, sdom_bio="1", milieu=0)
id2 <- data.frame(veg_pot=id2, sdom_bio="2E", milieu=1)
id3 <- data.frame(veg_pot=id3, sdom_bio="2O", milieu=2)
id4 <- data.frame(veg_pot=id4, sdom_bio="3E", milieu=3)
id5 <- data.frame(veg_pot=id5, sdom_bio="3O", milieu=4)
id6 <- data.frame(veg_pot=id6, sdom_bio="4E", milieu=5)
id7 <- data.frame(veg_pot=id7, sdom_bio="4O", milieu=6)
id8 <- data.frame(veg_pot=id8, sdom_bio="5E", milieu=7)
id9 <- data.frame(veg_pot=id9, sdom_bio="5O", milieu=8)
id10 <- data.frame(veg_pot=id10, sdom_bio="6E", milieu=9)
id11 <- data.frame(veg_pot=id11, sdom_bio="6O", milieu=0)

id <- bind_rows(id1, id2, id3, id4, id5, id6, id7, id8, id9, id10, id11) %>%
  mutate(id_pe=row_number(),
         p_tot=1000,
         t_ma=0.1,
         altitude=200)

# créer la liste des 63 arbres
# liste de toutes les essences traitées
ess <- unique(ht_ass_ess$essence) # 63
ess <- data.frame(essence=ess, dhpcm=10, nb_tige=1) %>%
  mutate(no_arbre = row_number())

# mettre les 63 arbres dans chacune des 34 placettes: 2142 lignes
data_arbre <- as.data.frame(unclass(expand_grid(id, ess))) %>%
  dplyr::select(id_pe, veg_pot, milieu, sdom_bio, altitude, t_ma, p_tot, no_arbre, essence, nb_tige, dhpcm)

saveRDS(data_arbre, "tests/testthat/fixtures/data_arbre.rds")


# fichier de ht attendu en mode déterministe
# je devrais faire ce fichier dans SAS pour m'assurer que R donne la meme chose
parametre_ht_attendu <- param_ht(fic_arbres=data_arbre)
data_arbre_attendu <- relation_h_d(fic_arbres=data_arbre, parametre_ht=parametre_ht_attendu)
saveRDS(data_arbre_attendu, "tests/testthat/fixtures/data_arbre_attendu.rds")
saveRDS(parametre_ht_attendu, "tests/testthat/fixtures/parametre_ht_attendu.rds")

# fichier attendu pour le volume deterministe: une seule ligne par essence est suffisant
data_arbre_vol <- data_arbre_attendu %>%
  group_by(essence) %>%
  slice(1) %>%
  ungroup()
saveRDS(data_arbre_vol, "tests/testthat/fixtures/data_arbre_vol.rds")
param_vol_attendu <- param_vol(fic_arbres=data_arbre_vol)
saveRDS(param_vol_attendu, "tests/testthat/fixtures/param_vol_attendu.rds")
data_arbre_vol_attendu <- cubage(fic_arbres=data_arbre_vol, mode_simul='DET', parametre_vol=param_vol_attendu)
saveRDS(data_arbre_vol_attendu, "tests/testthat/fixtures/data_arbre_vol_attendu.rds")


# fichier de ht attendu en mode stochastique, en fixant le seed à 20
parametre_ht_attendu_sto <- param_ht(fic_arbres=data_arbre, mode_simul = 'STO', nb_iter = 1, seed_value = 20)
data_arbre_attendu_sto <- relation_h_d(fic_arbres=data_arbre, parametre_ht=parametre_ht_attendu_sto, mode_simul = 'STO', iteration=1)
saveRDS(data_arbre_attendu_sto, "tests/testthat/fixtures/data_arbre_attendu_sto.rds")
saveRDS(parametre_ht_attendu_sto, "tests/testthat/fixtures/parametre_ht_attendu_sto.rds")


# fichier attendu pour le volume en mode stochastique
# une seule ligne par essence est suffisant
param_vol_attendu_sto <- param_vol(fic_arbres=data_arbre_vol, mode_simul = 'STO', nb_iter = 1, seed_value=20)
saveRDS(param_vol_attendu_sto, "tests/testthat/fixtures/param_vol_attendu_sto.rds")
data_arbre_vol_attendu_sto <- cubage(fic_arbres=data_arbre_vol, mode_simul='STO', iteration=1, parametre_vol=param_vol_attendu_sto)
saveRDS(data_arbre_vol_attendu_sto, "tests/testthat/fixtures/data_arbre_vol_attendu_sto.rds")







# fichier des resultats de simulation avec la version SAS de natura 3.0
pred_sas <- read.sas7bdat("U:\\Projets\\IsabelleAuger\\Natura-2020\\SimulateurNaturaSAS\\Donnees\\simulpep_nat20122_nat2014_v2.sas7bdat")
pred_sas2 <- pred_sas %>% dplyr::select(-contains("2014"), -contains("iqs"), -dom_bio, -reg_eco, -placette, -contains("vtige"), -contains("rtx"),-type_eco, -veg_pot, -sdom_bio, -contains("fi"), -pct_max0, -ess_dom0) %>%
  rename(id_pe=placette_new) %>%
  filter(pas<=10)

# fichier des pep au temps 0, sélectionner les placettes avec apresc et apresp =0 au temps 0
pep_temps0 <- read.sas7bdat("U:\\Projets\\IsabelleAuger\\Natura-2020\\SimulateurNaturaSAS\\Donnees\\depart_simul_pep_20230216.sas7bdat")
pep_temps0a <- pep_temps0 %>%
  dplyr::select(placette_new, sdom_bio_orig, type_eco_orig, origine, sttot0, sand, clay, ph, cec, oc, temp_gs, prec_gs, contains("iqs"),
                                            apresc, coupe, tbe, apresp, pert) %>%
  dplyr::select(-contains("cl_")) %>%
  filter(apresc==0, apresp==0, coupe==0, pert==0, tbe==0) %>%
  dplyr::select(-apresc, -coupe, -tbe, -apresp, -pert) %>%
  rename(sdom_bio=sdom_bio_orig, type_eco=type_eco_orig, id_pe=placette_new,
         iqs_pot_sab=iqs_sab, iqs_pot_epn=iqs_epn, iqs_pot_epb=iqs_epx, iqs_pot_pig=iqs_ri,
         iqs_pot_bop=iqs_bop, iqs_pot_pex=iqs_peu, iqs_pot_pib=iqs_ft, iqs_pot_tho=iqs_rt)

pred_sas_atendu <- inner_join(pep_temps0a,pred_sas2, by=c("id_pe","origine")) %>%
  dplyr::select(-sand, -clay, -ph, -cec, -oc, -temp_gs, -prec_gs, -contains("iqs"), -sttot0) %>%
  mutate(tbe=0, pert=0) %>%
  mutate(annee = (pas-1)*10) %>%
  dplyr::select(-pas) %>%
  filter(!(id_pe %in% c("7501301301_N_1970", "7501301302_N_1970", "9203003501_N_1945","8909902201_N_1933","8909900202_N_1960","9701100602_N_1934","8909902101_N_1934","8909902102_N_1938","9700700101_N_1952"))) %>%
  rename(nsab=nsabha, nepn=nepnha, nepx=nepxha, nrt=nrtha, nri=nriha, nbop=nbopha, npeu=npeuha, nft=nftha, ntot=ntotha)
  # on enlève les 8 placettes dont nrt/nepx dépasse le seuil, car dans SAS il n'y a pas de seuil max

pep_depart <- inner_join(pep_temps0a,pred_sas2, by=c("id_pe","origine")) %>% filter(pas==1) %>%
  dplyr::select(-sttot0, -pas, -contains("dq"), -contains("pct_"), -contains("tot")) %>%
  filter(!(id_pe %in% c("7501301301_N_1970", "7501301302_N_1970", "9203003501_N_1945","8909902201_N_1933","8909900202_N_1960","9701100602_N_1934","8909902101_N_1934","8909902102_N_1938","9700700101_N_1952"))) %>%
  rename(nsab=nsabha, nepn=nepnha, nepx=nepxha, nrt=nrtha, nri=nriha, nbop=nbopha, npeu=npeuha, nft=nftha)

saveRDS(pred_sas_atendu, "tests/testthat/fixtures/pred_sas_atendu.rds")
saveRDS(pep_depart, "tests/testthat/fixtures/pep_depart.rds")



# fichier de resultats de simulation pour tester la fct graphique
data_simul <- SimulNatura(file_compile=fichier_compile_aveccov, horizon=5, iqs=FALSE, sol=FALSE, climat=FALSE)
saveRDS(data_simul, "tests/testthat/fixtures/data_simul.rds")

# fichier de resultats de simulation pour tester la fct graphique
data_simul_sto <- SimulNatura(file_compile=fichier_compile_aveccov, horizon=5, iqs=FALSE, sol=FALSE, climat=FALSE, mode_simul = 'STO', nb_iter = 30)
saveRDS(data_simul_sto, "tests/testthat/fixtures/data_simul_sto.rds")
