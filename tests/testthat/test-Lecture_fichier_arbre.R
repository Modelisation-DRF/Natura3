# fct 2: Lecture_arbres(): importer le fichier d'arbres et garder les vivant et dhp>9.
#                          vérification si on n'a mis les bons noms de variables

test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=T, vol=T, iqs=T, climat=T, sol=T ", {

  fic1 = fichier_arbres_sanscov[1:10,]
  fic2 = data.frame(essence='EPN', ID_PE='2', TIGE_HA=25, dhpcm=8, ETAT=10, LATITUDE=47, LONGITUDE=-67, SDOM_BIO='4O', REG_ECO='4d', type_eco='MS22', ALTITUDE=100, ORIGINE='BR', pet='4',an_mes=1990, temps=50,id='1')
  fic_tous = bind_rows(fic1,fic2)

  arbre = Lecture_arbres(file=fic_tous, ht=T, vol=T, iqs=T, climat=T, sol=T)

  nbligne = nrow(arbre)
  nom_retourne = names(arbre)
  nom_lu = names(fic_tous)

  # filtrer les dhp et les etats
  expect_equal(nbligne,10)

  # variable de base manquante
  fic2 <- fic1 %>% dplyr::select(-ID_PE)
  arbre = Lecture_arbres(file=fic2, ht=T, vol=T, iqs=T, climat=T, sol=T)
  expect_equal(arbre,"Nom des variables de base incorrect dans le fichier des arbres")

  # retrait de variable non nécessaires: id et reg_eco et pet
  #retrait <- setdiff(tolower(nom_lu),nom_retourne)
  #expect_equal(retrait, c("reg_eco","pet","id"))
})


test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=F, vol=T, iqs=T, climat=T, sol=T ", {

  # il faut fournir lat-long-an_mes pour le climat et hauteur_pred

  # il manque hauteur_pred
  fic1 = fichier_arbres_sanscov[1:10,]
  arbre1 = Lecture_arbres(file=fic1, ht=F, vol=T, iqs=T, climat=T, sol=T)
  expect_equal(arbre1,"Nom de la variable de hauteur incorrect dans le fichier des arbres")

  nom_lu <- names(fic1) # hauteur_pred pas dans le fichier


  # si tous est là, ça retourne un data-frame
  fic1$hauteur_pred = 10
  arbre2 = Lecture_arbres(file=fic1, ht=F, vol=T, iqs=T, climat=T, sol=T)
  expect_equal(is.data.frame(arbre2),T)

  # s'il manque lat, retourne un message
  fic2 <- fic1 %>% dplyr::select(-LATITUDE)
  arbre1 = Lecture_arbres(file=fic2, ht=F, vol=T, iqs=T, climat=T, sol=T)
  expect_equal(arbre1,"Coordonnées des placettes manquantes et année de mesure pour extraire climat")

  # s'il manque an_mes, retourne un message
  fic2 <- fic1 %>% dplyr::select(-an_mes)
  arbre1 = Lecture_arbres(file=fic2, ht=F, vol=T, iqs=T, climat=T, sol=T)
  expect_equal(arbre1,"Coordonnées des placettes manquantes et année de mesure pour extraire climat")

})


test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=T, vol=F, iqs=T, climat=T, sol=T ", {

  # il manque la variable vol_dm3
  fic1 = fichier_arbres_sanscov[1:10,]
  arbre1 = Lecture_arbres(file=fic1, ht=T, vol=F, iqs=T, climat=T, sol=T)
  expect_equal(arbre1,"Nom de la variable du volume incorrect dans le fichier des arbres")

  # si tous est là, ça retourne un data-frame
  fic1$vol_dm3 = 100
  arbre2 = Lecture_arbres(file=fic1, ht=T, vol=F, iqs=T, climat=T, sol=T)
  expect_equal(is.data.frame(arbre2),T)

})

test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=T, vol=T, iqs=F, climat=T, sol=T ", {

  # il manque les variables d'iqs
  fic1 = fichier_arbres_sanscov[1:10,]
  arbre1 = Lecture_arbres(file=fic1, ht=T, vol=T, iqs=F, climat=T, sol=T)
  expect_equal(arbre1,"Nom des variables d'iqs incorrect dans le fichier des arbres")

  # si tous est là, ça retourne un data-frame
  fic1$iqs_pot_epn = 10
  fic1$iqs_pot_epb = 10
  fic1$iqs_pot_pig = 10
  fic1$iqs_pot_pib = 10
  fic1$iqs_pot_bop = 10
  fic1$iqs_pot_pex = 11
  fic1$iqs_pot_sab = 10
  fic1$iqs_pot_tho = 10
  arbre2 = Lecture_arbres(file=fic1, ht=T, vol=T, iqs=F, climat=T, sol=T)
  expect_equal(is.data.frame(arbre2),T)

})



test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=T, vol=T, iqs=T, climat=F, sol=T ", {

  # il faut les 4 variables climatiques si ht est à estimer
  fic1 = fichier_arbres_sanscov[1:10,]
  fic1$prec_gs=10
  fic1$temp_gs=10
  arbre1 = Lecture_arbres(file=fic1, ht=T, vol=T, iqs=T, climat=F, sol=T)
  expect_equal(arbre1,"Nom des variables climatiques incorrect dans le fichier des arbres")

  # pour estimer la hauteur quand climat=F, il faut l'altitude, en plus de p_tot et t_moy
  fic1 = fichier_arbres_sanscov[1:10,]
  fic2 <- fic1 %>% dplyr::select(-ALTITUDE)
  fic2$prec_gs=10
  fic2$temp_gs=10
  fic2$p_tot=10
  fic2$t_ma=10
  arbre1 = Lecture_arbres(file=fic2, ht=T, vol=T, iqs=T, climat=F, sol=T)
  expect_equal(arbre1,"Nom des variables incorrect dans le fichier des arbres pour estimer la hauteur")


  # si tous est là, ça retourne un data-frame
  fic1 = fichier_arbres_sanscov[1:10,]
  fic1$p_tot=10
  fic1$t_ma=0
  fic1$prec_gs=10
  fic1$temp_gs=10
  arbre2 = Lecture_arbres(file=fic1, ht=T, vol=T, iqs=T, climat=F, sol=T)
  expect_equal(is.data.frame(arbre2),T)

})





test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=F, vol=T, iqs=T, climat=F, sol=T ", {

  # il faut 2 variables climatiques si ht n'est pas à estimer
  fic1 = fichier_arbres_sanscov[1:10,]
  arbre1 = Lecture_arbres(file=fic1, ht=F, vol=T, iqs=T, climat=F, sol=T)
  expect_equal(arbre1,"Nom des variables climatiques annuelles incorrect dans le fichier des arbres")

  # si tous est là, ça retourne un data-frame
  fic1$prec_gs=10
  fic1$temp_gs=10
  fic1$hauteur_pred=10
  arbre2 = Lecture_arbres(file=fic1, ht=F, vol=T, iqs=T, climat=F, sol=T)
  expect_equal(is.data.frame(arbre2),T)

})

test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=T, vol=T, iqs=T, climat=T, sol=F ", {

  fic1 = fichier_arbres_sanscov[1:10,]
  arbre1 = Lecture_arbres(file=fic1, ht=T, vol=T, iqs=T, climat=T, sol=F)
  expect_equal(arbre1,"Nom des variables de sol incorrect dans le fichier des arbres")

  fic1$cec=10
  fic1$ph=5
  fic1$oc=10
  fic1$sand=10
  fic1$clay=10
  arbre2 = Lecture_arbres(file=fic1, ht=T, vol=T, iqs=T, climat=T, sol=F)
  expect_equal(is.data.frame(arbre2),T)

})

test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=F, vol=T, iqs=F, climat=F, sol=T ", {

  # si on extrait sol, il faut lat/long
  fic1 = fichier_arbres_sanscov[1:10,]
  fic2 <- fic1 %>% dplyr::select(-LATITUDE)
  fic2$hauteur_pred=10
  fic2$iqs_pot_epn = 10
  fic2$iqs_pot_epb = 10
  fic2$iqs_pot_pig = 10
  fic2$iqs_pot_pib = 10
  fic2$iqs_pot_bop = 10
  fic2$iqs_pot_pex = 11
  fic2$iqs_pot_sab = 10
  fic2$iqs_pot_tho = 10
  fic2$prec_gs=10
  fic2$temp_gs=10
  fic2$hauteur_pred=10
  arbre1 = Lecture_arbres(file=fic2, ht=F, vol=T, iqs=F, climat=F, sol=T)
  expect_equal(arbre1,"Coordonnées des placettes manquantes pour extraire iqs/sol")


  fic2 = fichier_arbres_sanscov[1:10,]
  fic2$hauteur_pred=10
  fic2$iqs_pot_epn = 10
  fic2$iqs_pot_epb = 10
  fic2$iqs_pot_pig = 10
  fic2$iqs_pot_pib = 10
  fic2$iqs_pot_bop = 10
  fic2$iqs_pot_pex = 11
  fic2$iqs_pot_sab = 10
  fic2$iqs_pot_tho = 10
  fic2$prec_gs=10
  fic2$temp_gs=10
  fic2$hauteur_pred=10
  arbre2 = Lecture_arbres(file=fic2, ht=F, vol=T, iqs=F, climat=F, sol=T)
  expect_equal(is.data.frame(arbre2),T)

})


test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=F, vol=T, iqs=T, climat=F, sol=F ", {

  # si on extrait iqs, il faut lat/long
  fic1 = fichier_arbres_sanscov[1:10,]
  fic2 <- fic1 %>% dplyr::select(-LATITUDE)
  fic2$hauteur_pred=10
  fic2$cec=10
  fic2$ph=5
  fic2$oc=10
  fic2$sand=10
  fic2$clay=10
  fic2$prec_gs=10
  fic2$temp_gs=10
  fic2$hauteur_pred=10
  arbre1 = Lecture_arbres(file=fic2, ht=F, vol=T, iqs=T, climat=F, sol=F)
  expect_equal(arbre1,"Coordonnées des placettes manquantes pour extraire iqs/sol")


  fic2 = fichier_arbres_sanscov[1:10,]
  fic2$hauteur_pred=10
  fic2$cec=10
  fic2$ph=5
  fic2$oc=10
  fic2$sand=10
  fic2$clay=10
  fic2$prec_gs=10
  fic2$temp_gs=10
  fic2$hauteur_pred=10
  arbre2 = Lecture_arbres(file=fic2, ht=F, vol=T, iqs=T, climat=F, sol=F)
  expect_equal(is.data.frame(arbre2),T)

})


test_that("La fonction Lecture_arbres fonctionne tel qu'attendu avec ht=T, vol=T, iqs=T, climat=T, sol=T ", {

  # il faut les 4 variables climatiques et altitude, sdom, type_eco si ht est à estimer et climat=T
  fic1 = fichier_arbres_sanscov[1:10,]
  fic2 <- fic1 %>% dplyr::select(-ALTITUDE)
  fic2$prec_gs=10
  fic2$temp_gs=10
  fic2$p_tot=10
  fic2$t_ma=0
  arbre1 = Lecture_arbres(file=fic2, ht=T, vol=T, iqs=T, climat=T, sol=T)
  expect_equal(arbre1,"Nom des variables incorrect dans le fichier des arbres pour estimer la hauteur")


  # si tous est là, ça retourne un data-frame
  fic1$prec_gs=10
  fic1$temp_gs=10
  fic1$p_tot=10
  fic1$t_ma=0
  arbre2 = Lecture_arbres(file=fic1, ht=T, vol=T, iqs=T, climat=F, sol=T)
  expect_equal(is.data.frame(arbre2),T)

})

test_that("La fonction Lecture_arbres fonctionne si altitude absent avec ht=F, vol=T, iqs=T, climat=T, sol=T ", {

  # si on n'a pas besoin d'estimer la hauteur, altitude n'est pas néccesaire
  fic1 = fichier_arbres_sanscov[1:10,]
  fic2 <- fic1 %>% dplyr::select(-ALTITUDE)
  fic2$hauteur_pred=10
  arbre1 = Lecture_arbres(file=fic2, ht=F, vol=T, iqs=T, climat=T, sol=T)
  expect_equal(is.data.frame(arbre1),T)

  # si on fournit altitude mais qu'elle n'est pas nécessaire, elle n'est plus dans le fichier
  #fic1 = fichier_arbres_sanscov[1:10,]
  #fic1$hauteur_pred=10
  #arbre1 = Lecture_arbres(file=fic1, ht=F, vol=T, iqs=T, climat=T, sol=T)
  #names(fic1)
  #nom_obtenu <- names(arbre1)
  #expect_equal("altitude" %in% nom_obtenu,F)

})


test_that("La fonction Lecture_arbres fonctionne comme attendu avec un contenue de variables hors limite ", {

  # si on n'a pas besoin d'estimer la hauteur, altitude n'est pas néccesaire
  fic1 = fichier_arbres_sanscov[1:10,] %>% dplyr::select(-dhpcm) %>% mutate(dhpcm=300)
  fic1$hauteur_pred=10
  arbre1 = Lecture_arbres(file=fic1, ht=F, vol=T, iqs=T, climat=T, sol=T)
  expect_equal(arbre1,"Valeurs de dhpcm non permises (>9.0 et <200 cm)")



})
