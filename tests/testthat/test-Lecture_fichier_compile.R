# fct 18: Lecture_compile(): importer le fichier compile à la placette.
#                            vérification si on n'a mis les bons noms de variables.
#                            Calcul des totaux et des pct.


test_that("La fonction Lecture_compile() fonctionne tel qu'attendu avec iqs=T, climat=T, sol=T", {

  # retourne un dataframe si toutes les variables sont correctes
  fic1 = fichier_compile_sanscov
  compile = Lecture_compile(file=fic1, iqs=T, climat=T, sol=T)
  expect_equal(is.data.frame(compile),T)

  # nom_retourne = names(compile)
  # # altitude et reg_eco ne sont plus là, lat-long ont été conservés car iqs/sol/climat à extraire, et nouvelles variables
  # coord <- c('latitude','longitude')
  # retrait <- c('reg_eco','altitude')
  # new <- c( "nbop1", "npeu1", "nft1", "nepn1", "nepx1", "nsab1", "nri1", "nrt1",
  #           "stbop1", "stpeu1", "stft1", "stepn1", "stepx1", "stsab1", "stri1", "strt1",
  #           "vbop1", "vpeu1", "vft1", "vepn1", "vepx1", "vsab1", "vri1", "vrt1",
  #           "hd1", "is1", "annee", "pert","tbe","ntot1","sttot1","vtot1",
  #           "pct_bop1", "pct_peu1", "pct_ft1", "pct_epn1", "pct_epx1", "pct_sab1", "pct_ri1", "vrt1")
  # expect_equal(coord %in% nom_retourne,c(T,T))
  # expect_equal(retrait %in% nom_retourne,c(F,F))
  # expect_equal(new %in% nom_retourne,rep(T,length(new)))

  # variable de base manquante
  fic2 <- fic1 %>% dplyr::select(-nbop)
  compile = Lecture_compile(file=fic2, iqs=T, climat=T, sol=T)
  expect_equal(compile,"Nom des variables de base incorrect dans le fichier d'inventaire compile")

  # variable de base manquante
  fic2 <- fic1 %>% dplyr::select(-id_pe)
  compile = Lecture_compile(file=fic2, iqs=T, climat=T, sol=T)
  expect_equal(compile,"Nom des variables de base incorrect dans le fichier d'inventaire compile")


})

test_that("La fonction Lecture_compile() fonctionne tel qu'attendu avec iqs=F, climat=T, sol=T", {

  fic1 = fichier_compile_sanscov
  compile = Lecture_compile(file=fic1, iqs=F, climat=T, sol=T)
  expect_equal(compile,"Nom des variables d'iqs incorrect dans le fichier d'inventaire compile")

  fic1$iqs_pot_epn = 10
  fic1$iqs_pot_epb = 10
  fic1$iqs_pot_pig = 10
  fic1$iqs_pot_pib = 10
  fic1$iqs_pot_bop = 10
  fic1$iqs_pot_pex = 10
  fic1$iqs_pot_sab = 10
  fic1$iqs_pot_tho = 10
  compile2 = Lecture_compile(file=fic1, iqs=F, climat=T, sol=T)
  expect_equal(is.data.frame(compile2),T)

})

test_that("La fonction Lecture_compile() fonctionne tel qu'attendu avec iqs=T, climat=F, sol=T", {

  fic1 = fichier_compile_sanscov
  compile = Lecture_compile(file=fic1, iqs=T, climat=F, sol=T)
  expect_equal(compile,"Nom des variables climatiques annuelles incorrect dans le fichier d'inventaire compile")

  fic1$prec_gs=10
  fic1$temp_gs=10
  compile2 = Lecture_compile(file=fic1, iqs=T, climat=F, sol=T)
  expect_equal(is.data.frame(compile2),T)

})

test_that("La fonction Lecture_compile() fonctionne tel qu'attendu avec iqs=T, climat=T, sol=F", {

  fic1 = fichier_compile_sanscov
  compile = Lecture_compile(file=fic1, iqs=T, climat=t, sol=F)
  expect_equal(compile,"Nom des variables de sol incorrect dans le fichier d'inventaire compile")

  fic1$cec=10
  fic1$ph=5
  fic1$oc=10
  fic1$sand=10
  fic1$clay=10
  compile2 = Lecture_compile(file=fic1, iqs=T, climat=T, sol=F)
  expect_equal(is.data.frame(compile2),T)

})



test_that("La fonction Lecture_compile fonctionne tel qu'attendu avec iqs=F, climat=F, sol=T ", {

  # si on extrait sol, il faut lat/long
  fic1 = fichier_compile_sanscov
  fic2 <- fic1 %>% dplyr::select(-latitude)
  fic2$iqs_pot_epn = 10
  fic2$iqs_pot_epb = 10
  fic2$iqs_pot_pig = 10
  fic2$iqs_pot_pib = 10
  fic2$iqs_pot_bop = 10
  fic2$iqs_pot_pex = 10
  fic2$iqs_pot_sab = 10
  fic2$iqs_pot_tho = 10
  fic2$prec_gs=10
  fic2$temp_gs=10
  compile = Lecture_compile(file=fic2, iqs=F, climat=F, sol=T)
  expect_equal(compile,"Coordonnées des placettes manquantes pour extraire iqs/sol")


  fic1 = fichier_compile_sanscov
  fic1$iqs_pot_epn = 10
  fic1$iqs_pot_epb = 10
  fic1$iqs_pot_pig = 10
  fic1$iqs_pot_pib = 10
  fic1$iqs_pot_bop = 10
  fic1$iqs_pot_pex = 10
  fic1$iqs_pot_sab = 10
  fic1$iqs_pot_tho = 10
  fic1$prec_gs=10
  fic1$temp_gs=10
  compile = Lecture_compile(file=fic1, iqs=F, climat=F, sol=T)
  expect_equal(is.data.frame(compile),T)

})


test_that("La fonction Lecture_compile fonctionne tel qu'attendu avec ht=F, vol=T, iqs=T, climat=F, sol=F ", {

  # si on extrait iqs, il faut lat/long
  fic1 = fichier_compile_sanscov
  fic2 <- fic1 %>% dplyr::select(-latitude)
  fic2$cec=10
  fic2$ph=5
  fic2$oc=10
  fic2$sand=10
  fic2$clay=10
  fic2$prec_gs=10
  fic2$temp_gs=10
  compile = Lecture_compile(file=fic2, iqs=T, climat=F, sol=F)
  expect_equal(compile,"Coordonnées des placettes manquantes pour extraire iqs/sol")


  fic2 = fichier_compile_sanscov
  fic2$cec=10
  fic2$ph=5
  fic2$oc=10
  fic2$sand=10
  fic2$clay=10
  fic2$prec_gs=10
  fic2$temp_gs=10
  compile = Lecture_compile(file=fic2, iqs=T, climat=F, sol=F)
  expect_equal(is.data.frame(compile),T)

})


test_that("La fonction Lecture_compile fonctionne tel qu'attendu avec un contenu des variables hors limite ", {


  fic1 <- fichier_compile_aveccov %>% dplyr::select(-iqs_pot_BOP) %>% mutate(iqs_pot_bop=45)
  compile = Lecture_compile(file=fic1, iqs=F, climat=F, sol=F)
  expect_equal(compile,"iqs_pot_bop à l'extérieur de la plage des valeurs possibles (>7 et <21 m)")

})

