# 3 types de fichier à valider le contenu:
# arbres
# compil: lat, long, prec_gs, temp_gs, an_mes, type_eco, origine, sdom, oc, clay, sand, ph, cec, iqs, hd, is, n, st, v
# valid: ntot, vtot, sttot


test_that("La fonction valid_placette() fonctionne comme attendu pour le fichier compil avec des erreurs, avec iqs=F, sol=F, climat=F", {

  compile = data.frame(prec_gs=c(1800, rep(1000,44)),
                       temp_gs=c(1, 21, rep(1,43)),
                       type_eco=c(rep('MS22',2),'FE32', rep('MS22',42)),
                       origine=c(rep('BR',3),'CH', rep('BR', 41)),
                       sdom_bio=c(rep('2O',4),'7E', rep('2O',40)),
                       temps=c(rep(50,5),8, rep(50,39)),
                       oc=c(rep(10,6),50, rep(10,38)),
                       clay=c(rep(50,7),100, rep(50,37)),
                       sand=c(rep(50,8),100, rep(50,36)),
                       ph=c(rep(5,9),9, rep(5,35)),
                       cec=c(rep(12,10),50, rep(12,34)),
                       iqs_pot_epn=c(rep(15,11),30, rep(15,33)),
                       iqs_pot_epb=c(rep(15,12),30, rep(15,32)),
                       iqs_pot_pig=c(rep(15,13),30, rep(15,31)),
                       iqs_pot_tho=c(rep(15,14),30, rep(15,30)),
                       iqs_pot_pib=c(rep(15,15),30, rep(15,29)),
                       iqs_pot_sab=c(rep(15,16),30, rep(15,28)),
                       iqs_pot_bop=c(rep(15,17),30, rep(15,27)),
                       iqs_pot_pex=c(rep(15,18),30, rep(15,26)),
                       hd=c(rep(15,19),50, rep(15,25)),
                       is=c(rep(0.5,20),1.5, rep(0.5,24)),
                       nsab=c(rep(1000,21),5000, rep(1000,23)),
                       nepn=c(rep(1000,22),5000, rep(1000,22)),
                       nepx=c(rep(1000,23),5000, rep(1000,21)),
                       nri=c(rep(1000,24),5000, rep(1000,20)),
                       nrt=c(rep(1000,25),5000, rep(1000,19)),
                       nbop=c(rep(1000,26),5000, rep(1000,18)),
                       npeu=c(rep(1000,27),5000, rep(1000,17)),
                       nft=c(rep(1000,28),5000, rep(1000,16)),
                       stsab=c(rep(10,29),70, rep(10,15)),
                       stepn=c(rep(10,30),70, rep(10,14)),
                       stepx=c(rep(10,31),70, rep(10,13)),
                       stri=c(rep(10,32),70, rep(10,12)),
                       strt=c(rep(10,33),70, rep(10,11)),
                       stbop=c(rep(10,34),70, rep(10,10)),
                       stpeu=c(rep(10,35),70, rep(10,9)),
                       stft=c(rep(10,36),70, rep(10,8)),
                       vsab=c(rep(10,37),600, rep(10,7)),
                       vepn=c(rep(10,38),600, rep(10,6)),
                       vepx=c(rep(10,39),600, rep(10,5)),
                       vri=c(rep(10,40),600, rep(10,4)),
                       vrt=c(rep(10,41),600, rep(10,3)),
                       vbop=c(rep(10,42),600, rep(10,2)),
                       vpeu=c(rep(10,43),600, rep(10,1)),
                       vft=c(rep(10,44),600))

  compile <- compile %>% mutate(id_pe = row_number())

  # il devrait avoir 45 messages d'erreurs
  verif <- valid_placette(type_fic='compile', fichier=compile, sol=F, iqs=F, climat=F)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,45)
  expect_equal(nb_filtre,0)

  #verif_rejet$message

})


test_that("La fonction valid_placette() fonctionne comme attendu pour le fichier compil avec des erreurs, avec iqs=T, sol=T, climat=T", {

  compile = data.frame(latitude=c(43, rep(46,32)),
                       longitude=c(-67, -80, rep(-67,31)),
                       an_mes=c(rep(1995,2), 1980, rep(1995,30)),
                       type_eco=c(rep('MS22',33)),
                       origine=c(rep('BR',33)),
                       sdom_bio=c(rep('2O',33)),
                       temps=c(rep(50,33)),
                       hd=c(rep(15,33)),
                       is=c(rep(0.5,33)),
                       nsab=c(rep(1000,33)),
                       nepn=c(rep(1000,33)),
                       nepx=c(rep(1000,33)),
                       nri=c(rep(1000,33)),
                       nrt=c(rep(1000,33)),
                       nbop=c(rep(1000,33)),
                       npeu=c(rep(1000,33)),
                       nft=c(rep(1000,33)),
                       stsab=c(rep(10,33)),
                       stepn=c(rep(10,33)),
                       stepx=c(rep(10,33)),
                       stri=c(rep(10,33)),
                       strt=c(rep(10,33)),
                       stbop=c(rep(10,33)),
                       stpeu=c(rep(10,33)),
                       stft=c(rep(10,33)),
                       vsab=c(rep(10,33)),
                       vepn=c(rep(10,33)),
                       vepx=c(rep(10,33)),
                       vri=c(rep(10,33)),
                       vrt=c(rep(10,33)),
                       vbop=c(rep(10,33)),
                       vpeu=c(rep(10,33)),
                       vft=c(rep(10,33)))

  compile <- compile %>% mutate(id_pe = row_number())

  # il devrait avoir 3 messages d'erreurs

  verif <- valid_placette(type_fic='compile', fichier=compile, sol=T, iqs=T, climat=T)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,3)
  expect_equal(nb_filtre,30)

  #verif_rejet$message

})

test_that("La fonction valid_placette() fonctionne comme attendu pour le fichier compil sans erreur avec iqs=T, sol=T, climat=T", {

  compile = data.frame(latitude=c(rep(46,33)),
                       longitude=c(rep(-72,33)),
                       an_mes=c(rep(1995,33)),
                       type_eco=c(rep('MS22',33)),
                       origine=c(rep('BR',33)),
                       sdom_bio=c(rep('2O',33)),
                       temps=c(rep(50,33)),
                       hd=c(rep(15,33)),
                       is=c(rep(0.5,33)),
                       nsab=c(rep(1000,33)),
                       nepn=c(rep(1000,33)),
                       nepx=c(rep(1000,33)),
                       nri=c(rep(1000,33)),
                       nrt=c(rep(1000,33)),
                       nbop=c(rep(1000,33)),
                       npeu=c(rep(1000,33)),
                       nft=c(rep(1000,33)),
                       stsab=c(rep(10,33)),
                       stepn=c(rep(10,33)),
                       stepx=c(rep(10,33)),
                       stri=c(rep(10,33)),
                       strt=c(rep(10,33)),
                       stbop=c(rep(10,33)),
                       stpeu=c(rep(10,33)),
                       stft=c(rep(10,33)),
                       vsab=c(rep(10,33)),
                       vepn=c(rep(10,33)),
                       vepx=c(rep(10,33)),
                       vri=c(rep(10,33)),
                       vrt=c(rep(10,33)),
                       vbop=c(rep(10,33)),
                       vpeu=c(rep(10,33)),
                       vft=c(rep(10,33)))

  compile <- compile %>% mutate(id_pe = row_number())

  verif <- valid_placette(type_fic='compile', fichier=compile, sol=T, iqs=T, climat=T)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,NULL)
  expect_equal(nb_filtre,33)



})


test_that("La fonction valid_placette() fonctionne comme attendu pour le fichier compil sans erreur avec iqs=F, sol=F, climat=F", {

  compile = data.frame(latitude=c(rep(46,33)),
                       longitude=c(rep(-67,33)),
                       an_mes=c(rep(1995,33)),
                       type_eco=c(rep('MS22',33)),
                       origine=c(rep('BR',33)),
                       sdom_bio=c(rep('2O',33)),
                       temps=c(rep(50,33)),
                       prec_gs=c(rep(1000,33)),
                       temp_gs=c(rep(5,33)),
                       oc=c(rep(10,33)),
                       clay=c(rep(50,33)),
                       sand=c(rep(50,33)),
                       ph=c(rep(5,33)),
                       cec=c(rep(12,33)),
                       iqs_pot_epn=c(rep(11,33)),
                       iqs_pot_epb=c(rep(11,33)),
                       iqs_pot_pig=c(rep(11,33)),
                       iqs_pot_tho=c(rep(11,33)),
                       iqs_pot_pib=c(rep(11,33)),
                       iqs_pot_sab=c(rep(11,33)),
                       iqs_pot_bop=c(rep(11,33)),
                       iqs_pot_pex=c(rep(11,33)),
                       hd=c(rep(15,33)),
                       is=c(rep(0.5,33)),
                       nsab=c(rep(1000,33)),
                       nepn=c(rep(1000,33)),
                       nepx=c(rep(1000,33)),
                       nri=c(rep(1000,33)),
                       nrt=c(rep(1000,33)),
                       nbop=c(rep(1000,33)),
                       npeu=c(rep(1000,33)),
                       nft=c(rep(1000,33)),
                       stsab=c(rep(10,33)),
                       stepn=c(rep(10,33)),
                       stepx=c(rep(10,33)),
                       stri=c(rep(10,33)),
                       strt=c(rep(10,33)),
                       stbop=c(rep(10,33)),
                       stpeu=c(rep(10,33)),
                       stft=c(rep(10,33)),
                       vsab=c(rep(10,33)),
                       vepn=c(rep(10,33)),
                       vepx=c(rep(10,33)),
                       vri=c(rep(10,33)),
                       vrt=c(rep(10,33)),
                       vbop=c(rep(10,33)),
                       vpeu=c(rep(10,33)),
                       vft=c(rep(10,33)))
  compile <- compile %>% mutate(id_pe = row_number())


  verif <- valid_placette(type_fic='compile', fichier=compile, sol=F, iqs=F, climat=F)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,NULL)
  expect_equal(nb_filtre,33)


})




test_that("La fonction valid_placette() fonctionne comme attendu pour le fichier arbres sans erreur avec ht=F, climat=F, iqs=F, sol=F", {

    arbre = data.frame(etat=c('10',rep('10',22)),
                       tige_ha=c(25, 25, rep(25,21)),
                       essence=c(rep('SAB',23)),
                       dhpcm=c(rep(12,23)),
                       type_eco=c(rep('MS22',23)),
                       origine=c(rep('BR', 23)),
                       sdom_bio=c(rep('2O',23)),
                       temps=c(rep(50,23)),
                       prec_gs=c(rep(1000,23)),
                       temp_gs=c(rep(5,23)),
                       oc=c(rep(10,23)),
                       clay=c(rep(50,23)),
                       sand=c(rep(50,23)),
                       ph=c(rep(5,23)),
                       cec=c(rep(12,23)),
                       iqs_pot_epn=c(rep(11,23)),
                       iqs_pot_epb=c(rep(11,23)),
                       iqs_pot_pig=c(rep(11,23)),
                       iqs_pot_tho=c(rep(11,23)),
                       iqs_pot_pib=c(rep(11,23)),
                       iqs_pot_sab=c(rep(11,23)),
                       iqs_pot_bop=c(rep(11,23)),
                       iqs_pot_pex=c(rep(11,23)))
    arbre <- arbre %>% mutate(id_pe = row_number())

  verif <- valid_placette(type_fic='arbres', fichier=arbre, ht=F, sol=F, iqs=F, climat=F)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,NULL)
  expect_equal(nb_filtre,23)

})




test_that("La fonction valid_placette() fonctionne comme attendu pour le fichier arbres avec erreur avec ht=T, climat=T, iqs=T, sol=T", {

  arbre = data.frame(latitude=c(43, rep(46,11)),
                     longitude=c(-67, -80, rep(-67,10)),
                     an_mes=c(rep(1995,2), 1980, rep(1995,9)),
                     altitude=c(rep(100,3),1500,rep(100,8)),
                     etat=c(rep('10',12)),
                     tige_ha=c(rep(25,12)),
                     essence=c(rep('SAB',12)),
                     dhpcm=c(rep(12,12)),
                     type_eco=c(rep('MS22',12)),
                     origine=c(rep('BR', 12)),
                     sdom_bio=c(rep('2O',12)),
                     temps=c(rep(50,12))
                     )
  arbre <- arbre %>% mutate(id_pe = row_number())

  verif <- valid_placette(type_fic='arbres', fichier=arbre, ht=T, sol=T, iqs=T, climat=T)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,4)
  expect_equal(nb_filtre,8)

})


test_that("La fonction valid_placette() fonctionne comme attendu pour le fichier arbres sans erreur avec ht=T, climat=F, iqs=T, sol=T", {

  arbre = data.frame(altitude=c(rep(100,15)),
                     p_tot=c(rep(1000,15)),
                     t_ma=c(rep(0,15)),
                     prec_gs=c(rep(800,15)),
                     temp_gs=c(rep(12,15)),
                     latitude=c(rep(48,15)),
                     longitude=c(rep(-67,15)),
                     etat=c(rep('10',15)),
                     tige_ha=c(rep(25,15)),
                     essence=c(rep('SAB',15)),
                     dhpcm=c(rep(12,15)),
                     type_eco=c(rep('MS22',15)),
                     origine=c(rep('BR', 15)),
                     sdom_bio=c(rep('2O',15)),
                     temps=c(rep(50,15)))
  arbre <- arbre %>% mutate(id_pe = row_number())

  verif <- valid_placette(type_fic='arbres', fichier=arbre, ht=T, sol=T, iqs=T, climat=F)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,NULL)
  expect_equal(nb_filtre,15)

})


test_that("La fonction valid_placette() fonctionne comme attendu pour le fichier arbres sans erreur avec ht=F, climat=T, iqs=T, sol=T", {

  arbre = data.frame(latitude=c(rep(48,15)),
                     longitude=c(rep(-67,15)),
                     an_mes=c(rep(1995,15)),
                     etat=c(rep('10',15)),
                     tige_ha=c(rep(25,15)),
                     essence=c(rep('SAB',15)),
                     dhpcm=c(rep(12,15)),
                     type_eco=c(rep('MS22',15)),
                     origine=c(rep('BR', 15)),
                     sdom_bio=c(rep('2O',15)),
                     temps=c(rep(50,15)))
  arbre <- arbre %>% mutate(id_pe = row_number())

  verif <- valid_placette(type_fic='arbres', fichier=arbre, ht=F, sol=T, iqs=T, climat=T)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,NULL)
  expect_equal(nb_filtre,15)

})



test_that("La fonction valid_placette() fonctionne comme attendu avec type_fic=valid avec un fichier avec des erreurs", {

  # nxxx1 doit être en 400 m2, donc ne pas dépasser 200
  fic <- data.frame(sdom_bio="4O",
                   nsab1=1000, nepn1=1000, nepx1=1000, nri1=1000, nrt1=1000, nft1=1000, nbop1=1000, npeu1=1000,
                   stsab1=1000, stepn1=1000, stepx1=1000, stri1=1000, strt1=1000, stft1=1000, stbop1=1000, stpeu1=1000,
                   vsab1=1000, vepn1=1000, vepx1=1000, vri1=1000, vrt1=1000, vft1=1000, vbop1=1000, vpeu1=1000)
  fic <- fic %>% mutate(id_pe = row_number())

  verif <- valid_placette(type_fic='valid', fichier=fic)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,3)
  expect_equal(nb_filtre,0)


})

test_that("La fonction valid_placette() fonctionne comme attendu avec type_fic=valid avec un fichier sans erreurs", {

  # nxxx1 doit être en 400 m2, donc ne pas dépasser 200
  fic = data.frame(sdom_bio="4O",
                   nsab1=10, nepn1=10, nepx1=10, nri1=10, nrt1=10, nft1=10, nbop1=10, npeu1=10,
                   stsab1=0, stepn1=0, stepx1=0, stri1=10, strt1=10, stft1=10, stbop1=10, stpeu1=10,
                   vsab1=0, vepn1=0, vepx1=0, vri1=0, vrt1=0, vft1=0, vbop1=0, vpeu1=0)
  fic <- fic %>% mutate(id_pe = row_number())

  verif <- valid_placette(type_fic='valid', fichier=fic)

  verif_filtre <- verif[[1]] # fichier filtré
  verif_rejet <- verif[[2]] # liste des placettes rejetées avec le message

  nb_filtre <- nrow(verif_filtre)
  nb_rejet <- nrow(verif_rejet)

  expect_equal(nb_rejet,NULL)
  expect_equal(nb_filtre,1)


})

test_that("La fonction valid_placette() fonctionne comme attendu avec sdom_bio a 1 caractere", {

  fic = data.frame(sdom_bio="4",
                   nsab1=10, nepn1=10, nepx1=10, nri1=10, nrt1=10, nft1=10, nbop1=10, npeu1=10,
                   stsab1=0, stepn1=0, stepx1=0, stri1=10, strt1=10, stft1=10, stbop1=10, stpeu1=10,
                   vsab1=0, vepn1=0, vepx1=0, vri1=0, vrt1=0, vft1=0, vbop1=0, vpeu1=0)
  fic <- fic %>% mutate(id_pe = row_number())

  verif <- valid_placette(type_fic='valid', fichier=fic)

  verif_filtre <- verif[[1]] # fichier filtré

  expect_equal(verif_filtre$sdom_bio,"4E")


})



