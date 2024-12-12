# 4 types de fichier à valider le contenu:
# arbres
# etudes: essence dhpcm, etage, hauteur: ok
# compil: lat, long, prec_gs, temp_gs, an_mes, type_eco, origine, sdom, oc, clay, sand, ph, cec, iqs, hd, is, n, st, v
# valid: ntot, vtot, sttot


test_that("La fonction valid_arbre() fonctionne comme attendu pour le fichier arbres_etudes avec des erreurs", {

  etude = data.frame(essence=c('ERX','SAB','EPN','BOJ','ERR','ERR'),
                     dhpcm=c(8,201,12,14,16,16),
                     etage=c('C','D','i','O','V','I'),
                     hauteur=c(3,4,5,1,41,12))
  # il devrait avoir 4 messages d'erreurs

  verif <- valid_arbre(type_fic='etudes', fichier=etude)
  #nb <- length(verif)
  #dataframe <- is.data.frame(verif)
  #expect_equal(nb,4)
  #expect_equal(dataframe,FALSE)
  expect_equal(verif,"Code d'essence a l'exterieur de la plage de valeurs possibles, Valeurs de dhpcm non permises (<200 cm), Code d'etage a l'exterieur des valeurs possibles (C, D, I, O, V), hauteur des arbres-etudes a l'exterieur de la plage des valeurs possibles (>2 et <40 m)")

})

test_that("La fonction valid_arbre() fonctionne comme attendu pour le fichier arbres_etudes sans erreurs", {

  etude = data.frame(essence=c('SAB','EPN','BOJ','ERR','ERR'),
                     dhpcm=c(8,12,14,16,16),
                     etage=c('C','D','O','V','I'),
                     hauteur=c(3,4,5,39,12))

  verif <- valid_arbre(type_fic='etudes', fichier=etude)
  nb <- nrow(verif)
  dataframe <- is.data.frame(verif)

  expect_equal(nb,5)
  expect_equal(dataframe,TRUE)

})



test_that("La fonction valid_arbre() fonctionne comme attendu pour le fichier arbres avec erreur", {

    arbre = data.frame(etat=c('21',rep('10',22)),
                       tige_ha=c(25, 0, rep(25,21)),
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



  verif <- valid_arbre(type_fic='arbres', fichier=arbre)
  expect_equal(verif,"Code d'etat a l'exterieur de la plage de valeurs possibles (10, 12, 30, 32, 40, 42, 50, 52), Valeurs de nb_tiges non permises (>0)")
  #nb <- length(verif)
  #dataframe <- is.data.frame(verif)
  #expect_equal(nb,2)
  #expect_equal(dataframe,FALSE)

})

test_that("La fonction valid_arbre() fonctionne comme attendu pour le fichier arbres sans erreur", {

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



  verif <- valid_arbre(type_fic='arbres', fichier=arbre)
  nb <- nrow(verif)
  dataframe <- is.data.frame(verif)

  expect_equal(nb,23)
  expect_equal(dataframe,TRUE)

})
