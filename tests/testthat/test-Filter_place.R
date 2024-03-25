# fct 6: Filtrer_place(): filtre les placettes: vp, sdom, origine, crée les variables binaires associées aux variables catégoriques, pas nécessaire de tester pour l'instant


test_that("La fonction Filtrer_place() fonctionne tel qu'attendu avec un fichier d'arbres", {

  # créer un fichier  de tous les type eco et tous les sdom, tous les origines, avec les 8 iqs
  fic1 <- data.frame(id_pe=1:18,
                        type_eco=c('ME10','MS21','MS42','MS63','RB14','RB25','RB56','RE17','RE28','RE39','RE40','RP11','RS12','RS23','RS34','RS45','RS56','RS77'),
                        sdom_bio=c('1','2E','2O','3E','3O','4E','4O','5E','5O','6E','6O','6O','6O','6O','6O','6O','6O','6O'),
                        origine=c('BR','CT','ES'),
                        temps=50, altitude=200, p_tot=1000, t_ma=2, prec_gs=500, temp_gs=5, cec=10, oc=10, ph=6, sand=30, clay=20,
                        iqs_pot_epn=10, iqs_pot_epb=10, iqs_pot_tho=10, iqs_pot_pig=10, iqs_pot_bop=10, iqs_pot_pex=10, iqs_pot_pib=10, iqs_pot_sab=10,
                        dhpcm=10, tige_ha=25, etat='10', no_arbre=1, essence='SAB',
                     var_bidon=1)


  # ajouter des sdom et vp et milieu et origine non traités
  fic2 <- data.frame(id_pe=19:22,
                     type_eco=c('FE23','MS22','MS22','RS2X'),
                     sdom_bio=c('2E','7E','2E','3O'),
                     origine=c('CT','CT','CH','BR'),
                     temps=50, altitude=200, p_tot=1000, t_ma=2, prec_gs=500, temp_gs=5, cec=10, oc=10, ph=6, sand=30, clay=20,
                     iqs_pot_epn=10, iqs_pot_epb=10, iqs_pot_tho=10, iqs_pot_pig=10, iqs_pot_bop=10, iqs_pot_pex=10, iqs_pot_pib=10, iqs_pot_sab=10,
                     dhpcm=10, tige_ha=25, etat='10', no_arbre=1, essence='SAB',
                     var_bidon=1)

  fic_tous <- bind_rows(fic1, fic2) # 22 lignes, 29 colonnes

  result <- Filtrer_place(fichier=fic_tous) # il doit en rester 18 lignes

  var_crees <- c("veg_pot", "milieu", "annee", "origBR", "origCT", "origES",
                 "vpms2", "vpms6", "vprb_", "vpre1", "vpre2", "vpre3", "vprp1", "vprs1", "vprs2", "vprs3",
                 "mp0","mp1","mp2","mp3","mp4","mp5","mp6","mp789",
                 "iqs_epn", "iqs_epx", "iqs_rt", "iqs_ri", "iqs_bop", "iqs_peu", "iqs_ft", "iqs_sab")


  nlignes=nrow(result)
  nom <- names(result) # 53


  expect_equal(nlignes, nrow(fic_tous)-nrow(fic2))
  #expect_equal(length(setdiff(var_crees,nom)), 0)


})


test_that("La fonction Filtrer_place() fonctionne tel qu'attendu avec un fichier compilé", {

  # créer un fichier  de tous les type eco et tous les sdom, tous les origines, avec les 8 iqs
  fic1 <- data.frame(id_pe=1:18,
                     type_eco=c('ME10','MS21','MS42','MS63','RB14','RB25','RB56','RE17','RE28','RE39','RE40','RP11','RS12','RS23','RS34','RS45','RS56','RS77'),
                     sdom_bio=c('1','2E','2O','3E','3O','4E','4O','5E','5O','6E','6O','6O','6O','6O','6O','6O','6O','6O'),
                     origine=c('BR','CT','ES'),
                     temps=50, altitude=200, p_tot=1000, t_ma=2, prec_gs=500, temp_gs=5, cec=10, oc=10, ph=6, sand=30, clay=20,
                     iqs_pot_epn=10, iqs_pot_epb=10, iqs_pot_tho=10, iqs_pot_pig=10, iqs_pot_bop=10, iqs_pot_pex=10, iqs_pot_pib=10, iqs_pot_sab=10,
                     nbop=25, npeu=25, nft=25, nepn=25, nepx=25, nsab=25, nri=25, nrt=25,
                     stbop=25, stpeu=25, stft=25, stepn=25, stepx=25, stsab=25, stri=25, strt=25,
                     vbop=25, vpeu=25, vft=25, vepn=25,vepx=25, vsab=25, vri=25, vrt=25,
                     hd=15, is=0.5,
                     var_bidon=1)


  # ajouter des sdom et vp et milieu et origine non traités
  fic2 <- data.frame(id_pe=19:22,
                     type_eco=c('FE23','MS22','MS22','RS2X'),
                     sdom_bio=c('2E','7E','2E','3O'),
                     origine=c('CT','CT','CH','BR'),
                     temps=50, altitude=200, p_tot=1000, t_ma=2, prec_gs=500, temp_gs=5, cec=10, oc=10, ph=6, sand=30, clay=20,
                     iqs_pot_epn=10, iqs_pot_epb=10, iqs_pot_tho=10, iqs_pot_pig=10, iqs_pot_bop=10, iqs_pot_pex=10, iqs_pot_pib=10, iqs_pot_sab=10,
                     nbop=25, npeu=25, nft=25, nepn=25, nepx=25, nsab=25, nri=25, nrt=25,
                     stbop=25, stpeu=25, stft=25, stepn=25, stepx=25, stsab=25, stri=25, strt=25,
                     vbop=25, vpeu=25, vft=25, vepn=25,vepx=25, vsab=25, vri=25, vrt=25,
                     hd=15, is=0.5,
                     var_bidon=1)

  fic_tous <- bind_rows(fic1, fic2) # 22 lignes, 50 colonnes

  result <- Filtrer_place(fichier=fic_tous) # il doit en rester 18 lignes

  var_crees <- c("veg_pot", "milieu", "annee", "origBR", "origCT", "origES",
                 "vpms2", "vpms6", "vprb_", "vpre1", "vpre2", "vpre3", "vprp1", "vprs1", "vprs2", "vprs3",
                 "mp0","mp1","mp2","mp3","mp4","mp5","mp6","mp789",
                 "iqs_epn", "iqs_epx", "iqs_rt", "iqs_ri", "iqs_bop", "iqs_peu", "iqs_ft", "iqs_sab")

  nlignes=nrow(result)
  nom <- names(result)

  expect_equal(nlignes, nrow(fic_tous)-nrow(fic2))
  #expect_equal(length(setdiff(var_crees,nom)), 0)


})

