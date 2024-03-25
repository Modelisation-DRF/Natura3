
# fct 17: simul_oneIter_arbre(): gère une itération complète (tous les pas de simulation) quand le fichier d'intrant est a l'echelle de l'arbre: À tester
# fct 22: simul_oneIter_compileV2(): gère une itération complète (tous les pas de simulation) quand le fichier d'intrant est a l'echelle de la placette: À tester

#1
test_that("La fonction simul_oneIter_compileV2() fonctionne en mode déterministe", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  data_info = data.frame(id_pe=1, sdom_bio='5E', altitude=100, p_tot=1000, t_ma=0.2, prec_gs=500, temp_gs=10,
                         iqs_sab=10, iqs_epn=10, iqs_epx=10, iqs_ri=10, iqs_rt=10, iqs_bop=10, iqs_peu=10, iqs_ft=10,
                         origine='BR', origBR=1, origCT=0, origES=0,
                         type_eco='MS22', veg_pot='MS2', vpms2=1, vpms6=0, vprb_=0, vpre1=0, vpre2=0, vpre3=0, vprp1=0, vprs1=0, vprs2=0, vprs3=0,
                         milieu=2, mp0=0, mp1=0, mp2=1, mp3=0, mp4=0, mp5=0, mp6=0, mp789=0,
                         cec=5, oc=10, ph=5, sand=30, silt=30, clay=40)


  PlacT0 = data.frame(id_pe=1, annee=0, temps=50, tbe=0, pert=0,
                    hd1=10, is1=0.8,
                    stbop1=5, stpeu1=2, stft1=1, stri1=1, strt1=3, stsab1=6, stepn1=4, stepx1=1, sttot1=23,
                    nbop1=5, npeu1=2, nft1=1, nri1=1, nrt1=3, nsab1=6, nepn1=4, nepx1=1, ntot1=23,
                    vbop1=10, vpeu1=20, vft1=10, vri1=10, vrt1=30, vsab1=60, vepn1=40, vepx1=10, vtot1=230,
                    pct_bop1=21.7, pct_peu1=8.7, pct_ft1=4.3, pct_ri1=4.3, pct_rt1=13.0, pct_sab1=26.1, pct_epn1=17.4, pct_epx1=4.3)

  fichier = bind_cols(data_info, PlacT0[,-1])
  fichier[colnames(data_info)[-1]]<- list(NULL)

  resultat = simul_oneIter_compileV2(fichier=fichier, PlacT0=PlacT0, data_info=data_info,
                                    horizon=5, mode_simul='DET', iteration=1,
                                    param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress,
                                    long_int=10, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)

  nligne=nrow(resultat)
  nvar=ncol(resultat)

  expect_equal(nligne, 6) # horizon+1 pour le point de départ
  expect_equal(nvar, 43) # 42 (nb var de de fichier + 1 pour l'iteration)

  expect_equal(resultat$annee, c(0,10,20,30,40,50))
  expect_equal(resultat$temps, c(50,60,70,80,90,100))
  expect_equal(round(resultat$vsab1,1), c(60, 32.6, 31.3, 30.5, 29.9, 29.5))

})

#2
test_that("La fonction simul_oneIter_compileV2() fonctionne en mode stochastique", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, mode_simul='STO', horizon=5, nb_iter = 5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='STO', horizon=5, liste_ess=liste_gress, nb_iter = 5)

  data_info = data.frame(id_pe=1, sdom_bio='5E', altitude=100, p_tot=1000, t_ma=0.2, prec_gs=500, temp_gs=10,
                         iqs_sab=10, iqs_epn=10, iqs_epx=10, iqs_ri=10, iqs_rt=10, iqs_bop=10, iqs_peu=10, iqs_ft=10,
                         origine='BR', origBR=1, origCT=0, origES=0,
                         type_eco='MS22', veg_pot='MS2', vpms2=1, vpms6=0, vprb_=0, vpre1=0, vpre2=0, vpre3=0, vprp1=0, vprs1=0, vprs2=0, vprs3=0,
                         milieu=2, mp0=0, mp1=0, mp2=1, mp3=0, mp4=0, mp5=0, mp6=0, mp789=0,
                         cec=5, oc=10, ph=5, sand=30, silt=30, clay=40)


  PlacT0 = data.frame(id_pe=1, annee=0, temps=50, tbe=0, pert=0,
                      hd1=10, is1=0.8,
                      stbop1=5, stpeu1=2, stft1=1, stri1=1, strt1=3, stsab1=6, stepn1=4, stepx1=1, sttot1=23,
                      nbop1=5, npeu1=2, nft1=1, nri1=1, nrt1=3, nsab1=6, nepn1=4, nepx1=1, ntot1=23,
                      vbop1=10, vpeu1=20, vft1=10, vri1=10, vrt1=30, vsab1=60, vepn1=40, vepx1=10, vtot1=230,
                      pct_bop1=21.7, pct_peu1=8.7, pct_ft1=4.3, pct_ri1=4.3, pct_rt1=13.0, pct_sab1=26.1, pct_epn1=17.4, pct_epx1=4.3)

  fichier = bind_cols(data_info, PlacT0[,-1])
  fichier[colnames(data_info)[-1]]<- list(NULL)

  resultat = simul_oneIter_compileV2(fichier=fichier, PlacT0=PlacT0, data_info=data_info,
                                    horizon=5, mode_simul='STO', iteration=3,
                                    param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress,
                                    long_int=10, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)

  nligne=nrow(resultat)
  nvar=ncol(resultat)

  expect_equal(nligne, 6) # horizon+1 pour le point de départ
  expect_equal(nvar, 43) # 42 (nb var de de fichier + 1 pour l'iteration)

  expect_equal(resultat$annee, c(0,10,20,30,40,50))
  expect_equal(resultat$temps, c(50,60,70,80,90,100))
  expect_equal(resultat$iter[1], 3)

})



