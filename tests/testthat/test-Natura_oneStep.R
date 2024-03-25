# fct 16: Natura_oneStep(): applique les équations au fichier des placettes pour estimer les valeurs du pas suivant. À tester

# si on est à la décennie de la perturbation, mettre la variable indicatrice de perturb à 1
# si on est à la décennie de la tbe , mettre la variable tbe à la valeur desiree (tbe1 et tbe2)
# s'il y a eu de la TBE ou de la perturb à la décennie précédente, on met la variable indicatrice apresp à 1

# vérifier les valeursa de dt = long_int,
       #annee = pas*long_int,
       #temps1 = temps,
       #temps = temps+long_int,
       #tbe = ind_tbe, pert = ind_pert, apresc = ind_apresc, apresp = ind_apresp)

test_that("La fonction Natura_oneStep() produit les bons estimés", {


  liste_plot = data.frame(id_pe=1)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  #param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  #param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot,  mode_simul='DET', horizon=5)
  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, nb_iter=1, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  #liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
  #data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
  #param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=1, pas=2, mode_simul='DET', param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)
  Data_param_iter <- Prep_parametre_iter(iteration=1, param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)


  data_info = data.frame(id_pe=1, sdom_bio='5E', altitude=100, p_tot=1000, t_ma=0.2, prec_gs=500, temp_gs=10,
                         iqs_sab=10, iqs_epn=10, iqs_epx=10, iqs_ri=10, iqs_rt=10, iqs_bop=10, iqs_peu=10, iqs_ft=10,
                         origine='BR', origbr=1, origct=0, origes=0,
                         type_eco='MS22', veg_pot='MS2', vpms2=1, vpms6=0, vprb_=0, vpre1=0, vpre2=0, vpre3=0, vprp1=0, vprs1=0, vprs2=0, vprs3=0,
                         milieu=2, mp0=0, mp1=0, mp2=1, mp3=0, mp4=0, mp5=0, mp6=0, mp789=0,
                         cec=5, oc=10, ph=5, sand=30, silt=30, clay=40)

 data = data.frame(id_pe=1, annee=10, temps=50, tbe=0, pert=0, hd1=10, is1=0.8,
                  stbop1=5, stpeu1=2, stft1=1, stri1=1, strt1=3, stsab1=6, stepn1=4, stepx1=1, sttot1=23,
                  nbop1=5, npeu1=2, nft1=1, nri1=1, nrt1=3, nsab1=6, nepn1=4, nepx1=1, ntot1=23,
                  vbop1=10, vpeu1=20, vft1=10, vri1=10, vrt1=30, vsab1=60, vepn1=40, vepx1=10, vtot1=230,
                  pct_bop1=21.7, pct_peu1=8.7, pct_ft1=4.3, pct_ri1=4.3, pct_rt1=13.0, pct_sab1=26.1, pct_epn1=17.4, pct_epx1=4.3)

 data_temp <-data %>% dplyr::select(id_pe, contains('pct_')) %>% ungroup()
 Data_param_pas <- Prep_parametre_pas(data=data_temp , data_info=data_info, iteration=1, mode_simul='DET', pas=1,
                                      param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)
 # ajouter les parametres qui ne changent pas à chaque pas
 Data_param <- inner_join(Data_param_pas, Data_param_iter, by=c("ess_dom"))

 pred = Natura_oneStep(data=data, param=Data_param, data_info=data_info, long_int=10, pas=2, dec_perturb=2, dec_tbe1=2, tbe1=5, dec_tbe2=4, tbe2=3)

  # pred_attendu <- data.frame(
  #                   nsab1=3.251244, nrt1=2.657035, nepn1=3.767351, nepx1=1.518501, nri1=1.53171, nbop1=6.072053, npeu1=2.997049, nft1=2.552797,
  #                   stsab1=2.812778, strt1=3.584193, stepn1=3.793893, stepx1=1.313454, stri1=1.501396, stbop1=6.187241, stpeu1=3.449232, stft1=2.190103,
  #                   hd1=11.57371, is1=0.7891384,
  #                   vsab1=14.77059, vrt1=27.04942, vepn1=25.92774, vepx1=8.450447, vri1=9.23318, vbop1=31.25119, vpeu1=25.18803, vft1=11.14384,
  #                   ntot1=24.34774, sttot1=24.83229, vtot1=153.0145,
  #                   pct_epn1=15.27806, pct_epx1=5.289299, pct_sab1=11.3271, pct_ft1=8.819576, pct_bop1=24.91611, pct_peu1=13.89011, pct_ri1=6.046144, pct_rt1=14.4336)
 pred_attendu <- data.frame(
   nsab1=3.251244, nrt1=2.657035, nepn1=3.767351, nepx1=1.518501, nri1=1.53171, nbop1=6.072053, npeu1=2.997049, nft1=2.552797,
   stsab1=2.812778, strt1=3.584193, stepn1=3.793893, stepx1=1.313454, stri1=1.501396, stbop1=6.187241, stpeu1=3.449232, stft1=2.190103,
   hd1=10.9792, is1=0.7229,
   vsab1=14.6039, vrt1=26.8105, vepn1=25.6837, vepx1=8.4127, vri1=9.1384, vbop1=30.9655, vpeu1=25.0659, vft1=11.0678,
   ntot1=24.34774, sttot1=24.83229, vtot1=151.7482,
   pct_epn1=15.27806, pct_epx1=5.289299, pct_sab1=11.3271, pct_ft1=8.819576, pct_bop1=24.91611, pct_peu1=13.89011, pct_ri1=6.046144, pct_rt1=14.4336)



  expect_equal(pred$annee, data$annee+10)
  expect_equal(pred$temps, data$temps+10)
  expect_equal(pred$tbe, 5)
  expect_equal(pred$pert, 1)
  expect_equal(round(pred[,6:42],4), round(pred_attendu,4))


})


test_that("La fonction Natura_oneStep() applique tbe2 comme il faut", {


  liste_plot = data.frame(id_pe=1)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  #param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  #param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot,  mode_simul='DET', horizon=5)
  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, nb_iter=1, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  #liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
  #data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
  #param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=1, pas=4, mode_simul='DET', param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)
  Data_param_iter <- Prep_parametre_iter(iteration=1, param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)




  data_info = data.frame(id_pe=1, sdom_bio='5E', altitude=100, p_tot=1000, t_ma=0.2, prec_gs=500, temp_gs=10,
                         iqs_sab=10, iqs_epn=10, iqs_epx=10, iqs_ri=10, iqs_rt=10, iqs_bop=10, iqs_peu=10, iqs_ft=10,
                         origine='BR', origbr=1, origct=0, origes=0,
                         type_eco='MS22', veg_pot='MS2', vpms2=1, vpms6=0, vprb_=0, vpre1=0, vpre2=0, vpre3=0, vprp1=0, vprs1=0, vprs2=0, vprs3=0,
                         milieu=2, mp0=0, mp1=0, mp2=1, mp3=0, mp4=0, mp5=0, mp6=0, mp789=0,
                         cec=5, oc=10, ph=5, sand=30, silt=30, clay=40)

  data = data.frame(id_pe=1, annee=30, temps=50, tbe=0, pert=0, hd1=10, is1=0.8,
                    stbop1=5, stpeu1=2, stft1=1, stri1=1, strt1=3, stsab1=6, stepn1=4, stepx1=1, sttot1=23,
                    nbop1=5, npeu1=2, nft1=1, nri1=1, nrt1=3, nsab1=6, nepn1=4, nepx1=1, ntot1=23,
                    vbop1=10, vpeu1=20, vft1=10, vri1=10, vrt1=30, vsab1=60, vepn1=40, vepx1=10, vtot1=230,
                    pct_bop1=21.7, pct_peu1=8.7, pct_ft1=4.3, pct_ri1=4.3, pct_rt1=13.0, pct_sab1=26.1, pct_epn1=17.4, pct_epx1=4.3)


  data_temp <-data %>% dplyr::select(id_pe, contains('pct_')) %>% ungroup()
  Data_param_pas <- Prep_parametre_pas(data=data_temp , data_info=data_info, iteration=1, mode_simul='DET', pas=1,
                                       param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)
  # ajouter les parametres qui ne changent pas à chaque pas
  Data_param <- inner_join(Data_param_pas, Data_param_iter, by=c("ess_dom"))

  pred = Natura_oneStep(data=data, param=Data_param, data_info=data_info, long_int=10, pas=4, dec_perturb=2, dec_tbe1=2, tbe1=4, dec_tbe2=4, tbe2=3)

  expect_equal(pred$tbe, 3)
  expect_equal(pred$pert, 0)

})


test_that("La fonction Natura_oneStep() fonctionne quand on n'utilise pas tbe1, tbe2 et dec_perturb, ", {


  liste_plot = data.frame(id_pe=1)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  #param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  #param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot,  mode_simul='DET', horizon=5)
  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, nb_iter=1, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  #liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
  #data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
  #param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=1, pas=2, mode_simul='DET', param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)
  Data_param_iter <- Prep_parametre_iter(iteration=1, param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)

  data_info = data.frame(id_pe=1, sdom_bio='5E', altitude=100, p_tot=1000, t_ma=0.2, prec_gs=500, temp_gs=10,
                         iqs_sab=10, iqs_epn=10, iqs_epx=10, iqs_ri=10, iqs_rt=10, iqs_bop=10, iqs_peu=10, iqs_ft=10,
                         origine='BR', origbr=1, origct=0, origes=0,
                         type_eco='MS22', veg_pot='MS2', vpms2=1, vpms6=0, vprb_=0, vpre1=0, vpre2=0, vpre3=0, vprp1=0, vprs1=0, vprs2=0, vprs3=0,
                         milieu=2, mp0=0, mp1=0, mp2=1, mp3=0, mp4=0, mp5=0, mp6=0, mp789=0,
                         cec=5, oc=10, ph=5, sand=30, silt=30, clay=40)

  data = data.frame(id_pe=1, annee=10, temps=50, tbe=0, pert=0, hd1=10, is1=0.8,
                    stbop1=5, stpeu1=2, stft1=1, stri1=1, strt1=3, stsab1=6, stepn1=4, stepx1=1, sttot1=23,
                    nbop1=5, npeu1=2, nft1=1, nri1=1, nrt1=3, nsab1=6, nepn1=4, nepx1=1, ntot1=23,
                    vbop1=10, vpeu1=20, vft1=10, vri1=10, vrt1=30, vsab1=60, vepn1=40, vepx1=10, vtot1=230,
                    pct_bop1=21.7, pct_peu1=8.7, pct_ft1=4.3, pct_ri1=4.3, pct_rt1=13.0, pct_sab1=26.1, pct_epn1=17.4, pct_epx1=4.3)

  data_temp <-data %>% dplyr::select(id_pe, contains('pct_')) %>% ungroup()
  Data_param_pas <- Prep_parametre_pas(data=data_temp , data_info=data_info, iteration=1, mode_simul='DET', pas=1,
                                       param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)
  # ajouter les parametres qui ne changent pas à chaque pas
  Data_param <- inner_join(Data_param_pas, Data_param_iter, by=c("ess_dom"))

  expect_no_error(Natura_oneStep(data=data, param=Data_param, data_info=data_info, long_int=10, pas=2))


})


