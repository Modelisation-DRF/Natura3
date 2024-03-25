test_that("La fonction prep_compile fonctionne comme attendu", {

  fic <- data.frame(
  iqs_pot_epn=15, iqs_pot_epb=15, iqs_pot_bop=15, iqs_pot_pex=15, iqs_pot_pig=15,
  iqs_pot_sab=15, iqs_pot_pib=15, iqs_pot_tho=15, latitude=47,    longitude=-67,
  altitude=500,    sdom_bio='5O',    reg_eco='5d',     type_eco='MS22',   id_pe=1,
  prec_gs=800,     temp_gs=5,     origine='BR',     temps=15,       stft1=2,
  strt1=2,       stri1=2,       stpeu1=2,      stbop1=2,      stepn1=2,
  stsab1=2,      stepx1=2,      vft1=20,        vrt1=20,        vri1=20,
  vpeu1=20,       vbop1=20,       vepn1=20,       vsab1=20,       vepx1=20,
  cec=20,         clay=20,        oc=20,          ph=5,          sand=20,
  silt=20,        p_tot=1000,       t_ma=0,        hd1=12,         is1=0.5,
  nbop1=2,       npeu1=2,       nft1=2,        nepn1=2,       nepx1=2,
  nsab1=2,       nri1=2,        nrt1=2,        veg_pot='MS2',     milieu='2',
  iter=1)

  nb_var_debut <- length(names(fic)) # 56
  res <- Prep_compile(fichier_compile=fic)
  nb_var_fin <- length(names(res)) # 84
  nom_fin <- names(res)

  # les variables latitude longitude altitude reg_eco silt p_tot t_ma ne sont plus là
  var_abs <- c('latitude', 'longitude', 'altitude', 'reg_eco', 'silt', 'p_tot', 't_ma')
  diff <- length(intersect(var_abs, nom_fin)) # doit être 0

  expect_equal(diff, 0)

})

# vérifier les variables inutiles si elles sont eneléves
# tester un cas où retrait serait vide, où il n'y a aucune variables inutiles
# vérifier les variables créées
# mutate(annee=0,
#        tbe=0,
#        pert=0,
#
#        veg_pot = substr(type_eco,1,3),
#        milieu = substr(type_eco,4,4),
#
#        ntot1 = nbop1+npeu1+nft1+nri1+nrt1+nsab1+nepn1+nepx1,
#        sttot1 = stbop1+stpeu1+stft1+stri1+strt1+stsab1+stepn1+stepx1,
#        vtot1 = vbop1+vpeu1+vft1+vri1+vrt1+vsab1+vepn1+vepx1,
#
#        pct_epn1 = stepn1/sttot1*100,
#        pct_epx1 = stepx1/sttot1*100,
#        pct_sab1 = stsab1/sttot1*100,
#        pct_ri1 = stri1/sttot1*100,
#        pct_rt1 = strt1/sttot1*100,
#        pct_bop1 = stbop1/sttot1*100,
#        pct_peu1 = stpeu1/sttot1*100,
#        pct_ft1 = stft1/sttot1*100,
#
#        origBR = ifelse(origine=='BR',1,0),
#        origCT = ifelse(origine=='CT',1,0),
#        origES = ifelse(origine=='ES',1,0),
#
#        vpms2 = ifelse(veg_pot %in% c('MS2','MS4','ME1'), 1, 0),
#        vpms6 = ifelse(veg_pot=='MS6', 1, 0),
#        vprb_ = ifelse(veg_pot %in% c('RB5','RB1','RB2'), 1, 0),
#        vpre1 = ifelse(veg_pot=='RE1', 1, 0),
#        vpre2 = ifelse(veg_pot %in% c('RE2','RE4'), 1, 0),
#        vpre3 = ifelse(veg_pot=='RE3', 1, 0),
#        vprp1 = ifelse(veg_pot=='RP1', 1, 0),
#        vprs1 = ifelse(veg_pot=='RS1', 1, 0),
#        vprs2 = ifelse(veg_pot %in% c('RS2','RS4','RS5','RS7'), 1, 0),
#        vprs3 = ifelse(veg_pot=='RS3', 1, 0),
#
#        mp0 = ifelse(milieu=='0', 1, 0),
#        mp1 = ifelse(milieu=='1', 1, 0),
#        mp2 = ifelse(milieu=='2', 1, 0),
#        mp3 = ifelse(milieu=='3', 1, 0),
#        mp4 = ifelse(milieu=='4', 1, 0),
#        mp5 = ifelse(milieu=='5', 1, 0),
#        mp6 = ifelse(milieu=='6', 1, 0),
#        mp789 = ifelse(milieu %in% c('7','8','9'), 1, 0)) %>%
#   rename(iqs_epn=iqs_pot_epn, iqs_epx=iqs_pot_epb, iqs_rt=iqs_pot_tho, iqs_ri=iqs_pot_pig, iqs_bop=iqs_pot_bop, iqs_peu=iqs_pot_pex, iqs_ft=iqs_pot_pib, iqs_sab=iqs_pot_sab)
