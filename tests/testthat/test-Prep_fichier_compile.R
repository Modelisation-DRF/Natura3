test_that("La fonction prep_compile() supprime les variables inutiles", {

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

test_that("La fonction prep_compile() fonctionne si aucune variables inutiles dans le fichier", {

  fic <- data.frame(
    iqs_pot_epn=15, iqs_pot_epb=15, iqs_pot_bop=15, iqs_pot_pex=15, iqs_pot_pig=15,
    iqs_pot_sab=15, iqs_pot_pib=15, iqs_pot_tho=15,
      sdom_bio='5O',      type_eco='MS22',   id_pe=1,
    prec_gs=800,     temp_gs=5,     origine='BR',     temps=15,       stft1=2,
    strt1=2,       stri1=2,       stpeu1=2,      stbop1=2,      stepn1=2,
    stsab1=2,      stepx1=2,      vft1=20,        vrt1=20,        vri1=20,
    vpeu1=20,       vbop1=20,       vepn1=20,       vsab1=20,       vepx1=20,
    cec=20,         clay=20,        oc=20,          ph=5,          sand=20,
           hd1=12,         is1=0.5,
    nbop1=2,       npeu1=2,       nft1=2,        nepn1=2,       nepx1=2,
    nsab1=2,       nri1=2,        nrt1=2,
    iter=1)

  nb_var_debut <- length(names(fic)) # 47

  expect_no_error(Prep_compile(fichier_compile=fic))

  res <- Prep_compile(fichier_compile=fic)
  nb_var_fin <- length(names(res)) # 84
  nom_fin <- names(res)

  # les variables latitude longitude altitude reg_eco silt p_tot t_ma ne sont plus là
  var_abs <- c('latitude', 'longitude', 'altitude', 'reg_eco', 'silt', 'p_tot', 't_ma')
  diff <- length(intersect(var_abs, nom_fin)) # doit être 0

  expect_equal(diff, 0)

})

test_that("La fonction prep_compile() ajoute les variables attendues", {

  fic <- data.frame(
    iqs_pot_epn=15, iqs_pot_epb=15, iqs_pot_bop=15, iqs_pot_pex=15, iqs_pot_pig=15,
    iqs_pot_sab=15, iqs_pot_pib=15, iqs_pot_tho=15,
    sdom_bio='5O',      type_eco='MS22',   id_pe=1,
    prec_gs=800,     temp_gs=5,     origine='BR',     temps=15,       stft1=2,
    strt1=2,       stri1=2,       stpeu1=2,      stbop1=2,      stepn1=2,
    stsab1=2,      stepx1=2,      vft1=20,        vrt1=20,        vri1=20,
    vpeu1=20,       vbop1=20,       vepn1=20,       vsab1=20,       vepx1=20,
    cec=20,         clay=20,        oc=20,          ph=5,          sand=20,
    hd1=12,         is1=0.5,
    nbop1=2,       npeu1=2,       nft1=2,        nepn1=2,       nepx1=2,
    nsab1=2,       nri1=2,        nrt1=2,
    iter=1)


  res <- Prep_compile(fichier_compile=fic)
  nom_fin <- names(res)

  nom_attendu <- c('annee', 'tbe', 'pert', 'veg_pot', 'milieu',
                   'ntot1', 'sttot1', 'vtot1',
                   'pct_epn1', 'pct_epx1', 'pct_sab1', 'pct_ri1', 'pct_rt1', 'pct_bop1', 'pct_peu1', 'pct_ft1',
                   'origBR', 'origCT', 'origES',
                   'vpms2', 'vpms6', 'vprb_', 'vpre1', 'vpre2', 'vpre3', 'vprp1', 'vprs1', 'vprs2', 'vprs3',
                   'mp0', 'mp1', 'mp2', 'mp3', 'mp4', 'mp5', 'mp6', 'mp789',
                   'iqs_epn', 'iqs_epx', 'iqs_rt', 'iqs_ri', 'iqs_bop', 'iqs_peu', 'iqs_ft', 'iqs_sab')

  diff <- length(setdiff(nom_attendu, nom_fin))  # setdiff(x, y) finds all rows in x that aren't in y

  expect_equal(diff, 0)

})



