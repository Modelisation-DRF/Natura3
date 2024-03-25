# fct 12: Prep_arbres(): calcul hauteur/volume des arbres,
#                        sélectionne les essences et attribue un groupe d'essences,
#                        compile les placettes (N/ST/V, pct, shannon):


test_that("La fonction Prep_arbres() retourne un fichier arbres avec les essences bien filtrées", {
  # ajouter un arbres qui sera supprimés car essence non traitée
  arbres0 <- data.frame(essence='PRP')
  arbres1 <- bind_rows(n_st_v_ass_ess,arbres0)

  # créer un fichier d'arbres avec toutes les essences possibles
  arbres <- arbres1 %>%
    dplyr::select(-groupe_ess, -ess_eq_hd) %>%
    mutate(id_pe="1", sdom_bio="5E", type_eco="RS22", origine='BR',  temps=50,
           altitude=100, p_tot=1000, t_ma=0, prec_gs=500, temp_gs=12,
           ph=3, cec=12, oc=12, sand=30, clay=10, silt=40,
           iqs_pot_epn=12, iqs_pot_epb=14, iqs_pot_pig=15, iqs_pot_sab=8, iqs_pot_tho=14, iqs_pot_pib=16, iqs_pot_bop=10, iqs_pot_pex=10,
           no_arbre=row_number(), dhpcm=10, nb_tige=1, tige_ha=25, etat='10', hauteur_pred=8, vol_dm3=300)

  arbres2 <- Filtrer_place(fichier=arbres)

  prep_arbre <- Prep_arbres(fic_arbre=arbres2, ht=F, vol=F, mode_simul='DET')

  arbres3 <- prep_arbre[[1]]

  ess_filtre <- unique(arbres3$essence)
  ess_attendu <- unique(n_st_v_ass_ess$essence)

  expect_equal(ess_filtre, ess_attendu)
})

# test_that("La fonction Prep_arbres() retourne un fichier arbres avec les colonnes attendues", {
#   # ajouter un arbres qui sera supprimés car essence non traitée
#   arbres0 <- data.frame(essence='PRP')
#   arbres1 <- bind_rows(n_st_v_ass_ess,arbres0)
#
#   # créer un fichier d'arbres avec toutes les essences possibles
#   arbres <- arbres1 %>%
#     dplyr::select(-groupe_ess, -ess_eq_hd) %>%
#     mutate(id_pe="1", sdom_bio="5E", type_eco="RS22", origine='BR',  temps=50,
#            altitude=100, p_tot=1000, t_ma=0, prec_gs=500, temp_gs=12,
#            ph=3, cec=12, oc=12, sand=30, clay=10, silt=40,
#            iqs_pot_epn=12, iqs_pot_epb=14, iqs_pot_pig=15, iqs_pot_sab=8, iqs_pot_tho=14, iqs_pot_pib=16, iqs_pot_bop=10, iqs_pot_pex=10,
#            no_arbre=row_number(), dhpcm=10, nb_tige=1, tige_ha=25, etat='10', hauteur_pred=8, vol_dm3=15)
#
#   arbres2 <- Filtrer_place(fichier=arbres)
#
#   prep_arbre <- Prep_arbres(fic_arbre=arbres2, ht=F, vol=F, mode_simul='DET')
#
#   arbres3 <- prep_arbre[[1]]
#
#   var_obs <- names(arbres3)
#   var_attendu <- c("id_pe", "sdom_bio", "type_eco", "origine", "temps",
#                    "vpms2", "vpms6", "vprb_", "vpre1", "vpre2", "vpre3", "vprp1", "vprs1", "vprs2", "vprs3",
#                    "altitude",  "temp_gs",  "prec_gs",  "p_tot",        "t_ma",
#                    "veg_pot", "milieu",
#                    "mp0", "mp1", "mp2", "mp3", "mp4", "mp5", "mp6", "mp789",
#                    "origBR", "origCT", "origES",
#                    "iqs_epn", "iqs_epx", "iqs_bop", "iqs_peu", "iqs_ri", "iqs_sab", "iqs_ft",  "iqs_rt",
#                    "ph", "cec", "oc", "sand", "clay", "silt",
#                    "no_arbre", "essence", "dhpcm", "nb_tige", "tige_ha", "etat",
#                    "hauteur_pred", "vol_dm3",
#                    "groupe_ess", "iter")
#
#
#   expect_setequal(var_obs, var_attendu)
#
# })


# test_that("La fonction Prep_arbres() retourne un fichier compilé avec les colonnes attendues", {
#
#   arbres <- data.frame(
#     id_pe="1", sdom_bio="5E", type_eco="RS22", origine='BR',  temps=50,
#     altitude=100, p_tot=1000, t_ma=0, prec_gs=500, temp_gs=12,
#     ph=3, cec=12, oc=12, sand=30, clay=10, silt=40,
#     iqs_pot_epn=12, iqs_pot_epb=14, iqs_pot_pig=15, iqs_pot_sab=8, iqs_pot_tho=14, iqs_pot_pib=16, iqs_pot_bop=10, iqs_pot_pex=10,
#     no_arbre=1, dhpcm=10, nb_tige=1, tige_ha=25, etat='10', essence='SAB', vol_dm3=15, hauteur_pred=10)
#
#   arbres2 <- Filtrer_place(fichier=arbres)
#
#   prep_arbre <- Prep_arbres(fic_arbre=arbres2, ht=F, vol=F, mode_simul='DET')
#   fic_compile <- prep_arbre[[2]]
#
#   var_obs <- names(fic_compile)
#   var_attendu <- c("id_pe", "sdom_bio", "type_eco", "origine", "temps",
#                    "vpms2", "vpms6", "vprb_", "vpre1", "vpre2", "vpre3", "vprp1", "vprs1", "vprs2", "vprs3",
#                    "altitude",  "temp_gs",  "prec_gs",  "p_tot",        "t_ma",
#                    "veg_pot", "milieu",
#                    "mp0", "mp1", "mp2", "mp3", "mp4", "mp5", "mp6", "mp789",
#                    "origBR", "origCT", "origES",
#                    "iqs_epn", "iqs_epx", "iqs_bop", "iqs_peu", "iqs_ri", "iqs_sab", "iqs_ft",  "iqs_rt",
#                    "ph", "cec", "oc", "sand", "clay", "silt",
#                    "annee", "tbe", "pert",
#                    "nbop1", "nepn1", "nepx1", "nft1", "npeu1", "nri1", "nrt1", "nsab1", "ntot1",
#                    "stbop1", "stepn1", "stepx1", "stft1", "stpeu1", "stri1", "strt1", "stsab1", "sttot1",
#                    "vbop1", "vepn1", "vepx1", "vft1", "vpeu1", "vri1", "vrt1", "vsab1", "vtot1",
#                    "pct_bop1", "pct_epn1", "pct_epx1", "pct_ft1", "pct_peu1", "pct_ri1", "pct_rt1", "pct_sab1",
#                    "is1",
#                    "iter")
#
#   expect_setequal(var_obs, var_attendu)
#
#
# })


test_that("La fonction Prep_arbres() fonctionne quand Ht et Vol doivent être calculés", {
  # ajouter un arbres qui sera supprimés car essence non traitée
  # créer un fichier d'arbres avec toutes les essences possibles
  arbres <- data.frame(
           id_pe="1", sdom_bio="5E", type_eco="RS22", origine='BR',  temps=50,
           altitude=100, p_tot=1000, t_ma=0, prec_gs=500, temp_gs=12,
           ph=3, cec=12, oc=12, sand=30, clay=10, silt=40,
           iqs_pot_epn=12, iqs_pot_epb=14, iqs_pot_pig=15, iqs_pot_sab=8, iqs_pot_tho=14, iqs_pot_pib=16, iqs_pot_bop=10, iqs_pot_pex=10,
           no_arbre=1, dhpcm=10, nb_tige=1, tige_ha=25, etat='10', essence='SAB')

  arbres2 <- Filtrer_place(fichier=arbres)
  prep_arbre <- Prep_arbres(fic_arbre=arbres2, ht=T, vol=T, mode_simul='DET')

  arbres3 <- prep_arbre[[1]]

  res_attendu <- round(c(arbres3$hauteur_pred, arbres3$vol_dm3),5)

  expect_equal(res_attendu, c(6.99607, 14.87973))
})

test_that("La fonction Prep_arbres() compile bien les placettes", {
  # ajouter un arbres qui sera supprimés car essence non traitée
  # créer un fichier d'arbres avec toutes les essences possibles
  info1 <- data.frame(id_pe=1, sdom_bio="5E", type_eco="RS22", origine='BR',  temps=50, altitude=100, p_tot=1000, t_ma=0, prec_gs=500, temp_gs=12, ph=3, cec=12, oc=12, sand=30, clay=10, silt=40, iqs_pot_epn=12, iqs_pot_epb=14, iqs_pot_pig=15, iqs_pot_sab=8, iqs_pot_tho=14, iqs_pot_pib=16, iqs_pot_bop=10, iqs_pot_pex=10)
  info2 <- data.frame(id_pe=2, sdom_bio="5E", type_eco="RS22", origine='BR',  temps=50, altitude=100, p_tot=1000, t_ma=0, prec_gs=500, temp_gs=12, ph=3, cec=12, oc=12, sand=30, clay=10, silt=40, iqs_pot_epn=12, iqs_pot_epb=14, iqs_pot_pig=15, iqs_pot_sab=8, iqs_pot_tho=14, iqs_pot_pib=16, iqs_pot_bop=10, iqs_pot_pex=10)
  info <- bind_rows(info1, info2)
  arbres1 <- data.frame(id_pe=1, no_arbre=1, dhpcm=10, nb_tige=1, tige_ha=25, etat='10', essence='SAB')
  arbres2 <- data.frame(id_pe=1, no_arbre=2, dhpcm=15, nb_tige=1, tige_ha=25, etat='10', essence='ERS')
  arbres3 <- data.frame(id_pe=1, no_arbre=3, dhpcm=20, nb_tige=1, tige_ha=25, etat='10', essence='BOP')
  arbres4 <- data.frame(id_pe=1, no_arbre=4, dhpcm=12, nb_tige=1, tige_ha=25, etat='10', essence='PET')
  arbres5 <- data.frame(id_pe=1, no_arbre=5, dhpcm=30, nb_tige=1, tige_ha=25, etat='10', essence='EPN')
  arbres6 <- data.frame(id_pe=1, no_arbre=6, dhpcm=15, nb_tige=1, tige_ha=25, etat='10', essence='EPB')
  arbres7 <- data.frame(id_pe=1, no_arbre=7, dhpcm=18, nb_tige=1, tige_ha=25, etat='10', essence='THO')
  arbres8 <- data.frame(id_pe=1, no_arbre=8, dhpcm=22, nb_tige=1, tige_ha=25, etat='10', essence='PIG')
  arbres9 <- data.frame(id_pe=2, no_arbre=1, dhpcm=28, nb_tige=1, tige_ha=25, etat='10', essence='SAB')
  arbres <- bind_rows(arbres1,arbres2,arbres3,arbres4,arbres5,arbres6,arbres7,arbres8,arbres9)

  arbres <- left_join(info, arbres, by="id_pe")

  arbres2 <- Filtrer_place(fichier=arbres)
  prep_arbre <- Prep_arbres(fic_arbre=arbres2, ht=T, vol=T, mode_simul='DET')

  fic_compile <- prep_arbre[[2]]
  # fic_compile2 <- fic_compile %>% dplyr::select(id_pe, annee, tbe, pert,
  #                                               nbop1, nepn1, nepx1, nft1, npeu1, nri1, nrt1, nsab1, ntot1,
  #                                               stbop1, stepn1, stepx1, stft1, stpeu1, stri1, strt1, stsab1, sttot1,
  #                                               vbop1, vepn1, vepx1, vft1, vpeu1, vri1, vrt1, vsab1, vtot1,
  #                                               pct_bop1, pct_epn1, pct_epx1, pct_ft1, pct_peu1, pct_ri1, pct_rt1, pct_sab1,
  #                                               is1)
  fic_compile2 <- fic_compile %>% dplyr::select(id_pe,
                                                nbop1, nepn1, nepx1, nft1, npeu1, nri1, nrt1, nsab1,
                                                stbop1, stepn1, stepx1, stft1, stpeu1, stri1, strt1, stsab1,
                                                vbop1, vepn1, vepx1, vft1, vpeu1, vri1, vrt1, vsab1,
                                                is1)
  fic_compile3 <- round(as.matrix(as.data.frame(fic_compile2), dimnames=NULL),3)
  dimnames(fic_compile3) <- NULL
  # res_attendu <- round(matrix(c(
  #   1,0,0,0,
  #   1,1,1,1,1,1,1,1,8,
  #   0.7853982,1.767146,0.4417865,0.4417865,0.2827433,0.9503318,0.6361725,0.1963495,5.501714,
  #   4.421823,12.00804,1.660848,1.677431,0.9999791,5.032742,1.861869,0.396402,28.059131,
  #   14.27552,32.11991,8.029979,8.029979,5.139186,17.27338,11.56317,3.568879,
  #   0.5979602,
  #
  #   2,0,0,0,
  #   0,0,0,0,0,0,0,1,1,
  #   0.0000000,0.000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,1.5393804,1.539380,
  #   0.000000,0.00000,0.000000,0.000000,0.0000000,0.000000,0.000000,9.751758,9.751758,
  #   0.00000,0.00000,0.000000,0.000000,0.000000,0.00000,0.00000,100.000000,
  #   0.0000000), nrow = 2, ncol=40, byrow = T),3)
  res_attendu <- round(matrix(c(
    1,
    1,1,1,1,1,1,1,1,
    0.7853982,1.767146,0.4417865,0.4417865,0.2827433,0.9503318,0.6361725,0.1963495,
    4.421823,12.00804,1.660848,1.677431,0.9999791,5.032742,1.861869,0.396402,
    0.5979602,

    2,
    0,0,0,0,0,0,0,1,
    0.0000000,0.000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,1.5393804,
    0.000000,0.00000,0.000000,0.000000,0.0000000,0.000000,0.000000,9.751758,
    0.0000000), nrow = 2, ncol=26, byrow = T),3)

  expect_equal(fic_compile3,res_attendu)
})





