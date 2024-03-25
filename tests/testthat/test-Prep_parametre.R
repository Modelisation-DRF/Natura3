# fct 14: Prep_parametre(): sélectionne les paramètres associés à la bonne itération et le bon time step pour toutes les placettes,
#                           identifie l'essence dominante et sélectionne l'iqs associé. À tester.
# fct 20: Prep_parametre_iter(): sélectionne les paramètres qui ne changent pas tout au long d'une itération (pour le cas fichier compile). À tester.
# fct 21: Prep_parametre_pas: sélectionne les paramètres associés a un pas de simulation (pour le cas fichier compile).
#                            identifie l'essence dominante et sélectionne l'iqs associé. À tester.

# # 1
# test_that("La fonction Prep_parametre() sélectionne les bons paramètres en mode déterministe", {
#
#   liste_plot = data.frame(id_pe=1)
#   liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
#
#   # si déterministe, l'horizon n'a pas d'impact sur les parametres, une seule ligne d'effets fixes
#   param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
#   param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot,  mode_simul='DET', horizon=5)
#   param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)
#
#   liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
#   data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
#
#   param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET',
#                           param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v,
#                           liste_ess=liste_gress)
#
#   nb_col = ncol(param)
#   nb_ligne = nrow(param)
#
#   # pour is et hd, il faut vérifier que ça retourne les paramètres de l'essence dominante
#   # pour n-st-v, les paramètres des 8 groupes d'essences doivent être mis bout à bout sur une meme ligne
#
#   # l'essence dominante doit être ri et l'iqs associé est 13
#
#   # en mode deterministe, il y a seulement une ligne de paramètres et 313 variables
#   expect_equal(nb_col, 313)
#   expect_equal(nb_ligne, 1)
#   expect_equal(param$ess_dom, 'ri')
#   expect_equal(param$iqs, 13)
#   expect_equal(round(param$bis_is,4), round(param_is_evol[[4]][[2]]$bis_is,4)) # parametre effet fixe de IS pour ri
#   expect_equal(round(param$beta0,4), round(param_hd_evol[[3]][[2]]$beta0,4)) # parametre effet fixe de HD pour ri
#   expect_equal(c(param$rand_plot_is, param$res_plot_is, param$rand_plot_hd, param$res_plot_hd), c(0,0,0,0)) # les erreurs sont toutes à 0 car déterministe
#
# })
#
# # 2
# test_that("La fonction Prep_parametre() sélectionne les bons paramètres en mode stochastique", {
#
#   liste_plot = data.frame(id_pe=1)
#   liste_plot <- unique(liste_plot$id_pe)
#   liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
#
#
#   param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, nb_iter=2, mode_simul='STO', horizon=5, seed_value = 20)
#   param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot, nb_iter=2, mode_simul='STO', horizon=5, seed_value = 20)
#   param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, nb_iter=2, mode_simul='STO', horizon=5, liste_ess=liste_gress, seed_value = 20)
#
#   liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
#   data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
#
#   param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=2, pas=2, mode_simul='STO',
#                           param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v,
#                           liste_ess=liste_gress)
#
#   nb_col = ncol(param)
#   nb_ligne = nrow(param)
#
#   # en mode stochastique, il y a plusieurs iterations de générées, mais la fct doit en retourner seulement 1, et 313 variables
#   # il faut vérifier qu'elle retourne la bonne ligne pour chaque série de paramètres
#   # pour is et hd, il faut vérifier que ça retourne les paramètres de l'essence dominante, et ensuite la bonne itération et le bon pas
#   # pour n-st-v, les paramètres des 8 groupes d'essences doivent être mis bout à bout sur une meme ligne, avec la bonne iter et le bon pas
#
#   # l'essence dominante doit être ri
#
#   expect_equal(nb_col, 313)
#   expect_equal(nb_ligne, 1)
#   expect_equal(param$ess_dom, 'ri')
#   expect_equal(param$iqs, 13)
#   expect_equal(round(param$bis_is,4),round(param_is_evol[[4]][[2]]$bis_is[2],4)) # parametre effet fixe de l'iteration 2 de IS pour ri
#   expect_equal(round(param$beta0,4), round(param_hd_evol[[3]][[2]]$beta0[2],4)) # parametre effet fixe de l'iteration 2 de HD pour ri
#   expect_equal(round(c(param$rand_plot_is, param$res_plot_is, param$rand_plot_hd, param$res_plot_hd),4),
#                round(c( param_is_evol[[4]][[3]]$random_plot[2],
#                         param_is_evol[[4]][[4]]$res_plot[4],
#                         param_hd_evol[[3]][[3]]$random_plot[2],
#                         param_hd_evol[[3]][[4]]$res_plot[4]) ,4)) # erreurs de l'iteration 2 et pas 2
#
#   expect_equal(round(c(param$l0, param$n0, param$o0, param$i0, param$m0, param$p0, param$k0, param$j0),4),
#                round(c(
#                  param_n_st_v[[8]][[2]]$l0[2], #ft
#                  param_n_st_v[[7]][[2]]$n0[2], #peu
#                  param_n_st_v[[6]][[2]]$o0[2], #bop
#                  param_n_st_v[[5]][[2]]$i0[2], #sab
#                  param_n_st_v[[4]][[2]]$m0[2], #ri
#                  param_n_st_v[[3]][[2]]$p0[2], #rt
#                  param_n_st_v[[2]][[2]]$k0[2], #epx
#                  param_n_st_v[[1]][[2]]$j0[2]), #epn
#                  4) # parametres effets fixes de l'iteration 2
#   )
#
#   retourne = round(param[,4:27],4)
#   names(retourne)<-NULL
#   row.names(retourne) <- NULL
#
#   attendu = round(
#     cbind(
#       param_n_st_v[[1]][[3]][4,4:6],
#       param_n_st_v[[2]][[3]][4,4:6],
#       param_n_st_v[[3]][[3]][4,4:6],
#       param_n_st_v[[4]][[3]][4,4:6],
#       param_n_st_v[[5]][[3]][4,4:6],
#       param_n_st_v[[6]][[3]][4,4:6],
#       param_n_st_v[[7]][[3]][4,4:6],
#       param_n_st_v[[8]][[3]][4,4:6] # les 3 erreurs (n-st-v) de chacun des 8 groupes = 24 nombres: epn, epx, rt, ri, sab, bop, peu, ft
#     ),4)
#   names(attendu) <- NULL
#   row.names(attendu) <- NULL
#
#   expect_equal(retourne, attendu)
#
#   })
#
# # 3
# test_that("La fonction Prep_parametre() choisi le bonne essence dominante en cas d'égalité", {
#
#   liste_plot = data.frame(id_pe=1)
#   liste_plot <- unique(liste_plot$id_pe)
#   liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
#
#   param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
#   param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot,  mode_simul='DET', horizon=5)
#   param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)
#
#   liste_place = data.frame(id_pe=1, pct_sab1=20, pct_epn1=20, pct_epx1=10, pct_ri1=10, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
#   data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
#
#   param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET',
#                           param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v,
#                           liste_ess=liste_gress)
#
#   # ici sab=epn, ça doit retourne la première
#   expect_equal(param$ess_dom, 'sab')
#   expect_equal(param$iqs, 10)
#
# })
#
# # 4
# test_that("La fonction Prep_parametre() choisi le bonne essence dominante quand EPX est l'essence dominante", {
#
#   liste_plot = data.frame(id_pe=1)
#   liste_plot <- unique(liste_plot$id_pe)
#   liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
#
#   param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
#   param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot,  mode_simul='DET', horizon=5)
#   param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)
#
#   liste_place = data.frame(id_pe=1, pct_sab1=20, pct_epn1=20, pct_epx1=30, pct_ri1=10, pct_rt1=10, pct_peu1=10, pct_bop1=0, pct_ft1=0)
#   data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
#
#   param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET',
#                           param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v,
#                           liste_ess=liste_gress)
#
#   expect_equal(param$ess_dom, 'epn') # retourne epn au lieu de epx
#   expect_equal(param$iqs, 12) # mais iqs de epx
#
# })
#
# # 5
# test_that("La fonction Prep_parametre() choisi le bonne essence dominante quand RT est l'essence dominante", {
#
#   liste_plot = data.frame(id_pe=1)
#   liste_plot <- unique(liste_plot$id_pe)
#   liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
#
#   param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
#   param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot,  mode_simul='DET', horizon=5)
#   param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)
#
#   liste_place = data.frame(id_pe=1, pct_sab1=20, pct_epn1=20, pct_epx1=0, pct_ri1=10, pct_rt1=30, pct_peu1=10, pct_bop1=10, pct_ft1=0)
#   data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
#
#   param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET',
#                           param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v,
#                           liste_ess=liste_gress)
#
#   expect_equal(param$ess_dom, 'epn') # retourne epn au lieu de res
#   expect_equal(param$iqs, 14) # mais iqs de rt
#
# })
#
# # 6
# test_that("La fonction Prep_parametre() choisi le bonne essence dominante quand FT est l'essence dominante", {
#
#   liste_plot = data.frame(id_pe=1)
#   liste_plot <- unique(liste_plot$id_pe)
#   liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
#
#   param_is_evol <- param_is_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
#   param_hd_evol <- param_hd_evol_stoch(liste_place=liste_plot,  mode_simul='DET', horizon=5)
#   param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)
#
#   liste_place = data.frame(id_pe=1, pct_sab1=20, pct_epn1=20, pct_epx1=0, pct_ri1=10, pct_rt1=0, pct_peu1=10, pct_bop1=10, pct_ft1=30)
#   data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)
#
#   param <- Prep_parametre(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET',
#                           param_is_evol=param_is_evol, param_hd_evol=param_hd_evol, param_n_st_v=param_n_st_v,
#                           liste_ess=liste_gress)
#
#   expect_equal(param$ess_dom, 'bop') # retourne epn au lieu de res
#   expect_equal(param$iqs, 17) # mais iqs de rt
#
# })


################################################################################################################
################################################################################################################
################################################################################################################

# 1
test_that("La fonction Prep_parametre_pas() sélectionne les bons paramètres en mode déterministe", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  # si déterministe, l'horizon n'a pas d'impact sur les parametres, une seule ligne d'effets fixes
  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
  data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)

  param <- Prep_parametre_pas(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET', param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)

  nb_col = ncol(param)
  nb_ligne = nrow(param)

  names(param)

  # cette fonction ne retour que les erreur_res et rand_plot avec iqs et ess_dom
  # l'essence dominante doit être ri et l'iqs associé est 13

  expect_equal(nb_col, 31)
  expect_equal(nb_ligne, 1) # une seule ligne en deterministe
  expect_equal(param$ess_dom, 'ri')
  expect_equal(param$iqs, 13)
  expect_equal(sum(param[,4:31]), 0) # les erreurs sont toutes à 0 car déterministe

})


# 2
test_that("La fonction Prep_parametre_pas() sélectionne les bons paramètres en mode stochastique", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, nb_iter=2, mode_simul='STO', horizon=5, seed_value = 20)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, nb_iter=2, mode_simul='STO', horizon=5, liste_ess=liste_gress, seed_value = 20)

  liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
  data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)

  param <- Prep_parametre_pas(data=liste_place, data_info=data_info, iteration=2, pas=2, mode_simul='STO', param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)

  nb_col = ncol(param)
  nb_ligne = nrow(param)

  # en mode stochastique, il y a plusieurs iterations de générées, mais la fct doit en retourner seulement 1, et 313 variables
  # il faut vérifier qu'elle retourne la bonne ligne pour chaque série de paramètres
  # pour is et hd, il faut vérifier que ça retourne les paramètres de l'essence dominante, et ensuite la bonne itération et le bon pas
  # pour n-st-v, les paramètres des 8 groupes d'essences doivent être mis bout à bout sur une meme ligne, avec la bonne iter et le bon pas

  # l'essence dominante doit être ri

  expect_equal(nb_col, 31)
  expect_equal(nb_ligne, 1)
  expect_equal(param$ess_dom, 'ri')
  expect_equal(param$iqs, 13)
  expect_equal(round(c(param$rand_plot_is, param$res_plot_is, param$rand_plot_hd, param$res_plot_hd),4),
               round(c( param_ishd_evol[[4]][[3]]$rand_plot_is[2],
                        param_ishd_evol[[4]][[4]]$res_plot_is[4],
                        param_ishd_evol[[4]][[3]]$rand_plot_hd[2],
                        param_ishd_evol[[4]][[4]]$res_plot_hd[4]) ,4)) # erreurs de l'iteration 2 et pas 2


  retourne = round(param[,4:27],4)
  names(retourne)<-NULL
  row.names(retourne) <- NULL

  attendu = round(
    cbind(
      param_n_st_v[[1]][[3]][4,4:6],
      param_n_st_v[[2]][[3]][4,4:6],
      param_n_st_v[[3]][[3]][4,4:6],
      param_n_st_v[[4]][[3]][4,4:6],
      param_n_st_v[[5]][[3]][4,4:6],
      param_n_st_v[[6]][[3]][4,4:6],
      param_n_st_v[[7]][[3]][4,4:6],
      param_n_st_v[[8]][[3]][4,4:6] # les 3 erreurs (n-st-v) de chacun des 8 groupes = 24 nombres: epn, epx, rt, ri, sab, bop, peu, ft
    ),4)
  names(attendu) <- NULL
  row.names(attendu) <- NULL

  expect_equal(retourne, attendu)

})

# 3
test_that("La fonction Prep_parametre_pas() choisi le bonne essence dominante en cas d'égalité", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  liste_place = data.frame(id_pe=1, pct_sab1=20, pct_epn1=20, pct_epx1=10, pct_ri1=10, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
  data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)

  param <- Prep_parametre_pas(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET', param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)

  # ici sab=epn, ça doit retourne la première
  expect_equal(param$ess_dom, 'sab')
  expect_equal(param$iqs, 10)

})

# 4
test_that("La fonction Prep_parametre_pas() choisi le bonne essence dominante quand EPX est l'essence dominante", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  liste_place = data.frame(id_pe=1, pct_sab1=20, pct_epn1=20, pct_epx1=30, pct_ri1=10, pct_rt1=10, pct_peu1=10, pct_bop1=0, pct_ft1=0)
  data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)

  param <- Prep_parametre_pas(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET',
                          param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v,
                          liste_ess=liste_gress)

  expect_equal(param$ess_dom, 'epn') # retourne epn au lieu de epx
  expect_equal(param$iqs, 11) # iqs de epn

})

# 5
test_that("La fonction Prep_parametre_pas() choisi le bonne essence dominante quand RT est l'essence dominante", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  liste_place = data.frame(id_pe=1, pct_sab1=20, pct_epn1=20, pct_epx1=0, pct_ri1=10, pct_rt1=30, pct_peu1=10, pct_bop1=10, pct_ft1=0)
  data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)

  param <- Prep_parametre_pas(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET',
                          param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v,
                          liste_ess=liste_gress)

  expect_equal(param$ess_dom, 'epn') # retourne epn au lieu de res
  expect_equal(param$iqs, 11) # iqs de repn

})

# 6
test_that("La fonction Prep_parametre_pas() choisi le bonne essence dominante quand FT est l'essence dominante", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  liste_place = data.frame(id_pe=1, pct_sab1=20, pct_epn1=20, pct_epx1=0, pct_ri1=10, pct_rt1=0, pct_peu1=10, pct_bop1=10, pct_ft1=30)
  data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)

  param <- Prep_parametre_pas(data=liste_place, data_info=data_info, iteration=1, pas=1, mode_simul='DET',
                          param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v,
                          liste_ess=liste_gress)

  expect_equal(param$ess_dom, 'bop') # retourne epn au lieu de res
  expect_equal(param$iqs, 16) # mais iqs de bop

})

#####################################################################################################
#####################################################################################################
#####################################################################################################

# 1
test_that("La fonction Prep_parametre_iter() sélectionne les bons paramètres en mode déterministe", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  # si déterministe, l'horizon n'a pas d'impact sur les parametres, une seule ligne d'effets fixes
  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, mode_simul='DET', horizon=5, liste_ess=liste_gress)

  liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
  data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)

  param <- Prep_parametre_iter(iteration=1, param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)

  nb_col = ncol(param)
  nb_ligne = nrow(param)

  # pour is et hd, il faut vérifier que ça retourne les paramètres de l'essence dominante
  # pour n-st-v, les paramètres des 8 groupes d'essences doivent être mis bout à bout sur une meme ligne

  # l'essence dominante doit être ri et l'iqs associé est 13

  # en mode deterministe, il y a  283 variables (les erreur et random ne sont pas là)
  expect_equal(nb_col, 283)
  expect_equal(nb_ligne, 5) # les param de hd et is ont une ligne par ess, donc 5 lignes et les param de n-st-v ont une seule ligne et recopié 5 fois

  expect_equal(round(param[param$ess_dom=='ri','bis_is'],4), round(param_ishd_evol[[4]][[2]]$bis_is,4)) # parametre effet fixe de IS pour ri
  expect_equal(round(param[param$ess_dom=='ri', 'beta0'],4), round(param_ishd_evol[[4]][[2]]$beta0,4)) # parametre effet fixe de HD pour ri
  expect_equal(round(param[3, 'j0'],4), round(param_n_st_v[[1]][[2]]$j0,4)) # parametre effet fixe de HD pour ri


})

# 2
test_that("La fonction Prep_parametre_iter() sélectionne les bons paramètres en mode stochastique", {

  liste_plot = data.frame(id_pe=1)
  liste_plot <- unique(liste_plot$id_pe)
  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')

  param_ishd_evol <- param_ishd_evol_stoch(liste_place=liste_plot, nb_iter=2, mode_simul='STO', horizon=5, seed_value = 20)
  param_n_st_v <- param_evol_n_st_v_stoch(liste_place=liste_plot, nb_iter=2, mode_simul='STO', horizon=5, liste_ess=liste_gress, seed_value = 20)

  liste_place = data.frame(id_pe=1, pct_sab1=15, pct_epn1=15, pct_epx1=10, pct_ri1=20, pct_rt1=10, pct_peu1=10, pct_bop1=10, pct_ft1=10)
  data_info = data.frame(id_pe=1, iqs_sab=10, iqs_epn=11, iqs_epx=12, iqs_ri=13, iqs_rt=14, iqs_peu=15, iqs_bop=16, iqs_ft=17)

  param <- Prep_parametre_iter(iteration=2, param_ishd_evol=param_ishd_evol, param_n_st_v=param_n_st_v, liste_ess=liste_gress)

  nb_col = ncol(param)
  nb_ligne = nrow(param)


  # en mode stochastique, il y a 283 variables (les erreur et random ne sont pas là)
  expect_equal(nb_col, 283)
  expect_equal(nb_ligne, 5) # les param de hd et is ont une ligne par ess, donc 5 lignes et les param de n-st-v ont une seule ligne et recopié 5 fois, car en stochastique, pour une itération, on garde les meme paramètre effet fixe

  expect_equal(round(param[param$ess_dom=='ri','bis_is'],4), round(param_ishd_evol[[4]][[2]]$bis_is[2],4)) # parametre effet fixe de IS pour ri
  expect_equal(round(param[param$ess_dom=='ri', 'beta0'],4), round(param_ishd_evol[[4]][[2]]$beta0[2],4)) # parametre effet fixe de HD pour ri

  expect_equal(round(c(param$l0[3], param$n0[3], param$o0[3], param$i0[3], param$m0[3], param$p0[3], param$k0[3], param$j0[3]),4),
               round(c(
                 param_n_st_v[[8]][[2]]$l0[2], #ft
                 param_n_st_v[[7]][[2]]$n0[2], #peu
                 param_n_st_v[[6]][[2]]$o0[2], #bop
                 param_n_st_v[[5]][[2]]$i0[2], #sab
                 param_n_st_v[[4]][[2]]$m0[2], #ri
                 param_n_st_v[[3]][[2]]$p0[2], #rt
                 param_n_st_v[[2]][[2]]$k0[2], #epx
                 param_n_st_v[[1]][[2]]$j0[2]), #epn
                 4) # parametres effets fixes de l'iteration 2
  )

})
