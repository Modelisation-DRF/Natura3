# fct 7: param_hdom0_ess_stoch(): il faudrait au moins tester que la moyenne des stochastiques donne la valeur déterministe
# fct 8: param_hdom0_stoch(): meme chose
# fct 9: param_is_evol_stoch(): meme chose
# fct 10: param_hd_evol_stoch(): meme chose
# fct 11: param_evol_n_st_v_stoch(): meme chose
# fct 19: param_ishd_evol:

test_that("La fonction param_hdom0_ess_stoch() retourne un message d'erreur nb_iter=1 en mode STO", {
  expect_error(param_hdom0_ess_stoch(liste_arbre=fic, nb_iter=1, mode_simul='STO'))
})

test_that("La fonction param_hdom0_ess_stoch() fourni les bons parametres b2 en mode stochastique avec un seed=20", {
  fic1 <- data.frame(id_pe='1', no_arbre=1)
  fic2 <- data.frame(id_pe='1', no_arbre=2)
  fic3 <- data.frame(id_pe='1', no_arbre=3)
  fic4 <- data.frame(id_pe='1', no_arbre=4)
  fic <- bind_rows(fic1,fic2,fic3,fic4)

  b2 <- round(hdom_param_ess_fixe[hdom_param_ess_fixe$Parameter=="b2", 3],5) # valeur du parametre b2
  b2_se <- round(hdom_param_ess_fixe[hdom_param_ess_fixe$Parameter=="b2", 4],5) # erreur type du parameter b2

  param_sto = param_hdom0_ess_stoch(liste_arbre=fic, nb_iter=200, mode_simul='STO', seed_value = 20)
  b2_sto = unlist(lapply(1:24, function(x) {round(mean(param_sto[[x]][[2]]$b2),5)})) # la moyenne des 200 b2 simules
  b2_se_sto = unlist(lapply(1:24, function(x) {round(sd(param_sto[[x]][[2]]$b2),5)})) # la sd des 200 b2 simules
  expect_equal(b2_sto, b2)
  expect_equal(b2_se_sto, b2_se)
})


test_that("La fonction param_hdom0_ess_stoch() fourni les bons parametres en mode deterministe", {
  fic1 <- data.frame(id_pe='1', no_arbre=1)
  fic2 <- data.frame(id_pe='1', no_arbre=2)
  fic3 <- data.frame(id_pe='1', no_arbre=3)
  fic4 <- data.frame(id_pe='1', no_arbre=4)
  fic <- bind_rows(fic1,fic2,fic3,fic4)

  b2 <- hdom_param_ess_fixe[hdom_param_ess_fixe$Parameter=="b2", 3]

  param_det = param_hdom0_ess_stoch(liste_arbre=fic, mode_simul='DET')
  b2_det =    unlist(lapply(1:24, function(x) {param_det[[x]][[2]]$b2}))

  expect_equal(b2_det, b2)

})


test_that("La fonction param_hdom0_ess_stoch() fourni les bonnes erreurs résiduelles en mode stochastique avec un seed=20", {
  fic1 <- data.frame(id_pe='1', no_arbre=1)
  fic2 <- data.frame(id_pe='1', no_arbre=2)
  fic3 <- data.frame(id_pe='1', no_arbre=3)
  fic4 <- data.frame(id_pe='1', no_arbre=4)
  fic <- bind_rows(fic1,fic2,fic3,fic4)

  var_u = round(hdom_param_ess_fixe[hdom_param_ess_fixe$Parameter=="s2u_arbre", 3],5) # variance de l'effet aléatoire
  var_e = round(hdom_param_ess_fixe[hdom_param_ess_fixe$Parameter=="var", 3],5) # variance de l'erreur residuelle

  param_sto = param_hdom0_ess_stoch(liste_arbre=fic, nb_iter=500, mode_simul='STO', seed_value = 20)
  var_u_mean = unlist(lapply(1:24, function(x) {round(mean(param_sto[[x]][[3]]$random_arbre),5)})) # moyenne des effets aléatoires simulés
  var_u_var = unlist(lapply(1:24, function(x) {round(var(param_sto[[x]][[3]]$random_arbre),5)})) # var des effets aléatoires simulés
  var_e_mean = unlist(lapply(1:24, function(x) {round(mean(param_sto[[x]][[3]]$resid_arbre),5)})) # moyenne des erreurs residuelles simulées
  var_e_var = unlist(lapply(1:24, function(x) {round(var(param_sto[[x]][[3]]$resid_arbre),5)})) # var des erreurs residuelles simulées

  expect_equal(var_u_mean, rep(0,24))
  expect_equal(var_u_var, var_u)
  expect_equal(var_e_mean, rep(0,24))
  expect_equal(var_e_var, var_e)
})

#####################################################################################


test_that("La fonction param_hdom0_stoch() retourne un message d'erreur nb_iter=1 en mode STO", {
  expect_error(param_hdom0_stoch(liste_arbre=fic, nb_iter=1, mode_simul='STO'))
})


test_that("La fonction param_hdom0_stoch() fourni les bons parametres b2 en mode stochastique avec un seed=20", {
  fic1 <- data.frame(id_pe='1', no_arbre=1)
  fic2 <- data.frame(id_pe='1', no_arbre=2)
  fic3 <- data.frame(id_pe='1', no_arbre=3)
  fic4 <- data.frame(id_pe='1', no_arbre=4)
  fic <- bind_rows(fic1,fic2,fic3,fic4)

  b2 <- round(hdom_param_global_fixe[hdom_param_global_fixe$Parameter=="b2", 2],5) # valeur du parametre b2
  b2_se <- round(hdom_param_global_fixe[hdom_param_global_fixe$Parameter=="b2", 3],7) # erreur type du parameter b2

  param_sto = param_hdom0_stoch(liste_arbre=fic, nb_iter=200, mode_simul='STO', seed_value = 20)
  b2_sto =   round(mean(unlist(lapply(1:200, function(x) {param_sto[[x]][[1]]$b2}))),5) # la moyenne des 200 b2 simules
  b2_se_sto = round(sd(unlist(lapply(1:200, function(x) {param_sto[[x]][[1]]$b2}))),7) # la sd des 200 b2 simules

  expect_equal(b2_sto, b2)
  expect_equal(b2_se_sto, b2_se)
})

test_that("La fonction param_hdom0_stoch() fourni les bons parametres en mode deterministe", {
  fic1 <- data.frame(id_pe='1', no_arbre=1)
  fic2 <- data.frame(id_pe='1', no_arbre=2)
  fic3 <- data.frame(id_pe='1', no_arbre=3)
  fic4 <- data.frame(id_pe='1', no_arbre=4)
  fic <- bind_rows(fic1,fic2,fic3,fic4)

  b2 <- hdom_param_global_fixe[hdom_param_global_fixe$Parameter=="b2", 2] # valeur du parametre b2

  param_det = param_hdom0_stoch(liste_arbre=fic, mode_simul='DET')
  b2_det =   as.numeric(param_det[[1]][[1]] %>% dplyr::select(-iter))

  expect_equal(b2_det, b2)

})



test_that("La fonction param_hdom0_stoch() fourni les bonnes erreurs en mode stochastique avec un seed=20", {
  fic1 <- data.frame(id_pe='1', no_arbre=1)
  fic2 <- data.frame(id_pe='1', no_arbre=2)
  fic3 <- data.frame(id_pe='1', no_arbre=3)
  fic4 <- data.frame(id_pe='1', no_arbre=4)
  fic <- bind_rows(fic1,fic2,fic3,fic4)

  var_u <- round(hdom_param_global_fixe[hdom_param_global_fixe$Parameter=="s2u_arbre", 2],5) # variance de l'effet aléatoire
  var_e <- round(hdom_param_global_fixe[hdom_param_global_fixe$Parameter=="var", 2],5) # variance de l'erreur residuelle

  param_sto = param_hdom0_stoch(liste_arbre=fic, nb_iter=200, mode_simul='STO', seed_value = 20)
  var_u_mean =   round(mean(unlist(lapply(1:200, function(x) {param_sto[[x]][[2]]$random_arbre}))),5) # la moyenne des 200 effet aléatoire simules
  var_u_var =   round(var(unlist(lapply(1:200, function(x) {param_sto[[x]][[2]]$random_arbre}))),5) # la variance des 200 effet aléatoire simules
  var_e_mean =   round(mean(unlist(lapply(1:200, function(x) {param_sto[[x]][[2]]$resid_arbre}))),5) # la moyenne des 200 erreur residuelle simules
  var_e_var =   round(var(unlist(lapply(1:200, function(x) {param_sto[[x]][[2]]$resid_arbre}))),5) # la variance des 200 effet erreurs residuelles simules


  expect_equal(var_u_mean, 0)
  expect_equal(var_u_var, var_u)
  expect_equal(var_e_mean, 0)
  expect_equal(var_e_var, var_e)
})

###########################################################################################

test_that("La fonction param_is_evol_stoch() retourne un message d'erreur nb_iter=1 en mode STO", {
  expect_error(param_is_evol_stoch(liste_place=fic, nb_iter=1, mode_simul='STO'))
})

test_that("La fonction param_is_evol_stoch() fourni les bons parametres en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  param_fixe <- as.matrix(round(is_param_fixe[, c(1,2,3,4,7)],5)) # valeur des paramètres pour les 5 essences
  param_var1 <- matrix(round(is_param_cov[is_param_cov$Parameter=="int", 2],8), byrow = TRUE)
  param_var2 <- matrix(round(is_param_cov[is_param_cov$Parameter=="bis", 3],8), byrow = TRUE)
  param_var3 <- matrix(round(is_param_cov[is_param_cov$Parameter=="btemps", 4],8), byrow = TRUE)
  param_var4 <- matrix(round(is_param_cov[is_param_cov$Parameter=="bdt", 5],8), byrow = TRUE)
  param_var5 <- matrix(round(is_param_cov[is_param_cov$Parameter=="btbe", 6],8), byrow = TRUE)
  param_var <- cbind(param_var1,param_var2,param_var3,param_var4,param_var5)


  param_sto = param_is_evol_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20)

  param_fixe_sto =   bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>%
    summarise(int_is=mean(int_is),bis_is=mean(bis_is),btemps_is=mean(btemps_is),bdt_is=mean(bdt_is),btbe_is=mean(btbe_is)) %>%
    dplyr::select(-ess)
  param_fixe_sto = round(as.matrix(param_fixe_sto),5)

  param_var_sto = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>%
    summarise(int_is=var(int_is),bis_is=var(bis_is),btemps_is=var(btemps_is),bdt_is=var(bdt_is),btbe_is=var(btbe_is)) %>%
    dplyr::select(-ess)
  param_var_sto = matrix(unlist(round(as.data.frame(param_var_sto),8)),nrow = 5)

  expect_equal(param_fixe_sto, param_fixe)
  expect_equal(param_var_sto, param_var)
})

test_that("La fonction param_is_evol_stoch() fonctionne en mode stochastique avec moins que 5 iter", {
  # quand moins d'iterations que de nombres de paramètres à générer, il a fallu que je fasse une exception
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  param_sto = param_is_evol_stoch(liste_place=liste_place, nb_iter=2, mode_simul='STO', horizon=2)

  nb_lignes_fixe = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(nb=n())
  nb_lignes_random = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]][,1:3] %>% mutate(ess=x)})) %>% group_by(ess, id_pe) %>% summarise(nb=n(), .groups="drop_last")
  nb_lignes_error = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]][,1:4] %>% mutate(ess=x)})) %>% group_by(ess, id_pe, pas) %>% summarise(nb=n(), .groups="drop_last")

  expect_equal(as.data.frame(nb_lignes_fixe[,2]), data.frame(nb=rep(2,5)))
  expect_equal(as.data.frame(nb_lignes_random[,3]), data.frame(nb=rep(2,10)))
  expect_equal(as.data.frame(nb_lignes_error[,4]), data.frame(nb=rep(2,20)))

})

test_that("La fonction param_is_evol_stoch() fourni les bons parametres pour ISevol en mode deterministe", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  is_param_fixe1 <- is_param_fixe %>% arrange(ess_dom)
  param_fixe <- as_tibble(is_param_fixe1[, c(1,2,3,4,7)]) # valeur des paramètres pour les 5 essences

  param_det = param_is_evol_stoch(liste_place=liste_place, mode_simul='DET', horizon=2)
  param_fixe_det = as_tibble(bind_rows(lapply(1:5, function(x) {param_det[[x]][[2]][,1:5]})))

  expect_equal(param_fixe_det, param_fixe)

})

test_that("La fonction param_is_evol_stoch() fourni les bonnes erreurs avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  var_u <- as.matrix(round(is_param_fixe[, 6],5)) # valeur var effet aléatoire des 5 essences
  var_e <- as.matrix(round(is_param_fixe[, 5],5)) # valeur var erreur residuelle des 5 essences

  param_sto = param_is_evol_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20)

  var_u_mean =   bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]][,3] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=mean(random_plot)) %>% dplyr::select(-ess)
  var_u_mean <- round(sum(var_u_mean),5)
  var_e_mean =   bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=mean(res_plot)) %>% dplyr::select(-ess)
  var_e_mean <- round(sum(var_u_mean),5)

  var_u_var =   bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=var(random_plot)) %>% dplyr::select(-ess)
  var_u_var <- matrix(unlist(round(as.data.frame(var_u_var),5)),nrow=5)

  var_e_var =   bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=var(res_plot)) %>% dplyr::select(-ess)
  var_e_var <- matrix(unlist(round(as.data.frame(var_e_var),5)),nrow=5)

  expect_equal(var_u_mean, 0)
  expect_equal(var_e_mean, 0)
  expect_equal(var_u_var, var_u)
  expect_equal(var_e_var, var_e)
})

###########################################################################################

test_that("La fonction param_hd_evol_stoch() retourne un message d'erreur nb_iter=1 en mode STO", {
  expect_error(param_hd_evol_stoch(liste_place=fic, nb_iter=1, mode_simul='STO'))
})

test_that("La fonction param_hd_evol_stoch() fourni les bons parametres en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  hdevol_param_fixe1 <- hdevol_param_fixe %>% arrange(ess_dom)
  hdevol_param_cov1 <- hdevol_param_cov %>% arrange(ess_dom)
  param_fixe <- as.matrix(round(hdevol_param_fixe1[, c(1,2,3,4,7)],5)) # valeur des paramètres pour les 5 essences
  param_var1 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta0", 2],8), byrow = TRUE)
  param_var2 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta0iqs", 3],8), byrow = TRUE)
  param_var3 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta2", 4],8), byrow = TRUE)
  param_var4 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta2is", 5],8), byrow = TRUE)
  param_var5 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta2tbe", 6],8), byrow = TRUE)
  param_var <- cbind(param_var1,param_var2,param_var3,param_var4,param_var5)


  param_sto = param_hd_evol_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20)

  param_fixe_sto = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>%
    summarise(beta0=mean(beta0),beta0iqs=mean(beta0iqs),beta2=mean(beta2),beta2is=mean(beta2is),beta2tbe=mean(beta2tbe)) %>%
    mutate(ess2 = ifelse(ess==1,"epn",
                         ifelse(ess==2,"peu",
                                ifelse(ess==3,"ri",
                                       ifelse(ess==4,"bop","sab"))))) %>%
    arrange(ess2) %>%
    dplyr::select(-ess,-ess2)
  param_fixe_sto = round(as.matrix(param_fixe_sto),5)

  param_var_sto  = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>%
    summarise(beta0=var(beta0),beta0iqs=var(beta0iqs),beta2=var(beta2),beta2is=var(beta2is),beta2tbe=var(beta2tbe)) %>%
    mutate(ess2 = ifelse(ess==1,"epn",
                         ifelse(ess==2,"peu",
                                ifelse(ess==3,"ri",
                                       ifelse(ess==4,"bop","sab"))))) %>%
    arrange(ess2) %>%
    dplyr::select(-ess,-ess2)
  param_var_sto = matrix(unlist(round(as.data.frame(param_var_sto),8)),nrow = 5)

  expect_equal(param_fixe_sto, param_fixe)
  expect_equal(param_var_sto, param_var)
})


test_that("La fonction param_hd_evol_stoch() fonctionne en mode stochastique avec moins que 5 iter", {
  # quand moins d'iterations que de nombres de paramètres à générer, il a fallu que je fasse une exception
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  param_sto = param_hd_evol_stoch(liste_place=liste_place, nb_iter=2, mode_simul='STO', horizon=2)

  nb_lignes_fixe = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(nb=n())
  nb_lignes_random = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]][,1:3] %>% mutate(ess=x)})) %>% group_by(ess, id_pe) %>% summarise(nb=n(), .groups="drop_last")
  nb_lignes_error = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]][,1:4] %>% mutate(ess=x)})) %>% group_by(ess, id_pe, pas) %>% summarise(nb=n(), .groups="drop_last")

  expect_equal(as.data.frame(nb_lignes_fixe[,2]), data.frame(nb=rep(2,5)))
  expect_equal(as.data.frame(nb_lignes_random[,3]), data.frame(nb=rep(2,10)))
  expect_equal(as.data.frame(nb_lignes_error[,4]), data.frame(nb=rep(2,20)))

})


test_that("La fonction param_hd_evol_stoch() fourni les bons parametres pour HDevol en mode deterministe", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  hdevol_param_fixe1 <- hdevol_param_fixe %>% arrange(ess_dom)
  param_fixe <- as_tibble(hdevol_param_fixe1[, c(1,2,3,4,7)]) # valeur des paramètres pour les 5 essences

  param_det = param_hd_evol_stoch(liste_place=liste_place, mode_simul='DET', horizon=2)
  param_fixe_det = as_tibble(bind_rows(lapply(1:5, function(x) {param_det[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>%
    mutate(ess2 = ifelse(ess==1,"epn",
                         ifelse(ess==2,"peu",
                                ifelse(ess==3,"ri",
                                       ifelse(ess==4,"bop","sab"))))) %>%
    arrange(ess2) %>%
    dplyr::select(-ess,-ess2))

  expect_equal(param_fixe_det, param_fixe)

})


test_that("La fonction param_hd_evol_stoch() fourni les bonnes erreurs en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  hdevol_param_fixe1 <- hdevol_param_fixe %>% arrange(ess_dom)

  var_u <- as.matrix(round(hdevol_param_fixe1[, 6],5))
  var_e <- as.matrix(round(hdevol_param_fixe1[, 5],5))

  param_sto = param_hd_evol_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20)

  var_u_mean = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=mean(random_plot)) %>%
    mutate(ess2 = ifelse(ess==1,"epn",
                         ifelse(ess==2,"peu",
                                ifelse(ess==3,"ri",
                                       ifelse(ess==4,"bop","sab"))))) %>%
    arrange(ess2) %>%
    dplyr::select(-ess,-ess2)
  var_u_mean = sum(round(as.matrix(var_u_mean),5))

  var_e_mean = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=mean(res_plot)) %>%
    mutate(ess2 = ifelse(ess==1,"epn",
                         ifelse(ess==2,"peu",
                                ifelse(ess==3,"ri",
                                       ifelse(ess==4,"bop","sab"))))) %>%
    arrange(ess2) %>%
    dplyr::select(-ess,-ess2)
  var_e_mean = sum(round(as.matrix(var_e_mean),5))

  var_u_var = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=var(random_plot)) %>%
    mutate(ess2 = ifelse(ess==1,"epn",
                         ifelse(ess==2,"peu",
                                ifelse(ess==3,"ri",
                                       ifelse(ess==4,"bop","sab"))))) %>%
    arrange(ess2) %>%
    dplyr::select(-ess,-ess2)
  var_u_var <- matrix(unlist(round(as.data.frame(var_u_var),5)),nrow=5)

  var_e_var = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=var(res_plot)) %>%
    mutate(ess2 = ifelse(ess==1,"epn",
                         ifelse(ess==2,"peu",
                                ifelse(ess==3,"ri",
                                       ifelse(ess==4,"bop","sab"))))) %>%
    arrange(ess2) %>%
    dplyr::select(-ess,-ess2)
  var_e_var <- matrix(unlist(round(as.data.frame(var_e_var),5)),nrow=5)

  expect_equal(var_u_mean, 0)
  expect_equal(var_e_mean, 0)

  expect_equal(var_u_var, var_u)
  expect_equal(var_e_var, var_e)
})

#################################################################################################


test_that("La fonction param_ishd_evol_stoch() retourne un message d'erreur nb_iter=1 en mode STO", {
  expect_error(param_ishd_evol_stoch(liste_place=fic, nb_iter=1, mode_simul='STO'))
})


test_that("La fonction param_ishd_evol_stoch() fourni les bons parametres pour HDevol en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  hdevol_param_fixe1 <- hdevol_param_fixe %>% arrange(ess_dom)
  hdevol_param_cov1 <- hdevol_param_cov %>% arrange(ess_dom)
  param_fixe <- as.matrix(round(hdevol_param_fixe1[, c(1,2,3,4,7)],5)) # valeur des paramètres pour les 5 essences
  param_var1 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta0", 2],8), byrow = TRUE)
  param_var2 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta0iqs", 3],8), byrow = TRUE)
  param_var3 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta2", 4],8), byrow = TRUE)
  param_var4 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta2is", 5],8), byrow = TRUE)
  param_var5 <- matrix(round(hdevol_param_cov1[hdevol_param_cov1$parameter=="beta2tbe", 6],8), byrow = TRUE)
  param_var <- cbind(param_var1,param_var2,param_var3,param_var4,param_var5)


  param_sto = param_ishd_evol_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20)

  param_fixe_sto = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,6:10] %>% mutate(ess=x)})) %>% group_by(ess) %>%
    summarise(beta0=mean(beta0),beta0iqs=mean(beta0iqs),beta2=mean(beta2),beta2is=mean(beta2is),beta2tbe=mean(beta2tbe)) %>%
    dplyr::select(-ess)
  param_fixe_sto = round(as.matrix(param_fixe_sto),5)

  param_var_sto  = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,6:10] %>% mutate(ess=x)})) %>% group_by(ess) %>%
    summarise(beta0=var(beta0),beta0iqs=var(beta0iqs),beta2=var(beta2),beta2is=var(beta2is),beta2tbe=var(beta2tbe)) %>%
    dplyr::select(-ess)
  param_var_sto = matrix(unlist(round(as.data.frame(param_var_sto),8)),nrow = 5)

  expect_equal(param_fixe_sto, param_fixe)
  expect_equal(param_var_sto, param_var)
})

test_that("La fonction param_ishd_evol_stoch() fonctionne en mode stochastique avec moins que 5 iter", {
  # quand moins d'iterations que de nombres de paramètres à générer, il a fallu que je fasse une exception
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  param_sto = param_ishd_evol_stoch(liste_place=liste_place, nb_iter=2, mode_simul='STO', horizon=2)

  nb_lignes_fixe = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(nb=n())
  nb_lignes_random = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]][,1:3] %>% mutate(ess=x)})) %>% group_by(ess, id_pe) %>% summarise(nb=n(), .groups="drop_last")
  nb_lignes_error = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]][,1:4] %>% mutate(ess=x)})) %>% group_by(ess, id_pe, pas) %>% summarise(nb=n(), .groups="drop_last")

  expect_equal(as.data.frame(nb_lignes_fixe[,2]), data.frame(nb=rep(2,5)))
  expect_equal(as.data.frame(nb_lignes_random[,3]), data.frame(nb=rep(2,10)))
  expect_equal(as.data.frame(nb_lignes_error[,4]), data.frame(nb=rep(2,20)))

})


test_that("La fonction param_ishd_evol_stoch() fourni les bons parametres pour HDevol en mode deterministe", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  hdevol_param_fixe1 <- hdevol_param_fixe %>% arrange(ess_dom)
  param_fixe <- as_tibble(hdevol_param_fixe1[, c(1,2,3,4,7)]) # valeur des paramètres pour les 5 essences

  param_det = param_ishd_evol_stoch(liste_place=liste_place, mode_simul='DET', horizon=2)
  param_fixe_det = as_tibble(bind_rows(lapply(1:5, function(x) {param_det[[x]][[2]][,6:10]})))

  expect_equal(param_fixe_det, param_fixe)

})

test_that("La fonction param_ishd_evol_stoch() fourni les bons parametres pour ISevol en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  is_param_fixe1 <- is_param_fixe %>% arrange(ess_dom)
  is_param_cov1 <- is_param_cov %>% arrange(ess_dom)
  param_fixe <- as.matrix(round(is_param_fixe1[, c(1,2,3,4,7)],5)) # valeur des paramètres pour les 5 essences
  param_var1 <- matrix(round(is_param_cov1[is_param_cov1$Parameter=="int", 2],8), byrow = TRUE)
  param_var2 <- matrix(round(is_param_cov1[is_param_cov1$Parameter=="bis", 3],8), byrow = TRUE)
  param_var3 <- matrix(round(is_param_cov1[is_param_cov1$Parameter=="btemps", 4],8), byrow = TRUE)
  param_var4 <- matrix(round(is_param_cov1[is_param_cov1$Parameter=="bdt", 5],8), byrow = TRUE)
  param_var5 <- matrix(round(is_param_cov1[is_param_cov1$Parameter=="btbe", 6],8), byrow = TRUE)
  param_var <- cbind(param_var1,param_var2,param_var3,param_var4,param_var5)


  param_sto = param_ishd_evol_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20)

  param_fixe_sto = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>%
    summarise(int_is=mean(int_is),bis_is=mean(bis_is),btemps_is=mean(btemps_is),bdt_is=mean(bdt_is),btbe_is=mean(btbe_is)) %>%
    dplyr::select(-ess)
  param_fixe_sto = round(as.matrix(param_fixe_sto),5)

  param_var_sto  = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5] %>% mutate(ess=x)})) %>% group_by(ess) %>%
    summarise(int_is=var(int_is),bis_is=var(bis_is),btemps_is=var(btemps_is),bdt_is=var(bdt_is),btbe_is=var(btbe_is)) %>%
    dplyr::select(-ess)
  param_var_sto = matrix(unlist(round(as.data.frame(param_var_sto),8)),nrow = 5)

  expect_equal(param_fixe_sto, param_fixe)
  expect_equal(param_var_sto, param_var)
})

test_that("La fonction param_ishd_evol_stoch() fourni les bons parametres pour ISevol en mode deterministe", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  is_param_fixe1 <- is_param_fixe %>% arrange(ess_dom)
  param_fixe <- as_tibble(is_param_fixe1[, c(1,2,3,4,7)]) # valeur des paramètres pour les 5 essences

  param_sto = param_ishd_evol_stoch(liste_place=liste_place, mode_simul='DET', horizon=2)
  param_fixe_sto = as_tibble(bind_rows(lapply(1:5, function(x) {param_sto[[x]][[2]][,1:5]})))

  expect_equal(param_fixe_sto, param_fixe)

})



test_that("La fonction param_ishd_evol_stoch() fourni les bonnes erreurs pour HDevol en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  hdevol_param_fixe1 <- hdevol_param_fixe %>% arrange(ess_dom)

  var_u <- as.matrix(round(hdevol_param_fixe1[, 6],5))
  var_e <- as.matrix(round(hdevol_param_fixe1[, 5],5))

  param_sto = param_ishd_evol_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20)

  var_u_mean = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=mean(rand_plot_hd)) %>%
    dplyr::select(-ess)
  var_u_mean = sum(round(as.matrix(var_u_mean),5))

  var_e_mean = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=mean(res_plot_hd)) %>%
    dplyr::select(-ess)
  var_e_mean = sum(round(as.matrix(var_e_mean),5))

  var_u_var = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=var(rand_plot_hd)) %>%
    dplyr::select(-ess)
  var_u_var <- matrix(unlist(round(as.data.frame(var_u_var),5)),nrow=5)

  var_e_var = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=var(res_plot_hd)) %>%
    dplyr::select(-ess)
  var_e_var <- matrix(unlist(round(as.data.frame(var_e_var),5)),nrow=5)

  expect_equal(var_u_mean, 0)
  expect_equal(var_e_mean, 0)

  expect_equal(var_u_var, var_u)
  expect_equal(var_e_var, var_e)
})

test_that("La fonction param_ishd_evol_stoch() fourni les bonnes erreurs pour ISevol en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  is_param_fixe1 <- is_param_fixe %>% arrange(ess_dom)

  var_u <- as.matrix(round(is_param_fixe1[, 6],5))
  var_e <- as.matrix(round(is_param_fixe1[, 5],5))

  param_sto = param_ishd_evol_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20)

  var_u_mean = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=mean(rand_plot_is)) %>%
    dplyr::select(-ess)
  var_u_mean = sum(round(as.matrix(var_u_mean),5))

  var_e_mean = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=mean(res_plot_is)) %>%
    dplyr::select(-ess)
  var_e_mean = sum(round(as.matrix(var_e_mean),5))

  var_u_var = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=var(rand_plot_is)) %>%
    dplyr::select(-ess)
  var_u_var <- matrix(unlist(round(as.data.frame(var_u_var),5)),nrow=5)

  var_e_var = bind_rows(lapply(1:5, function(x) {param_sto[[x]][[4]] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(m1=var(res_plot_is)) %>%
    dplyr::select(-ess)
  var_e_var <- matrix(unlist(round(as.data.frame(var_e_var),5)),nrow=5)

  expect_equal(var_u_mean, 0)
  expect_equal(var_e_mean, 0)

  expect_equal(var_u_var, var_u)
  expect_equal(var_e_var, var_e)
})


#######################################################################################


test_that("La fonction param_evol_n_st_v_stoch() retourne un message d'erreur nb_iter=1 en mode STO", {
  expect_error(param_evol_n_st_v_stoch(liste_place=fic, nb_iter=1, mode_simul='STO'))
})



test_that("La fonction param_evol_n_st_v_stoch() fourni les bons parametres pour en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  param_fixe = round(bind_cols(lapply(1:8, function(x) {n_st_v_param_fixe[[x]] %>% dplyr::select(-essence)})),6)
  param_var = unlist(lapply(1:8, function(x) {round(diag(as.matrix(n_st_v_param_cov[[x]] %>% dplyr::select(-essence))),8)}))

  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  param_sto = param_evol_n_st_v_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20, liste_ess=liste_gress)

  param_fixe_sto = bind_rows(lapply(1:8, function(x) {param_sto[[x]][[2]] %>% mutate(ess=x)}))
  param_temp <- param_fixe_sto  %>% dplyr::select(-iter,-ess)
  vari <- names(param_temp)
  param_fixe_sto2 <- param_fixe_sto %>%
    group_by(ess) %>%
    summarise(across(all_of(vari), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    summarise(across(all_of(vari), ~ mean(.x, na.rm = TRUE)))
  param_fixe_sto2 = round(param_fixe_sto2,6)

  param_var_sto <- param_fixe_sto %>%
    group_by(ess) %>%
    summarise(across(all_of(vari), ~ var(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    summarise(across(all_of(vari), ~ mean(.x, na.rm = TRUE)))
  param_var_sto2 = as.vector(unlist(round(as.data.frame(param_var_sto),8)))

  expect_equal(param_fixe_sto2, param_fixe)
  expect_equal(param_var_sto2, param_var)
})


test_that("La fonction param_evol_n_st_v_stoch() fonctionne en mode stochastique avec moins que 5 iter", {
  # quand moins d'iterations que de nombres de paramètres à générer, il a fallu que je fasse une exception
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  liste_ess <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  param_sto = param_evol_n_st_v_stoch(liste_place=liste_place, nb_iter=2, mode_simul='STO', horizon=2, liste_ess=liste_ess)

  nb_lignes_fixe = bind_rows(lapply(1:8, function(x) {param_sto[[x]][[2]][,1:4] %>% mutate(ess=x)})) %>% group_by(ess) %>% summarise(nb=n(), .groups="drop_last")
  nb_lignes_error = bind_rows(lapply(1:8, function(x) {param_sto[[x]][[3]][,1:4] %>% mutate(ess=x)})) %>% group_by(ess, id_pe, pas) %>% summarise(nb=n(), .groups="drop_last")

  expect_equal(as.data.frame(nb_lignes_fixe[,2]), data.frame(nb=rep(2,8)))
  expect_equal(as.data.frame(nb_lignes_error[,4]), data.frame(nb=rep(2,32)))

})


test_that("La fonction param_evol_n_st_v_stoch() fourni les bons parametres pour en mode deterministe", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  param_fixe = round(bind_cols(lapply(1:8, function(x) {n_st_v_param_fixe[[x]] %>% dplyr::select(-essence)})),6)


  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  param_sto = param_evol_n_st_v_stoch(liste_place=liste_place, mode_simul='DET', horizon=2, liste_ess=liste_gress)

  param_fixe_det = bind_rows(lapply(1:8, function(x) {param_sto[[x]][[2]] %>% mutate(ess=x)}))
  param_temp <- param_fixe_det  %>% dplyr::select(-iter,-ess)
  vari <- names(param_temp)
  param_fixe_det2 <- param_fixe_det %>%
    summarise(across(all_of(vari), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup()
  param_fixe_det2 = round(param_fixe_det2,6)

  expect_equal(param_fixe_det2, param_fixe)

})

test_that("La fonction param_evol_n_st_v_stoch() fourni les bonnes erreurs en mode stochastique avec un seed=20", {
  fic <- data.frame(id_pe=c('1','2'))
  liste_place <- unique(fic$id_pe)

  var_e <- NULL
  for(i in liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')) {
    var1 <- as.vector(diag(as.matrix(n_st_v_param_random %>% filter(essence==i) %>% dplyr::select(paste0("n",i), paste0("st",i), paste0("v",i)))))
    var_e <- c(var_e,var1)
  }
  var_e <- round(var_e,5)

  liste_gress <- c('epn','epx','rt','ri','sab','bop','peu','ft')
  param_sto = param_evol_n_st_v_stoch(liste_place=liste_place, nb_iter=200, mode_simul='STO', horizon=2, seed_value = 20, liste_ess=liste_gress)

  var_e_mean = sum(round(bind_rows(lapply(1:8, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>%
     summarise(m1=mean(res_n),m2=mean(res_st),m3=mean(res_v)) %>%
     dplyr::select(-ess)
    ,5))

  var_e_var = as.matrix(round(bind_rows(lapply(1:8, function(x) {param_sto[[x]][[3]] %>% mutate(ess=x)})) %>% group_by(ess) %>%
                           summarise(m1=var(res_n),m2=var(res_st),m3=var(res_v)) %>%
                           dplyr::select(-ess)
                         ,5))
  var_e_var1 <- as.vector(c(var_e_var[1,],var_e_var[2,],var_e_var[3,],var_e_var[4,],var_e_var[5,],var_e_var[6,],var_e_var[7,],var_e_var[8,]))

  expect_equal(var_e_mean, 0)
  expect_equal(var_e_var1, var_e)
})
