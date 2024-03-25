# fct 15: ecrire_eq(): écrit l'équation à partir d'un fichier de parametre de SAS: à tester

test_that("La fonction ecrire_eq() retourne la bonne équation pour IS", {
  is = ecrire_eq(vari="is")
  attendu = paste0(is_eq$is,'+rand_plot_is+res_plot_is')
  expect_equal(is, attendu)
})

test_that("La fonction ecrire_eq() retourne la bonne équation pour HD", {
  hd = ecrire_eq(vari="hd", ess='')
  attendu = paste0(hdevol_eq$hd,'+res_plot_hd')
  expect_equal(hd, attendu)
})

test_that("La fonction ecrire_eq() retourne la bonne équation pour le parameter a de HD", {
  hd = ecrire_eq(vari="hd", ess='a')
  attendu = paste0(hdevol_eq$a,'+rand_plot_hd')
  expect_equal(hd, attendu)
})

test_that("La fonction ecrire_eq() retourne la bonne équation pour le parameter c de HD", {
  hd = ecrire_eq(vari="hd", ess='c')
  attendu = hdevol_eq$c
  expect_equal(hd, attendu)
})

test_that("La fonction ecrire_eq() retourne la bonne équation pour le stsab", {

  vari='st'
  ess='sab'

  resultat = ecrire_eq(vari=vari, ess=ess)

  eqa <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_a_',vari)]
  eqb <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_b_',vari)]
  eqc <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_c_',vari)]
  eqb <- paste0("(", eqb, ")", "* log(temps1+1)")
  eqc <- paste0("(", eqc, ")", "* (log(temps1+1))^2")
  eq <- paste0("ifelse(",vari,ess,"1>0,exp( (", eqa, ")+(", eqb, ")+(", eqc,")) + res_", vari, ess,",0)")

  expect_equal(resultat, eq)
})

test_that("La fonction ecrire_eq() retourne la bonne équation pour le nri", {

  vari='n'
  ess='ri'

  resultat = ecrire_eq(vari=vari, ess=ess)

  eqa <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_a_',vari)]
  eqb <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_b_',vari)]
  eqc <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_c_',vari)]
  eqb <- paste0("(", eqb, ")", "* log(temps1+1)")
  eqc <- paste0("(", eqc, ")", "* (log(temps1+1))^2")
  eq <- paste0("ifelse(",vari,ess,"1>0,exp( (", eqa, ")+(", eqb, ")+(", eqc,")) + res_", vari, ess,",0)")

  expect_equal(resultat, eq)
})

test_that("La fonction ecrire_eq() retourne la bonne équation pour le vepx", {

  vari='v'
  ess='epx'

  resultat = ecrire_eq(vari=vari, ess=ess)

  eqa <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_a_',vari)]
  eqb <- '0'
  eqc <- '0'
  eq <- paste0("ifelse(",vari,ess,"1>0,exp( (", eqa, ")+(", eqb, ")+(", eqc,")) + res_", vari, ess,",0)")

  expect_equal(resultat, eq)
})


