
# fct 1: CheckArguments(): vérifications des paramètres.

test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour l'absence de fichiers 1", {
  chk = CheckArguments(file_arbre=, file_etude=, file_compile=, horizon=10)
  expect_equal(chk, "Au moins un des deux: file_arbre+file_etude OU file_compile doit être specifié")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour l'absence de fichiers 2", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, file_compile=fic3, horizon=10)
  expect_equal(chk, "Seulement un des deux: file_arbre+file_etude OU file_compile doit être specifé")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu poutr l'absence de fichiers 3", {
  chk = CheckArguments(file_arbre=fic1, file_etude=, file_compile=fic3, horizon=10)
  expect_equal(chk, "Seulement un des deux: file_arbre+file_etude OU file_compile doit être specifé")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour l'absence de fichiers 4", {
  chk = CheckArguments(file_arbre=, file_etude=fic2, file_compile=fic3, horizon=10)
  expect_equal(chk, "Seulement un des deux: file_arbre+file_etude OU file_compile doit être specifé")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour l'absence de fichiers 5", {
  chk = CheckArguments(file_arbre=fic1, file_etude=, file_compile=, horizon=10)
  expect_equal(chk, "Si file_compile n'est pas specifié, file_arbre ET file_etude doivent être specifiés")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour l'absence de fichiers 5", {
  chk = CheckArguments(file_arbre=, file_etude=fic2, file_compile=, horizon=10)
  expect_equal(chk, "Si file_compile n'est pas specifié, file_arbre ET file_etude doivent être specifiés")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour l'horizon 1", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=16)
  expect_equal(chk, "horizon doit être de 1 a 15")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour l'horizon 2", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=0)
  expect_equal(chk, "horizon doit être de 1 a 15")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe1 1", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=3)
  expect_equal(chk, "dec_perturb doit être <= horizon")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe1 2", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=3, tbe1 = 1)
  expect_equal(chk, "dec_tbe1 doit être <= horizon")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe1 3", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=2, tbe1=0)
  expect_equal(chk, "Si dec_tbe1 est specifié, tbe1 doit être > 0")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe1 4", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=0, tbe1=2)
  expect_equal(chk, "Si tbe1 est specifié, dec_tbe1 doit être specifié aussi")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe1 5", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=2, tbe1=2.2)
  expect_equal(chk, "Si dec_tbe1 est specifié, tbe1 doit etre 1, 2, 3, 4 ou 5")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe2 1", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=2, tbe2=0)
  expect_equal(chk, "dec_tbe1 et tbe1 doivent être specifiés pour pouvoir utiliser dec_tbe2")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe2 2", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=1, tbe1=1, dec_tbe2=3, tbe2=0)
  expect_equal(chk, "dec_tbe2 doit être <= horizon")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pou4 tbe2 3", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=2, tbe1=1, dec_tbe2=1, tbe2=0)
  expect_equal(chk, "dec_tbe2 doit être > dec_tbe1")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe2 4", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=1, tbe1=1, dec_tbe2=2, tbe2=0)
  expect_equal(chk, "Si dec_tbe2 est specifié, tbe2 doit etre > 0")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe2 5", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=1, tbe1=1, dec_tbe2=0, tbe2=1)
  expect_equal(chk, "Si tbe2 est specifié, dec_tbe2 doit être specifié aussi")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour tbe2 6", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, dec_perturb=0, dec_tbe1=1, tbe1=1, dec_tbe2=2, tbe2=1.1)
  expect_equal(chk, "Si dec_tbe2 est specifié, tbe2 doit être 1, 2, 3, 4 ou 5")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour mode_deter", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, mode_simul='DE', nb_iter=1, iqs=T, climat=T, sol=T, ht=T, vol=T, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  expect_equal(chk, "mode_simul doit être DET ou STO")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour nb_iter", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, mode_simul='STO', nb_iter=1, iqs=T, climat=T, sol=T, ht=T, vol=T, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  expect_equal(chk, "nb_iter doit être >=30 en mode stochastique")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu ht", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, mode_simul='DET', nb_iter=1, iqs=T, climat=T, sol=T, ht='TR', vol=T, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  expect_equal(chk, "ht doit être TRUE ou FALSE")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour vol", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, mode_simul='DET', nb_iter=1, iqs=T, climat=T, sol=T, ht=T, vol='TR', dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  expect_equal(chk, "vol doit être TRUE ou FALSE")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour iqs", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, mode_simul='DET', nb_iter=1, iqs='TR', climat=T, sol=T, ht=T, vol=T, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  expect_equal(chk, "iqs doit être TRUE ou FALSE")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour climat", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, mode_simul='DET', nb_iter=1, iqs=T, climat='TR', sol=T, ht=T, vol=T, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  expect_equal(chk, "climat doit être TRUE ou FALSE")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu pour sol", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, mode_simul='DET', nb_iter=1, iqs=T, climat=T, sol='TR', ht=T, vol=T, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  expect_equal(chk, "sol doit être TRUE ou FALSE")
})
test_that("La fonction CheckArguments() fonctionne tel qu'attendu", {
  chk = CheckArguments(file_arbre=fic1, file_etude=fic2, horizon=2, mode_simul='DET', nb_iter=1, iqs=T, climat=T, sol=T, ht=T, vol=T, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0)
  expect_equal(chk, "ok")
})



