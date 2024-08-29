test_that("La fonction Sortir_moyenne() calcule la moyenne des itérations correctement", {

  fic <- readRDS(test_path("fixtures", "data_simul_sto.rds"))
  res <- Sortir_moyenne(SortieIter=fic)

  nom <- names(res)
  expect_true("message" %in% nom)


  res1 <- res[1,]

  # dq attendu quand n=0
  verif_dq <- res1$dqbop
  expect_equal(verif_dq, NA_real_)

  # dq attendu quand n>0
  dq_verif <- res1$dqrt
  dq_attendu <- sqrt((res1$strt*40000)/(res1$nrt*pi))
  expect_equal(dq_verif, dq_attendu)

  # pct attendu
  pct_verif <- res1$pct_epn
  pct_attendu <- res1$stepn/res1$sttot*100
  expect_equal(pct_verif, pct_attendu)

  # nb de lignes attendu
  expect_equal(nrow(res),12)

})
