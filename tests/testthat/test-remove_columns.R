test_that("La fonction remove_columns() fonctionne correctement s'il reste au moins 2 colonnes", {

  fic <- data.frame(id_pe=1:10, y=1:10, x=rep("Q",10), vp=rep("MS2",10))
  colonne <- c("x","y","z")
  res <- remove_columns(data=fic, columns_to_remove=colonne)
  res2 <- names(res)

  expect_equal(res2,c("id_pe","vp"))
})

test_that("La fonction remove_columns() fonctionne correctement s'il reste une seule colonne", {

  fic <- data.frame(id_pe=1:10, y=1:10, x=rep("Q",10))
  colonne <- c("x","y","z")
  res <- remove_columns(data=tibble(fic), columns_to_remove=colonne)
  # ça ne fonctionne pas s'il ne reste qu'une seule colonne dans le data après avoir enlevé celles demandées
  # mais ça fonctionne si c'est un tibble, mais ça ne fonctionne pas si c'est un data.frame
  res2 <- names(res)

  expect_equal(res2,"id_pe")
})

test_that("La fonction remove_columns() fonctionne correctement s'il ne reste pas de colonnes", {

  fic <- data.frame(x=1:10, y=1:10)
  colonne <- c("x","y","z")
  res <- remove_columns(data=fic, columns_to_remove=colonne)
  res2 <- length(names(res))
  #nrow(res) # 10
  # il n'y a plus de variables, mais ça dit quand même 10 obs : ok
  expect_equal(res2,0)
})

test_that("La fonction remove_columns() fonctionne correctement s'il n'y a pas de colonnes à enlever", {

  fic <- data.frame(id_pe=1:10, y=1:10, x=rep("Q",10), vp=rep("MS2",10))
  colonne <- c("z")
  res <- remove_columns(data=fic, columns_to_remove=colonne)
  res2 <- names(res)

  expect_equal(res2,names(fic))
})

##################################################################################
##################################################################################
##################################################################################

test_that("La fonction valide_placette_etudes() fonctionne correctement s'il y a des placettes en commun dans arbre et etude", {

  arbre <- data.frame(id_pe=1:10)
  etude <- data.frame(id_pe=1:15)
  res <- valide_placette_etudes(data_arbre=arbre , data_etude=etude)

  expect_equal(length(res),0)
})

test_that("La fonction valide_placette_etudes() fonctionne correctement si aucune des placettes dans arbre n'est dans etude", {

  arbre <- data.frame(id_pe=1:10)
  etude <- data.frame(id_pe=15:20)
  res <- valide_placette_etudes(data_arbre=arbre , data_etude=etude)

  expect_equal(res,"Les placettes suivantes ne sont pas dans le fichier des arbres-etudes :  1, 2, 3, 4, 5, 6, 7, 8, 9, 10")
})


