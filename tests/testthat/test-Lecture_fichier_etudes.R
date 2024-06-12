# fct 3: Lecture_etudes(): importer le fichier d'arbres-études et filtrer dhp, etage, hauteur.
#                          vérification si on n'a mis les bons noms de variables

# test_that("La fonction Lecture_etudes() fonctionne tel qu'attendu avec un dataframe", {
#
#   etu1 = fichier_arbres_etudes
#   etu2 = data.frame(ID_PE="0400102903", ESSENCE='BOP',etage='D', dhpcm=8, hauteur=10)
#   etu_tous = bind_rows(etu1, etu2)
#
#   etude = Lecture_etudes(etu_tous)
#
#   nbrow = nrow(etude)
#   nom = names(etude)
#
#   expect_equal(nbrow, 5)
#   expect_equal(nom, c("id_pe","essence", "dhpcm","hauteur"))
#
# })

test_that("La fonction Lecture_etudes() fonctionne tel qu'attendu avec un dataframe", {

  etu1 = fichier_arbres_etudes
  etude = Lecture_etudes(etu1)

  nbrow = nrow(etude)

  expect_equal(nbrow, 6)

})

test_that("La fonction Lecture_etudes() fonctionne tel qu'attendu avec un Excel", {

  fic = "./fixtures/fichier_etude_test.xlsx"
  etude = Lecture_etudes(fic)

  nbrow = nrow(etude)
  expect_equal(nbrow, 1)

})

test_that("La fonction Lecture_etudes() fonctionne tel qu'attendu avec un csv", {

  fic = "./fixtures/fichier_etude_test.csv"
  etude = Lecture_etudes(fic)

  nbrow = nrow(etude)
  expect_equal(nbrow, 1)

})

test_that("La fonction Lecture_etudes() retourne un message quand il n'y a pas les bons noms de variables", {

  fic = data.frame(ID_PE="0400102903", ess='SAB',etage='D', dhpcm=10, hauteur=10)
  etude = Lecture_etudes(fic)

  expect_equal(etude, "Nom des variables incorrect dans le fichier des arbres-etudes")

})

# test_that("La fonction Lecture_etudes() retourne un message quand il n'y a pas le bon contenu des variables", {
#
#   fic = data.frame(ID_PE="0400102903", essence='SAB', etage='d', dhpcm=10, hauteur=10)
#   etude = Lecture_etudes(fic)
#
#   expect_equal(etude, "Code d'étage à l'extérieur des valeurs possibles (C, D, I, O, V)")
#
# })


