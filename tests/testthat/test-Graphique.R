


test_that("La fonction Graph() retourne un object ggplot", {

  fic <- readRDS(test_path("fixtures", "data_simul.rds"))
  result <- Graph(Data=fic, Espece = "tot", Variable = 'st')

  expect_true(inherits(result, "ggplot"))
})

test_that("La fonction Graph() retourne un object ggplot quand il y a des NA pour dq", {

  fic <- readRDS(test_path("fixtures", "data_simul.rds")) %>% filter(id_pe=="0700200501_N_1970")

  #expect_warning(Graph(Data=fic, Espece = "ft", Variable = 'dq'),"No valid numeric values found for mean_value.")

  expect_warning(expect_true(inherits(Graph(Data=fic, Espece = "ft", Variable = 'dq'), "ggplot")),"No valid numeric values found for mean_value.")

})


test_that("La fonction Graph() fonctionne s'il y a des itérations", {

  fic <- readRDS(test_path("fixtures", "data_simul_sto.rds"))
  result <- Graph(Data=fic, Espece = "tot", Variable = 'st')

  expect_true(inherits(result, "ggplot"))
})
