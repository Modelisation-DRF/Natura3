


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


test_that("La fonction Graph() retourne un object ggplot même si toutes les placettes ont été rejetées", {

  fic <- readRDS(test_path("fixtures", "test_na.rds"))
  simul <- SimulNatura(file_compile = fic, horizon=1, iqs=T, sol=T, climat=T)
  expect_warning(expect_true(inherits(Graph(Data=simul, Espece = "tot", Variable = 'st'), "ggplot")),"No valid numeric values found for mean_value.")
})

test_that("La fonction Graph() retourne un object ggplot même si certaines placettes ont été rejetées", {

  fic <- readRDS(test_path("fixtures", "test_na.rds"))
  fic2 <- fic %>% mutate(latitude=50, longitude=-73)
  fic <- bind_rows(fic, fic2) %>% mutate(id_pe = row_number())
  simul <- SimulNatura(file_compile = fic, horizon=1, iqs=T, sol=T, climat=T)
  result <- Graph(Data=simul, Espece = "tot", Variable = 'st')
  expect_true(inherits(result, "ggplot"))
})


test_that("La fonction Graph() en mode STO retourne un object ggplot même si toutes les placettes ont été rejetées", {

  fic <- readRDS(test_path("fixtures", "test_na.rds"))
  simul <- SimulNatura(file_compile = fic, horizon=1, iqs=T, sol=T, climat=T, mode_simul = 'STO', nb_iter = 30)
  expect_warning(expect_true(inherits(Graph(Data=simul, Espece = "tot", Variable = 'st'), "ggplot")),"No valid numeric values found for mean_value.")
})
