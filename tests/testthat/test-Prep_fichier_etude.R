# fct 13: Prep_etude(): calcul de HDom, il faut des arbres debout,
#                      vérifier l'application des equations de hauteur sto et det, il faudrait ajouter un test

test_that("La fonction Prep_etude() retourne un fichier à l'échelle de la placette avec les 2 bonnes colonnes", {

  etudes <- fichier_arbres_etudes
  arbres <- fichier_arbres_aveccov %>% mutate(no_arbre=row_number())
  names(etudes) <- tolower(names(etudes))
  names(arbres) <- tolower(names(arbres))
  #arbres <- Filtrer_place(fichier=arbres) %>% mutate(iter=1)
  arbres <- arbres %>% mutate(iter=1)

  hd <- Prep_etude(fic_etude=etudes, fic_arbre=arbres, mode_simul='DET')

  nom_obtenu <- names(hd)
  nom_attendu <- c('iter' , 'id_pe', 'hd1')

  nb_plot <- nrow(hd)

  expect_equal(nom_obtenu, nom_attendu)
  expect_equal(nb_plot,2)

})

test_that("La fonction Prep_etude() fonctionne en mode stochastique", {

  etudes <- fichier_arbres_etudes
  arbres <- fichier_arbres_aveccov %>% mutate(no_arbre=row_number())
  names(etudes) <- tolower(names(etudes))
  names(arbres) <- tolower(names(arbres))
  #arbres1 <- Filtrer_place(fichier=arbres) %>% mutate(iter=1)
  #arbres2 <- Filtrer_place(fichier=arbres) %>% mutate(iter=2)
  arbres1 <- arbres %>% mutate(iter=1)
  arbres2 <- arbres %>% mutate(iter=2)
  arbres <- bind_rows(arbres1, arbres2)

  hd <- Prep_etude(fic_etude=etudes, fic_arbre=arbres, nb_iter=2, mode_simul='STO')

  nb_hd <- length(hd$hd1>0)

  expect_equal(nb_hd, 4)

})


test_that("La fonction Prep_etude() fonctionne s'il y a moins que 4 arbres dans une placette", {

  etudes <- fichier_arbres_etudes
  arbres <- fichier_arbres_aveccov %>% mutate(no_arbre=row_number())
  names(etudes) <- tolower(names(etudes))
  names(arbres) <- tolower(names(arbres))
  arbresb <- arbres %>% filter((id_pe=='0400102903' & no_arbre==22) | id_pe=='0319801702')

  #arbresc <- Filtrer_place(fichier=arbresb) %>% mutate(iter=1)
  arbresc <- arbresb %>% mutate(iter=1)

  hd <- Prep_etude(fic_etude=etudes, fic_arbre=arbresc, mode_simul='DET')

  nb_hd <- length(hd$hd1>0)

  expect_equal(nb_hd, 1)

})

test_that("La fonction Prep_etude() fonctionne s'il y a une placette sans arbres-etudes", {

  etudes <- fichier_arbres_etudes
  names(etudes) <- tolower(names(etudes))
  etudesb <- etudes %>% filter(id_pe=='0400102903')

  arbres <- fichier_arbres_aveccov %>% mutate(no_arbre=row_number())
  names(arbres) <- tolower(names(arbres))

  #arbres <- Filtrer_place(fichier=arbres) %>% mutate(iter=1)
  arbres <- arbres %>% mutate(iter=1)

  hd <- Prep_etude(fic_etude=etudesb, fic_arbre=arbres,mode_simul='DET')

  nb_hd <- length(hd$hd1>0)

  expect_equal(nb_hd, 1)

})


# À FAIRE
# tester toutes les essences pour le calcul de la ht
# tester un cas où il faut divisier les arbres pour choisir les 100 plus gros à l'ha
# faire un fichier d'arbres où l'essence d'un des 4 plus gros ne se retrouve pas de les arbres-etudes
# tester la valeur de la hd



