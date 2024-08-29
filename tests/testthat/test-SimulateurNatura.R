
# tester un fichier compilé
# tester un fichier echelle arbre
# tester le stochastique: avec peu de placettes et bcp de placettes, pour tester le parallèlisme qui fait des groupes de 5000 placettes

# liste des fonctions
# fct 1: CheckArguments(): ça fait juste des vérifications des paramètres. trop long à tester pour l'instant
# fct 2: Lecture_arbres(): ça fait juste importer le fichier d'arbres et garder les vivant et dhp>9. Ça fait aussi une vérification si on n'a mis les bons noms de variables, trop long à tester pour l'instant
# fct 3: Lecture_etudes(): ça fait juste importer le fichier d'arbres-études et filtrer dhp, etage, hauteur. Ça fait aussi une vérification si on n'a mis les bons noms de variables, trop long à tester pour l'instant
# fct 4: extract_map_plot(): à tester dans le package ExtractMap
# fct 5: extract_climat(): à tester dans le package ExtractMap
# fct 6: Filtrer_place(): filtre les placettes: vp, sdom, origine, crée les variables binaires associées aux variables catégoriques, pas nécessaire de tester pour l'instant
# fct 7: param_hdom0_ess_stoch(): il faudrait au moins tester que la moyenne des stochastiques donne la valeur déterministe
# fct 8: param_hdom0_stoch(): meme chose
# fct 9: param_is_evol_stoch(): meme chose
# fct 10: param_hd_evol_stoch(): meme chose
# fct 11: param_evol_n_st_v_stoch(): meme chose
# fct 12: Prep_arbres(): calcul hauteur/volume des arbres, sélectionne les essences et attribue un groupe d'essences, compile les placettes (N/ST/V, pct, shannon): il faudrait au moins faire un test avec une placette et toutes les essences pour vérifier les compilations
# fct 13: Prep_etude(): calcul de HDom, il faut des arbres debout, vérifier l'application des equations de hauteur sto et det, il faudrait ajouter un test
# fct 14: Prep_parametre(): sélectionne les paramètres associés à la bonne itération et le bon time step pour toutes les placettes, identifie l'essence dominante et sélectionne l'iqs associé. À tester.
# fct 15: ecrire_eq(): écrit l'équation à partir d'un fichier de parametre de SAS: à tester
# fct 16: Natura_oneStep(): applique les équations au fichier des placettes pour estimer les valeurs du pas suivants. À tester
# fct 17: simul_oneIter_arbre(): gère une itération complète (tous les pas de simulation) quand le fichier d'intrant est a l'echelle de l'arbre: À tester
# fct 18: Lecture_compile(): ça fait juste importer le fichier compile à la placette. Ça fait aussi une vérification si on n'a mis les bons noms de variables. Calcul des totaux et des pct. Trop long à tester pour l'instant
# fct 19: param_ishd_evol:
# fct 20: Prep_parametre_iter(): sélectionne les paramètres qui ne changent pas tout au long d'une itération (pour le cas fichier compile). À tester.
# fct 21: Prep_parametre_pas: sélectionne les paramètres associés a un pas de simulation (pour le cas fichier compile). identifie l'essence dominante et sélectionne l'iqs associé. À tester.
# fct 22: simul_oneIter_compileV2(): gère une itération complète (tous les pas de simulation) quand le fichier d'intrant est a l'echelle de la placette: À tester



test_that("La fonction SimulNatura() retourne un message d'erreur si nb_iter=1 en mode stochastique", {
  expect_error(SimulNatura(file_compile=fichier_compile_aveccov, horizon=2, mode_simul='STO', nb_iter=1),"nb_iter doit etre >=30 en mode stochastique")
})

test_that("La fonction SimulNatura() retourne un message d'erreur quand la fct CheckArguments() retourne une erreur", {
  expect_error(SimulNatura(file_compile=fichier_compile_aveccov, horizon=100, mode_simul='DET'), "horizon doit etre de 1 a 15")
})

test_that("La fonction SimulNatura() retourne un message d'erreur si climat n'est pas T ou F", {
  expect_error(SimulNatura(file_arbre = fichier_arbres_sanscov, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', climat='TR'),
               "climat doit etre TRUE ou FALSE")
})



test_that("La fonction SimulNatura() fonctionne en mode déterministe avec fichier arbres avec covariables et ht et vol à calculer", {

  expect_no_error(SimulNatura(file_arbre = fichier_arbres_aveccov, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', ht=T, vol=T, iqs=F, sol=F, climat=F))

  simul = SimulNatura(file_arbre = fichier_arbres_aveccov, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', ht=T, vol=T, iqs=F, sol=F, climat=F)

  # 2 placettes, 5 pas + temps 0 = 12 lignes
  nb_row_attendu <- 12
  nb_row_obtenu <- nrow(simul)
  expect_equal(nb_row_obtenu,nb_row_attendu)

  # nom des colonnes
  nom_attendu <- c("id_pe",    "sdom_bio", "type_eco", "origine",  "annee",    "temps",
                   "tbe",      "pert",     "hd",      "is",      "stbop",   "stpeu",
                   "stft",    "stri",    "strt",    "stsab",   "stepn",   "stepx",
                   "sttot",   "nbop",    "npeu",    "nft",     "nri",     "nrt",
                   "nsab",    "nepn",    "nepx",    "ntot",    "vbop",    "vpeu",
                   "vft",     "vri",     "vrt",     "vsab",    "vepn",    "vepx",
                   "vtot",    "pct_bop", "pct_peu", "pct_ft",  "pct_ri",  "pct_rt",
                   "pct_sab", "pct_epn", "pct_epx", "dqrt",    "dqft",    "dqri",
                   "dqepn",   "dqepx",   "dqsab",   "dqbop",   "dqpeu",   "dqtot",
                   "message")
  nom_obtenu <- names(simul)
  expect_equal(nom_obtenu,nom_attendu)

  # horizon=5 donc 50 ans
  annee_max_attendu <- 50
  anne_max_obtenu <- max(simul$annee)
  expect_equal(anne_max_obtenu,annee_max_attendu)

})

test_that("La fonction SimulNatura() fonctionne en mode déterministe avec fichier arbres avec covariables et ht et vol pas à calculer", {
  fichier_arbres_aveccov2 <- fichier_arbres_aveccov %>% mutate(vol_dm3=100, hauteur_pred=10)
  expect_no_error( SimulNatura(file_arbre = fichier_arbres_aveccov2, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', ht=F, vol=F, iqs=F, sol=F, climat=F))
})

test_that("La fonction SimulNatura() fonctionne en mode déterministe avec fichier arbres sans covariables et ht et vol à calculer", {
  expect_no_error(SimulNatura(file_arbre = fichier_arbres_sanscov, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', ht=T, vol=T, iqs=T, sol=T, climat=T))
})

test_that("La fonction SimulNatura() fonctionne en mode déterministe avec fichier arbres sans covariables et ht et vol pas à calculer", {
  fichier_arbres_sanscov2 <- fichier_arbres_sanscov %>% mutate(vol_dm3=100, hauteur_pred=10)
  expect_no_error(SimulNatura(file_arbre = fichier_arbres_sanscov2, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', ht=F, vol=F, iqs=T, sol=T, climat=T))
})




test_that("La fonction SimulNatura() fonctionne en mode déterministe avec fichier compilé avec covariables", {

  expect_no_error(SimulNatura(file_compile = fichier_compile_aveccov, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F))

  simul <- SimulNatura(file_compile = fichier_compile_aveccov, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F)

  # 2 placettes, 5 pas + temps 0 = 12 lignes
  nb_row_attendu <- 12
  nb_row_obtenu <- nrow(simul)
  expect_equal(nb_row_obtenu,nb_row_attendu)

  # nom des colonnes
  nom_attendu <- c("id_pe",    "sdom_bio", "type_eco", "origine",  "annee",    "temps",
                   "tbe",      "pert",     "hd",      "is",      "stbop",   "stpeu",
                   "stft",    "stri",    "strt",    "stsab",   "stepn",   "stepx",
                   "sttot",   "nbop",    "npeu",    "nft",     "nri",     "nrt",
                   "nsab",    "nepn",    "nepx",    "ntot",    "vbop",    "vpeu",
                   "vft",     "vri",     "vrt",     "vsab",    "vepn",    "vepx",
                   "vtot",    "pct_bop", "pct_peu", "pct_ft",  "pct_ri",  "pct_rt",
                   "pct_sab", "pct_epn", "pct_epx", "dqrt",    "dqft",    "dqri",
                   "dqepn",   "dqepx",   "dqsab",   "dqbop",   "dqpeu",   "dqtot",
                   "message")
  nom_obtenu <- names(simul)
  expect_equal(nom_obtenu,nom_attendu)

  # horizon=5 donc 50 ans
  annee_max_attendu <- 50
  anne_max_obtenu <- max(simul$annee)
  expect_equal(anne_max_obtenu,annee_max_attendu)

})

test_that("La fonction SimulNatura() fonctionne en mode déterministe avec fichier compilé sans covariables", {
  expect_no_error(SimulNatura(file_compile = fichier_compile_sanscov, horizon=5, mode_simul='DET', iqs=T, sol=T, climat=T))

})



# ajouter un test en mode déterministe qui va montrer que les résultats d'une simulation donne la même chose que sas
test_that("La fonction SimulNatura() en mode déterministe donne les même prévision que natura3 version sas sur 10 ans (avec covariable fournie et fichier compilé)", {

  pred_sas_atendu <- readRDS(test_path("fixtures", "pred_sas_atendu.rds"))
  pred_sas_atendu <- pred_sas_atendu %>% arrange(id_pe, annee)
  pep_depart <-      readRDS(test_path("fixtures", "pep_depart.rds"))  # 4297 placettes

  simul <- SimulNatura(file_compile = pep_depart, horizon=9, mode_simul='DET', iqs=F, sol=F, climat=F) %>%
    arrange(id_pe, annee)

  # n
  pred_sas_atendu_n <- pred_sas_atendu %>% dplyr::select(id_pe, annee, nepn, nepx, nri, nrt, nsab, nft, nbop, npeu, ntot)
  simul_n <- simul %>% dplyr::select(id_pe, annee, nepn, nepx, nri, nrt, nsab, nft, nbop, npeu, ntot)

  pred_sas_atendu_n2 <- pred_sas_atendu %>% dplyr::select(id_pe, annee, ntot) %>% rename(ntot_sas=ntot)
  simul_n2 <- simul_n  %>% dplyr::select(id_pe, annee, ntot) %>% rename(ntot_r=ntot)

  diff_n <- inner_join(pred_sas_atendu_n2,simul_n2, by=c("id_pe","annee")) %>%
    mutate(diff = round(ntot_r,0) - round(ntot_sas,0)) %>%
    filter(diff != 0) # ok


  # st
  pred_sas_atendu_st <- pred_sas_atendu %>% dplyr::select(id_pe, annee, stepn, stepx, stri, strt, stsab, stft, stbop, stpeu, sttot)
  simul_st <- simul %>% dplyr::select(id_pe, annee, stepn, stepx, stri, strt, stsab, stft, stbop, stpeu, sttot)

  pred_sas_atendu_st2 <- pred_sas_atendu_st %>% dplyr::select(id_pe, annee, sttot) %>% rename(sttot_sas=sttot)
  simul_st2 <- simul_st  %>% dplyr::select(id_pe, annee, sttot) %>% rename(sttot_r=sttot)

  diff_st <- inner_join(pred_sas_atendu_st2,simul_st2, by=c("id_pe","annee")) %>%
    mutate(sttot_r=round(sttot_r,2),
           sttot_sas=round(sttot_sas,2),
           diff = sttot_r - sttot_sas) %>%
    filter(annee>0,diff != 0) # 0 (il y a quelques différences à 0.01 sur le total au temps 0, car le total est fait dans r)

  # hd
  pred_sas_atendu_hd <- pred_sas_atendu %>% dplyr::select(id_pe, annee, hd, is)
  simul_hd <- simul %>% dplyr::select(id_pe, annee, hd, is)

  pred_sas_atendu_hd2 <- pred_sas_atendu_hd %>% rename(hd_sas=hd, is_sas=is)
  simul_hd2 <- simul_hd  %>% rename(hd_r=hd, is_r=is)

  diff_hd <- inner_join(pred_sas_atendu_hd2,simul_hd2, by=c("id_pe","annee")) %>%
    mutate(hd_r1=round(hd_r,1),
           hd_sas1=round(hd_sas,1),
           diff_hd = hd_r1 - hd_sas1) %>%
    filter(diff_hd != 0)

  diff_is <- inner_join(pred_sas_atendu_hd2,simul_hd2, by=c("id_pe","annee")) %>%
    mutate(is_r1=round(is_r,2),
           is_sas1=round(is_sas,2),
           diff_is = is_r1 - is_sas1) %>%
    filter(diff_is != 0) # 0

  # vol
  pred_sas_atendu_v <- pred_sas_atendu %>% dplyr::select(id_pe, annee, vepn, vepx, vri, vrt, vsab, vbop, vpeu, vft, vtot)
  simul_v <- simul %>% dplyr::select(id_pe, annee, vepn, vepx, vri, vrt, vsab, vbop, vpeu, vft, vtot)

  pred_sas_atendu_v2 <- pred_sas_atendu_v %>% dplyr::select(id_pe, annee, vtot) %>% rename(vtot_sas=vtot)
  simul_v2 <- simul_v  %>% dplyr::select(id_pe, annee, vtot) %>% rename(vtot_r=vtot)

  diff_v <- inner_join(pred_sas_atendu_v2,simul_v2, by=c("id_pe","annee")) %>%
    mutate(vtot_r=round(vtot_r,2),
           vtot_sas=round(vtot_sas,2),
           diff = vtot_r - vtot_sas) %>%
    filter(annee>0,diff != 0)

  nobs = nrow(diff_n)+nrow(diff_st)+nrow(diff_v)+nrow(diff_hd)+nrow(diff_is)

  expect_equal(nobs,0)
})



# test de file_export
# test_that("La fonction SimulNatura() export le fichier des résultats", {
#
# expect_no_error(SimulNatura(file_compile = fichier_compile_aveccov, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F, file_export = "tests\\testthat\\fixtures\\export.csv"))
#
# })



# ajouter des tests avec tbe1, tbe2, et dec_pert
test_that("La fonction SimulNatura() fonctionne comme attendu avec tbe1,tbe2,pert", {

  simul <- SimulNatura(file_compile = fichier_compile_aveccov, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F, tbe1=2, dec_tbe1 = 2, tbe2 = 3, dec_tbe2 = 3, dec_perturb = 4)

  simul2 <- simul %>% dplyr::select(tbe, annee, pert)

  expect_equal(simul2$tbe, c(0,0,2,3,0,0,
                             0,0,2,3,0,0))
  expect_equal(simul2$pert, c(0,0,0,0,1,0,
                              0,0,0,0,1,0))
})


# test_that("La fonction SimulNatura() retourne un message quand il n'y a pas le bon contenu des variables du fichier compile", {
#
#   fic = fichier_compile_aveccov %>% dplyr::select(-nsab, -nepn, -nepx, -nri,- nrt, -nft, -nbop, -npeu) %>%
#     mutate(nsab=1000, nepn=1000, nepx=1000, nri=1000, nrt=1000, nft=1000, nbop=1000, npeu=1000)
#
#   #simul <- SimulNatura(file_compile = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F)
#
#   # "La somme des nombres de tiges des 8 groupes d'essences est à l'extérieur de la plage des valeurs possibles (>0 à 5000 tiges/ha ou 200 tiges dans 400 m2)"
#   expect_error(SimulNatura(file_compile = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F))
#
# })
# tester aucune placette valide dans file_compile
test_that("La fonction SimulNatura() arrête quand toutes les placettes du file_compile sont rejetées", {

  fic = fichier_compile_aveccov %>% dplyr::select(-nsab, -nepn, -nepx, -nri,- nrt, -nft, -nbop, -npeu) %>%
    mutate(nsab=6000, nepn=1000, nepx=1000, nri=1000, nrt=1000, nft=1000, nbop=1000, npeu=1000)

  expect_error(SimulNatura(file_compile = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F), "Aucune placette valide dans le fichier file_compile")

})

# tester aucune placette valide dans arbres
test_that("La fonction SimulNatura() arrête quand toutes les placettes du fichier arbres sont rejetées", {

  fic = fichier_arbres_aveccov %>% mutate(type_eco='FE32')
  expect_error(SimulNatura(file_arbre = fic, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F),
               "Aucune placette valide dans le fichier des arbres")

})

# tester aucune placette valide dans arbres-etude
test_that("La fonction SimulNatura() arrête quand toutes les placettes du fichier arbres-etude sont rejetées", {

  fic = fichier_arbres_etudes %>% mutate(etage='I')
  expect_error(SimulNatura(file_arbre = fichier_arbres_aveccov, file_etude = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F),
               "Aucun arbre avec l'etage C ou D")

})

# tester aucun arbre valide dans arbres
test_that("La fonction SimulNatura() arrête quand tous les arbres du fichier arbres sont rejetés", {

  fic = fichier_arbres_aveccov  %>% mutate(dhpcm=8)
  expect_error(SimulNatura(file_arbre = fic, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F),
               "Aucune placette valide dans le fichier des arbres")

})

# tester avec un arbre non valide dans arbres
test_that("La fonction SimulNatura() arrête quand un arbre du fichier arbres est rejeté", {

  #fic = fichier_arbres_aveccov  %>% mutate(dhpcm=201)
  fic = fichier_arbres_aveccov  %>% mutate(essence="XXX")
  expect_error(SimulNatura(file_arbre = fic, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F),
               "Code d'essence a l'exterieur de la plage de valeurs possibles")

})

# tester avec un arbre-arbre non valide
test_that("La fonction SimulNatura() arrête quand un arbre du fichier arbres-etudes est rejeté", {

  fic = fichier_arbres_etudes %>% mutate(etage='x')
  expect_error(SimulNatura(file_arbre = fichier_arbres_aveccov, file_etude = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F),
               "Aucun arbre avec l'etage C ou D")

})


# tester aucune placette valide dans totaux
test_that("La fonction SimulNatura() arrête quand toutes les placettes ont des totaux trop grands", {

  fic = fichier_compile_aveccov %>% dplyr::select(-nsab, -nepn, -nepx, -nri,- nrt, -nft, -nbop, -npeu) %>%
    mutate(nsab=1000, nepn=1000, nepx=1000, nri=1000, nrt=1000, nft=1000, nbop=1000, npeu=1000)

  expect_error(SimulNatura(file_compile = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F),
               "Aucune placette valide selon les totaux de N, St ou V")

})



# tester quand juste quelques placettes rejetées dans arbres
test_that("La fonction SimulNatura() fonctionne quelques placettes du fichier arbres sont rejetées", {

  fic <- fichier_arbres_aveccov %>% mutate(type_eco = ifelse(type_eco=='RE20','FE32',type_eco))

  simul <- SimulNatura(file_arbre = fic, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F)

  nb_message <- nrow(simul %>% filter(!is.na(message)))
  nb_valid <- nrow(simul %>% filter(is.na(message)))

  expect_equal(nb_message,1)
  expect_equal(nb_valid,6)

})



# tester quand juste quelques placettes rejetées dans compil
test_that("La fonction SimulNatura() fonctionne quelques placettes du fichier file_compile sont rejetées", {

  fic <- fichier_compile_aveccov %>% mutate(type_eco = ifelse(type_eco=='RE22','FE32',type_eco))

  simul <- SimulNatura(file_compile = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F)

  nb_message <- nrow(simul %>% filter(!is.na(message)))
  nb_valid <- nrow(simul %>% filter(is.na(message)))

  expect_equal(nb_message,1)
  expect_equal(nb_valid,6)

})

# et juste quelques placettes rejetées dans etudes
test_that("La fonction SimulNatura() fonctionne quelques placettes du fichier arbres-études sont rejetées", {

  fic <- fichier_arbres_etudes %>% mutate(etage = ifelse(ID_PE=='0319801702','I',etage))

  simul <- SimulNatura(file_arbre = fichier_arbres_aveccov, file_etude = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F)

  nb_valid <- nrow(simul) # il n'y en a pas 12, car une placette des deux placettes n'avait plus d'arbres-etude, donc seulement 6 obs

  expect_equal(nb_valid,6)

})


# test avec iqs sol et climat dans le fichier arbres mais qu'on demande d'extraire des cartes: les variables doivent etre supprimées et la simul doit marcher sans erreur
test_that("La fonction SimulNatura() fonctionne avec un fichier arbres avec sol/iqs/climat mais qu'on demande de les extraires des cartes", {

  fic <- fichier_arbres_aveccov %>% mutate(an_mes=2000)
  expect_no_error(SimulNatura(file_arbre = fic, file_etude = fichier_arbres_etudes, horizon=5, iqs=T, sol=T, climat=T))

})
# et dans un fichier compile
test_that("La fonction SimulNatura() fonctionne avec un fichier compilé avec sol/iqs/climat mais qu'on demande de les extraires des cartes", {

  fic <- fichier_compile_aveccov %>% mutate(an_mes=2000)
  expect_no_error(SimulNatura(file_compile = fic, horizon=5, iqs=T, sol=T, climat=T))

})



#########################################################################################
#########################################################################################

# LES 4 TESTS EN MODE STOCHATIQUES RÉUSSISSENT QUAND JE FAIS DEVTOOLS::TEST(),
# MAIS QUED JE FAIS CHECK PACKAGE, CES 4 TESTS FONT UNE ERREUR À CAUSE DU MODE PARALLELE
# DONC JE LES METS EN COMMENTAIRES, MAIS À CHAQUE MODIF DU PACKAGE, IL FAUT LES PASSER MANUELLEMENT

# test_that("La fonction SimulNatura() fonctionne en mode stochastique avec fichier arbres avec covariables et ht et vol à calculer", {
#
#   #expect_no_error(SimulNatura(file_arbre = fichier_arbres_aveccov, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='STO', nb_iter = 30, ht=T, vol=T, iqs=F, sol=F, climat=F))
#
#   simul <- SimulNatura(file_arbre = fichier_arbres_aveccov, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='STO', nb_iter = 30, ht=T, vol=T, iqs=F, sol=F, climat=F)
#
#   nom_attendu <- c("id_pe",    "sdom_bio", "type_eco", "origine", "iter", "annee",    "temps",
#                    "tbe",      "pert",     "hd",      "is",      "stbop",   "stpeu",
#                    "stft",    "stri",    "strt",    "stsab",   "stepn",   "stepx",
#                    "sttot",   "nbop",    "npeu",    "nft",     "nri",     "nrt",
#                    "nsab",    "nepn",    "nepx",    "ntot",    "vbop",    "vpeu",
#                    "vft",     "vri",     "vrt",     "vsab",    "vepn",    "vepx",
#                    "vtot",    "pct_bop", "pct_peu", "pct_ft",  "pct_ri",  "pct_rt",
#                    "pct_sab", "pct_epn", "pct_epx", "dqrt",    "dqft",    "dqri",
#                    "dqepn",   "dqepx",   "dqsab",   "dqbop",   "dqpeu",   "dqtot",
#                    "message")
#   nom_obtenu <- names(simul)
#   expect_equal(nom_obtenu,nom_attendu)
#
#   # vtot au temps 0 est différent à chaque iter
#   vtot <- simul %>% filter(annee==0, id_pe=='0319801702') %>% ungroup() %>%  dplyr::select(vtot)
#   expect_true(abs(vtot[1,1]-vtot[2,1])>0)
#
# })
#
# test_that("La fonction SimulNatura() fonctionne en mode stochastique avec fichier arbres avec covariables et ht et vol pas à calculer", {
#   fichier_arbres_aveccov2 <- fichier_arbres_aveccov %>% mutate(vol_dm3=100, hauteur_pred=10)
#   #expect_no_error(SimulNatura(file_arbre = fichier_arbres_aveccov2, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='STO', nb_iter = 30, ht=F, vol=F, iqs=F, sol=F, climat=F))
#
#   simul <- SimulNatura(file_arbre = fichier_arbres_aveccov2, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='STO', nb_iter = 30, ht=F, vol=F, iqs=F, sol=F, climat=F)
#
#   # vtot au temps 0 est le même à chaque iter
#   vtot <- simul %>% filter(annee==0, id_pe=='0319801702') %>% ungroup() %>%  dplyr::select(vtot)
#   expect_true(abs(vtot[1,1]-vtot[2,1])==0)
#
# })
#
# test_that("La fonction SimulNatura() fonctionne en mode stochastique avec fichier compilé avec covariables", {
#   #expect_no_error(SimulNatura(file_compile = fichier_compile_aveccov, horizon=5, mode_simul='STO', nb_iter = 30, iqs=F, sol=F, climat=F))
#
#   simul <- SimulNatura(file_compile = fichier_compile_aveccov, horizon=5, mode_simul='STO', nb_iter = 30, iqs=F, sol=F, climat=F)
#
#   nom_attendu <- c("id_pe",    "sdom_bio", "type_eco", "origine", "iter", "annee",    "temps",
#                    "tbe",      "pert",     "hd",      "is",      "stbop",   "stpeu",
#                    "stft",    "stri",    "strt",    "stsab",   "stepn",   "stepx",
#                    "sttot",   "nbop",    "npeu",    "nft",     "nri",     "nrt",
#                    "nsab",    "nepn",    "nepx",    "ntot",    "vbop",    "vpeu",
#                    "vft",     "vri",     "vrt",     "vsab",    "vepn",    "vepx",
#                    "vtot",    "pct_bop", "pct_peu", "pct_ft",  "pct_ri",  "pct_rt",
#                    "pct_sab", "pct_epn", "pct_epx", "dqrt",    "dqft",    "dqri",
#                    "dqepn",   "dqepx",   "dqsab",   "dqbop",   "dqpeu",   "dqtot",
#                    "message")
#   nom_obtenu <- names(simul)
#   expect_equal(nom_obtenu,nom_attendu)
#
#   # vtot au temps 0 est le même à chaque iter avec un fihcier compiler
#   vtot <- simul %>% filter(annee==0, id_pe=='0700200501_N_1970') %>% ungroup() %>%  dplyr::select(vtot)
#   expect_true(abs(vtot[1,1]-vtot[2,1])==0)
# })
#
# test_that("La fonction SimulNatura() fonctionne en mode stochastique avec fichier compilé sans covariables", {
#   #expect_no_error(SimulNatura(file_compile = fichier_compile_sanscov, horizon=5, mode_simul='STO', nb_iter = 30, iqs=T, sol=T, climat=T))
#
#   simul <- SimulNatura(file_compile = fichier_compile_sanscov, horizon=5, mode_simul='STO', nb_iter = 30, iqs=T, sol=T, climat=T)
#
#   nom_attendu <- c("id_pe",    "sdom_bio", "type_eco", "origine", "iter", "annee",    "temps",
#                    "tbe",      "pert",     "hd",      "is",      "stbop",   "stpeu",
#                    "stft",    "stri",    "strt",    "stsab",   "stepn",   "stepx",
#                    "sttot",   "nbop",    "npeu",    "nft",     "nri",     "nrt",
#                    "nsab",    "nepn",    "nepx",    "ntot",    "vbop",    "vpeu",
#                    "vft",     "vri",     "vrt",     "vsab",    "vepn",    "vepx",
#                    "vtot",    "pct_bop", "pct_peu", "pct_ft",  "pct_ri",  "pct_rt",
#                    "pct_sab", "pct_epn", "pct_epx", "dqrt",    "dqft",    "dqri",
#                    "dqepn",   "dqepx",   "dqsab",   "dqbop",   "dqpeu",   "dqtot",
#                    "message")
#   nom_obtenu <- names(simul)
#   expect_equal(nom_obtenu,nom_attendu)
#
# })


# # ajouter test quand lecture_arbres, lecture_etude ou lecture_compile retourne une erreur
# test_that("La fonction SimulNatura() retourne un message quand il n'y a pas le bon contenu des variables du fichier arbre", {
#
#   fic <- fichier_arbres_aveccov %>% mutate(type_eco = ifelse(type_eco=='RE20','FE32',type_eco))
#
#   simul <- SimulNatura(file_arbre = fic, file_etude = fichier_arbres_etudes, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F)
#
#   # "La somme des nombres de tiges des 8 groupes d'essences est à l'extérieur de la plage des valeurs possibles (>0 à 5000 tiges/ha ou 200 tiges dans 400 m2)"
#   expect_error(SimulNatura(file_compile = fic, horizon=5, mode_simul='DET', iqs=F, sol=F, climat=F))
#
# })



# ajouter un test en mode stochastique avec seed.value qui va montrer qu'on obtient toujours les mêmes résulats à chaque fois qu'on exécute les tests
# on ne pourra pas le comparer à un résultat externe car il n'y a pas d'autres version de natura 3.0 stochastique






