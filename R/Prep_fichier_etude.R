################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function for step 0 plot dominant height estimation        #
#   from study trees and top trees                             #
#                                                              #
#   Use n_st_v_ass_ess.rda                                     #
#                                                              #
################################################################

#' Calcule la hauteur dominante à la step 0
#'
#' @description Calcule la hauteur dominante des placettes à la step 0 à partir des arbres-études et du fichier des arbres.
#'
#' @param fic_etude Table des arbres-études
#' @param fic_arbre Tables des arbres averc leur hauteur estimée, avec les itérations si stochastiques
#' @inheritParams SimulNatura
#'
#' @return Table contenant la hauteur dominante de chaque placette, une ligne par placette/iter.
#' @export
#'
# @examples
Prep_etude <- function(fic_etude, fic_arbre, nb_iter=1, mode_simul='DET', seed_value = NULL){

  # fic_etude=EtudeA; fic_arbre=Arbres_prep; nb_iter=nb_iter; mode_simul=mode_simul; seed_value = seed_value;
  # fic_etude=etudes; fic_arbre=arbres; nb_iter=1; mode_simul='DET'; seed_value = seed_value;

  # générer les paramètres des modèles utilisés
  #liste_arbre <-  fic_arbre[fic_arbre$iter==1,c('id_pe', 'no_arbre')]
  liste_arbre <- fic_arbre %>% dplyr::select(id_pe, no_arbre) %>% unique()
  param_hdom0_ess <- param_hdom0_ess_stoch(liste_arbre=liste_arbre, nb_iter=nb_iter, mode_simul=mode_simul, seed_value = seed_value)
  param_hdom0_global <- param_hdom0_stoch(liste_arbre=liste_arbre, nb_iter=nb_iter, mode_simul=mode_simul, seed_value = seed_value)

  # on a besoin de la hauteur moyenne et du dhp moyen des arbre-étude C-D par placette/essence

  # calculer dhp et ht moyens des AE par placette/essence
  etude_moy <- fic_etude %>%
    group_by(id_pe, essence) %>%
    summarise(hauteurm_ess = mean(hauteur),
              dhpcm_ess  = mean(dhpcm),
              .groups = 'drop_last')

  # calculer dhp et ht moyens des AE par placette
  etude_moy_tous <- fic_etude %>%
    group_by(id_pe) %>%
    summarise(hauteurm_ess = mean(hauteur),
              dhpcm_ess  = mean(dhpcm),
              .groups = 'drop_last')

# sélectionner les 4 plus gros dhp par placette toutes essence confondues dans le fichier des arbres
tiges4 <- fic_arbre %>%
  filter(etat %in% c(10,30,40,50)) %>%  # arbre debout seulement
  dplyr::select(iter, id_pe, essence, no_arbre, dhpcm, tige_ha) %>%
  arrange(iter, id_pe, desc(dhpcm)) %>%
  group_by(iter, id_pe) %>%
  mutate(somme_poids = cumsum(tige_ha),
         somme_poids_prec = lag(somme_poids),
         somme_poids_prec = ifelse(is.na(somme_poids_prec),0,somme_poids_prec),
         select_id = ifelse(somme_poids_prec>=100 & somme_poids>100,0,1)) %>%
  filter(select_id==1) %>%
  mutate(poids = ifelse(somme_poids<=100, tige_ha, 100-somme_poids_prec))

# ajouter l'essence à utiliser pour l'équation de ht dominante au fichier des 4 arbres (n_st_v_ass_ess.rda)
espece <- n_st_v_ass_ess %>% dplyr::select(essence, ess_eq_hd)

tiges4a <- inner_join(tiges4, espece, by="essence") # on laisse tomber les arbres d'essence qui ne sont pas dans le fichier des especes

# ajouter la moyenne des AE au fichier des arbres et ne garder que les arbres dont l'essence est dans les AE
tiges4b <- inner_join(tiges4a, etude_moy, by=c("id_pe", "essence"))

# faire un fichier des arbres dont l'essence n'est pas dans les arbres-etudes
autre <- anti_join(tiges4a, etude_moy, by=c("id_pe", "essence")) #  return all rows from x without a match in y.
# ajouter la moyenne des AE par placette
autre2 <- inner_join(autre, etude_moy_tous, by="id_pe") # on va perdre les placettes qui n'ont pas d'AE


# ajouter le paramètre de l'equation au fichier des arbres et calculer la ht
liste_ess <- names(param_hdom0_ess)
param_hd <- bind_rows(lapply(liste_ess, function(x) param_hdom0_ess[[x]]$effet_fixe)) # une liste par essence
tiges4c <- inner_join(tiges4b, param_hd, by=c("iter", "ess_eq_hd"))

if (mode_simul=='STO'){
  # erreur résiduelle et random arbre: une ligne par placette/arbre/essence
  erreur <- bind_rows(lapply(liste_ess, function(x) param_hdom0_ess[[x]]$erreur_arbre))
  tiges4d <- left_join(tiges4c, erreur, by = c("iter", "id_pe","no_arbre","ess_eq_hd"))
}
else{
  tiges4d <- tiges4c %>% mutate(random_arbre=0, resid_arbre=0)
}

tiges4d <- tiges4d %>%
  mutate(ht_pred = 1.3 + (dhpcm /(  (dhpcm_ess/(hauteurm_ess-1.3)) + b2*(dhpcm-dhpcm_ess) ) ) + random_arbre + resid_arbre )

# ajouter le parametre et calculer la ht des arbres d'essence qu'on retrouve pas dans les AE
param_hd_tous <- bind_rows(lapply(1:nb_iter, function(x) param_hdom0_global[[x]]$effet_fixe))
if (mode_simul=='STO')
  {
  param_hd_erreur <- bind_rows(lapply(1:nb_iter, function(x) param_hdom0_global[[x]]$erreur_arbre))
  autre2a <- inner_join(autre2, param_hd_erreur, by=c('iter', 'id_pe', 'no_arbre'))
}
else
  {
    autre2a <- autre2 %>% mutate(random_arbre=0, resid_arbre=0)
}

autre3 <- left_join(autre2a, param_hd_tous, by='iter') %>%
  mutate(ht_pred = 1.3 + (dhpcm /(  (dhpcm_ess/(hauteurm_ess-1.3)) + b2*(dhpcm-dhpcm_ess) ) ) + random_arbre + resid_arbre  )

# mettre les 2 fichiers ensemble
tiges_tous <- bind_rows(tiges4d, autre3) %>% arrange(iter, id_pe, somme_poids)

# faire la moyenne des 4 arbres par placette, les 100 plus gros à l'ha
# faire la moyenne pondérée par le poids
hd <- tiges_tous %>%
  group_by(iter, id_pe) %>%
  summarise(hd1 = sum(poids*ht_pred)/sum(poids),
            nb = sum(poids)) %>%
  filter(nb==100) %>%  # on ne garde pas les placettes qui n'ont pas au moins 4 arbres ou 100/ha
  dplyr::select(-nb)

#print("Fin Prep_etude()")
return(hd)
}
