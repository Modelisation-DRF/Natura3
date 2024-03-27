################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function for plot compilation from tree list               #
#   including tree height and volume estimation                #
#                                                              #
#                                                              #
################################################################

#' Compilation des placettes à la step 0
#'
#' @description Compilation des placettes à la step 0: estime la hauteur et le volume des arbres si nécessaire, calcule l'indice de Shannon et calcule les caractéristiques dendrométriques (N, ST, V) par groupe d'essences.
#'
#' @param fic_arbre Table avec une ligne par arbre (ou par classe de dhp) pour chaque placette, avec les colonnes : id_pe, sdom_bio, altitude, veg_pot, dummy vp, milieu, dummy mp, type_eco, p_tot, t_ma, prec_gs, temp_gs, dummy origine, temps, iqs_xxx, ph, cec, oc, sand, clay, no_arbre, essence, dhpcm, nb_tige, tige_ha, etat, (hauteur_pred, vol_dm3 optionnels)
#' @inheritParams SimulNatura
#'
#' @return Une liste de 2 éléments: table contenant les arbres avec leur hauteur et volume (une ligne par arbre) et une table des caractérisatiques des placettes (une ligne par placette)
# #' @export
#'
# @examples
Prep_arbres <- function(fic_arbre, ht, vol, nb_iter=1, nb_step=1, mode_simul='DET', seed_value=NULL, dt=dt){

  # fic_arbre=Arbres; ht=ht; vol=vol; nb_iter=nb_iter; nb_step=1; mode_simul=mode_simul; seed_value=seed_value; dt=dt;
Data <- fic_arbre

# Vérifier la présence des variables Ht et Vol
if (isTRUE(ht)) { # si la ht n'est pas fournie, on crée la variable
  Data <- Data %>% mutate(hauteur_pred=NA)
}
if (isTRUE(vol)) { # si le volume n'est pas fourni, on crée la variable
  Data <- Data %>% mutate(vol_dm3=NA)
}
if (isFALSE(vol)) { # si le volume est fourni, on n'a pas besoin de la hauteur
  Data <- Data %>% mutate(hauteur_pred=NA)
}

# sélectionner les variables
Data1 <- Data %>%
  mutate(nb_tige = tige_ha*400/10000) #%>% # Natura utilise le nombre dans 400 m2, mais pourquoi je calcule ça ici? probablement pas necessaire
  #dplyr::select(id_pe, sdom_bio, altitude, veg_pot, contains("vp"), milieu, contains("mp"), type_eco, p_tot, t_ma, prec_gs, temp_gs, contains("orig"), temps, iqs_epn, iqs_epx, iqs_bop, iqs_peu,
  #              iqs_ri, iqs_sab, iqs_ft, iqs_rt, ph, cec, oc, sand, clay, no_arbre, essence, dhpcm, nb_tige, tige_ha, etat, hauteur_pred, vol_dm3)

# créer un fichier des info placettes: enlever altitude, p_tot, t_ma de la liste, car on en a plus besoin après avoir estimer la hauteur des arbres
#info_plac <- Data1[, c(variables_fixes_temps,"temps")]
info_plac <- Data1 %>%
  dplyr::select(-dhpcm, -etat, -essence, -tige_ha, -hauteur_pred, -vol_dm3, -nb_tige) %>% # enlever les variables à l'échelle de l'arbre
 # mutate(annee=0, tbe=0, pert=0) %>%
  group_by(id_pe) %>%
  slice(1) # ne garder qu'une ligne par placette

# info_plac <- Data1 %>%
#   dplyr::select(id_pe, sdom_bio, prec_gs, temp_gs, contains("iqs_"), temps, contains("orig"), type_eco, veg_pot,
#                 contains("vp"), milieu, contains("mp"), cec, oc, ph, sand, clay) %>%
#   mutate(annee=0, tbe=0, pert=0) %>%
#   group_by(id_pe) %>%
#   slice(1) # ne garder qu'une ligne par placette

# préparer le fichier pour mode stochastique
if (mode_simul=='STO'){
  arbreht <- Data1 %>% mutate(step=1)
  arbreht <- do.call(rbind, replicate(nb_iter, arbreht, simplify = FALSE))
  arbreht <- arbreht %>%
    group_by(id_pe, no_arbre, step) %>%
    mutate(iter = row_number()) %>%
    ungroup()
}
else {arbreht <- Data1 %>% mutate(iter=1)}


# calcul de la hauteur des arbres: Data1 doit contenir les NC, car pour la relation HD, il faut la ST incluant les NC,
if (isTRUE(ht)) {
  #DataHt <- relation_h_d(fic_arbres=Data1, mode_simul=mode_simul, iteration=iteration, parametre_ht=parametre_ht, step=1)
  DataHt <- TarifQC::relation_h_d(fic_arbre=arbreht, mode_simul=mode_simul, nb_iter=nb_iter, nb_step=nb_step, dt=dt, seed_value=seed_value)
}
else DataHt <- arbreht

# Calcul du volume des arbres: prévoit le volumme en dm3 pour un arbre entier (ne tient pas compte du nombre d'arbres)
if (isTRUE(vol)) {
  #DataHtVol <- cubage(fic_arbres=DataHt, iteration=iteration, mode_simul=mode_simul, parametre_vol=parametre_vol, step=1)
  DataHtVol <- TarifQC::cubage(fic_arbre=DataHt, mode_simul=mode_simul, nb_iter=nb_iter, nb_step=1, seed_value=seed_value)
}
else DataHtVol <- DataHt

# lire le fichier d'association des essences pour les groupes de natura (n_st_v_ass_ess.rda)
espece <- n_st_v_ass_ess %>% dplyr::select(essence, groupe_ess)

# filtrer les essences Natura et Convertir essence en groupe_ess Natura
DataHtVola <- inner_join(DataHtVol, espece, by='essence')

# compilation de N, St, Vol par placette/groupe_ess/iter
compil <- DataHtVola %>%
  group_by(iter,id_pe, groupe_ess) %>%
  mutate(vol_m3ha = vol_dm3/1000 * tige_ha,
         st_m2ha = pi * (dhpcm/2/100)^2 * tige_ha) %>%
  summarise(nb_tige = sum(tige_ha)/25, # dans Natura il faut le nb dans 400m2
            st_m2ha = sum(st_m2ha),
            vol_m3ha = sum(vol_m3ha),
            .groups="drop_last") %>%
  ungroup()

# ajouter les groupes d'essences absent du fichier de données pour toutes les iterations
gr <- data.frame('groupe_ess'=unique(espece$groupe_ess))
gr <- do.call(rbind, replicate(nb_iter, gr, simplify = FALSE))
gr <- gr %>%
  group_by(groupe_ess) %>%
  mutate(iter = row_number()) %>%
  ungroup()

compil2 <- full_join(compil, gr, by=c('iter','groupe_ess'))


#### n-st-shannon, transposer seulement l'iter 1 car les dhp sopnt tous les memes pour toutes les iter, seulement v change selon l'iter, ça va etre moins gros à rouler

# transposer les groupes d'essences en colonne pour n-st-v
compil_n <- compil2 %>%
  filter(iter==1) %>%
  dplyr::select(id_pe, groupe_ess, nb_tige) %>%
  mutate(groupe_ess = paste0(groupe_ess,'1')) %>%
  group_by(id_pe) %>%
  pivot_wider(names_from = groupe_ess, values_from = nb_tige, names_prefix = "n") %>%
  filter(!is.na(id_pe)) %>%
  replace(is.na(.),0)
  # %>%
  # mutate(ntot1 = nbop1+npeu1+nft1+nri1+nrt1+nsab1+nepn1+nepx1)

compil_st <- compil2 %>%
  filter(iter==1) %>%
  dplyr::select(id_pe, groupe_ess, st_m2ha) %>%
  mutate(groupe_ess = paste0(groupe_ess,'1')) %>%
  group_by(id_pe) %>%
  pivot_wider(names_from = groupe_ess, values_from = st_m2ha, names_prefix = "st") %>%
  filter(!is.na(id_pe)) %>%
  replace(is.na(.),0)
  # %>%
  # mutate(sttot1 = stbop1+stpeu1+stft1+stri1+strt1+stsab1+stepn1+stepx1,
  #        pct_epn1 = stepn1/sttot1*100,
  #        pct_epx1 = stepx1/sttot1*100,
  #        pct_sab1 = stsab1/sttot1*100,
  #        pct_ri1 = stri1/sttot1*100,
  #        pct_rt1 = strt1/sttot1*100,
  #        pct_bop1 = stbop1/sttot1*100,
  #        pct_peu1 = stpeu1/sttot1*100,
  #        pct_ft1 = stft1/sttot1*100)

compil_v <- compil2 %>%
  dplyr::select(iter, id_pe, groupe_ess, vol_m3ha) %>%
  mutate(groupe_ess = paste0(groupe_ess,'1')) %>%
  group_by(iter, id_pe) %>%
  pivot_wider(names_from = groupe_ess, values_from = vol_m3ha, names_prefix = "v") %>%
  filter(!is.na(id_pe)) %>%
  replace(is.na(.),0)
  # %>%
  # mutate(vtot1 = vbop1+vpeu1+vft1+vri1+vrt1+vsab1+vepn1+vepx1)

# calcul de l'indice de Shannon par placette
shanon <- DataHtVola %>%
  filter(iter==1) %>%
  # sommation des arbres par classe de 2 cm, toutes essences confondues
  mutate(st_m2ha = pi * (dhpcm/2/100)^2 * tige_ha,
         cl_dhp=round((dhpcm-0.1)/2,0)*2) %>%
  group_by(id_pe, cl_dhp) %>%
  summarise(sttiges_cls = sum(st_m2ha),.groups="drop_last") %>%
  group_by(id_pe) %>%
  mutate(st_ha_tot = sum(sttiges_cls),
         p=sttiges_cls/st_ha_tot,
         p_logp = p * log(p)
         ) %>%
  summarise(is1 = -sum(p_logp)/log(19),.groups="drop_last")

# mettre tous les fichiers compilés ensemble
compil3 <- inner_join(info_plac, shanon, by = c("id_pe"))
compil4 <- inner_join(compil3, compil_n, by = c("id_pe"))
compil5 <- inner_join(compil4, compil_st, by = c("id_pe"))
compil6 <- inner_join(compil5, compil_v, by = c("id_pe"))


fic <- list(DataHtVola, compil6)

#print("Fin Prep_arbres()")

return(fic)
}
