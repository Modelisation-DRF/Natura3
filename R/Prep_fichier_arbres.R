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

#' Plot compilation from tree list
#'
#' @description Estimate tree height and/or tree volume if necessary, calculate shanon index from trees dhp, calculate stand characterics (n-st-v) per species group
#'
#' @param fic_arbre Dataframe with one line per tree per plot, with variables : id_pe, sdom_bio, altitude, veg_pot, dummy vp, milieu, dummy mp, type_eco, p_tot, t_ma, prec_gs, temp_gs, dummy orig, temps, all iqs, ph, cec, oc, sand, clay, silt, no_arbre, essence, dhpcm, nb_tige, tige_ha, etat, hauteur_pred, vol_dm3
#' @param iteration Iteration number
#' @param parametre_ht Object with the tree height model parameter values
#' @param parametre_vol Object with the tree volume model parameter values
#' @param liste_ess_ht Vector of species code name with a tree height model
#' @inheritParams SimulNatura
#'
#' @return A list of 2 elements: dataframe of tree characteristics (with height and volume) and dataframe of plot characteristics ready to be simulated
#' @export
#'
# @examples
Prep_arbres <- function(fic_arbre, ht, vol, iteration, parametre_ht, parametre_vol, mode_simul, liste_ess_ht){

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
  mutate(nb_tige = tige_ha*400/10000) %>%
  dplyr::select(id_pe, sdom_bio, altitude, veg_pot, contains("vp"), milieu, contains("mp"), type_eco, p_tot, t_ma, prec_gs, temp_gs, contains("orig"), temps, iqs_epn, iqs_epx, iqs_bop, iqs_peu,
         iqs_ri, iqs_sab, iqs_ft, iqs_rt, ph, cec, oc, sand, clay, silt, no_arbre, essence, dhpcm, nb_tige, tige_ha, etat, hauteur_pred, vol_dm3)

# créer un fichier des info placettes
info_plac <- Data1 %>%
  dplyr::select(id_pe, sdom_bio, altitude, p_tot, t_ma, prec_gs, temp_gs, contains("iqs_"), temps, contains("orig"), type_eco, veg_pot,
                contains("vp"), milieu, contains("mp"), cec, oc, ph, sand, silt, clay) %>%
  mutate(annee=0, tbe=0, pert=0) %>%
  group_by(id_pe) %>%
  slice(1)

# calcul de la hauteur des arbres: Data1 doit contenir les NC, car pour la relation HD, il faut la ST incluant les NC,
if (isTRUE(ht)) {
  DataHt <- relation_h_d(fic_arbres=Data1, mode_simul=mode_simul, iteration=iteration, parametre_ht=parametre_ht)
}
else DataHt <- Data1

# Calcul du volume des arbres: prévoit le volumme en dm3 pour un arbre entier (ne tient pas compte du nombre d'arbres)
if (isTRUE(vol)) {
  DataHtVol <- cubage(fic_arbres=DataHt, iteration=iteration, mode_simul=mode_simul, parametre_vol=parametre_vol)
}
else DataHtVol <- DataHt

# lire le fichier d'association des essences pour les groupes de natura (n_st_v_ass_ess.rda)
espece <- n_st_v_ass_ess %>% dplyr::select(essence, groupe_ess)

# filtrer les essences Natura et Convertir essence en groupe_ess Natura
DataHtVola <- inner_join(DataHtVol, espece, by='essence')

# compilation de N, St, Vol par placette/groupe_ess
compil <- DataHtVola %>%
  group_by(id_pe, groupe_ess) %>%
  mutate(vol_m3ha = vol_dm3/1000 * tige_ha,
         st_m2ha = pi * (dhpcm/2/100)^2 * tige_ha) %>%
  summarise(nb_tige = sum(tige_ha)/25, # dans Natura il faut le nb dans 400m2
            st_m2ha = sum(st_m2ha),
            vol_m3ha = sum(vol_m3ha),
            .groups="drop_last") %>%
  ungroup()

# ajouter les groupes d'essences absent du fichier de données
gr <- data.frame('groupe_ess'=unique(espece$groupe_ess))
compil2 <- full_join(compil, gr, by='groupe_ess')

# transposer les groupes d'essences en colonne pour n-st-v
compil_n <- compil2 %>%
  dplyr::select(id_pe, groupe_ess, nb_tige) %>%
  mutate(groupe_ess = paste0(groupe_ess,'1')) %>%
  group_by(id_pe) %>%
  pivot_wider(names_from = groupe_ess, values_from = nb_tige, names_prefix = "n") %>%
  filter(!is.na(id_pe)) %>%
  replace(is.na(.),0) %>%
  mutate(ntot1 = nbop1+npeu1+nft1+nri1+nrt1+nsab1+nepn1+nepx1)

compil_st <- compil2 %>%
  dplyr::select(id_pe, groupe_ess, st_m2ha) %>%
  mutate(groupe_ess = paste0(groupe_ess,'1')) %>%
  group_by(id_pe) %>%
  pivot_wider(names_from = groupe_ess, values_from = st_m2ha, names_prefix = "st") %>%
  filter(!is.na(id_pe)) %>%
  replace(is.na(.),0) %>%
  mutate(sttot1 = stbop1+stpeu1+stft1+stri1+strt1+stsab1+stepn1+stepx1,
         pct_epn1 = stepn1/sttot1*100,
         pct_epx1 = stepx1/sttot1*100,
         pct_sab1 = stsab1/sttot1*100,
         pct_ft1 = stft1/sttot1*100,
         pct_bop1 = stbop1/sttot1*100,
         pct_peu1 = stpeu1/sttot1*100,
         pct_ri1 = stri1/sttot1*100,
         pct_rt1 = strt1/sttot1*100)

compil_v <- compil2 %>%
  dplyr::select(id_pe, groupe_ess, vol_m3ha) %>%
  mutate(groupe_ess = paste0(groupe_ess,'1')) %>%
  group_by(id_pe) %>%
  pivot_wider(names_from = groupe_ess, values_from = vol_m3ha, names_prefix = "v") %>%
  filter(!is.na(id_pe)) %>%
  replace(is.na(.),0) %>%
  mutate(vtot1 = vbop1+vpeu1+vft1+vri1+vrt1+vsab1+vepn1+vepx1)

# calcul de l'indice de Shannon par placette
shanon <- DataHtVola %>%
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
compil3 <- inner_join(info_plac, compil_n, by = "id_pe")
compil4 <- inner_join(compil3, compil_st, by = "id_pe")
compil5 <- inner_join(compil4, compil_v, by = "id_pe")
compil6 <- inner_join(compil5, shanon, by = "id_pe")

fic <- list(DataHtVola, compil6)

#print("Fin Prep_arbres()")

return(fic)
}
