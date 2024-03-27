################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function simulating one step with Natura model             #
#                                                              #
#   Use n_st_max.rda                                           #
#                                                              #
################################################################

#' Simulation d'un pas de simulation (période) avec le modèle Natura
#'
#' @description Simulation d'un pas de simulation (période) avec le modèle Natura, habituellement de 10 ans.
#'
#' @param data Table contenant les placettes et leurs caractéristiques dendrométriques (Nxxx, STxxx, Vxxx, HD, IS). Une ligne par placette. Ne doit pas contenir les covariables ni les paramètres.
#' @param param Table contenant les paramètres du modèle associés à chacune des placettes. Une ligne par placette.
#' @param data_info Table contenant les covariables fixes dans le temps pour chacune des placettes. Une ligne par placette.
#' @param long_int Nombre d'années d'un pas de simulation.
#' @param pas Le numéro du pas de simulation à simuler
#' @inheritParams SimulNatura
#'
#' @return Table contenant les caractéristiques dendrométriques prédites par le modèle pour le pas de simulation demandé. Une ligne par placette.
# #' @export
#'
# @examples
Natura_oneStep <- function(data, param, data_info, long_int, pas, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0){


  # data=DataCompile_final; param=Data_param; data_info=data_info; long_int=long_int; pas=k;
  # dec_perturb=dec_perturb; dec_tbe1=dec_tbe1; tbe1=tbe1; dec_tbe2=dec_tbe2; tbe2=tbe2


  # Après coupe forcé à 0
  apresc <- 0
  ind_apresc <- apresc

  # si on est à la décennie de la perturbation, mettre la variable indicatrice de perturb à 1
  if (pas==dec_perturb) {ind_pert <- 1}
  else {ind_pert <- 0}

  # si on est à la décennie de la tbe , mettre la variable tbe à la valeur desiree
  if (pas==dec_tbe1) {
    ind_tbe <- tbe1
  } else if (pas==dec_tbe2) {
    ind_tbe <- tbe2
  } else {ind_tbe <- 0}

  # s'il y a eu de la TBE ou de la perturb à la décennie précédente, on met la variable indicatrice apresp à 1
  if ((dec_tbe1>0 & pas==dec_tbe1+1) | (dec_tbe2>0 & pas==dec_tbe2+1) | (dec_perturb>0 & pas==dec_perturb+1)) {apresp <- 1}
  else {apresp <- 0}
  ind_apresp <- apresp


  # fichier en début de période
  names(data) <- tolower(names(data))
  names(data_info) <- tolower(names(data_info))
  names(param) <- tolower(names(param))

  Plac <- inner_join(data, data_info,by='id_pe') %>%
    inner_join(param, by='id_pe') %>%
    ungroup() %>%
    mutate(dt = long_int,
           annee = pas*long_int,
           temps1 = temps,
           temps = temps+long_int,
           tbe = ind_tbe, pert = ind_pert, apresc = ind_apresc, apresp = ind_apresp)

  #print("debut ecrire eq n-st")
  # Creating equation of N et ST for each species group
  liste_var <-  c('nsab','nrt','nepn','nepx','nri','nbop','npeu','nft',
                  'stsab','strt','stepn','stepx','stri','stbop','stpeu','stft')
  suppressMessages(
  vari <- bind_cols(lapply(liste_var, function(x) {
    x1 <- paste0(x,1)
    if (grepl('st',x)==TRUE) {
      varia <- 'st'
      ess = substr(x,3,5)
      max <- as.numeric(n_st_max[n_st_max$ess==ess, 3])
    }

    if (grepl('st',x)==FALSE) { # pour n
      varia <- substr(x,1,1)
      ess = substr(x,2,4)
      max <- as.numeric(n_st_max[n_st_max$ess==ess, 2])
    }
    eq = tolower(ecrire_eq(varia,ess))
    temp <- Plac %>%
      mutate(nom2 = eval(parse(text = eq)),
             nom = ifelse(nom2<0, 1,
                          ifelse(nom2>max,max,nom2))) %>% #### pour les cas où avec erreur résiduelle ça tombe dans le negatif, je vais mettre 1 au lieu de 0, sinon, ça se peut que tous les gr ess soit à 0 et le calcul de pct_ess ne marchera pas
                                                          # je vais aussi mettre des max car ça monte souvent à l'infini en stochastique
      dplyr::select(nom)
    }
    )))
  names(vari) <- liste_var
  #print("fin ecrire eq n-st")
  Plac2 <- bind_cols(Plac,vari)

  # pour Hd et Is
  Plac2 <- Plac2 %>%
    mutate(a = eval(parse(text = tolower(ecrire_eq('hd','a')))),
           a = ifelse(a<0,2,a),
           c = eval(parse(text = tolower(ecrire_eq('hd','c')))),
           hd = eval(parse(text = tolower(ecrire_eq('hd','')))),
           hd = ifelse(hd<0,2,hd),
           is = eval(parse(text = tolower(ecrire_eq('is','')))),
           is = ifelse(is<0,0,is)
           ) %>%
    dplyr::select(-a,-c)

  # ensuite faire les V
  liste_var <-  c('vsab','vrt','vepn','vepx','vri','vbop','vpeu','vft')
  suppressMessages(
  vari <- bind_cols(lapply(liste_var, function(x) {
    if (grepl('st',x)==TRUE) {
      varia <- 'st'
      ess = substr(x,3,5)
    }
    if (grepl('st',x)==FALSE) { # pour n et v
      varia <- substr(x,1,1)
      ess = substr(x,2,4)
    }
    eq = tolower(ecrire_eq(varia,ess))
    temp <- Plac2 %>%
      mutate(nom2 = eval(parse(text = eq)),
             nom = ifelse(nom2<0, 1, nom2)) %>% #### pour les cas où avec erreur résiduelle ça tombe dans le negatif
      dplyr::select(nom)
  }
  )))
  #print("fin ecrire eq V")
  names(vari) <- liste_var

  # calculer les totaux et les proportions
  Plac3 <- bind_cols(Plac2,vari) %>%
    mutate(ntot = nsab+nepn+nepx+nrt+nri+nbop+npeu+nft,
           sttot = stsab+stepn+stepx+strt+stri+stbop+stpeu+stft,
           vtot = vsab+vepn+vepx+vrt+vri+vbop+vpeu+vft,
           pct_epn = stepn/sttot*100,
           pct_epx = stepx/sttot*100,
           pct_sab = stsab/sttot*100,
           pct_ft = stft/sttot*100,
           pct_bop = stbop/sttot*100,
           pct_peu = stpeu/sttot*100,
           pct_ri = stri/sttot*100,
           pct_rt = strt/sttot*100) %>%
    dplyr::select(-dt,-temps1, -apresc, -apresp)

  # enlever les info placette et les parametres
  Plac3[colnames(data_info)[-1]]<- list(NULL)
  Plac3[colnames(param)[-1]]<- list(NULL)
  # il faut aussi enlever les variables du temps 1
  nom1 <- grep('1',colnames(Plac3)[-1], value=TRUE)
  Plac3[nom1]<- list(NULL)

  # ajouter un 1 au bout du nom des variables
  nom0 <- unlist((lapply(nom1, function(x) substr(x,1,nchar(x)-1))))
  # position des variables nom0
  for (j in nom0) {
    colnames(Plac3)[grep(j,colnames(Plac3))] <- paste0(j,1)
  }

  #print("fin Natura_oneStep()")

  return(Plac3)
}
