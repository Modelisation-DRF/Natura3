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

#' Simulation of one time-step with Natura 3.0 growth model
#'
#' @description Simulation of one time-step with Natura 3.0 growth model, usually of 10 years, of the growth of characteristics of a stand (number of trees, basal area and volume per group species, dominant height and shannon index), based on plot origine and time since origine.
#'
#' @param data Dataframe to be simulated. One line per plot. Must contain only the variables associated to stands characteristics to be simulated and plot id (no covariates an no paramteers)
#' @param param Dataframe (one line per plot) of parameter values of all the models of Natura 3.0 for each plot
#' @param data_info Dataframe (one line per plot) with only variables associated to fixed stand characteristics over time
#' @param long_int Lenght of simulation time step in years
#' @param pas The simulation step number
#' @inheritParams SimulNatura
#'
#' @return Dataframe of predicted stand characteristics
#' @export
#'
# @examples
Natura_oneStep <- function(data, param, data_info, long_int, pas, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0){

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
                                                          # je vais aussi mettre des max car ça monte souvewnt à l'infini en stochastique
      dplyr::select(nom)
    }
    )))
  names(vari) <- liste_var
  #print("fin ecrire eq n-st")
  Plac2 <- bind_cols(Plac,vari)

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

  # ensuite faire les v
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
