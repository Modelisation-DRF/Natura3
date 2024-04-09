#' Produit un graphique du résultat d'une simulation déterministe
#'
#' @description \code{plot_simul()} produit un graphique du résultat d'une simulation déterministe pour une variable.
#'
#' @param data Table contenant les résultats d'une simulation déterministe avec la fonction \code{SimulNatura}
#' @param variable Le nom de la variable pour laquelle on veut un graphique, ex: sttot, stepn, hd, etc.
#' @param orig TRUE: facet_wrap(~origine) (par défaut), FALSE: sans facet-wrap
#'
#' @return Un graphique

#' @export
#'
#' @examples
#' \dontrun{
#' data_simul <- SimulNatura(file_arbre=fichier_arbres_sanscov, file_etude=fichier_arbres_etudes, horizon=5)
#' plot_simul(data=data_simul, variable=sttot)
#' }
#'
plot_simul <- function(data, variable, orig=TRUE)
{
  if (isFALSE(orig)){
 eval(substitute(ggplot(data, aes(x=temps, y=variable, group=id_pe))+
    geom_line(color='black')))
  }

  if (isTRUE(orig)){
    eval(substitute(ggplot(data, aes(x=temps, y=variable, group=id_pe))+
                      facet_wrap(~origine)+
                      geom_line(color='black')))
  }


}


###########################################################################################
###########################################################################################
###########################################################################################

#' Produit un graphique par essence du résultat d'une simulation déterministe
#'
#' @description \code{plot_simul_ess()} produit un graphique par essence du résultat d'une simulation déterministe pour une variable.
#'
#' @param data Table contenant les résultats d'une simulation déterministe avec la fonction \code{SimulNatura}
#' @param variable Le nom de la variable pour laquelle on veut un graphique: 'st', 'v', 'n', ou 'dq'
#'
#' @return Un graphique par essence

#' @export
#'
#' @examples
#' \dontrun{
#' data_simul <- SimulNatura(file_arbre=fichier_arbres_sanscov, file_etude=fichier_arbres_etudes, horizon=5)
#' plot_simul_ess(data=data_simul, variable='st')
#' }
#'
plot_simul_ess <- function(data, variable, mode_simul='DET')
{
  liste_ess <- c('sab','epn','epx','rt','ri','bop','peu','ft')
  liste_var <- paste0(variable,liste_ess)

  data_t <- data %>% dplyr::select(id_pe, temps, all_of(liste_var)) %>% group_by(id_pe, temps) %>% pivot_longer(cols = all_of(liste_var), names_to = "ess", values_to = "var")

  ggplot(data_t, aes(x=temps, y=var, group=id_pe))+
    geom_line(color='black')+
    facet_wrap(~ess)

}


##########################################################################################
###########################################################################################
###########################################################################################

#' Calcul du sommaire des itérations en mode stochastique
#'
#' @description \code{sommaire_sto()} calcule le sommaire des itérations en mode stochastique.
#'
#' @details Calcule de la moyenne, du 97.5 et 2.5 percentile des itérations par placette.
#'
#' @param data Table contenant les résultats d'une simulation en mode stochastique avec la fonction \code{SimulNatura}
#' @param variable Le nom de la variable pour laquelle on veut un sommaire des itérations
#'
#' @return Une table avec la moyenne, le 2.5 et le 97.5 percentile

#' @export
#'
#' @examples
#' \dontrun{
#' data_simul_sto <- SimulNatura(file_arbre=fichier_arbres_sanscov, file_etude=fichier_arbres_etudes, horizon=5, mode_simil='STO', nb_iter=30)
#' sommaire_sto(data=data_simul_sto, variable=sttot)
#' }
sommaire_sto <- function(data, variable)
{
  eval(substitute(
    sommaire <- data %>%
    group_by(id_pe, temps) %>%
    summarise(moy = mean(variable, na.rm = T),
              p2.5 = quantile(variable, probs = 0.025, na.rm = T),
              p97.5 = quantile(variable, probs = 0.975, na.rm = T)) %>%
      ungroup()
  ))
  names(sommaire)[3:5] <- paste0(deparse(substitute(variable)), '.', names(sommaire)[3:5])
  return(sommaire)

}


##########################################################################################
###########################################################################################
###########################################################################################

#' Produit un graphique du résultat d'une simulation stochastique
#'
#' @description \code{plot_simul_sto()} produit un graphique du résultat d'une simulation stochastique pour une variable.
#'
#' @details un graphique
#'
#' @param data Table contenant les résultats d'une simulation stochastique avec la fonction \code{SimulNatura}
#' @param variable Le nom de la variable pour laquelle on veut un graphique, ex: sttot, vepn, hd
#'
#' @return Un graphique

#' @export
#'
#' @examples
#' \dontrun{
#' data_simul_sto <- SimulNatura(file_arbre=fichier_arbres_sanscov, file_etude=fichier_arbres_etudes, horizon=5, mode_simil='STO', nb_iter=30)
#' plot_simul_sto(data=data_simul_sto, variable=sttot)
#' }
#'
plot_simul_sto <- function(data, variable)
{
  eval(substitute(

    sommaire <- data %>%
      group_by(id_pe, temps) %>%
      summarise(moy = mean(variable, na.rm = T),
                lower = quantile(variable, probs = 0.05, na.rm = T),
                upper = quantile(variable, probs = 0.95, na.rm = T)) %>%
      ungroup() ))

  sommaire %>%
    ggplot(aes(x=temps, y=moy, group=id_pe))+
                    #geom_line(color='black')+
                    geom_line(aes(y=lower), color='black', linetype=2)+
                    geom_line(aes(y=upper), color='black', linetype=2)+
                    geom_ribbon(aes(ymin=lower,
                                    ymax=upper),
                                fill='gray',
                                alpha=0.5,
                                color=NA, show.legend = FALSE)+
    labs(x="Temps depuis l'origine (ans)", y=deparse(substitute(variable)))

}


##########################################################################################
###########################################################################################
###########################################################################################

