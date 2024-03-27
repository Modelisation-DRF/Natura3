################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Write equations for N, St, V species  roup, and Hd and Is  #
#                                                              #
#   Les équations sont dans les fichiers                       #
#   is_eq.rda,  hdevol_eq.rda et n_st_v_eq.rda                 #
#                                                              #
#   Forme des modèles pour N et ST                             #
#  if nepn[i-1] >0 then nepn[i] = exp((&nepn_a) + (&nepn_b)*log(temps[i-1]+1) + j000*(log(temps[i-1]+1))**2); else nepn[i]=0
#                                                              #
#  Forme du modele de V                                        #
#  if vepn[i-1] >0 then vepn[i] = exp(&vepn_a); else vepn[i]=0 #
#                                                              #
#  Forme du modele HD                                          #
#   a =  beta0 + beta0iqs*iqs +e1;                             #
#   c =  beta2 + beta2is*is[i-1] + beta2tbe*tbe[i] ;           #
#   hd[i] = a*hd[i-1]*temps[i]**c/(a*temps[i-1]**c+hd[i-1]*(temps[i]**c-temps[i-1]**c));
#                                                              #
#  Forme du modele IS                                          #
#  is[i] = int_is + bis_is1*is[i-1] + bis_temps/temps[i-1] + bis_dt*dt + bis_tbe*tbe;
#                                                              #
################################################################


#' Génère en code R les équations du simulateur de croissance Natura 3.0
#'
#' @description Génère en code R les équations du simulateur de croissance Natura 3.0
#'
#' @param vari Nom de la variable réponse pour laquelle on veut l'équation : n, st, v, hd ou is
#' @param ess  Code du groupe d'essences pour lequel on veut l'équation, si \code{vari} est n, st ou v : sab, epn, epx, rt, ri, bop, peu, ft. Vide pour \code{vari=is}. Si \code{vari=hd},
#'             utiliser ce paramètre pour générer chacune des parties de l'équation: a pour générer la partie associé au paramètre a, c pour générer la partie associée au paramètre c,
#'             vide pour générer la forme générale de l'équation hd
#'
#' @return Une chaine de caractères contenant l'équation pour prédire la variable \code{vari} du groupe d'essences \code{ess}
# #' @export
#'
#' @examples
#' \dontrun{
#' ecrire_eq(vari="n", ess="bop")
#' ecrire_eq(vari="is")
#' ecrire_eq(vari="hd", ess="a")
#' }
ecrire_eq <- function(vari,ess)
{
  if (vari %in% c("n","st","v"))
  {
    eqa <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_a_',vari)]
    if (vari == 'v')
    {
      eqb <- '0'
      eqc <- '0'
    }
    else if (vari != 'v')
    {
      eqb <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_b_',vari)]
      eqc <- n_st_v_eq[n_st_v_eq$essence==ess, paste0('part_c_',vari)]
      eqb <- paste0("(", eqb, ")", "* log(temps1+1)")
      eqc <- paste0("(", eqc, ")", "* (log(temps1+1))^2")
    }
    eq <- paste0("ifelse(",vari,ess,"1>0,exp( (", eqa, ")+(", eqb, ")+(", eqc,")) + res_", vari, ess,",0)")
  }
  else if (vari=='is')
  {
    eq <- paste0(is_eq$is,'+rand_plot_',vari,'+res_plot_',vari)
  }
  else if (vari=='hd')
  {
    if (ess=='a') {eq <- paste0(hdevol_eq$a, '+rand_plot_',vari)}
    else if (ess=='c') {eq <- hdevol_eq$c}
    else if (ess=='') {eq <- paste0(hdevol_eq$hd,'+res_plot_',vari)}
    else eq <- NULL
  }
  else eq <- NULL

  return(eq)
}

