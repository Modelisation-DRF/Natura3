#' Fichier d'intrant à l'échelle de l'arbres pour Natura, sans les covariables
#'
#' Deux placettes avec leur liste d'arbres
#'
#' @format ## `fichier_arbres_sanscov`
#' A data frame with 29 lignes et 13 colonnes:
#' \describe{
#'   \item{essence}{Code d'essence}
#'   \item{ID_PE}{Identifiant de la placette}
#'   \item{TIGE_HA}{Nombre de tiges dans la classe de DHP=dhpcm et de l'essence=essence}
#'   \item{dhpcm}{Classe de DHP (cm)}
#'   \item{ETAT}{Code d'état de l'arbre}
#'   \item{LATITUDE}{Latitude de la placette}
#'   \item{LONGITUDE}{Latitude de la placette}
#'   \item{SDOM_BIO}{Code du sous-domaine bioclimatique}
#'   \item{REG_ECO}{Code de la région écologique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{ALTITUDE}{Altitude (m)}
#'   \item{ORIGINE}{Code de la perturbation d'origine}
#'   \item{temps}{Temps depuis l'orgine}
#'   \item{an_mes}{Année de la mesure}
#' }
#' @examples
#' fichier_arbres_sanscov
"fichier_arbres_sanscov"




#' Fichier d'intrant à l'échelle de l'arbre pour Natura, avec les covariables
#'
#' Deux placettes avec leur liste d'arbres
#'
#' @format ## `fichier_arbres_aveccov`
#' A data frame with 29 lignes et 31 colonnes:
#' \describe{
#'   \item{essence}{Code d'essence}
#'   \item{ID_PE}{Identifiant de la placette}
#'   \item{TIGE_HA}{Nombre de tiges dans la classe de DHP=dhpcm et de l'essence=essence}
#'   \item{dhpcm}{Classe de DHP (cm)}
#'   \item{ETAT}{Code d'état de l'arbre}
#'   \item{LATITUDE}{Latitude de la placette}
#'   \item{LONGITUDE}{Latitude de la placette}
#'   \item{SDOM_BIO}{Code du sous-domaine bioclimatique}
#'   \item{REG_ECO}{Code de la région écologique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{ALTITUDE}{Altitude (m)}
#'   \item{ORIGINE}{Code de la perturbation d'origine}
#'   \item{temps}{Temps depuis l'orgine}
#'   \item{iqs_pot_EPN}{IQS potentiel de EPN}
#'   \item{iqs_pot_EPB}{IQS potentiel de EPB}
#'   \item{iqs_pot_BOP}{IQS potentiel de BOP}
#'   \item{iqs_pot_PIB}{IQS potentiel de PIB}
#'   \item{iqs_pot_PEX}{IQS potentiel de PEX}
#'   \item{iqs_pot_PIG}{IQS potentiel de PIG}
#'   \item{iqs_pot_SAB}{IQS potentiel de SAB}
#'   \item{iqs_pot_THO}{IQS potentiel de THO}
#'   \item{clay}{Pourcentage d'argile dans le 0-5cm}
#'   \item{silt}{Pourcentage de limon dans le 0-5cm}
#'   \item{sand}{Pourcentage de sable dans le 0-5cm}
#'   \item{cec}{Capacité d'échange cationique dans le 0-5cm}
#'   \item{oc}{Matière organique dans le 0-5cm}
#'   \item{ph}{pH dans le 0-5cm}
#'   \item{ptot}{Précipitation totales annuelles normale 30 ans}
#'   \item{t_ma}{Température annuelle moyenne  normale 30 ans}
#'   \item{prec_gs}{Précipitation durant la saison de croissance moyenne des 10 dernières années}
#'   \item{temp_gs}{Température durant la saison de croissance moyenne des 10 dernières années}
#' }
#' @examples
#' fichier_arbres_aveccov
"fichier_arbres_aveccov"



#' Fichier d'intrant des arbres-études pour Natura
#'
#' Deux placettes avec leur liste d'arbres-études
#'
#' @format ## `fichier_arbres_etudes`
#' A data frame with 6 lignes et 5 colonnes:
#' \describe{
#'   \item{ID_PE}{Identifiant de la placette}
#'   \item{ESSENCE}{Code d'essence}
#'   \item{etage}{Code d'étage de l'arbre}
#'   \item{dhpcm}{DHP de l'arbre (cm)}
#'   \item{hauteur}{Hauteur de l'arbre (m)}
#' }
#' @examples
#' fichier_arbres_etudes
"fichier_arbres_etudes"




#' Fichier d'intrant à l'échelle de la placette pour Natura, sans les covariables
#'
#' Deux placettes
#'
#' @format ## `fichier_compile_sanscov`
#' A data frame with 2 lignes et 35 colonnes:
#' \describe{
#'   \item{id_pe}{Identifiant de la placette}
#'   \item{latitude}{Latitude de la placette}
#'   \item{longitude}{Latitude de la placette}
#'   \item{sdom_bio}{Code du sous-domaine bioclimatique}
#'   \item{reg_eco}{Code de la région écologique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{altitude}{Altitude (m)}
#'   \item{origine}{Code de la perturbation d'origine}
#'   \item{temps}{Temps depuis l'orgine}
#'   \item{an_mes}{Année de la mesure}
#'   \item{stft}{Surface terrière marchande des feuillus tolérants (m2/ha)}
#'   \item{strt}{Surface terrière marchandedes résineux tolérants (m2/ha)}
#'   \item{stri}{Surface terrière marchandedes résineux intolérants (m2/ha)}
#'   \item{stpeu}{Surface terrière marchandedes peupliers (m2/ha)}
#'   \item{stbop}{Surface terrière marchandedu bouleaux à papier (m2/ha)}
#'   \item{stepn}{Surface terrière marchandede l'épinette noire (m2/ha)}
#'   \item{stsab}{Surface terrière marchandedu sapin baumier (m2/ha)}
#'   \item{stepx}{Surface terrière marchandedes autres épinettes (m2/ha)}
#'   \item{vft}{Volume marchand des feuillus tolérants (m3/ha)}
#'   \item{vrt}{Volume marchand des résineux tolérants (m3/ha)}
#'   \item{vri}{Volume marchand des résineux intolérants (m3/ha)}
#'   \item{vpeu}{Volume marchand des peupliers (m3/ha)}
#'   \item{vbop}{Volume marchande du bouleaux à papier (m3/ha)}
#'   \item{vepn}{Volume marchand de l'épinette noire (m3/ha)}
#'   \item{vsab}{Volume marchand du sapin baumier (m3/ha)}
#'   \item{vepx}{Volume marchand des autres épinettes (m3/ha)}
#'   \item{nft}{Densité marchande des feuillus tolérants (tiges/ha)}
#'   \item{nrt}{Densité marchande des résineux tolérants (tiges/ha)}
#'   \item{nri}{Densité marchande des résineux intolérants (tiges/ha)}
#'   \item{npeu}{Densité marchande des peupliers (tiges/ha)}
#'   \item{nbop}{Densité marchande du bouleaux à papier (tiges/ha)}
#'   \item{nepn}{Densité marchande de l'épinette noire (tiges/ha)}
#'   \item{nsab}{Densité marchande du sapin baumier (tiges/ha)}
#'   \item{nepx}{Densité marchande des autres épinettes (tiges/ha)}
#'   \item{hd}{Hauteur dominante (m)}
#'   \item{is}{Indice de structure diamétrale de Shannon}
#' }
#' @examples
#' fichier_compile_sanscov
"fichier_compile_sanscov"


#' Fichier d'intrant à l'échelle de la placette pour Natura, sans les covariables
#'
#' Deux placettes
#'
#' @format ## `fichier_compile_aveccov`
#' A data frame with 2 lignes et 53 colonnes:
#' \describe{
#'   \item{id_pe}{Identifiant de la placette}
#'   \item{latitude}{Latitude de la placette}
#'   \item{longitude}{Latitude de la placette}
#'   \item{sdom_bio}{Code du sous-domaine bioclimatique}
#'   \item{reg_eco}{Code de la région écologique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{altitude}{Altitude (m)}
#'   \item{origine}{Code de la perturbation d'origine}
#'   \item{temps}{Temps depuis l'orgine}
#'   \item{stft}{Surface terrière marchande des feuillus tolérants (m2/ha)}
#'   \item{strt}{Surface terrière marchandedes résineux tolérants (m2/ha)}
#'   \item{stri}{Surface terrière marchandedes résineux intolérants (m2/ha)}
#'   \item{stpeu}{Surface terrière marchandedes peupliers (m2/ha)}
#'   \item{stbop}{Surface terrière marchandedu bouleaux à papier (m2/ha)}
#'   \item{stepn}{Surface terrière marchandede l'épinette noire (m2/ha)}
#'   \item{stsab}{Surface terrière marchandedu sapin baumier (m2/ha)}
#'   \item{stepx}{Surface terrière marchandedes autres épinettes (m2/ha)}
#'   \item{vft}{Volume marchand des feuillus tolérants (m3/ha)}
#'   \item{vrt}{Volume marchand des résineux tolérants (m3/ha)}
#'   \item{vri}{Volume marchand des résineux intolérants (m3/ha)}
#'   \item{vpeu}{Volume marchand des peupliers (m3/ha)}
#'   \item{vbop}{Volume marchande du bouleaux à papier (m3/ha)}
#'   \item{vepn}{Volume marchand de l'épinette noire (m3/ha)}
#'   \item{vsab}{Volume marchand du sapin baumier (m3/ha)}
#'   \item{vepx}{Volume marchand des autres épinettes (m3/ha)}
#'   \item{nft}{Densité marchande des feuillus tolérants (tiges/ha)}
#'   \item{nrt}{Densité marchande des résineux tolérants (tiges/ha)}
#'   \item{nri}{Densité marchande des résineux intolérants (tiges/ha)}
#'   \item{npeu}{Densité marchande des peupliers (tiges/ha)}
#'   \item{nbop}{Densité marchande du bouleaux à papier (tiges/ha)}
#'   \item{nepn}{Densité marchande de l'épinette noire (tiges/ha)}
#'   \item{nsab}{Densité marchande du sapin baumier (tiges/ha)}
#'   \item{nepx}{Densité marchande des autres épinettes (tiges/ha)}
#'   \item{hd}{Hauteur dominante (m)}
#'   \item{is}{Indice de structure diamétrale de Shannon}
#'   \item{iqs_pot_EPN}{IQS potentiel de EPN}
#'   \item{iqs_pot_EPB}{IQS potentiel de EPB}
#'   \item{iqs_pot_BOP}{IQS potentiel de BOP}
#'   \item{iqs_pot_PIB}{IQS potentiel de PIB}
#'   \item{iqs_pot_PEX}{IQS potentiel de PEX}
#'   \item{iqs_pot_PIG}{IQS potentiel de PIG}
#'   \item{iqs_pot_SAB}{IQS potentiel de SAB}
#'   \item{iqs_pot_THO}{IQS potentiel de THO}
#'   \item{clay}{Pourcentage d'argile dans le 0-5cm}
#'   \item{silt}{Pourcentage de limon dans le 0-5cm}
#'   \item{sand}{Pourcentage de sable dans le 0-5cm}
#'   \item{cec}{Capacité d'échange cationique dans le 0-5cm}
#'   \item{oc}{Matière organique dans le 0-5cm}
#'   \item{ph}{pH dans le 0-5cm}
#'   \item{ptot}{Précipitation totales annuelles normale 30 ans}
#'   \item{t_ma}{Température annuelle moyenne  normale 30 ans}
#'   \item{prec_gs}{Précipitation durant la saison de croissance moyenne des 10 dernières années}
#'   \item{temp_gs}{Température durant la saison de croissance moyenne des 10 dernières années}
#' }
#' @examples
#' fichier_compile_aveccov
"fichier_compile_aveccov"




