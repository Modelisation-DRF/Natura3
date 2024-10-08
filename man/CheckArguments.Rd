% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Verification_arguments.R
\name{CheckArguments}
\alias{CheckArguments}
\title{Vérification des arguments de la fonction principale du simulateur Natura}
\usage{
CheckArguments(
  file_arbre,
  file_etude,
  file_compile,
  horizon,
  mode_simul,
  nb_iter,
  iqs,
  climat,
  sol,
  ht,
  vol,
  dec_perturb,
  dec_tbe1,
  tbe1,
  dec_tbe2,
  tbe2
)
}
\arguments{
\item{file_arbre}{Nom du fichier contenant les informations sur les arbres et les placettes (table, xlsx ou csv). Le fichier doit contenir une ligne par arbre ou par classe de dhp/essence.
Si ce paramètre n'est pas utilisé, \code{file_compile} doit l'être. Le nom des colonnes doit être:
 \itemize{
   \item id_pe: identifiant unique de la placette
   \item origine: code d'origine de la placette (ES: épidémie sévère, BR: brulis, CT: coupe totale)
   \item temps: temps depuis l'origine (ans)
   \item type_eco: code du type écologique (ex: MS22)
   \item essence: code de l'essences de l'arbre (ex: SAB, EPN, BOP)
   \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (seuls les arbres de plus de 9 cm sont retenus)
   \item tige_ha: nombre d'arbres de l'essence et de la classe de dhp, à l'ha
   \item etat: code d'état de l'arbre, seuls les vivants sont retenus, codes 10, 12, 30, 32, 40, 42, 50, 52
   \item sdom_bio: code du sous-domaine bioclimatique, en majuscule (ex: 2E, 4O).
   \item altitude: altitude (m). Optionel: nécessaire seulement si la hauteur des arbres est à estimer
   \item p_tot: précipitations totales annuelles, moyenne sur la période 1980-2010 (mm). Optionel: nécessaire seulement si la hauteur des arbres est à estimer
   \item t_ma: température moyenne annuelle sur la période 1980-2010 (Celcius). Optionel: nécessaire seulement si la hauteur des arbres est à estimer
   \item haut_pred: hauteur de l'arbre (m). Optionnel: peut être estimer avec le package TarifQC en utilisant \code{ht=T}
   \item vol_dm3: volume de l'arbre ou volume d'un arbre de la classe de dhp  (dm3). Optionnel: peut être estimer avec le package TarifQC en utilisant \code{vol=T}
   \item latitude, longitude: coordonnées des placettes, en degré décimal. Optionel: nécessaires si \code{iqs=T} ou \code{sol=T} ou \code{climat=T}
   \item an_mes: année de mesure de la placette. Optionnel, nécessaire si \code{climat=T}
   \item propriétés de sol SIIGSOL dans la profondeurt 0-5 cm: ph, clay (proportion d'argile (pct)), sand (pct) proportion de sable (pct)), cec (capacité d'échange cationique (méq/100g)), oc (proportion de matière organique (pct)). Optionnel: peuvent être récupérés avec le package ExtractMap en utilisant \code{sol=T}
   \item IQS potentiels (m, hauteur dominante à 50 ans): iqs_pot_sab, iqs_pot_epn, iqs_pot_epb, iqs_pot_pig, iqs_pot_tho, iqs_pot_pib, iqs_pot_bop, iqs_pot_pex (peupliers). Optionel, peuvent être récupérés avec le package ExtractMap en utilisant \code{iqs=T}
   \item variable climatiques: prec_gs (précipitations durant la saison de croissance, moyenne sur les 10 dernières années avant le mesurage de la placette (mm), temp_gs (température moyenne durant la saison de croissance, moyenne sur les 10 dernières années avant le mesurage de la placette (Celcius)). Optionnel: peuvent être récupérés avec le package ExtractMap en utilisant \code{climat=T}
 }}

\item{file_etude}{Nom du fichier contenant les arbres-études (table, xlsx ou csv). Le fichier doit contenir une ligne par arbre.
Si ce paramètre n'est pas utilisé, \code{file_compile} doit l'être. Le nom des colonnes doit être:
\itemize{
  \item id_pe: identifiant unique de la placette
  \item etage: code d'étage de l'arbre: C, D, O, I, V, seuls les C et D sont retenus
  \item essence: code de l'essences de l'arbre-étude (ex: SAB, EPN, BOP)
  \item dhpcm: dhp de l'arbre-étude (cm)
  \item hauteur: hauteur totale de l'arbre-étude (m)
}}

\item{file_compile}{Optionnel. Nom du fichier contenant les caractéristiques dendrométriques des placettes (table, xlsx ou csv). Une ligne par placette.
 Si ce paramètre n'est pas utilisé, \code{file_arbre} et \code{file_etude} doivent l'être. La composition de chacun des groupes d'essences est décrite dans la section Details.
 Le nom des colonnes doit être:
 \itemize{
   \item id_pe: identifiant unique de la placette
   \item origine: code d'origine de la placette (ES: épidémie sévère, BR: brulis, CT: coupe totale)
   \item temps: temps depuis l'origine (ans)
   \item type_eco: code du type écologique (ex: MS22)
   \item sdom_bio: code du sous-domaine bioclimatique, en majuscule (ex: 2E, 4O).
   \item is: indice de structure diamètrale de Shannon (valeur entre 0 et 1; 0 : toutes les tiges sont dans la même classe de dhp; 1 : les tiges sont également distribuées dans les classes de dhp)
   \item hd: hauteur dominante, hauteur moyenne des 100 plus gros arbres à l'ha (m)
   \item nxxx : nombre d'arbres de plus de 9 cm du groupe d'essences xxx (/ha), mettre 0 si absent. xxx : chacun des groupes d'essences (bop, peu, ft, ri, rt, epx, epn, sab)
   \item stxxx: surface terrière marchande du groupe d'essences xxx (m2/ha), mettre 0 si absent. xxx : chacun des groupes d'essences (bop, peu, ft, ri, rt, epx, epn, sab)
   \item vxxx: volume marchand brut du groupe d'essences xxx (m3/ha), mettre 0 si absent. Si aucun volume, mettre les 8 groupes à 0 et l'évolution du volume ne sera pas effectué. xxx : chacun des groupes d'essences (bop, peu, ft, ri, rt, epx, epn, sab)
   \item propriétés de sol SIIGSOL dans la profondeurt 0-5 cm: ph, clay (proportion d'argile (pct)), sand (pct) proportion de sable (pct)), cec (capacité d'échange cationique (méq/100g)), oc (proportion de matière organique (pct)). Optionnel: peuvent être récupérés avec le package ExtractMap en utilisant \code{sol=T}
   \item IQS potentiels (m, hauteur dominante à 50 ans): iqs_pot_sab, iqs_pot_epn, iqs_pot_epb, iqs_pot_pig, iqs_pot_tho, iqs_pot_pib, iqs_pot_bop, iqs_pot_pex (peupliers). Optionel, peuvent être récupérés avec le package ExtractMap en utilisant \code{iqs=T}
   \item variable climatiques: prec_gs (précipitations durant la saison de croissance, moyenne sur les 10 dernières années avant le mesurage de la placette (mm), temp_gs (température moyenne durant la saison de croissance, moyenne sur les 10 dernières années avant le mesurage de la placette (Celcius)). Optionnel: peuvent être récupérés avec le package ExtractMap en utilisant \code{climat=T}
   \item latitude, longitude: coordonnées des placettes, en degré décimal. Optionel: nécessaires si \code{iqs=T} ou \code{sol=T} ou \code{climat=T}
   \item an_mes: année de mesure de la placette. Optionnel, nécessaire si \code{climat=T}
}}

\item{horizon}{Nombre de décennies à simuler, un chiffre de 1 à 15.}

\item{mode_simul}{Mode de simulation: \code{"DET"} : déterministe ou \code{"STO"} : stochastique}

\item{nb_iter}{Si mode stochastique, le nombre d'itérations à effectuer (> 1), ignoré si \code{mode_simul="DET"}}

\item{iqs}{Booléen
\itemize{
   \item \code{TRUE} (par défaut): si les IQS potentils doivent être extraits des cartes. Les colonnes latitude et longitude doivent alors être fournies dans \code{file_arbre} ou dans \code{file_compile}.
   \item \code{FALSE}: si les IQS potentiels (m, hauteur dominante à 50 ans) sont fournis dans \code{file_arbre} ou \code{in file_compile}.
 }}

\item{climat}{Booléen
\itemize{
  \item \code{TRUE} (par défaut): si les variables climatiques doivent être extraites des cartes. Les colonnes latitude, longitude et l'année de mesure de la placette doivent alors être fournies dans \code{file_arbre} ou dans \code{file_compile}.
  \item \code{FALSE}: si les variables climatiques sont fournies dans \code{file_arbre} ou \code{file_compile}.
}}

\item{sol}{Booléen
\itemize{
  \item \code{TRUE} (par défaut): si les variables sur les propriétés des sols sont à extraires des cartes. Les colonnes latitude et longitude doivent alors être fournies dans \code{file_arbre} ou dans \code{file_compile}.
  \item \code{FALSE}: si les variables sur les propriétés des sols sont fournies dans \code{file_arbre} ou dans \code{file_compile}.
}}

\item{ht}{Booléen
\itemize{
  \item \code{TRUE} (par défaut): si la hauteur des arbres doit être estimée, seulement pour un fichier \code{file_arbre}
  \item \code{FALSE}: si la hauteur des arbres est fournie dans \code{file_arbre}, ignoré si \code{file_compile}. Le nom de la colonne doit être hauteur_pred (en mètres)
  }}

\item{vol}{Booléen
\itemize{
  \item \code{TRUE} (par défaut): si le volume des arbres doit être estimé, seulement pour un fichier \code{file_arbre}
  \item \code{FALSE}: si le volume des arbres est fourni dans \code{file_arbre}, ignoré si \code{file_compile}. Le nom de la colonne doit être vol_dm3 (en dm3)
  }}

\item{dec_perturb}{Optionnel. Le numéro de la décennie dans laquelle on simule la présence d'une perturbation partielle autre qu'une épidémie de TBE (chablis partiel, verglas pertiel, dépérissement partiel, brulis partiel).}

\item{dec_tbe1}{Optionnel. Le numéro de la décennie dans laquelle on simule une défoliation due à la TBE. \code{tbe1} doit alors est spécifié.}

\item{tbe1}{Optionnel. Indice de réduction de croissance due à la TBE durant la décennie \code{dec_tbe1}, un entier 1 à 5. \code{dec_tbe1} doit alors est spécifié.
\itemize{
  \item 1 = 1 année de défoliation sévères
  \item 2 = 4 années de défoliations sévères
  \item 3 = 6 années de défoliations sévères
  \item 4 = 8 années de défoliations sévères
  \item 5= 10 années de défoliations sévères.
  }}

\item{dec_tbe2}{Optionnel. Le numéro de la 2e décennie dans laquelle on simule une défoliation due à la TBE. \code{tbe2} doit alors est spécifié.}

\item{tbe2}{Optionnel. Indice de réduction de croissance due à la TBE durant la décennie \code{dec_tbe2}, un entier 1 à 5. Voir \code{tbe1} pour définition des niveaux.}
}
\value{
Une chaine de caractères contant "ok" s'il n'y a pas d'erreur, sinon, contient un message d'erreur.
}
\description{
Vérification des arguments de la fonction principale \code{SimulNatura()} du simulateur Natura
}
