## Natura3

Un package pour la simulation de la croissance de peuplements forestiers avec le simulateur Natura 3.0

Auteurs: Isabelle Auger - Ministère des Ressources Naturelles et des Forêts du Québec

Courriel: isabelle.auger@mrnf.gouv.qc.ca

This R package is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This library is distributed with the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

See the license LGPL-3.0 at http://www.gnu.org/copyleft/lesser.html.

## Introduction
Le package Natura3 permet la simulation de la croissance d'un peuplement forestier avec le simulateur Natura 3.0. Cette version de Natura utilise l'origine du peuplement et le temps depuis l'origine. La simulation peut être déterministe ou stachastique.

## Documentation et références
Non disponibles pour l'instant.

## Dépendences
Ce package dépends des packages ExtractMap et TarifQC.

- TarifQC est disponible ici https://github.com/Modelisation-DRF/TarifQC

- ExtractMap est disponible sur demande.

## Comment obtenir le code source
Taper cette ligne dans une invite de commande pour cloner le dépôt dans un sous-dossier "natura3":

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
git clone https://github.com/Modelisation-DRF/Natura3 natura3
```

## Comment installer le package Natura3 dans R

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
require(remotes)
install_github("https://github.com/Modelisation-DRF/Natura3", ref="main", auth_token = "demander_un_token")
```
## Exemple

Ce package inclut des objets de type data.frame contenent des listes d'arbres regroupées en placettes. Ces objets peuvent être utilisés pour essayer le package.

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
library(Natura3)
data_simul <- SimulNatura(file_arbre=fichier_arbres_sanscov, file_etude=fichier_arbres_etudes, horizon=5)
```
De l'aide supplémentaire peut être obtenu sur la fonction avec la commande
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
?SimulNatura
```
Pour obtenir la liste des data.frame disponibles
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
data(package='Natura3')
```
Pour une description du data.frame
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
?fichier_arbres_sanscov
```

## Historique des versions

| Date |  Version  | Features et bugs | Détails |
|:-----|:---------:|:-----------------|:--------|
| 2024-04-05 | 1.0.3 |  | ajouter la liste des placettes rejetées au fichier des résultats |
| 2024-04-03 | 1.0.2 |  | élargir les pages de valeurs possibles |
| 2024-03-27 | 1.0.1 |  | déplacer les packages de Depends à Imports dans DESCRIPTION, ajouter les références aux packages avec :: pour TarifQC et ExtractMap |
| 2024-03-25 | 1.0.0 |  | Première version stable |
