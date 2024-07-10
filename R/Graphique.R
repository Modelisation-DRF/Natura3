

#' Graphique d'évolution des variables forestières
#'
#' Cette fonction génère un graphique d'évolution pour différentes variables forestières
#' sur une période de temps donnée, en fonction des espèces et des placettes spécifiées.
#'
#' @param Data Un data.frame contenant les données forestières. Le data.frame doit inclure
#'   au moins les colonnes suivantes : id_pe (identifiant de la placette), temps, et les variables
#'   à tracer (par exemple, sttot, stbop, etc.).
#' @param Espece Une chaîne de caractères spécifiant l'espèce d'arbre pour laquelle générer
#'   le graphique. Les valeurs possibles incluent "tot" (toutes essences), "bop" (bouleau à papier),
#'   "peu" (peupliers), "ft" (feuillus tolérants), "sab" (sapin baumier), "epn" (épinette noire),
#'   "epx" (autres épinettes), "ri" (résineux intolérants), "rt" (autres résineux tolérants).
#'   La valeur par défaut est "tot".
#' @param Variable Une chaîne de caractères spécifiant la variable à tracer. Les valeurs possibles
#'   incluent "st" (surface terrière marchande en m2/ha), "n" (nombre d’arbres marchands par ha),
#'   "v" (volume marchand en m3/ha), "hd" (hauteur dominante en m), "dq" (diamètre quadratique moyen
#'   en cm). La valeur par défaut est "st".
#' @param listePlacette Une liste ou un vecteur contenant les identifiants des placettes à inclure
#'   dans le graphique.
#' @return Un objet ggplot représentant le graphique d'évolution de la variable spécifiée sur la
#'   période de temps
#' @export

Graph <- function(Data, Espece = "tot", Variable = 'st', listePlacette) {
  var <- paste0(paste(Variable), paste(Espece))

  # Ensure the variable column exists and is numeric
  if (Variable == 'st') {
    Etiquette <- "Surface terrière marchande (m2/ha)"
  } else if (Variable == 'n') {
    Etiquette = "Nombre d’arbres marchands par ha"
  } else if (Variable == 'v') {
    Etiquette = "Volume marchand (m3/ha)"
  } else if (Variable == 'hd') {
    var <- "hd"
    Etiquette = "Hauteur dominante (m)"
  } else if (Variable == 'dq') {
    Etiquette = "Diamètre quadratique moyen (cm)"
  }
  if (!(var %in% colnames(Data))) {
    stop(paste("The column", var, "does not exist in the data."))
  }

  if (!is.numeric(Data[[var]])) {
    stop(paste("The column", var, "is not numeric."))
  }

  Data <- Data %>%
    group_by(id_pe, temps) %>%
    summarise(mean_value = mean(get(var), na.rm = TRUE), .groups = 'drop') %>%
    ungroup()



  tempsMin <- min(Data$temps, na.rm = TRUE)
  tempsMax <- max(Data$temps, na.rm = TRUE)

  # Calculate ymax only if there are valid numeric values
  if (any(!is.na(Data$mean_value))) {
    ymax <- max(Data$mean_value, na.rm = TRUE)
  } else {
    ymax <- NA
    warning("No valid numeric values found for mean_value.")
  }

  Essence <- switch(Espece,
                    "tot" = "Toutes essences",
                    "bop" = "Bouleau à papier",
                    "peu" = "Peupliers",
                    "ft" = "Feuillus tolérants",
                    "sab" = "Sapin baumier",
                    "epn" = "Épinette noire",
                    "epx" = "Autres épinettes",
                    "ri" = "Résineux intolérants",
                    "rt" = "Autres résineux tolérants",
                    "Toutes essences"
  )

  dernieres_valeurs <- Data %>%
    group_by(id_pe) %>%
    slice(n()) %>%
    ungroup()

  GraphEvol <- ggplot(data = Data, aes(x = temps, y = mean_value, group = id_pe, label = id_pe)) +
    geom_line(show.legend = FALSE, lwd = 1.25, colour = "#008000") +
    ylim(0, ifelse(is.na(ymax), 5, ymax + 5)) +
    xlab(bquote(bold("Temps depuis la perturbation"))) +
    ylab(Etiquette) +
    scale_x_continuous(breaks = seq(tempsMin, tempsMax, by = 5)) +
    ggtitle(paste(Etiquette, "  ", Essence)) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "white"),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    geom_text(data = dernieres_valeurs, aes(label = id_pe), hjust = 1, vjust = -0.2, size = 3)

  return(GraphEvol)
}
