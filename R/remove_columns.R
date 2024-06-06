
#' Cette fonction supprime les colonnes spécifiées d'un data frame si elles existent.
#'
#' @param data Un data frame à partir duquel les colonnes seront supprimées.
#' @param columns_to_remove Un vecteur de caractères contenant les noms des colonnes à vérifier et à supprimer si elles existent dans le data frame.
#'
#' @return Un data frame avec les colonnes spécifiées supprimées si elles existent.
#' @export
#'
#' @examples
#'
#' # Liste des colonnes à supprimer
#' variable_climat <- c("p_tot", "t_ma", "prec_gs", "temp_gs")
#'
#' # Appliquer la fonction pour supprimer les colonnes spécifiées
#' data <- remove_columns(data, columns_to_remove)
#'
#'


remove_columns <- function(data, columns_to_remove) {

  names(data) <- tolower(names(data))

  columns_present <- columns_to_remove[columns_to_remove %in% colnames(data)]

  data <- data[, !(colnames(data) %in% columns_present)]

  return(data)
}
