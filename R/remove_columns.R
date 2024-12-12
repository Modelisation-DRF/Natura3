
#' Cette fonction supprime les colonnes spécifiées d'un data frame si elles existent.
#'
#' @param data Un data frame à partir duquel les colonnes seront supprimées.
#' @param columns_to_remove Un vecteur de caractères contenant les noms des colonnes à vérifier et à supprimer si elles existent dans le data frame.
#'
#' @return Un data frame avec les colonnes spécifiées supprimées si elles existent.
#' @export
#'
remove_columns <- function(data, columns_to_remove) {

  # data=fic; columns_to_remove=colonne
  names(data) <- tolower(names(data))

  columns_present <- columns_to_remove[columns_to_remove %in% colnames(data)]

 # data <- data[, !(colnames(data) %in% columns_present)]
  # si data est un data.frame et qu'il ne reste qu'une colonne après avoir enlevé celles demandées,
  # le résultat n'est plus un data mais un vecteur sans nom...
  # ça fonctionne si data est un tibble, je vais donc changer le code pour que ça reste un tibble
  data <- data %>% dplyr::select(-all_of(columns_present))

  return(data)
}






#' Valide la correspondance des placettes entre le fichier des arbres et celui des arbres-études
#'
#' Cette fonction vérifie qu'au moins une des placettes présentes dans le fichier des arbres est également présente dans le fichier des arbres-études
#'
#' @param data_arbre Nom du fichier contenant les informations sur les arbres et les placettes
#' @param data_etude Nom du fichier contenant les arbres-études
#' @return Une liste de messages d'erreurs. Si aucune erreur n'est trouvée, une liste vide est retournée.
#' @export
valide_placette_etudes <-function (data_arbre , data_etude){

  names(data_arbre) <- tolower(names(data_arbre))
  names(data_etude) <- tolower(names(data_etude))

  erreurs <-list()

  if(!"id_pe" %in% names(data_arbre) || !"id_pe" %in% names(data_etude) ){
    erreurs<-paste("La colonne 'id_pe' est manquante dans le fichier etude ou dans le fichier des arbres.")

    return(erreurs)
  }else  if(length(data_arbre$id_pe) == 0|| length(data_etude$id_pe) == 0){
    erreurs<-paste("La colonne 'id_pe' est vide dans le fichier etude ou dans le fichier des arbres.")
    return(erreurs)
  }


  placette_arbre <- unique(data_arbre$id_pe)
  placette_etude <- unique(data_etude$id_pe)

  diff_placette <- setdiff(placette_arbre, placette_etude)

  erreurs <-list()

  # si aucune des placettes de data_arbre n'est dans data_etude
  if (length(diff_placette) != 0 & all(placette_arbre == diff_placette)) { # si la liste des placettes de data_arbre et data_etude ne sont pas pareils, et si la différence entre les 2 liste est la liste complete de data_arbre
    erreurs <- paste("Les placettes suivantes ne sont pas dans le fichier des arbres-etudes : ", paste(diff_placette, collapse = ", "))
  }



  return(erreurs)
}

