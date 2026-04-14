
#' Filtrer un trajet selon un vecteur de numéros de boucle
#'
#' Cette fonction sélectionne les lignes du data.frame dont la colonne
#' `Numéro de boucle` correspond aux valeurs fournies dans le vecteur `boucle`.
#'
#' @param trajet Un data.frame ou tibble contenant au moins la colonne
#' `Numéro de boucle`.
#' @param boucle Un vecteur de numéros de boucle à sélectionner (numérique ou texte).
#'
#' @return Un tibble filtré contenant uniquement les boucles sélectionnées.
#'
#' @export
#'
#' @examples
#' filtrer_trajet(trajet = df_velo, boucle = c("880", "881"))
filtrer_trajet <- function(trajet, boucle) {
  if (!"Numéro de boucle" %in% names(trajet)) {
    stop("Le data.frame doit contenir une colonne 'Numéro de boucle'.")
  }

  trajet[trajet$`Numéro de boucle` %in% boucle, , drop = FALSE]
}
