#' Compter le nombre total de trajets
#'
#' Cette fonction additionne la colonne `Total` d'un jeu de données
#' contenant le nombre de trajets.
#'
#' @param trajet Un data frame ou tibble contenant une colonne `Total`.
#'
#' @return Un nombre indiquant la somme totale des trajets.
#'
#' @import dplyr
#' @export
#'
compter_nombre_trajets <- function(trajet) {
  trajet |>
    dplyr::pull(Total) |>
    sum()
}


#' Compter le nombre de boucles distinctes
#'
#' Cette fonction calcule le nombre de valeurs distinctes présentes dans la colonne
#' `Numéro de boucle` d'un jeu de données de trajets.
#'
#' @param trajet Un data frame ou tibble contenant une colonne `Numéro de boucle`.
#'
#' @return Un entier correspondant au nombre de boucles distinctes.
#'
#' @export
#'
#' @import dplyr
#'
compter_nombre_boucle <- function(trajet) {
  trajet |>
    dplyr::pull(`Numéro de boucle`) |>
    dplyr::n_distinct()
}


#' Trouver le trajet avec le total maximal
#'
#' Cette fonction renvoie la ou les lignes correspondant au trajet ayant
#' la valeur la plus élevée dans la colonne `Total`.
#'
#' @param trajet Un data frame ou tibble contenant au moins les colonnes
#' `Boucle de comptage`, `Jour` et `Total`.
#'
#' @return Un tibble avec les colonnes `Boucle de comptage`, `Jour` et `Total`
#' correspondant au trajet maximal.
#'
#' @export
#'
#' @import dplyr
#'
trouver_trajet_max <- function(trajet) {
  trajet |>
    dplyr::slice_max(Total, n = 1, with_ties = TRUE) |>
    dplyr::select(`Boucle de comptage`, Jour, Total)
}


#' Filtrer les trajets avec anomalie potentielle
#'
#' Cette fonction conserve les lignes où la probabilité de présence d'anomalies
#' est manquante, tout en gardant des valeurs de `Total` strictement positives
#' et inférieures à 10000.
#'
#' @param trajet Un data frame ou tibble contenant au moins les colonnes
#' `Probabilité de présence d'anomalies` et `Total`.
#'
#' @return Un tibble filtré contenant uniquement les trajets répondant aux critères.
#'
#' @export
#'
#' @import dplyr
#'
filtre_anomalie <- function(trajet){
  trajet |>
    dplyr::filter(
      is.na(`Probabilité de présence d'anomalies`),
      Total < 10000,
      Total > 0
    )
}


#' Tracer la distribution hebdomadaire des trajets
#'
#' Cette fonction filtre d'abord les anomalies, calcule la distribution par jour
#' de la semaine, puis affiche un diagramme en barres des trajets.
#'
#' @param trajet Un data frame ou tibble contenant au moins les colonnes
#' `Jour de la semaine`, `Total` et celles nécessaires aux fonctions
#' `filtre_anomalie()` et `calcul_distribution_semaine()`.
#'
#' @return Un objet `ggplot`.
#'
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @export
#'
plot_distribution_semaine <- function(trajet) {

  trajet_filtre <- trajet |>
    filtre_anomalie()

  trajet_weekday <- trajet_filtre |>
    calcul_distribution_semaine() |>
    dplyr::mutate(
      jour = forcats::fct_recode(
        factor(`Jour de la semaine`),
        "lundi" = "1",
        "mardi" = "2",
        "mercredi" = "3",
        "jeudi" = "4",
        "vendredi" = "5",
        "samedi" = "6",
        "dimanche" = "7"
      )
    )

  ggplot2::ggplot(trajet_weekday) +
    ggplot2::aes(x = jour, y = trajets) +
    ggplot2::geom_col(fill = "#2C7BB6") +
    ggplot2::labs(
      x = "Jour de la semaine",
      y = "Nombre de trajets",
      title = "Distribution hebdomadaire des trajets"
    ) +
    ggplot2::theme_minimal()
}


#' Trouver le trajet maximal et calculer des moyennes associées
#'
#' Cette fonction filtre d'abord les anomalies, puis identifie la ligne ayant
#' la valeur maximale de `Total`. Elle ajoute ensuite deux moyennes :
#' la moyenne des trajets pour le même jour et la moyenne des trajets pour
#' la même boucle de comptage.
#'
#' @param trajet Un data frame ou tibble contenant au moins les colonnes
#' `Boucle de comptage`, `Jour` et `Total`, ainsi que celles nécessaires à
#' `filtre_anomalie()`.
#'
#' @return Un tibble avec les colonnes `Boucle de comptage`, `Jour`, `Total`,
#' `moyenne_jour_identique` et `moyenne_boucle_identique`.
#'
#' @import dplyr
#' @export
#'
trouver_trajet_max <- function(trajet){
  trajet_max <- trajet |>
    filtre_anomalie() |>
    dplyr::slice_max(Total, n = 1, with_ties = TRUE) |>
    dplyr::select(`Boucle de comptage`, Jour, Total)

  trajet_max$moyenne_jour_identique <- trajet |>
    dplyr::filter(Jour == trajet_max$Jour) |>
    dplyr::pull(Total) |>
    mean()

  trajet_max$moyenne_boucle_identique <- trajet |>
    dplyr::filter(`Boucle de comptage` == trajet_max$`Boucle de comptage`) |>
    dplyr::pull(Total) |>
    mean()

  return(trajet_max)
}
