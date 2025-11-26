#' Calendrier climatique journalier
#'
#' Transforme un ensemble de dates de semis et de récolte en un calendrier climatique pour l'intervalle de chaque combinaison de dates.
#' Chaque combinaison de dates est étendue par incrément d'une journée où les données climatiques journalières y sont jointes, et ce, pour trois scénarios distincts.
#'
#' @param data Données de rendement contenant au moins une colonne d’identification des parcelles, une date de semi, un numéro de champ et une année. L'année peut être extraite de la date de semi.
#' @param weather_data_j Données climatiques journalières contenant au moins la température minimale et maximale et les précipitations totales.
#' @param id_col Variable d'identification des parcelles/unité de culture
#' @param date_semi Variable de type `date` sous format `"%Y-%m-%d`.
#' @param date_recolte Variable de type `date` sous format `"%Y-%m-%d`.
#'
#' @returns Une liste de `dataframe` contenant:
#'    \describe{
#'      \item{Presemis_7j}{Un calendrier contenant les données climatiques 7 jours avant le semis}
#'      \item{Postsemis_14j}{Un calendrier contenant les données climatiques 14 jours après le semis}
#'      \item{Recolte}{Un calendrier contenant les données climatiques du semis à la récolte moins un jour}
#'    }
#' @importFrom dplyr select left_join mutate distinct
#' @importFrom tidyr unnest drop_na
#' @importFrom purrr map2
#' @export
#'
#' @examples
calendrier_climatique_journalier <- function(data, weather_data_j, id_col, date_semi = date_semi, date_recolte = date_recolte){
  # 7 jours pré-semis
  calendrier_7j_presemis <- data |>
    dplyr::select(any_of({{id_col}}, {{date_semi}}, nochamp, annee)) |>
    dplyr::mutate(date_semi = as.Date({{date_semi}}),
                  date = purrr::map2(date_semi - 7, date_semi, ~ seq(.x, .y, by = "day"))) |>
    dplyr::distinct() |>
    tidyr::unnest(date) |>
    dplyr::left_join(weather_data_j, by = "date") |>
    tidyr::drop_na(c(min_temp, max_temp, total_precip)) |>
    dplyr::select(any_of({{id_col}}, {{date_semi}}, nochamp, annee, date, min_temp, max_temp, total_precip, globalrad))

  # 14 jours post-semis
  calendrier_14j <- data |>
    dplyr::select(any_of({{id_col}}, {{date_semi}}, nochamp, annee)) |>
    dplyr::mutate(date_semi = as.Date({{date_semi}}),
                  date = purrr::map2(date_semi, date_semi + 14, ~ seq(.x, .y, by = "day"))) |>
    dplyr::distinct() |>
    tidyr::unnest(date) |>
    dplyr::left_join(weather_data_j, by = "date") |>
    tidyr::drop_na(c(min_temp, max_temp, total_precip)) |>
    dplyr::select(any_of({{id_col}}, {{date_semi}}, nochamp, annee, date, min_temp, max_temp, total_precip, globalrad))

  # Du semis à la récolte
  calendrier_recolte <- data |>
    dplyr::select(any_of({{id_col}}, {{date_semi}}, {{date_recolte}}, nochamp, annee)) |>
    dplyr::mutate(date_semi = as.Date({{date_semi}}),
                  date_recolte = as.Date({{date_recolte}}),
                  date = purrr::map2(date_semi, date_recolte - 1, ~ seq(.x, .y, by = "day"))) |>
    dplyr::distinct() |>
    tidyr::unnest(date) |>
    dplyr::left_join(weather_data_j, by = "date") |>
    tidyr::drop_na(c(min_temp, max_temp, total_precip)) |>
    dplyr::select(any_of({{id_col}}, date_semi, date_recolte, nochamp, annee, date, min_temp, max_temp, total_precip, globalrad))

  return(list("Presemis_7j" = calendrier_7j_presemis, "Postsemis_14j" = calendrier_14j, "Recolte" = calendrier_recolte))
}

#' Calendrier climatique horaire
#'
#' Transforme un ensemble de dates de semis en un calendrier climatique.
#' Chaque date de semis est étendue par incrément d'une heure jusqu'à `date_semi + 14` où les données climatiques horaires y sont jointes.
#' Ce calendrier est utilisé pour calculer les périodes de gel.
#'
#' @param data Données de rendement contenant au moins une colonne d'identification des parcelles, une date de semi, un numéro de champ et une année. L'année peut être extraite de la date de semi.
#' @param weather_data_h Données climatiques horaires contenant au moins la température
#' @param id_col Variable d'identification des parcelles/unité de culture
#' @param date_semi Variable de type `date` sous format `"%Y-%m-%d`.
#'
#' @returns Un `dataframe` contenant le calendrier climatique de date -> date + 14 pour chaque date de semis.
#' @importFrom dplyr select mutate distinct left_join
#' @importFrom tidyr unnest
#' @importFrom purrr map2
#' @export
#'
#' @examples
calendrier_climatique_horaire <- function(data, weather_data_h, id_col, date_semi = date_semi){
  calendrier_h_semis <- data |>
    dplyr::select(any_of({{id_col}}, {{date_semi}}, nochamp, annee)) |>
    # On cale les timezones sur les mêmes que les données météo! Sinon décalage de 4h
    dplyr::mutate(date_semi = as.POSIXct({{date_semi}}, tz = attr(weather_data_h$time, "tzone")),
                  date_semi_14 = as.POSIXct({{date_semi}} + days(14)),
                  time = purrr::map2(date_semi, date_semi_14, ~ seq(.x, .y, by = "hour"))) |>
    dplyr::distinct() |>
    tidyr::unnest(time) |>
    dplyr::left_join(weather_data_h) |>
    dplyr::select(any_of({{id_col}}, date_semi, nochamp, annee, time, temp))
  return(calendrier_h_semis)
}

#' Calcul des UTMs
#'
#' Les unités thermiques maïs sont utilisées comme barème d'atteinte de maturité physiologique chez le soya et le maïs grain.
#'
#' @param min_temp Température minimale journalière
#' @param max_temp Température maximale journalière
#'
#' @returns
#' @export
#'
#' @examples
utm <- function(min_temp, max_temp){
  tmax_m10 <- pmax(max_temp - 10, 0)
  tmin_m444 <- pmax(min_temp - 4.44, 0)
  ymax <- 3.33 * tmax_m10 - 0.084 * (tmax_m10^2)
  ymin <- 1.8 * tmin_m444
  return((ymax + ymin)*0.5)
}

# Calcul des degrés jour
#' Calcul des degrés jour
#'
#' @param min_temp Température minimale journalière
#' @param max_temp Température maximale journalière
#' @param tbase Température basale où l'accumulation des degrés jour commence, variable par culture
#' @param tlim Température maximale où l'accumulation des degrés jour arrête, variable par culture
#'
#' @returns
#' @export
#'
#' @examples
gdd <- function(min_temp, max_temp, tbase = 5, tlim = 32){
  tmin <- pmax(min_temp, 0)
  tmax <- pmax(max_temp, 0)
  tmean <- (tmax + tmin)/2
  return(ifelse (tmean < tbase | tmax > tlim, 0, tmean - tbase))
}

#' Cummul des UTM
#'
#' Les unités thermiques maïs sont utilisées comme barème d'atteinte de maturité physiologique chez le soya et le maïs grain.
#'
#' @param calendrier Calendrier climatique journalier du semis à la récolte
#' @param id_col Variable d'identification des parcelles/unité de culture
#' @param date_semi Variable de type `date` sous format `"%Y-%m-%d`
#' @param min_temp Variable contenant la température minimale journalière
#' @param max_temp Variable contenant la température maximale journalière
#' @param thresh Seuil d'UTM où la maturité physiologique est atteinte, variable par culture
#'
#' @returns Un `dataframe` contenant:
#'    \describe{
#'      \item{id_cols}{Variable d'identification des parcelles/unité de culture}
#'      \item{date_semi}{Variable de type `date` sous format `"%Y-%m-%d`}
#'      \item{nochamp}{Variable d'identification des champs}
#'      \item{annee}{Variable d'identification de l'année}
#'      \item{utm_atteint}{Nombre d'UTM atteints au jour où le seuil est dépassé. Retourne `NA_real_` lorsque le seuil n'est pas atteint à la récolte.}
#'      \item{nb_jour_utm}{Nombre de jours de croissance pour que le seuil soit obtenu. Retourne `NA_real_` lorsque le seuil n'est pas atteint à la récolte.}
#'    }
#' @importFrom dplyr group_by mutate summarise select
#' @export
#'
#' @examples
utm_cummul <- function(calendrier, id_col, date_semi, min_temp, max_temp, thresh){
  utm_recolte <- calendrier |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee) |>
    dplyr::mutate(utm = utm(min_temp = min_temp, max_temp = max_temp) |> purrr::accumulate(`+`)) |>
    dplyr::summarise(idx = which(utm >= thresh)[1],
                     date_atteinte = if (!is.na(idx)) date[idx] else as.Date(NA),
                     utm_atteint = if (!is.na(idx)) utm[idx] else NA_real_,
                     .groups = "drop") |>
    dplyr::mutate(nb_jour_utm = case_when(!is.na(date_atteinte) ~ as.factor(date_atteinte - date_semi),
                                          TRUE ~ as.factor("Non atteint"))) |>
    dplyr::select(any_of({{id_col}}, {{date_semi}}, nochamp, annee, utm_atteint, nb_jour_utm))
  return(utm_recolte)
}

#' Cummul des degrés jour
#'
#' Les degrés jour sont utilisés comme barème d'atteinte de maturité physiologique chez le blé.
#' Cette fonction retourne le nombre de degrés jour atteints au nombre de jours requis pour l'atteinte de la maturité physiologique.
#'
#' @param calendrier Calendrier climatique journalier du semis à la récolte
#' @param id_col Variable d'identification des parcelles/unité de culture
#' @param date_semi Variable de type `date` sous format `"%Y-%m-%d`
#' @param min_temp Variable contenant la température minimale journalière
#' @param max_temp Variable contenant la température maximale journalière
#' @param tbase Température basale où l'accumulation des degrés jour commence, variable par culture
#' @param tlim Température maximale où l'accumulation des degrés jour arrête, variable par culture
#' @param jour_maturite Nombre de jours pour l'atteinte de la maturité physiologique (fiche technique de la variété)

#'
#' @returns Un `dataframe` contenant:
#'    \describe{
#'      \item{id_cols}{Variable d'identification des parcelles/unité de culture}
#'      \item{date_semi}{Variable de type `date` sous format `"%Y-%m-%d`}
#'      \item{nochamp}{Variable d'identification des champs}
#'      \item{annee}{Variable d'identification de l'année}
#'      \item{gdd_atteint}{Nombre de degrés jours atteints au seuil de maturité physiologique. Retourne `NA_real_` lorsque le seuil n'est pas atteint à la récolte.}
#'    }
#' @importFrom dplyr group_by mutate summarise select
#' @importFrom purrr accumulate
#' @export
#'
#' @examples
gdd_cummul <- function(calendrier, id_col, date_semi, min_temp, max_temp, tbase, tlim, jour_maturite){
  gdd_90 <- calendrier |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee) |>
    dplyr::mutate(gdd = gdd(min_temp = {{min_temp}}, max_temp = {{max_temp}}, tbase = {{tbase}}, tlim = {{tlim}}) |> purrr::accumulate(`+`)) |>
    dplyr::summarise(idx = which(date >= {{date_semi}} + jour_maturite)[1],
                     date_atteinte = if (!is.na(idx)) date[idx] else as.Date(NA),
                     gdd_atteint = if (!is.na(idx)) gdd[idx] else NA_real_,
                     .groups = "drop") |>
    dplyr::select(any_of({{id_col}}, {{date_semi}}, nochamp, annee, gdd_atteint))
  return(gdd_90)
}

#' Cummul de la radiation solaire
#'
#' @param calendrier Calendrier climatique journalier du semis à la récolte
#' @param id_col Variable d'identification des parcelles/unité de culture
#' @param date_semi Variable de type `date` sous format `"%Y-%m-%d`
#'
#' @returns Un `dataframe` contenant:
#'    \describe{
#'      \item{id_cols}{Variable d'identification des parcelles/unité de culture}
#'      \item{date_semi}{Variable de type `date` sous format `"%Y-%m-%d`}
#'      \item{nochamp}{Variable d'identification des champs}
#'      \item{annee}{Variable d'identification de l'année}
#'      \item{globalrad}{Somme de la radiation globale, unités variables selon la source des données.}
#'    }
#' @importFrom dplyr group_by summarise
#' @export
#'
#' @examples
rad_cummul <- function(calendrier, id_col, date_semi){
  rad_recolte <- calendrier |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee) |>
    dplyr::summarise(total_rad = sum(globalrad))
}

#' Cummul des précipitation
#'
#' Cette fonction cumule les précipitations selon trois scénarios distincts. Une variable contenant les précipitations totales journalière `total_precip` doit absolument être incluse.
#'
#' @param calendrier Liste de calendriers climatiques journaliers. Cette fonction accepte le output de la fonction `calendrier_climatique_journalier}
#' @param id_col Variable d'identification des parcelles/unité de culture
#' @param date_semi Variable de type `date` sous format `"%Y-%m-%d`
#'
#' @returns Une liste de `dataframe` contenant:
#'    \describe{
#'      \item{Presemis_7j}{Un calendrier contenant les précipitations cummulées 7 jours avant le semis}
#'      \item{Postsemis_14j}{Un calendrier contenant les précipitations cummulées 14 jours après le semis}
#'      \item{Recolte}{Un calendrier contenant les précipitations cummulées du semis à la récolte moins un jour}
#'    }
#' @importFrom dplyr group_by summarise
#' @export
#'
#' @examples
precipitation <- function(calendrier, id_col, date_semi){
  pr_7j <- calendrier$Presemis_7j |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee) |>
    dplyr::summarise(total_precip_7j = sum(total_precip))
  ## 14 après le semis
  pr_14j <- calendrier$Postsemis_14j |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee) |>
    dplyr::summarise(total_precip_14j = sum(total_precip))
  ## À la récolte
  pr_recolte <- calendrier$Recolte |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee) |>
    dplyr::summarise(total_precip = sum(total_precip))
  return(list("Presemis_7j" = pr_7j, "Postsemis_14j" = pr_14j, "Recolte" = pr_recolte))
}

#' Fréquence des épisodes de gel
#'
#' Cette fonction dénombre le nombre d'épisodes de gel pour chaque date de semis entre `date_semi` et `date_semi + 14`.
#' Un épisode de gel est caractérisé par un seuil (`thresh`) et une durée d'exposition au seuil (`span`).
#'
#' @param calendrier_horaire Calendrier climatique horaire du semis à la récolte obtenu avec la fonction `calendrier_climatique_horaire}
#' @param id_col Variable d'identification des parcelles/unité de culture
#' @param date_semi Variable de type `date` sous format `"%Y-%m-%d`
#' @param thresh Température minimale de résistance au gel de la culture, variable par culture
#' @param span Nombre d'heure d'exposition à la température minimale
#'
#' @returns Un `dataframe` contenant:
#'    \describe{
#'      \item{id_cols}{Variable d'identification des parcelles/unité de culture}
#'      \item{date_semi}{Variable de type `date` sous format `"%Y-%m-%d`}
#'      \item{nochamp}{Variable d'identification des champs}
#'      \item{annee}{Variable d'identification de l'année}
#'      \item{cold}{Nombre d'épisodes de gel entre `date_semi` et `date_semi + 14`. Retourne 0 lorsque qu'aucun épisode de gel n'a été enregistré.}
#'    }
#' @importFrom dplyr group_by mutate case_when lag row_number summarise reframe
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples
frequence_gel <- function(calendrier_horaire, id_col, date_semi, thresh, span){
  nb_gel <- calendrier_horaire |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee) |>
    dplyr::mutate(cold = temp < thresh,
                  #
                  gap_h = as.numeric(difftime(time, dplyr::lag(time), units = "hours")),
                  new_block = dplyr::case_when(
                    # Première ligne du groupe et si temp < seuil
                    dplyr::row_number() == 1 ~ T,
                    # Si précédent Non NA et temp >= seuil, et actuel cold
                    !dplyr::lag(cold) & cold ~ TRUE,
                    # Si précédent Non NA et temp < seuil et actuel cold
                    dplyr::lag(cold) & cold ~ FALSE,
                    # Si précédent Non NA cold et actuel non cold
                    dplyr::lag(cold) & !cold ~ TRUE,
                    # Deux heures où temp < seuil mais séparées par NA
                    dplyr::lag(cold) & cold & !is.na(gap_h) & gap_h > 2 ~ TRUE,
                    # Si gap > 2h, trop d'incertitude
                    is.na(dplyr::lag(temp)) & cold & !is.na(gap_h) & gap_h > 2 ~ TRUE,
                    TRUE ~ FALSE),
                  new_block = tidyr::replace_na(new_block, FALSE),
                  block_id = cumsum(new_block)) |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee, block_id) |>
    dplyr::summarise(cold = sum(cold, na.rm = T), .groups = "drop_last") |>
    dplyr::reframe(cold = cold >= span) |>
    dplyr::group_by({{id_col}}, {{date_semi}}, nochamp, annee) |>
    dplyr::summarise(cold = sum(cold))
  return(nb_gel)
}
