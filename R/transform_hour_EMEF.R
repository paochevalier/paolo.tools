#' Transform EMEF hour variables (24–32 -> 0–8)
#'
#' Recodes EMEF hour variables where values 24 to 32 correspond to hours
#' between 00:00 and 08:00. These values are converted to 0–8.
#'
#' Non-response values such as "NS/NC" are explicitly converted to NA.
#'
#' @param EMEF A data frame containing EMEF data.
#' @param variable_hora A character string naming the variable to transform.
#'
#' @return The input data frame with the transformed hour variable.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Converts the variable to character, then numeric
#'   \item Maps values 24–32 to 0–8
#'   \item Converts known non-response values (e.g. "NS/NC") to NA
#' }
#'
#' @examples
#' \dontrun{
#' EMEF <- transform_hour_EMEF(EMEF, "V03D")
#' }
#'
#' @export

transform_hour_EMEF <- function(EMEF, variable_hora) {
  variable_sym <- rlang::sym(variable_hora)

  missing_values <- c("NS/NC")

  EMEF <- EMEF %>%
    dplyr::mutate(
      .hora_chr = as.character(!!variable_sym),
      .hora_num = suppressWarnings(as.numeric(.hora_chr))
    ) %>%
    dplyr::mutate(
      !!variable_sym := dplyr::case_when(
        .hora_chr %in% missing_values ~ NA_real_,
        .hora_num >= 24 & .hora_num <= 32 ~ .hora_num - 24,
        TRUE ~ .hora_num
      )
    ) %>%
    dplyr::select(-.hora_chr, -.hora_num)

  return(EMEF)
}
