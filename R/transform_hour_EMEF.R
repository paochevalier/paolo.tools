#' Transform EMEF hour variables (24–32 -> 00–08)
#'
#' This function recodes EMEF hour variables (values 24–32) into their
#' equivalent 0–08 format, modifying the variable indicated by `variable_hora`.
#' Non-numeric values used to encode non-response, such as "NS/NC", are
#' converted to NA.
#'
#' @param EMEF A data frame containing the EMEF data.
#' @param variable_hora A string naming the column to transform (e.g. "V03D").
#'
#' @return The modified data frame.
#' @export
#'
#' @importFrom dplyr mutate case_when select
#' @importFrom rlang sym `:=`
#' @importFrom magrittr `%>%`
#'
#' @examples
#' \dontrun{
#' EMEF <- transform_hour_EMEF(EMEF, "V03D")
#' }
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
