#' Transform EMEF hour variables (24–32 → 00–08)
#'
#' This function recodes EMEF hour variables (values 24–32) into their
#' equivalent 0–08 format, modifying the variable indicated by `variable_hora`.
#'
#' @param EMEF A data frame containing the EMEF data.
#' @param variable_hora A string naming the column to transform (e.g. "V03D").
#'
#' @return The modified data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' EMEF <- transform_hour_EMEF(EMEF, "V03D")
#' }
transform_hour_EMEF <- function(EMEF, variable_hora) {
  variable_sym <- rlang::sym(variable_hora)

  EMEF <- EMEF %>%
    mutate(variable_sym = as.numeric(as.character(!!variable_sym))) %>%
    mutate(
      !!sym(variable_hora) := case_when(
        variable_sym == 24 ~ 0,
        variable_sym == 25 ~ 1,
        variable_sym == 26 ~ 2,
        variable_sym == 27 ~ 3,
        variable_sym == 28 ~ 4,
        variable_sym == 29 ~ 5,
        variable_sym == 30 ~ 6,
        variable_sym == 31 ~ 7,
        variable_sym == 32 ~ 8,
        TRUE ~ variable_sym
      )
    )

  return(EMEF)
}
