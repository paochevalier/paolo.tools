#' Load EMEF survey data (Desplaçaments or Opinió)
#'
#' This function loads the raw EMEF survey data (Mobilitat en dia feiner)
#' from the local directory structure of Institut Metròpoli. It supports
#' both “desplaçaments” and “opinió” datasets, automatically detecting
#' the correct `.sav` file according to the selected type and year.
#'
#' @param any_EMEF Integer or string indicating the survey year
#'   (e.g., `2022` or `"2022"`).
#' @param tipus Character string specifying the dataset type:
#'   either `"desplaçaments"` or `"opinió"`. Defaults to `"desplaçaments"`.
#'
#' @details
#' The function expects that the data are stored under the following path:
#' `HOMEPATH/Institut Metròpoli/Mobilitat-BD - General/01_ENQUESTES/EMEF/3-BBDD/`
#'
#' Depending on the `tipus` argument, it will look for one of the following files:
#'
#' - `"YYYY BBDD DESPLAÇAMENTS EMEF.sav"`
#' - `"YYYY BBDD OPINIÓ EMEF.sav"`
#'
#' The function reads the SPSS file using `foreign::read.spss()`, converts it
#' into a data frame, and attaches an attribute `fitxer_origen` containing the
#' name of the original file.
#'
#' @return A data frame containing the loaded EMEF dataset, with an additional
#'   attribute `"fitxer_origen"` that records the original filename.
#'
#' @examples
#' \dontrun{
#' # Load mobility data
#' EMEF_despl <- get_EMEF(2022, tipus = "desplaçaments")
#'
#' # Load opinion data
#' EMEF_opinio <- get_EMEF(2021, tipus = "opinió")
#' }
#'
#' @importFrom foreign read.spss
#' @export
get_EMEF <- function(any_EMEF, tipus = c("desplaçaments", "opinió")) {
  tipus <- match.arg(tipus, choices = c("desplaçaments", "opinió"))
  tipus_l <- tolower(tipus)

  base_path <- file.path(Sys.getenv("HOMEPATH"),
                         "Institut Metròpoli/Mobilitat-BD - General/01_ENQUESTES/EMEF/3-BBDD")

  if (tipus_l == "desplaçaments") {
    path_taula <- file.path(base_path, paste0(as.character(any_EMEF),
                                              " BBDD DESPLAÇAMENTS EMEF.sav"))
  } else {
    path_taula <- file.path(base_path, paste0(as.character(any_EMEF),
                                              " BBDD OPINIÓ EMEF.sav"))
  }

  if (!file.exists(path_taula)) {
    stop("No s’ha trobat el fitxer especificat: ", path_taula)
  }

  EMEF <- foreign::read.spss(path_taula, to.data.frame = TRUE)
  attr(EMEF, "fitxer_origen") <- basename(path_taula)
  message("Fitxer carregat correctament: ", basename(path_taula))
  return(EMEF)
}
