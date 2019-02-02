#' Create a Data Dictionary
#'
#' `get_dictionary` creates a data dictionary from data frame variable attributes
#'
#' This function takes a data frame created using `haven::read_sav` and
#' returns a data frame with the SPSS metadata for each variable
#'
#' @param data A data frame or tibble created using `haven::read_sav`
#' @return A data frame with the `variable` names, SPSS `class`, and
#'     their respective `value`
#' @keywords SPSS haven sav
#' @export
#'


get_dictionary <- function(data) {
  if (purrr::some(data, haven::is.labelled)==FALSE) {stop("Not a labelled data set")}
  purrr::map(data, attributes) %>%
    tibble::enframe(name = "variable") %>%
    dplyr::mutate(value = purrr::map(value, ~tibble::enframe(., name = "class"))) %>%
    tidyr::unnest() %>%
    tidyr::complete(variable, class)
}
