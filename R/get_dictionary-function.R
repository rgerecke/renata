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


#' Create table of variable labels
#'
#' `get_var_labels` creates a data frame of variable names and labels
#'
#' This function takes a data frame created using `haven::read_sav` and
#' returns a data frame with the SPSS variable labels for each variable
#'
#' @param data A data frame or tibble created using `haven::read_sav`
#' @return A data frame with the `variable` names and respective SPSS `label`
#' @keywords SPSS haven sav
#' @export
#'


get_var_labels <- function(data) {
  if (purrr::some(data, haven::is.labelled)==FALSE) {stop("Not a labelled data set")}
  purrr::map_chr(data, ~attr(., "label")) %>%
    tibble::enframe(name = "variable", value ="label")
}

#' Create table of value labels
#'
#' `get_val_labels` creates a data frame of variable names and labels
#'
#' This function takes a data frame created using `haven::read_sav` and
#' returns a data frame with the SPSS labelled values for each variable
#'
#' @param data A data frame or tibble created using `haven::read_sav`
#' @return A data frame with the `variable`, `value` (which has labels), and respective SPSS `value_name`
#' @keywords SPSS haven sav
#' @export
#'

get_val_labels <- function(data) {
  if (purrr::some(data, haven::is.labelled)==FALSE) {stop("Not a labelled data set")}
  purrr::map(data, ~attr(., "labels")) %>%
    purrr::compact() %>%
    tibble::enframe(name = "variable") %>%
    dplyr::mutate(value_name = map(value, names)) %>%
    tidyr::unnest()
}

