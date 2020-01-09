#' Make a tabler object
#'
#' This function produces the underlying tabler object.
#' @importFrom purrr map map_chr map_dfr map_if
#' @importFrom dplyr mutate
#' @export
tabler <- function(...,
                   title = NA_character_,
                   notes = NA_character_,
                   number = NA_character_,
                   latex_label = NA_character_,
                   alias = NA_character_,
                   suppress = NA_character_,
                   omit = NA_character_,
                   gof_list = c(N = 'N',
                                r.squared = 'R^2',
                                adj.r.squared = 'Adj. R^2')) {
  in_cols <- list(...)

  # Allow for a list of results to be supplied.
  if (length(in_cols) == 1L) {
    if (class(in_cols) == 'list') {
      in_cols <- in_cols[[1]]
    }
  }

  # Check to make sure that every element of in_cols is a tabler_column
  # If it's not, then convert it.
  in_cols <- purrr::map_if(in_cols, not_tabler_column, make_column)

  # Create the tabler object
  tblr_obj <- list()
  attr(tblr_obj, "class") <- "tabler_object"
  tblr_obj$title <- title
  tblr_obj$notes <- notes
  tblr_obj$number <- number
  tblr_obj$latex_label <- latex_label
  tblr_obj$osa <- create_osa(omit, suppress, alias)

  tblr_obj$dep_vars <- purrr::map_chr(in_cols, ~.x$dep_var)
  tblr_obj$var_names <- purrr::map(in_cols, ~.x$var_names) %>%
    unlist() %>%
    unique()
  tblr_obj$est_types <- purrr::map_chr(in_cols, ~.x$est_type)

  # I am going to stack the coefficient matrices
  # estNum will record which estimation (column) the coefficients belong in
  tblr_obj$coefs <- purrr::map_dfr(in_cols, ~.x$coefs, .id = 'est_num') %>%
    mutate(est_num = as.numeric(est_num))

  # Combining gof data frames will be more difficult because the number of columns (stats)
  # may not be identical
  tblr_obj$gofs <- purrr::map_dfr(in_cols, ~.x$gof)

  tblr_obj$xlevels <- combine_xlevels(in_cols)
  tblr_obj$absorbed_vars <- purrr::map(in_cols, ~ .x$absorbed_vars)

  tblr_obj$theme <- tabler_theme() # Set the theme values as defaults

  # Order the coefficient vector
  tblr_obj$coefs <- order_variables(tblr_obj$coefs,
                                    tblr_obj$var_names,
                                    tblr_obj$xlevels)

  tblr_obj$gof_list <- gof_list

  return(tblr_obj)
}

#' Create Omit-Suppress-Alias (OSA) Object
#'
#' Create an OSA object.
create_osa <- function(omit = NULL, suppress = NULL, alias = NULL) {
  osa_obj <- list()
  class(osa_obj) <- 'tabler_osa'
  osa_obj$omit <- NA_character_
  osa_obj$suppress <- NA_character_
  osa_obj$alias <- NA_character_

  ret_val <- set_osa(osa_obj, omit = omit, suppress = suppress, alias = alias)
}

#' Set Omit-Suppress-Alias (OSA)
#'
#' Replaces an existing OSA specification with the terms supplied to this function.
#' If any one of the elments is not included, the any existing values of that term
#' will be maintained.  To erase the existing elements of a term, set its value to NA.
#' @param to A tabler_object
#' @param omit Character vector of variable names to be omitted
#' @param suppress Character vector of variable names to be suppressed. Only factor
#' variables can be suppressed.
#' @param alias Named character vector of variables to rename.  The name portion of the
#' vector should be the existing variable name and the value should be its new name.
#' @return An updated tabler_object
#' @export
set_osa <- function(osa, omit = NULL, suppress = NULL, alias = NULL) {

  if (!class(osa) == 'tabler_osa')
    stop('Must supply a valid OSA object to set_osa.')

  if (!is.null(omit)) osa$omit <- omit
  if (!is.null(suppress)) osa$suppress <- suppress
  if (!is.null(alias)) osa$alias <- alias

  osa
}

#' Add New Results to Existing Table
#'
#' Allows user to add an additional column of results to a pre-existing
#' tabler_object
#' @param tblr_obj A tabler_object
#' @param new_object The result object to be added.
#' @return A new tabler_object
#' @importFrom dplyr bind_rows
#' @export
`+.tabler_object` <- function(tblr_obj, new_object) {
  if (class(new_object) == "tabler_theme") tblr_obj$theme <- new_object
  else {
    if (class(new_object) != "tabler_col")
      new_object <- make_column(new_object)

    tblr_obj$dep_vars <- c(tblr_obj$dep_vars, new_object$dep_var)
    tblr_obj$var_names <- unique(c(tblr_obj$var_names, new_object$var_names))
    tblr_obj$estTypes <- c(tblr_obj$est_types, new_object$est_type)

    # Stack the coefficient data.frames
    new_object$coefs$est_num <- max(tblr_obj$coefs$est_num) + 1
    tblr_obj$coefs <- dplyr::bind_rows(tblr_obj$coefs, new_object$coefs)

    # Combine the gof data.frames.  This is complicated because the statistics will differ across
    # the columns.
    tblr_obj$gofs <- dplyr::bind_rows(tblr_obj$gofs, new_object$gof)

    # Consolidate xlevels
    # There must be a more efficient way to do this.
    tblr_obj$xlevels <- combine_xlevels(tblr_object, new_object)
    tblr_obj$absorbed_vars <- c(tblr_obj$absorbed_vars, new_object$absorbed_vars)
  }

  # Order the coefficient vector
  tblr_obj$coefs <- order_variables(tblr_obj$coefs, tblr_obj$var_names, tblr_obj$xlevels)

  tblr_obj
}

not_tabler_column <- function(x) {
  !is_tabler_column(x)
}

is_tabler_column <- function(x) {
  any(class(x) == 'tabler_column')
}

is_tabler <- function(x) {
  any(class(x) == 'tabler_object')
}

#' Combine xlevels
#'
#' This function takes a series of tabler_column objects and combines the xlevels
#' into a single list.  This provides a list of factor variables and all the
#' levels observed in the columns.
#' @param ... One or more tabler_column objects
#' @return A named list of character vectors where each name is a factor variable
#' included in the supplied tabler_objects and the character vector gives all of
#' the levels associated with that factor.
#' @importFrom purrr map
combine_xlevels <- function(cols) {
  xlevels <- purrr::map(cols, ~names(.x$xlevels)) %>%
    unlist() %>%
    unique()

  ret_val <- list()
  for (this_level in xlevels) {
    values <- purrr::map(cols, ~.x$xlevels[[this_level]]) %>%
      unlist() %>%
      unique()

    ret_val[[this_level]] <- values[!is.na(values)]
  }

  ret_val
}

#' Add Caption
#'
#' Adds caption to an existing tabler object or sum_tabler object..
#' @param tblr_obj A valid tabler object
#' @param title A character scalar with the new title to use
#' @return A tabler object with the new caption as a title
#' @export
add_caption <- function(tblr_obj, title = NA_character_) {
  if (!(is_tabler(tblr_obj) | is_sumtabler(tblr_obj)))
    stop('Must supply a valid tabler object to add_caption.')

  tblr_obj$title <- title
  tblr_obj
}
