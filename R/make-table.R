#' Make a tabler object
#'
#' This function produces the underlying tabler object.
#' @importFrom purrr map
#' @export
tabler <- function(...,
                   title = NA_character_,
                   notes = NA_character_,
                   number = NA_character_,
                   latex_label = NA_character_) {
  in_cols <- list(...)

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

  # xlevels will follow the same method as gof.  It will be tougher though because
  # I do not know in advance the size of the vector.  This doesn't need to be a
  # data frame.  I'll make it a list.
  temp <- list()
  xlevels <- unique(unlist(lapply(in_cols, function(x) names(x$xlevels))))
  for (this_level in xlevels) {
    temp[[this_level]] <- unique(unlist(lapply(in_cols, function(x) ifelse(is.element(this_level, names(x$xlevels)), x$xlevels[this_level],NA))))
    if (any(is.na(temp[[this_level]]))) {  # If there are NA's, get rid of them
      temp[[this_level]] <- temp[[this_level]][-which(is.na(temp[[this_level]]))]
    }
  }
  tblr_obj$xlevels <- temp

  tblr_obj$theme <- tabler_theme() # Set the theme values as defaults

  # Order the coefficient vector
  tblr_obj$coefs <- order_variables(tblr_obj$coefs,
                                    tblr_obj$var_names,
                                    tblr_obj$xlevels)

  return(tblr_obj)
}


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
    temp <- list()
    xlevels <- unique(c(names(tblr_obj$xlevels), names(new_object$xlevels)))
    for (this_level in xlevels) {
      if (this_level %in% names(tblr_obj$xlevels)) {
        if (this_level %in% names(new_object$xlevels)) {
          temp[[this_level]] <- unique(c(tblr_obj$xlevels[[this_level]], new_object$xlevels[[this_level]]))
        } else {
          temp[[this_level]] <- tblr_obj$xlevels[[this_level]]
        }
      } else {
        temp[[this_level]] <- new_object$xlevels[[this_level]]
      }
    }
    tblr_obj$xlevels <- temp
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

combine_xlevels <- function(...) {
  in_levels <- list(...)

  xlevels <- purrr::map(in_levels, ~names(.x$xlevels)) %>%
    unlist() %>%
    unique()

  ret_val <- list()
  for (this_level in xlevels) {
    values <- purrr::map(in_levels, ~.x$xlevels[[this_level]]) %>%
      unlist() %>%
      unique()

    ret_val[[this_level]] <- values[[!is.na(values)]]
  }

  ret_val
}